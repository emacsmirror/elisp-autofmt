;;; elisp-autofmt.el --- Emacs lisp auto-format -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2019-2022  Campbell Barton

;; Author: Campbell Barton <ideasman42@gmail.com>

;; URL: https://codeberg.org/ideasman42/emacs-elisp-autofmt
;; Version: 0.1
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; Auto format emacs-lisp code on save.

;;; Usage

;; (elisp-autofmt-buffer) ; Auto-format the current buffer.
;;
;; You may also use the minor mode `elisp-autofmt-mode' which enables
;; formatting the buffer on save.

;;; Code:


;; ---------------------------------------------------------------------------
;; Public Custom Variables

(defgroup elisp-autofmt nil
  "Configure emacs-lisp auto-formatting behavior."
  :group 'tools)

;; Customization (Style).

(defcustom elisp-autofmt-style 'native
  "The formatting style to use."
  :type
  '(choice (const :tag "Native (Emacs indentation)" native)
           (const :tag "Fixed (Fixed indentation)" fixed)))
;;;###autoload
(put 'elisp-autofmt-style 'safe-local-variable #'symbolp)

(defcustom elisp-autofmt-format-quoted t
  "Format single-quoted S-expressions.
Otherwise existing line-breaks are kept and only indentation is performed."
  :type 'boolean)
;;;###autoload
(put 'elisp-autofmt-format-quoted 'safe-local-variable #'booleanp)

(defcustom elisp-autofmt-empty-line-max 2
  "The maximum number of blank lines to preserve."
  :type 'int)
;;;###autoload
(put 'elisp-autofmt-empty-line-max 'safe-local-variable #'integerp)

;; Customization (API Definitions).

(defcustom elisp-autofmt-use-function-defs t
  "When non nil, generate function definitions for the auto-formatter to use."
  :type 'boolean)


(defcustom elisp-autofmt-use-default-override-defs t
  "When non nil, make opinionated changes to how line breaks are handled."
  :type 'boolean)

(defcustom elisp-autofmt-load-packages-local nil
  "Additional packages/modules to include definitions from.

Each entry may be:
- A package identifier which will be loaded
  which isn't loaded by default on Emacs startup.
- A buffer relative path (beginning with a \".\"),
  which is intended to support sharing definitions for multi-file packages.

This is intended to be set from file or directory locals and is marked safe."
  :type '(repeat string))
;;;###autoload
(put 'elisp-autofmt-load-packages-local 'safe-local-variable #'elisp-autofmt-list-of-strings-p)
(make-variable-buffer-local 'elisp-autofmt-load-packages-local)

(defcustom elisp-autofmt-ignore-autoload-packages
  (list
   "babel"
   "gnus-fun"
   "gnus-xmas"
   "mailcrypt"
   "mc-toplev"
   "message"
   "messagexmas"
   "mh-tool-bar"
   "nnimap"
   "vcard")
  "Exclude these packages from inclusion in API definition lists.
Note that this should not need to be modified for typical use cases."
  :type '(repeat string))

;; Customization (Integration).

(defcustom elisp-autofmt-on-save-p 'elisp-autofmt-check-elisp-autofmt-exists
  "Only reformat on save if this function returns non-nil.

You may wish to choose one of the following options:
- `always': To always format on save.
- `elisp-autofmt-check-elisp-autofmt-exists':
  Only reformat when \".elisp-autofmt\" exists.

Otherwise you can set this to a user defined function."
  :type 'function)

(defcustom elisp-autofmt-python-bin nil
  "The Python binary to call to run the auto-formatting utility."
  :type 'string)

(defcustom elisp-autofmt-cache-directory
  (locate-user-emacs-file "elisp-autofmt-cache" ".elisp-autofmt-cache")
  "The directory to store cache data."
  :type 'string)

;; Customization (Parallel Computation).

(defcustom elisp-autofmt-parallel-jobs 0
  "The number of jobs to run in parallel.

- Use 0 to select automatically.
- Use -1 to disable parallel computation entirely."
  :type 'int)

(defcustom elisp-autofmt-parallel-threshold 32768
  "Buffers under this size will not use parallel computation.

- Use 0 to enable parallel computation for buffers of any size."
  :type 'int)


;; ---------------------------------------------------------------------------
;; Public Variables

(defvar elisp-autofmt-debug-extra-info nil
  "Show additional debug information.")

(defvar elisp-autofmt-debug-mode nil
  "Enable additional checks when formatting (enabled for tests).")


;; ---------------------------------------------------------------------------
;; Internal Variables

;; Run this command to format.
(defconst elisp-autofmt--this-file load-file-name)
(defconst elisp-autofmt--base (file-name-sans-extension elisp-autofmt--this-file))
(defconst elisp-autofmt--bin (concat elisp-autofmt--base ".py"))

;; Include these in the default emacs-binary API list.
;; Only use this for:
;; - Common packages so users don't have to manually list them.
;; - Packages that are not loaded by default.
(defconst elisp-autofmt--packages-default
  (list
   ;; For `pcase' & `pcase-let'.
   'pcase))


;; ---------------------------------------------------------------------------
;; Internal Utilities

(defun elisp-autofmt--bol-unless-non-blank (pos)
  "Return the line-beginning of POS when there is only blank space before point."
  (save-excursion
    (goto-char pos)
    (let ((bol (line-beginning-position)))
      (cond
       ((eq pos bol)
        bol)
       (t
        (goto-char bol)
        (skip-chars-forward "[:blank:]" (1+ pos))
        (when (< (point) pos)
          (setq bol nil))
        bol)))))

(defun elisp-autofmt--bool-as-int (val)
  "Return 0/1 from VAL, nil/t."
  (cond
   (val
    1)
   (t
    0)))

(defun elisp-autofmt--s-expr-range-around-pos (pos)
  "Return range around POS or nil."
  (let ((beg
         (ignore-errors
           (elt (syntax-ppss pos) 1))))
    (cond
     (beg
      ;; Note that `end' may be nil for un-matched brackets.
      ;; The caller must handle this case.
      (let ((end
             (ignore-errors
               (scan-sexps beg 1))))
        (cons beg end)))
     (t
      nil))))

(defun elisp-autofmt--s-expr-range-around-pos-dwim (pos)
  "Return range around POS, context sensitive."
  (save-excursion
    (goto-char pos)
    (let ((region-range (elisp-autofmt--s-expr-range-around-pos (line-beginning-position))))
      (unless region-range
        ;; Search for the widest range in this line.
        (let ((eol (line-end-position))
              (bol (line-beginning-position))

              (range-best-around-pos nil)
              (range-best-length-around-pos 0)

              (range-best nil)
              (range-best-length 0))

          (goto-char bol)
          (while (< (point) eol)
            (skip-syntax-forward "^()")
            (let ((syntax (car (syntax-after (point))))
                  (range-test nil)
                  (range-test-length nil))
              (cond
               ((eq syntax 4) ;; Opening bracket.
                ;; Matches `C-M-n', (forward-list 1).
                (let ((pos-other (scan-sexps (point) 1)))
                  (when pos-other
                    (setq range-test (cons (point) pos-other))
                    (setq range-test-length (- pos-other (point))))))
               ((eq syntax 5) ;; Closing bracket.
                ;; Matches `C-M-p', (forward-list -1).
                ;; Point must be after ')'.
                (let ((pos-other (scan-sexps (1+ (point)) -1)))
                  (when pos-other
                    (setq range-test (cons pos-other (point)))
                    (setq range-test-length (- (point) pos-other))))))

              (when range-test
                (when (< range-best-length range-test-length)
                  (setq range-best range-test)
                  (setq range-best-length range-test-length))
                (when (< range-best-length-around-pos range-test-length)
                  (when (and (<= (car range-test) pos) (<= pos (cdr range-test)))
                    (setq range-best-around-pos range-test)
                    (setq range-best-length-around-pos range-test-length)))))
            (forward-char 1))

          (setq region-range (or range-best-around-pos range-best))
          (when region-range
            (let ((beg-bol (elisp-autofmt--bol-unless-non-blank (car region-range))))
              (when beg-bol
                (setcar region-range beg-bol))))))
      region-range)))

(defun elisp-autofmt--call-checked (command-with-args)
  "Run COMMAND-WITH-ARGS, returning t on success.

Any `stderr' is output a message and is interpreted as failure."

  (when elisp-autofmt-debug-extra-info
    (message "elisp-autofmt: running command: %s" (mapconcat #'identity command-with-args " ")))

  (let ((sentinel-called nil)
        (this-buffer (current-buffer))
        (stdout-buffer nil)
        (stderr-buffer nil)
        (stderr-as-string nil)
        (proc-id "elisp-autofmt--call-checked"))
    (with-temp-buffer
      (setq stdout-buffer (current-buffer))
      (with-temp-buffer
        (setq stderr-buffer (current-buffer))
        (with-current-buffer this-buffer
          (let ((proc
                 (make-process
                  :name proc-id
                  :buffer stdout-buffer
                  :stderr stderr-buffer
                  :command command-with-args
                  :sentinel
                  (lambda (_proc _msg)
                    (setq sentinel-called t)

                    ;; FIXME: how to avoid printing status text in the first place?
                    (unless (zerop (buffer-size stderr-buffer))
                      (with-current-buffer stderr-buffer
                        (goto-char (point-min))
                        (save-match-data
                          (when (search-forward (concat "Process " proc-id " stderr finished")
                                                nil
                                                t)
                            (replace-match "" t nil nil)))
                        (goto-char (point-max))
                        (skip-chars-backward " \t\n" (point-min))
                        (when (bobp)
                          (erase-buffer))))
                    ;; End awkward hack.

                    (unless (zerop (buffer-size stderr-buffer))
                      (with-current-buffer stderr-buffer
                        (setq stderr-as-string (buffer-string))
                        (erase-buffer)))))))

            (while (not sentinel-called)
              (accept-process-output))
            (set-process-sentinel proc #'ignore)

            ;; May be an actual error, may be some warning about newer byte-code,
            ;; don't consider it fatal.
            (when stderr-as-string
              (message "elisp-autofmt: error output\n%s" stderr-as-string))

            (let ((exit-code (process-exit-status proc)))
              (cond
               ((not (zerop exit-code))
                (message "elisp-autofmt: Command %S failed with exit code %d!"
                         command-with-args
                         exit-code)
                nil)
               (t
                ;; Do nothing.
                t)))))))))

;; ---------------------------------------------------------------------------
;; Internal Introspection / Cache Functions

;; For `find-library-name'.
(require 'find-func)
;; For `file-loadhist-lookup'.
(require 'loadhist)
;; For `find-lisp-object-file-name'.
;; (require 'help-fns)

(defun elisp-autofmt--cache-api-val-as-str (val)
  "Return the string representation of VAL (use for JSON encoding)."
  (cond
   ((symbolp val)
    (concat "\"" (symbol-name val) "\""))
   (t
    (number-to-string val))))

(defun elisp-autofmt--cache-api-file-is-older-list (file-test file-list)
  "Return t when FILE-TEST is older than any files in FILE-LIST."
  (catch 'result
    (let ((file-test-time (file-attribute-modification-time (file-attributes file-test))))
      (dolist (file-new file-list)
        (when (time-less-p
               file-test-time (file-attribute-modification-time (file-attributes file-new)))
          (throw 'result t)))
      nil)))

(defun elisp-autofmt--cache-api-file-is-older (file-test &rest file-list)
  "Return t when FILE-TEST is older than any files in FILE-LIST."
  (elisp-autofmt--cache-api-file-is-older-list file-test file-list))

(defun elisp-autofmt--cache-api-encode-name (filename)
  "Return the cache name in cache-dir from FILENAME."
  (concat (url-hexify-string filename) ".json"))

;; Use a different name for externally generated definitions
;; because it's possible they contain less/different information.
;; In this case it's possible that the order of generating different
;; definitions files could give different results,
;; so name them differently to avoid confusion.
(defun elisp-autofmt--cache-api-encode-name-external (filename)
  "Return the Python cache name in cache-dir from FILENAME."
  (concat (url-hexify-string filename) ".external.json"))

(defun elisp-autofmt--cache-api-directory-ensure ()
  "Ensure the cache API directory exists."
  (unless (file-directory-p elisp-autofmt-cache-directory)
    (make-directory elisp-autofmt-cache-directory t)))

(defun elisp-autofmt--cache-api-insert-function-to-file (sym-id sym-name sym-ty arity)
  "Insert JSON data from SYM-ID, SYM-NAME, SYM-TY and ARITY."
  ;; `arity' is an argument because built-in functions use different logic.

  ;; There are many other properties, however they don't relate to formatting so much.
  (let ((properties nil))
    (let ((val (function-get sym-id 'lisp-indent-function t)))
      (when val
        (cond
         ((numberp val)
          (push (format "\"indent\": %d" val) properties))
         ((symbolp val)
          ;; Perform the lookup here, avoid the burden on the caller having to check!
          (push (format "\"indent\": \"%s\"" (symbol-name val)) properties)))))

    (let ((val (function-get sym-id 'doc-string-elt t)))
      (when val
        (cond
         ((numberp val)
          (push (format "\"doc-string\": %d" val) properties))
         ((symbolp val)
          (push (format "\"doc-string\": \"%s\"" (symbol-name val)) properties)))))

    (insert "\"" (string-replace "\\" "\\\\" sym-name) "\": ")
    (insert
     "["
     (concat "\"" (symbol-name sym-ty) "\"")
     ", "
     (elisp-autofmt--cache-api-val-as-str (car arity))
     ", "
     (elisp-autofmt--cache-api-val-as-str (cdr arity))
     ;; Dictionary for additional hints.
     ", {"
     (cond
      (properties
       (mapconcat #'identity properties ", "))
      (t
       ""))
     "}],\n")))

(defun elisp-autofmt--fn-type (sym-id)
  "Return the type of function SYM-ID or nil."
  (cond
   ((functionp sym-id)
    'func)
   ((macrop sym-id)
    'macro)
   ((special-form-p sym-id)
    'special)
   (t
    nil)))

(defun elisp-autofmt--fn-defs-insert (defs include-private)
  "Insert all function from DEFS into the current buffer.
When INCLUDE-PRIVATE is nil, exclude functions with \"--\" in their names."
  (while defs
    (let ((n (pop defs)))
      (when (consp n)
        (pcase-let ((`(,_sym-ty-xx . ,sym-id) n))
          (let ((sym-ty (elisp-autofmt--fn-type sym-id)))
            (when sym-ty
              (let ((sym-name (symbol-name sym-id)))
                ;; Ignore "--" separators as this is a convention for private names.
                (when (or include-private (null (string-match-p "--" sym-name)))
                  (elisp-autofmt--cache-api-insert-function-to-file
                   sym-id sym-name sym-ty (func-arity sym-id)))))))))))

(defun elisp-autofmt--cache-api-generate-for-builtins (filepath)
  "Generate API cache for built-in output at FILEPATH."
  (with-temp-buffer
    (insert "{\n")
    (insert "\"functions\": {\n")
    (let ((block-beg (point)))
      (mapatoms
       (lambda (sym-id)
         (let ((sym-fn (symbol-function sym-id)))
           (when sym-fn
             (let ((auto-load-pkg (and (autoloadp sym-fn) (cadr sym-fn)))
                   (sym-ty (elisp-autofmt--fn-type sym-id)))

               (when (and sym-ty
                          ;; Is it non-interactive?
                          ;; (not (commandp (symbol-function sym-id)))
                          ;; Is it built-in? (speeds up accessing the file-path which is slow).
                          (subrp sym-fn)
                          (or (null auto-load-pkg)
                              (not (member auto-load-pkg elisp-autofmt-ignore-autoload-packages))))
                 ;; (autoload sym-id)

                 ;; Note that we could check for C-source only using.
                 ;; (find-lisp-object-file-name sym-id sym-fn)

                 (when t
                   ;; (eq file 'C-source)
                   (elisp-autofmt--cache-api-insert-function-to-file
                    sym-id (symbol-name sym-id) sym-ty
                    (cond
                     ((subrp sym-fn)
                      (subr-arity sym-fn))
                     (t
                      (func-arity sym-id)))))))))))

      ;; Inline built-in packages:
      ;; This avoids the hassles of having to hand maintain a list of built-in packages.
      ;; While the result is much larger, it avoids a lot of knit-picking over what
      ;; should/shouldn't be included. Just include everything loaded as part of Emacs
      ;; (in batch mode), and script can manually include other packages they depend on.

      ;; Load some additional packages.
      (dolist (package-id elisp-autofmt--packages-default)
        (require package-id))

      (let ((item-list load-history))
        (while item-list
          (let ((item (pop item-list)))
            (let ((defs (cdr item)))
              (elisp-autofmt--fn-defs-insert defs nil)))))

      ;; Remove trailing comma (tsk).
      (delete-region (max block-beg (- (point) 2)) (max block-beg (- (point) 1))))

    (insert "}\n") ; "functions".
    (insert "}\n")
    (write-region nil nil filepath nil 0)))

(defun elisp-autofmt--cache-api-generate-for-package (filepath package-id skip-require)
  "Generate API cache for PACKAGE-ID at FILEPATH.

When SKIP-REQUIRE is non-nil, the package is not required."
  (let ((package-sym (intern package-id)))
    (when (cond
           (skip-require
            t)
           ((member package-id (list "subr"))
            t)
           ((with-demoted-errors "%S"
              (require package-sym)
              t)
            t)
           (t
            (message "Unable to load %s" package-id)
            nil))

      ;; Ensure the cache is newer than it's source.
      (with-temp-buffer
        (insert "{\n")
        ;; Allow for other kinds of data in these files in the future.
        (insert "\"functions\": {\n")
        (let ((block-beg (point)))
          (let ((defs (file-loadhist-lookup package-id)))
            (elisp-autofmt--fn-defs-insert defs t)
            ;; Remove trailing comma (tsk).
            (delete-region (max block-beg (- (point) 2)) (max block-beg (- (point) 1))))
          (insert "}\n") ; "functions".
          (insert "}\n")
          (write-region nil nil filepath nil 0))))))

(defun elisp-autofmt--gen-builtin-defs ()
  "Generate builtin definitions.

Writes outputs to `ELISP_AUTOFMT_OUTPUT'."
  (let ((output-path (getenv "ELISP_AUTOFMT_OUTPUT")))
    (unless output-path
      (error "elisp-autofmt: $ELISP_AUTOFMT_OUTPUT was not set for built-ins!"))
    (elisp-autofmt--cache-api-generate-for-builtins output-path)))

(defun elisp-autofmt--gen-package-defs ()
  "Generate builtin definitions.

Uses package from environment variable `ELISP_AUTOFMT_PACKAGE'.
Writes outputs to environment variable `ELISP_AUTOFMT_OUTPUT'."
  (let ((output-path (getenv "ELISP_AUTOFMT_OUTPUT"))
        (package-id (getenv "ELISP_AUTOFMT_PACKAGE")))
    (unless output-path
      (error "elisp-autofmt: $ELISP_AUTOFMT_OUTPUT was not set for package!"))
    (unless output-path
      (error "elisp-autofmt: $ELISP_AUTOFMT_PACKAGE was not set for package!"))
    (elisp-autofmt--cache-api-generate-for-package output-path package-id nil)))

(defun elisp-autofmt--cache-api-ensure-cache-for-emacs (use-external-emacs)
  "Ensure cache exists.

Call an external Emacs when USE-EXTERNAL-EMACS is non-nil."
  ;; Emacs binary location `filename'.
  (let* ((filename (expand-file-name invocation-name invocation-directory))
         (filename-cache-name-only (elisp-autofmt--cache-api-encode-name filename))
         (filename-cache-name-full
          (file-name-concat elisp-autofmt-cache-directory filename-cache-name-only)))
    (when (or (not (file-exists-p filename-cache-name-full))
              (elisp-autofmt--cache-api-file-is-older filename-cache-name-full filename))

      (cond
       (use-external-emacs
        (let ((process-environment
               (cons
                (concat "ELISP_AUTOFMT_OUTPUT=" filename-cache-name-full) process-environment)))

          (elisp-autofmt--call-checked
           (list
            filename
            "--batch"
            "-l"
            elisp-autofmt--this-file
            "--eval"
            "(elisp-autofmt--gen-builtin-defs)"))))
       (t
        (elisp-autofmt--cache-api-generate-for-builtins filename-cache-name-full))))
    filename-cache-name-only))

(defun elisp-autofmt--cache-api-ensure-cache-for-package (package-id skip-require)
  "Ensure cache for PACKAGE-ID is up to date in CACHE-DIR.

When SKIP-REQUIRE is set, don't require the package."
  (let ((package-sym (intern package-id)))

    (when (cond
           (skip-require
            t)
           ((with-demoted-errors "%S"
              (require package-sym)
              t)
            t)
           (t
            (message "Unable to load %s" package-id)
            nil))

      (let* ((filename (find-library-name package-id))
             (filename-cache-name-only (elisp-autofmt--cache-api-encode-name filename))
             (filename-cache-name-full
              (file-name-concat elisp-autofmt-cache-directory filename-cache-name-only)))

        ;; Ensure the cache is newer than it's source.
        (when (or (not (file-exists-p filename-cache-name-full))
                  (elisp-autofmt--cache-api-file-is-older filename-cache-name-full filename))
          (elisp-autofmt--cache-api-generate-for-package
           filename-cache-name-full package-id skip-require))
        filename-cache-name-only))))

(defun elisp-autofmt--cache-api-ensure-cache-for-filepath (filepath)
  "Generate cache for FILEPATH."
  (let* ((filename-cache-name-only (elisp-autofmt--cache-api-encode-name-external filepath))
         (filename-cache-name-full
          (file-name-concat elisp-autofmt-cache-directory filename-cache-name-only)))

    (when (or (not (file-exists-p filename-cache-name-full))
              (elisp-autofmt--cache-api-file-is-older filename-cache-name-full filepath))

      (let ((command-with-args
             (append
              ;; Python command.
              (list (or elisp-autofmt-python-bin "python"))
              ;; Debug mode.
              (cond
               (elisp-autofmt-debug-mode
                (list))
               (t
                (list "-OO")))
              ;; Main command.
              (list
               elisp-autofmt--bin
               "--gen-defs"
               filepath
               (expand-file-name filename-cache-name-full)))))

        (elisp-autofmt--call-checked command-with-args)))))

(defun elisp-autofmt--cache-api-cache-update (buffer-directory)
  "Ensure packages are up to date for `current-buffer' in BUFFER-DIRECTORY."
  (elisp-autofmt--cache-api-directory-ensure)
  (let ((cache-files (list)))
    (push (elisp-autofmt--cache-api-ensure-cache-for-emacs t) cache-files)
    (let ((package-list-paths (list))
          (package-list (list)))

      (let ((packages elisp-autofmt-load-packages-local))
        (while packages
          (let ((var (pop packages)))
            (cond
             ((string-prefix-p "." var)
              (push var package-list-paths))
             (t
              (push var package-list))))))

      ;; Merge default and any local features into a list.
      (let ((packages-all (delete-dups package-list)))
        (dolist (package-id packages-all)
          (cond
           ((stringp package-id)
            (push (elisp-autofmt--cache-api-ensure-cache-for-package package-id t) cache-files))
           (t ; Unlikely, just helpful hint to users.
            (message "elisp-autofmt: skipping non-string feature reference %S" package-id)))))

      ;; Ensure external definitions.
      (when package-list-paths
        (while package-list-paths
          (let ((var (pop package-list-paths)))
            (let ((filename (file-name-concat buffer-directory (substring var 1))))
              (push (elisp-autofmt--cache-api-ensure-cache-for-filepath filename) cache-files))))))

    cache-files))


;; ---------------------------------------------------------------------------
;; Internal Functions

(defun elisp-autofmt--replace-buffer-contents-isolate-region (buf-src beg end is-interactive)
  "Isolate the region to be replaced in BEG END to format the region/selection.
Argument BUF-SRC is the buffer containing the formatted text."
  ;; Use a simple trick, replace the beginning and of the formatted buffer
  ;; with the original (unformatted) text.

  ;; Keep the original beginning because we may want to expand back to the beginning
  ;; of the line if there is only white-space before the contracted bounds.
  ;; This is needed so formatting a block does not have wrong indentation.
  (let ((beg-orig beg)
        (changed nil))
    ;; Contract region to bracket bounds, quote or comment bounds,
    ;; note that we are not strict about the syntax, it's possible these
    ;; characters are inside comments or strings. The logic will still work.
    (while (and beg (not (memq (char-after beg) (list ?\( ?\[ ?\" ?\;))))
      (setq beg (1+ beg))
      (unless (<= beg end)
        (setq beg nil)))

    (unless beg
      (setq end nil))

    (while (and end (not (memq (char-before end) (list ?\) ?\] ?\" ?\;))))
      (setq end (1- end))
      (unless (<= beg end)
        (setq end nil)))

    (unless (and beg end)
      (user-error "Region contains no S-expressions or vector literals!"))

    (let* ((buf-dst (current-buffer))

           (buf-dst-pos-min (point-min))
           (buf-dst-pos-max (point-max))

           (beg-index 0)
           (end-index 0)

           (beg-char (char-after beg))
           (end-char (char-before end))

           (beg-str (char-to-string beg-char))
           (end-str (char-to-string end-char))

           (beg-dst-pos nil)
           (end-dst-pos nil)
           (beg-dst-pos-bol nil)

           (beg-src-pos nil)
           (end-src-pos nil)
           (beg-src-pos-bol nil))

      (save-excursion
        (goto-char (point-min))
        (let ((limit (1+ beg)))
          (while (search-forward beg-str limit t)
            (setq beg-index (1+ beg-index)))
          ;; The point before the character.
          (setq beg-dst-pos (1- (point)))
          (setq beg-dst-pos-bol (elisp-autofmt--bol-unless-non-blank beg-dst-pos))
          (setq limit (1+ end))
          (while (search-forward end-str limit t)
            (setq end-index (1+ end-index)))
          (setq end-dst-pos (point)))

        ;; Load the formatted buffer and replace the head & tail with unformatted text
        ;; so as only to reformat the requested region.
        (with-current-buffer buf-src
          (let ((i 0))
            (goto-char (point-min))
            (while (and (< i beg-index) (search-forward beg-str nil t))
              (setq i (1+ i)))
            ;; The point before the character.
            (setq beg-src-pos (1- (point)))
            (setq beg-src-pos-bol (elisp-autofmt--bol-unless-non-blank beg-src-pos))
            (setq i 0)
            (while (and (< i end-index) (search-forward end-str nil t))
              (setq i (1+ i)))
            (setq end-src-pos (point)))

          ;; Optionally expand the beginning to include indentation,
          ;; without this lines may be badly indented.
          ;; Only do this when:
          ;; - When white-space is included in the original region.
          ;; - When there is space before the formatted text both before & after formatting.
          (when (and beg-dst-pos-bol beg-src-pos-bol (<= beg-dst-pos-bol beg-orig))
            (setq beg-dst-pos beg-dst-pos-bol)
            (setq beg-src-pos beg-src-pos-bol))

          (when is-interactive
            ;; Report if formatting was performed.
            (cond
             ((not (eq (- end-dst-pos beg-dst-pos) (- end-src-pos beg-src-pos)))
              (setq changed t))
             (t
              (let ((str-src (buffer-substring-no-properties beg-src-pos end-src-pos))
                    (str-dst
                     (with-current-buffer buf-dst
                       (buffer-substring-no-properties beg-dst-pos end-dst-pos))))
                (unless (string-equal str-src str-dst)
                  (setq changed t))))))

          ;; Replace unformatted code at the beginning and end.
          (delete-region end-src-pos (point-max))
          (delete-region (point-min) beg-src-pos)

          (goto-char (point-max))
          (insert-buffer-substring buf-dst end-dst-pos buf-dst-pos-max)

          (goto-char (point-min))
          (insert-buffer-substring buf-dst buf-dst-pos-min beg-dst-pos))))
    changed))

(defun elisp-autofmt--replace-buffer-contents-with-fastpath (buf region-range is-interactive)
  "Replace buffer contents with BUF, fast-path when undo is disabled.

Useful for fast operation, especially for automated conversion or tests.
Argument REGION-RANGE optionally replaces a region when non-nil.
Argument IS-INTERACTIVE is set when running interactively."
  (let ((is-beg (bobp))
        (is-end (eobp))
        (changed t))

    ;; Optionally format within a region,
    (when region-range
      (setq changed
            (elisp-autofmt--replace-buffer-contents-isolate-region
             buf (car region-range) (cdr region-range) is-interactive))

      (when is-interactive
        (message "elisp-autofmt: %s"
                 (cond
                  (changed
                   "reformat")
                  (t
                   "reformat (unnecessary)")))))

    (cond
     ((null changed))
     ((and (eq t buffer-undo-list) (or is-beg is-end))
      ;; No undo, use a simple method instead of `replace-buffer-contents',
      ;; which has no benefit unless undo is in use.
      (erase-buffer)
      (insert-buffer-substring buf)
      (cond
       (is-beg
        (goto-char (point-min)))
       (is-end
        (goto-char (point-max)))))
     (t
      (cond
       (is-interactive
        ;; When run interactively replace the buffer contents if this takes over 1 second.
        (replace-buffer-contents buf 1.0))
       (t
        (replace-buffer-contents buf)))))))

(defun elisp-autofmt--region-impl
    (stdout-buffer stderr-buffer region-range to-file is-interactive &optional assume-file-name)
  "Auto format the current region using temporary STDOUT-BUFFER & STDERR-BUFFER.
Optional argument ASSUME-FILE-NAME overrides the file name used for this buffer.

Argument REGION-RANGE optionally defines a region to format.
Argument TO-FILE writes to the file directly, without updating the buffer.
Argument IS-INTERACTIVE is set when running interactively."

  (unless assume-file-name
    (setq assume-file-name buffer-file-name))

  (let* ((stderr-as-string nil)
         (pipe-err-as-string nil)
         (sentinel-called nil)
         (default-coding
          (cond
           ((boundp 'default-buffer-file-coding-system)
            default-buffer-file-coding-system)
           (t
            'utf-8)))

         ;; Cache files.
         (cache-defs
          (cond
           (elisp-autofmt-use-function-defs
            (elisp-autofmt--cache-api-cache-update
             (cond
              (assume-file-name
               (file-name-directory assume-file-name))
              (t
               ;; In this case, any relative path references
               ;; from a buffer without a path, uses the default directory.
               ;; In practice it seems unlikely the kinds of buffers that aren't backed
               ;; by a file would reference relative tags, nevertheless, there is no need
               ;; for this operation to fail with an error, see #2.
               default-directory))))
           (t
            nil)))

         (command-with-args
          (append
           ;; Python command.
           (list (or elisp-autofmt-python-bin "python"))
           ;; Debug mode.
           (cond
            (elisp-autofmt-debug-mode
             (list))
            (t
             (list "-OO")))
           ;; Main command.
           (list
            elisp-autofmt--bin
            ;; No messages.
            "--quiet"
            ;; Don't use the file, use the stdin instead.
            "--stdin"
            ;; Use the standard outpt.
            "--stdout"
            ;; Follow the 'fill-column' setting.
            (format "--fmt-fill-column=%d" fill-column)
            (format "--fmt-empty-lines=%d" elisp-autofmt-empty-line-max)
            (format "--fmt-style=%s" (symbol-name elisp-autofmt-style))
            (format "--fmt-quoted=%d" (elisp-autofmt--bool-as-int elisp-autofmt-format-quoted))

            (format "--parallel-jobs=%d"
                    (cond
                     ;; Disable multi-processing on MS-Windows,
                     ;; seems to cause performance issues which need investigating.
                     ((memq system-type (list 'ms-dos 'windows-nt))
                      -1)
                     ((<= (buffer-size) elisp-autofmt-parallel-threshold)
                      -1)
                     (t
                      elisp-autofmt-parallel-jobs)))

            ;; Not 0 or 1.
            "--exit-code=2")

           ;; Optionally read in definitions.
           (cond
            ((or elisp-autofmt-use-function-defs elisp-autofmt-use-default-override-defs)
             (list
              (concat
               "--fmt-defs-dir="
               (convert-standard-filename (expand-file-name elisp-autofmt-cache-directory)))
              (concat
               "--fmt-defs="
               (mapconcat #'identity
                          (append
                           ;; May be nil (skipped).
                           cache-defs
                           ;; Optionally
                           (cond
                            (elisp-autofmt-use-default-override-defs
                             (list (concat elisp-autofmt--base ".overrides.json")))
                            (t
                             (list))))
                          path-separator))))
            (t
             (list))))))

    (when elisp-autofmt-debug-extra-info
      (message "elisp-autofmt: running piped process: %s"
               (mapconcat #'identity command-with-args " ")))

    (let ((proc
           (make-process
            :name "elisp-autofmt"
            :buffer stdout-buffer
            :stderr stderr-buffer
            :connection-type 'pipe
            :command command-with-args
            :coding (cons default-coding default-coding)
            :sentinel
            (lambda (_proc _msg)
              (setq sentinel-called t)

              ;; Assign in the sentinel to prevent "Process .. finished"
              ;; being written to `stderr-buffer' otherwise it's difficult
              ;; to know if there was an error or not since an exit value
              ;; of 2 may be used for invalid arguments as well as to check
              ;; if the buffer was re-formatted.
              (unless (zerop (buffer-size stderr-buffer))
                (with-current-buffer stderr-buffer
                  (setq stderr-as-string (buffer-string))
                  (erase-buffer)))))))

      (condition-case err
          (progn
            (process-send-region proc (point-min) (point-max))
            (process-send-eof proc))
        (file-error
         ;; Formatting exited with an error, closing the `stdin' during execution.
         ;; Even though the `stderr' will almost always be set,
         ;; store the error as it may show additional context.
         (setq pipe-err-as-string (error-message-string err))))

      (while (not sentinel-called)
        (accept-process-output))
      (set-process-sentinel proc #'ignore)

      (let ((exit-code (process-exit-status proc)))
        (cond
         ((or (not (eq exit-code 2)) stderr-as-string pipe-err-as-string)
          (when pipe-err-as-string
            (message "elisp-autofmt: error code %d, sending input (%s)"
                     exit-code
                     pipe-err-as-string))
          (when stderr-as-string
            (message "elisp-autofmt: error code %d, output\n%s" exit-code stderr-as-string))

          (when elisp-autofmt-debug-extra-info
            (message "elisp-autofmt: Command %S failed with exit code %d!"
                     command-with-args
                     exit-code))
          nil)
         (t
          (cond
           (to-file
            (with-current-buffer stdout-buffer
              (write-region (point-min) (point-max) assume-file-name)))
           (t
            (elisp-autofmt--replace-buffer-contents-with-fastpath
             stdout-buffer region-range is-interactive)))))))))

(defun elisp-autofmt--region (region-range to-file is-interactive &optional assume-file-name)
  "Auto format the current buffer in REGION-RANGE.
Optional argument ASSUME-FILE-NAME overrides the file name used for this buffer.

See `elisp-autofmt--region-impl' for TO-FILE and IS-INTERACTIVE doc-strings."
  (let ((stdout-buffer nil)
        (stderr-buffer nil)
        (this-buffer (current-buffer)))
    (with-temp-buffer
      (setq stdout-buffer (current-buffer))
      (with-temp-buffer
        (setq stderr-buffer (current-buffer))
        (with-current-buffer this-buffer
          (elisp-autofmt--region-impl
           stdout-buffer stderr-buffer region-range to-file is-interactive
           assume-file-name))))))

(defun elisp-autofmt--buffer-impl (buf region-range to-file is-interactive)
  "Auto-format the entire buffer BUF in REGION-RANGE.

See `elisp-autofmt--region-impl' for TO-FILE and IS-INTERACTIVE doc-strings."
  (with-current-buffer buf
    (elisp-autofmt--region region-range to-file is-interactive)))

(defun elisp-autofmt--buffer-format-for-save-hook ()
  "The hook to run on buffer saving to format the buffer."
  ;; Demote errors as this is user configurable, we can't be sure it wont error.
  (when (with-demoted-errors "elisp-autofmt: Error %S"
          (funcall elisp-autofmt-on-save-p))
    (elisp-autofmt-buffer))
  ;; Continue to save.
  nil)

(defun elisp-autofmt--enable ()
  "Setup an auto-format save hook for this buffer."
  ;; Buffer local hook.
  (add-hook 'before-save-hook #'elisp-autofmt--buffer-format-for-save-hook nil t))

(defun elisp-autofmt--disable ()
  "Disable the hooks associated with `elisp-autofmt-mode'."
  (remove-hook 'before-save-hook #'elisp-autofmt--buffer-format-for-save-hook t))


;; ---------------------------------------------------------------------------
;; Public Functions


;;;###autoload
(defun elisp-autofmt-buffer-to-file ()
  "Auto format the current buffer, writing it's output to a file.

This is intended for use by batch processing scripts,
where loading changes back into the buffer is not important."
  (unless buffer-file-name
    (error "A buffer with a valid file-name expected!"))
  (elisp-autofmt--buffer-impl (current-buffer) nil t nil))

;;;###autoload
(defun elisp-autofmt-buffer ()
  "Auto format the current buffer."
  (interactive)
  (let ((is-interactive (called-interactively-p 'interactive)))
    (elisp-autofmt--buffer-impl (current-buffer) nil nil is-interactive)))

;;;###autoload
(defun elisp-autofmt-region (&optional beg end)
  "Auto format the active region of the current buffer.
Optionally use BEG & END, otherwise an active region is required."
  (interactive)

  (unless (and beg end)
    (unless (region-active-p)
      (user-error "No active region"))
    (setq beg (region-beginning))
    (setq end (region-end)))

  (let ((is-interactive (called-interactively-p 'interactive)))
    (elisp-autofmt--buffer-impl (current-buffer) (cons beg end) nil is-interactive)))

;;;###autoload
(defun elisp-autofmt-region-dwim ()
  "Context sensitive auto formatting of the current buffer.
When there is an active region, this is used,
otherwise format the surrounding S-expression."
  (interactive)
  (cond
   ((region-active-p)
    (elisp-autofmt-region))
   (t
    (let ((region-range (elisp-autofmt--s-expr-range-around-pos-dwim (point))))
      (unless region-range
        (user-error "Unable to find surrounding brackets!"))

      (let ((is-interactive (called-interactively-p 'interactive)))
        (elisp-autofmt--buffer-impl (current-buffer) region-range nil is-interactive))))))

;;;###autoload
(defun elisp-autofmt-check-elisp-autofmt-exists ()
  "Return non-nil when `.elisp-autofmt' is found in a parent directory."
  (let ((cfg (locate-dominating-file (file-name-directory buffer-file-name) ".elisp-autofmt")))
    (cond
     (cfg
      t)
     (t
      nil))))

;; Auto load as this is a callback for `safe-local-variable'.
;; NOTE: `list-of-strings-p' can be used once EMACS-29 is the minimum supported version.
;;;###autoload
(defun elisp-autofmt-list-of-strings-p (obj)
  "Return t when OBJ is a list of strings."
  (and (listp obj) (not (memq nil (mapcar #'stringp obj)))))

;;;###autoload
(define-minor-mode elisp-autofmt-mode
  "Elisp-AutoFMT minor mode."
  :global nil
  :lighter ""
  :keymap nil

  (cond
   (elisp-autofmt-mode
    (elisp-autofmt--enable))
   (t
    (elisp-autofmt--disable))))

(provide 'elisp-autofmt)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; elisp-autofmt-format-quoted: nil
;; End:
;;; elisp-autofmt.el ends here
