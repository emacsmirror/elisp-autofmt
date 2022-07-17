;;; elisp-autofmt.el --- Emacs lisp auto-format -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-2.0-or-later
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

;;; Code:

;; Public variables.

(defgroup elisp-autofmt nil "Configure emacs-lisp auto-formatting." :group 'tools)

(defcustom elisp-autofmt-empty-line-max 2 "The maximum number of blank lines to keep." :type 'int)

(defcustom elisp-autofmt-on-save-p 'elisp-autofmt-check-elisp-autofmt-exists
  "Only reformat on save if this function returns non-nil.

You may wish to choose one of the following options:
- `always': To always format on save.
- `elisp-autofmt-check-elisp-autofmt-exists':
  Only reformat when \"pyproject.toml\" exists.

Otherwise you can set this to a user defined function."
  :type 'function)


(defcustom elisp-autofmt-use-function-defs nil
  "When non nil, generate function definitions for the auto-formatter to use.

Can be slow!"
  :type 'boolean)

(defcustom elisp-autofmt-python-bin nil
  "The Python binary to call when running auto-formatting."
  :type 'string)

(defcustom elisp-autofmt-cache-packages-default
  ;; (list "custom" "subr")
  nil
  "Packages to load by default (without being explicitly required)."
  :type '(repeat string))

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
  "Packages to load by default (without being explicitly required)."
  :type '(repeat string))

(defcustom elisp-autofmt-cache-directory
  (file-name-concat (locate-user-emacs-file ".cache" ".cache") "elisp-autofmt")
  "The directory to store cache data."
  :type 'string)


;; Internal variables.

;; Run this command to format.
(defvar elisp-autofmt--bin)
(setq elisp-autofmt--bin (file-name-sans-extension load-file-name))

(defconst elisp-autofmt--this-file load-file-name)


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
        (when
          (time-less-p
            file-test-time
            (file-attribute-modification-time (file-attributes file-new)))
          (throw 'result t)))
      nil)))

(defun elisp-autofmt--cache-api-file-is-older (file-test &rest file-list)
  "Return t when FILE-TEST is older than any files in FILE-LIST."
  (elisp-autofmt--cache-api-file-is-older-list file-test file-list))

(defun elisp-autofmt--cache-api-encode-name (filename)
  "Return the cache name in cache-dir from FILENAME."
  (concat (url-hexify-string filename) ".json"))

(defun elisp-autofmt--cache-api-directory-ensure ()
  "Ensure the cache API directory exists."
  (unless (file-directory-p elisp-autofmt-cache-directory)
    (make-directory elisp-autofmt-cache-directory t)))

(defun elisp-autofmt--cache-api-insert-function-to-file (sym-id sym-ty arity)
  "Insert JSON data from SYM-ID, SYM-TY and ARITY."
  ;; `arity' is an argument because built-in functions use different logic.

  (insert "\"" (string-replace "\\" "\\\\" (symbol-name sym-id)) "\": ")
  (insert
    "["
    (concat "\"" (symbol-name sym-ty) "\"")
    ", "
    (elisp-autofmt--cache-api-val-as-str (car arity))
    ", "
    (elisp-autofmt--cache-api-val-as-str (cdr arity))
    "],\n"))

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
              (let
                (
                  (auto-load-pkg (and (autoloadp sym-fn) (cadr sym-fn)))
                  (sym-ty (elisp-autofmt--fn-type sym-id)))

                (when
                  (and
                    sym-ty
                    ;; Is it non-interactive?
                    ;; (not (commandp (symbol-function sym-id)))
                    ;; Is it built-in? (speeds up accessing the file-path which is slow).
                    (subrp sym-fn)
                    (or
                      (null auto-load-pkg)
                      (not (member auto-load-pkg elisp-autofmt-ignore-autoload-packages))))
                  ;; (autoload sym-id)

                  ;; Note that we could check for C-source only using.
                  ;; (find-lisp-object-file-name sym-id sym-fn)

                  (when t
                    ;; (eq file 'C-source)
                    (elisp-autofmt--cache-api-insert-function-to-file
                      sym-id sym-ty
                      (cond
                        ((subrp sym-fn)
                          (subr-arity sym-fn))
                        (t
                          (func-arity sym-id)))))))))))
      ;; Remove trailing comma (tsk).
      (delete-region (max block-beg (- (point) 2)) (max block-beg (- (point) 1))))
    (insert "}\n") ;; "functions".
    (insert "}\n")
    (write-region nil nil filepath nil 0)))

(defun elisp-autofmt--cache-api-generate-for-package (filepath package-id skip-require)
  "Generate API cache for PACKAGE-ID at FILEPATH.

When SKIP-REQUIRE is non-nil, the package is not required."
  (let ((package-sym (intern package-id)))
    (when
      (cond
        (skip-require
          t)
        ((member package-id (list "subr"))
          t)
        ((with-demoted-errors "%S" (require package-sym) t)
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
            (while defs
              (let ((n (pop defs)))
                (when (consp n)
                  (pcase-let ((`(,_sym-ty-xx . ,sym-id) n))
                    (let ((sym-ty (elisp-autofmt--fn-type sym-id)))
                      (when sym-ty
                        (elisp-autofmt--cache-api-insert-function-to-file
                          sym-id
                          sym-ty
                          (func-arity sym-id))))))))
            ;; )
            ;; Remove trailing comma (tsk).
            (delete-region (max block-beg (- (point) 2)) (max block-beg (- (point) 1))))
          (insert "}\n") ;; "functions".
          (insert "}\n")
          (write-region nil nil filepath nil 0))))))

(defun elisp-autofmt-gen-builtin-defs ()
  "Generate builtin definitions.

Writes outputs to `ELISP_AUTOFMT_OUTPUT'."
  (elisp-autofmt--cache-api-generate-for-builtins (getenv "ELISP_AUTOFMT_OUTPUT")))

(defun elisp-autofmt-gen-package-defs ()
  "Generate builtin definitions.

Uses package from environment variable `ELISP_AUTOFMT_PACKAGE'.
Writes outputs to environment variable `ELISP_AUTOFMT_OUTPUT'."
  (elisp-autofmt--cache-api-generate-for-package
    (getenv "ELISP_AUTOFMT_OUTPUT")
    (getenv "ELISP_AUTOFMT_PACKAGE")
    nil))

(defun elisp-autofmt--cache-api-ensure-cache-for-emacs (use-external-emacs)
  "Ensure cache exists.

Call an external Emacs when USE-EXTERNAL-EMACS is non-nil."
  (let*
    (
      ;; Emacs binary location.
      (filename (expand-file-name invocation-name invocation-directory))
      (filename-cache-name-only (elisp-autofmt--cache-api-encode-name filename))
      (filename-cache-name-full
        (file-name-concat elisp-autofmt-cache-directory filename-cache-name-only)))
    (when
      (or
        (not (file-exists-p filename-cache-name-full))
        (elisp-autofmt--cache-api-file-is-older filename-cache-name-full filename))

      (cond
        (use-external-emacs
          (let
            (
              (process-environment
                (cons
                  (concat "ELISP_AUTOFMT_OUTPUT=" filename-cache-name-full)
                  process-environment)))
            (call-process
              filename
              nil
              nil
              nil

              ;; args.
              "--batch"
              "-l"
              elisp-autofmt--this-file
              "--eval"
              "-f"
              "elisp-autofmt-gen-builtin-defs")))
        (t
          (elisp-autofmt--cache-api-generate-for-builtins filename-cache-name-full))))
    filename-cache-name-only))

(defun elisp-autofmt--cache-api-ensure-cache-for-package (package-id skip-require)
  "Ensure cache for PACKAGE-ID is up to date in CACHE-DIR.

When SKIP-REQUIRE is set, don't require the package."
  (let ((package-sym (intern package-id)))

    (when
      (cond
        (skip-require
          t)
        ((with-demoted-errors "%S" (require package-sym) t)
          t)
        (t
          (message "Unable to load %s" package-id)
          nil))

      (let*
        (
          (filename (find-library-name package-id))
          (filename-cache-name-only (elisp-autofmt--cache-api-encode-name filename))
          (filename-cache-name-full
            (file-name-concat elisp-autofmt-cache-directory filename-cache-name-only)))

        ;; Ensure the cache is newer than it's source.
        (when
          (or
            (not (file-exists-p filename-cache-name-full))
            (elisp-autofmt--cache-api-file-is-older filename-cache-name-full filename))
          (elisp-autofmt--cache-api-generate-for-package
            filename-cache-name-full
            package-id
            skip-require))
        filename-cache-name-only))))

(defun elisp-autofmt--cache-api-cache-update ()
  "Ensure packages are up to date."
  (elisp-autofmt--cache-api-directory-ensure)
  (let ((cache-files (list)))
    (push (elisp-autofmt--cache-api-ensure-cache-for-emacs t) cache-files)
    (dolist (package-id elisp-autofmt-cache-packages-default)
      (push (elisp-autofmt--cache-api-ensure-cache-for-package package-id t) cache-files))
    cache-files))


;; ---------------------------------------------------------------------------
;; Internal Functions

(defun elisp-autofmt--replace-buffer-contents-with-fastpath (buf)
  "Replace buffer contents with BUF, fast-path when undo is disabled.

Useful for fast operation, especially for automated conversion or tests."
  (let
    (
      (is-beg (eq (point) (point-min)))
      (is-end (eq (point) (point-max))))
    (cond
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
        (replace-buffer-contents buf)))))

(defun elisp-autofmt--region-impl (stdout-buffer stderr-buffer &optional assume-file-name)
  "Auto format the current region using temporary STDOUT-BUFFER & STDERR-BUFFER.
Optional argument ASSUME-FILE-NAME overrides the file name used for this buffer."

  ;; TODO, add support for auto-formatting a sub-region,
  ;; until this is supported keep this private.
  (interactive
    (cond
      ((use-region-p)
        (list (region-beginning) (region-end)))
      (t
        (list (point) (point)))))

  (unless assume-file-name
    (setq assume-file-name buffer-file-name))

  (let*
    (
      (stderr-as-string nil)
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
            (elisp-autofmt--cache-api-cache-update))
          (t
            nil)))

      (command-with-args
        (append
          (list
            (or elisp-autofmt-python-bin "python3")
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
            ;; Not 0 or 1.
            "--exit-code=2")

          ;; Optionally read in definitions.
          (cond
            (elisp-autofmt-use-function-defs
              (list
                (concat
                  "--fmt-defs-dir="
                  (convert-standard-filename (expand-file-name elisp-autofmt-cache-directory)))
                (concat "--fmt-defs=" (mapconcat 'identity cache-defs ":"))))
            (t
              (list))))))

    (let
      (
        (proc
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
              (message "elisp-autofmt: error sending input (%s)" pipe-err-as-string))
            (when stderr-as-string
              (message "elisp-autofmt: error output\n%s" stderr-as-string))

            (message
              "elisp-autofmt: Command %S failed with exit code %d!"
              command-with-args
              exit-code)
            nil)
          (t
            (elisp-autofmt--replace-buffer-contents-with-fastpath stdout-buffer)))))))

(defun elisp-autofmt--region (&optional assume-file-name)
  "Auto format the current region.
Optional argument ASSUME-FILE-NAME overrides the file name used for this buffer."
  (let
    (
      (stdout-buffer nil)
      (stderr-buffer nil)
      (this-buffer (current-buffer)))
    (with-temp-buffer
      (setq stdout-buffer (current-buffer))
      (with-temp-buffer
        (setq stderr-buffer (current-buffer))
        (with-current-buffer this-buffer
          (elisp-autofmt--region-impl stdout-buffer stderr-buffer assume-file-name))))))

(defun elisp-autofmt--buffer-impl (buf)
  "Auto-format the entire buffer BUF."
  (with-current-buffer buf (elisp-autofmt--region)))

(defun elisp-autofmt--buffer-format-for-save-hook ()
  "The hook to run on buffer saving to format the buffer."
  ;; Demote errors as this is user configurable, we can't be sure it wont error.
  (when (with-demoted-errors "elisp-autofmt: Error %S" (funcall elisp-autofmt-on-save-p))
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
(defun elisp-autofmt-buffer ()
  "Auto format the current buffer."
  (elisp-autofmt--buffer-impl (current-buffer)))

;;;###autoload
(defun elisp-autofmt-check-elisp-autofmt-exists ()
  "Return non-nil when `.elisp-autofmt' is found in a parent directory."
  (let ((cfg (locate-dominating-file (file-name-directory buffer-file-name) ".elisp-autofmt")))
    (not (null cfg))))

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
;;; elisp-autofmt.el ends here
