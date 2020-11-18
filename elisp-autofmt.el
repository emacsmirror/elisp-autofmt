;;; elisp-autofmt.el --- Emacs lisp auto-format -*- lexical-binding: t -*-

;; Copyright (C) 2019  Campbell Barton

;; Author: Campbell Barton <ideasman42@gmail.com>

;; URL: https://gitlab.com/ideasman42/emacs-elisp-autofmt
;; Version: 0.1
;; Package-Requires: ((emacs "26.2"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Interactive scrolling which can be cancelled by pressing escape.

;;; Usage

;; (elisp-autofmt-format) ; Auto-format the current buffer.
;;

;;; Code:

;; Public variables.

(defcustom elisp-autofmt-empty-line-max 2
  "The maximum number of blank lines to keep."
  :group 'elisp-autofmt
  :type 'int)

(defcustom elisp-autofmt-use-function-defs nil
  "When non nil, generate function definitions for the auto-formatter to use.

Can be slow!"
  :group 'elisp-autofmt
  :type 'boolean)

;; Internal variables.

;; Run this command to format.
(defvar elisp-autofmt--bin)
(setq elisp-autofmt--bin (file-name-sans-extension load-file-name))

;; TODO, add support for auto-formatting a sub-region, until this is supported keep this private.
(defun elisp-autofmt--region (&optional assume-file-name)
  "Auto format the current region.
Optional argument ASSUME-FILE-NAME overrides the file name used for this buffer."
  (interactive
    (if (use-region-p)
      (list (region-beginning) (region-end))
      (list (point) (point))))

  (unless assume-file-name
    (setq assume-file-name buffer-file-name))

  (let
    (
      (this-buffer (current-buffer))
      (temp-buffer (generate-new-buffer " *lisp_fmt-format-temp*"))
      ;; Use for format output or stderr in the case of failure.
      (temp-file (make-temp-file "lisp_fmt_format" nil ".el"))
      (temp-file-cfg
        (if elisp-autofmt-use-function-defs
          (make-temp-file "lisp_fmt_defs" nil ".py")
          nil))
      ;; Always use ‘utf-8-unix’ and ignore the buffer coding system.
      (default-process-coding-system '(utf-8-unix . utf-8-unix)))

    ;; Write a config file.
    (when elisp-autofmt-use-function-defs
      (with-temp-file
        temp-file-cfg
        (insert
          ;; Declare singletons.
          "many = object()\n"
          ;; For our purposes this is the same as many.
          "unevalled = object()\n"
          ;; Declare it as a variable so it can be imported.
          "fn_arity = {\n")
        (mapatoms
          (lambda (x)
            (when
              (and
                ;; Does x name a function?
                (fboundp x)
                ;; Extra check.
                (or (functionp x) (macrop x))
                ;; Is it non-interactive?
                (not (commandp (symbol-function x)))
                ;; Is it built-in?
                ;; (symbol-function x)
                ;; (subrp (symbol-function x))
                ;;
                )
              ;; (message "%S %s" (fboundp x) (symbol-name x))
              (let
                (
                  (arity
                    (condition-case _err
                      (func-arity x)
                      (error nil))))
                (when arity
                  (let
                    (
                      (arity-min (car arity))
                      (arity-max (cdr arity)))
                    (insert
                      ;; Key.
                      "r\"\"\"" (symbol-name x) "\"\"\": "
                      ;; Value.
                      (format "(%S, %S),\n" arity-min arity-max))))))))
        (insert "}")))

    (condition-case err
      (unwind-protect
        (let
          (
            (status
              (progn
                (apply #'call-process-region
                  nil nil "python3" nil
                  ;; stdout is a temp buffer, stderr is file.
                  `(,temp-buffer ,temp-file) nil
                  ;; Command line arguments.
                  `
                  (,elisp-autofmt--bin
                    ;; No messages.
                    "--quiet"
                    ;; Don't use the file, use the stdin instead.
                    "--stdin"
                    ;; Optionally read in definitions.
                    ,@
                    (when elisp-autofmt-use-function-defs
                      '(format "--fmt-defs=%s" temp-file-cfg))
                    ;; Follow the 'fill-column' setting.
                    ,(format "--fmt-fill-column=%d" fill-column)
                    ,(format "--fmt-empty-lines=%d" elisp-autofmt-empty-line-max)
                    ;; For the init file, use trailing parens.
                    ,temp-file))))
            (stderr
              (with-temp-buffer
                (unless (zerop (cadr (insert-file-contents temp-file)))
                  (insert ":\n"))
                (buffer-substring-no-properties (point-min) (point-max)))))
          (cond
            ((stringp status)
              (error "Command: lisp_fmt-format killed by signal %s%s" status stderr))
            ((not (zerop status))
              (error "Command: lisp_fmt-format failed with code %d%s" status stderr))
            (t
              ;; Include the stdout as a message, useful to check on how the program runs.
              (let
                (
                  (stdout
                    (with-current-buffer temp-buffer
                      (buffer-substring-no-properties (point-min) (point-max)))))
                (unless (string-equal stdout "")
                  (message "%s" stdout)))))

          ;; Load the temp file into a temp buffer and replace this-buffers contents.
          (with-temp-buffer
            (insert-file-contents temp-file)
            (let ((temp-buffer (current-buffer)))
              (with-current-buffer this-buffer (replace-buffer-contents temp-buffer))))))
      ;; Report error.
      (error (message "%s" (error-message-string err))))

    ;; Cleanup.
    (delete-file temp-file)
    (when elisp-autofmt-use-function-defs
      (delete-file temp-file-cfg))
    (when (buffer-name temp-buffer)
      (kill-buffer temp-buffer))))

;;;###autoload
(defun elisp-autofmt-buffer (&optional buf)
  "Auto-format the entire buffer.

Optional argument BUF the buffer to format, otherwise use the current buffer."
  (with-current-buffer (or buf (current-buffer)) (elisp-autofmt--region)))

;;;###autoload
(defun elisp-autofmt-save-hook-for-this-buffer (&optional force)
  "Setup an auto-format save hook for this buffer.

Optional argument FORCE auto-formats the buffer
even when `.elisp-autofmt' isn't in any of the buffers parent directories."
  (add-hook 'before-save-hook
    (lambda ()
      (let
        ((cfg (locate-dominating-file (file-name-directory buffer-file-name) ".elisp-autofmt")))
        (when (or cfg force)
          (elisp-autofmt-buffer)))
      ;; Continue to save.
      nil)
    nil
    ;; Buffer local hook.
    t))

(provide 'elisp-autofmt)

;;; elisp-autofmt.el ends here
