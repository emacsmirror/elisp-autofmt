;;; elisp-autofmt.el --- Emacs lisp auto-format -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-2.0-or-later
;; Copyright (C) 2019-2022  Campbell Barton

;; Author: Campbell Barton <ideasman42@gmail.com>

;; URL: https://gitlab.com/ideasman42/emacs-elisp-autofmt
;; Version: 0.1
;; Package-Requires: ((emacs "26.2"))

;;; Commentary:

;; Auto format emacs-lisp code on save.

;;; Usage

;; (elisp-autofmt-buffer) ; Auto-format the current buffer.
;;

;;; Code:

;; Public variables.

(defgroup elisp-autofmt nil "Configure emacs-lisp auto-formatting." :group 'tools)

(defcustom elisp-autofmt-empty-line-max 2 "The maximum number of blank lines to keep." :type 'int)

(defcustom elisp-autofmt-use-function-defs nil
  "When non nil, generate function definitions for the auto-formatter to use.

Can be slow!"
  :type 'boolean)

(defcustom elisp-autofmt-python-bin nil
  "The Python binary to call when running auto-formatting."
  :type 'string)

;; Internal variables.

;; Run this command to format.
(defvar elisp-autofmt--bin)
(setq elisp-autofmt--bin (file-name-sans-extension load-file-name))


;; ---------------------------------------------------------------------------
;; Internal Functions

(defun elisp-autofmt--generate-defs (temp-file-cfg)
  "Generate a definition list at path TEMP-FILE-CFG."
  (with-temp-file temp-file-cfg
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
      (sentinel-called nil)

      (temp-file-cfg
        (cond
          (elisp-autofmt-use-function-defs
            (make-temp-file "elisp-autofmt_defs" nil ".py"))
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
              (list (format "--fmt-defs=%s" temp-file-cfg)))
            (t
              (list))))))

    ;; Write a configuration file.
    (when elisp-autofmt-use-function-defs
      (setq (elisp-autofmt--generate-defs)))

    (let
      (
        (proc
          (make-process
            :name "elisp-autofmt"
            :buffer stdout-buffer
            :stderr stderr-buffer
            :connection-type 'pipe
            :command command-with-args
            :coding (cons default-buffer-file-coding-system default-buffer-file-coding-system)
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

      (process-send-region proc (point-min) (point-max))
      (process-send-eof proc)

      (while (not sentinel-called)
        (accept-process-output))
      (set-process-sentinel proc #'ignore)

      (let ((exit-code (process-exit-status proc)))
        (cond
          ((or (not (eq exit-code 2)) stderr-as-string)
            (unless stderr-as-string
              (message "elisp-autofmt: error output\n%s" stderr-as-string))
            (message
              "elisp-autofmt: Command %S failed with exit code %d!"
              command-with-args
              exit-code)
            nil)
          (t
            (replace-buffer-contents stdout-buffer)))))

    ;; Cleanup.
    (when elisp-autofmt-use-function-defs
      (delete-file temp-file-cfg))))

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


;; ---------------------------------------------------------------------------
;; Public Functions

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
      (let ((cfg (locate-dominating-file (file-name-directory buffer-file-name) ".elisp-autofmt")))
        (when (or cfg force)
          (elisp-autofmt-buffer)))
      ;; Continue to save.
      nil)
    nil
    ;; Buffer local hook.
    t))

(provide 'elisp-autofmt)

;;; elisp-autofmt.el ends here
