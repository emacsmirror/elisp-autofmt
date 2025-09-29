###################
Emacs ELisp AutoFmt
###################

This is a package to auto-format Emacs lisp.

Available via `melpa <https://melpa.org/#/elisp-autofmt>`__.

----

Apply idiomatic formatting to ELisp code,
removing the need to have to concern with the details of white-space, line wrapping & bracket placement.

For a simple example, the following input can be auto formatted as follows.

.. code-block:: elisp

   ( defun mark-word ( &optional arg allow-extend
   ) "Set mark ARG words away from point.
   The place mark goes is the same place \\[forward-word] would
   move to with the same argument.
   Interactively, if this command is repeated
   or (in Transient Mark mode) if the mark is active,
   it marks the next ARG words after the ones already marked."
   ( interactive "P\np") ( cond ( ( and allow-extend (
   or ( and ( eq last-command this-command ) ( mark  t
   ) ) ( region-active-p ) ) ) ( setq arg ( if arg (
   prefix-numeric-value arg ) ( if (< ( mark ) ( point
   ) ) -1 1 ) ) ) ( set-mark ( save-excursion (
   goto-char ( mark ) ) ( forward-word arg ) ( point )
   ) ) ) ( t ( push-mark ( save-excursion (
   forward-word ( prefix-numeric-value arg ) ) ( point
   ) ) nil t ) ) ) )


Formats to this:

.. code-block:: elisp

   (defun mark-word (&optional arg allow-extend)
     "Set mark ARG words away from point.
   The place mark goes is the same place \\[forward-word] would
   move to with the same argument.
   Interactively, if this command is repeated
   or (in Transient Mark mode) if the mark is active,
   it marks the next ARG words after the ones already marked."
     (interactive "P\np")
     (cond
      ((and allow-extend
            (or (and (eq last-command this-command) (mark t))
                (region-active-p)))
       (setq arg
             (if arg
                 (prefix-numeric-value arg)
               (if (< (mark) (point))
                   -1
                 1)))
       (set-mark
        (save-excursion
          (goto-char (mark))
          (forward-word arg)
          (point))))
      (t
       (push-mark
        (save-excursion
          (forward-word (prefix-numeric-value arg))
          (point))
        nil t))))


Examples
========

See: `Projects using elisp-autofmt <emacs-elisp-autofmt/src/branch/main/doc/projects.rst>`_.


Motivation
==========

This tool removes the need to manually format and indent code,
it can be useful to re-arrange code without the need to manually reformat it.


Features
========

- Reliable, tested with Emacs 28 source (over 1.6 million lines of code),
  formatting all files without making functional changes.
- Enforces maximum line width (using the fill column).
- Keeps blank lines.
- Keeps trailing comments at the end of lines (without joining lines which may change their meaning).
- Support for disabling auto-formatting via comments.
- Extracts function arguments from locally defined functions,
  with support for including definitions from external files.
- Parallel computation.


Usage
=====

Format Buffer Command
---------------------

To try out auto-formatting you may wish to run the command directly.

In this case simply run ``elisp-autofmt-buffer`` on the buffer you wish to format.


Minor Mode
----------

The ``elisp-autofmt-mode`` minor mode is intended for developers who work on projects which are entirely auto-formatted.

This will format emacs-lisp buffers on save which is more convenient than having to remember
to run for format command after performing each edit.

Since you may work on code-bases that *don't* have auto-formatting enabled,
the default behavior is to check for the existence of an ``.elisp-autofmt`` file
in the buffers directory (including parent paths).

This behavior can be configured by changing ``elisp-autofmt-on-save-p``.

.. note::

   ``.elisp-autofmt`` will eventually be used for configuration, for now it should be left empty.


Command Line
------------

You may wish to format a directory of files, in this case there is a command line utility: ``elisp-autofmt-cmd.py``
this takes Emacs-Lisp files as an arguments, formatting them and exiting.

This may be preferred if you wish to batch format files without having to load each file into Emacs manually.


Requirements
------------

- Emacs 29.1 (or newer).
- Python 3.9 (or newer).


Commands
--------

``elisp-autofmt-mode``
   Toggle the minor mode which formats upon saving.

``elisp-autofmt-buffer``
   Auto formats the current buffer (doesn't depend on the minor mode).

``elisp-autofmt-region``
   Auto formats the selected region.

``elisp-autofmt-region-dwim``
   Auto formats the selected region or the surrounding multi-line block when there is no active region.


Customization (Style)
---------------------

``elisp-autofmt-style`` (``'native``), added to ``safe-local-variable``.
   Style to use for formatting, currently the options are:

   ``'native``
      Follow Emacs default indentation style.

   ``'fixed``
      Use fixed (2 space) indentation (simple behavior).

      For Emacs to match this formatting set the defaults:

      .. code-block:: elisp

         (setq-local indent-tabs-mode nil)
         (setq-local lisp-indent-function nil)
         (setq-local lisp-indent-offset 2)

``elisp-autofmt-quoted`` (``t``), added to ``safe-local-variable``.
   Format single-quoted S-expressions.

   When nil, single quoted S-expressions keep existing line-breaks and only indentation is performed.

``elisp-autofmt-empty-line-max`` (``2``), added to ``safe-local-variable``.
   The maximum number of empty lines to keep.


Customization (API Definitions)
-------------------------------

``elisp-autofmt-use-function-defs`` (``t``)
   When non-nil, use function information generated from Emacs.
``elisp-autofmt-use-default-override-defs`` (``t``)
   When non-nil, use a preset list of opinionated overrides that adjust the behavior of common functions & macros.
``elisp-autofmt-load-packages-local`` (``nil``), added to ``safe-local-variable``.
   A list of strings representing:

   - Packages to load definitions from (e.g. ``ert``, ``abbrev``).
   - Paths relative to the current file (any string starting with a ``.``),
     e.g. ``"./multi-file-package.el"``.

     Referencing local paths is needed so multi-file packages can be aware of definitions stored elsewhere.

   This variable is marked as *safe* so it can be defined in file/directory locals.
   This example shows it's use in file locals.

   .. code-block:: elisp

      ;; Local variables:
      ;; elisp-autofmt-load-packages-local: ("ert" "./my-relative-file.el")
      ;; end:

``elisp-autofmt-ignore-autoload-packages``
   Auto-loaded packages not to load when generating built-in API definitions.

   *Note that this should not need to be modified for typical use-cases.*


Customization (Integration)
---------------------------

``elisp-autofmt-on-save-p`` (``'elisp-autofmt-check-elisp-autofmt-exists``)
   A symbol referencing the function used to check if the buffer should be formatted on save.
   By default the ``.elisp-autofmt`` file is detected in current & parent directories.
   You may set this to ``'always`` to always format the buffer when ``elisp-autofmt-mode`` is enabled.

``elisp-autofmt-python-bin`` (``nil``)
   Optionally set the Python binary, use when ``python`` is not in your ``PATH``.

``elisp-autofmt-cache-directory`` (``"~/.config/emacs/elisp-autofmt-cache"``)
   The directory where API cache is stored.
``elisp-autofmt-use-diff-range`` nil
   For whole buffer formatting, compute the changed region & only update that.

   *Note that this may be useful for systems where the sub-process overhead is significant.*


Customization (Parallel Computation)
------------------------------------

``elisp-autofmt-parallel-jobs`` (``0``)
   Number of jobs to run in parallel.

   - ``0`` to set this automatically.
   - ``-1`` disables parallel computation.

``elisp-autofmt-parallel-threshold`` (``32768`` 32 kilobytes)
   Buffers below this size will not use parallel computation.

   - ``0`` to use parallel computation for all buffers.

Note that this is disabled on MS-Windows currently until performance issues can be investigated.


Comments
--------

Formatting can be disabled by a single line comment:

.. code-block:: elisp

   ;; format: off
   (progn (this block (will
                       keep wrapping (from
                                      input))))
   ;; format: on

You may also disable wrapping for a single line which doesn't require a paired comment to re-enable:

.. code-block:: elisp

   (let ((var
          ;; format-next-line: off
          (concat
           "lines"
           "kept"
           "separate")))
     (fn var))


Notes:

- This only applies to S-expressions enclosed between the comments,
  be sure to add these comments outside the S-expression which is manually formatted.
- While the line-wrapping from the input is preserved, indentation is still applied.
- Additional space is ignored so both ``;format:off`` and ``;; format:  off`` are valid.
- Space or punctuation after ``on``, ``off`` are allowed, you may wish to note why formatting was disabled.

  .. code-block:: elisp

     ;; format: off. Manually wrap for better visual alignment.


Installation
============

This can be installed with ``use-package``:

.. code-block:: elisp

   (use-package elisp-autofmt
     :commands (elisp-autofmt-mode elisp-autofmt-buffer)
     :hook (emacs-lisp-mode . elisp-autofmt-mode))


Limitations
===========

- Currently only ``utf-8`` encoding is supported.


Compatibility
=============

Since 30.1 ``use-package`` is built into emacs and re-structured.

If you wish to auto-format your ``init.el`` file (or any files with ``use-pacakge``)
``use-package-core`` must be included in ``elisp-autofmt-load-packages-local``.

For example, this may be included at the bottom of your ``init.el``:

.. code-block:: elisp

   ;; Local variables:
   ;; elisp-autofmt-load-packages-local: ("use-package" "use-package-core")
   ;; end:



TODO
====

- Use: ``.elisp-autofmt`` as a configuration file.
- Support indenting with Tabs *(low priority)*.


Development
===========

See: `Hacking elisp-autofmt <emacs-elisp-autofmt/src/branch/main/doc/hacking.rst>`_.
