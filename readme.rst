#############
Emacs AutoFmt
#############

This is a package to auto-format Emacs lisp.

Available via `melpa <https://melpa.org/#/elisp-autofmt>`__.


Examples
========

Projects using this:

- `bookmark-in-project <https://codeberg.org/ideasman42/emacs-bookmark-in-project>`__
- `counsel-at-point <https://codeberg.org/ideasman42/emacs-counsel-at-point>`__
- `cycle-at-point <https://codeberg.org/ideasman42/emacs-cycle-at-point>`__
- `default-font-presets <https://codeberg.org/ideasman42/emacs-default-font-presets>`__
- `diff-ansi <https://codeberg.org/ideasman42/emacs-diff-ansi>`__
- `diff-at-point <https://codeberg.org/ideasman42/emacs-diff-at-point>`__
- `doc-show-inline <https://codeberg.org/ideasman42/emacs-doc-show-inline>`__
- `fancy-compilation <https://codeberg.org/ideasman42/emacs-fancy-compilation>`__
- `hl-block-mode <https://codeberg.org/ideasman42/emacs-hl-block-mode>`__
- `hl-indent-scope <https://codeberg.org/ideasman42/emacs-hl-indent-scope>`__
- `hl-prog-extra <https://codeberg.org/ideasman42/emacs-hl-prog-extra>`__
- `idle-highlight-mode <https://codeberg.org/ideasman42/emacs-idle-highlight-mode>`__
- `jit-lock-stealth-progress <https://codeberg.org/ideasman42/emacs-jit-lock-stealth-progress>`__
- `magit-commit-mark <https://codeberg.org/ideasman42/emacs-magit-commit-mark>`__
- `mode-line-idle <https://codeberg.org/ideasman42/emacs-mode-line-idle>`__
- `py-autopep8 <https://codeberg.org/ideasman42/emacs-py-autopep8>`__
- `recomplete <https://codeberg.org/ideasman42/emacs-recomplete>`__
- `revert-buffer-all <https://codeberg.org/ideasman42/emacs-revert-buffer-all>`__
- `run-stuff <https://codeberg.org/ideasman42/emacs-run-stuff>`__
- `scroll-on-drag <https://codeberg.org/ideasman42/emacs-scroll-on-drag>`__
- `scroll-on-jump <https://codeberg.org/ideasman42/emacs-scroll-on-jump>`__
- `sidecar-locals <https://codeberg.org/ideasman42/emacs-sidecar-locals>`__
- `spatial-navigate <https://codeberg.org/ideasman42/emacs-spatial-navigate>`__
- `spell-fu <https://codeberg.org/ideasman42/emacs-spell-fu>`__
- `undo-fu <https://codeberg.org/ideasman42/emacs-undo-fu>`__
- `undo-fu-session <https://codeberg.org/ideasman42/emacs-undo-fu-session>`__
- `utimeclock <https://codeberg.org/ideasman42/emacs-utimeclock>`__
- `xref-rst <https://codeberg.org/ideasman42/emacs-xref-rst>`__

*Currently my projects, just to give examples of how it works.*


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
- Extracts function arguments from locally defined functions,
  with support for including definitions from external files.
- Parallel computation *(disabled on MS-Windows until performance issues can be investigated).*


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

- Emacs 27.2 (or newer).
- Python 3.8 (or newer).


Commands
--------

``elisp-autofmt-mode``
   Toggle the minor mode which formats upon saving.

``elisp-autofmt-buffer``
   Auto formats the current buffer (doesn't depend on the minor mode).


Customization (Style)
---------------------

``elisp-autofmt-style`` (``'native``)
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

``elisp-autofmt-quoted`` (``t``)
   Format single-quoted S-expressions.

   When nil, single quoted S-expressions keep existing line-breaks and only indentation is performed.

``elisp-autofmt-empty-line-max`` (``2``)
   The maximum number of empty lines to keep.


Customization (Integration)
---------------------------

``elisp-autofmt-on-save-p``
   Function used to check if the buffer should be formatted on save.
   By default the ``.elisp-autofmt`` file is detected in current & parent directories.
   You may set this to ``'always`` to always format the buffer when ``elisp-autofmt-mode`` is enabled.


``elisp-autofmt-python-bin`` (``nil``)
   Optionally set the Python binary, use when ``python`` is not in your ``PATH``.


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


Customization (API Definitions)
-------------------------------

``elisp-autofmt-use-function-defs`` (``t``)
   When non-nil, use function information generated from Emacs.
``elisp-autofmt-use-default-override-defs`` (``t``)
   When non-nil, use a preset list of opinionated overrides that adjust the behavior of common functions & macros.
``elisp-autofmt-load-packages-local``
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


TODO
====

- Use: ``.elisp-autofmt`` as a configuration file.
- Support indenting with Tabs *(low priority)*.
