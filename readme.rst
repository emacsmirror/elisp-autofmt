#############
Emacs AutoFmt
#############

This is a utility to auto-format Emacs lisp.

This is a command line tool which requires Python3.8 as well as
an Emacs utility to run this tool on saving.

Projects using this:

- `bookmark-in-project <https://codeberg.org/ideasman42/emacs-bookmark-in-project>`__
- `counsel-at-point <https://codeberg.org/ideasman42/emacs-counsel-at-point>`__
- `cycle-at-point <https://codeberg.org/ideasman42/emacs-cycle-at-point>`__
- `default-font-presets <https://codeberg.org/ideasman42/emacs-default-font-presets>`__
- `diff-ansi <https://codeberg.org/ideasman42/emacs-diff-ansi>`__
- `diff-at-point <https://codeberg.org/ideasman42/emacs-diff-at-point>`__
- `doc-show-inline <https://codeberg.org/ideasman42/emacs-doc-show-inline>`__
- `hl-block-mode <https://codeberg.org/ideasman42/emacs-hl-block-mode>`__
- `hl-prog-extra <https://codeberg.org/ideasman42/emacs-hl-prog-extra>`__
- `idle-highlight-mode <https://codeberg.org/ideasman42/emacs-idle-highlight-mode>`__
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
- Consistent 2 space indentation.
- Keeps blank lines.
- Keeps trailing comments at the end of lines.
- Extracts function arguments from locally defined functions.
- Experimental support for exporting function argument lengths.

  *Currently writes all function information making it slow.*


Usage
=====

The save hook can be enabled in the mode hook.

Since it's likely you will work on code-bases that *don't* have auto-formatting enabled,
this checks for the existence of an ``.elisp-autofmt`` file in the buffers directory (including parent paths).

.. code-block:: elisp

   (add-hook 'emacs-lisp-mode-hook
     (lambda ()
       (require 'elisp-autofmt)
       (elisp-autofmt-save-hook-for-this-buffer)))

.. note::

   ``.elisp-autofmt`` will eventually be used for configuration, for now it should be left empty.


Functions
---------

``(elisp-autofmt-buffer &optional buf)``
   Auto formats the current buffer (or ``buf``).
``(elisp-autofmt-save-hook-for-this-buffer &optional force)``
   Setup auto-formatting for this buffer, optionally you can pass in ``force`` = ``t``
   to enable auto-formatting even when ``.elisp-autofmt`` isn't found.


Customization
-------------

``elisp-autofmt-empty-line-max`` (``2``)
   The maximum number of empty lines to keep.
``elisp-autofmt-use-function-defs``
   When non-nil, use function argument lengths generated from Emacs (default ``nil``).
``elisp-autofmt-python-bin`` (``nil``)
   Optionally set the Python binary, use when Python is not in your ``PATH``.


Installation
============

This can be installed using ``straight``, example:

.. code-block:: elisp

   (use-package elisp-autofmt
     :commands (elisp-autofmt-save-hook-for-this-buffer)
     :hook (emacs-lisp-mode . elisp-autofmt-save-hook-for-this-buffer)

     :straight
     (elisp-autofmt
       :type git
       :host gitlab
       :files (:defaults "elisp-autofmt")
       :repo "ideasman42/emacs-elisp-autofmt"))


Limitations
===========

- Currently only ``utf-8`` encoding is supported.


TODO
====

- Use: ``.elisp-autofmt`` as a configuration file.

- Support conventional ``let`` formatting:

  .. code-block:: elisp

     ;; Support this.
     (let ((foo 1)
           (bar 2))
       *body*)

     ;; As an alternative to this.
     (let
       (
         (foo 1)
         (bar 2))
       *body*)

  *Moving away from the current rule of fixed 2 space indentation.*

- Scan the buffer for functions, only writing function data for functions in use.
- Use the indentation width from Emacs (currently fixed to 2).
