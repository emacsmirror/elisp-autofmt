#############
Emacs AutoFmt
#############

This is a package to auto-format Emacs lisp.

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
- Consistent 2 space indentation.
- Keeps blank lines.
- Keeps trailing comments at the end of lines.
- Extracts function arguments from locally defined functions,
  with support for including definitions from external files.


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


Requirements
------------

- Emacs 27.2.
- Python 3.10.


Performance
-----------

The first re-format may be slow as there are likely to be a large number of changes be applied back into Emacs.
Running again is typically fast enough to be enabled on save without noticeable interruption.


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
``elisp-autofmt-python-bin`` (``nil``)
   Optionally set the Python binary, use when Python is not in your ``PATH``.


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

This can be installed using ``straight``, example:

.. code-block:: elisp

   (use-package elisp-autofmt
     :commands (elisp-autofmt-mode)
     :hook (emacs-lisp-mode . elisp-autofmt-mode)

     :straight
     (elisp-autofmt
       :files (:defaults "elisp-autofmt.py" "elisp-autofmt.overrides.json")
       :host nil
       :type git
       :repo "https://codeberg.org/ideasman42/emacs-elisp-autofmt.git"))


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

- Use the indentation width from Emacs (currently fixed to 2).
