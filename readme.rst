
Emacs AutoFmt
=============

This is a utility to auto-format Emacs lisp.

This is a command line tool which requires Python3.8 as well as
an Emacs utility to run this tool on saving.

Projects using this:

- `diff-at-point <https://gitlab.com/ideasman42/emacs-diff-at-point>`__
- `hl-block-mode <https://gitlab.com/ideasman42/emacs-hl-block-mode>`__
- `run-stuff <https://gitlab.com/ideasman42/emacs-run-stuff>`__
- `scoll-on-drag <https://gitlab.com/ideasman42/emacs-scroll-on-drag>`__
- `undo-fu <https://gitlab.com/ideasman42/emacs-undo-fu>`__

*Currently my projects, just to give examples of how it works.*


Motivation
----------

This tool removes the need to manually format and indent code,
it can be useful to re-arrange code without the need to manually reformat it.


Features
--------

- Enforces maximum line width (using the fill column).
- Consistent 2 space indentation.
- Keeps blank lines.
- Keeps trailing commends at the end of lines.
- Experimental support for exporting function argument lengths.

  *Currently writes all function informationg making it slow.*


Usage
-----

The save hook can be enabled in the mode hook.

.. code-block:: elisp

   (add-hook 'emacs-lisp-mode-hook
     (lambda ()
       (require 'elisp-autofmt)
       (elisp-autofmt-save-hook-for-this-buffer)))


Customization
-------------

``elisp-autofmt-empty-line-max``
   The maximum number of empty lines to keep (default ``2``).
``elisp-autofmt-use-function-defs``
   When non-nil, use function argument lengths generated from Emacs (default ``nil``).

TODO
----

- Add a configuration file, e.g: ``.elisp-autofmt``
  which can be used to detect if auto-formatting should be used.

  Allowing auto-formatting to be selectively enabled on a per-project basis.
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
- Support un-escaped character literals.
