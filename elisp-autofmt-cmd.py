#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later

'''
This program is a command line interface to elisp-autofmt.
This uses EMACS to ensure formatting includes EMACS built-in definitions.

Printing the formatted result to the STDOUT is also supported,
making this useful as a quick way to preview.
'''

import argparse
import os
import subprocess
import sys

from typing import (
    Tuple,
    Sequence,
)

__all__ = (
    'main',
)

THIS_DIR = os.path.normpath(os.path.join(os.path.dirname(os.path.abspath(__file__))))


def colorize_lisp(text: str, error_on_no_color: bool) -> str:
    '''
    Syntax highlight ``text``.
    '''
    try:
        import pygments
        del pygments
    except ImportError:
        if error_on_no_color:
            sys.stderr.write('Python module "pygments" not found, install this or remove "--color=always" argument!')
            sys.exit(1)
        return text

    from pygments import highlight
    from pygments.lexers import get_lexer_by_name
    from pygments.formatters import Terminal256Formatter

    return highlight(
        text,
        lexer=get_lexer_by_name('emacs-lisp'),
        formatter=Terminal256Formatter(style='monokai'),
    )


def py_to_lisp_bool(val: bool) -> str:
    '''
    Represent val as a LISP boolean.
    '''
    return 't' if val else 'nil'


def emacs_elisp_autofmt_file_as_str(
        emacs_bin: str,
        filepaths: Sequence[str],
        to_file: bool,
) -> Tuple[int, bytes, bytes]:
    '''
    Take a path and return a string representing the formatted text.
    Or write into ``filepaths`` when ``to_file`` is True.
    '''
    cmd = (
        emacs_bin,
        '--batch',
        '-l', os.path.join(THIS_DIR, 'elisp-autofmt.el'),
        *filepaths,
        '--eval', f'''
(dolist (buf (buffer-list))
  (with-current-buffer buf
    (when (buffer-file-name)
      (setq buffer-undo-list t) ;; Disable undo.
      (setq elisp-autofmt-python-bin (getenv "PYTHON_BIN"))
      (cond
        ({py_to_lisp_bool(to_file):s}
          (princ buffer-file-name)
          (princ "\n")
          (elisp-autofmt-buffer-to-file))
        (t
          (elisp-autofmt-buffer)
          (princ (buffer-substring-no-properties (point-min) (point-max))))))))''',
    )

    env = os.environ.copy()
    env['PYTHON_BIN'] = sys.executable

    completed_proc = subprocess.run(
        cmd,
        capture_output=not to_file,
        check=False,
    )
    return (
        completed_proc.returncode,
        b'' if to_file else completed_proc.stdout,
        b'' if to_file else completed_proc.stderr,
    )


def argparse_create() -> argparse.ArgumentParser:
    '''
    Create the argument parser used to format from the command line.
    '''

    # When `--help` or no arguments are given, print this help.
    usage_text = 'Format emacs-lisp files in-place or to the standard output.'

    epilog = __doc__

    parser = argparse.ArgumentParser(description=usage_text, epilog=epilog)

    parser.add_argument(
        '--emacs-bin',
        dest='emacs_bin',
        metavar='EMACS_BIN',
        default='emacs',
        type=str,
        required=False,
        help='The path used to call EMACS.',
    )

    parser.add_argument(
        '--stdout',
        dest='use_stdout',
        default=False,
        action='store_true',
        required=False,
        help='Use the stdout to output the file contents instead of writing to the file name passed in.',
    )

    color_choices = ('auto', 'always', 'never')
    parser.add_argument(
        '--color',
        dest='color',
        choices=color_choices,
        default=color_choices[0],
        required=False,
        metavar='COLOR',
        help='Use color output {:s}.'.format(repr(color_choices)),
    )

    parser.add_argument(
        'files',
        nargs=argparse.REMAINDER,
        help='All trailing arguments are treated as file paths to format.'
    )
    return parser


def main() -> None:
    '''
    Handle command line arguments for formatting ELISP files.
    '''
    args = argparse_create().parse_args()
    if not args.files:
        print('No files passed in for formatting, see "--help" for details.')
        return

    to_file = not args.use_stdout
    returncode, stdout, stderr = emacs_elisp_autofmt_file_as_str(args.emacs_bin, args.files, to_file)
    if returncode != 0:
        sys.stderr.write(stderr.decode('utf-8'))
        return
    if stderr:
        sys.stderr.write(stderr.decode('utf-8'))

    if not to_file:
        output = stdout.decode('utf-8')

        if args.color == 'never':
            use_color = False
            error_on_no_color = False
        elif args.color == 'auto':
            use_color = True
            if to_file:
                use_color = False
            if not sys.stdout.isatty():
                use_color = False
            error_on_no_color = False
        elif args.color == 'always':
            use_color = True
            error_on_no_color = True
        else:
            # Internal error, should not happen.
            assert False

        # Use `pygments` if available.
        if use_color:
            output = colorize_lisp(output, error_on_no_color)

        sys.stdout.write(output)


if __name__ == '__main__':
    main()
