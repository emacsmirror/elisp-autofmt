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

from collections.abc import (
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


def emacs_elisp_autofmt_file_as_str(
        emacs_bin: str,
        filepaths: Sequence[str],
        use_stdout: bool,
) -> tuple[int, bytes, bytes]:
    '''
    Take a path and return a string representing the formatted text.
    Or write into ``filepaths`` when ``use_stdout`` is False.
    '''
    cmd = (
        emacs_bin,
        '--batch',
        '-l', os.path.join(THIS_DIR, 'elisp-autofmt.el'),
        *filepaths,
        '--eval', '''
(progn
 (setq elisp-autofmt-python-bin (getenv "PYTHON_BIN"))
 (let ((use-stdout (string-equal "1" (getenv "USE_STDOUT"))))
   (dolist (buf (buffer-list))
     (with-current-buffer buf
       (when (buffer-file-name)
         (setq buffer-undo-list t) ;; Disable undo.
         (cond
          (use-stdout
           (elisp-autofmt-buffer)
           (princ (buffer-substring-no-properties (point-min) (point-max))))
          (t
           (princ buffer-file-name)
           (princ "\n")
           (elisp-autofmt-buffer-to-file))))))))
''',
    )
    env = os.environ.copy()
    env['PYTHON_BIN'] = sys.executable
    env['USE_STDOUT'] = str(int(use_stdout))

    completed_proc = subprocess.run(
        cmd,
        capture_output=use_stdout,
        check=False,
        env=env,
    )
    return (
        completed_proc.returncode,
        completed_proc.stdout if use_stdout else b'',
        completed_proc.stderr if use_stdout else b'',
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

    use_stdout = args.use_stdout

    returncode, stdout, stderr = emacs_elisp_autofmt_file_as_str(args.emacs_bin, args.files, use_stdout)
    if returncode != 0:
        sys.stderr.write(stderr.decode('utf-8'))
        return
    if stderr:
        sys.stderr.write(stderr.decode('utf-8'))

    if use_stdout:
        output = stdout.decode('utf-8')
        if args.color == 'never':
            use_color = False
            error_on_no_color = False
        elif args.color == 'auto':
            use_color = True
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
