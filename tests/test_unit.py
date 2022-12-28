#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later

import os
import subprocess
import tempfile
import unittest
import shutil

from typing import (
    Tuple,
)

'''
bash -c 'while true; do inotifywait -e close_write ./elisp-autofmt.el ./elisp-autofmt.py ./tests/test_unit.py; tput clear; python tests/test_unit.py CompareFormatSingleExpr; done'
'''

# Un-comment to see full error messages.
# import unittest.util
# unittest.util._MAX_LENGTH = 1000000

# When investigating problems, set this
TEMP_DIR_OVERRIDE = ""

THIS_DIR = os.path.normpath(os.path.join(os.path.dirname(os.path.abspath(__file__))))
BASE_DIR = os.path.normpath(os.path.join(THIS_DIR, '..'))

EMACS_BIN = "emacs"

# Fancy + useful delta output.
USE_DELTA_DIFF = shutil.which("delta") is not None


def emacs_elisp_autofmt_str_as_str(
        input_str: str,
        fill_column: int,
        style: str,
) -> Tuple[str, bytes, bytes]:
    """
    Take a path and return a string representing the formatted text.
    """
    with tempfile.TemporaryDirectory() as temp_dir:
        if TEMP_DIR_OVERRIDE:
            os.makedirs(TEMP_DIR_OVERRIDE, exist_ok=True)
            temp_dir = TEMP_DIR_OVERRIDE

        filepath_temp = os.path.join(temp_dir, "elisp_autofmt_unit_test.el")
        with open(filepath_temp, 'w', encoding='utf-8') as fh:
            fh.write(input_str)
        cmd = (
            EMACS_BIN,
            "--batch",
            "-l", os.path.join(BASE_DIR, "elisp-autofmt.el"),
            filepath_temp,
            "--eval", (
                "(progn"
                # The extension is `.data`, so the mode needs to be activated.
                "(setq buffer-undo-list t)"
                "(setq-local fill-column " + str(fill_column) + ")"
                "(setq elisp-autofmt-style '" + style + ")"
                "(elisp-autofmt-buffer)"
                "(write-region nil nil (buffer-file-name) nil 0))"
            ),
        )

        completed_proc = subprocess.run(cmd, check=True, capture_output=True)
        with open(filepath_temp, 'r', encoding='utf-8') as fh:
            return (fh.read(), completed_proc.stdout, completed_proc.stderr)


class ELispAutoFormat(unittest.TestCase):
    def compare(
            self, code_format: str,
            code_expect: str,
            stdout_expect: bytes = b'',
            stderr_expect: bytes = b'',
            fill_column: int = 79,
            style: str = 'fixed',
    ) -> None:
        self.maxDiff = None

        import difflib
        code_result, stdout, stderr = emacs_elisp_autofmt_str_as_str(code_format, fill_column, style)
        diff_output = "\n".join(
            difflib.unified_diff(
                code_expect.splitlines(),
                code_result.splitlines(),
                fromfile='expected.el',
                tofile='actual.el',
            )
        )

        if diff_output != "":
            if USE_DELTA_DIFF:
                completed_proc = subprocess.run(
                    ("delta", "--side-by-side"),
                    check=True,
                    capture_output=True,
                    input=diff_output.encode('utf-8'),
                )
                raise self.failureException(
                    "Unexpected difference:\n" + completed_proc.stdout.decode('utf-8'),
                )
            else:
                self.assertEqual("", diff_output)

        self.assertEqual(stdout, stdout_expect)

        if not stderr_expect and stderr:
            raise self.failureException(
                "Expected empty stderr:\n" + stderr.decode('utf-8'),
            )
        else:
            self.assertEqual(stderr, stderr_expect)


class CompareFormatNOP(ELispAutoFormat):

    def test_empty(self) -> None:
        self.compare(
            code_format="",
            code_expect="\n",
        )

    def test_empty_excess_newline(self) -> None:
        self.compare(
            code_format="\n\n\n\n",
            code_expect="\n\n",
        )


class CompareFormatEmpty(ELispAutoFormat):

    def test_sexpr(self) -> None:
        self.compare(
            code_format="()",
            code_expect="()\n",
        )

    def test_vector(self) -> None:
        self.compare(
            code_format="[]",
            code_expect="[]\n",
        )

    def test_string(self) -> None:
        self.compare(
            code_format="\"\"",
            code_expect="\"\"\n",
        )

    def test_comment(self) -> None:
        self.compare(
            code_format=";",
            code_expect=";\n",
        )


class CompareFormatSingle(ELispAutoFormat):

    def test_comment(self) -> None:
        self.compare(
            code_format=";; Test.",
            code_expect=";; Test.\n",
        )

    def test_sexpr(self) -> None:
        self.compare(
            code_format="(x)",
            code_expect="(x)\n",
        )

    def test_char(self) -> None:
        self.compare(
            code_format="?x",
            code_expect="?x\n",
        )

    def test_string(self) -> None:
        self.compare(
            code_format="\"x\"",
            code_expect="\"x\"\n",
        )


class CompareFormatSingleExpr(ELispAutoFormat):

    def test_if_simple(self) -> None:
        self.compare(
            code_format="(if t t t)",
            code_expect=(
                "(if t\n"
                "  t\n"
                "  t)\n"
            ))

    def test_apply_no_line_break(self) -> None:
        self.compare(
            code_format=";; Test\n(apply 'test 8)\n",
            code_expect=";; Test\n(apply 'test 8)\n",
        )

    def test_apply_line_break(self) -> None:
        self.compare(
            code_format="(defvar 'test 8)\n",
            code_expect=(
                "(defvar 'test\n"
                "  8)\n"
            ),
            fill_column=14,
        )

    def test_setq_multi(self) -> None:
        self.compare(
            code_format="(setq a b c d e f)\n",
            code_expect=("(setq\n"
                         "  a b\n"
                         "  c d\n"
                         "  e f)\n"),
        )

    def test_setq_single(self) -> None:
        self.compare(
            code_format="(setq a b)\n",
            code_expect=("(setq a b)\n"),
        )


class CompareFormatExpectError(ELispAutoFormat):

    def test_unbalanced_open(self) -> None:
        self.compare(
            code_format="(\n",
            code_expect="(\n",
            stderr_expect=(
                b'elisp-autofmt: error code 1, output\n'
                b'Error: unbalanced S-expressions at file-end, found 1 levels, expected 0\n'
                b'\n'
            ),
        )

    def test_unbalanced_close(self) -> None:
        self.compare(
            code_format=")\n",
            code_expect=")\n",
            stderr_expect=(
                b'elisp-autofmt: error code 1, output\n'
                b'Error: additional closing brackets, line 0\n'
                b'\n'
            ),
        )

    def test_unbalanced_types_parens(self) -> None:
        self.compare(
            code_format="(]\n",
            code_expect="(]\n",
            stderr_expect=(
                b'elisp-autofmt: error code 1, output\n'
                b'Error: closing bracket "]" line 0, unmatched bracket types, expected "]"\n'
                b'\n'
            ),
        )

    def test_unbalanced_types_vector(self) -> None:
        self.compare(
            code_format="[)\n",
            code_expect="[)\n",
            stderr_expect=(
                b'elisp-autofmt: error code 1, output\n'
                b'Error: closing bracket ")" line 0, unmatched bracket types, expected ")"\n'
                b'\n'
            ),
        )

    def test_unbalanced_quote(self) -> None:
        self.compare(
            code_format="\"\n",
            code_expect="\"\n",
            stderr_expect=(
                b'elisp-autofmt: error code 1, output\n'
                b'Error: parsing string at line 1\n'
                b'\n'
            ),
        )


class CompareFormatNative(ELispAutoFormat):

    def test_if_simple(self) -> None:
        self.compare(
            code_format="(if a b c)\n",
            code_expect=(
                "(if a\n"
                "    b\n"
                "  c)\n"
            ),
            style='native',
        )

    def test_if_complex(self) -> None:
        self.compare(
            code_format="(if a (progn b c d) (progn e f g))\n",
            code_expect=(
                "(if a\n"
                "    (progn\n"
                "      b\n"
                "      c\n"
                "      d)\n"
                "  (progn\n"
                "    e\n"
                "    f\n"
                "    g))\n"
            ),
            style='native',
        )

    def test_while_multiline(self) -> None:
        self.compare(
            code_format="(while (progn a b c) (foo))\n",
            code_expect=(
                "(while (progn\n"
                "         a\n"
                "         b\n"
                "         c)\n"
                "  (foo))\n"
            ),
            style='native',
        )

    def test_while_multiline_with_comments(self) -> None:
        self.compare(
            code_format=("(while\n"
                         ";; A.\n"
                         "(progn a b c)\n"
                         ";; B.\n"
                         "(foo))\n"),
            code_expect=(
                "(while\n"
                "    ;; A.\n"
                "    (progn\n"
                "      a\n"
                "      b\n"
                "      c)\n"
                "  ;; B.\n"
                "  (foo))\n"
            ),
            style='native',
        )

    def test_and_multiline(self) -> None:
        self.compare(
            code_format="(and a b c d e f g)\n",
            code_expect=(
                "(and a\n"
                "     b\n"
                "     c\n"
                "     d\n"
                "     e\n"
                "     f\n"
                "     g)\n"
            ),
            fill_column=14,
            style='native',
        )


if __name__ == '__main__':
    unittest.main(exit=False)
