#!/usr/bin/env python3
# GPL License, Version 3.0 or later

import os
import subprocess
import tempfile
import unittest

from typing import (
    Tuple,
)

# import unittest.util
# unittest.util._MAX_LENGTH = 1000000

# When investigating problems, set this
TEMP_DIR_OVERRIDE = ""

THIS_DIR = os.path.normpath(os.path.join(os.path.dirname(os.path.abspath(__file__))))
BASE_DIR = os.path.normpath(os.path.join(THIS_DIR, '..'))

EMACS_BIN = "emacs"


def emacs_elisp_autofmt_str_as_str(
        input_str: str,
        fill_column: int,
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
    ) -> None:
        self.maxDiff = None

        import difflib
        code_result, stdout, stderr = emacs_elisp_autofmt_str_as_str(code_format, fill_column)
        diff_output = "\n".join(
            difflib.unified_diff(
                code_expect.splitlines(),
                code_result.splitlines(),
            )
        )
        self.assertEqual("", diff_output)
        self.assertEqual(stdout, stdout_expect)
        self.assertEqual(stderr, stderr_expect)


class CompareFormat(ELispAutoFormat):
    """
    Sub-classes must have a ``code_format`` & ``code_expect`` property.
    """

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


if __name__ == '__main__':
    unittest.main(exit=False)
