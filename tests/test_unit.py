#!/usr/bin/env python3
# GPL License, Version 3.0 or later

import os
import subprocess
import tempfile
import unittest

# When investigating problems, set this
TEMP_DIR_OVERRIDE = "/tmp/out"
# TEMP_DIR_OVERRIDE = ""

THIS_DIR = os.path.normpath(os.path.join(os.path.dirname(os.path.abspath(__file__))))
BASE_DIR = os.path.normpath(os.path.join(THIS_DIR, '..'))

EMACS_BIN = "emacs"


def emacs_elisp_autofmt_str_as_str(input_str: str, fill_column: int) -> str:
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

        subprocess.check_call(cmd)
        with open(filepath_temp, 'r', encoding='utf-8') as fh:
            return fh.read()


class MyFullCompareFormat(unittest.TestCase):
    """
    Sub-classes must have a ``code_format`` & ``code_expect`` property.
    """

    def compare(
            self, code_format: str,
            code_expect: str,
            fill_column: int = 79,
    ) -> None:
        self.maxDiff = None

        import difflib
        code_result = emacs_elisp_autofmt_str_as_str(code_format, fill_column)

        diff_output = "\n".join(
            difflib.unified_diff(
                code_result.splitlines(),
                code_expect.splitlines(),
            )
        )
        self.assertEqual("", diff_output)

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


if __name__ == '__main__':
    unittest.main(exit=False)
