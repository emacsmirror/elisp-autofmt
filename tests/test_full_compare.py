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

# Re-format the test data, use this when an intentional change in formatting has been made.
UPDATE_TEST_DATA = False


def emacs_elisp_autofmt_file_as_str(filepath: str) -> str:
    """
    Take a path and return a string representing the formatted text.
    """
    cmd = (
        EMACS_BIN,
        "--batch",
        "-l", os.path.join(BASE_DIR, "elisp-autofmt.el"),
        filepath,
        "--eval", (
            "(progn"
            # The extension is `.data`, so the mode needs to be activated.
            "(setq buffer-undo-list t)"
            "(setq elisp-autofmt-style 'fixed)"
            "(elisp-autofmt-buffer)"
            "(write-region nil nil (getenv \"ELISP_AUTOFMT_OUTPUT\") nil 0))"
        ),
    )
    with tempfile.TemporaryDirectory() as temp_dir:
        if TEMP_DIR_OVERRIDE:
            os.makedirs(TEMP_DIR_OVERRIDE, exist_ok=True)
            temp_dir = TEMP_DIR_OVERRIDE
        filepath_head, filepath_tail = filepath.rpartition(".")[0::2]

        output = os.path.join(temp_dir, os.path.basename(filepath_head) + ".autofmt." + filepath_tail)
        env = os.environ.copy()
        env.update({
            "ELISP_AUTOFMT_OUTPUT": output,
        })
        subprocess.check_call(cmd, env=env)
        with open(output, 'r', encoding='utf-8') as fh:
            return fh.read()


class MyFullCompareFormat(unittest.TestCase):
    """
    Sub-classes must have a ``file_format`` & ``file_expect`` property.
    """

    def compare(self, file_format: str, file_expect: str) -> None:
        self.maxDiff = None

        import difflib
        file_format = os.path.join(THIS_DIR, file_format)
        file_expect = os.path.join(THIS_DIR, file_expect)

        data_result = emacs_elisp_autofmt_file_as_str(file_format)
        with open(file_expect, 'r', encoding='utf-8') as fh:
            data_expect = fh.read()

        if UPDATE_TEST_DATA:
            with open(file_expect, 'w', encoding='utf-8') as fh:
                fh.write(data_result)
            data_expect = data_result

        diff_output = "\n".join(
            difflib.unified_diff(
                data_expect.splitlines(),
                data_result.splitlines(),
            )
        )
        self.assertEqual("", diff_output)

    def test_simple(self) -> None:
        self.compare(
            file_format="full_compare_data/simple.data",
            file_expect="full_compare_data/simple.autofmt.data",
        )

    def test_spell_fu(self) -> None:
        self.compare(
            file_format="full_compare_data/spell-fu.data",
            file_expect="full_compare_data/spell-fu.autofmt.data",
        )

    def test_undo_fu(self) -> None:
        self.compare(
            file_format="full_compare_data/undo-fu.data",
            file_expect="full_compare_data/undo-fu.autofmt.data",
        )

    def test_undo_fu_session(self) -> None:
        self.compare(
            file_format="full_compare_data/undo-fu-session.data",
            file_expect="full_compare_data/undo-fu-session.autofmt.data",
        )

    def test_xref_rst(self) -> None:
        self.compare(
            file_format="full_compare_data/xref-rst.data",
            file_expect="full_compare_data/xref-rst.autofmt.data",
        )


if __name__ == '__main__':
    unittest.main(exit=False)
