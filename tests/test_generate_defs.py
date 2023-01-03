#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later

import shutil
import subprocess
import unittest
import tempfile

import os

from typing import (
    Any,
    Dict,
)

EMACS_BIN = "emacs"
VERBOSE = os.environ.get('VERBOSE', False)
TEMP_LOCAL = ""
# TEMP_LOCAL = "/tmp/out"

if TEMP_LOCAL:
    if not os.path.exists(TEMP_LOCAL):
        os.makedirs(TEMP_LOCAL)

BASE_DIR = os.path.normpath(os.path.join(os.path.dirname(os.path.abspath(__file__)), '..'))


def generate_defs_builtin(output: str) -> None:
    cmd = (
        EMACS_BIN,
        "--batch",
        "-l", os.path.join(BASE_DIR, "elisp-autofmt.el"),
        "--eval", (
            "(progn"
            "(setq elisp-autofmt-debug-mode t)"
            ")"
        ),
        "-f", "elisp-autofmt--gen-builtin-defs",
    )
    env = os.environ.copy()
    env.update({
        "ELISP_AUTOFMT_OUTPUT": output,
    })
    subprocess.check_call(cmd, env=env)


def generate_defs_package(output: str, package: str) -> None:
    cmd = (
        EMACS_BIN,
        "--batch",
        "-l", os.path.join(BASE_DIR, "elisp-autofmt.el"),
        "--eval", (
            "(progn"
            "(setq elisp-autofmt-debug-mode t)"
            ")"
        ),
        "-f", "elisp-autofmt--gen-package-defs",
    )
    env = os.environ.copy()
    env.update({
        "ELISP_AUTOFMT_OUTPUT": output,
        "ELISP_AUTOFMT_PACKAGE": package,
    })
    subprocess.check_call(cmd, env=env)


def generate_defs_package_as_json(package: str) -> Dict[Any, Any]:
    import json
    with tempfile.TemporaryDirectory() as d:
        if TEMP_LOCAL:
            d = TEMP_LOCAL
        p = os.path.join(d, package + ".out.json")
        generate_defs_package(p, package)
        with open(p, 'r', encoding='utf-8') as fh:
            result = json.load(fh)
    assert type(result) is dict
    return result


def generate_defs_builtin_as_json() -> Dict[Any, Any]:
    import json
    with tempfile.TemporaryDirectory() as d:
        if TEMP_LOCAL:
            d = TEMP_LOCAL
        p = os.path.join(d, "emacs.out.json")
        generate_defs_builtin(p)
        with open(p, 'r', encoding='utf-8') as fh:
            result = json.load(fh)
    assert type(result) is dict
    return result


class SimpleTestBuiltinPackage_SubrX(unittest.TestCase):
    def test_check_simple(self) -> None:
        data = generate_defs_package_as_json("subr-x")
        self.assertEqual(data['functions']['string-join'], ['func', 1, 2, {}])
        self.assertEqual(data['functions']['named-let'], ['macro', 2, 'many', {'indent': 2}])


class SimpleTestBuiltinPackage_Subr(unittest.TestCase):
    def test_check_simple(self) -> None:
        data = generate_defs_package_as_json("subr")
        self.assertEqual(data['functions']['with-syntax-table'], ['macro', 1, 'many', {'indent': 1}])
        self.assertEqual(data['functions']['defvar-local'], ['macro', 2, 3, {'doc-string': 3, 'indent': 2}])


class SimpleTestBuiltinPackage_Simple(unittest.TestCase):
    def test_check_simple(self) -> None:
        data = generate_defs_package_as_json("simple")
        self.assertEqual(data['functions']['backward-word'], ['func', 0, 1, {}])
        self.assertEqual(data['functions']['shell-command-on-region'], ['func', 3, 8, {}])


class SimpleTestBuiltinPackage_File(unittest.TestCase):
    def test_check_simple(self) -> None:
        data = generate_defs_package_as_json("files")
        self.assertEqual(data['functions']['directory-abbrev-make-regexp'], ['func', 1, 1, {}])
        self.assertEqual(data['functions']['insert-directory-safely'], ['func', 2, 4, {}])


class SimpleTestBuiltin(unittest.TestCase):
    def test_check_simple(self) -> None:
        data = generate_defs_builtin_as_json()
        self.assertEqual(data['functions']['file-attributes'], ['func', 1, 2, {}])
        self.assertEqual(data['functions']['string-prefix-p'], ['func', 2, 3, {}])


def global_setup() -> Any:
    data = None
    # shutil.rmtree(TEMP_LOCAL, ignore_errors=True)
    return data


def global_teardown(_data: Any) -> None:
    # shutil.rmtree(TEMP_LOCAL, ignore_errors=True)
    pass


if __name__ == '__main__':
    data = global_setup()
    unittest.main(exit=False)
    global_teardown(data)
