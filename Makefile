# SPDX-License-Identifier: GPL-3.0-or-later

tests: FORCE  # Run the tests.
	python tests/test_full_compare.py
	python tests/test_generate_defs.py
	python tests/test_unit.py

check_mypy: FORCE
	mypy --strict elisp-autofmt.py elisp-autofmt-cmd.py tests/*.py

check_pylint: FORCE
	pylint elisp-autofmt.py elisp-autofmt-cmd.py --max-line-length=120 --output-format=parseable --reports=n \
		--disable=C0103,C0123,C0209,C0302,C0415,R0902,R0903,R0904,R0912,R0913,R0914,R0915,R0916,R0917,R1702,W0201,W0212,W0511

check_flake8: FORCE
	flake8 elisp-autofmt.py elisp-autofmt-cmd.py --max-line-length=120

check_ruff: FORCE
	ruff check elisp-autofmt.py elisp-autofmt-cmd.py

check_vulture: FORCE
	vulture elisp-autofmt.py elisp-autofmt-cmd.py

FORCE:
