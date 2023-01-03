# SPDX-License-Identifier: GPL-3.0-or-later

test:  # Run the tests.
	python tests/test_full_compare.py
	python tests/test_generate_defs.py
	python tests/test_unit.py

check_mypy:
	mypy --strict elisp-autofmt.py tests/*.py

check_pylint:
	pylint elisp-autofmt.py --max-line-length=120 --output-format=parseable --reports=n \
		--disable=C0103,C0123,C0209,C0302,C0415,R0902,R0903,R0912,R0914,R0915,R0916,R1702,W0201,W0212,W0511
