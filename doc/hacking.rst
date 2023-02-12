
#####################
Hacking ELisp-AutoFmt
#####################

Tips
====

It can be useful to preview changes to the formatted output whenever the file changes.
This can be done on Linux using ``inotifywait``.

.. code-block:: bash

   while true; do inotifywait -e close_write ./elisp-autofmt.el ./elisp-autofmt.py ; tput clear ; ./elisp-autofmt-cmd.py --stdout /path/to/script.el; done

Where ``/path/to/script.el`` is the script you wish to preview.

If Bash is not your default shell, the command can run in a single quoted string ``bash -c '...'``.
