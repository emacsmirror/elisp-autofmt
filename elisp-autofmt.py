#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later

'''
Emacs lisp auto formatter.
'''

from __future__ import annotations
from typing import (
    Any,
    Callable,
    Dict,
    Generator,
    Iterable,
    List,
    NamedTuple,
    Optional,
    TextIO,
    Tuple,
    Union,
)
from types import (
    ModuleType,
    TracebackType,
)


import sys
import os
import argparse

HintType = Dict[str, Union[str, int, Tuple[int, int]]]

# ------------------------------------------------------------------------------
# Globals

# Disable mainly for testing what happens when we don't wrap lines at all.
USE_WRAP_LINES = True

# For debugging, check who added the newline.
TRACE_NEWLINES = False

# Extract argument count from functions and macros.
USE_EXTRACT_DEFS = True

# Report missing definitions.
LOG_MISSING_DEFS = None  # '/tmp/out.log'

# ------------------------------------------------------------------------------
# Exceptions


# Exception for failure to parse the file,
# show this in the command line wite.
class FmtException(Exception):
    '''An exception raised for malformed files, where formatting cannot complete.'''


# ------------------------------------------------------------------------------
# Utilities


def _function_id(num_frames_up: int) -> str:
    '''
    Create a string naming the function n frames up on the stack.
    '''
    co = sys._getframe(num_frames_up + 1).f_code
    return '%d %s' % (co.co_firstlineno, co.co_name)


def execfile(filepath: str, mod: Optional[ModuleType] = None) -> ModuleType:
    # module name isn't used or added to 'sys.modules'.
    # passing in 'mod' allows re-execution without having to reload.

    import importlib.util
    from importlib.abc import Loader
    mod_spec = importlib.util.spec_from_file_location('__main__', filepath)
    if mod_spec is None:
        raise Exception('unable to load {:s}'.format(filepath))
    if mod is None:
        mod = importlib.util.module_from_spec(mod_spec)
    # Make 'mypy' happy.
    assert isinstance(mod_spec.loader, Loader)
    mod_spec.loader.exec_module(mod)
    return mod


def is_hash_prefix_special_case(text: str) -> bool:
    return (
        text.startswith('#') and
        (not text.startswith('#\''))
    )


# ------------------------------------------------------------------------------
# Formatting Utilities

class NdSexp_SoftWrap:
    '''
    Track any newly enabled nodes ``force_newline``,
    setting ``force_newline_soft`` so they may can be used as candidates to remove later.

    This is done parent level nodes that are wrapped may remove wrapping added to child nodes.
    '''
    __slots__ = (
        'force_newline_orig',
        'node_parent',
    )

    def __init__(self, node_parent: NdSexp):
        self.node_parent = node_parent

    def __enter__(self) -> None:
        self.force_newline_orig = tuple(node.force_newline for node in self.node_parent.nodes_only_code)

    def __exit__(
            self,
            exc_type: Optional[type[BaseException]],
            exc_value: Optional[BaseException],
            exc_traceback: Optional[TracebackType],
    ) -> None:
        # NOTE: use `strict=True` for Python 3.10+.
        for node, force_newline_orig in zip(self.node_parent.nodes_only_code, self.force_newline_orig):
            if (
                    (not node.force_newline_soft) and
                    (not force_newline_orig) and
                    (node.force_newline is True)
            ):
                node.force_newline_soft = True


def apply_comment_force_newline(root: NdSexp) -> None:
    # Special calculation for lines with comments on same line,
    # don't merge these lines since it would make it seem as if
    # the comment applies to the statements on that line.
    if not root.nodes:
        return

    node_line_start = root.nodes[0]
    for node, node_parent in root.iter_nodes_recursive_with_parent():
        if node is node_line_start:
            continue
        if isinstance(node, NdComment) and node.is_own_line is False:
            if not isinstance(node_line_start, NdWs):
                if node_line_start.force_newline:
                    pass
                elif node_line_start is node_parent.nodes[0]:
                    node_parent.force_newline = True
                else:
                    node_line_start.force_newline = True
        if node_line_start.original_line != node.original_line:
            node_line_start = node


def apply_relaxed_wrap(node_parent: NdSexp, style: FormatStyle) -> None:
    node_prev = None
    force_newline = False

    if node_parent.hints is not None:
        hint_group = node_parent.hints.get('group')
    else:
        hint_group = None

    if hint_group is not None:
        assert isinstance(hint_group, list) and len(hint_group) == 2
        group_beg, group_len = hint_group
        assert isinstance(group_beg, int)
        assert isinstance(group_len, int)

        nodes_iter = node_parent.nodes_only_code[group_beg + 1:]
    else:
        nodes_iter = node_parent.nodes_only_code[node_parent.index_wrap_hint:]

    i_last = len(nodes_iter) - 1

    # Make a map, we could store this for reuse.
    nodes_with_trailing_comment_or_newline = set()
    node_prev = None
    for node in node_parent.nodes:
        if isinstance(node, (NdComment, NdWs)):
            nodes_with_trailing_comment_or_newline.add(id(node_prev))
        node_prev = node
    # Finish building 'nodes_with_trailing_comment_or_newline'.

    if hint_group is not None:
        group_len = hint_group[1]
        for i, node in enumerate(nodes_iter):
            ok = True

            if (i % group_len) != 0:
                ok = False

            if ok:
                node.force_newline = True
            force_newline |= node.force_newline

            node_prev = node

    else:
        for i, node in enumerate(nodes_iter):
            node_next = None if i == i_last else nodes_iter[i + 1]

            ok = True
            # Keep pairs:
            #     (foo
            #       :keyword value)
            #
            # Or:
            #     (foo
            #       :keyword value
            #       :other other-value)
            #
            # But only pairs, so multiple values each get their own line:
            #     (foo
            #       :keyword
            #       value
            #       other-value)
            #
            # .. Better for use-package 'config' sections.
            #
            # But but... keep ther pairs in the case of a blank line or comment.
            #     (foo
            #       :keyword value
            #
            #       other-value)
            #
            # .. Useful for macros such as 'define-minor-mode' which have properties, then a &rest.
            #
            if (
                    (isinstance(node_prev, NdSymbol) and node_prev.data.startswith(':')) and
                    (
                        (node_next is None) or
                        (id(node) in nodes_with_trailing_comment_or_newline) or
                        (isinstance(node_next, NdSymbol) and node_next.data.startswith(':'))
                    )
            ):
                ok = False

            if ok:
                node.force_newline = True
            force_newline |= node.force_newline

            node_prev = node

    if not style.use_native:
        if force_newline:
            node_parent.force_newline = True


def apply_relaxed_wrap_when_multiple_args(node_parent: NdSexp, style: FormatStyle) -> None:
    if len(node_parent.nodes_only_code) - node_parent.index_wrap_hint > 1:
        apply_relaxed_wrap(node_parent, style)


def parse_local_defs(defs: Defs, node_parent: NdSexp) -> None:
    # Extract the number of functions from local definitions.
    # Currently only used so we can break the number of arguments at '&rest'
    # since it's nearly always where we have the body of macros which is logically
    # where we want to break.
    if node_parent.nodes_only_code and node_parent.brackets == '()':
        node = node_parent.nodes_only_code[0]
        if isinstance(node, NdSymbol):
            symbol_type = None
            if node.data in {
                    'defsubst',
                    'defun',
                    'defadvice',
                    'iter-defun',
            }:
                symbol_type = 'func'
            elif node.data in {
                    'defmacro',
            }:
                symbol_type = 'macro'
            else:
                return

            # Sanity check, should never fail.
            if len(node_parent.nodes_only_code) >= 3:
                node_symbol = node_parent.nodes_only_code[1]
                node_args = node_parent.nodes_only_code[2]
                if isinstance(node_symbol, NdSymbol) and isinstance(node_args, NdSexp):
                    symbol = node_symbol.data
                    arg_index_min = 0
                    arg_index_max: Union[int, str] = 0
                    hints: Optional[HintType] = None
                    for i, node_arg in enumerate(node_args.nodes_only_code):
                        if not isinstance(node_arg, NdSymbol):
                            continue

                        if node_arg.data == '&rest':
                            arg_index_min = i
                            arg_index_max = 'many'
                            break
                        if node_arg.data == '&optional':
                            arg_index_min = i
                            arg_index_max = i
                            # Count remaining arguments and exit.
                            for j in range(i + 1, len(node_args.nodes_only_code)):
                                if isinstance(node_args.nodes_only_code[j], NdSymbol):
                                    arg_index_max = j
                            break

                    if node.data in {'defun', 'defmacro'}:
                        if len(node_parent.nodes_only_code) >= 4:
                            node_decl = None
                            if isinstance(node_parent.nodes_only_code[3], NdString):
                                if len(node_parent.nodes_only_code) >= 5:
                                    if isinstance(node_parent.nodes_only_code[4], NdSexp):
                                        node_decl = node_parent.nodes_only_code[4]
                            else:
                                if isinstance(node_parent.nodes_only_code[3], NdSexp):
                                    node_decl = node_parent.nodes_only_code[3]

                            # First argument after a function may be 'declare'.
                            if node_decl is not None and len(node_decl.nodes_only_code) > 1:
                                if (
                                        (isinstance(node_decl.nodes_only_code[0], NdSymbol)) and
                                        (node_decl.nodes_only_code[0].data == 'declare')
                                ):
                                    # The second value is currently unused (max arguments).
                                    for node_iter in node_decl.nodes_only_code[1:]:
                                        if (
                                                isinstance(node_iter, NdSexp) and
                                                len(node_iter.nodes_only_code) == 2
                                        ):
                                            node_key, node_val = node_iter.nodes_only_code
                                            if isinstance(node_key, NdSymbol):
                                                key = node_key.data
                                                if key == 'indent':
                                                    if hints is None:
                                                        hints = {}
                                                    if isinstance(node_val, NdSymbol):
                                                        val = node_val.data
                                                        hints[key] = int(val) if val.isdigit() else val
                                                elif key == 'doc-string':
                                                    if hints is None:
                                                        hints = {}
                                                    if isinstance(node_val, NdSymbol):
                                                        val = node_val.data
                                                        hints[key] = int(val) if val.isdigit() else val

                    defs.fn_arity[symbol] = FnArity(
                        symbol_type=symbol_type,
                        nargs_min=arg_index_min,
                        nargs_max=arg_index_max,
                        hints=hints,
                    )

    for node in node_parent.nodes_only_code:
        if isinstance(node, NdSexp):
            parse_local_defs(defs, node)


def apply_rules(cfg: FormatConfig, node_parent: NdSexp) -> None:
    use_native = cfg.style.use_native

    # Optional
    if node_parent.nodes_only_code and node_parent.brackets == '()':
        node = node_parent.nodes_only_code[0]
        if isinstance(node, NdSymbol):

            node_parent.index_wrap_hint = 1

            if node.data in {
                    'cl-letf',
                    'cl-letf*',
                    'if-let',
                    'if-let*',
                    'let',
                    'let*',
                    'pcase-let',
                    'pcase-let*',
                    'when-let',
                    'when-let*',
            }:
                # Only wrap with multiple declarations.
                if use_native:
                    if isinstance(node_parent.nodes_only_code[1], NdSexp):
                        if len(node_parent.nodes_only_code[1].nodes_only_code) > 1:
                            for subnode in node_parent.nodes_only_code[1].nodes_only_code[1:]:
                                subnode.force_newline = True
                else:
                    if isinstance(node_parent.nodes_only_code[1], NdSexp):
                        if len(node_parent.nodes_only_code[1].nodes_only_code) > 1:
                            for subnode in node_parent.nodes_only_code[1].nodes_only_code:
                                subnode.force_newline = True

                # A new line for each body of the let-statement.
                node_parent.index_wrap_hint = 2

                if not use_native:
                    if len(node_parent.nodes_only_code) > 2:
                        # While this should always be true, while editing it can be empty at times.
                        # Don't error in this case because it's annoying.
                        node_parent.nodes_only_code[2].force_newline = True

                node_parent.hints['indent'] = 1
                apply_relaxed_wrap(node_parent, cfg.style)
            elif node.data == 'cond':
                for subnode in node_parent.nodes_only_code[1:]:
                    subnode.force_newline = True
                    if isinstance(subnode, NdSexp) and len(subnode.nodes_only_code) >= 2:
                        subnode.nodes_only_code[1].force_newline = True
                        apply_relaxed_wrap_when_multiple_args(subnode, cfg.style)
            else:
                # First lookup built-in definitions, if they exist.
                if (fn_data := cfg.defs.fn_arity.get(node.data)) is not None:
                    # May be `FnArity` or a list.
                    symbol_type, nargs_min, nargs_max, hints = fn_data
                    if nargs_min is None:
                        nargs_min = 0

                    if hints is not None:
                        node_parent.hints.update(hints)
                    hints = node_parent.hints

                    hint_indent = hints.get('indent')
                    if hint_indent is not None:
                        # node_parent.index_wrap_hint = 1 + hint_indent
                        if symbol_type in {'special', 'macro'}:
                            if 'break' not in hints:
                                hints['break'] = 'always'

                    # First symbol counts for 1, another since wrapping takes place after this argument.
                    node_parent.index_wrap_hint = nargs_min + 1

                    # Wrap the first argument, instead of the last argument
                    # so all arguments are at an equal level as having the last
                    # argument split from the rest doesn't signify an important difference.
                    if symbol_type == 'func':
                        if hint_indent is None:
                            if hints is not None and hints.get('break_point') != 'overflow':
                                if node_parent.index_wrap_hint >= len(node_parent.nodes_only_code):
                                    node_parent.index_wrap_hint = 1

                    elif symbol_type == 'macro':
                        if nargs_max == 'many':
                            # So (with ...) macros don't keep the first argument aligned.
                            node_parent.wrap_all_or_nothing_hint = True
                            hints['break_point'] = 'overflow'

                    elif symbol_type == 'special':
                        # Used for special forms `unwind-protect`, `progn` .. etc.
                        node_parent.wrap_all_or_nothing_hint = True
                        if hints is None:
                            hints = {}
                        if 'break' not in hints:
                            hints['break'] = 'always'

                    if hints:
                        # Always wrap the doc-string.
                        if (hint_docstring := hints.get('doc-string')) is not None:
                            # NOTE: no support for evaluating EMACS-lisp from Python
                            # (so no support for symbol types).
                            if isinstance(hint_docstring, int):
                                node_parent.index_wrap_hint = min(node_parent.index_wrap_hint, hint_docstring)

                        if (hint_group := hints.get('group')) is not None:
                            assert isinstance(hint_group, list) and len(hint_group) == 2
                            group_beg, group_len = hint_group
                            assert isinstance(group_beg, int)
                            assert isinstance(group_len, int)
                            if len(node_parent.nodes_only_code) > group_beg + group_len + 1:
                                node_parent.index_wrap_hint = group_beg + group_len + 1
                            else:
                                # Group not in use.
                                del hints['group']

                        if (val := hints.get('break')) is not None:
                            if val == 'always':
                                apply_relaxed_wrap(node_parent, cfg.style)
                            elif val == 'multi':
                                apply_relaxed_wrap_when_multiple_args(node_parent, cfg.style)
                            elif val == 'to_wrap':  # Default
                                pass
                            else:
                                raise FmtException((
                                    'unknown "break" for {:s}, expected a value in '
                                    '["always", "multi", "to_wrap"]'
                                ).format(node.data))
                else:
                    if LOG_MISSING_DEFS is not None:
                        with open(LOG_MISSING_DEFS, 'a', encoding='utf-8') as fh:
                            fh.write('Missing: {:s}\n'.format(node.data))

    for node in node_parent.nodes_only_code:
        if isinstance(node, NdSexp):
            apply_rules(cfg, node)
        if not use_native:
            if node.force_newline:
                node_parent.force_newline = True

    if not use_native:
        node_parent.flush_newlines_from_nodes()


def apply_pre_indent_1(cfg: FormatConfig, node_parent: NdSexp, level: int, trailing_parens: int) -> None:
    # First be relaxed, then again if it fails.
    # NOTE: The caller should use a `NdSexp_SoftWrap` context manager.
    if node_parent.fmt_check_exceeds_colum_max(cfg, level, trailing_parens, calc_score=False):
        apply_relaxed_wrap(node_parent, cfg.style)
        if not cfg.style.use_native:
            node_parent.force_newline = True


def apply_pre_indent_2(cfg: FormatConfig, node_parent: NdSexp, level: int, trailing_parens: int) -> None:
    # NOTE: The caller should use a `NdSexp_SoftWrap` context manager.
    assert node_parent.index_wrap_hint != 0
    assert len(node_parent.nodes_only_code) > 1

    node_force_newline = []
    # Wrap items before if absolutely needed, one at a time.
    force_newline = False
    i = min(node_parent.index_wrap_hint, len(node_parent.nodes_only_code) - 1)
    assert i > 0

    node = node_parent.nodes_only_code[i]
    score_init = node_parent.fmt_check_exceeds_colum_max(
        cfg,
        level,
        trailing_parens,
        calc_score=True,
        test_node_terminate=node,
    )

    if score_init:
        # Don't attempt the wrap the first item,
        # as this will simply push it onto the line below.
        #
        # This:
        #   (arg "Long string that does not fit")
        #
        # Gets converted to this:
        #   (
        #     arg
        #     "Long string that does not fit")
        #
        # Where as we would prefer this:
        #   (arg
        #     "Long string that does not fit")
        #
        while i != 0:
            node = node_parent.nodes_only_code[i]
            if not node.force_newline:
                node.force_newline = True
                node_force_newline.append(node)
                force_newline = True

                if not node_parent.fmt_check_exceeds_colum_max(
                    cfg,
                    level,
                    trailing_parens,
                    calc_score=False,
                    test_node_terminate=node,
                ):
                    # Imply 'node_parent.wrap_all_or_nothing_hint', even when not set.
                    hints = node_parent.hints
                    if hints.get('break_point') == 'overflow':
                        pass
                    else:
                        # TODO: warn about unknown break_point.

                        # When the break was added before `node_parent.index_wrap_hint`,
                        # wrap everything so there are no breaks added in random locations
                        # that might seems significant.
                        if i < node_parent.index_wrap_hint:
                            for j in range(1, i):
                                node_iter = node_parent.nodes_only_code[j]
                                if not node_iter.force_newline:
                                    node_iter.force_newline = True
                                    node_force_newline.append(node_iter)
                    break
            i -= 1

        i = min(node_parent.index_wrap_hint, len(node_parent.nodes_only_code) - 1)
        node = node_parent.nodes_only_code[i]
        score_test = node_parent.fmt_check_exceeds_colum_max(
            cfg,
            level,
            trailing_parens,
            calc_score=True,
            test_node_terminate=node,
        )

        # If none of the changes made an improvement, revert them.
        if score_init <= score_test:
            for node_iter in node_force_newline:
                node_iter.force_newline = False
            force_newline = False

    if not cfg.style.use_native:
        if force_newline:
            node_parent.force_newline = True

    # If after wrapping 'everything', we still overflow,
    # don't use  this for tests in future, it confuses checks
    # causing other lines to wrap because of this node.
    if cfg.style.use_native:
        if len(node_parent.nodes_only_code) > 1:
            node = node_parent.nodes_only_code[1]
            if not node.force_newline:
                score_init = node_parent.fmt_check_exceeds_colum_max(
                    cfg, level, trailing_parens, calc_score=True)
                if score_init:
                    node.force_newline = True
                    score_test = node_parent.fmt_check_exceeds_colum_max(
                        cfg,
                        level,
                        trailing_parens,
                        calc_score=True,
                    )
                    if score_test < score_init:
                        # Success, don't exclude.
                        pass
                    else:
                        # Don't add line break.
                        node.force_newline = False


def apply_pre_indent(cfg: FormatConfig, node_parent: NdSexp, level: int, trailing_parens: int) -> None:
    '''
    For lists that will need wrapping even when all parents are wrapped,
    wrap these beforehand.
    '''
    if not node_parent.nodes_only_code:
        return

    node_parent_is_multiline_prev = node_parent.is_multiline()

    node_trailing_parens = node_parent.node_last_for_trailing_parens_test()

    level_next_data = node_parent.calc_nodes_level_next(cfg, level)
    level_next_data_last = len(level_next_data) - 1
    for i, node in enumerate(node_parent.nodes):
        if isinstance(node, NdSexp):
            level_next = level_next_data[min(i, level_next_data_last)]
            apply_pre_indent(cfg, node, level_next, trailing_parens + 1 if node is node_trailing_parens else 0)
            if not cfg.style.use_native:
                if node.force_newline:
                    node_parent.force_newline = True

    if len(node_parent.nodes_only_code) > 1:
        with NdSexp_SoftWrap(node_parent):
            apply_pre_indent_1(cfg, node_parent, level, trailing_parens)
            apply_pre_indent_2(cfg, node_parent, level, trailing_parens)

    # Some blocks don't allow mixed wrapping.
    if node_parent.wrap_all_or_nothing_hint:
        if node_parent_is_multiline_prev or node_parent.is_multiline():
            if node_parent_is_multiline_prev:
                apply_relaxed_wrap(node_parent, cfg.style)
            else:
                with NdSexp_SoftWrap(node_parent):
                    apply_relaxed_wrap(node_parent, cfg.style)


def apply_pre_indent_unwrap_recursive(cfg: FormatConfig, node_parent: NdSexp, level: int, trailing_parens: int) -> None:
    if not node_parent.nodes_only_code:
        return

    node_trailing_parens = node_parent.node_last_for_trailing_parens_test()

    level_next_data = node_parent.calc_nodes_level_next(cfg, level)
    level_next_data_last = len(level_next_data) - 1
    force_newline_soft_any = False
    for i, node in enumerate(node_parent.nodes):
        if not isinstance(node, NODE_CODE_TYPES):
            continue

        if isinstance(node, NdSexp):
            level_next = level_next_data[min(i, level_next_data_last)]
            apply_pre_indent_unwrap_recursive(
                cfg,
                node,
                level_next,
                trailing_parens + 1 if node is node_trailing_parens else 0,
            )

        force_newline_soft_any |= node.force_newline_soft

    # Check if any of these nodes that were wrapped to fit into the fill-column could be unwrapped.
    if force_newline_soft_any:
        nl = [
            (node.force_newline, node.force_newline_soft)
            for node in node_parent.nodes_only_code
        ]
        for node in node_parent.nodes_only_code:
            if node.force_newline_soft:
                node.force_newline = False
                node.force_newline_soft = False

        if node_parent.fmt_check_exceeds_colum_max(cfg, level, trailing_parens, calc_score=False):
            # Failure, restore the previous state.
            for i, node in enumerate(node_parent.nodes_only_code):
                node.force_newline, node.force_newline_soft = nl[i]


# ------------------------------------------------------------------------------
# Classes

# Immutable configuration,
# created from command line arguments.


class FormatStyle(NamedTuple):
    '''Details relating to formatting style.'''
    use_native: bool


class FormatConfig(NamedTuple):
    '''Configuration options relating to how the file should be formatted.'''
    style: FormatStyle
    use_trailing_parens: bool
    fill_column: int
    empty_lines: int
    defs: Defs


class FnArity(NamedTuple):
    '''Data associated with a function.'''
    # Type in: [`macro`, `func`, `special`].
    symbol_type: str
    # Minimum number of arguments.
    nargs_min: int
    # Maximum number of arguments, or strings: `many`, `unevalled`.
    nargs_max: Union[int, str]
    # Optional additional hints.
    hints: Optional[HintType]


class Defs:
    '''
    Function definition data, hints about when to wrap arguments.
    '''
    __slots__ = (
        'fn_arity',
        '_is_complete',
        '_has_local',
    )

    def __init__(
            self,
            *,
            # The key is the function name.
            fn_arity: Dict[str, FnArity],
            is_complete: bool,
    ):
        self.fn_arity = fn_arity
        self._is_complete = is_complete
        self._has_local: bool = False

    @staticmethod
    def from_json_files(fmt_defs: Iterable[str]) -> Defs:
        import json
        functions = {}
        for filepath in fmt_defs:
            with open(filepath, 'r', encoding='utf-8') as fh:
                try:
                    fh_as_json = json.load(fh)
                except BaseException as ex:
                    sys.stderr.write('JSON definition: error ({:s}) parsing {!r}!\n'.format(str(ex), filepath))
                    continue

                functions_from_json = fh_as_json.get('functions')
                if functions_from_json is None:
                    continue

                if type(functions_from_json) is not dict:
                    sys.stderr.write(
                        'JSON definition: "functions" entry is a {!r}, expected a dict in {!r}!\n'.format(
                            type(functions_from_json).__name__,
                            filepath,
                        )
                    )
                    continue

                functions.update(functions_from_json)

        return Defs(fn_arity=functions, is_complete=True)


class WriteCtx:
    '''Track context while writing.'''
    __slots__ = (
        'last_node',
        'is_newline',
        'line',
        'line_terminate',
        'cfg',
    )

    last_node: Optional[Node]
    is_newline: bool
    line: int
    line_terminate: int
    cfg: FormatConfig

    def __init__(self, cfg: FormatConfig):
        self.last_node = None
        self.is_newline = True
        self.line = 0
        self.line_terminate = -1
        self.cfg = cfg


class Node:
    '''Base class for all kinds of Lisp elements.'''
    __slots__ = (
        'force_newline',
        'force_newline_soft',
        'original_line',
    )

    force_newline: bool
    force_newline_soft: bool
    original_line: int

    def calc_force_newline(self, style: FormatStyle) -> None:
        raise Exception('All subclasses must define this')

    def fmt(
            self,
            ctx: WriteCtx,
            write_fn: Callable[[str], Any],
            level: int,
            *,
            test: bool = False,
    ) -> None:
        raise Exception('All subclasses must define this')


if TRACE_NEWLINES:
    _Node = Node
    Node.__slots__ = tuple(s for s in Node.__slots__ if s != 'force_newline')  # type: ignore
    del Node

    class NodeTraceLines(_Node):
        '''Base class for all kinds of Lisp elements.'''
        __slots__ = (
            '_force_newline',
            '_force_newline_tracepoint',
        )

        original_line: int

        @property
        def force_newline(self) -> bool:
            return self._force_newline

        @force_newline.setter
        def force_newline(self, value: bool) -> None:
            assert (value is True or value is False)
            if getattr(self, '_force_newline', None) != value:
                if value:
                    self._force_newline_tracepoint = (
                        _function_id(1) + ' ' +
                        _function_id(2)
                    )
            self._force_newline = value

    Node = NodeTraceLines  # type: ignore


class NdSexp(Node):
    '''Represents S-expressions (lists with curved or square brackets).'''
    __slots__ = (
        'prefix',
        'brackets',
        'nodes',
        'nodes_only_code',
        'index_wrap_hint',
        'wrap_all_or_nothing_hint',
        'hints',
    )

    def __init__(self, line: int, brackets: str, nodes: Optional[List[Node]] = None):
        self.original_line = line
        self.prefix: str = ''
        self.brackets = brackets
        self.nodes = nodes or []
        self.index_wrap_hint: int = 1
        self.wrap_all_or_nothing_hint: bool = False
        self.hints: HintType = {}

    def __repr__(self) -> str:
        return '{}(line={} prefix=({}) newline={}\n  {}\n)'.format(
            self.__class__.__name__,
            self.original_line,
            self.prefix,
            self.force_newline,
            '\n  '.join(repr(node) for node in self.nodes),
        )

    def is_multiline(self) -> bool:
        for node in self.iter_nodes_recursive():
            if node.force_newline:
                return True
        return False

    def node_last_for_trailing_parens_test(self) -> Optional[Node]:
        '''
        Return the node which would have trialing parenthesis written after it or None if it's not a code node
        since a trailing comment for e.g. will never have parenthesis written directly after it.
        '''
        if self.nodes and self.nodes_only_code:
            if self.nodes[-1] is self.nodes_only_code[-1]:
                return self.nodes[-1]
        return None

    def iter_nodes_recursive(self) -> Generator[Node, None, None]:
        for node in self.nodes:
            yield node
            if isinstance(node, NdSexp):
                yield from node.iter_nodes_recursive()

    def iter_nodes_recursive_with_parent(self) -> Generator[Tuple[Node, NdSexp], None, None]:
        for node in self.nodes:
            yield (node, self)
            if isinstance(node, NdSexp):
                yield from node.iter_nodes_recursive_with_parent()

    def calc_nodes_level_next(self, cfg: FormatConfig, level: int) -> List[int]:
        '''
        Return a ``self.nodes`` aligned list of next levels.
        The list may be shorter, in this case the last element should be used
        for node indices that exceed this lists range.
        '''
        if not cfg.style.use_native:
            if level == -1:
                return [0]
            return [level + 2]

        # The complex 'native' case.
        node_code_index_pre_newline = 0
        if self.hints:
            indent = indent_orig = self.hints.get('indent')
            if type(indent) is str:
                fn_data_test = cfg.defs.fn_arity.get(indent)
                if fn_data_test is not None:
                    hints_test = fn_data_test[3]
                    if hints_test is not None:
                        indent = hints_test.get('indent')
                        if not isinstance(indent, int):
                            # Unlikely, avoid unexpected cases.
                            indent = None
                del fn_data_test

            if indent is not None:
                if isinstance(indent, int):
                    hint_docstring = self.hints.get('doc-string')
                    if hint_docstring is not None:
                        # TODO: no support for evaluating EMACS-lisp from Python.
                        if isinstance(hint_docstring, int):
                            indent = min(hint_docstring - 1, indent)
                    del hint_docstring
                else:
                    indent = None
        else:
            indent_orig = None
            indent = None

        if level == -1:
            level_next_first = 0
            level_next_pre = 0
            level_next_post = 0
        else:
            level_next_base = level + len(self.prefix)
            level_next_first = level_next_base + 1
            level_next_pre = level_next_base + 1
            level_next_post = level_next_base + 1

            if (len(self.nodes_only_code) > 1) and isinstance(self.nodes_only_code[0], NdSymbol):
                if self.nodes_only_code[0].force_newline is False:
                    if self.nodes_only_code[1].force_newline is False:
                        # Add 1 for the space for the trailing space.
                        # Values may be overwritten below.
                        level_next_first = level_next_pre = level_next_base + len(self.nodes_only_code[0].data) + 2
                        node_code_index_pre_newline = 1

            if indent is not None:
                if isinstance(self.nodes_only_code[0], NdSymbol):
                    fn_data = cfg.defs.fn_arity.get(self.nodes_only_code[0].data)
                else:
                    fn_data = None
                if fn_data is not None:
                    symbol_type, nargs_min, nargs_max, _hints = fn_data

                # Perhaps this should be indented further.
                if (
                        len(self.nodes_only_code) > 1 and
                        isinstance(self.nodes_only_code[0], NdSymbol) and
                        self.nodes_only_code[0].force_newline is False
                ):

                    two_or_more_non_wrapped_args = True
                    for node in self.nodes_only_code[1:3]:
                        if node.force_newline:
                            two_or_more_non_wrapped_args = False
                            break

                    if self.nodes_only_code[1].force_newline:
                        is_aligned = False
                    elif fn_data and symbol_type == 'special':
                        # This is used for e.g.
                        #    (condition-case err
                        #        (progn
                        #          test
                        #          case)
                        #      (error case))
                        is_aligned = False
                    elif (
                            fn_data is not None and
                            (nargs_min <= indent) and
                            two_or_more_non_wrapped_args and
                            # fancy-compilation needs this.
                            # WARNING: this is odd but for e.g.
                            #    (defun foo
                            #        (long arg list)
                            #      "Doc string.")
                            # is what emacs does. so follow this.
                            self.nodes_only_code[0].data != 'defun'
                    ):
                        # This is used for e.g.
                        #
                        #    (example foo bar
                        #             test)
                        is_aligned = True
                    else:
                        # This is used for e.g.
                        #    (when
                        #        (progn
                        #          a
                        #          b)
                        #      c)
                        is_aligned = False

                    if is_aligned:
                        level_next_pre = level_next_base + len(self.nodes_only_code[0].data) + 2
                    else:
                        level_next_pre = level_next_base + 4

                    level_next_post = level_next_base + 2

                    if fn_data is not None:
                        # All wrapped, no special indents handling.
                        if indent_orig == 'defun':
                            if (
                                    (nargs_max == 'unevalled' and symbol_type == 'special') or
                                    symbol_type in {'func', 'macro'}
                            ):
                                level_next_pre = level_next_base + 2
                                level_next_post = level_next_base + 2

        node_code_index = 0
        node_code_index_next = 0
        if indent is not None:
            assert isinstance(indent, int)
            node_code_index_to_indent = indent
        else:
            # Never use 'level_next_post', when no indent, everything is 'pre'.
            node_code_index_to_indent = len(self.nodes_only_code) + 1

        level_next_data = []
        for node in self.nodes:
            node_code_index = node_code_index_next
            if node_code_index <= node_code_index_pre_newline:
                level_next_data.append(level_next_first)
            elif node_code_index <= node_code_index_to_indent:
                level_next_data.append(level_next_pre)
            else:
                level_next_data.append(level_next_post)
                # All future elements will use this value.
                break

            if isinstance(node, NODE_CODE_TYPES):
                node_code_index_next = node_code_index + 1
            else:
                node_code_index_next = node_code_index

        return level_next_data

    def flush_newlines_from_nodes(self) -> bool:
        # assert not cfg.use_native
        changed = False
        if not self.force_newline:
            for node in self.nodes_only_code:
                if node.force_newline:
                    self.force_newline = True
                    changed = True
                    break
        return changed

    def flush_newlines_from_nodes_recursive_for_native(self) -> bool:
        # Ensure There is never trailing non-wrapped S-expressions: e.g:
        #
        #    (a b c d (e
        #              f
        #              g))
        #
        # This is only permissible for the first or second arguments, e.g:
        #
        #    (a (e
        #        f
        #        g))
        #
        # While this could be supported currently it's not and I feel this adds awkward right shift.
        # assert cfg.use_native
        changed = False
        for i, node in enumerate(self.nodes_only_code):
            if i > 1 and isinstance(node, NdSexp) and not node.force_newline and node.is_multiline():
                node.force_newline = True
                changed = True
                # if not self.force_newline:
                #     self.force_newline = True
            if isinstance(node, NdSexp):
                changed |= node.flush_newlines_from_nodes_recursive_for_native()
        return changed

    def flush_newlines_from_nodes_recursive(self) -> bool:
        # assert not cfg.use_native
        changed = False
        for node in self.nodes_only_code:
            if node.force_newline:
                if not self.force_newline:
                    changed = True
                    self.force_newline = True
            if isinstance(node, NdSexp):
                changed |= node.flush_newlines_from_nodes_recursive()
        return changed

    def calc_force_newline(self, style: FormatStyle) -> None:
        force_newline = False
        node_prev = None
        for node in self.nodes:
            node.calc_force_newline(style)
            if not node.force_newline:
                if node_prev:
                    if isinstance(node_prev, NdComment):
                        node.force_newline = True
                    elif isinstance(node_prev, NdSymbol):
                        # Always keep trailing back-slashes,
                        # these are used to define a string literal across multiple lines.
                        if node_prev.data == '\\' and (node_prev.original_line != node.original_line):
                            node.force_newline = True
            force_newline |= node.force_newline
            node_prev = node

        if not style.use_native:
            self.force_newline = force_newline
        else:
            self.force_newline = False
        self.force_newline_soft = False

    def finalize(self, cfg: FormatConfig) -> None:
        # Connect: ' (  to '(
        i = len(self.nodes) - 1
        while i > 0:
            node = self.nodes[i]
            if isinstance(node, NdSexp):
                node_prev = self.nodes[i - 1]
                if isinstance(node_prev, NdSymbol):
                    if (
                            not node_prev.data.strip('#,`\'') or
                            # Some macros use `#foo(a(b(c)))` which need to be connected.
                            is_hash_prefix_special_case(node_prev.data)
                    ):
                        del self.nodes[i - 1]
                        node.prefix = node_prev.data
                        i -= 1
            i -= 1

        # Strip blank lines at the start or end of Sexpr's.
        for i in (-1, 0):
            while self.nodes and isinstance(self.nodes[i], NdWs):
                del self.nodes[i]

        # Apply maximum blank lines.
        i = len(self.nodes) - 1

        count = 0
        while i > 1:
            node = self.nodes[i]
            if isinstance(node, NdWs):
                if count == cfg.empty_lines:
                    del self.nodes[i]
                else:
                    count += 1
            else:
                count = 0
            i -= 1
        del count

        self.nodes_only_code: List[Node] = [
            node for node in self.nodes
            if isinstance(node, NODE_CODE_TYPES)
        ]
        for node in self.nodes_only_code:
            if isinstance(node, NdSexp):
                node.finalize(cfg)

    def fmt_check_exceeds_colum_max(
            self,
            cfg: FormatConfig,
            level: int,
            trailing_parens: int,
            *,
            calc_score: bool,
            test_node_terminate: Optional[Node] = None,
    ) -> int:
        '''
        :arg calc_score: When true, the return value is a score,
           zero when all values are within the fill column.
           This is the accumulated ``2 ** overflow``.
           The power is used so breaking a single line into two which both overflow
           return a better (lower) score than a single line that overflows.
        '''

        # Simple optimization, don't calculate excess white-space.
        fill_column_offset = level
        fill_column = cfg.fill_column - fill_column_offset
        level = 0

        _ctx = WriteCtx(cfg)
        _data: List[str] = []
        self.fmt_with_terminate_node(_ctx, _data.append, level, test=True, test_node_terminate=test_node_terminate)
        data = ''.join(_data)
        del _data

        # Step over `\n` characters instead of data.split('\n')
        # so multiple characters are handled separately.
        line_step = 0
        i = 0

        score = 0
        if (not cfg.use_trailing_parens) and (_ctx.line_terminate == _ctx.line):
            line_terminate = _ctx.line_terminate
            while line_step != -1:
                line_step_next = data.find('\n', line_step)
                if line_step_next == -1:
                    line_length = len(data) - line_step
                    line_step = -1
                else:
                    line_length = line_step_next - line_step
                    line_step = line_step_next + 1

                if line_terminate == i:
                    line_length += trailing_parens
                    if line_length > fill_column:
                        if not calc_score:
                            return 1
                        score += 2 ** (line_length - fill_column)
                    break
                if line_length > fill_column:
                    if not calc_score:
                        return 1
                    score += 2 ** (line_length - fill_column)
                i += 1
        else:
            while line_step != -1:
                line_step_next = data.find('\n', line_step)
                if line_step_next == -1:
                    line_length = len(data) - line_step
                    line_step = -1
                else:
                    line_length = line_step_next - line_step
                    line_step = line_step_next + 1

                if line_length > fill_column:
                    if not calc_score:
                        return 1
                    score += 2 ** (line_length - fill_column)
                i += 1

        return score

    def fmt_pre_wrap(self, ctx: WriteCtx, level: int, trailing_parens: int) -> None:
        # First handle Sexpr's one at a time, then all of them.
        # not very efficient, but it avoids over wrapping.

        nl = [node.force_newline for node in self.nodes_only_code]
        nodes_backup = self.nodes_only_code[:]
        for i in range(self.index_wrap_hint, len(nodes_backup) + 1):
            if i == len(nodes_backup):
                self.nodes_only_code = nodes_backup
            else:
                if not isinstance(nodes_backup[i], NdSexp):
                    continue
                if nodes_backup[i].force_newline:
                    continue
                self.nodes_only_code[:] = [nodes_backup[i]]

            assert ctx.line_terminate == -1
            if self.fmt_check_exceeds_colum_max(ctx.cfg, level, trailing_parens, calc_score=False):
                if i == len(nodes_backup):
                    with NdSexp_SoftWrap(self):
                        apply_relaxed_wrap(self, ctx.cfg.style)
                else:
                    if not self.nodes_only_code[0].force_newline:
                        self.nodes_only_code[0].force_newline = True
                        self.nodes_only_code[0].force_newline_soft = True

        # Disable newlines for individual items where possible.
        for i, node_only_code in enumerate(self.nodes_only_code):
            if nl[i] is False and node_only_code.force_newline is True:
                node_only_code.force_newline = False
                assert ctx.line_terminate == -1
                if self.fmt_check_exceeds_colum_max(
                        ctx.cfg,
                        level,
                        trailing_parens,
                        calc_score=False,
                        test_node_terminate=node_only_code,
                ):
                    node_only_code.force_newline = True
                    node_only_code.force_newline_soft = True
                else:
                    break

        node_trailing_parens = self.node_last_for_trailing_parens_test()

        force_newline = False

        level_next_data = self.calc_nodes_level_next(ctx.cfg, level)
        level_next_data_last = len(level_next_data) - 1
        for i, node in enumerate(self.nodes):
            if isinstance(node, NdSexp):
                level_next = level_next_data[min(i, level_next_data_last)]
                node.fmt_pre_wrap(ctx, level_next, trailing_parens + 1 if node is node_trailing_parens else 0)
            force_newline |= node.force_newline

        use_native = ctx.cfg.style.use_native

        if use_native:
            pass
        else:
            if force_newline:
                self.force_newline = True
        del force_newline

        # Finally, if the node is multi-line, ensure it's also split at the hinted location.
        # Ensures we don't get:
        #     (or foo
        #        (bar
        #          baz))
        #
        # Instead it's all one line:
        #     (or foo (bar baz))
        #
        # Or both are wrapped onto a new line:
        #     (or
        #       foo
        #       (bar baz))
        #
        # ... respecting the hint for where to split.
        #
        if self.is_multiline():
            if len(self.nodes_only_code) > self.index_wrap_hint:
                node = self.nodes_only_code[self.index_wrap_hint]
                node.force_newline = True
                if not use_native:
                    self.force_newline = True

            # Ensure colon prefixed arguments are on new-lines
            # if the block is multi-line.
            #
            # When multi-line, don't do:
            #     (foo
            #       :keyword long-value-which-causes-next-line-to-wrap
            #       :other value :third value)
            #
            # Instead do:
            #     (foo
            #       :keyword long-value-which-causes-next-line-to-wrap
            #       :other value
            #       :third value)
            # But don't do:
            #     (:eval
            #        ...)

            # node_prev = None
            for node in self.nodes_only_code[self.index_wrap_hint:]:
                if not node.force_newline:
                    if (
                            isinstance(node, NdSymbol) and
                            node.data.startswith(':') and
                            (node is not self.nodes_only_code[0])
                    ):
                        # if (
                        #         isinstance(node_prev_prev, NdSymbol) and
                        #         node_prev_prev.data.startswith(':')
                        # ):
                        node.force_newline = True
                        if not use_native:
                            self.force_newline = True

                # node_prev = node

    def fmt(self,
            ctx: WriteCtx,
            write_fn: Callable[[str], Any],
            level: int,
            *,
            test: bool = False,
            ) -> None:
        self.fmt_with_terminate_node(ctx, write_fn, level, test=test)

    def fmt_with_terminate_node(
            self,
            ctx: WriteCtx,
            write_fn: Callable[[str], Any],
            level: int,
            *,
            test: bool = False,
            test_node_terminate: Optional[Node] = None,
    ) -> None:

        line_sexpr_prev = ctx.line

        if level != -1:
            if ctx.is_newline:
                write_fn(' ' * level)

            if self.prefix:
                write_fn(self.prefix)

                if ctx.cfg.style.use_native:
                    pass
                else:
                    # A single `#` is used for advanced macros,
                    # we can't put a new-line after them.
                    if (not is_hash_prefix_special_case(self.prefix)) and self.is_multiline():
                        write_fn('\n')

                        if TRACE_NEWLINES:
                            if self.force_newline:
                                if not test:
                                    write_fn(' $' + self._force_newline_tracepoint)  # type: ignore

                        ctx.line += 1
                        ctx.is_newline = True
                        write_fn(' ' * level)
                        ctx.is_newline = False

            write_fn(self.brackets[0])
            ctx.is_newline = False

        ctx.last_node = self

        is_first = True

        node_prev_is_multiline = False

        level_next_data = self.calc_nodes_level_next(ctx.cfg, level)
        level_next_data_last = len(level_next_data) - 1
        for i, node in enumerate(self.nodes):
            level_next = level_next_data[min(i, level_next_data_last)]

            if test:
                # Use only for testing.
                if node is test_node_terminate:
                    # We could return however this misses trailing parenthesis on the same line.
                    assert ctx.line_terminate == -1
                    ctx.line_terminate = ctx.line

            if (
                    node.force_newline or
                    (
                        node_prev_is_multiline and
                        # Don't push trailing comments onto new line.
                        not (isinstance(node, NdComment) and node.is_own_line is False)
                    )
            ):
                if not ctx.is_newline:
                    write_fn('\n')
                    ctx.line += 1
                    ctx.is_newline = True

            if isinstance(node, NdWs):
                pass
            else:
                if ctx.is_newline:

                    if TRACE_NEWLINES:
                        if node.force_newline:
                            if not test:
                                write_fn(' $' + node._force_newline_tracepoint)  # type: ignore

                    write_fn(' ' * level_next)

                    ctx.is_newline = False
                else:
                    if (
                            (not is_first) or

                            # Ensure we have:
                            #     ( ;; Comment.
                            #
                            # Instead of:
                            #     (;; Comment.
                            (isinstance(node, NdComment) and node.is_own_line is False)
                    ):
                        write_fn(' ')

            if ctx.line_terminate not in (-1, ctx.line):
                return

            line = ctx.line

            node.fmt(ctx, write_fn, level_next, test=test)
            node_prev_is_multiline = (line != ctx.line)

            ctx.last_node = node

            is_first = False

            if ctx.line_terminate not in (-1, ctx.line):
                return

        if level != -1:
            if ctx.is_newline:
                write_fn(' ' * level_next)
                ctx.is_newline = False
            else:
                if (
                        (ctx.cfg.use_trailing_parens and ctx.line != line_sexpr_prev) or
                        isinstance(ctx.last_node, NdComment)
                ):
                    write_fn('\n')
                    ctx.line += 1
                    ctx.is_newline = True
                    write_fn(' ' * level_next)
                    ctx.is_newline = False
            write_fn(self.brackets[1])
            ctx.is_newline = False


# ------------------------------------------------------------------------------
# Formatting Utilities

# Currently this always represents a blank line.
class NdWs(Node):
    '''This represents white-space to be kept in the output.'''
    __slots__ = ()

    def __init__(self, line: int):
        self.original_line = line

    def __repr__(self) -> str:
        return '{}(line={})'.format(
            self.__class__.__name__,
            self.original_line,
        )

    def calc_force_newline(self, style: FormatStyle) -> None:
        # False because this forces it's own newline
        self.force_newline = True
        self.force_newline_soft = False

    def fmt(
            self,
            ctx: WriteCtx,
            write_fn: Callable[[str], Any],
            level: int,
            *,
            test: bool = False,
    ) -> None:
        write_fn('\n')
        ctx.line += 1
        ctx.is_newline = True


class NdComment(Node):
    '''Code-comment.'''
    __slots__ = (
        'data',
        'is_own_line',
    )

    data: str
    is_own_line: bool

    def __init__(self, line: int, data: str, is_own_line: bool):
        self.original_line = line
        self.data = data
        self.is_own_line = is_own_line

    def __repr__(self) -> str:
        return '{}(line={} \'{}\')'.format(
            self.__class__.__name__,
            self.original_line,
            self.data,
        )

    def calc_force_newline(self, style: FormatStyle) -> None:
        self.force_newline = self.is_own_line
        self.force_newline_soft = False

    def fmt(
            self,
            ctx: WriteCtx,
            write_fn: Callable[[str], Any],
            level: int,
            *,
            test: bool = False,
    ) -> None:
        write_fn(';')
        write_fn(self.data)
        ctx.is_newline = False


class NdString(Node):
    '''A string literal.'''
    __slots__ = (
        'data',
        'lines',
    )

    data: str
    lines: int

    def __init__(self, line: int, data: str):
        self.original_line = line
        self.data = data
        self.lines = self.data.count('\n')

    def __repr__(self) -> str:
        return '{}(line={} \'{}\')'.format(
            self.__class__.__name__,
            self.original_line,
            self.data,
        )

    def calc_force_newline(self, style: FormatStyle) -> None:
        if USE_WRAP_LINES:
            self.force_newline = ((not self.data.startswith('\n')) and self.lines > 0)
        else:
            self.force_newline = False
        self.force_newline_soft = False

    def fmt(
            self,
            ctx: WriteCtx,
            write_fn: Callable[[str], Any],
            level: int,
            *,
            test: bool = False,
    ) -> None:
        write_fn('"')
        write_fn(self.data)
        write_fn('"')
        ctx.is_newline = False
        ctx.line += self.lines


class NdSymbol(Node):
    '''This represents any identifier that isn't an S-expression, string, comment or white-space.'''
    __slots__ = (
        'data',
    )

    data: str

    def __init__(self, line: int, data: str):
        self.original_line = line
        self.data = data

    def __repr__(self) -> str:
        return '{}(line={} \'{}\')'.format(
            self.__class__.__name__,
            self.original_line,
            self.data,
        )

    def calc_force_newline(self, style: FormatStyle) -> None:
        self.force_newline = False
        self.force_newline_soft = False

    def fmt(
            self,
            ctx: WriteCtx,
            write_fn: Callable[[str], Any],
            level: int,
            *,
            test: bool = False,
    ) -> None:
        write_fn(self.data)
        ctx.is_newline = False


NODE_CODE_TYPES = (NdSymbol, NdString, NdSexp)


# ------------------------------------------------------------------------------
# File Parsing

def parse_file(cfg: FormatConfig, fh: TextIO) -> Tuple[str, NdSexp]:
    from io import StringIO

    line = 0

    # Fake top level S-expression to populate, (brackets aren't used).
    root = NdSexp(line, brackets='()')

    # Current S-expressions.
    sexp_ctx = [root]
    sexp_level = 0

    line_has_contents = False

    # Special case, a lisp file with a shebang.
    first_line_unparsed = ''
    c_peek: Optional[str] = fh.read(1)
    if c_peek == '#':
        first_line_chars = [c_peek]
        c_peek = None
        while c := fh.read(1):
            first_line_chars.append(c)
            if c == '\n':
                line += 1
                break
        first_line_unparsed = ''.join(first_line_chars)
        del first_line_chars

    while c := c_peek or fh.read(1):
        c_peek = None
        # NOTE: Can use 'match c' here, will bump minimum Python version to 3.10
        if c == '(':  # Open S-expression.
            sexp_ctx.append(NdSexp(line, '()'))
            sexp_ctx[sexp_level].nodes.append(sexp_ctx[-1])
            sexp_level += 1
            line_has_contents = True

        elif c == '[':  # Open vector.
            sexp_ctx.append(NdSexp(line, '[]'))
            sexp_ctx[sexp_level].nodes.append(sexp_ctx[-1])
            sexp_level += 1
            line_has_contents = True
        elif c == ')':  # Close S-expression.
            if sexp_level == 0:
                raise FmtException('additional closing brackets, line {}'.format(line))
            if sexp_ctx.pop().brackets[0] != '(':
                raise FmtException(
                    'closing bracket "{:s}" line {:d}, unmatched bracket types, expected ")"'.format(c, line)
                )
            sexp_level -= 1
            line_has_contents = True
        elif c == ']':  # Close vector.
            if sexp_level == 0:
                raise FmtException('additional closing brackets, line {}'.format(line))
            if sexp_ctx.pop().brackets[0] != '[':
                raise FmtException(
                    'closing bracket "{:s}" line {:d}, unmatched bracket types, expected "]"'.format(c, line)
                )
            sexp_level -= 1
            line_has_contents = True
        elif c == '"':  # Open & close string.
            data = StringIO()
            is_slash = False
            while (c := fh.read(1)):
                if c == '"' and not is_slash:
                    break
                data.write(c)
                if c == '\\':
                    is_slash = not is_slash
                else:
                    is_slash = False
                    if c == '\n':
                        line += 1

            if not c:
                raise FmtException('parsing string at line {}'.format(line))

            sexp_ctx[sexp_level].nodes.append(NdString(line, data.getvalue()))
            del data, is_slash, c
            line_has_contents = True
        elif c == ';':  # Comment.
            data = StringIO()
            while (c_peek := fh.read(1)) not in {'', '\n'}:
                c = c_peek
                c_peek = None
                data.write(c)

            is_own_line = not line_has_contents
            sexp_ctx[sexp_level].nodes.append(NdComment(line, data.getvalue(), is_own_line))
            del data, is_own_line
            line_has_contents = True
        elif c == '\n':  # White-space (newline).
            line += 1
            # Respect blank lines up until the limit.
            if line_has_contents is False:
                sexp_ctx[sexp_level].nodes.append(NdWs(line))
            line_has_contents = False
        elif c in {' ', '\t'}:  # White-space (space, tab) - ignored.
            pass
        else:  # Symbol (any other character).
            data = StringIO()
            is_slash = False
            while c:
                if c == '\\':
                    is_slash = not is_slash
                else:
                    is_slash = False
                data.write(c)
                c_peek = fh.read(1)
                if not c_peek:
                    break
                if c_peek == '\n':
                    break
                if not is_slash:
                    if c_peek in {
                            '(', ')',
                            '[', ']',
                            ';',
                            ' ', '\t',
                            # Lisp doesn't require spaces are between symbols and quotes.
                            '"',
                    }:
                        break

                c = c_peek
                c_peek = None

            text = data.getvalue()
            del data

            # Special support for character literals.
            if text[0] == '?':
                if c_peek:
                    # Always include the next character
                    # even if it's normally a delimiting character such as ';', '"'
                    # (un-escaped literal support, even allowing for `?;` or `?\C-;`).
                    if (
                            # Support `? ` and `?;`.
                            (len(text) == 1) or
                            # Support `?\C- ` and `?\C-;` and `?\C-\s- `.
                            (len(text) >= 4 and (
                                text[-1] == '-' and
                                text[-2].isalpha() and
                                text[-3] == '\\')
                             )
                    ):
                        text = text + c_peek
                        c_peek = None

            sexp_ctx[sexp_level].nodes.append(NdSymbol(line, text))
            del is_slash
            line_has_contents = True

    if sexp_level != 0:
        raise FmtException('unbalanced S-expressions at file-end, found {} levels, expected 0'.format(sexp_level))

    # Has newline at file start?
    # it will disable the settings such as lexical binding.
    # The intention of re-formatting is not to make any functional changes,
    # so it's important to add the blank line back.
    stars_with_bank_line = False
    if root.nodes:
        if isinstance(root.nodes[0], NdWs):
            stars_with_bank_line = True

    root.finalize(cfg)

    if stars_with_bank_line:
        # Add back the blank line.
        root.nodes.insert(0, NdWs(0))

    root.calc_force_newline(cfg.style)

    apply_comment_force_newline(root)

    if USE_WRAP_LINES:

        if USE_EXTRACT_DEFS:
            if not cfg.defs._has_local:
                parse_local_defs(cfg.defs, root)
                cfg.defs._has_local = True

        apply_rules(cfg, root)

        apply_pre_indent(cfg, root, -1, 0)

        if cfg.style.use_native:
            apply_pre_indent_unwrap_recursive(cfg, root, -1, 0)

        # All root level nodes get their own line always.
        for node in root.nodes_only_code:
            node.force_newline = True

    return first_line_unparsed, root


def write_file(
        cfg: FormatConfig,
        fh: TextIO,
        root: NdSexp,
        first_line: str,
) -> None:

    ctx = WriteCtx(cfg)

    if USE_WRAP_LINES:
        root.fmt_pre_wrap(ctx, -1, 0)
    if first_line:
        fh.write(first_line)
    root.fmt(ctx, fh.write, -1)
    fh.write('\n')


def format_file(
        filepath: str,
        cfg: FormatConfig,
        *,
        use_stdin: bool = False,
        use_stdout: bool = False,
) -> None:

    # Needed as files may contain '\r' only, see emacs own:
    # `lisp/cedet/semantic/grammar-wy.el`
    newline = '\r\n' if (os.name == 'nt') else '\n'

    if use_stdin:
        first_line, root = parse_file(cfg, sys.stdin)
    else:
        with open(filepath, 'r', encoding='utf-8', newline=newline) as fh:
            first_line, root = parse_file(cfg, fh)

    # Redundant but needed for the assertion not to fail in the case when `len(root.nodes_only_code) == 1`.
    root.force_newline = True

    if cfg.style.use_native:
        root.flush_newlines_from_nodes_recursive_for_native()
    else:
        assert root.flush_newlines_from_nodes_recursive() is False

    if use_stdout:
        write_file(cfg, sys.stdout, root, first_line)
    else:
        with open(filepath, 'w', encoding='utf-8', newline=newline) as fh:
            write_file(cfg, fh, root, first_line)


# ------------------------------------------------------------------------------
# Argument Parsing


def argparse_create() -> argparse.ArgumentParser:

    # When --help or no args are given, print this help
    usage_text = 'Format emacs-lisp.'

    epilog = (
        'This program formats emacs lisp, from the standard input, '
        'or operating on files, in-place.'
    )

    parser = argparse.ArgumentParser(description=usage_text, epilog=epilog)

    parser.add_argument(
        '--fmt-defs-dir',
        dest='fmt_defs_dir',
        metavar='DIR',
        default='',
        type=str,
        required=False,
        help='Directory used for storing definitions.',
    )
    parser.add_argument(
        '--fmt-defs',
        dest='fmt_defs',
        metavar='FILES',
        default='',
        type=str,
        required=False,
        help=(
            'Definition filenames within "--fmt-defs-dir" when only a filename is specified, '
            'otherwise absolute paths are used. '
            'split by PATH_SEPARATOR. '
            '(internal use, this is written by Emacs).'
        ),
    )
    parser.add_argument(
        '--fmt-style',
        dest='fmt_style',
        default='native',
        help=(
            'Formatting style in where "native" mimics EMACS default indentation and "fixed" '
            'formats using a simple 2-space for each nested block rule.'
        ),
        required=False,
        choices=('native', 'fixed')
    )

    parser.add_argument(
        '--quiet',
        dest='use_quiet',
        default=False,
        action='store_true',
        required=False,
        help='Don\t output any status messages.',
    )

    parser.add_argument(
        '--fmt-trailing-parens',
        dest='fmt_use_trailing_parens',
        default=False,
        action='store_true',
        required=False,
        help='Give each trailing parenthesis it\'s own line.',
    )

    parser.add_argument(
        '--fmt-fill-column',
        dest='fmt_fill_column',
        default=99,
        nargs='?',
        type=int,
        required=False,
        help='Maxumum column width.',
    )

    parser.add_argument(
        '--fmt-empty-lines',
        dest='fmt_empty_lines',
        default=2,
        nargs='?',
        type=int,
        required=False,
        help='Maximum column width.',
    )

    parser.add_argument(
        '--stdin',
        dest='use_stdin',
        default=False,
        action='store_true',
        required=False,
        help='Use the stdin for file contents instead of the file name passed in.',
    )

    parser.add_argument(
        '--stdout',
        dest='use_stdout',
        default=False,
        action='store_true',
        required=False,
        help='Use the stdout to output the file contents instead of the file name passed in.',
    )

    parser.add_argument(
        '--exit-code',
        dest='exit_code',
        default=0,
        nargs='?',
        type=int,
        required=False,
        help='Exit code to use upon successfully re-formatting',
    )

    parser.add_argument(
        'files',
        nargs=argparse.REMAINDER,
        help='All trailing arguments are treated as file paths to format.'
    )
    return parser


# ------------------------------------------------------------------------------
# Main Function


def main_generate_defs() -> bool:
    try:
        i = sys.argv.index('--gen-defs')
    except ValueError:
        return False

    args_rest = sys.argv[i + 1:]

    defs = Defs(fn_arity={}, is_complete=False)

    # Only for `defs`.
    cfg = FormatConfig(
        style=FormatStyle(
            use_native=False,
        ),
        use_trailing_parens=False,  # Ignored.
        fill_column=80,  # Ignored.
        empty_lines=0,  # Ignored.
        defs=defs,
    )

    while args_rest:
        file_input = args_rest.pop(0)
        file_output = args_rest.pop(0)

        with open(file_input, 'r', encoding='utf-8') as fh:
            _, root = parse_file(cfg, fh)

            parse_local_defs(cfg.defs, root)

        with open(file_output, 'w', encoding='utf-8') as fh:
            fh.write('{\n')
            fh.write('"functions": {\n')
            is_first = False
            for key, val in defs.fn_arity.items():
                if is_first:
                    fh.write(',\n')
                # Generated hints are always empty.
                symbol_type, nargs_min, nargs_max, _hints = val
                nargs_min_str = str(nargs_min) if isinstance(nargs_min, int) else '"{:s}"'.format(nargs_min)
                nargs_max_str = str(nargs_max) if isinstance(nargs_max, int) else '"{:s}"'.format(nargs_max)
                fh.write('"{:s}": ["{:s}", {:s}, {:s}, {{}}]'.format(
                    key, symbol_type, nargs_min_str, nargs_max_str,
                ))
                is_first = True
            fh.write('')
            fh.write('}\n')  # 'functions'.
            fh.write('}\n')

    return True


def main_no_except() -> None:

    if main_generate_defs():
        return

    args = argparse_create().parse_args()

    if args.use_stdin and args.use_stdout:
        if args.files:
            sys.stderr.write(
                'The \'--files\' argument cannot be used when both stdin and stdout are used {!r}\n'.format(args.files))
            sys.exit(1)
    elif args.use_stdin or args.use_stdout:
        if len(args.files) != 1:
            sys.stderr.write(
                'The \'--stdin\' & \'--stdout\' arguments are limited to a single file {!r}\n'.format(args.files))
            sys.exit(1)
    elif not args.files:
        sys.stderr.write(
            'No files passed in, pass in files or use both \'--stdin\' & \'--stdout\'\n')
        sys.exit(1)

    if args.fmt_defs:
        defs = Defs.from_json_files(
            os.path.join(args.fmt_defs_dir, filename) if (os.sep not in filename) else filename
            for filename in args.fmt_defs.split(os.pathsep)
        )
    else:
        defs = Defs(fn_arity={}, is_complete=False)

    cfg = FormatConfig(
        style=FormatStyle(
            use_native=args.fmt_style == 'native',
        ),
        use_trailing_parens=args.fmt_use_trailing_parens,
        fill_column=args.fmt_fill_column,
        empty_lines=args.fmt_empty_lines,
        defs=defs,
    )

    for filepath in (args.files or ['']):
        # Use the `stderr` to avoid conflicting with `stdout` when it's set.
        if (not args.use_quiet) and (not args.use_stdout):
            sys.stdout.write('%s\n' % filepath)

        format_file(
            filepath,
            cfg=cfg,
            use_stdin=args.use_stdin,
            use_stdout=args.use_stdout,
        )

    sys.exit(args.exit_code)


def main() -> None:
    try:
        main_no_except()
    except FmtException as ex:
        sys.stderr.write('Error: {}\n'.format(str(ex)))
        sys.exit(1)


if __name__ == '__main__':
    main()
