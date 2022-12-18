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
)


import sys
import os
import argparse


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


def apply_relaxed_wrap(node_parent: NdSexp) -> None:
    node_prev = None
    force_newline = False

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

    if force_newline:
        node_parent.force_newline = True


def apply_relaxed_wrap_when_multiple_args(node_parent: NdSexp) -> None:
    if len(node_parent.nodes_only_code) - node_parent.index_wrap_hint > 1:
        apply_relaxed_wrap(node_parent)


def parse_local_defs(defs: Defs, node_parent: NdSexp) -> None:
    # Extract the number of functions from local definitions.
    # Currently only used so we can break the number of arguments at '&rest'
    # since it's nearly always where we have the body of macros which is logically
    # where we want to break.
    if node_parent.nodes_only_code and node_parent.bracket_open == '(':
        node = node_parent.nodes[0]
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
            if len(node_parent.nodes) >= 3:
                node_symbol = node_parent.nodes[1]
                node_args = node_parent.nodes[2]
                if isinstance(node_symbol, NdSymbol) and isinstance(node_args, NdSexp):
                    symbol = node_symbol.data
                    arg_index_min = 0
                    arg_index_max: Union[int, str] = 0
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

                    # The second value is currently unused (max arguments).
                    defs.fn_arity[symbol] = FnArity(
                        symbol_type=symbol_type,
                        nargs_min=arg_index_min,
                        nargs_max=arg_index_max,
                        hints=None,
                    )

    for node in node_parent.nodes_only_code:
        if isinstance(node, NdSexp):
            parse_local_defs(defs, node)


def apply_rules(defs: Defs, node_parent: NdSexp) -> None:

    # Optional
    if node_parent.nodes_only_code and node_parent.bracket_open == '(':
        node = node_parent.nodes[0]
        if isinstance(node, NdSymbol):

            node_parent.index_wrap_hint = 1

            if node.data == 'lambda':
                if (
                        len(node_parent.nodes_only_code) >= 3 and
                        isinstance(node_parent.nodes_only_code[2], NdString)
                ):
                    node_parent.index_wrap_hint = 3
                else:
                    node_parent.index_wrap_hint = 2
                node_parent.wrap_all_or_nothing_hint = True

            elif node.data in {
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
                if isinstance(node_parent.nodes_only_code[1], NdSexp):
                    if len(node_parent.nodes_only_code[1].nodes_only_code) > 1:
                        for subnode in node_parent.nodes_only_code[1].nodes_only_code:
                            subnode.force_newline = True
                # A new line for each body of the let-statement.
                if len(node_parent.nodes_only_code) > 2:
                    # While this should always be true, while editing it can be empty at times.
                    # Don't error in this case because it's annoying.
                    node_parent.nodes_only_code[2].force_newline = True
                node_parent.index_wrap_hint = 2
                apply_relaxed_wrap(node_parent)
            elif node.data == 'cond':
                for subnode in node_parent.nodes_only_code[1:]:
                    subnode.force_newline = True
                    if isinstance(subnode, NdSexp) and len(subnode.nodes_only_code) >= 2:
                        subnode.nodes_only_code[1].force_newline = True
                        apply_relaxed_wrap_when_multiple_args(subnode)

            elif node.data in {
                    'cl-defmethod',
                    'defadvice',
                    'defmacro',
                    'defsubst',
                    'defun',
                    'ert-deftest',
                    'iter-defun',
            }:
                node_parent.hints = {'break_point': 'overflow'}
                if len(node_parent.nodes_only_code) >= 4:
                    node_parent.index_wrap_hint = 3
                    if isinstance(node_parent.nodes_only_code[3], NdString):
                        apply_relaxed_wrap(node_parent)
                    else:
                        apply_relaxed_wrap_when_multiple_args(node_parent)
            elif node.data in {
                    'fset',
                    'setf',
                    'setq',
                    'setq-default',
                    'setq-local',
            }:
                node_parent.index_wrap_hint = 2
                if len(node_parent.nodes_only_code) > 3:
                    # Pairs.
                    i = 2
                    for subnode in node_parent.nodes_only_code[1:]:
                        if i == 2:
                            subnode.force_newline = True
                            i = 0
                        i += 1
            else:
                # First lookup built-in definitions, if they exist.
                if (fn_data := defs.fn_arity.get(node.data)) is not None:
                    # May be `FnArity` or a list.
                    symbol_type, nargs_min, nargs_max, hints = fn_data
                    if nargs_min is not None:
                        # First symbol counts for 1, another since wrapping takes place after this argument.
                        node_parent.index_wrap_hint = nargs_min + 1

                        # Wrap the first argument, instead of the last argument
                        # so all arguments are at an equal level as having the last
                        # argument split from the rest doesn't signify an important difference.
                        if symbol_type == 'func':
                            if node_parent.index_wrap_hint >= len(node_parent.nodes_only_code):
                                node_parent.index_wrap_hint = 1
                        elif symbol_type == 'macro':
                            if nargs_max == 'many':
                                # So (with ...) macros don't keep the first argument aligned.
                                node_parent.wrap_all_or_nothing_hint = True
                        elif symbol_type == 'special':
                            # Used for special forms `unwind-protect`, `progn` .. etc.
                            node_parent.wrap_all_or_nothing_hint = True
                            if hints is None:
                                hints = {}
                            if 'break' not in hints:
                                hints['break'] = 'always'

                        # TODO: detect a way to wrap macros that expand the remaining body argument.
                        # if nargs_max in {'many', 'unevalled'} and nargs_min != 0:
                        #     apply_relaxed_wrap(node_parent)
                        #     # apply_relaxed_wrap_when_multiple_args(node_parent)
                    if hints is not None:
                        if (val := hints.get('break')) is not None:
                            if val == 'always':
                                apply_relaxed_wrap(node_parent)
                            elif val == 'multi':
                                apply_relaxed_wrap_when_multiple_args(node_parent)
                            # elif val == 'to_wrap':  # Default
                            #     pass
                            else:
                                # TODO: warn about unknown `break`.
                                pass

                            # with open('/tmp/out.log', 'a', encoding='utf-8') as fh:
                            #     fh.write(
                            #         'Missing: {:s} {:s} {:s}\n'.format(
                            #             node.data, repr(fn_data), str(
                            #                 node_parent.index_wrap_hint)))
                        node_parent.hints = hints
                else:
                    if LOG_MISSING_DEFS is not None:
                        with open(LOG_MISSING_DEFS, 'a', encoding='utf-8') as fh:
                            fh.write('Missing: {:s}\n'.format(node.data))

    for node in node_parent.nodes_only_code:
        if isinstance(node, NdSexp):
            apply_rules(defs, node)
        if node.force_newline:
            node_parent.force_newline = True

    node_parent.flush_newlines_from_nodes()


def apply_pre_indent_1(cfg: FormatConfig, node_parent: NdSexp, level: int, trailing_parens: int) -> None:

    # First be relaxed, then again if it fails.
    if node_parent.fmt_check_exceeds_colum_max(cfg, level, trailing_parens):
        apply_relaxed_wrap(node_parent)
        node_parent.force_newline = True


def apply_pre_indent_2(cfg: FormatConfig, node_parent: NdSexp, level: int, trailing_parens: int) -> None:

    # Wrap items before if absolutely needed, one at a time.
    force_newline = False
    if node_parent.index_wrap_hint > 0:
        i = min(node_parent.index_wrap_hint, len(node_parent.nodes_only_code) - 1)
        if i > 0:
            node = node_parent.nodes_only_code[i]
            if node_parent.fmt_check_exceeds_colum_max(cfg, level, trailing_parens, test_node_terminate=node):
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
                        force_newline = True
                        if not node_parent.fmt_check_exceeds_colum_max(
                                cfg,
                                level,
                                trailing_parens,
                                test_node_terminate=node,
                        ):
                            # Imply 'node_parent.wrap_all_or_nothing_hint', even when not set.
                            hints = node_parent.hints
                            if hints is not None and hints.get('break_point') == 'overflow':
                                pass
                            else:
                                # TODO: warn about unknown break_point.

                                # When the break was added before `node_parent.index_wrap_hint`,
                                # wrap everything so there are no breaks added in random locations
                                # that might seems significant.
                                if i < node_parent.index_wrap_hint:
                                    for j in range(1, i):
                                        node_parent.nodes_only_code[j].force_newline = True
                            break
                    i -= 1

    if force_newline:
        node_parent.force_newline = True

    # If after wrapping 'everything', we still overflow,
    # don't use  this for tests in future, it confuses checks
    # causing other lines to wrap because of this node.
    if node_parent.fmt_check_exceeds_colum_max(cfg, level, trailing_parens):
        node_parent.test_blacklist = True


def apply_pre_indent(cfg: FormatConfig, node_parent: NdSexp, level: int, trailing_parens: int) -> None:
    '''
    For lists that will need wrapping even when all parents are wrapped,
    wrap these beforehand.
    '''
    if not node_parent.nodes_only_code:
        return

    node_trailing_parens = node_parent.node_last_for_trailing_parens_test()

    for node in node_parent.nodes_only_code:
        if isinstance(node, NdSexp):
            apply_pre_indent(cfg, node, level + 1, trailing_parens + 1 if node is node_trailing_parens else 0)
            if node.force_newline:
                node_parent.force_newline = True

    apply_pre_indent_1(cfg, node_parent, level, trailing_parens)
    apply_pre_indent_2(cfg, node_parent, level, trailing_parens)

    # Some blocks don't allow mixed wrapping.
    if node_parent.wrap_all_or_nothing_hint:
        if node_parent.is_multiline():
            apply_relaxed_wrap(node_parent)


# ------------------------------------------------------------------------------
# Classes

# Immutable configuration,
# created from command line arguments.

class FormatConfig(NamedTuple):
    '''Configuration options relating to how the file should be formatted.'''
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
    hints: Optional[Dict[str, str]]


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
        'original_line',
        'test_blacklist',
    )

    force_newline: bool
    original_line: int
    test_blacklist: bool

    def calc_force_newline(self) -> None:
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
        test_blacklist: bool

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
        'bracket_open',
        'bracket_close',
        'nodes',
        'nodes_only_code',
        'index_wrap_hint',
        'wrap_all_or_nothing_hint',
        'hints',
    )

    bracket_open: str
    bracket_close: str

    def __init__(self, line: int, nodes: Optional[List[Node]] = None):
        self.original_line = line
        self.prefix: str = ''
        # Leave bracket_open, bracket_close unset.
        # self.bracket_open = ''
        # self.bracket_close = ''
        self.nodes = nodes or []
        self.index_wrap_hint: int = 1
        self.wrap_all_or_nothing_hint: bool = False
        self.hints: Optional[Dict[str, str]] = None

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

    def flush_newlines_from_nodes(self) -> bool:
        changed = False
        for node in self.nodes_only_code:
            if node.force_newline:
                self.force_newline = True
                changed = True
                break
        return changed

    def flush_newlines_from_nodes_recursive(self) -> bool:
        changed = False
        for node in self.nodes_only_code:
            if node.force_newline:
                if not self.force_newline:
                    changed = True
                self.force_newline = True
            if isinstance(node, NdSexp):
                changed |= node.flush_newlines_from_nodes_recursive()
        return changed

    def calc_force_newline(self) -> None:
        force_newline = False
        node_prev = None
        for node in self.nodes:
            node.calc_force_newline()
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
        self.force_newline = force_newline

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

        self.nodes_only_code = [
            node for node in self.nodes
            if isinstance(node, (NdSymbol, NdString, NdSexp))
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
            test_node_terminate: Optional[Node] = None,
    ) -> bool:
        _ctx = WriteCtx(cfg)
        _data: List[str] = []
        self.fmt_with_terminate_node(_ctx, _data.append, level, test=True, test_node_terminate=test_node_terminate)
        data = ''.join(_data)
        del _data

        fill_column = cfg.fill_column
        line_terminate = _ctx.line_terminate

        # Step over `\n` characters instead of data.split('\n')
        # so multiple characters are handled separately.
        line_step = 0
        i = 0

        if (not cfg.use_trailing_parens) and (_ctx.line_terminate == _ctx.line):
            while line_step != -1:
                line_step_next = data.find('\n', line_step)
                if line_step_next == -1:
                    line_length = len(data) - line_step
                    line_step = -1
                else:
                    line_length = line_step_next - line_step
                    line_step = line_step_next + 1

                if line_terminate == i:
                    if line_length > fill_column - trailing_parens:
                        return True
                    return False
                if line_length > fill_column:
                    return True
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
                    return True
                i += 1

        return False

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
            if self.fmt_check_exceeds_colum_max(ctx.cfg, level, trailing_parens):
                if i == len(nodes_backup):
                    apply_relaxed_wrap(self)
                else:
                    self.nodes_only_code[0].force_newline = True

        # Disable newlines for individual items where possible.
        for i, node_only_code in enumerate(self.nodes_only_code):
            if nl[i] is False and node_only_code.force_newline is True:
                node_only_code.force_newline = False
                assert ctx.line_terminate == -1
                if self.fmt_check_exceeds_colum_max(
                        ctx.cfg,
                        level,
                        trailing_parens,
                        test_node_terminate=node_only_code,
                ):
                    node_only_code.force_newline = True
                else:
                    break

        node_trailing_parens = self.node_last_for_trailing_parens_test()

        force_newline = False
        for node in self.nodes:
            if isinstance(node, NdSexp):
                node.fmt_pre_wrap(ctx, level + 1, trailing_parens + 1 if node is node_trailing_parens else 0)
            force_newline |= node.force_newline

        if force_newline:
            self.force_newline = True

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

        level_next = level + 1

        if level != -1:
            if ctx.is_newline:
                write_fn('  ' * level)

            if self.prefix:
                write_fn(self.prefix)

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
                    write_fn('  ' * level)
                    ctx.is_newline = False

            write_fn(self.bracket_open)
            ctx.is_newline = False

        ctx.last_node = self

        is_first = True

        node_prev_is_multiline = False

        for node in self.nodes:
            if test:
                # Use only for testing.
                if node is test_node_terminate:
                    # We could return however this misses trailing parenthesis on the same line.
                    assert ctx.line_terminate == -1
                    ctx.line_terminate = ctx.line
                if getattr(node, 'test_blacklist', False):
                    continue

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

                    write_fn('  ' * level_next)
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
                write_fn('  ' * level_next)
                ctx.is_newline = False
            else:
                if (
                        (ctx.cfg.use_trailing_parens and ctx.line != line_sexpr_prev) or
                        isinstance(ctx.last_node, NdComment)
                ):
                    write_fn('\n')
                    ctx.line += 1
                    ctx.is_newline = True
                    write_fn('  ' * level_next)
                    ctx.is_newline = False
            write_fn(self.bracket_close)
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

    def calc_force_newline(self) -> None:
        # False because this forces it's own newline
        self.force_newline = True

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

    def calc_force_newline(self) -> None:
        self.force_newline = self.is_own_line

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

    def calc_force_newline(self) -> None:
        if USE_WRAP_LINES:
            self.force_newline = ((not self.data.startswith('\n')) and self.lines > 0)
        else:
            self.force_newline = False

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

    def calc_force_newline(self) -> None:
        self.force_newline = False

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


# ------------------------------------------------------------------------------
# File Parsing

def parse_file(cfg: FormatConfig, fh: TextIO) -> Tuple[str, NdSexp]:
    from io import StringIO

    line = 0

    # Fake top level S-expression to populate.
    root = NdSexp(line)
    # Unused, set to avoid issues with checks later.
    root.bracket_open = '('
    root.bracket_close = ')'

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

        if c in {'(', '['}:
            _ = NdSexp(line)
            _.bracket_open = c
            sexp_ctx[sexp_level].nodes.append(_)
            sexp_ctx.append(_)
            sexp_level += 1
            del _
            line_has_contents = True
        elif c in {')', ']'}:
            if sexp_level == 0:
                raise FmtException('additional closing brackets, line {}'.format(line))
            _ = sexp_ctx.pop()
            if _.bracket_open == '(':
                if c != ')':
                    raise FmtException(
                        'closing bracket "{:s}" line {:d}, unmatched bracket types, expected ")"'.format(c, line)
                    )
            else:
                if c != ']':
                    raise FmtException(
                        'closing bracket "{:s}" line {:d}, unmatched bracket types, expected "]"'.format(c, line)
                    )
            _.bracket_close = c
            sexp_level -= 1
            line_has_contents = True
        elif c == '"':
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
        elif c == ';':
            data = StringIO()
            while (c_peek := fh.read(1)) not in {'', '\n'}:
                c = c_peek
                c_peek = None
                data.write(c)

            is_own_line = not line_has_contents
            sexp_ctx[sexp_level].nodes.append(NdComment(line, data.getvalue(), is_own_line))
            del data, is_own_line
            line_has_contents = True
        elif c == '\n':
            line += 1
            # Respect blank lines up until the limit.
            if line_has_contents is False:
                sexp_ctx[sexp_level].nodes.append(NdWs(line))
            line_has_contents = False
        elif c in {' ', '\t'}:
            pass
        else:
            # Symbol
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
                    # Always include the next character even if it's normally a delimiting character such as ';', '"'
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

    root.calc_force_newline()

    apply_comment_force_newline(root)

    if USE_WRAP_LINES:

        if USE_EXTRACT_DEFS:
            if not cfg.defs._has_local:
                parse_local_defs(cfg.defs, root)
                cfg.defs._has_local = True

        apply_rules(cfg.defs, root)

        apply_pre_indent(cfg, root, -1, 0)

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
