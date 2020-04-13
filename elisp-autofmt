#!/bin/env python3

# ##### BEGIN GPL LICENSE BLOCK #####
#
#  This program is free software; you can redistribute it and/or
#  modify it under the terms of the GNU General Public License
#  as published by the Free Software Foundation; either version 2
#  of the License, or (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software Foundation,
#  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
#
# ##### END GPL LICENSE BLOCK #####

import sys
import os


# ------------------------------------------------------------------------------
# Globals

# Disable mainly for testing what happens when we don't wrap lines at all.
USE_WRAP_LINES = True

# For debugging, check who added the newline.
TRACE_NEWLINES = False

# Extract argument count from functions and macros.
USE_EXTRACT_DEFS = True

# ------------------------------------------------------------------------------
# Utilities

def _function_id(num_frames_up):
    ''' Create a string naming the function n frames up on the stack.
    '''
    co = sys._getframe(num_frames_up + 1).f_code
    return '%d %s' % (co.co_firstlineno, co.co_name)


def execfile(filepath, mod=None):
    # module name isn't used or added to 'sys.modules'.
    # passing in 'mod' allows re-execution without having to reload.

    import importlib.util
    mod_spec = importlib.util.spec_from_file_location('__main__', filepath)
    if mod is None:
        mod = importlib.util.module_from_spec(mod_spec)
    mod_spec.loader.exec_module(mod)
    return mod


# ------------------------------------------------------------------------------
# Formatting Utilities

def apply_comment_force_newline(root):
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


def apply_relaxed_wrap(node_parent):
    node_prev = None
    is_multi_line = False

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
        is_multi_line |= node.force_newline

        node_prev = node


def apply_relaxed_wrap_when_multiple_args(node_parent):
    if len(node_parent.nodes_only_code) - node_parent.index_wrap_hint > 1:
        apply_relaxed_wrap(node_parent)


def parse_local_defs(defs, node_parent):
    # Extract the number of functions from local definitions.
    # Currently only used so we can break the number of arguments at '&rest'
    # since it's nearly always where we have the body of macros which is logically
    # where we want to break.
    if node_parent.nodes_only_code and node_parent.bracket_open == '(':
        node = node_parent.nodes[0]
        if isinstance(node, NdSymbol):
            if node.data in {'defun', 'defmacro'}:
                # Sanity check, should never fail.
                if len(node_parent.nodes) >= 3:
                    node_symbol = node_parent.nodes[1]
                    node_args = node_parent.nodes[2]
                    if isinstance(node_symbol, NdSymbol) and isinstance(node_args, NdSexp):
                        symbol = node_symbol.data
                        arg_index = -1
                        for i, node_arg in enumerate(node_args.nodes_only_code):
                            if isinstance(node_arg, NdSymbol):
                                if node_arg.data == '&rest':
                                    arg_index = i
                                    break
                        if arg_index != -1:
                            # The second value is currently unused (max arguments).
                            defs.fn_arity[symbol] = (arg_index, None)

    for node in node_parent.nodes_only_code:
        if isinstance(node, NdSexp):
            parse_local_defs(defs, node)


def apply_rules(defs, node_parent):

    # Optional
    if node_parent.nodes_only_code and node_parent.bracket_open == '(':
        node = node_parent.nodes[0]
        if isinstance(node, NdSymbol):

            node_parent.index_wrap_hint = 1

            # -------------------
            # Only wrap if needed
            #
            # Don't wrap, these may fit on one line.
            #
            if node.data in {'and', 'or'}:
                node_parent.index_wrap_hint = 1
            elif node.data in {
                    'with-auto-compression-mode',
                    'with-temp-buffer',
            }:
                node_parent.index_wrap_hint = 1
                node_parent.wrap_all_or_nothing_hint = True
            elif node.data in {
                    'add-hook',
                    'add-to-list',
                    'advice-add',
                    'advice-remove',
                    'apply',
                    'catch',
                    'custom-theme-set-faces',
                    'declare',
                    'defadvice',
                    'defalias',
                    'defclass',
                    'defconst',
                    'defface',
                    'define-minor-mode',
                    'defparameter',
                    'defstruct',
                    'defsubst',
                    'evil-leader/set-key',
                    'global-set-key',
                    'pcase',
                    'pcase-let',
                    'pcase-let*',
                    'setcar',
                    'setcdr',
            }:
                node_parent.index_wrap_hint = 2

            elif node.data in {
                    'with-current-buffer',
                    'with-demoted-errors',
                    'with-eval-after-load',
                    'with-syntax-table',
                    'with-timeout',
            }:
                node_parent.index_wrap_hint = 2
                node_parent.wrap_all_or_nothing_hint = True

            elif node.data == 'lambda':
                if (
                        len(node_parent.nodes_only_code) >= 3 and
                        isinstance(node_parent.nodes_only_code[2], NdString)
                ):
                    node_parent.index_wrap_hint = 3
                else:
                   node_parent.index_wrap_hint = 2
                node_parent.wrap_all_or_nothing_hint = True

            elif node.data in {
                    'cl-destructuring-bind',
                    'defcustom',
                    'defgroup',
                    'define-key',
                    'defvar',
                    'defvar-local',
            }:
                node_parent.index_wrap_hint = 3

            #
            # Wrap these always
            #
            elif node.data in {'if', 'prog1'}:
                node_parent.index_wrap_hint = 2
                apply_relaxed_wrap(node_parent)
            elif node.data in {
                    'condition-case',
                    'dolist',
                    'dotimes',
                    'unless',
                    'use-package',
                    'when',
                    'while',
            }:
                node_parent.index_wrap_hint = 2
                apply_relaxed_wrap(node_parent)
            elif node.data == 'progn':
                node_parent.index_wrap_hint = 1
                apply_relaxed_wrap(node_parent)
            elif node.data in {
                    'eval-when-compile',
                    'save-excursion',
                    'save-match-data',
                    'save-restriction',
                    'save-window-excursion',
            }:
                apply_relaxed_wrap_when_multiple_args(node_parent)
            elif node.data in {
                    'cl-letf',
                    'cl-letf*',
                    'let',
                    'let*',
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
                    'defmacro',
                    'defun',
            }:
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
                arity_min, arity_max = defs.fn_arity.get(node.data, (None, None))
                if arity_min is not None:
                    # First symbol counts for 1.
                    node_parent.index_wrap_hint = 1 + arity_min
                else:
                    if defs._is_complete:
                        print('Missing:', node.data)

    for node in node_parent.nodes_only_code:
        if isinstance(node, NdSexp):
            apply_rules(defs, node)
        if node.force_newline:
            node_parent.force_newline = True

    node_parent.flush_newlines_from_nodes()


def apply_pre_indent_1(cfg, node_parent, level):

    # First be relaxed, then again if it fails.
    if node_parent.fmt_check_exceeds_colum_max(cfg, level):
        apply_relaxed_wrap(node_parent)
        node_parent.force_newline = True


def apply_pre_indent_2(cfg, node_parent, level):

    # Wrap items before if absolutely needed, one at a time.
    if node_parent.index_wrap_hint > 0:
        i = node_parent.index_wrap_hint
        if i >= len(node_parent.nodes_only_code):
            i = len(node_parent.nodes_only_code) - 1
        node = node_parent.nodes_only_code[i]
        if node_parent.fmt_check_exceeds_colum_max(cfg, level, test_node_terminate=node):
            while i != -1:
                node = node_parent.nodes_only_code[i]
                if not node.force_newline:
                    node.force_newline = True
                if not node_parent.fmt_check_exceeds_colum_max(
                        cfg, level, test_node_terminate=node,
                ):
                    break
                i -= 1

    # If after wrapping 'everything', we still overflow,
    # don't use  this for tests in future, it confuses checks
    # causing other lines to wrap because of this node.
    if node_parent.fmt_check_exceeds_colum_max(cfg, level):
        node_parent.test_blacklist = True


def apply_pre_indent(cfg, node_parent, level):
    '''
    For lists that will need wrapping even when all parents are wrapped,
    wrap these beforehand.
    '''
    if not node_parent.nodes_only_code:
        return

    for node in node_parent.nodes_only_code:
        if isinstance(node, NdSexp):
            apply_pre_indent(cfg, node, level + 1)
            if node.force_newline:
                node_parent.force_newline = True

    apply_pre_indent_1(cfg, node_parent, level)
    apply_pre_indent_2(cfg, node_parent, level)

    # Some blocks don't allow mixed wrapping.
    if node_parent.wrap_all_or_nothing_hint:
        if node_parent.is_multiline():
            apply_relaxed_wrap(node_parent)


# ------------------------------------------------------------------------------
# Classes

# Immutable configuration,
# created from command line arguments.

from collections import namedtuple
FormatConfig = namedtuple(
    'FormatConfig',
    (
        'use_trailing_parens',
        'fill_column',
        'empty_lines',
        'defs',
    ),
)
del namedtuple


class WriteCtx:
    __slots__ = (
        'last_node',
        'is_newline',
        'line',
        'cfg',
    )

    def __init__(self, cfg):
        self.last_node = None
        self.is_newline = True
        self.line = 0
        self.cfg = cfg


class Node:
    __slots__ = (
        *(
            (
                'force_newline',
            )
            if not TRACE_NEWLINES  else
            (
                '_force_newline',
                '_force_newline_tracepoint',
            )
        ),
        'original_line',
        'test_blacklist',
    )

    if TRACE_NEWLINES:
        @property
        def force_newline(self):
            return self._force_newline

        @force_newline.setter
        def force_newline(self, value):
            assert(value is True or value is False)
            if getattr(self, '_force_newline', None) != value:
                if value:
                    self._force_newline_tracepoint = (
                        _function_id(1) + ' ' +
                        _function_id(2)
                    )
            self._force_newline = value


class NdSexp(Node):
    __slots__ = (
        'prefix',
        'bracket_open',
        'bracket_close',
        'nodes',
        'nodes_only_code',
        'index_wrap_hint',
        'wrap_all_or_nothing_hint',
    )

    def __init__(self, line, nodes=None):
        self.original_line = line
        self.prefix = ''
        self.bracket_open = None
        self.bracket_close = None
        self.nodes = nodes or []
        self.index_wrap_hint = 1
        self.wrap_all_or_nothing_hint = False

    def __repr__(self):
        return '{}(line={} prefix=({}) newline={}\n  {}\n)'.format(
            self.__class__.__name__,
            self.original_line,
            self.prefix,
            self.force_newline,
            '\n  '.join(repr(node) for node in self.nodes),
        )

    def is_multiline(self):
        for node in self.iter_nodes_recursive():
            if node.force_newline:
                return True
        return False

    def iter_nodes_recursive(self):
        for node in self.nodes:
            yield node
            if isinstance(node, NdSexp):
                yield from node.iter_nodes_recursive()

    def iter_nodes_recursive_with_parent(self):
        for node in self.nodes:
            yield (node, self)
            if isinstance(node, NdSexp):
                yield from node.iter_nodes_recursive_with_parent()

    def flush_newlines_from_nodes(self):
        changed = False
        for node in self.nodes_only_code:
            if node.force_newline:
                self.force_newline = True
                changed = True
                break
        return changed

    def flush_newlines_from_nodes_recursive(self):
        changed = False
        for node in self.nodes_only_code:
            if node.force_newline:
                self.force_newline = True
            if isinstance(node, NdSexp):
                changed |= node.flush_newlines_from_nodes_recursive()
        return changed

    def calc_force_newline(self):
        force_newline = False
        node_prev = None
        for node in self.nodes:
            node.calc_force_newline()
            if node_prev and isinstance(node_prev, NdComment):
                node.force_newline = True
            force_newline |= node.force_newline
            node_prev = node
        self.force_newline = force_newline

    def finalize(self, cfg):
        # Connect: ' (  to '(
        i = len(self.nodes) - 1
        while i > 0:
            node = self.nodes[i]
            if isinstance(node, NdSexp):
                node_prev = self.nodes[i - 1]
                if isinstance(node_prev, NdSymbol):
                    if not node_prev.data.strip('#,`\''):
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

    def fmt_check_exceeds_colum_max(self, cfg, level, *, test_node_terminate=None):
        from io import StringIO
        _ctx = WriteCtx(cfg)
        _fh = StringIO()
        self.fmt(_ctx, _fh, level, test=True, test_node_terminate=test_node_terminate)
        _fh.seek(0, os.SEEK_SET)
        data = _fh.read()
        ok = True
        for line in data.split('\n'):
            if len(line) > cfg.fill_column:
                return True
        return False

    def fmt_pre_wrap(self, ctx, level):

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

            if self.fmt_check_exceeds_colum_max(ctx.cfg, level):
                if i == len(nodes_backup):
                    apply_relaxed_wrap(self)
                else:
                    self.nodes_only_code[0].force_newline = True

        # Disable newlines for individual items where possible.
        for i in range(len(self.nodes_only_code)):
            if nl[i] is False and self.nodes_only_code[i].force_newline is True:
                self.nodes_only_code[i].force_newline = False
                if self.fmt_check_exceeds_colum_max(
                        ctx.cfg,
                        level,
                        test_node_terminate=self.nodes_only_code[i],
                ):
                    self.nodes_only_code[i].force_newline = True
                    break

        for node in self.nodes:
            if isinstance(node, NdSexp):
                node.fmt_pre_wrap(ctx, level + 1)

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

            node_prev = None
            for node in self.nodes_only_code:
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

                node_prev = node

    def fmt(self, ctx, fh, level, *, test=False, test_node_terminate=None):

        line_sexpr_prev = ctx.line

        level_next = level + 1

        if level != -1:
            if ctx.is_newline:
                fh.write('  ' * level)

            if self.prefix:
                fh.write(self.prefix)


                if self.is_multiline():
                    fh.write('\n')

                    if TRACE_NEWLINES:
                        if self.force_newline:
                            if not test:
                                fh.write(' $' + self._force_newline_tracepoint)

                    ctx.line += 1
                    ctx.is_newline = True
                    fh.write('  ' * level)
                    ctx.is_newline = False

            fh.write(self.bracket_open)
            ctx.is_newline = False

        ctx.last_node = self

        is_first = True

        node_prev_is_multiline = False

        for node in self.nodes:
            if test:
                # Use only for testing.
                if node is test_node_terminate:
                    return
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
                    fh.write('\n')
                    ctx.line += 1
                    ctx.is_newline = True

            if isinstance(node, NdWs):
                pass
            else:
                if ctx.is_newline:

                    if TRACE_NEWLINES:
                        if node.force_newline:
                            if not test:
                                fh.write(' $' + node._force_newline_tracepoint)

                    fh.write('  ' * level_next)
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
                        fh.write(' ')

            line = ctx.line

            node.fmt(ctx, fh, level_next, test=test)
            node_prev_is_multiline = (line != ctx.line)

            ctx.last_node = node

            is_first = False

        if level != -1:
            if ctx.is_newline:
                fh.write('  ' * level_next)
                ctx.is_newline = False
            else:
                if (
                        (ctx.cfg.use_trailing_parens and ctx.line != line_sexpr_prev) or
                        isinstance(ctx.last_node, NdComment)
                ):
                    fh.write('\n')
                    ctx.line += 1
                    ctx.is_newline = True
                    fh.write('  ' * level_next)
                    ctx.is_newline = False
            fh.write(self.bracket_close)
            ctx.is_newline = False



# ------------------------------------------------------------------------------
# Formatting Utilities

# Currently this always represents a blank line.
class NdWs(Node):
    __slots__ = ()

    def __init__(self, line):
        self.original_line = line
        pass

    def __repr__(self):
        return '{}(line={})'.format(
            self.__class__.__name__,
            self.original_line,
        )

    def calc_force_newline(self):
        # False because this forces it's own newline
        self.force_newline = True

    def fmt(self, ctx, fh, level, test=False):
        fh.write('\n')
        ctx.line += 1
        ctx.is_newline = True


class NdComment(Node):
    __slots__ = (
        'data',
        'is_own_line',
    )

    def __init__(self, line, data, is_own_line):
        self.original_line = line
        self.data = data
        self.is_own_line = is_own_line

    def __repr__(self):
        return '{}(line={} \'{}\')'.format(
            self.__class__.__name__,
            self.original_line,
            self.data,
        )

    def calc_force_newline(self):
        self.force_newline = self.is_own_line

    def fmt(self, ctx, fh, level, *, test=False):
        fh.write(';')
        fh.write(self.data)
        ctx.is_newline = False


class NdString(Node):
    __slots__ = (
        'data',
        'lines',
    )

    def __init__(self, line, data):
        self.original_line = line
        self.data = data
        self.lines = self.data.count('\n')

    def __repr__(self):
        return '{}(line={} \'{}\')'.format(
            self.__class__.__name__,
            self.original_line,
            self.data,
        )

    def calc_force_newline(self):
        if USE_WRAP_LINES:
            self.force_newline = ((not self.data.startswith('\n')) and self.lines > 0)
        else:
            self.force_newline = False

    def fmt(self, ctx, fh, level, *, test=False):
        fh.write('"')
        fh.write(self.data)
        fh.write('"')
        ctx.is_newline = False
        ctx.line += self.lines


class NdSymbol(Node):
    __slots__ = (
        'data',
    )

    def __init__(self, line, data):
        self.original_line = line
        self.data = data

    def __repr__(self):
        return '{}(line={} \'{}\')'.format(
            self.__class__.__name__,
            self.original_line,
            self.data,
        )

    def calc_force_newline(self):
        self.force_newline = False

    def fmt(self, ctx, fh, level, *, test=False):
        fh.write(self.data)
        ctx.is_newline = False


# ------------------------------------------------------------------------------
# File Parsing

def parse_file(cfg, fh):
    from io import StringIO

    line = 0

    # Fake top level s-expression to poppulate.
    root = NdSexp(line)
    # Unused, set to avoid issues with checks later.
    root.bracket_open = '('
    root.bracket_close = ')'

    # Current s-expressions.
    sexp_ctx = [root]
    sexp_level = 0

    line_has_contents = True

    c_peek = None
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
            _ = sexp_ctx.pop()
            _.bracket_close = c
            assert((_.bracket_open + _.bracket_close) in {'()', '[]'})
            sexp_level -= 1
            line_has_contents = True
        elif c == '"':
            data = StringIO()
            is_slash = False
            while (c:= fh.read(1)):
                if c == '"' and not is_slash:
                    break
                data.write(c)
                if c == '\n':
                    line += 1
                elif c == '\\':
                    is_slash = not is_slash
                else:
                    is_slash = False

            if not c:
                raise Exception('Error parsing string at line {}'.format(line))

            data.seek(0, os.SEEK_SET)
            _ = NdString(line, data.read())
            sexp_ctx[sexp_level].nodes.append(_)
            del _, data, is_slash, c
            line_has_contents = True
        elif c == ';':
            data = StringIO()
            while (c_peek := fh.read(1)) not in {'', '\n'}:
                c = c_peek
                c_peek = None
                data.write(c)

            is_own_line = not line_has_contents
            data.seek(0, os.SEEK_SET)
            _ = NdComment(line, data.read(), is_own_line)
            sexp_ctx[sexp_level].nodes.append(_)
            del _, data, is_own_line
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
                    if c_peek in {'(', ')', '[', ']', ';', ' ', '\t'}:
                        break

                c = c_peek
                c_peek = None

            data.seek(0, os.SEEK_SET)
            _ = NdSymbol(line, data.read())
            sexp_ctx[sexp_level].nodes.append(_)
            del _, data, is_slash
            line_has_contents = True

    if sexp_level != 0:
        raise Exception('Unbalanced parentheses, expected 0, found:', sexp_level)

    root.finalize(cfg)

    root.calc_force_newline()

    apply_comment_force_newline(root)

    if USE_WRAP_LINES:

        if USE_EXTRACT_DEFS:
            if not cfg.defs._has_local:
                parse_local_defs(cfg.defs, root)
                cfg.defs._has_local = True

        apply_rules(cfg.defs, root)

        apply_pre_indent(cfg, root, -1)

        # All root level nodes get their own line always.
        for node in root.nodes_only_code:
            node.force_newline = True

    return root


def format_file(
        filepath,
        cfg,
        *,
        use_stdin=False,
):
    if use_stdin:
        root = parse_file(cfg, sys.stdin)
    else:
        with open(filepath, 'r', encoding='utf-8') as fh:
            root = parse_file(cfg, fh)

    assert(root.flush_newlines_from_nodes_recursive() == False)

    with open(filepath, 'w', encoding='utf-8') as fh:
        ctx = WriteCtx(cfg)

        if USE_WRAP_LINES:
            root.fmt_pre_wrap(ctx, -1)
        root.fmt(ctx, fh, -1)
        fh.write('\n')


# ------------------------------------------------------------------------------
# Argument Parsing

def argparse_create():
    import argparse

    # When --help or no args are given, print this help
    usage_text = 'Format emacs-lisp.'

    epilog = (
        'This program formats emacs lisp, from the standard input, '
        'or operating on files, in-place.'
    )

    parser = argparse.ArgumentParser(description=usage_text, epilog=epilog)

    parser.add_argument(
        '--fmt-defs',
        dest='fmt_defs',
        metavar='FILE',
        default='',
        type=str,
        required=False,
        help='Definitions (internal use, this is written by Emacs).',
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
        help='Maxumum column width.',
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
        'files',
        nargs=argparse.REMAINDER,
        help='All trailing arguments are treated as file paths to format.'
    )
    return parser


# ------------------------------------------------------------------------------
# Main Function

def main():
    args = argparse_create().parse_args()

    if args.use_stdin and len(args.files) != 1:
        sys.stderr.write('The \'--stdin\' argument can only output to a single file {!r}\n'.format(args.files))
        sys.exit(1)

    if args.fmt_defs:
        defs = execfile(args.fmt_defs)
        defs._is_complete = True
    else:
        from types import ModuleType
        defs = ModuleType('defs')
        defs.fn_arity = {}
        defs._is_complete = False
    defs._has_local = False

    cfg = FormatConfig(
        use_trailing_parens=args.fmt_use_trailing_parens,
        fill_column=args.fmt_fill_column,
        empty_lines=args.fmt_empty_lines,
        defs=defs,
    )

    for filepath in args.files:
        if not args.use_quiet:
            print(filepath)

        format_file(
            filepath,
            cfg=cfg,
            use_stdin=args.use_stdin,
        )


if __name__ == '__main__':
    main()
