% New BSD License, part of efene, see LICENSE for details

Nonterminals
    expr bool_expr bool_and_expr comp_expr add_expr mul_expr literal bool_lit
    attrs prefix_number.

Terminals
    bool_or_op bool_and_op bool_not bin_not comp_op add_op or_op shift_op mul_op
    slash and_op identifier didentifier dot integer float string open close boolean.

Rootsymbol expr.

expr -> bool_expr : '$1'.

bool_expr -> bool_and_expr bool_or_op bool_expr : {op, line('$2'), op(unwrap('$2')), '$1', '$3'}.
bool_expr -> bool_and_expr                      : '$1'.

bool_and_expr -> comp_expr bool_and_op bool_and_expr : {op, line('$2'), op(unwrap('$2')), '$1', '$3'}.
bool_and_expr -> comp_expr                           : '$1'.

comp_expr -> add_expr comp_op add_expr : {op, line('$2'), op(unwrap('$2')), '$1', '$3'}.
comp_expr -> add_expr                  : '$1'.

add_expr -> add_expr add_op mul_expr   : {op, line('$2'), op(unwrap('$2')), '$1', '$3'}.
add_expr -> add_expr or_op mul_expr    : {op, line('$2'), op(unwrap('$2')), '$1', '$3'}.
add_expr -> add_expr shift_op mul_expr : {op, line('$2'), op(unwrap('$2')), '$1', '$3'}.
add_expr -> mul_expr                   : '$1'.

mul_expr -> mul_expr mul_op literal : {op, line('$2'), op(unwrap('$2')), '$1', '$3'}.
mul_expr -> mul_expr slash literal  : {op, line('$2'), op(unwrap('$2')), '$1', '$3'}.
mul_expr -> mul_expr and_op literal : {op, line('$2'), op(unwrap('$2')), '$1', '$3'}.
mul_expr -> literal                 : '$1'.

prefix_number -> add_op integer : {op, line('$2'), unwrap('$1'), '$2'}.
prefix_number -> add_op float   : {op, line('$2'), unwrap('$1'), '$2'}.
prefix_number -> bin_not integer: {op, line('$2'), op(unwrap('$1')), '$2'}.
prefix_number -> integer        : '$1'.
prefix_number -> float          : '$1'.


literal -> prefix_number        : '$1'.
literal -> bool_lit             : '$1'.
literal -> string               : {string,  line('$1'), unwrap('$1')}.
literal -> attrs                : gen_get_attrs('$1', line('$1'), []).
literal -> open expr close      : '$2'.
literal -> didentifier          : {var, line('$1'), unwrap('$1')}.
% TODO
% literal -> fun_call             : '$1'.

bool_lit -> boolean             : {atom, line('$1'), unwrap('$1')}.
bool_lit -> bool_not boolean    : {op, line('$2'), unwrap('$1'), {atom, line('$1'), unwrap('$1')}}.

attrs -> identifier dot attrs : ['$1'|'$3'].
attrs -> identifier: ['$1'].

Erlang code.

unwrap({_,V})   -> V;
unwrap({_,_,V}) -> V.

line(T) when is_tuple(T) -> element(2, T);
line([H|_T]) -> element(2, H).

op('%') -> 'rem';
op('<<') -> 'bsl';
op('>>') -> 'bsr';
op('<=') -> '=<';
op('===') -> '=:=';
op('!=') -> '/=';
op('!==') -> '=/=';
op('|') -> 'bor';
op('&') -> 'band';
op('^') -> 'bxor';
op('~') -> 'bnot';
op('and') -> 'andalso';
op('or') -> 'orelse';
op(Op) -> Op.

gen_get_attrs([], Line, Accum)->
    Attrs = lists:reverse(Accum),
    AttrsAst = erl_parse:abstract(Attrs, Line),

    {call, Line,
        {remote, Line,
            {atom, Line, fnt}, {atom, Line, get}}, [{var, Line, 'Context'}, AttrsAst]};

gen_get_attrs([{_, _, Name}|T], Line, Accum) ->
    gen_get_attrs(T, Line, [Name|Accum]).

