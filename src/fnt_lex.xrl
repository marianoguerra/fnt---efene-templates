% New BSD License, part of efene, see LICENSE for details

Definitions.

Identifier = ([a-zA-Z_][a-zA-Z0-9\_@]*)
Dot        = \.
Slash      = /
White      = [\s|\t]+

% binary operators
BinOr       = (\||\^)
BinAnd      = \&
Shift       = (<<|>>)
BinNot      = ~

% bool operators
BoolAnd     = &&
BoolOr      = ||
BoolNot     = !
Bool        = (true|false)

% arithmetic operators
Add         = (\+|-)
Mul         = (\*|//|%)
Comp        = (<|<=|==|===|>=|>|!=|!==)

% numbers
Number      = [0-9]
Float       = [0-9]+\.[0-9]+([eE][-+]?[0-9]+)?

% delimiters and operators
Open        = \(
Close       = \)
OpenList    = \[
CloseList   = \]

String      = "(\\\^.|\\.|[^\"])*"

Rules.

\${Identifier} : make_token(didentifier, TokenLine, d_to_var_name(TokenChars)).
{Identifier}   : make_token(identifier, TokenLine, TokenChars).
{Dot}          : make_token(dot, TokenLine, TokenChars).
{Slash}        : make_token(slash, TokenLine, TokenChars).
{White}        : skip_token.

% binary operators
{Shift}                  : make_token(shift_op, TokenLine, TokenChars).
{BinNot}                 : make_token(bin_not,  TokenLine, TokenChars).
{BinAnd}                 : make_token(and_op,   TokenLine, TokenChars).
{BinOr}                  : make_token(or_op,    TokenLine, TokenChars).

% bool operators
{BoolAnd}                : make_token(bool_and_op, TokenLine, TokenChars).
{BoolOr}                 : make_token(bool_or_op,  TokenLine, TokenChars).
{BoolNot}                : make_token(bool_not,     TokenLine, TokenChars).
{Bool}                   : make_token(boolean,      TokenLine, TokenChars).

% arithmetic operators
{Add}                    : make_token(add_op,  TokenLine, TokenChars).
{Mul}                    : make_token(mul_op,  TokenLine, TokenChars).

{Comp}                   : make_token(comp_op, TokenLine, TokenChars).

% numbers
{Float}                  : make_token(float,   TokenLine, TokenChars, fun erlang:list_to_float/1).
{Number}+                : make_token(integer, TokenLine, TokenChars, fun erlang:list_to_integer/1).

% delimiters and operators
{Open}                   : make_token(open,        TokenLine, TokenChars).
{Close}                  : make_token(close,       TokenLine, TokenChars).
{OpenList}               : make_token(open_list,   TokenLine, TokenChars).
{CloseList}              : make_token(close_list,  TokenLine, TokenChars).

% string stuff
{String}                 : build_string(string, TokenChars, TokenLine, TokenLen).

Erlang code.

make_token(Name, Line, Chars) when is_list(Chars) ->
    {token, {Name, Line, list_to_atom(Chars)}};
make_token(Name, Line, Chars) ->
    {token, {Name, Line, Chars}}.

make_token(Name, Line, Chars, Fun) ->
    {token, {Name, Line, Fun(Chars)}}.

build_string(Type, Chars, Line, Len) ->
  String = unescape_string(lists:sublist(Chars, 2, Len - 2), Line),
    {token, {Type, Line, String}}.

unescape_string(String, Line) -> unescape_string(String, Line, []).

unescape_string([], _Line, Output) ->
  lists:reverse(Output);
unescape_string([$\\, Escaped | Rest], Line, Output) ->
  Char = map_escaped_char(Escaped, Line),
  unescape_string(Rest, Line, [Char|Output]);
unescape_string([Char|Rest], Line, Output) ->
  unescape_string(Rest, Line, [Char|Output]).

d_to_var_name([$$|[First|Rest]]) ->
    if
        First >= $a andalso First =< $z ->
            [First - 32|Rest];
        true ->
            [First|Rest]
    end.


map_escaped_char(Escaped, Line) ->
  case Escaped of
    $\\ -> $\\;
    $/ -> $/;
    $\" -> $\";
    $\' -> $\';
    $\( -> $(;
    $b -> $\b;
    $d -> $\d;
    $e -> $\e;
    $f -> $\f;
    $n -> $\n;
    $r -> $\r;
    $s -> $\s;
    $t -> $\t;
    $v -> $\v;
    _ -> throw({error, {Line, fn_lexer, ["unrecognized escape sequence: ", [$\\, Escaped]]}})
  end.

