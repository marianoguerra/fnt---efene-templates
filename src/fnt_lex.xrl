% New BSD License, part of efene, see LICENSE for details

Definitions.

OpenBlock  = {{
CloseBlock = }}
Open       = \${
Close      = }

Text       = .*

Rules.

{OpenBlock}  : make_token(open_block, TokenLine, TokenChars).
{CloseBlock} : make_token(close_block, TokenLine, TokenChars).
{Open}       : make_token(open, TokenLine, TokenChars).
{Close}      : make_token(close, TokenLine, TokenChars).
{Text}       : make_token(text, TokenLine, TokenChars).

Erlang code.

make_token(Name, Line, Chars) ->
    {token, {Name, Line, Chars}}.
