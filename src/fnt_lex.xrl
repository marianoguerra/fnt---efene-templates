% New BSD License, part of efene, see LICENSE for details

Definitions.

Dolar      = \$
Open       = {
Close      = \}

Text       = .|[^\${}]*

Rules.

{Open}{Open}   : make_token(open_block, TokenLine, TokenChars).
{Close}{Close} : make_token(close_block, TokenLine, TokenChars).
{Dolar}{Open}  : make_token(open, TokenLine, TokenChars).
{Close}        : make_token(close, TokenLine, TokenChars).
{Text}         : make_token(text, TokenLine, TokenChars).

Erlang code.

make_token(Name, Line, Chars) ->
    {token, {Name, Line, Chars}}.
