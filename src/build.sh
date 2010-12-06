#!/usr/bin/env sh
erlc fnt_lex.xrl
erlc fnt_lex.erl
erlc fnt_parser.yrl
erlc fnt_parser.erl
erlc fnt.erl
mv *.beam ../ebin
rm fnt_parser.erl
rm fnt_lex.erl
