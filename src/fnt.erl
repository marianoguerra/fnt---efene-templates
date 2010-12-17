-module(fnt).
-export([get/2, each/2, escape/1, from_file/1, parse/1, to_module/2, to_erlang/1, compile/2, to_ast/3,
        bin_to_file/2, build/2, build/3]).

% TODO:
%  - separate external from internal functions inside this module
%  - erl docs?
%  - simplify external API
%  - use real line numbers where dummy line numbers are used

% parse a template that comes as a string
parse(String) when is_list(String) ->
    parse(list_to_binary(String));

% parse a template that comes as a binary
parse(Binary) when is_binary(Binary) ->
    Tokens = parse(Binary, [], false, false),
    %io:format("tokens: ~p~n", [Tokens]),
    Compacted = compact(Tokens),
    %io:format("compacted: ~p~n", [Compacted]),
    Grouped = group(Compacted),
    %io:format("grouped: ~p~n", [Grouped]),
    Structured = group_blocks(Grouped),
    % put all the expressions as a list to make it a io_list
    % XXX Line number
    Consified  = exprs_to_io_list(3, Structured),

    Consified.

exprs_to_io_list(Line, [LastAst]) ->
    {cons, Line, LastAst, {nil, Line}};

exprs_to_io_list(Line, [Ast|Asts]) ->
    {cons, Line, Ast, exprs_to_io_list(Line, Asts)}.

% parse a template from a file
from_file(Path) ->
    case file:read_file(Path) of
        {ok, Binary} ->
            parse(Binary);

        {error, _Reason} = Error -> Error
    end.

% generate the ast for a module *ModName* that has a public functions in
% *FunsAst*
to_module(ModName, FunsAst) ->
    [{attribute, 1, module, ModName},
         {attribute, 2, export, exports_from_fun_list(FunsAst)}|FunsAst].

% return a list of tuples containing function names and arities from a list of
% function definitions
exports_from_fun_list(FunsAst) ->
    exports_from_fun_list(FunsAst, []).

exports_from_fun_list([], Accum) ->
    lists:reverse(Accum);

exports_from_fun_list([H|T], Accum) ->
    FunName  = element(3, H),
    FunArity = element(4, H),
    exports_from_fun_list(T, [{FunName, FunArity}|Accum]).

% generate the ast for a function *FunName* that when called with the Context
% parameter runs *Ast*
to_function(Line, FunName, Ast) ->
    {function, Line, FunName, 1,
        [{clause, Line, [{var, Line, 'Context'}], [], Ast}]}.

% convert the generated ast to erlang code
to_erlang(Ast) ->
    erl_prettypr:format(erl_syntax:form_list(Ast)).

% same as build/3 but compiles to the current working directory
build(ModName, Templates) ->
    build(ModName, Templates, ".").

% compile templates like compile/2 and save it in DirPath (the module name and .beam is automatically
% appended)
build(ModName, Templates, DirPath) ->
    Mod = fnt:compile(ModName, Templates),
    fnt:bin_to_file(Mod, DirPath ++ "/" ++ atom_to_list(ModName) ++ ".beam").

% compile a module *ModName* with functions taken from a property list where
% keys are function names and values are file paths to get the templates from
compile(ModName, Templates) ->
    Ast = to_ast(ModName, Templates, []),
    to_code(Ast).

% transform the Ast to bytecode
to_code(Ast) ->
    case compile:forms(Ast) of
        {ok, _, Code} -> Code;
        {ok, _, Code, _} -> Code;
        Error -> throw(Error)
    end.

bin_to_file(Bin, Path) ->
    to_file(Bin, Path, [binary, write]).

to_file(Data, Path, Mode) ->
    Device = case file:open(Path, Mode) of
        {ok, Return} -> Return;
        {error, _Reason} = Error -> throw(Error)
    end,

    file:write(Device, Data).

to_ast(ModName, [], Accum) ->
    to_module(ModName, lists:reverse(Accum));

to_ast(ModName, [{FunName, Path}|Templates], Accum) ->
    Code = case file:read_file(Path) of
        {ok, Return} -> Return;
        {error, _Reason} = Error -> throw(Error)
    end,

    % XXX Line number
    Ast = to_function(2, FunName, [parse(Code)]),

    to_ast(ModName, Templates, [Ast|Accum]).

% if we get "{{" but we are Expecting a "}" then fail
parse(<<${, ${, _Rest/binary>>, _Accum, true, _ExpectCloseBlock) ->
    throw({parser_error, {expected_close, "{{"}});

% if we get "{{" but we are Expecting a "}}" then fail
parse(<<${, ${, _Rest/binary>>, _Accum, false, true) ->
    throw({parser_error, "expecting }} got {{"});

% if we find a "{{" and we are no expecting a close block insert it and set
% ExpectCloseBlock to true
parse(<<${, ${, Rest/binary>>, Accum, ExpectClose, false) ->
    parse(Rest, [open_block|Accum], ExpectClose, true);

% if we find a "}}" and we are expecting a close block insert it and set
% ExpectCloseBlock to false
parse(<<$}, $}, Rest/binary>>, Accum, ExpectClose, true) ->
    parse(Rest, [close_block|Accum], ExpectClose, false);

% if we find a "${" and we are expecting a "}" fail
parse(<<$$, ${, _Rest/binary>>, _Accum, true, _ExpectCloseBlock) ->
    throw({parser_error, nested_value_open});

% if we find a "${", append it and set ExpectClose to true
parse(<<$$, ${, Rest/binary>>, Accum, false, ExpectCloseBlock) ->
    parse(Rest, [open|Accum], true, ExpectCloseBlock);

% if we find a "}" and we are expecting it insert it and set ExpectClose to
% false
parse(<<$}, Rest/binary>>, Accum, true, ExpectCloseBlock) ->
    parse(Rest, [close|Accum], false, ExpectCloseBlock);

% the next 3 clauses match the different kinds of new lines (MS, Apple, *nix)
% and insert an atom that signals a new line
parse(<<$\r, $\n, Rest/binary>>, Accum, ExpectClose, ExpectCloseBlock) ->
    parse(Rest, [endl|Accum], ExpectClose, ExpectCloseBlock);

parse(<<$\r, Rest/binary>>, Accum, ExpectClose, ExpectCloseBlock) ->
    parse(Rest, [endl|Accum], ExpectClose, ExpectCloseBlock);

parse(<<$\n, Rest/binary>>, Accum, ExpectClose, ExpectCloseBlock) ->
    parse(Rest, [endl|Accum], ExpectClose, ExpectCloseBlock);

% of we get any char that doesn't match any of the above insert it
parse(<<Char, Rest/binary>>, Accum, ExpectClose, ExpectCloseBlock) ->
    parse(Rest, [Char|Accum], ExpectClose, ExpectCloseBlock);

% if we end parsing and we are still expecting something to be closed fail
parse(<<>>, _Accum, true, _ExpectCloseBlock) ->
    throw({parser_error, expected_close_but_end});

parse(<<>>, _Accum, false, true) ->
    throw({parser_error, expected_close_block_but_end});

% if we end correctly compact the result into a nicer representation
parse(<<>>, Accum, false, false) ->
    lists:reverse(Accum).


compact(List) ->
    compact(List, [], [], 1).

% if we end and have nothing in TAccum
compact([], Accum, [], _Line) ->
    lists:reverse(Accum);

% if we end and have something in TAccum
compact([], Accum, TAccum, Line) ->
    String = lists:reverse(TAccum),
    lists:reverse([{string, Line, String}|Accum]);

% if we find an end line indicator we insert a new line in TAccum and
% increase the line counter by one
compact([endl|T], Accum, TAccum, Line) ->
    compact(T, Accum, [$\n|TAccum], Line + 1);

% if we find an atom and there is nothing in the TAccum we just prepend the
% atom
compact([H|T], Accum, []=TAccum, Line) when is_atom(H) ->
    compact(T, [{H, Line}|Accum], TAccum, Line);

% if we find an atom and there is something in TAccum we prepend the content of
% TAccum as a string and then prepend the atom
compact([H|T], Accum, TAccum, Line) when is_atom(H) ->
    String = lists:reverse(TAccum),
    compact(T, [{H, Line}|[{string, Line, String}|Accum]], [], Line);

% if we get a character we prepend it to TAccum
compact([H|T], Accum, TAccum, Line) when is_integer(H) ->
    compact(T, Accum, [H|TAccum], Line).

% group the items between open and close inside them
group(List) ->
    group(List, []).

group([], Accum) ->
    lists:reverse(Accum);

group([{open, Line}|T], Accum) ->
    subgroup(T, close, value, Line, Accum);

group([{open_block, Line}|T], Accum) ->
    subgroup(T, close_block, block, Line, Accum);

group([H|T], Accum) ->
    group(T, [H|Accum]).

subgroup(List, Until, Name, Line, Accum) ->
    {Rest, Until, _, SubGroup} = group_until(Until, List),

    Lexed = case length(SubGroup) of
        1 ->
            [{string, _, Content}] = SubGroup,
            case fnt_lex:string(Content) of
                {ok, Tokens, _} -> Tokens;
                {error, {Line, fnt_lex, Error}, _} ->
                    throw({parser_error, {lex, Line, Error}})
            end;
        _ ->
            % TODO: better error
            throw({parser_error, invalid_group})
    end,

    {NewName, Expr} = case Name of
        block ->
            get_identifier_and_expression(Lexed);
        value ->
            {value, Lexed}
    end,

    ParsedExpr = if
        length(Expr) > 0 ->
            case fnt_parser:parse(Expr) of
                {ok, Result} ->
                    Result;
                {error, {LineNumber, _Module, Message}} ->
                    throw({parser_error, LineNumber, Message})
            end;
        true ->
            []
    end,

    Final = if
        NewName == value -> gen_escape(ParsedExpr);
        NewName == html  -> ParsedExpr;
        true -> {NewName, ParsedExpr}
    end,

    group(Rest, [Final|Accum]).

% group the block expressions (if/else if/else, each) with the content between
% open and close tags inside the blocks
group_blocks(Groups) ->
    group_blocks(Groups, []).

group_blocks([], Accum) ->
    lists:reverse(Accum);

group_blocks([{'if', Ast}|T], Accum) ->
    {Rest, Block} = group_if(T, Ast, []),
    group_blocks(Rest, [Block|Accum]);

group_blocks([{each, Expr}|T], Accum) ->
    {Rest, close_each, _, SubGroup} = group_until(close_each, T),
    % XXX pass the line
    Block = each_to_ast(1, Expr, SubGroup),
    group_blocks(Rest, [Block|Accum]);

group_blocks([H|T], Accum) ->
    group_blocks(T, [H|Accum]).

% group if expression
group_if(Groups, CurrentExpr, Asts) ->
    {Rest, Until, Expr, SubGroup} = group_until([else, close_if], Groups),

    case Until of
        else ->
            group_if(Rest, Expr, [{CurrentExpr, SubGroup}|Asts]);
        close_if ->
            {Rest,
                if_to_case(lists:reverse([{CurrentExpr, SubGroup}|Asts]), 1)}
    end.

% return the value of the first element of a token list if it's an identifier,
% if not fail with expected_identifier

% if there is an expression in a close tag fail
get_identifier_and_expression([{slash, Line, _}, {identifier, _, _}|T]) when length(T) > 0 ->
    throw({parser_error, {expression_in_close_tag, Line}});

% if it is a close tag create the identifier as close_ + identifier
get_identifier_and_expression([{slash, _, _}, {identifier, _, Identifier}]) ->
    NewIdentifier = list_to_atom("close_" ++ atom_to_list(Identifier)),
    {NewIdentifier, []};

% if it is an identifier
get_identifier_and_expression([{identifier, _Line, Identifier}|T]) ->
    {Identifier, T};

% if none matches fail
get_identifier_and_expression([{_, Line, _}=Got|_]) ->
    throw({parser_error, {identifier_expected, Line, Got}}).

% group until we find the Tag, return a tuple containing the remaining items
% the matched tag and the items accumulated before finding the tag
group_until(Tag, List) ->
    group_until(Tag, List, []).

group_until(Tag, [], _Accum) ->
    throw({parser_error, {expected_tag_but_end, Tag}});

% if the first argument is a list of tags check if the tag is in the list
group_until(Tags, [{Tag, TagContent}=H|Rest], Accum) when is_list(Tags)->
    case lists:member(Tag, Tags) of
        true ->
            {Rest, Tag, TagContent, lists:reverse(Accum)};
        false ->
            group_until(Tags, Rest, [H|Accum])
   end;

group_until(Tag, [{Tag, TagContent}|Rest], Accum) ->
    {Rest, Tag, TagContent, lists:reverse(Accum)};

group_until(Tag, [H|Rest], Accum) ->
    group_until(Tag, Rest, [H|Accum]).

% convert an each statement to Ast that runs and accumulated the BodyAst for
% each element of the ExprAst

each_to_ast(Line, ExprAst, BodyAst) ->
    {call, Line, {remote, Line, {atom, Line, fnt}, {atom, Line, each}},
        [ExprAst, {'fun', Line,
            {clauses, [{clause, Line,
                [{var, Line, 'Index'}, {var, Line, 'Value'}],
                [], [exprs_to_io_list(Line, BodyAst)]}]}}]}.

% convert the list of clauses in an {if A} 1 {else B} 2 {else} 3{/fi}
% to the Ast representing a nested case statement

if_to_case([], Line) ->
    dummy_body(Line);

if_to_case([{[], BodyAst}], _Line) ->
    BodyAst;

if_to_case([{CondAst, BodyAst}|T], Line) ->
    Ast = if_to_case(T, Line),

    NewAst = case is_list(Ast) of
        true -> Ast;
        false -> [Ast]
    end,

    gen_bool_case(Line, CondAst, BodyAst, NewAst).

% generate a body that does nothing

dummy_body(Line) ->
    {string, Line, ""}.

% generate a case that matches CondAst agains true and false, run TrueAst if
% true FalseAst if False

gen_bool_case(Line, CondAst, TrueAst, FalseAst) ->
    NewTrueAst = case length(TrueAst) > 1 of
        true -> [exprs_to_io_list(Line, TrueAst)];
        false -> TrueAst
    end,

    NewFalseAst = case length(FalseAst) > 1 of
        true -> [exprs_to_io_list(Line, FalseAst)];
        false -> FalseAst
    end,

    {'case', Line, CondAst,
        [gen_bool_clause(Line, true,  NewTrueAst),
            {clause, Line, [{var, Line, '_'}], [], NewFalseAst}]}.

% generate a clause where the value to match is a boolean
% Line is the line number and AstsBody is a list of Ast nodes representing the
% body of the clause

gen_bool_clause(Line, Val, AstsBody) ->
    {clause, Line, [{atom, Line, Val}], [], AstsBody}.

% generate ast of a call to fnt:escape to be used by ${name} and others
% the Ast must have the line number as the second item in the tuple
gen_escape(Ast) ->
    Line = element(2, Ast),
    {call, Line, {remote, Line, {atom, Line, fnt}, {atom, Line, escape}}, [Ast]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% utility functions used in templates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% if it's a json/efene struct pass the proplist
get({struct, Context}, Fields) ->
    get(Context, Fields);

% if we ended getting the attrs, return what we have in Context
get(Context, []) ->
    Context;

get(Context, [Field|Fields]) when is_list(Context)->
    case proplists:get_value(Field, Context) of
        undefined ->
            undefined;
        Value ->
            get(Value, Fields)
    end;

get(_Context, _Fields) ->
    undefined.

% used in {{each}}
each(Items, Fun) when is_list(Items) ->
    each(Items, Fun, 0, []);

each(_Val, _Fun) ->
    "".

each([], _Fun, _Index, Accum) ->
    lists:reverse(Accum);

each([H|T], Fun, Index, Accum) ->
    each(T, Fun, Index + 1, [Fun(Index, H)|Accum]).

% TODO: actually escape it
escape(Val) when is_list(Val) ->
    xmerl_lib:export_text(Val);

escape(Val) when is_integer(Val) ->
    integer_to_list(Val);

escape(Val) when is_float(Val) ->
    float_to_list(Val);

escape(Val) when is_atom(Val) ->
    atom_to_list(Val).

% handle the {{html Expr}} tag
% TODO
% html(_Expr) -> throw(not_implemented).

% handle the {{wrap Expr}} tag
% TODO
% wrap(_Expr) -> throw(not_implemented).

% handle the {{tmpl Expr}} tag
% TODO
% tmpl(_Expr) -> throw(not_implemented).

