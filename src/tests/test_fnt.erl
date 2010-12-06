-module(test_fnt).
-compile(export_all).

equals(A, A) ->
    io:format("ok: both are ~p~n", [A]);

equals(A, B) ->
    io:format("error: ~p != ~p~n", [A, B]).

template_path(Name) ->
    "../../examples/" ++ Name ++ ".fnt".

test_ast() ->
    Ast = fnt:to_ast(templates, [
        {basic, template_path("basic")},
        {value, template_path("value")},
        {if_t, template_path("if")},
        {html, template_path("html")},
        {each, template_path("each")}
    ], []),

    %io:format("~p~n", [Ast]).
    io:format("~s~n", [fnt:to_erlang(Ast)]),
    Ast.

test_compile() ->
    Mod = fnt:compile(templates, [
        {basic, template_path("basic")},
        {value, template_path("value")},
        {if_t, template_path("if")},
        {html, template_path("html")},
        {each, template_path("each")}
    ]),

    fnt:bin_to_file(Mod, "templates.beam").

test_get() ->
    equals(fnt:get([{a, 5}], [a]), 5),
    equals(fnt:get([{a, 5}], [b]), undefined),
    equals(fnt:get([{a, 5}], [a, b]), undefined),
    equals(fnt:get([], [a]), undefined),

    equals(fnt:get([{a, [{b, 5}]}], [b]), undefined),
    equals(fnt:get([{a, [{b, 5}]}], [a, b]), 5),
    equals(fnt:get([{a, [{b, 5}]}], [a, a]), undefined),
    equals(fnt:get([{a, [{b, 5}]}], [a, b, a]), undefined)
    .

run() ->
    test_get(),
    test_ast(),
    test_compile(),
    ok.

