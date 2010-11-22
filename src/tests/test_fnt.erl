-module(test_fnt).
-compile(export_all).

assert_tokens_num(Tpl, Count) ->
    {ok, Tokens} = fnt:lex(string, Tpl),
    RealCount = length(Tokens),

    case RealCount  of
        Count ->
            io:format("ok: ~s has ~p tokens~n", [Tpl, Count]);
        _ ->
            io:format("error: ~s has ~p tokens not ~p~n       ~p~n", [Tpl, RealCount, Count, Tokens])
    end.

test_template_tag() ->
    assert_tokens_num("${b}", 3),
    assert_tokens_num("a ${b} c", 5),
    assert_tokens_num("<a href='#'>${b} c</a> ${d}", 8),
    assert_tokens_num("<a href='#'>${b} $c</a> ${d}", 10),
    assert_tokens_num("<a href='#'>${b} {c</a> ${d}", 10),
    assert_tokens_num("<a href='#'>${b} c}</a> ${d}", 10),
    assert_tokens_num("${b.c.d}", 3),
    assert_tokens_num("aasd ${b.foo()} casdgf  sfsg gfdsg", 5),
    ok.

run() ->
    test_template_tag(),
    ok.

