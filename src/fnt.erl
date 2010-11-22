-module(fnt).
-export([lex/2]).

lex(string, String) ->
    case fnt_lex:string(String) of
        {ok, Tokens, _Endline} ->
            {ok, Tokens};

        Errors -> {error, Errors}
    end;

lex(file, Path) ->
    case file:read_file(Path) of
        {ok, Binary} ->
            String = binary_to_list(Binary),
            lex(string, String);

        {error, _Reason} = Error -> Error
    end.


