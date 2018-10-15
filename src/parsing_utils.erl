-module(parsing_utils).

-compile(export_all).

compile_all() -> 
    yecc:file(parser),
    leex:file(lex),
    compile:file(lex),
    code:purge(lex),
    code:load_file(lex),
    compile:file(parser),
    code:purge(parser),
    code:load_file(parser).


scan_and_parse(Input) ->
    case lex:string(Input) of
        {ok, Tokens, _} -> 
            parser:parse(Tokens);
        Err ->
            Err
    end.
    
scan_and_parse_file(File) ->
    {ok, String} = file:read_file(File),
    case lex:string(binary_to_list(String)) of
        {ok, Tokens, _} -> 
            parser:parse(Tokens);
        Err ->
            Err
    end.
