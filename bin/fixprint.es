#!/usr/bin/env escript
%% -*-erlang-*-
%%! -smp disable -pa ../ebin

-author('saleyn@gmail.com').
-mode  (compile).

main([[$-,$h | _]]) ->
    usage();
main([[$-,$-,$h | _]]) ->
    usage();
main(["-f" | F]) ->
    {ok, Io} = file:open(F, [read, binary, raw]),
    read_file(Io);
main([String]) ->
    try
        case lists:reverse(String) of
        [$| | _] -> fix:print(String);
        L        -> fix:print(lists:reverse([$| | L]))
        end
    catch _:_ ->
        usage()
    end;
main([]) ->
    % Read from stdin
    read_file(standard_io);
main(_) ->
    usage().

usage() ->
    io:format("Usage: ~s [FixInputString] [-f Filename]\n", [escript:script_name()]),
    halt(1).

read_file(Io) ->
    case file:read_line(Io) of
    {ok, Line} ->
        io:put_chars(Line),
        fix:print(Line),
        read_file(Io);
    eof ->
        ok
    end.
