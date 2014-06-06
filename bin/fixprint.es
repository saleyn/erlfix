#!/usr/bin/env escript
%% -*-erlang-*-
%%! -smp disable -pa ../ebin

-author('saleyn@gmail.com').
-mode  (compile).

main(["-h" | _]) ->
    usage();
main(["--help" | _]) ->
    usage();
main(["-f", F | _]) when hd(F) =/= $- ->
    {ok, Io} = file:open(F, [read, binary, raw, {read_ahead, 256*1024}]),
    read_file(Io);
main(["8=FIX" ++ _ = String]) ->
    try
        fix:print(list_to_binary(String))
    catch _:_ ->
        usage()
    end;
main([]) ->
    % Read from stdin
    io:setopts(standard_io, [binary]),
    read_file(standard_io);
main(_) ->
    usage().

usage() ->
    io:format("FIX message decoder\n"
              "Usage: ~s [FixInputString] [-f Filename]\n", [escript:script_name()]),
    halt(1).

read_file(Io) ->
    case file:read_line(Io) of
    {ok, <<"8=FIX", _/binary>> = Line} ->
        io:put_chars(Line),
        fix:print(Line),
        read_file(Io);
    {ok, _Other} ->
        read_file(Io);
    eof ->
        ok
    end.
