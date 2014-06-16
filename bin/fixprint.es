#!/usr/bin/env escript
%% -*-erlang-*-
%%! -smp disable -pa ../ebin +d -env ERL_CRASH_DUMP /dev/null

-author('saleyn@gmail.com').
-mode  (compile).

-record(args, {file, line, error = abort}).

main(Args) ->
    main2(args(Args, #args{})).

main2(#args{file = undefined, line = undefined, error = E}) ->
    % Read from stdin
    io:setopts(standard_io, [binary]),
    read_file(standard_io, E);
main2(#args{file = File, error = E}) when is_list(File) ->
    {ok, Io} = file:open(File, [read, binary, raw, {read_ahead, 256*1024}]),
    read_file(Io, E);
main2(#args{line = Line}) when is_list(Line) ->
    try
        fix:print(list_to_binary(Line))
    catch _:Error ->
        io:format(standard_error, "Error: ~p\n  ~p\n", [Error, erlang:get_stacktrace()])
    end.

args(["-h" | _], _Acc) ->
    usage();
args(["--help" | _], _Acc) ->
    usage();
args(["-f", F | T], Args) when hd(F) =/= $- ->
    args(T, Args#args{file = F});
args(["-e" | T], Args) ->
    args(T, Args#args{error = ignore});
args(["8=FIX" ++ _ = String | T], Args) ->
    args(T, Args#args{line = String});
args([Opt | _], _Args) ->
    io:format(standard_error, "Invalid option: ~p\n\n", [Opt]),
    usage();
args([], Args) ->
    Args.
    
usage() ->
    io:format(standard_error, "FIX message decoder\n"
              "Usage: ~s [FixInputString] [-f Filename]\n", [escript:script_name()]),
    halt(1).

read_file(Io, IgnoreError) ->
    case file:read_line(Io) of
    {ok, <<"8=FIX", _/binary>> = Line} ->
        io:put_chars(Line),
        fix:print(Line, IgnoreError),
        read_file(Io, IgnoreError);
    {ok, _Other} ->
        read_file(Io, IgnoreError);
    eof ->
        ok
    end.
