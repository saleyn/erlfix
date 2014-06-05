%%%----------------------------------------------------------------------------
%%% @doc FIX Protocol Parser.
%%% @author Serge Aleynikov <saleyn@gmail.com>
%%% @copyright 2011 Serge Aleynikov
%%% @end
%%%----------------------------------------------------------------------------
%%% Created: 2011-01-04
%%%----------------------------------------------------------------------------
-module(fix).
-author('saleyn@gmail.com').

%% API
-export([parse/1, parse/2, print/1, test/0]).

-include("fix_parse.hrl").

%%%----------------------------------------------------------------------------
%%% External API
%%%----------------------------------------------------------------------------

%%-----------------------------------------------------------------------------
%% @spec (Input) -> {ok, Pid} | {error, Reason} 
%%          Input = Filename::string() | Msgs::binary()
%% @doc Start the server outside of supervision tree.
%% @end
%%-----------------------------------------------------------------------------
parse(File) when is_list(File) ->
    parse(File, []);

parse(Bin) when is_binary(Bin) ->
    parse(Bin, []).

parse(File, Options) when is_list(File) ->
    {ok, Bin} = file:read_file(File),
    parse(Bin, Options);
parse(Bin, Options) when is_binary(Bin) ->
    Sep = case lists:member(pipe, Options) of
          true -> $|;
          false -> guess_sep(Bin)
          end,
    {Msgs, _Rest} = deliminate(Bin, [], Sep),
    case lists:member(full, Options) of
    true ->
        [fix_parse:full_decode(M) || M <- Msgs];
    false ->
        [fix_parse:decode(M) || M <- Msgs]
    end.

print(Bin) when is_list(Bin) -> print(list_to_binary(Bin));
print(Bin) when is_binary(Bin) ->
    {Msgs, _Rest} = deliminate(Bin, [], $|),
    [begin
        [print(I, V) || {I,V} <- Msg],
        io:put_chars("\n")
     end || Msg <- Msgs],
    ok.

print(I, V) ->
    io:format("~5w ~30w ~s\n",
        [I, try
                element(1, fix_parse:decode_field(I))
            catch _:_ ->
                undefined
            end, try_decode(I, V)]).

try_decode(I, V) ->
    try
        case fix_parse:decode_value(I, V) of 
        J when is_integer(J) -> io_lib:format("~w", [J]);
        J when is_float(J)   -> io_lib:format("~w", [J]);
        J when is_atom(J) andalso I =:= 55 -> io_lib:format("~w", [J]); % Symbol
        J when is_atom(J)    -> io_lib:format("~s (~w)", [binary_to_list(V), J]);
        J when is_list(J)    -> J;
        J ->
            io_lib:format("~s", [binary_to_list(J)])
        end
    catch _:_ ->
        binary_to_list(V)
    end.

deliminate(Bin, Acc, Sep) ->
    case deliminate_one(Bin, Sep) of
    {ok, Msg, Rest} ->
        deliminate(Rest, [replace1(Msg, Sep) | Acc], Sep);
    {not_found, Msg} ->
        {lists:reverse(Acc), Msg}
    end.

deliminate_one(Data, Sep) ->
    case binary:match(Data, <<Sep, "10=">>) of
    {Start, _} when byte_size(Data) >= Start + 8 ->
        case Data of
        <<Msg:Start/binary, _:7/binary, Sep,$\n, Rest/binary>> ->
            {ok, Msg, Rest};
        <<Msg:Start/binary, _:7/binary, Sep, Rest/binary>> ->
            {ok, Msg, Rest};
        <<Msg:Start/binary, _:7/binary, Sep,$\n, Rest/binary>> ->
            {ok, Msg, Rest};
        <<Msg:Start/binary, _:7/binary, Rest/binary>> ->
            {ok, Msg, Rest}
        end;
    _ ->
        {not_found, Data}
    end.

replace1(Msg, Sep) ->
    %binary:replace(Msg, <<1>>, <<$|>>, [global]).
    [parse_field(B) || B <- binary:split(Msg, <<Sep>>, [global])].

parse_field(Bin) ->
    {Pos, 1} = binary:match(Bin, <<$=>>),
    N = lists:filter(fun(I) -> I >= $0 andalso I =< $9 end, binary:bin_to_list(Bin, {0, Pos})),
    try
        {list_to_integer(N), binary:part(Bin, {Pos+1, byte_size(Bin)-Pos-1})}
    catch _:Error ->
        io:format(standard_error, "Error parsing: ~p: ~p\n  ~p\n",
            [Bin, Error, erlang:get_stacktrace()])
    end.

%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------

guess_sep(Bin) ->
    case binary:match(Bin, [<<$|>>, <<1>>], [{scope, {0, 15}}]) of
    {I, _} ->
        binary:at(Bin, I);
    nomatch ->
        throw({cannot_determine_delimiter, binary_part(Bin, {0, 30})})
    end.

test() ->
    io:format("~p\n",
        [parse(<<"8=FIX.4.2",1,"9=55",1,"35=5",1,"34=1",1,"49=USA",1,"52=20101231-14:30:02.185",1,"56=DTTX",1,"10=124",1,"8=FIX.4.2",1,"9=110",1,"35=5",1,"49=DTTX",1,"56=USA",1,"34=2",1,"52=20101231-14:30:13.125",1,"58=MsgSeqNum too low, expecting 2 but received 1 Logon",1,"10=087",1,"8=FIX.4.2",1,"9=226",1,"35=8",1,"49=DTTX",1,"56=USA",1,"34=32",1,"52=20101231-14:41:06.593",1,"32=0",1,"31=0.0000",1,"151=10",1,"14=0",1,"6=0.0000",1,"60=20101231-14:41:06.592",1,"11=O2",1,"55=REE",1,"54=2",1,"38=10",1,"59=3",1,"40=2",1,"44=1.6000",1,"17=791490963",1,"1=StockUSA",1,"48=REE",1,"20=0",1,"39=0",1,"150=0",1,"37=791490964",1,"30=S",1,"10=128",1>>)]).


