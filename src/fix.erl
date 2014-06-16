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
-export([parse/1, parse/2, print/1, print/2, test/0]).

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

-spec parse(string() | binary(), [{error, abort|ignore} | pipe | full]) ->
        [{integer() | any()}].
parse(File, Options) when is_list(File) ->
    {ok, Bin} = file:read_file(File),
    parse(Bin, Options);
parse(Bin, Options) when is_binary(Bin) ->
    Err = proplists:get_value(error, Options, abort),
    Sep = case proplists:get_value(pipe, Options, false) of
          true -> $|;
          false -> guess_sep(Bin)
          end,
    {Msgs, _Rest} = deliminate(Bin, [], Sep, Err),
    DecodeFun     = decode_fun(proplists:get_value(full, Options, false)),
    [decode(DecodeFun, M, Err) || M <- Msgs].

print(Bin) ->
    print(Bin, abort).
print(Bin, IgnoreError) when is_list(Bin) ->
    print(list_to_binary(Bin), IgnoreError);
print(Bin, IgnoreError) when is_binary(Bin) ->
    {Msgs, _Rest} = deliminate(Bin, [], $|, IgnoreError),
    [begin
        [print2(I, V) || {I,V} <- Msg],
        io:put_chars("\n")
     end || Msg <- Msgs],
    ok.

%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------

print2(I, V) ->
    io:format("~5w | ~30w | ~s\n",
        [I, try
                element(1, fix_parse:decode_field(I))
            catch _:_ ->
                undefined
            end, try_decode(I, V)]).

decode_fun(true)  -> fun(M) -> fix_parse:full_decode(M) end;
decode_fun(false) -> fun(M) -> fix_parse:decode(M)      end.

decode(Fun, Msg, Err) ->
    try
        Fun(Msg)
    catch _:Error when Err =:= ignore ->
        io:format(standard_error, "Msg:   ~p\nError: ~p\n       ~p\n",
            [Msg, Error, erlang:get_stacktrace()])
    end.

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

deliminate(Bin, Acc, Sep, Err) ->
    case deliminate_one(Bin, Sep) of
    {ok, Msg, Rest} ->
        Acc1 = deliminate2(Msg, Acc, Sep, Err),
        deliminate(Rest, Acc1, Sep, Err);
    {not_found, Msg} ->
        {lists:reverse(Acc), Msg}
    end.

deliminate2(Msg, Acc, Sep, Err) ->
    try [replace1(Msg, Sep) | Acc]
    catch _:_ when Err =:= ignore ->
        Acc
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
    lists:foldr(
        fun(Bin, Acc) ->
            case binary:match(Bin, <<$=>>) of
            {0, _} ->
                throw({missing_field_tag, Bin});
            {Pos, _} ->
                N = lists:filter(fun(I) -> I >= $0 andalso I =< $9 end,
                                 binary:bin_to_list(Bin, {0, Pos})),
                try
                    [{list_to_integer(N), binary:part(Bin, {Pos+1, byte_size(Bin)-Pos-1})} | Acc]
                catch _:Error ->
                    throw({'Parsing error', Error, Bin})
                end;
            nomatch ->
                Acc
            end
        end, [], binary:split(Msg, <<Sep>>, [global])).

guess_sep(Bin) ->
    case binary:match(Bin, [<<$|>>, <<1>>], [{scope, {0, 15}}]) of
    {I, _} ->
        binary:at(Bin, I);
    nomatch ->
        throw({cannot_determine_delimiter, binary_part(Bin, {0, 30})})
    end.

%%%----------------------------------------------------------------------------
%%% Test functions
%%%----------------------------------------------------------------------------

test() ->
    io:format("~p\n",
        [parse(<<"8=FIX.4.2",1,"9=55",1,"35=5",1,"34=1",1,"49=USA",1,"52=20101231-14:30:02.185",1,"56=DTTX",1,"10=124",1,"8=FIX.4.2",1,"9=110",1,"35=5",1,"49=DTTX",1,"56=USA",1,"34=2",1,"52=20101231-14:30:13.125",1,"58=MsgSeqNum too low, expecting 2 but received 1 Logon",1,"10=087",1,"8=FIX.4.2",1,"9=226",1,"35=8",1,"49=DTTX",1,"56=USA",1,"34=32",1,"52=20101231-14:41:06.593",1,"32=0",1,"31=0.0000",1,"151=10",1,"14=0",1,"6=0.0000",1,"60=20101231-14:41:06.592",1,"11=O2",1,"55=REE",1,"54=2",1,"38=10",1,"59=3",1,"40=2",1,"44=1.6000",1,"17=791490963",1,"1=StockUSA",1,"48=REE",1,"20=0",1,"39=0",1,"150=0",1,"37=791490964",1,"30=S",1,"10=128",1>>)]).


