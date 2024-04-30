%%%---------------------------------------------------------------------
%%% @copyright 2024 William Fank Thomé
%%% @author William Fank Thomé <willilamthome@hotmail.com>
%%% @doc JSON encoder.
%%%
%%% Copyright 2024 William Fank Thomé
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% @end
%%%---------------------------------------------------------------------
-module(bel_json_encoder).

-compile({inline, [do_encode/3, traverse_codecs/5]}).

% API functions
-export([ encode/1
        , encode/2
        , encode_to_iodata/1
        , encode_to_iodata/2
        ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(DEFAULT_OPTS, #{}).

-define(is_codec_fun(X), (
    is_function(X, 4)
)).

-define(min(X, Min), (
    is_integer(X) andalso X >= Min
)).

-define(in_range(X, Min, Max), (
    is_integer(X) andalso X >= Min andalso X =< Max)
).

-record(state, {
    escape,
    encode_list,
    encode_tuple
}).

%%%=====================================================================
%%% API functions
%%%=====================================================================

encode(Term) ->
    iolist_to_binary(json:encode(Term)).

encode(Term, #state{} = State) ->
    iolist_to_binary(do_encode(Term, State));
encode(Term, Opts) when is_map(Opts) ->
    iolist_to_binary(do_encode(Term, parse_opts(Opts))).

encode_to_iodata(Term) ->
    json:encode(Term).

encode_to_iodata(Term, #state{} = State) ->
    do_encode(Term, State);
encode_to_iodata(Term, Opts) when is_map(Opts) ->
    do_encode(Term, parse_opts(Opts)).

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

parse_opts(Opts) ->
    Escape = fun json:encode_binary/1,
    UnsupportedTypeError = fun(Unsupported, _Encode) ->
        unsupported_type_error(Unsupported)
    end,
    #state{
        escape = Escape,
        encode_list = encode_callback(
            normalize_list_codecs(maps:get(list_codecs, Opts, [])),
            Escape, fun json:encode_list/2
        ),
        encode_tuple = encode_callback(
            normalize_tuple_codecs(maps:get(tuple_codecs, Opts, [])),
            Escape, UnsupportedTypeError
        )
    }.

encode_callback([], _Escape, Next) when is_function(Next, 2) ->
    Next;
encode_callback(Codecs, Escape, Next) ->
    fun(Term, Encode) ->
        traverse_codecs(Codecs, Term, Escape, Encode, Next)
    end.

traverse_codecs([{Codec, Opts} | Codecs], Term, Escape, Encode, Next) ->
    case Codec(Term, Opts, Escape, Encode) of
        next ->
            traverse_codecs(Codecs, Term, Escape, Encode, Next);
        {halt, NewTerm} ->
            NewTerm
    end;
traverse_codecs([], Term, _Escape, Encode, Next) ->
    Next(Term, Encode).

unsupported_type_error(Unsupported) ->
    error({unsupported_type, Unsupported}).

do_encode(Term, State) ->
    json:encode(Term, fun(X, Encode) -> do_encode(X, Encode, State) end).

do_encode(Atom, Encode, _State) when is_atom(Atom) ->
    json:encode_atom(Atom, Encode);
do_encode(Bin, _Encode, #state{escape = Escape}) when is_binary(Bin) ->
    Escape(Bin);
do_encode(Int, _Encode, _State) when is_integer(Int) ->
    json:encode_integer(Int);
do_encode(Float, _Encode, _State) when is_float(Float) ->
    json:encode_float(Float);
do_encode(List, Encode, State) when is_list(List) ->
    (State#state.encode_list)(List, Encode);
do_encode(Map, Encode, _State) when is_map(Map) ->
    json:encode_map(Map, Encode);
do_encode(Tuple, Encode, State) when is_tuple(Tuple) ->
    (State#state.encode_tuple)(Tuple, Encode);
do_encode(Unsupported, _Encode, _State) ->
    unsupported_type_error(Unsupported).

%%%=====================================================================
%%% List codecs
%%%=====================================================================

normalize_list_codecs(Codecs) ->
    lists:map(fun
        (proplist) ->
            {fun proplist_list_codec/4, fun is_proplist/1};
        ({proplist, IsProplist}) when is_function(IsProplist, 1) ->
            {fun proplist_list_codec/4, IsProplist};
        (Fun) when ?is_codec_fun(Fun) ->
            {Fun, ?DEFAULT_OPTS};
        ({Fun, Opts}) when ?is_codec_fun(Fun) ->
            {Fun, Opts}
    end, Codecs).

is_proplist(List) ->
    lists:all(fun is_proplist_prop/1, List).

is_proplist_prop({Key, _}) ->
    is_binary(Key) orelse is_atom(Key) orelse is_integer(Key);
is_proplist_prop(Key) ->
    is_binary(Key) orelse is_atom(Key).

proplist_list_codec(List, IsProplist, _Escape, Encode) ->
    case IsProplist(List) of
        true ->
            {halt, Encode(proplists:to_map(List), Encode)};
        false ->
            next
    end.

%%%=====================================================================
%%% Tuple codecs
%%%=====================================================================

normalize_tuple_codecs(Codecs) ->
    lists:map(fun
        (datetime) ->
            {fun datetime_tuple_codec/4, ?DEFAULT_OPTS};
        (timestamp) ->
            {fun timestamp_tuple_codec/4, ?DEFAULT_OPTS};
        (ipv4) ->
            {fun ipv4_tuple_codec/4, ?DEFAULT_OPTS};
        (ipv6) ->
            {fun ipv6_tuple_codec/4, ?DEFAULT_OPTS};
        ({record, RecordsList}) when is_list(RecordsList) ->
            Records = maps:from_list([
                {Name, {length(Fields)+1, Fields}}
                || {Name, Fields} <- RecordsList
            ]),
            {fun record_tuple_codec/4, Records};
        (Fun) when ?is_codec_fun(Fun) ->
            {Fun, ?DEFAULT_OPTS};
        ({Fun, Opts}) when ?is_codec_fun(Fun) ->
            {Fun, Opts}
    end, Codecs).

datetime_tuple_codec({{YYYY, MM, DD}, {H, M, S}}, _Opts, Escape, _Encode)
  when ?min(YYYY, 0), ?in_range(MM, 1, 12), ?in_range(DD, 1, 31)
     , ?in_range(H, 0, 23), ?in_range(M, 0, 59), ?in_range(S, 0, 59) ->
    Datetime = iolist_to_binary(io_lib:format(
        "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0BZ",
        [YYYY,MM,DD,H,M,S])
    ),
    {halt, Escape(Datetime)};
datetime_tuple_codec(_, _, _, _) ->
    next.

timestamp_tuple_codec({MegaSecs, Secs, MicroSecs} = Timestamp, _Opts, Escape, _Encode)
  when ?min(MegaSecs, 0), ?min(Secs, 0), ?min(MicroSecs, 0) ->
    MilliSecs = MicroSecs div 1000,
    {{YYYY,MM,DD},{H,M,S}} = calendar:now_to_datetime(Timestamp),
    DateTime = iolist_to_binary(io_lib:format(
        "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B.~3.10.0BZ",
        [YYYY,MM,DD,H,M,S,MilliSecs])
    ),
    {halt, Escape(DateTime)};
timestamp_tuple_codec(_, _, _, _) ->
    next.

ipv4_tuple_codec({_A,_B,_C,_D} = Tuple, _Opts, Escape, _Encode) ->
    case inet_parse:ntoa(Tuple) of
        {error, einval} ->
            next;
        Ipv4 ->
            {halt, Escape(list_to_binary(Ipv4))}
    end;
ipv4_tuple_codec(_, _, _, _) ->
    next.

ipv6_tuple_codec({_A,_B,_C,_D,_E,_F,_G,_H} = Tuple, _Opts, Escape, _Encode) ->
    case inet_parse:ntoa(Tuple) of
        {error, einval} ->
            next;
        Ipv6 ->
            {halt, Escape(list_to_binary(Ipv6))}
    end;
ipv6_tuple_codec(_, _, _, _) ->
    next.

record_tuple_codec(Tuple, Records, _Escape, Encode) when tuple_size(Tuple) > 1 ->
    Name = element(1, Tuple),
    case Records of
        #{Name := {Size, Keys}} ->
            case tuple_size(Tuple) =:= Size of
                true ->
                    [Name | Values] = tuple_to_list(Tuple),
                    Proplist = lists:zip(Keys, Values),
                    {halt, Encode(proplists:to_map(Proplist), Encode)};
                false ->
                    next
            end;
        #{} ->
            next
    end;
record_tuple_codec(_, _, _, _) ->
    next.

%%%=====================================================================
%%% Tests
%%%=====================================================================

-ifdef(TEST).

list_codecs_test() ->
    [ { "proplist"
      , ?assertEqual(
            <<"{\"foo\":\"foo\",\"bar\":true}">>,
            encode(
                [{foo, foo}, bar],
                #{list_codecs => [proplist]}
            )
        )}
    ].

-record(foo, {foo, bar}).
tuple_codecs_test() ->
    [ { "datetime"
      , ?assertEqual(
            <<"{\"foo\":\"2024-04-29T22:34:35Z\"}">>,
            encode(
                #{foo => {{2024,04,29},{22,34,35}}},
                #{tuple_codecs => [datetime]}
            )
        )}
    , { "timestamp"
      , ?assertEqual(
            <<"\"1970-01-01T00:00:00.000Z\"">>,
            encode(
                {0,0,0},
                #{tuple_codecs => [timestamp]}
            )
        )}
    , { "ipv4"
      , ?assertEqual(
            <<"\"0.0.0.0\"">>,
            encode(
                {0,0,0,0},
                #{tuple_codecs => [ipv4]}
            )
        )}
    , { "ipv6"
      , ?assertEqual(
            <<"\"fe80::204:acff:fe17:bf38\"">>,
            encode(
                {16#fe80,0,0,0,16#204,16#acff,16#fe17,16#bf38},
                #{tuple_codecs => [ipv6]}
            )
        )}
    , { "record"
      , ?assertEqual(
            <<"{\"foo\":\"foo\",\"bar\":\"bar\"}">>,
            encode(
                #foo{foo = foo, bar = bar},
                #{tuple_codecs => [{record, [
                    {foo, record_info(fields, foo)}
                ]}]}
            )
        )}
    ].

-endif.
