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

-compile({inline, [do_encode/3, traverse_codecs/3]}).

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
    is_function(X, 3)
)).

-define(min(X, Min), (
    is_integer(X) andalso X >= Min
)).

-define(in_range(X, Min, Max), (
    is_integer(X) andalso X >= Min andalso X =< Max)
).

-record(state, {
    escape,
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
    TupleCodecs = normalize_tuple_codecs(maps:get(tuple_codecs, Opts, [])),
    #state{
        escape = Escape,
        encode_tuple = fun(Term) ->
            traverse_codecs(TupleCodecs, Term, Escape)
        end
    }.

normalize_tuple_codecs(Codecs) ->
    lists:map(fun
        (datetime) ->
            {fun datetime_tuple_codec/3, ?DEFAULT_OPTS};
        (Fun) when ?is_codec_fun(Fun) ->
            {Fun, ?DEFAULT_OPTS};
        ({Fun, Opts}) when ?is_codec_fun(Fun) ->
            {Fun, Opts}
    end, Codecs).

traverse_codecs([{Codec, Opts} | Codecs], Term, Escape) ->
    case Codec(Term, Opts, Escape) of
        next ->
            traverse_codecs(Codecs, Term, Escape);
        {halt, NewTerm} ->
            NewTerm
    end;
traverse_codecs([], Other, _Escape) ->
    error({unsupported_type, Other}).

datetime_tuple_codec({{YYYY, MM, DD}, {H, M, S}}, _Opts, Escape)
  when ?min(YYYY, 0), ?in_range(MM, 1, 12), ?in_range(DD, 1, 31)
     , ?in_range(H, 0, 23), ?in_range(M, 0, 59), ?in_range(S, 0, 59) ->
    Term = iolist_to_binary(io_lib:format(
        "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0BZ",
        [YYYY,MM,DD,H,M,S])
    ),
    {halt, Escape(Term)};
datetime_tuple_codec(_Term, _Opts, _Escape) ->
    next.

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
do_encode(List, Encode, _State) when is_list(List) ->
    json:encode_list(List, Encode);
do_encode(Map, Encode, _State) when is_map(Map) ->
    json:encode_map(Map, Encode);
do_encode(Tuple, _Encode, #state{encode_tuple = Encode}) when is_tuple(Tuple) ->
    Encode(Tuple);
do_encode(Unsupported, _Encode, _State) ->
    error({unsupported_type, Unsupported}).

%%%=====================================================================
%%% Tests
%%%=====================================================================

-ifdef(TEST).

encode_test() ->
    ?assertEqual(
        <<"{\"foo\":\"2024-04-29T22:34:35Z\"}">>,
        encode(
            #{foo => {{2024,04,29},{22,34,35}}},
            #{tuple_codecs => [datetime]}
        )
    ).

-endif.
