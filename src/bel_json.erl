%%%---------------------------------------------------------------------
%%% @copyright 2024 William Fank Thomé
%%% @author William Fank Thomé <willilamthome@hotmail.com>
%%% @doc JSON parser and generator.
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
-module(bel_json).

% API functions
-export([ encode/1
        , encode/2
        , encode_to_iodata/1
        , encode_to_iodata/2
        , decode/1
        ]).

%%%=====================================================================
%%% API functions
%%%=====================================================================

encode(Term) ->
    bel_json_encoder:encode(Term).

encode(Term, Opts) ->
    bel_json_encoder:encode(Term, Opts).

encode_to_iodata(Term) ->
    bel_json_encoder:encode_to_iodata(Term).

encode_to_iodata(Term, Opts) ->
    bel_json_encoder:encode_to_iodata(Term, Opts).

decode(Bin) ->
    bel_json_decoder:decode(Bin).

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

% nothing here yet!
