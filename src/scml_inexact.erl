%%% The MIT License
%%%
%%% Copyright (C) 2013-2014 by Joseph Wayne Norton <norton@alum.mit.edu>
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.

%%% @doc Scheme inexact library
%%% @author CSCM Contributor <the-concurrent-schemer@googlegroups.com>

-module(scml_inexact).

%% SCML Exports
-export(['$scml_exports'/0]).

%% API
-export(['finite?'/1
         , 'infinite?'/1
         , 'nan?'/1
         , 'exp'/1
         , 'log'/1, 'log'/2
         , 'sin'/1
         , 'cos'/1
         , 'tan'/1
         , 'asin'/1
         , 'acos'/1
         , 'atan'/1, 'atan'/2
         , 'sqrt'/1
        ]).

-include("scml.hrl").

%%%===================================================================
%%% Types/Specs/Records
%%%===================================================================

%%%===================================================================
%%% SCML Exports
%%%===================================================================

-spec '$scml_exports'() -> [{scm_symbol(), scmi_nip()}].
'$scml_exports'() ->
    [{'finite?', #nipn{val=fun ?MODULE:'finite?'/1}}
     , {'infinite?', #nipn{val=fun ?MODULE:'infinite?'/1}}
     , {'nan?', #nipn{val=fun ?MODULE:'nan?'/1}}
     , {'exp', #nipn{val=fun ?MODULE:'exp'/1}}
     , {'log', #nipn{val=[fun ?MODULE:'log'/1, fun ?MODULE:'log'/2]}}
     , {'sin', #nipn{val=fun ?MODULE:'sin'/1}}
     , {'cos', #nipn{val=fun ?MODULE:'cos'/1}}
     , {'tan', #nipn{val=fun ?MODULE:'tan'/1}}
     , {'asin', #nipn{val=fun ?MODULE:'asin'/1}}
     , {'acos', #nipn{val=fun ?MODULE:'acos'/1}}
     , {'atan', #nipn{val=[fun ?MODULE:'atan'/1, fun ?MODULE:'atan'/2]}}
     , {'sqrt', #nipn{val=fun ?MODULE:'sqrt'/1}}
    ].

%%%===================================================================
%%% API
%%%===================================================================

-spec 'finite?'(scm_z()) -> scm_boolean().
'finite?'(Z) ->
    %% @TODO
    erlang:error({roadmap,'v0.7.0'}, [Z]).

-spec 'infinite?'(scm_z()) -> scm_boolean().
'infinite?'(Z) ->
    %% @TODO
    erlang:error({roadmap,'v0.7.0'}, [Z]).

-spec 'nan?'(scm_z()) -> scm_boolean().
'nan?'(Z) ->
    %% @TODO
    erlang:error({roadmap,'v0.7.0'}, [Z]).

-spec 'exp'(scm_z()) -> scm_z().
'exp'(Z) ->
    %% @TODO
    erlang:error({roadmap,'v0.7.0'}, [Z]).

-spec 'log'(scm_z()) -> scm_z().
'log'(Z) ->
    %% @TODO
    erlang:error({roadmap,'v0.7.0'}, [Z]).

-spec 'log'(scm_z(), scm_z()) -> scm_z().
'log'(Z1, Z2) ->
    %% @TODO
    erlang:error({roadmap,'v0.7.0'}, [Z1, Z2]).

-spec 'sin'(scm_z()) -> scm_z().
'sin'(Z) ->
    %% @TODO
    erlang:error({roadmap,'v0.7.0'}, [Z]).

-spec 'cos'(scm_z()) -> scm_z().
'cos'(Z) ->
    %% @TODO
    erlang:error({roadmap,'v0.7.0'}, [Z]).

-spec 'tan'(scm_z()) -> scm_z().
'tan'(Z) ->
    %% @TODO
    erlang:error({roadmap,'v0.7.0'}, [Z]).

-spec 'asin'(scm_z()) -> scm_z().
'asin'(Z) ->
    %% @TODO
    erlang:error({roadmap,'v0.7.0'}, [Z]).

-spec 'acos'(scm_z()) -> scm_z().
'acos'(Z) ->
    %% @TODO
    erlang:error({roadmap,'v0.7.0'}, [Z]).

-spec 'atan'(scm_z()) -> scm_z().
'atan'(Z) ->
    %% @TODO
    erlang:error({roadmap,'v0.7.0'}, [Z]).

-spec 'atan'(scm_y(), scm_x()) -> scm_z().
'atan'(Y, X) ->
    %% @TODO
    erlang:error({roadmap,'v0.7.0'}, [Y, X]).

-spec 'sqrt'(scm_z()) -> scm_z().
'sqrt'(Z) ->
    %% @TODO
    erlang:error({roadmap,'v0.7.0'}, [Z]).

%%%===================================================================
%%% internal helpers
%%%===================================================================
