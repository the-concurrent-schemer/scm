%%% The MIT License
%%%
%%% Copyright (C) 2013 by Joseph Wayne Norton <norton@alum.mit.edu>
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

%%% @doc This module contains functions that are not yet implemented
%%% or temporary in nature.  This functions will be deprecated or
%%% moved to other modules in a future release.

-module(scmtmp).

%% External exports
-export(['exact'/2
         , 'inexact'/2
         , 'make-rational'/2
         , 'make-rational'/3
         , 'make-real'/2
         , 'make-polar'/3
         , 'make-rectangular'/3
        ]).


-include("scmi.hrl").

%%%----------------------------------------------------------------------
%%% Types/Specs/Records
%%%----------------------------------------------------------------------

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

%% @doc This function is a temporary place holder
-spec exact(scm_x(), scmi_env()) -> scm_x().
exact(X, _Env) ->
    X.

%% @doc This function is a temporary place holder
-spec inexact(scm_x(), scmi_env()) -> scm_x().
inexact(X, _Env) ->
    X.

%% @doc This function is a temporary place holder
-spec 'make-rational'(scm_x(), scmi_env()) -> scm_q().
'make-rational'({N, D}, Env) when erlang:is_number(N), erlang:is_number(D) ->
    'make-rational'(N, D, Env);
'make-rational'(X, _Env) ->
    {X, 1}.

%% @doc This function is a temporary place holder
-spec 'make-rational'(scm_n(), scm_k(), scmi_env()) -> scm_q().
'make-rational'(0, 0, Env) ->
    'make-rational'(0, Env);
'make-rational'(N, D, _Env) ->
    {N, D}.

%% @doc This function is a temporary place holder
-spec 'make-real'(scm_x() | {scm_n(), scm_n()}, scmi_env()) -> scm_x().
'make-real'(?PINF, _Env) ->
    ?PINF;
'make-real'(?NINF, _Env) ->
    ?NINF;
'make-real'(?PNAN, _Env) ->
    ?PNAN;
'make-real'(?NNAN, _Env) ->
    ?NNAN;
'make-real'(?NZER, _Env) ->
    ?NZER;
'make-real'({0, 0}, _Env) ->
    0;
'make-real'({N, D}, _Env) when N==0, D==0 ->
    ?PNAN;
'make-real'({N, D}, _Env) when N < 0, D==0 ->
    ?NINF;
'make-real'({N, D}, _Env) when N > 0, D==0 ->
    ?PINF;
'make-real'({N, _D}, _Env) when N==0 ->
    N;
'make-real'({N, 1}, _Env) when erlang:is_integer(N) ->
    N;
'make-real'({N, D}, _Env) ->
    N / D;
'make-real'(X, _Env) when erlang:is_number(X) ->
    X.

%% @doc This function is a temporary place holder
-spec 'make-polar'(scm_x(), scm_y(), scmi_env()) -> scm_z().
'make-polar'(X, Y, _Env) ->
    {polar, {X, Y}}.

%% @doc This function is a temporary place holder
-spec 'make-rectangular'(scm_x(), scm_y(), scmi_env()) -> scm_z().
'make-rectangular'(X, Y, _Env) ->
    {rectangular, {X, Y}}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
