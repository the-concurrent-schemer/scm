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
-export(['eqv?'/2
         , 'features'/0
         , 'libraries'/0
         , 'not'/1
         , 'exact'/2
         , 'inexact'/2
         , 'make-rational'/2
         , 'make-rational'/3
         , 'make-real'/2
         , 'make-polar'/3
         , 'make-rectangular'/3
        ]).

-include("scm.hrl").
-include("scmi.hrl").

%%%----------------------------------------------------------------------
%%% Types/Specs/Records
%%%----------------------------------------------------------------------

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

%% @doc This function is a temporary place holder and is not (yet)
%% compliant with the R7RS specification.
-spec 'eqv?'(scm_obj(), scm_obj()) -> scm_boolean().
'eqv?'(?TRUE, ?TRUE) ->
    ?TRUE;
'eqv?'(?FALSE, ?FALSE) ->
    ?TRUE;
'eqv?'(A, A) when is_atom(A) ->
    ?TRUE;
'eqv?'({Sha, A}, {Sha, A}) when is_atom(Sha), is_binary(A) ->
    ?TRUE;
'eqv?'(A, B) when is_number(A), is_number(B) ->
    case A == B of
        true ->
            ?TRUE;
        false ->
            ?FALSE
    end;
'eqv?'({A, B}, {C, D}) when is_number(A), is_number(B), is_number(C), is_number(D) ->
    case A == B andalso C == D of
        true ->
            ?TRUE;
        false ->
            ?FALSE
    end;
'eqv?'(?PINF, ?PINF) ->
    ?TRUE;
'eqv?'(?NINF, ?NINF) ->
    ?TRUE;
'eqv?'(?NZER, ?NZER) ->
    ?TRUE;
'eqv?'(A, ?NZER) when A == 0 ->
    ?TRUE;
'eqv?'(?NZER, B) when B == 0 ->
    ?TRUE;
'eqv?'({Complex, {A, B}}, {Complex, {C, D}}) when Complex==rectangular; Complex==polar ->
    case 'eqv?'(A, C) of
        ?FALSE ->
            ?FALSE;
        _ ->
            'eqv?'(B, D)
    end;
'eqv?'(#character{val=A}, #character{val=A}) ->
    ?TRUE;
'eqv?'([], []) ->
    ?TRUE;
'eqv?'(#vector{val=A}, #vector{val=B}) ->
    'eqv?'(tuple_to_list(A), tuple_to_list(B));
'eqv?'(#bytevector{val=A}, #bytevector{val=A}) ->
    ?TRUE;
'eqv?'(#string{val=A}, #string{val=A}) ->
    ?TRUE;
'eqv?'([H1|T1], [H2|T2]) ->
    case 'eqv?'(H1, H2) of
        ?FALSE ->
            ?FALSE;
        _ ->
            'eqv?'(T1, T2)
    end;
'eqv?'(_, _) ->
    ?FALSE.

%% @doc This function is a temporary place holder
-spec features() -> [scm_symbol()].
features() ->
    ['r7rs'
     %% , 'exact-closed'  @TODO v0.5.0
     %% , 'exact-complex' @TODO v0.5.0
     %% , 'ieee-float'    @TODO v0.5.0
     , 'full-unicode'
     %% , 'ratios'        @TODO v0.5.0
    ]
        ++ [ 'posix' || has_posix() ]
        ++ [ 'windows' || has_windows() ]
        ++ system_info()
        ++ [ 'big-endian' ]
        ++ [ ?SCM, ?SCMVSN ].

%% @doc This function is a temporary place holder
-spec libraries() -> [scm_symbol()].
libraries() ->
    [
     %% , 'base'            @TODO v0.4.0/v0.5.0
     %% , 'case-lambda'     @TODO v0.6.0
     %% , 'char'            @TODO v0.6.0
     %% , 'complex'         @TODO v0.5.0
     %% , 'cxr'             @TODO v0.6.0
     %% , 'file'            @TODO v0.6.0
     %% , 'inexact'         @TODO v0.5.0
     %% , 'lazy'            @TODO v0.6.0
     %% , 'load'            @TODO v0.6.0
     %% , 'process-context' @TODO v0.6.0
     %% , 'read'            @TODO v0.6.0
     %% , 'repl'            @TODO v0.6.0
     %% , 'time'            @TODO v0.6.0
     %% , 'write'           @TODO v0.6.0
     %% , 'r5rs'            @TODO v0.6.0
     ].

%% @doc This function is a temporary place holder
-spec 'not'(scm_obj()) -> scm_boolean().
'not'(?FALSE) ->
    ?TRUE;
'not'(_) ->
    ?FALSE.

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

has_posix() ->
    case os:type() of
        {unix, _} ->
            true;
        _ ->
            false
    end.

has_windows() ->
    case os:type() of
        {win32, _} ->
            true;
        _ ->
            false
    end.

system_info() ->
    {OSFamily, OSName} = os:type(),
    OTPRel = erlang:system_info(otp_release),
    SysArch = erlang:system_info(system_architecture),
    WordSize = integer_to_list(erlang:system_info({wordsize, external}) * 8),
    Strs = [OTPRel, SysArch, WordSize] ++ string:tokens(SysArch, "-"),
    lists:sort([OSFamily, OSName] ++ [ list_to_atom(Str) || Str <- Strs ]).
