%%% The MIT License
%%%
%%% Copyright (C) 2013-2015 by Joseph Wayne Norton <norton@alum.mit.edu>
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

%%% @doc Scheme interpreter expression evaluator
%%% @author Joseph Wayne Norton <norton@alum.mit.edu>

-module(scmi_eval).

%% External exports
-export([eval/1
        , eval/2
        , eval/3
        , eval/4
        , exec/1
        , exec/2
        , exec/3
        , exec/4
        , default_ok/2
        , default_ng/1
        ]).

-include("scmi.hrl").

%%%----------------------------------------------------------------------
%%% Types/Specs/Records
%%%----------------------------------------------------------------------

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

-spec eval(scmi_exp()) -> scmi_exp().
eval(Exp) ->
    exec(scmi_analyze:analyze(Exp)).

-spec eval(scmi_exp(), scmi_denv()) -> scmi_exp().
eval(Exp, Env) ->
    exec(scmi_analyze:analyze(Exp), Env).

-spec eval(scmi_exp(), scmi_denv(), scmi_dok()) -> scmi_exp().
eval(Exp, Env, Ok) ->
    exec(scmi_analyze:analyze(Exp), Env, Ok).

-spec eval(scmi_exp(), scmi_denv(), scmi_dok(), scmi_dng()) -> scmi_exp().
eval(Exp, Env, Ok, Ng) ->
    exec(scmi_analyze:analyze(Exp), Env, Ok, Ng).

-spec exec(scmi_dexec()) -> scmi_exp().
exec(Exec) ->
    exec(Exec, scmi_env:the_empty()).

-spec exec(scmi_dexec(), scmi_denv()) -> scmi_exp().
exec(Exec, Env) ->
    exec(Exec, Env, fun default_ok/2).

-spec exec(scmi_dexec(), scmi_denv(), scmi_dok()) -> scmi_exp().
exec(Exec, Env, Ok) ->
    exec(Exec, Env, Ok, fun default_ng/1).

-spec exec(scmi_dexec(), scmi_denv(), scmi_dok(), scmi_dng()) -> scmi_exp().
exec(Exec, Env, Ok, Ng) ->
    Exec(Env, Ok, Ng).

-spec default_ok(scmi_exp(), scmi_dng()) -> scmi_exp().
default_ok(Value, _Ng) ->
    Value.

-spec default_ng(scmi_exp()) -> no_return().
default_ng(Error) ->
    erlang:error(scm_exception, [Error]).

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
