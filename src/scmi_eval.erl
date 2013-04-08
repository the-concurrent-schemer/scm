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

-module(scmi_eval).

-include("scmd_type.hrl").
-include("scmi.hrl").

%% External exports
-export([eval/1
         , eval/2
         , eval/3
         , eval/4
         , exec/1
         , exec/2
         , exec/3
         , exec/4
        ]).

%%%----------------------------------------------------------------------
%%% Types/Specs/Records
%%%----------------------------------------------------------------------

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

-spec eval(scm_any()) -> scm_any().
eval(Exp) ->
    exec(scmi_analyze:analyze(Exp)).

-spec eval(scm_any(), scmi_env()) -> scm_any().
eval(Exp, Env) ->
    exec(scmi_analyze:analyze(Exp), Env).

-spec eval(scm_any(), scmi_env(), scmi_ccok()) -> scm_any().
eval(Exp, Env, Ok) ->
    exec(scmi_analyze:analyze(Exp), Env, Ok).

-spec eval(scm_any(), scmi_env(), scmi_ccok(), scmi_ccng()) -> scm_any().
eval(Exp, Env, Ok, Ng) ->
    exec(scmi_analyze:analyze(Exp), Env, Ok, Ng).

-spec exec(scmi_exec()) -> scm_any().
exec(Exec) ->
    exec(Exec, scmi_env:the_empty()).

-spec exec(scmi_exec(), scmi_env()) -> scm_any().
exec(Exec, Env) ->
    exec(Exec, Env, fun ccok/2).

-spec exec(scmi_exec(), scmi_env(), scmi_ccok()) -> scm_any().
exec(Exec, Env, Ok) ->
    exec(Exec, Env, Ok, fun ccng/1).

-spec exec(scmi_exec(), scmi_env(), scmi_ccok(), scmi_ccng()) -> scm_any().
exec(Exec, Env, Ok, Ng) ->
    Exec(Env, Ok, Ng).

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

ccok(Value, _Ng) ->
    Value.

ccng(Error) ->
    erlang:error(scm_exception, [Error]).
