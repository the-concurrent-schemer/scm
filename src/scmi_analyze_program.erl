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

%%% @doc Scheme interpreter syntactic analyzer for program expressions
%%% @author Joseph Wayne Norton <norton@alum.mit.edu>

-module(scmi_analyze_program).

%% External exports
-export([analyze_import/2
         , analyze_define/2
         , analyze_define_values/2
         , analyze_define_syntax/2
         , analyze_define_record_type/2
         , analyze_define_library/2
        ]).

%% Internal imports
-import(scmi_analyze, [analyze/2, validate_variable/1, validate_variables/1, flatten_variables/1, make_tmp_variables/1]).

-include("scmi_analyze.hrl").

%%%----------------------------------------------------------------------
%%% Types/Specs/Records
%%%----------------------------------------------------------------------

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

analyze_import(Exp, Ana) ->
    %% @TODO
    erlang:error({roadmap,'v0.6.0'}, [Exp, Ana]).

analyze_define([Variable, Exp], Ana) when not is_list(Variable) ->
    validate_variable(Variable), % validate variable
    Exec = analyze(Exp, Ana),
    fun(Env, Ok, Ng) ->
            %% execute operands
            Exec(Env,
                 fun(Val, Ng1) ->
                         %% define value
                         scmi_env:define_variable(Variable, Val, Env),
                         Ok(?FALSE,
                            fun(Err) -> Ng1(Err) end)
                 end,
                 Ng)
    end;
analyze_define(Exp, Ana) ->
    analyze(from_define(Exp), Ana).

analyze_define_values(Exp, Ana) ->
    analyze(from_define_values(Exp), Ana).

analyze_define_syntax(Exp, Ana) ->
    %% @TODO
    erlang:error({roadmap,'v0.7.0'}, [Exp, Ana]).

analyze_define_record_type(Exp, Ana) ->
    %% @TODO
    erlang:error({roadmap,'v0.7.0'}, [Exp, Ana]).

analyze_define_library(Exp, Ana) ->
    %% @TODO
    erlang:error({roadmap,'v0.6.0'}, [Exp, Ana]).

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%% define
from_define([[Variable|Formal]|Body]) when not is_list(Variable), not is_list(Formal)  ->
    make_define(Variable, make_lambda(Formal, Body));
from_define([[[Variable|Formals]|Formal]|Body]) when not is_list(Variable), is_list(Formals), not is_list(Formal)  ->
    make_define(Variable, make_lambda([Formals|Formal], Body));
from_define([[Variable|Formals]|Body]) when not is_list(Variable), is_list(Formals) ->
    make_define(Variable, make_lambda(Formals, Body)).

%% define-values
from_define_values([[], Body]) ->
    make_begin(Body ++ [?FALSE]);
from_define_values([Formals|Body]) ->
    Tmps = make_tmp_variables(Formals),
    Fs = flatten_variables(Formals),
    validate_variables(Fs), % validate formals
    Ts = flatten_variables(Tmps),
    Defines = [ make_define(F, ?UNASSIGNED) || F <- Fs ],
    Sets = [ make_setb(F, T) || {F, T} <- lists:zip(Fs, Ts) ],
    Producer = make_thunk(Body),
    Consumer = make_lambda(Tmps, Sets),
    make_begin(Defines ++ [make_call_with_values(Producer, Consumer), ?FALSE]).
