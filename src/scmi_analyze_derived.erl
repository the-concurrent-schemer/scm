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

-module(scmi_analyze_derived).

-include("scmi.hrl").
-include("scmi_analyze.hrl").

%% External exports
-export([analyze_cond/2
         , analyze_case/2
         , analyze_and/2
         , analyze_or/2
         , analyze_when/2
         , analyze_unless/2
         , analyze_cond_expand/2
         , analyze_let/2
         , analyze_lets/2
         , analyze_letrec/2
         , analyze_letrecs/2
         , analyze_let_values/2
         , analyze_lets_values/2
         , analyze_letrec_values/2
         , analyze_begin/2
         , analyze_do/2
         , analyze_make_parameter/2
         , analyze_parameterize/2
         , analyze_guard/2
         , analyze_quasiquote/2
         , analyze_unquote/2
         , analyze_unquote_splicing/2
         , scan_out_internal_definitions/1
        ]).

%%%----------------------------------------------------------------------
%%% Types/Specs/Records
%%%----------------------------------------------------------------------

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

-spec analyze_cond(scm_any(), scmi_ana()) -> scmi_exec().
analyze_cond(Exp, Ana) ->
    %% @TODO
    erlang:error({roadmap,'v0.3.0'}, [Exp, Ana]).

-spec analyze_case(scm_any(), scmi_ana()) -> scmi_exec().
analyze_case(Exp, Ana) ->
    %% @TODO
    erlang:error({roadmap,'v0.3.0'}, [Exp, Ana]).

-spec analyze_and(scm_any(), scmi_ana()) -> scmi_exec().
analyze_and(Exp, Ana) ->
    %% @TODO
    erlang:error({roadmap,'v0.3.0'}, [Exp, Ana]).

-spec analyze_or(scm_any(), scmi_ana()) -> scmi_exec().
analyze_or(Exp, Ana) ->
    %% @TODO
    erlang:error({roadmap,'v0.3.0'}, [Exp, Ana]).

-spec analyze_when(scm_any(), scmi_ana()) -> scmi_exec().
analyze_when(Exp, Ana) ->
    %% @TODO
    erlang:error({roadmap,'v0.3.0'}, [Exp, Ana]).

-spec analyze_unless(scm_any(), scmi_ana()) -> scmi_exec().
analyze_unless(Exp, Ana) ->
    %% @TODO
    erlang:error({roadmap,'v0.3.0'}, [Exp, Ana]).

-spec analyze_cond_expand(scm_any(), scmi_ana()) -> scmi_exec().
analyze_cond_expand(Exp, Ana) ->
    %% @TODO
    erlang:error({roadmap,'v0.3.0'}, [Exp, Ana]).

-spec analyze_let(scm_any(), scmi_ana()) -> scmi_exec().
analyze_let(Exp, Ana) ->
    %% @TODO
    erlang:error({roadmap,'v0.3.0'}, [Exp, Ana]).

-spec analyze_lets(scm_any(), scmi_ana()) -> scmi_exec().
analyze_lets(Exp, Ana) ->
    %% @TODO
    erlang:error({roadmap,'v0.3.0'}, [Exp, Ana]).

-spec analyze_letrec(scm_any(), scmi_ana()) -> scmi_exec().
analyze_letrec(Exp, Ana) ->
    %% @TODO
    erlang:error({roadmap,'v0.3.0'}, [Exp, Ana]).

-spec analyze_letrecs(scm_any(), scmi_ana()) -> scmi_exec().
analyze_letrecs(Exp, Ana) ->
    %% @TODO
    erlang:error({roadmap,'v0.3.0'}, [Exp, Ana]).

-spec analyze_let_values(scm_any(), scmi_ana()) -> scmi_exec().
analyze_let_values(Exp, Ana) ->
    %% @TODO
    erlang:error({roadmap,'v0.3.0'}, [Exp, Ana]).

-spec analyze_lets_values(scm_any(), scmi_ana()) -> scmi_exec().
analyze_lets_values(Exp, Ana) ->
    %% @TODO
    erlang:error({roadmap,'v0.3.0'}, [Exp, Ana]).

-spec analyze_letrec_values(scm_any(), scmi_ana()) -> scmi_exec().
analyze_letrec_values(Exp, Ana) ->
    %% @TODO
    erlang:error({roadmap,'v0.3.0'}, [Exp, Ana]).

-spec analyze_begin(scm_any(), scmi_ana()) -> scmi_exec().
analyze_begin(Exp, Ana) ->
    %% @TODO
    erlang:error({roadmap,'v0.3.0'}, [Exp, Ana]).

-spec analyze_do(scm_any(), scmi_ana()) -> scmi_exec().
analyze_do(Exp, Ana) ->
    %% @TODO
    erlang:error({roadmap,'v0.3.0'}, [Exp, Ana]).

-spec analyze_make_parameter(scm_any(), scmi_ana()) -> scmi_exec().
analyze_make_parameter(Exp, Ana) ->
    %% @TODO
    erlang:error({roadmap,'v0.3.0'}, [Exp, Ana]).

-spec analyze_parameterize(scm_any(), scmi_ana()) -> scmi_exec().
analyze_parameterize(Exp, Ana) ->
    %% @TODO
    erlang:error({roadmap,'v0.3.0'}, [Exp, Ana]).

-spec analyze_guard(scm_any(), scmi_ana()) -> scmi_exec().
analyze_guard(Exp, Ana) ->
    %% @TODO
    erlang:error({roadmap,'v0.3.0'}, [Exp, Ana]).

-spec analyze_quasiquote(scm_any(), scmi_ana()) -> scmi_exec().
analyze_quasiquote(Exp, Ana) ->
    %% @TODO
    erlang:error({roadmap,'v0.7.0'}, [Exp, Ana]).

-spec analyze_unquote(scm_any(), scmi_ana()) -> scmi_exec().
analyze_unquote(Exp, Ana) ->
    %% @TODO
    erlang:error({roadmap,'v0.7.0'}, [Exp, Ana]).

-spec analyze_unquote_splicing(scm_any(), scmi_ana()) -> scmi_exec().
analyze_unquote_splicing(Exp, Ana) ->
    %% @TODO
    erlang:error({roadmap,'v0.7.0'}, [Exp, Ana]).

-spec scan_out_internal_definitions([scm_any(),...]) -> [scm_any()].
scan_out_internal_definitions(Body) ->
    %% @TODO - This is a placeholder until {roadmap,'v0.3.0'}
    Body.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
