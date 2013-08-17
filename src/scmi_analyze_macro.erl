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

%%% @doc Scheme interpreter syntactic analyzer for syntax expressions
%%% @author Joseph Wayne Norton <norton@alum.mit.edu>

-module(scmi_analyze_macro).

%% SCMI Exports
-export(['$scmi_exports'/0]).

%% External exports
-export([analyze_let_syntax/2
         , analyze_letrec_syntax/2
         , analyze_syntax_rules/2
         , analyze_syntax_error/2
        ]).

-include("scmi_analyze.hrl").

%%%----------------------------------------------------------------------
%%% Types/Specs/Records
%%%----------------------------------------------------------------------

%%%----------------------------------------------------------------------
%%% SCMI Exports
%%%----------------------------------------------------------------------

-spec '$scmi_exports'() -> [{scm_symbol(), scmi_sugar()}].
'$scmi_exports'() ->
    [{'let-syntax', #sugar{val=fun ?MODULE:'analyze_let_syntax'/2}}
     , {'letrec-syntax', #sugar{val=fun ?MODULE:'analyze_letrec_syntax'/2}}
     , {'syntax-rules', #sugar{val=fun ?MODULE:'analyze_syntax_rules'/2}}
     , {'syntax-error', #sugar{val=fun ?MODULE:'analyze_syntax_error'/2}}
    ].

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

analyze_let_syntax(Exp, SEnv) ->
    %% @TODO scan_out_internal_definitions(Body)
    erlang:error({roadmap,'v0.5.0'}, [Exp, SEnv]).

analyze_letrec_syntax(Exp, SEnv) ->
    %% @TODO scan_out_internal_definitions(Body)
    erlang:error({roadmap,'v0.5.0'}, [Exp, SEnv]).

analyze_syntax_rules(Exp, SEnv) ->
    %% @TODO
    erlang:error({roadmap,'v0.5.0'}, [Exp, SEnv]).

analyze_syntax_error(Exp, SEnv) ->
    %% @TODO
    erlang:error({roadmap,'v0.5.0'}, [Exp, SEnv]).

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
