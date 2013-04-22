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

%%% @doc Scheme eval library
%%% @author Joseph Wayne Norton <norton@alum.mit.edu>

-module(scml_eval).

%% SCML Exports
-export(['$scml_exports'/0]).

%% API
-export(['environment'/1
         , 'eval'/2
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
    [{'environment', #nipv{val=fun 'environment'/1}}
     , {'eval', #nipn{val=fun 'eval'/2}}
    ].

%%%===================================================================
%%% API
%%%===================================================================

-spec 'environment'(scmi_vargs()) -> scmi_env().
'environment'([]) ->
    scmi_env:the_empty();
'environment'(L) ->
    %% @TODO
    erlang:error({roadmap,'v0.6.0'}, [L]).

-spec 'eval'(scm_any(), scmi_env()) -> scm_any().
'eval'(ExprOrDef, Env) ->
    scmi_eval:'eval'(ExprOrDef, Env).

%%%===================================================================
%%% internal helpers
%%%===================================================================
