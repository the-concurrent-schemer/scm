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

-module(scml_base_control).

-include("scmi.hrl").

%% SCML Exports
-export(['$scml_exports'/0]).

%% API
-export(['procedure?'/1
         , 'apply'/1
         , 'map'/1
         , 'string-map'/1
         , 'vector-map'/1
         , 'for-each'/1
         , 'string-for-each'/1
         , 'vector-for-each'/1
         , 'call-with-current-continuation'/1
         , 'values'/1
         , 'call-with-values'/2
         , 'dynamic-wind'/3
        ]).

-include("scmi.hrl").

%%%===================================================================
%%% SCML Exports
%%%===================================================================

-spec '$scml_exports'() -> [{scm_symbol(), scmi_nip()}].
'$scml_exports'() ->
    [{'procedure?', #nipn{val=fun 'procedure?'/1}}
     , {'apply', #nipv{val=fun 'apply'/1}}
     , {'map', #nipv{val=fun 'map'/1}}
     , {'string-map', #nipv{val=fun 'string-map'/1}}
     , {'vector-map', #nipv{val=fun 'vector-map'/1}}
     , {'for-each', #nipv{val=fun 'for-each'/1}}
     , {'string-for-each', #nipv{val=fun 'string-for-each'/1}}
     , {'vector-for-each', #nipv{val=fun 'vector-for-each'/1}}
     , {'call-with-current-continuation', #nipn{val=fun 'call-with-current-continuation'/1}}
     , {'call/cc', #nipn{val=fun 'call-with-current-continuation'/1}}
     , {'values', #nipv{val=fun 'values'/1}}
     , {'call-with-values', #nipn{val=fun 'call-with-values'/2}}
     , {'dynamic-wind', #nipn{val=fun 'dynamic-wind'/3}}
    ].

%%%===================================================================
%%% API
%%%===================================================================

-spec 'procedure?'(scm_obj()) -> scm_boolean().
'procedure?'(Obj) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Obj]).

-spec 'apply'([scm_any(),...]) -> scm_any().
'apply'([Proc|Args]) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Proc, Args]).

-spec 'map'([scm_any(),...]) -> [scm_any()].
'map'([Proc|Args]) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Proc, Args]).

-spec 'string-map'([scm_any(),...]) -> scm_string().
'string-map'([Proc|Args]) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Proc, Args]).

-spec 'vector-map'([scm_any(),...]) -> scm_vector().
'vector-map'([Proc|Args]) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Proc, Args]).

-spec 'for-each'([scm_any(),...]) -> scm_false().
'for-each'([Proc|Args]) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Proc, Args]).

-spec 'string-for-each'([scm_any(),...]) -> scm_false().
'string-for-each'([Proc|Args]) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Proc, Args]).

-spec 'vector-for-each'([scm_any(),...]) -> scm_false().
'vector-for-each'([Proc|Args]) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Proc, Args]).

-spec 'call-with-current-continuation'(scm_proc()) -> scm_any().
'call-with-current-continuation'(Proc) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Proc]).

-spec 'values'([scm_any(),...]) -> scm_any().
'values'(Args) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Args]).

-spec 'call-with-values'(scm_thunk(), scm_proc()) -> scm_any().
'call-with-values'(Producer, Consumer) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Producer, Consumer]).

-spec 'dynamic-wind'(scm_thunk(), scm_thunk(), scm_thunk()) -> scm_any().
'dynamic-wind'(Before, Thunk, After) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Before, Thunk, After]).

%%%===================================================================
%%% internal helpers
%%%===================================================================
