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

%%% @doc Scheme lazy library
%%% @author CSCM Contributor <the-concurrent-schemer@googlegroups.com>

-module(scml_lazy).

%% SCML Exports
-export(['$scml_exports'/0]).

%% API
-export(['delay'/1
        , 'delay-force'/1
        , 'force'/1
        , 'promise?'/1
        , 'make-promise'/1
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
    [{'delay', #nipv{val=fun ?MODULE:'delay'/1}}
    , {'delay-force', #nipv{val=fun ?MODULE:'delay-force'/1}}
    , {'force', #nipv{val=fun ?MODULE:'force'/1}}
    , {'promise?', #nipv{val=fun ?MODULE:'promise?'/1}}
    , {'make-promise', #nipv{val=fun ?MODULE:'make-promise'/1}}
    ].

%%%===================================================================
%%% API
%%%===================================================================

-spec 'delay'(scm_obj()) -> scm_obj().
'delay'(Exp) ->
    %% @TODO
    erlang:error({roadmap,'v0.6.0'}, [Exp]).

-spec 'delay-force'(scm_obj()) -> scm_obj().
'delay-force'(Exp) ->
    %% @TODO
    erlang:error({roadmap,'v0.6.0'}, [Exp]).

-spec 'force'(scm_obj()) -> scm_obj().
'force'(Promise) ->
    %% @TODO
    erlang:error({roadmap,'v0.6.0'}, [Promise]).

-spec 'promise?'(scm_obj()) -> scm_boolean().
'promise?'(Obj) ->
    %% @TODO
    erlang:error({roadmap,'v0.6.0'}, [Obj]).

-spec 'make-promise'(scm_obj()) -> scm_obj().
'make-promise'(Obj) ->
    %% @TODO
    erlang:error({roadmap,'v0.6.0'}, [Obj]).

%%%===================================================================
%%% internal helpers
%%%===================================================================
