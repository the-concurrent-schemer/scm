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

%%% @doc Scheme complex library
%%% @author CSCM Contributor <the-concurrent-schemer@googlegroups.com>

-module(scml_complex).

%% SCML Exports
-export(['$scml_exports'/0]).

%% API
-export(['make-rectangular'/2
         , 'make-polar'/2
         , 'real-part'/1
         , 'imag-part'/1
         , 'magnitude'/1
         , 'angle'/1
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
    [{'make-rectangular', #nipn{val=fun 'make-rectangular'/2}}
     , {'make-polar', #nipn{val=fun 'make-polar'/2}}
     , {'real-part', #nipn{val=fun 'real-part'/1}}
     , {'imag-part', #nipn{val=fun 'imag-part'/1}}
     , {'magnitude', #nipn{val=fun 'magnitude'/1}}
     , {'angle', #nipn{val=fun 'angle'/1}}
    ].

%%%===================================================================
%%% API
%%%===================================================================

-spec 'make-rectangular'(scm_x(), scm_x()) -> scm_z().
'make-rectangular'(X1, X2) ->
    %% @TODO
    erlang:error({roadmap,'v0.7.0'}, [X1, X2]).

-spec 'make-polar'(scm_x(), scm_x()) -> scm_z().
'make-polar'(X1, X2) ->
    %% @TODO
    erlang:error({roadmap,'v0.7.0'}, [X1, X2]).

-spec 'real-part'(scm_z()) -> scm_x().
'real-part'(Z) ->
    %% @TODO
    erlang:error({roadmap,'v0.7.0'}, [Z]).

-spec 'imag-part'(scm_z()) -> scm_x().
'imag-part'(Z) ->
    %% @TODO
    erlang:error({roadmap,'v0.7.0'}, [Z]).

-spec 'magnitude'(scm_z()) -> scm_x().
'magnitude'(Z) ->
    %% @TODO
    erlang:error({roadmap,'v0.7.0'}, [Z]).

-spec 'angle'(scm_z()) -> scm_x().
'angle'(Z) ->
    %% @TODO
    erlang:error({roadmap,'v0.7.0'}, [Z]).

%%%===================================================================
%%% internal helpers
%%%===================================================================
