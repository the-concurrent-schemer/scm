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

%%% @doc Scheme base library for bytevectors
%%% @author CSCM Contributor <the-concurrent-schemer@googlegroups.com>

-module(scml_base_bytevector).

%% SCML Exports
-export(['$scml_exports'/0]).

%% API
-export(['bytevector?'/1
         , 'make-bytevector'/1
         , 'make-bytevector'/2
         , 'bytevector'/1
         , 'bytevector-length'/1
         , 'bytevector-u8-ref'/2
         , 'bytevector-u8-set!'/3
         , 'bytevector-copy'/1
         , 'bytevector-copy'/2
         , 'bytevector-copy'/3
         , 'bytevector-copy!'/3
         , 'bytevector-copy!'/4
         , 'bytevector-copy!'/5
         , 'bytevector-append'/1
         , 'utf8->string'/1
         , 'utf8->string'/2
         , 'utf8->string'/3
         , 'string->utf8'/1
         , 'string->utf8'/2
         , 'string->utf8'/3
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
    [{'bytevector?', #nipn{val=fun 'bytevector?'/1}}
     , {'make-bytevector', #nipn{val=[fun 'make-bytevector'/1, fun 'make-bytevector'/2]}}
     , {'bytevector', #nipn{val=fun 'bytevector'/1}}
     , {'bytevector-length', #nipn{val=fun 'bytevector-length'/1}}
     , {'bytevector-u8-ref', #nipn{val=fun 'bytevector-u8-ref'/2}}
     , {'bytevector-u8-set!', #nipn{val=fun 'bytevector-u8-set!'/3}}
     , {'bytevector-copy', #nipn{val=[fun 'bytevector-copy'/1, fun 'bytevector-copy'/2, fun 'bytevector-copy'/3]}}
     , {'bytevector-copy!', #nipn{val=[fun 'bytevector-copy!'/3, fun 'bytevector-copy!'/4, fun 'bytevector-copy!'/5]}}
     , {'bytevector-append', #nipv{val=fun 'bytevector-append'/1}}
     , {'utf8->string', #nipn{val=[fun 'utf8->string'/1, fun 'utf8->string'/2, fun 'utf8->string'/3]}}
     , {'string->utf8', #nipn{val=[fun 'string->utf8'/1, fun 'string->utf8'/2, fun 'string->utf8'/3]}}
    ].

%%%===================================================================
%%% API
%%%===================================================================

-spec 'bytevector?'(scm_obj()) -> scm_boolean().
'bytevector?'(Obj) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Obj]).

-spec 'make-bytevector'(scm_k()) -> scm_bytevector().
'make-bytevector'(K) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [K]).

-spec 'make-bytevector'(scm_k(), scm_byte()) -> scm_bytevector().
'make-bytevector'(K, Byte) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [K, Byte]).

-spec 'bytevector'([scm_byte(),...]) -> scm_bytevector().
'bytevector'(Bytes) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Bytes]).

-spec 'bytevector-length'(scm_bytevector()) -> scm_k().
'bytevector-length'(V) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [V]).

-spec 'bytevector-u8-ref'(scm_bytevector(), scm_k()) -> scm_byte().
'bytevector-u8-ref'(V, K) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [V, K]).

-spec 'bytevector-u8-set!'(scm_bytevector(), scm_k(), scm_byte()) -> scm_false().
'bytevector-u8-set!'(V, K, Byte) ->
    erlang:error(unsupported, [V, K, Byte]).

-spec 'bytevector-copy'(scm_bytevector()) -> scm_bytevector().
'bytevector-copy'(V) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [V]).

-spec 'bytevector-copy'(scm_bytevector(), scm_start()) -> scm_bytevector().
'bytevector-copy'(V, Start) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [V, Start]).

-spec 'bytevector-copy'(scm_bytevector(), scm_start(), scm_end()) -> scm_bytevector().
'bytevector-copy'(V, Start, End) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [V, Start, End]).

-spec 'bytevector-copy!'(scm_bytevector(), scm_k(), scm_bytevector()) -> scm_false().
'bytevector-copy!'(To, At, From) ->
    erlang:error(unsupported, [To, At, From]).

-spec 'bytevector-copy!'(scm_bytevector(), scm_k(), scm_bytevector(), scm_start()) -> scm_false().
'bytevector-copy!'(To, At, From, Start) ->
    erlang:error(unsupported, [To, At, From, Start]).

-spec 'bytevector-copy!'(scm_bytevector(), scm_k(), scm_bytevector(), scm_start(), scm_end()) -> scm_false().
'bytevector-copy!'(To, At, From, Start, End) ->
    erlang:error(unsupported, [To, At, From, Start, End]).

-spec 'bytevector-append'([scm_bytevector(),...]) -> scm_bytevector().
'bytevector-append'(Vs) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Vs]).

-spec 'utf8->string'(scm_bytevector()) -> scm_string().
'utf8->string'(V) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [V]).

-spec 'utf8->string'(scm_bytevector(), scm_start()) -> scm_string().
'utf8->string'(V, Start) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [V, Start]).

-spec 'utf8->string'(scm_bytevector(), scm_start(), scm_end()) -> scm_string().
'utf8->string'(V, Start, End) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [V, Start, End]).

-spec 'string->utf8'(scm_string()) -> scm_bytevector().
'string->utf8'(S) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [S]).

-spec 'string->utf8'(scm_string(), scm_start()) -> scm_bytevector().
'string->utf8'(S, Start) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [S, Start]).

-spec 'string->utf8'(scm_string(), scm_start(), scm_end()) -> scm_bytevector().
'string->utf8'(S, Start, End) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [S, Start, End]).

%%%===================================================================
%%% internal helpers
%%%===================================================================
