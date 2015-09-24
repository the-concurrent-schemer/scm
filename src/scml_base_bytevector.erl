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
    [{'bytevector?', #nipn{val=fun ?MODULE:'bytevector?'/1}}
    , {'make-bytevector', #nipn{val=[fun ?MODULE:'make-bytevector'/1, fun ?MODULE:'make-bytevector'/2]}}
    , {'bytevector', #nipn{val=fun ?MODULE:'bytevector'/1}}
    , {'bytevector-length', #nipn{val=fun ?MODULE:'bytevector-length'/1}}
    , {'bytevector-u8-ref', #nipn{val=fun ?MODULE:'bytevector-u8-ref'/2}}
    , {'bytevector-u8-set!', #nipn{val=fun ?MODULE:'bytevector-u8-set!'/3}}
    , {'bytevector-copy', #nipn{val=[fun ?MODULE:'bytevector-copy'/1, fun ?MODULE:'bytevector-copy'/2, fun ?MODULE:'bytevector-copy'/3]}}
    , {'bytevector-copy!', #nipn{val=[fun ?MODULE:'bytevector-copy!'/3, fun ?MODULE:'bytevector-copy!'/4, fun ?MODULE:'bytevector-copy!'/5]}}
    , {'bytevector-append', #nipv{val=fun ?MODULE:'bytevector-append'/1}}
    , {'utf8->string', #nipn{val=[fun ?MODULE:'utf8->string'/1, fun ?MODULE:'utf8->string'/2, fun ?MODULE:'utf8->string'/3]}}
    , {'string->utf8', #nipn{val=[fun ?MODULE:'string->utf8'/1, fun ?MODULE:'string->utf8'/2, fun ?MODULE:'string->utf8'/3]}}
    ].

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Returns #t if obj is a bytevector, otherwise returns #f.
-spec 'bytevector?'(scm_obj()) -> scm_boolean().
'bytevector?'(#bytevector{}) ->
    ?TRUE;
'bytevector?'(_) ->
    ?FALSE.

%% @equiv 'make-bytevector'(K, 0)
-spec 'make-bytevector'(scm_k()) -> scm_bytevector().
'make-bytevector'(K) ->
    'make-bytevector'(K, 0).

%% @doc Returns a bytevector of k bytes.
-spec 'make-bytevector'(scm_k(), scm_byte()) -> scm_bytevector().
'make-bytevector'(K, Byte) ->
    #bytevector{val=binary:copy(integer_to_binary(Byte), K)}.

%% @doc Returns a bytevector whose bytes contain the given arguments.
-spec 'bytevector'([scm_byte(),...]) -> scm_bytevector().
'bytevector'(Bytes) ->
    #bytevector{val=binary:list_to_bin(Bytes)}.

%% @doc Returns the number of bytes in the given bytevector.
-spec 'bytevector-length'(scm_bytevector()) -> scm_k().
'bytevector-length'(#bytevector{val=V}) ->
    byte_size(V).

%% @doc Returns byte k of bytevector using zero-origin indexing.  It
%% is an error if k is not a valid index of bytevector.
-spec 'bytevector-u8-ref'(scm_bytevector(), scm_k()) -> scm_byte().
'bytevector-u8-ref'(#bytevector{val=V}, K) ->
    binary:at(V, K).

%% @doc _unsupported_
-spec 'bytevector-u8-set!'(scm_bytevector(), scm_k(), scm_byte()) -> scm_false().
'bytevector-u8-set!'(V, K, Byte) ->
    erlang:error(unsupported, [V, K, Byte]).

%% @equiv 'bytevector-copy'(V, 0, 'bytevector-length'(V))
-spec 'bytevector-copy'(scm_bytevector()) -> scm_bytevector().
'bytevector-copy'(V) ->
    V.

%% @equiv 'bytevector-copy'(V, Start, 'bytevector-length'(V))
-spec 'bytevector-copy'(scm_bytevector(), scm_start()) -> scm_bytevector().
'bytevector-copy'(#bytevector{val=V}, Start) ->
    #bytevector{val=scml:binary_part(V, Start)}.

%% @doc Returns a bytevector constructed from the bytes of bytevector
%% beginning with index start and ending with index end.
-spec 'bytevector-copy'(scm_bytevector(), scm_start(), scm_end()) -> scm_bytevector().
'bytevector-copy'(#bytevector{val=V}, Start, End) ->
    #bytevector{val=scml:binary_part(V, Start, End)}.

%% @doc _unsupported_
-spec 'bytevector-copy!'(scm_bytevector(), scm_k(), scm_bytevector()) -> scm_false().
'bytevector-copy!'(To, At, From) ->
    erlang:error(unsupported, [To, At, From]).

%% @doc _unsupported_
-spec 'bytevector-copy!'(scm_bytevector(), scm_k(), scm_bytevector(), scm_start()) -> scm_false().
'bytevector-copy!'(To, At, From, Start) ->
    erlang:error(unsupported, [To, At, From, Start]).

%% @doc _unsupported_
-spec 'bytevector-copy!'(scm_bytevector(), scm_k(), scm_bytevector(), scm_start(), scm_end()) -> scm_false().
'bytevector-copy!'(To, At, From, Start, End) ->
    erlang:error(unsupported, [To, At, From, Start, End]).

%% @doc Returns a bytevector whose bytes are the concatenation of the
%% bytes in the given bytevectors.
-spec 'bytevector-append'([scm_bytevector(),...]) -> scm_bytevector().
'bytevector-append'(Vs) ->
    #bytevector{val=binary:list_to_bin([ V || #bytevector{val=V} <- Vs ])}.

%% @equiv 'utf8->string'(V, 0, 'bytevector-length'(V))
-spec 'utf8->string'(scm_bytevector()) -> scm_string().
'utf8->string'(#bytevector{val=V}) ->
    #string{val=list_to_tuple(scml:utf8_to_unicode(V))}.

%% @equiv 'utf8->string'(V, Start, 'bytevector-length'(V))
-spec 'utf8->string'(scm_bytevector(), scm_start()) -> scm_string().
'utf8->string'(#bytevector{val=V}, Start) ->
    #string{val=list_to_tuple(scml:utf8_to_unicode(V, Start))}.

%% @doc Decodes the bytes of a bytevector between start and end and
%% returns the corresponding string.
-spec 'utf8->string'(scm_bytevector(), scm_start(), scm_end()) -> scm_string().
'utf8->string'(#bytevector{val=V}, Start, End) ->
    #string{val=list_to_tuple(scml:utf8_to_unicode(V, Start, End))}.

%% @equiv 'string->utf8'(S, 0, 'string-length'(S))
-spec 'string->utf8'(scm_string()) -> scm_bytevector().
'string->utf8'(#string{val=S}) ->
    #bytevector{val=scml:unicode_to_utf8(tuple_to_list(S))}.

%% @equiv 'string->utf8'(S, Start, 'string-length'(S))
-spec 'string->utf8'(scm_string(), scm_start()) -> scm_bytevector().
'string->utf8'(#string{val=S}, Start) ->
    #bytevector{val=scml:unicode_to_utf8(tuple_to_list(S), Start)}.

%% @doc encodes the characters of a string between start and end and
%% returns the corresponding bytevector.
-spec 'string->utf8'(scm_string(), scm_start(), scm_end()) -> scm_bytevector().
'string->utf8'(#string{val=S}, Start, End) ->
    #bytevector{val=scml:unicode_to_utf8(tuple_to_list(S), Start, End)}.

%%%===================================================================
%%% internal helpers
%%%===================================================================
