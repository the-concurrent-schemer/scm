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

%%% @doc Scheme base library
%%% @author Joseph Wayne Norton <norton@alum.mit.edu>

-module(scml_base).

%% SCML Exports
-export(['$scml_exports'/0]).

%% API
-export([
        ]).

%% Proxy API
-compile({parse_transform,xfm_import_as_export}).

-import_as_export({scml_base_boolean,
                   ['boolean=?'/1
                   , 'boolean?'/1
                   , 'not'/1
                   ]}).

-import_as_export({scml_base_bytevector,
                   ['bytevector'/1
                   , 'bytevector-append'/1
                   , 'bytevector-copy'/1
                   , 'bytevector-copy'/2
                   , 'bytevector-copy'/3
                   , 'bytevector-copy!'/3
                   , 'bytevector-copy!'/4
                   , 'bytevector-copy!'/5
                   , 'bytevector-length'/1
                   , 'bytevector-u8-ref'/2
                   , 'bytevector-u8-set!'/3
                   , 'bytevector?'/1
                   , 'make-bytevector'/1
                   , 'make-bytevector'/2
                   , 'string->utf8'/1
                   , 'string->utf8'/2
                   , 'string->utf8'/3
                   , 'utf8->string'/1
                   , 'utf8->string'/2
                   , 'utf8->string'/3
                   ]}).

-import_as_export({scml_base_exception,
                   ['error'/4
                   , 'error-object-irritants'/1
                   , 'error-object-message'/1
                   , 'error-object?'/1
                   , 'raise'/4
                   , 'raise-continuable'/4
                   , 'read-error?'/1
                   , 'file-error?'/1
                   , 'with-exception-handler'/5
                   ]}).

-import_as_export({scml_base_char,
                   ['char->integer'/1
                   , 'char<=?'/1
                   , 'char<?'/1
                   , 'char=?'/1
                   , 'char>=?'/1
                   , 'char>?'/1
                   , 'char?'/1
                   , 'integer->char'/1
                   ]}).

-import_as_export({scml_base_control,
                   ['apply'/4
                   , 'call-with-values'/5
                   , 'call/cc'/4
                   , 'dynamic-wind'/6
                   , 'for-each'/4
                   , 'map'/4
                   , 'string-for-each'/4
                   , 'string-map'/4
                   , 'values'/4
                   , 'vector-for-each'/4
                   , 'vector-map'/4
                   , 'procedure?'/1
                   ]}).

-import_as_export({scml_base_equality,
                   ['eq?'/2
                   , 'equal?'/2
                   , 'eqv?'/2
                   ]}).

-import_as_export({scml_base_io,
                   ['binary-port?'/1
                   , 'call-with-port'/5
                   , 'char-ready?'/0
                   , 'char-ready?'/1
                   , 'close-input-port'/1
                   , 'close-output-port'/1
                   , 'close-port'/1
                   , 'current-error-port'/0
                   , 'current-error-port'/1
                   , 'current-error-port'/2
                   , 'current-input-port'/0
                   , 'current-output-port'/0
                   , 'eof-object'/0
                   , 'eof-object?'/1
                   , 'flush-output-port'/1
                   , 'flush-output-port'/0
                   , 'get-output-bytevector'/1
                   , 'get-output-string'/1
                   , 'input-port-open?'/1
                   , 'input-port?'/1
                   , 'newline'/0
                   , 'newline'/1
                   , 'open-input-bytevector'/1
                   , 'open-input-string'/1
                   , 'open-output-bytevector'/0
                   , 'open-output-string'/0
                   , 'output-port-open?'/1
                   , 'output-port?'/1
                   , 'peek-char'/0
                   , 'peek-char'/1
                   , 'peek-u8'/0
                   , 'peek-u8'/1
                   , 'port?'/1
                   , 'read-bytevector'/1
                   , 'read-bytevector!'/1
                   , 'read-bytevector!'/2
                   , 'read-bytevector!'/3
                   , 'read-bytevector!'/4
                   , 'read-bytevector'/2
                   , 'read-char'/0
                   , 'read-char'/1
                   , 'read-line'/0
                   , 'read-line'/1
                   , 'read-string'/1
                   , 'read-string'/2
                   , 'read-u8'/0
                   , 'read-u8'/1
                   , 'textual-port?'/1
                   , 'u8-ready?'/0
                   , 'u8-ready?'/1
                   , 'write-bytevector'/1
                   , 'write-bytevector'/2
                   , 'write-bytevector'/3
                   , 'write-bytevector'/4
                   , 'write-char'/1
                   , 'write-char'/2
                   , 'write-string'/1
                   , 'write-string'/2
                   , 'write-string'/3
                   , 'write-string'/4
                   , 'write-u8'/1
                   , 'write-u8'/2
                   ]}).

-import_as_export({scml_base_list,
                   ['append'/1
                   , 'assoc'/5
                   , 'assoc'/6
                   , 'assq'/5
                   , 'assv'/5
                   , 'caar'/1
                   , 'cadr'/1
                   , 'car'/1
                   , 'cdar'/1
                   , 'cddr'/1
                   , 'cdr'/1
                   , 'cons'/2
                   , 'length'/1
                   , 'list'/1
                   , 'list-copy'/1
                   , 'list-ref'/2
                   , 'list-set!'/3
                   , 'list-tail'/2
                   , 'list?'/1
                   , 'make-list'/1
                   , 'make-list'/2
                   , 'member'/5
                   , 'member'/6
                   , 'memq'/5
                   , 'memv'/5
                   , 'null?'/1
                   , 'pair?'/1
                   , 'reverse'/1
                   , 'set-car!'/2
                   , 'set-cdr!'/2
                   ]}).

-import_as_export({scml_base_number,
                   ['*'/1
                   , '+'/1
                   , '-'/1
                   , '/'/1
                   , '<'/1
                   , '<='/1
                   , '='/1
                   , '>'/1
                   , '>='/1
                   , 'abs'/1
                   , 'ceiling'/1
                   , 'complex?'/1
                   , 'denominator'/1
                   , 'even?'/1
                   , 'exact'/1
                   , 'exact-integer-sqrt'/1
                   , 'exact-integer?'/1
                   , 'exact?'/1
                   , 'expt'/2
                   , 'floor'/1
                   , 'floor-quotient'/2
                   , 'floor-remainder'/2
                   , 'floor/'/2
                   , 'gcd'/1
                   , 'inexact'/1
                   , 'inexact?'/1
                   , 'integer?'/1
                   , 'lcm'/1
                   , 'max'/1
                   , 'min'/1
                   , 'modulo'/2
                   , 'negative?'/1
                   , 'number->string'/1
                   , 'number->string'/2
                   , 'number?'/1
                   , 'numerator'/1
                   , 'odd?'/1
                   , 'positive?'/1
                   , 'quotient'/2
                   , 'rational?'/1
                   , 'rationalize'/2
                   , 'real?'/1
                   , 'remainder'/2
                   , 'round'/1
                   , 'square'/1
                   , 'string->number'/1
                   , 'string->number'/2
                   , 'truncate'/1
                   , 'truncate-quotient'/2
                   , 'truncate-remainder'/2
                   , 'truncate/'/2
                   , 'zero?'/1
                   ]}).

-import_as_export({scml_base_string,
                   ['make-string'/1
                   , 'make-string'/2
                   , 'list->string'/1
                   , 'string'/1
                   , 'string->list'/1
                   , 'string-append'/1
                   , 'string-copy'/1
                   , 'string-copy!'/3
                   , 'string-copy!'/4
                   , 'string-copy!'/5
                   , 'string-fill!'/2
                   , 'string-length'/1
                   , 'string-ref'/2
                   , 'string-set!'/3
                   , 'string<=?'/1
                   , 'string<?'/1
                   , 'string=?'/1
                   , 'string>=?'/1
                   , 'string>?'/1
                   , 'string?'/1
                   , 'substring'/3
                   ]}).

-import_as_export({scml_base_symbol,
                   ['string->symbol'/1
                   , 'symbol->string'/1
                   , 'symbol=?'/1
                   , 'symbol?'/1
                   ]}).

-import_as_export({scml_base_system,
                   ['features'/0
                   ]}).

-import_as_export({scml_base_vector,
                   ['list->vector'/1
                   , 'make-vector'/1
                   , 'make-vector'/2
                   , 'string->vector'/1
                   , 'string->vector'/2
                   , 'string->vector'/3
                   , 'vector'/1
                   , 'vector->list'/1
                   , 'vector->string'/1
                   , 'vector->string'/2
                   , 'vector->string'/3
                   , 'vector-append'/1
                   , 'vector-copy'/1
                   , 'vector-copy'/2
                   , 'vector-copy'/3
                   , 'vector-copy!'/3
                   , 'vector-copy!'/4
                   , 'vector-copy!'/5
                   , 'vector-fill!'/2
                   , 'vector-length'/1
                   , 'vector-ref'/2
                   , 'vector-set!'/3
                   , 'vector?'/1
                   ]}).

-include("scml.hrl").

%%%===================================================================
%%% Types/Specs/Records
%%%===================================================================

%%%===================================================================
%%% SCML Exports
%%%===================================================================

-spec '$scml_exports'() -> [{scm_symbol(), scmi_nip()}].
'$scml_exports'() ->
    [{'*', #nipv{val=fun ?MODULE:'*'/1}}
    , {'+', #nipv{val=fun ?MODULE:'+'/1}}
    , {'-', #nipv{val=fun ?MODULE:'-'/1}}
     %% macro - ...
    , {'/', #nipv{val=fun ?MODULE:'/'/1}}
    , {'<', #nipv{val=fun ?MODULE:'<'/1}}
    , {'<=', #nipv{val=fun ?MODULE:'<='/1}}
    , {'=', #nipv{val=fun ?MODULE:'='/1}}
     %% derived - =>
    , {'>', #nipv{val=fun ?MODULE:'>'/1}}
    , {'>=', #nipv{val=fun ?MODULE:'>='/1}}
    , {'abs', #nipn{val=fun ?MODULE:'abs'/1}}
     %% macro - _
     %% derived - and
    , {'append', #nipv{val=fun ?MODULE:'append'/1}}
    , {'apply', #xnipv{val=fun ?MODULE:'apply'/4}}
    , {'assoc', #xnipn{val=[fun ?MODULE:'assoc'/5, fun ?MODULE:'assoc'/6]}}
    , {'assq', #xnipn{val=fun ?MODULE:'assq'/5}}
    , {'assv', #xnipn{val=fun ?MODULE:'assv'/5}}
     %% derived - begin
    , {'binary-port?', #nipn{val=fun ?MODULE:'binary-port?'/1}}
    , {'boolean=?', #nipn{val=fun ?MODULE:'boolean=?'/1}}
    , {'boolean?', #nipn{val=fun ?MODULE:'boolean?'/1}}
    , {'bytevector', #nipn{val=fun ?MODULE:'bytevector'/1}}
    , {'bytevector-append', #nipv{val=fun ?MODULE:'bytevector-append'/1}}
    , {'bytevector-copy', #nipn{val=[fun ?MODULE:'bytevector-copy'/1, fun ?MODULE:'bytevector-copy'/2, fun ?MODULE:'bytevector-copy'/3]}}
    , {'bytevector-copy!', #nipn{val=[fun ?MODULE:'bytevector-copy!'/3, fun ?MODULE:'bytevector-copy!'/4, fun ?MODULE:'bytevector-copy!'/5]}}
    , {'bytevector-length', #nipn{val=fun ?MODULE:'bytevector-length'/1}}
    , {'bytevector-u8-ref', #nipn{val=fun ?MODULE:'bytevector-u8-ref'/2}}
    , {'bytevector-u8-set!', #nipn{val=fun ?MODULE:'bytevector-u8-set!'/3}}
    , {'bytevector?', #nipn{val=fun ?MODULE:'bytevector?'/1}}
    , {'caar', #nipn{val=fun ?MODULE:'caar'/1}}
    , {'cadr', #nipn{val=fun ?MODULE:'cadr'/1}}
    , {'call-with-current-continuation', #xnipn{val=fun ?MODULE:'call/cc'/4}}
    , {'call-with-port', #xnipn{val=fun ?MODULE:'call-with-port'/5}}
    , {'call-with-values', #xnipn{val=fun ?MODULE:'call-with-values'/5}}
    , {'call/cc', #xnipn{val=fun ?MODULE:'call/cc'/4}}
    , {'car', #nipn{val=fun ?MODULE:'car'/1}}
     %% derived - case
    , {'cdar', #nipn{val=fun ?MODULE:'cdar'/1}}
    , {'cddr', #nipn{val=fun ?MODULE:'cddr'/1}}
    , {'cdr', #nipn{val=fun ?MODULE:'cdr'/1}}
    , {'ceiling', #nipn{val=fun ?MODULE:'ceiling'/1}}
    , {'char->integer', #nipn{val=fun ?MODULE:'char->integer'/1}}
    , {'char-ready?', #nipn{val=[fun ?MODULE:'char-ready?'/0, fun ?MODULE:'char-ready?'/1]}}
    , {'char<=?', #nipv{val=fun ?MODULE:'char<=?'/1}}
    , {'char<?', #nipv{val=fun ?MODULE:'char<?'/1}}
    , {'char=?', #nipv{val=fun ?MODULE:'char=?'/1}}
    , {'char>=?', #nipv{val=fun ?MODULE:'char>=?'/1}}
    , {'char>?', #nipv{val=fun ?MODULE:'char>?'/1}}
    , {'char?', #nipv{val=fun ?MODULE:'char?'/1}}
    , {'close-input-port', #nipn{val=fun ?MODULE:'close-input-port'/1}}
    , {'close-output-port', #nipn{val=fun ?MODULE:'close-output-port'/1}}
    , {'close-port', #nipn{val=fun ?MODULE:'close-port'/1}}
    , {'complex?', #nipn{val=fun ?MODULE:'complex?'/1}}
     %% derived - cond
     %% derived - cond-expand
    , {'cons', #nipn{val=fun ?MODULE:'cons'/2}}
    , {'current-error-port', #nipn{val=[fun ?MODULE:'current-error-port'/0, fun ?MODULE:'current-error-port'/1, fun ?MODULE:'current-error-port'/2]}}
    , {'current-input-port', #nip0{val=fun ?MODULE:'current-input-port'/0}}
    , {'current-output-port', #nip0{val=fun ?MODULE:'current-output-port'/0}}
     %% program - define
     %% program define-record-type
     %% program - define-syntax
     %% program define-values
    , {'denominator', #nipn{val=fun ?MODULE:'denominator'/1}}
     %% derived - do
    , {'dynamic-wind', #xnipn{val=fun ?MODULE:'dynamic-wind'/6}}
     %% derived - else
    , {'eof-object', #nip0{val=fun ?MODULE:'eof-object'/0}}
    , {'eof-object?', #nipn{val=fun ?MODULE:'eof-object?'/1}}
    , {'eq?', #nipn{val=fun ?MODULE:'eq?'/2}}
    , {'equal?', #nipn{val=fun ?MODULE:'equal?'/2}}
    , {'eqv?', #nipn{val=fun ?MODULE:'eqv?'/2}}
    , {'error', #xnipv{val=fun ?MODULE:'error'/4}}
    , {'error-object-irritants', #nipn{val=fun ?MODULE:'error-object-irritants'/1}}
    , {'error-object-message', #nipn{val=fun ?MODULE:'error-object-message'/1}}
    , {'error-object?', #nipn{val=fun ?MODULE:'error-object?'/1}}
    , {'even?', #nipn{val=fun ?MODULE:'even?'/1}}
    , {'exact', #nipn{val=fun ?MODULE:'exact'/1}}
    , {'exact-integer-sqrt', #nipn{val=fun ?MODULE:'exact-integer-sqrt'/1}}
    , {'exact-integer?', #nipn{val=fun ?MODULE:'exact-integer?'/1}}
    , {'exact?', #nipn{val=fun ?MODULE:'exact?'/1}}
    , {'expt', #nipn{val=fun ?MODULE:'expt'/2}}
    , {'features', #nip0{val=fun ?MODULE:'features'/0}}
    , {'file-error?', #nipn{val=fun ?MODULE:'file-error?'/1}}
    , {'floor', #nipn{val=fun ?MODULE:'floor'/1}}
    , {'floor-quotient', #nipn{val=fun ?MODULE:'floor-quotient'/2}}
    , {'floor-remainder', #nipn{val=fun ?MODULE:'floor-remainder'/2}}
    , {'floor/', #nipn{val=fun ?MODULE:'floor/'/2}}
    , {'flush-output-port', #nipn{val=[fun ?MODULE:'flush-output-port'/0, fun ?MODULE:'flush-output-port'/1]}}
    , {'for-each', #xnipv{val=fun ?MODULE:'for-each'/4}}
    , {'gcd', #nipv{val=fun ?MODULE:'gcd'/1}}
    , {'get-output-bytevector', #nipn{val=fun ?MODULE:'get-output-bytevector'/1}}
    , {'get-output-string', #nipn{val=fun ?MODULE:'get-output-string'/1}}
     %% derived - guard
     %% primitive - if
     %% primitive - include
     %% primitive - include-ci
    , {'inexact', #nipn{val=fun ?MODULE:'inexact'/1}}
    , {'inexact?', #nipn{val=fun ?MODULE:'inexact?'/1}}
    , {'input-port-open?', #nipn{val=fun ?MODULE:'input-port-open?'/1}}
    , {'input-port?', #nipn{val=fun ?MODULE:'input-port?'/1}}
    , {'integer->char', #nipn{val=fun ?MODULE:'integer->char'/1}}
    , {'integer?', #nipn{val=fun ?MODULE:'integer?'/1}}
     %% primitive - lambda
    , {'lcm', #nipv{val=fun ?MODULE:'lcm'/1}}
    , {'length', #nipn{val=fun ?MODULE:'length'/1}}
     %% derived - let
     %% derived - let*
     %% derived - let*-values
     %% derived - let-syntax
     %% derived - let-values
     %% derived - letrec
     %% derived - letrec*
     %% macro - letrec-syntax
    , {'list', #nipn{val=fun ?MODULE:'list'/1}}
    , {'list->string', #nipn{val=fun ?MODULE:'list->string'/1}}
    , {'list->vector', #nipn{val=fun ?MODULE:'list->vector'/1}}
    , {'list-copy', #nipv{val=fun ?MODULE:'list-copy'/1}}
    , {'list-ref', #nipn{val=fun ?MODULE:'list-ref'/2}}
    , {'list-set!', #nipn{val=fun ?MODULE:'list-set!'/3}}

    , {'list-tail', #nipn{val=fun ?MODULE:'list-tail'/2}}
    , {'list?', #nipn{val=fun ?MODULE:'list?'/1}}
    , {'make-bytevector', #nipn{val=[fun ?MODULE:'make-bytevector'/1, fun ?MODULE:'make-bytevector'/2]}}
    , {'make-list', #nipn{val=[fun ?MODULE:'make-list'/1, fun ?MODULE:'make-list'/2]}}
     %% derived - make-parameter
    , {'make-string', #nipn{val=[fun ?MODULE:'make-string'/1, fun ?MODULE:'make-string'/2]}}
    , {'make-vector', #nipn{val=[fun ?MODULE:'make-vector'/1, fun ?MODULE:'make-vector'/2]}}
    , {'map', #xnipv{val=fun ?MODULE:'map'/4}}
    , {'max', #nipv{val=fun ?MODULE:'max'/1}}
    , {'member', #xnipn{val=[fun ?MODULE:'member'/5, fun ?MODULE:'member'/6]}}
    , {'memq', #xnipn{val=fun ?MODULE:'memq'/5}}
    , {'memv', #xnipn{val=fun ?MODULE:'memv'/5}}
    , {'min', #nipv{val=fun ?MODULE:'min'/1}}
    , {'modulo', #nipn{val=fun ?MODULE:'modulo'/2}}
    , {'negative?', #nipn{val=fun ?MODULE:'negative?'/1}}
    , {'newline', #nipn{val=[fun ?MODULE:'newline'/0, fun ?MODULE:'newline'/1]}}
    , {'not', #nipn{val=fun ?MODULE:'not'/1}}
    , {'null?', #nipn{val=fun ?MODULE:'null?'/1}}
    , {'number->string', #nipn{val=[fun ?MODULE:'number->string'/1, fun ?MODULE:'number->string'/2]}}
    , {'number?', #nipn{val=fun ?MODULE:'number?'/1}}
    , {'numerator', #nipn{val=fun ?MODULE:'numerator'/1}}
    , {'odd?', #nipn{val=fun ?MODULE:'odd?'/1}}
    , {'open-input-bytevector', #nipn{val=fun ?MODULE:'open-input-bytevector'/1}}
    , {'open-input-string', #nipn{val=fun ?MODULE:'open-input-string'/1}}
    , {'open-output-bytevector', #nip0{val=fun ?MODULE:'open-output-bytevector'/0}}
    , {'open-output-string', #nip0{val=fun ?MODULE:'open-output-string'/0}}
     %% primitive - or
    , {'output-port-open?', #nipn{val=fun ?MODULE:'output-port-open?'/1}}
    , {'output-port?', #nipn{val=fun ?MODULE:'output-port?'/1}}
    , {'pair?', #nipn{val=fun ?MODULE:'pair?'/1}}
     %% derived - parameterize
    , {'peek-char', #nipn{val=[fun ?MODULE:'peek-char'/0, fun ?MODULE:'peek-char'/1]}}
    , {'peek-u8', #nipn{val=[fun ?MODULE:'peek-u8'/0, fun ?MODULE:'peek-u8'/1]}}
    , {'port?', #nipn{val=fun ?MODULE:'port?'/1}}
    , {'positive?', #nipn{val=fun ?MODULE:'positive?'/1}}
    , {'procedure?', #nipn{val=fun ?MODULE:'procedure?'/1}}
     %% derived - quasiquote
     %% primitive - quote
    , {'quotient', #nipn{val=fun ?MODULE:'quotient'/2}}
    , {'raise', #xnipn{val=fun ?MODULE:'raise'/4}}
    , {'raise-continuable', #xnipn{val=fun ?MODULE:'raise-continuable'/4}}
    , {'rational?', #nipn{val=fun ?MODULE:'rational?'/1}}
    , {'rationalize', #nipn{val=fun ?MODULE:'rationalize'/2}}
    , {'read-bytevector', #nipn{val=[fun ?MODULE:'read-bytevector'/1, fun ?MODULE:'read-bytevector'/2]}}
    , {'read-bytevector!', #nipn{val=[fun ?MODULE:'read-bytevector!'/1, fun ?MODULE:'read-bytevector!'/2, fun ?MODULE:'read-bytevector!'/3, fun ?MODULE:'read-bytevector!'/4]}}
    , {'read-char', #nipn{val=[fun ?MODULE:'read-char'/0, fun ?MODULE:'read-char'/1]}}
    , {'read-error?', #nipn{val=fun ?MODULE:'read-error?'/1}}
    , {'read-line', #nipn{val=[fun ?MODULE:'read-line'/0, fun ?MODULE:'read-line'/1]}}
    , {'read-string', #nipn{val=[fun ?MODULE:'read-string'/1, fun ?MODULE:'read-string'/2]}}
    , {'read-u8', #nipn{val=[fun ?MODULE:'read-u8'/0, fun ?MODULE:'read-u8'/1]}}
    , {'real?', #nipn{val=fun ?MODULE:'real?'/1}}
    , {'remainder', #nipn{val=fun ?MODULE:'remainder'/2}}
    , {'reverse', #nipn{val=fun ?MODULE:'reverse'/1}}
    , {'round', #nipn{val=fun ?MODULE:'round'/1}}
     %% primitive - set!
    , {'set-car!', #nipn{val=fun ?MODULE:'set-car!'/2}}
    , {'set-cdr!', #nipn{val=fun ?MODULE:'set-cdr!'/2}}
    , {'square', #nipn{val=fun ?MODULE:'square'/1}}
    , {'string', #nipv{val=fun ?MODULE:'string'/1}}
    , {'string->list', #nipn{val=fun ?MODULE:'string->list'/1}}
    , {'string->number', #nipn{val=[fun ?MODULE:'string->number'/1, fun ?MODULE:'string->number'/2]}}
    , {'string->symbol', #nipn{val=fun ?MODULE:'string->symbol'/1}}
    , {'string->utf8', #nipn{val=[fun ?MODULE:'string->utf8'/1, fun ?MODULE:'string->utf8'/2, fun ?MODULE:'string->utf8'/3]}}
    , {'string->vector', #nipn{val=[fun ?MODULE:'string->vector'/1, fun ?MODULE:'string->vector'/2, fun ?MODULE:'string->vector'/3]}}
    , {'string-append', #nipv{val=fun ?MODULE:'string-append'/1}}
    , {'string-copy', #nipn{val=fun ?MODULE:'string-copy'/1}}
    , {'string-copy!', #nipn{val=[fun ?MODULE:'string-copy!'/3, fun ?MODULE:'string-copy!'/4, fun ?MODULE:'string-copy!'/5]}}
    , {'string-fill!', #nipn{val=fun ?MODULE:'string-fill!'/2}}
    , {'string-for-each', #xnipv{val=fun ?MODULE:'string-for-each'/4}}
    , {'string-length', #nipn{val=fun ?MODULE:'string-length'/1}}
    , {'string-map', #xnipv{val=fun ?MODULE:'string-map'/4}}
    , {'string-ref', #nipn{val=fun ?MODULE:'string-ref'/2}}
    , {'string-set!', #nipn{val=fun ?MODULE:'string-set!'/3}}
    , {'string<=?', #nipv{val=fun ?MODULE:'string<=?'/1}}
    , {'string<?', #nipv{val=fun ?MODULE:'string<?'/1}}
    , {'string=?', #nipv{val=fun ?MODULE:'string=?'/1}}
    , {'string>=?', #nipv{val=fun ?MODULE:'string>=?'/1}}
    , {'string>?', #nipv{val=fun ?MODULE:'string>?'/1}}
    , {'string?', #nipn{val=fun ?MODULE:'string?'/1}}
    , {'substring', #nipn{val=fun ?MODULE:'substring'/3}}
    , {'symbol->string', #nipn{val=fun ?MODULE:'symbol->string'/1}}
    , {'symbol=?', #nipv{val=fun ?MODULE:'symbol=?'/1}}
    , {'symbol?', #nipn{val=fun ?MODULE:'symbol?'/1}}
     %% macro - syntax-error
     %% macro - syntax-rules
    , {'textual-port?', #nipn{val=fun ?MODULE:'textual-port?'/1}}
    , {'truncate', #nipn{val=fun ?MODULE:'truncate'/1}}
    , {'truncate-quotient', #nipn{val=fun ?MODULE:'truncate-quotient'/2}}
    , {'truncate-remainder', #nipn{val=fun ?MODULE:'truncate-remainder'/2}}
    , {'truncate/', #nipn{val=fun ?MODULE:'truncate/'/2}}
    , {'u8-ready?', #nipn{val=[fun ?MODULE:'u8-ready?'/0, fun ?MODULE:'u8-ready?'/1]}}
     %% derived - unless
     %% derived - unquote
     %% derived - unquote-splicing
    , {'utf8->string', #nipn{val=[fun ?MODULE:'utf8->string'/1, fun ?MODULE:'utf8->string'/2, fun ?MODULE:'utf8->string'/3]}}
    , {'values', #xnipv{val=fun ?MODULE:'values'/4}}
    , {'vector', #nipv{val=fun ?MODULE:'vector'/1}}
    , {'vector->list', #nipn{val=fun ?MODULE:'vector->list'/1}}
    , {'vector->string', #nipn{val=[fun ?MODULE:'vector->string'/1, fun ?MODULE:'vector->string'/2, fun ?MODULE:'vector->string'/3]}}
    , {'vector-append', #nipv{val=fun ?MODULE:'vector-append'/1}}
    , {'vector-copy', #nipn{val=[fun ?MODULE:'vector-copy'/1, fun ?MODULE:'vector-copy'/2, fun ?MODULE:'vector-copy'/3]}}
    , {'vector-copy!', #nipn{val=[fun ?MODULE:'vector-copy!'/3, fun ?MODULE:'vector-copy!'/4, fun ?MODULE:'vector-copy!'/5]}}
    , {'vector-fill!', #nipn{val=fun ?MODULE:'vector-fill!'/2}}
    , {'vector-for-each', #xnipv{val=fun ?MODULE:'vector-for-each'/4}}
    , {'vector-length', #nipn{val=fun ?MODULE:'vector-length'/1}}
    , {'vector-map', #xnipv{val=fun ?MODULE:'vector-map'/4}}
    , {'vector-ref', #nipn{val=fun ?MODULE:'vector-ref'/2}}
    , {'vector-set!', #nipn{val=fun ?MODULE:'vector-set!'/3}}
    , {'vector?', #nipn{val=fun ?MODULE:'vector?'/1}}
     %% derived - when
    , {'with-exception-handler', #xnipn{val=fun ?MODULE:'with-exception-handler'/5}}
    , {'write-bytevector', #nipn{val=[fun ?MODULE:'write-bytevector'/1, fun ?MODULE:'write-bytevector'/2, fun ?MODULE:'write-bytevector'/3, fun ?MODULE:'write-bytevector'/4]}}
    , {'write-char', #nipn{val=[fun ?MODULE:'write-char'/1, fun ?MODULE:'write-char'/2]}}
    , {'write-string', #nipn{val=[fun ?MODULE:'write-string'/1, fun ?MODULE:'write-string'/2, fun ?MODULE:'write-string'/3, fun ?MODULE:'write-string'/4]}}
    , {'write-u8', #nipn{val=[fun ?MODULE:'write-u8'/1, fun ?MODULE:'write-u8'/2]}}
    , {'zero?', #nipn{val=fun ?MODULE:'zero?'/1}}
    ].

%%%===================================================================
%%% API
%%%===================================================================


%%%===================================================================
%%% internal helpers
%%%===================================================================
