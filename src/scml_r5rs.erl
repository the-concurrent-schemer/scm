%%% The MIT License
%%%
%%% Copyright (C) 2013-2014 by Joseph Wayne Norton <norton@alum.mit.edu>
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

%%% @doc Scheme r5rs library
%%% @author Joseph Wayne Norton <norton@alum.mit.edu>

-module(scml_r5rs).

%% SCML Exports
-export(['$scml_exports'/0]).

%% API
-export(['call-with-current-continuation'/4
         , 'exact->inexact'/1
         , 'inexact->exact'/1
         , 'null-environment'/1
         , 'scheme-report-environment'/1
         , 'transcript-on'/1
         , 'transcript-off'/0
        ]).

%% Proxy API
-compile({parse_transform,xfm_import_as_export}).

-import_as_export({scml_base_boolean,
                   ['boolean?'/1
                    , 'not'/1
                   ]}).

-import_as_export({scml_base_char,
                   ['char?'/1
                    , 'char=?'/1
                    , 'char<?'/1
                    , 'char>?'/1
                    , 'char<=?'/1
                    , 'char>=?'/1
                    , 'char->integer'/1
                    , 'integer->char'/1
                   ]}).

-import_as_export({scml_base_control,
                   ['procedure?'/1
                    , 'apply'/4
                    , 'map'/4
                    , 'for-each'/4
                    , 'values'/4
                    , 'call-with-values'/5
                    , 'dynamic-wind'/6
                   ]}).

-import_as_export({scml_base_equality,
                   ['eqv?'/2
                    , 'eq?'/2
                    , 'equal?'/2
                   ]}).

-import_as_export({scml_base_io,
                   ['current-input-port'/0
                    , 'current-output-port'/0
                    , 'close-input-port'/1
                    , 'close-output-port'/1
                    , 'eof-object?'/1
                    , 'char-ready?'/0
                    , 'char-ready?'/1
                    , 'input-port?'/1
                    , 'newline'/0
                    , 'newline'/1
                    , 'output-port?'/1
                    , 'peek-char'/0
                    , 'peek-char'/1
                    , 'read-char'/0
                    , 'read-char'/1
                    , 'write-char'/1
                    , 'write-char'/2
                   ]}).

-import_as_export({scml_base_list,
                   ['pair?'/1
                    , 'cons'/2
                    , 'car'/1
                    , 'cdr'/1
                    , 'set-car!'/2
                    , 'set-cdr!'/2
                    , 'caar'/1
                    , 'cadr'/1
                    , 'cdar'/1
                    , 'cddr'/1
                    , 'null?'/1
                    , 'list?'/1
                    , 'list'/1
                    , 'length'/1
                    , 'append'/1
                    , 'reverse'/1
                    , 'list-tail'/2
                    , 'list-ref'/2
                    , 'memq'/5
                    , 'memv'/5
                    , 'member'/5
                    , 'member'/6
                    , 'assq'/5
                    , 'assv'/5
                    , 'assoc'/5
                    , 'assoc'/6
                   ]}).

-import_as_export({scml_base_number,
                   ['number?'/1
                    , 'complex?'/1
                    , 'real?'/1
                    , 'rational?'/1
                    , 'integer?'/1
                    , 'exact?'/1
                    , 'inexact?'/1
                    , '='/1
                    , '<'/1
                    , '>'/1
                    , '<='/1
                    , '>='/1
                    , 'zero?'/1
                    , 'positive?'/1
                    , 'negative?'/1
                    , 'odd?'/1
                    , 'even?'/1
                    , 'max'/1
                    , 'min'/1
                    , '+'/1
                    , '*'/1
                    , '-'/1
                    , '/'/1
                    , 'abs'/1
                    , 'quotient'/2
                    , 'remainder'/2
                    , 'modulo'/2
                    , 'gcd'/1
                    , 'lcm'/1
                    , 'numerator'/1
                    , 'denominator'/1
                    , 'floor'/1
                    , 'ceiling'/1
                    , 'truncate'/1
                    , 'round'/1
                    , 'rationalize'/2
                    , 'expt'/2
                    , 'number->string'/1
                    , 'number->string'/2
                    , 'string->number'/1
                    , 'string->number'/2
                   ]}).

-import_as_export({scml_base_string,
                   ['string?'/1
                    , 'make-string'/1
                    , 'make-string'/2
                    , 'string'/1
                    , 'string-length'/1
                    , 'string-ref'/2
                    , 'string-set!'/3
                    , 'string=?'/1
                    , 'string<?'/1
                    , 'string>?'/1
                    , 'string<=?'/1
                    , 'string>=?'/1
                    , 'substring'/3
                    , 'string-append'/1
                    , 'string->list'/1
                    , 'list->string'/1
                    , 'string-copy'/1
                    , 'string-fill!'/2
                   ]}).

-import_as_export({scml_base_symbol,
                   ['symbol?'/1
                    , 'symbol->string'/1
                    , 'string->symbol'/1
                   ]}).

-import_as_export({scml_base_vector,
                   ['vector?'/1
                    , 'make-vector'/1
                    , 'make-vector'/2
                    , 'vector'/1
                    , 'vector-length'/1
                    , 'vector-ref'/2
                    , 'vector-set!'/3
                    , 'vector->list'/1
                    , 'list->vector'/1
                    , 'vector-fill!'/2
                   ]}).

-import_as_export({scml_char,
                   ['char-ci=?'/1
                    , 'char-ci<?'/1
                    , 'char-ci>?'/1
                    , 'char-ci<=?'/1
                    , 'char-ci>=?'/1
                    , 'char-alphabetic?'/1
                    , 'char-numeric?'/1
                    , 'char-whitespace?'/1
                    , 'char-upper-case?'/1
                    , 'char-lower-case?'/1
                    , 'char-upcase'/1
                    , 'char-downcase'/1
                    , 'string-ci=?'/1
                    , 'string-ci<?'/1
                    , 'string-ci>?'/1
                    , 'string-ci<=?'/1
                    , 'string-ci>=?'/1
                   ]}).

-import_as_export({scml_complex,
                   ['make-rectangular'/2
                    , 'make-polar'/2
                    , 'real-part'/1
                    , 'imag-part'/1
                    , 'magnitude'/1
                    , 'angle'/1
                   ]}).

-import_as_export({scml_cxr,
                   ['caaar'/1
                    , 'caadr'/1
                    , 'cadar'/1
                    , 'caddr'/1
                    , 'cdaar'/1
                    , 'cdadr'/1
                    , 'cddar'/1
                    , 'cdddr'/1
                    , 'caaaar'/1
                    , 'caaadr'/1
                    , 'caadar'/1
                    , 'caaddr'/1
                    , 'cadaar'/1
                    , 'cadadr'/1
                    , 'caddar'/1
                    , 'cadddr'/1
                    , 'cdaaar'/1
                    , 'cdaadr'/1
                    , 'cdadar'/1
                    , 'cdaddr'/1
                    , 'cddaar'/1
                    , 'cddadr'/1
                    , 'cdddar'/1
                    , 'cddddr'/1
                   ]}).

-import_as_export({scml_eval,
                   ['eval'/2
                   ]}).


-import_as_export({scml_file,
                   ['call-with-input-file'/2
                    , 'call-with-output-file'/2
                    , 'with-input-from-file'/2
                    , 'with-output-to-file'/2
                    , 'open-input-file'/1
                    , 'open-output-file'/1
                   ]}).

-import_as_export({scml_inexact,
                   ['exp'/1
                    , 'log'/1, 'log'/2
                    , 'sin'/1
                    , 'cos'/1
                    , 'tan'/1
                    , 'asin'/1
                    , 'acos'/1
                    , 'atan'/1, 'atan'/2
                    , 'sqrt'/1
                   ]}).

-import_as_export({scml_lazy,
                   ['delay'/1
                    , 'force'/1
                   ]}).

-import_as_export({scml_load,
                   ['load'/1
                   ]}).

-import_as_export({scml_read,
                   ['read'/0
                    , 'read'/1
                   ]}).

-import_as_export({scml_repl,
                   ['interaction-environment'/0
                   ]}).

-import_as_export({scml_write,
                   ['display'/1
                    , 'display'/2
                    , 'write'/1
                    , 'write'/2
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
     , {'/', #nipv{val=fun ?MODULE:'/'/1}}
     , {'<', #nipv{val=fun ?MODULE:'<'/1}}
     , {'<=', #nipv{val=fun ?MODULE:'<='/1}}
     , {'=', #nipv{val=fun ?MODULE:'='/1}}
     , {'>', #nipv{val=fun ?MODULE:'>'/1}}
     , {'>=', #nipv{val=fun ?MODULE:'>='/1}}
     , {'abs', #nipn{val=fun ?MODULE:'abs'/1}}
     , {'acos', #nipn{val=fun ?MODULE:'acos'/1}}
     %% derived - and
     , {'angle', #nipn{val=fun ?MODULE:'angle'/1}}
     , {'append', #nipv{val=fun ?MODULE:'append'/1}}
     , {'apply', #xnipv{val=fun ?MODULE:'apply'/4}}
     , {'asin', #nipn{val=fun ?MODULE:'asin'/1}}
     , {'assoc', #xnipn{val=[fun ?MODULE:'assoc'/5, fun ?MODULE:'assoc'/6]}}
     , {'assq', #xnipn{val=fun ?MODULE:'assq'/5}}
     , {'assv', #xnipn{val=fun ?MODULE:'assv'/5}}
     , {'atan', #nipn{val=[fun ?MODULE:'atan'/1, fun ?MODULE:'atan'/2]}}
     %% derived - begin
     , {'boolean?', #nipn{val=fun ?MODULE:'boolean?'/1}}
     , {'caaaar', #nipn{val=fun ?MODULE:'caaaar'/1}}
     , {'caaadr', #nipn{val=fun ?MODULE:'caaadr'/1}}
     , {'caaar', #nipn{val=fun ?MODULE:'caaar'/1}}
     , {'caadar', #nipn{val=fun ?MODULE:'caadar'/1}}
     , {'caaddr', #nipn{val=fun ?MODULE:'caaddr'/1}}
     , {'caadr', #nipn{val=fun ?MODULE:'caadr'/1}}
     , {'caar', #nipn{val=fun ?MODULE:'caar'/1}}
     , {'cadaar', #nipn{val=fun ?MODULE:'cadaar'/1}}
     , {'cadadr', #nipn{val=fun ?MODULE:'cadadr'/1}}
     , {'cadar', #nipn{val=fun ?MODULE:'cadar'/1}}
     , {'caddar', #nipn{val=fun ?MODULE:'caddar'/1}}
     , {'cadddr', #nipn{val=fun ?MODULE:'cadddr'/1}}
     , {'caddr', #nipn{val=fun ?MODULE:'caddr'/1}}
     , {'cadr', #nipn{val=fun ?MODULE:'cadr'/1}}
     , {'call-with-current-continuation', #xnipn{val=fun ?MODULE:'call-with-current-continuation'/4}}
     , {'call-with-input-file', #nipn{val=fun ?MODULE:'call-with-input-file'/2}}
     , {'call-with-output-file', #nipn{val=fun ?MODULE:'call-with-output-file'/2}}
     , {'call-with-values', #xnipn{val=fun ?MODULE:'call-with-values'/5}}
     , {'car', #nipn{val=fun ?MODULE:'car'/1}}
     %% derived - case
     , {'cdaaar', #nipn{val=fun ?MODULE:'cdaaar'/1}}
     , {'cdaadr', #nipn{val=fun ?MODULE:'cdaadr'/1}}
     , {'cdaar', #nipn{val=fun ?MODULE:'cdaar'/1}}
     , {'cdadar', #nipn{val=fun ?MODULE:'cdadar'/1}}
     , {'cdaddr', #nipn{val=fun ?MODULE:'cdaddr'/1}}
     , {'cdadr', #nipn{val=fun ?MODULE:'cdadr'/1}}
     , {'cdar', #nipn{val=fun ?MODULE:'cdar'/1}}
     , {'cddaar', #nipn{val=fun ?MODULE:'cddaar'/1}}
     , {'cddadr', #nipn{val=fun ?MODULE:'cddadr'/1}}
     , {'cddar', #nipn{val=fun ?MODULE:'cddar'/1}}
     , {'cdddar', #nipn{val=fun ?MODULE:'cdddar'/1}}
     , {'cddddr', #nipn{val=fun ?MODULE:'cddddr'/1}}
     , {'cdddr', #nipn{val=fun ?MODULE:'cdddr'/1}}
     , {'cddr', #nipn{val=fun ?MODULE:'cddr'/1}}
     , {'cdr', #nipn{val=fun ?MODULE:'cdr'/1}}
     , {'ceiling', #nipn{val=fun ?MODULE:'ceiling'/1}}
     , {'char->integer', #nipn{val=fun ?MODULE:'char->integer'/1}}
     , {'char-alphabetic?', #nipn{val=fun ?MODULE:'char-alphabetic?'/1}}
     , {'char-ci<=?', #nipv{val=fun ?MODULE:'char-ci<=?'/1}}
     , {'char-ci<?', #nipv{val=fun ?MODULE:'char-ci<?'/1}}
     , {'char-ci=?', #nipv{val=fun ?MODULE:'char-ci=?'/1}}
     , {'char-ci>=?', #nipv{val=fun ?MODULE:'char-ci>=?'/1}}
     , {'char-ci>?', #nipv{val=fun ?MODULE:'char-ci>?'/1}}
     , {'char-downcase', #nipn{val=fun ?MODULE:'char-downcase'/1}}
     , {'char-lower-case?', #nipn{val=fun ?MODULE:'char-lower-case?'/1}}
     , {'char-numeric?', #nipn{val=fun ?MODULE:'char-numeric?'/1}}
     , {'char-ready?', #nipn{val=[fun ?MODULE:'char-ready?'/0, fun ?MODULE:'char-ready?'/1]}}
     , {'char-upcase', #nipn{val=fun ?MODULE:'char-upcase'/1}}
     , {'char-upper-case?', #nipn{val=fun ?MODULE:'char-upper-case?'/1}}
     , {'char-whitespace?', #nipn{val=fun ?MODULE:'char-whitespace?'/1}}
     , {'char<=?', #nipv{val=fun ?MODULE:'char<=?'/1}}
     , {'char<?', #nipv{val=fun ?MODULE:'char<?'/1}}
     , {'char=?', #nipv{val=fun ?MODULE:'char=?'/1}}
     , {'char>=?', #nipv{val=fun ?MODULE:'char>=?'/1}}
     , {'char>?', #nipv{val=fun ?MODULE:'char>?'/1}}
     , {'char?', #nipv{val=fun ?MODULE:'char?'/1}}
     , {'close-input-port', #nipn{val=fun ?MODULE:'close-input-port'/1}}
     , {'close-output-port', #nipn{val=fun ?MODULE:'close-output-port'/1}}
     , {'complex?', #nipn{val=fun ?MODULE:'complex?'/1}}
     %% derived - cond
     , {'cons', #nipn{val=fun ?MODULE:'cons'/2}}
     , {'cos', #nipn{val=fun ?MODULE:'cos'/1}}
     , {'current-input-port', #nip0{val=fun ?MODULE:'current-input-port'/0}}
     , {'current-output-port', #nip0{val=fun ?MODULE:'current-output-port'/0}}
     %% program - define
     %% program - define-syntax
     , {'delay', #nipv{val=fun ?MODULE:'delay'/1}}
     , {'denominator', #nipn{val=fun ?MODULE:'denominator'/1}}
     , {'display', #nipn{val=[fun ?MODULE:'display'/1, fun ?MODULE:'display'/2]}}
     %% derived - do
     , {'dynamic-wind', #xnipn{val=fun ?MODULE:'dynamic-wind'/6}}
     , {'eof-object?', #nipn{val=fun ?MODULE:'eof-object?'/1}}
     , {'eq?', #nipn{val=fun ?MODULE:'eq?'/2}}
     , {'equal?', #nipn{val=fun ?MODULE:'equal?'/2}}
     , {'eqv?', #nipn{val=fun ?MODULE:'eqv?'/2}}
     , {'eval', #nipn{val=fun ?MODULE:'eval'/2}}
     , {'even?', #nipn{val=fun ?MODULE:'even?'/1}}
     , {'exact->inexact', #nipn{val=fun ?MODULE:'exact->inexact'/1}}
     , {'exact?', #nipn{val=fun ?MODULE:'exact?'/1}}
     , {'exp', #nipn{val=fun ?MODULE:'exp'/1}}
     , {'expt', #nipn{val=fun ?MODULE:'expt'/2}}
     , {'floor', #nipn{val=fun ?MODULE:'floor'/1}}
     , {'for-each', #xnipv{val=fun ?MODULE:'for-each'/4}}
     , {'force', #nipv{val=fun ?MODULE:'force'/1}}
     , {'gcd', #nipv{val=fun ?MODULE:'gcd'/1}}
     %% primitive - if
     , {'imag-part', #nipn{val=fun ?MODULE:'imag-part'/1}}
     , {'inexact->exact', #nipn{val=fun ?MODULE:'inexact->exact'/1}}
     , {'inexact?', #nipn{val=fun ?MODULE:'inexact?'/1}}
     , {'input-port?', #nipn{val=fun ?MODULE:'input-port?'/1}}
     , {'integer->char', #nipn{val=fun ?MODULE:'integer->char'/1}}
     , {'integer?', #nipn{val=fun ?MODULE:'integer?'/1}}
     , {'interaction-environment', #nip0{val=fun ?MODULE:'interaction-environment'/0}}
     %% primitive - lambda
     , {'lcm', #nipv{val=fun ?MODULE:'lcm'/1}}
     , {'length', #nipn{val=fun ?MODULE:'length'/1}}
     %% derived - let
     %% derived - let*
     %% derived - let-syntax
     %% derived - letrec
     %% derived - letrec-syntax
     , {'list', #nipn{val=fun ?MODULE:'list'/1}}
     , {'list->string', #nipn{val=fun ?MODULE:'list->string'/1}}
     , {'list->vector', #nipn{val=fun ?MODULE:'list->vector'/1}}
     , {'list-ref', #nipn{val=fun ?MODULE:'list-ref'/2}}
     , {'list-tail', #nipn{val=fun ?MODULE:'list-tail'/2}}
     , {'list?', #nipn{val=fun ?MODULE:'list?'/1}}
     , {'load', #nipn{val=fun ?MODULE:'load'/1}}
     , {'log', #nipn{val=[fun ?MODULE:'log'/1, fun ?MODULE:'log'/2]}}
     , {'magnitude', #nipn{val=fun ?MODULE:'magnitude'/1}}
     , {'make-polar', #nipn{val=fun ?MODULE:'make-polar'/2}}
     , {'make-rectangular', #nipn{val=fun ?MODULE:'make-rectangular'/2}}
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
     , {'null-environment', #nipn{val=fun ?MODULE:'null-environment'/1}}
     , {'null?', #nipn{val=fun ?MODULE:'null?'/1}}
     , {'number->string', #nipn{val=[fun ?MODULE:'number->string'/1, fun ?MODULE:'number->string'/2]}}
     , {'number?', #nipn{val=fun ?MODULE:'number?'/1}}
     , {'numerator', #nipn{val=fun ?MODULE:'numerator'/1}}
     , {'odd?', #nipn{val=fun ?MODULE:'odd?'/1}}
     , {'open-input-file', #nipn{val=fun ?MODULE:'open-input-file'/1}}
     , {'open-output-file', #nipn{val=fun ?MODULE:'open-output-file'/1}}
     %% primitive - or
     , {'output-port?', #nipn{val=fun ?MODULE:'output-port?'/1}}
     , {'pair?', #nipn{val=fun ?MODULE:'pair?'/1}}
     , {'peek-char', #nipn{val=[fun ?MODULE:'peek-char'/0, fun ?MODULE:'peek-char'/1]}}
     , {'positive?', #nipn{val=fun ?MODULE:'positive?'/1}}
     , {'procedure?', #nipn{val=fun ?MODULE:'procedure?'/1}}
     %% derived - quasiquote
     %% primitive - quote
     , {'quotient', #nipn{val=fun ?MODULE:'quotient'/2}}
     , {'rational?', #nipn{val=fun ?MODULE:'rational?'/1}}
     , {'rationalize', #nipn{val=fun ?MODULE:'rationalize'/2}}
     , {'read', #nipn{val=[fun ?MODULE:'read'/0, fun ?MODULE:'read'/1]}}
     , {'read-char', #nipn{val=[fun ?MODULE:'read-char'/0, fun ?MODULE:'read-char'/1]}}
     , {'real-part', #nipn{val=fun ?MODULE:'real-part'/1}}
     , {'real?', #nipn{val=fun ?MODULE:'real?'/1}}
     , {'remainder', #nipn{val=fun ?MODULE:'remainder'/2}}
     , {'reverse', #nipn{val=fun ?MODULE:'reverse'/1}}
     , {'round', #nipn{val=fun ?MODULE:'round'/1}}
     , {'scheme-report-environment', #nipn{val=fun ?MODULE:'scheme-report-environment'/1}}
     %% primitive - set!
     , {'set-car!', #nipn{val=fun ?MODULE:'set-car!'/2}}
     , {'set-cdr!', #nipn{val=fun ?MODULE:'set-cdr!'/2}}
     , {'sin', #nipn{val=fun ?MODULE:'sin'/1}}
     , {'sqrt', #nipn{val=fun ?MODULE:'sqrt'/1}}
     , {'string', #nipv{val=fun ?MODULE:'string'/1}}
     , {'string->list', #nipn{val=fun ?MODULE:'string->list'/1}}
     , {'string->number', #nipn{val=[fun ?MODULE:'string->number'/1, fun ?MODULE:'string->number'/2]}}
     , {'string->symbol', #nipn{val=fun ?MODULE:'string->symbol'/1}}
     , {'string-append', #nipv{val=fun ?MODULE:'string-append'/1}}
     , {'string-ci<=?', #nipv{val=fun ?MODULE:'string-ci<=?'/1}}
     , {'string-ci<?', #nipv{val=fun ?MODULE:'string-ci<?'/1}}
     , {'string-ci=?', #nipv{val=fun ?MODULE:'string-ci=?'/1}}
     , {'string-ci>=?', #nipv{val=fun ?MODULE:'string-ci>=?'/1}}
     , {'string-ci>?', #nipv{val=fun ?MODULE:'string-ci>?'/1}}
     , {'string-copy', #nipn{val=fun ?MODULE:'string-copy'/1}}
     , {'string-fill!', #nipn{val=fun ?MODULE:'string-fill!'/2}}
     , {'string-length', #nipn{val=fun ?MODULE:'string-length'/1}}
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
     , {'symbol?', #nipn{val=fun ?MODULE:'symbol?'/1}}
     , {'tan', #nipn{val=fun ?MODULE:'tan'/1}}
     , {'transcript-on', #nipn{val=fun ?MODULE:'transcript-on'/1}}
     , {'transcript-off', #nip0{val=fun ?MODULE:'transcript-off'/0}}
     , {'truncate', #nipn{val=fun ?MODULE:'truncate'/1}}
     , {'values', #xnipv{val=fun ?MODULE:'values'/4}}
     , {'vector', #nipv{val=fun ?MODULE:'vector'/1}}
     , {'vector->list', #nipn{val=fun ?MODULE:'vector->list'/1}}
     , {'vector-fill!', #nipn{val=fun ?MODULE:'vector-fill!'/2}}
     , {'vector-length', #nipn{val=fun ?MODULE:'vector-length'/1}}
     , {'vector-ref', #nipn{val=fun ?MODULE:'vector-ref'/2}}
     , {'vector-set!', #nipn{val=fun ?MODULE:'vector-set!'/3}}
     , {'vector?', #nipn{val=fun ?MODULE:'vector?'/1}}
     , {'with-input-from-file', #nipn{val=fun ?MODULE:'with-input-from-file'/2}}
     , {'with-output-to-file', #nipn{val=fun ?MODULE:'with-output-to-file'/2}}
     , {'write', #nipn{val=[fun ?MODULE:'write'/1, fun ?MODULE:'write'/2]}}
     , {'write-char', #nipn{val=[fun ?MODULE:'write-char'/1, fun ?MODULE:'write-char'/2]}}
     , {'zero?', #nipn{val=fun ?MODULE:'zero?'/1}}
    ].

%%%===================================================================
%%% API
%%%===================================================================

-spec 'call-with-current-continuation'(scm_proc(), scmi_denv(), scmi_dok(), scmi_dng()) -> scm_any().
'call-with-current-continuation'(Proc, Env, Ok, Ng) ->
    scml_base_control:'call/cc'(Proc, Env, Ok, Ng).

-spec 'exact->inexact'(scm_z()) -> scm_z().
'exact->inexact'(Z) ->
    scml_base_number:'inexact'(Z).

-spec 'inexact->exact'(scm_z()) -> scm_z().
'inexact->exact'(Z) ->
    scml_base_number:'exact'(Z).

-spec 'null-environment'(scm_k()) -> scmi_denv().
'null-environment'(K) ->
    erlang:error({roadmap,'v0.4.0'}, [K]).

-spec 'scheme-report-environment'(scm_k()) -> scmi_denv().
'scheme-report-environment'(K) ->
    erlang:error({roadmap,'v0.4.0'}, [K]).

-spec 'transcript-on'(scm_string()) -> scm_false().
'transcript-on'(S) ->
    erlang:error(unsupported, [S]).

-spec 'transcript-off'() -> scm_false().
'transcript-off'() ->
    erlang:error(unsupported, []).

%%%===================================================================
%%% internal helpers
%%%===================================================================
