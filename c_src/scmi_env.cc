// The MIT License
//
// Copyright (C) 2013 by Joseph Wayne Norton <norton@alum.mit.edu>
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.

//
// TODO:
//  - add map from variable to value
//  - add stats for set-variable-value
//  - implement gc for the vals ErlNifEnv
//  - implement support for undo set! operations

#include "scmi_env.h"
#include "scmi_env_lib.h"

#include <stdio.h>
#include <utility>
#include <vector>

static inline ERL_NIF_TERM
make_badarg(ErlNifEnv* env, unsigned line=0) {
    if (line) {
        fprintf(stderr, "MAKEBADARG %s:%d\n", __FILE__, line);
    }
    return enif_make_badarg(env);
}

#if 0
#define MAKEBADARG(env) make_badarg(env, __LINE__)
#else
#define MAKEBADARG(env) make_badarg(env)
#endif

// NifHandle
typedef std::vector<std::pair<ErlNifPid, ERL_NIF_TERM> > ErlNifPidVec;
typedef std::vector<ERL_NIF_TERM> ErlNifVec;

typedef struct handle
{
    ErlNifPidVec* notifiees;
    struct handle* base;
    bool immutable;
    ErlNifEnv* env;
    ErlNifEnv* vals;
    ErlNifVec* ivar;
    ErlNifVec* ival;
} scmi_env_handle;

static ErlNifResourceType* scmi_env_RESOURCE = NULL;
static unsigned scmi_env_RESOURCE_SIZE = sizeof(scmi_env_handle);
static ErlNifFunc nif_funcs[] =
    {
        {"$notify_when_destroyed", 3, scmi_env_notify_when_destroyed3},
        {"$is_resource", 1, scmi_env_is_resource1},
        {"$the_empty", 0, scmi_env_the_empty0},
        {"$extend", 3, scmi_env_extend3},
        {"$make_immutable", 1, scmi_env_make_immutable1},
        {"$is_immutable", 1, scmi_env_is_immutable1},
        {"$safe_lookup_variable", 2, scmi_env_safe_lookup_variable2},
        {"$set_variable", 3, scmi_env_set_variable3},
        {"$define_variable", 3, scmi_env_define_variable3}
    };

static void
scmi_env_resource_dtor(ErlNifEnv* env, void* arg)
{
    (void) env;
    scmi_env_handle* h = (scmi_env_handle*) arg;

    if (h->notifiees) {
        // notify in reverse order
        ErlNifPidVec::reverse_iterator rbegin = h->notifiees->rbegin();
        ErlNifPidVec::reverse_iterator rend = h->notifiees->rend();
        for (ErlNifPidVec::reverse_iterator i = rbegin; i != rend; ++i) {
            enif_send(NULL, &(i->first), h->env, i->second);
        }

        delete h->notifiees;
    }

    delete h->ivar;
    delete h->ival;

    enif_free_env(h->env);
    enif_free_env(h->vals);

    if (h->base) {
        enif_release_resource(h->base);
    }
}

static int
on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    (void) priv_data;
    (void) load_info;

    if (!scmi_env_lib_init(env)) {
        return -1;
    }

    ErlNifResourceFlags flags = (ErlNifResourceFlags) (ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);
    scmi_env_RESOURCE = enif_open_resource_type(env, NULL,
                                                "scmi_env_resource",
                                                &scmi_env_resource_dtor,
                                                flags, NULL);
    if (scmi_env_RESOURCE == NULL) {
        return -1;
    }

    return 0;
}

static int
on_reload(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    (void) env;
    (void) priv_data;
    (void) load_info;

    return 0;
}

static int
on_upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info)
{
    (void) env;
    (void) load_info;

    *priv_data = *old_priv_data;

    return 0;
}

static void
on_unload(ErlNifEnv* env, void* priv_data)
{
    (void) env;
    (void) priv_data;

    return;
}

ERL_NIF_INIT(scmi_env, nif_funcs, &on_load, &on_reload, &on_upgrade, &on_unload)

ERL_NIF_TERM
scmi_env_notify_when_destroyed3(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void) argc;

    scmi_env_handle* h;
    ErlNifPid pid;
    ERL_NIF_TERM msg;

    if (!enif_get_local_pid(env, argv[0], &pid)) {
        return MAKEBADARG(env);
    }

    if (!enif_get_resource(env, argv[2], scmi_env_RESOURCE, (void**)&h)) {
        return MAKEBADARG(env);
    }

    msg = enif_make_copy(h->env, argv[1]);

    if (!h->notifiees) {
        h->notifiees = new ErlNifPidVec();
        if (!h->notifiees) {
            return MAKEBADARG(env);
        }
    }
    h->notifiees->push_back(std::make_pair(pid, msg));

    return scmi_env_atom_true;
}

ERL_NIF_TERM
scmi_env_is_resource1(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void) argc;

    scmi_env_handle* h;

    if (!enif_get_resource(env, argv[0], scmi_env_RESOURCE, (void**)&h)) {
        return scmi_env_atom_false;
    }

    return scmi_env_atom_true;
}

ERL_NIF_TERM
scmi_env_the_empty0(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void) argc;
    (void) argv;
    ErlNifVec* ivar;
    ErlNifVec* ival;

    ivar = new ErlNifVec();
    ival = new ErlNifVec();
    if (!ivar || !ival) {
        delete ivar;
        delete ival;
        return MAKEBADARG(env);
    }

    // @TODO determine if this is tuned properly or should be made configurable?
    ivar->reserve(100);
    ival->reserve(100);

    scmi_env_handle* h = (scmi_env_handle*) enif_alloc_resource(scmi_env_RESOURCE, scmi_env_RESOURCE_SIZE);
    if (!h) {
        delete ivar;
        delete ival;
        return MAKEBADARG(env);
    }
    h->notifiees = NULL;
    h->base = NULL;
    h->immutable = false;
    h->env = enif_alloc_env();
    h->vals = enif_alloc_env();
    h->ivar = ivar;
    h->ival = ival;

    ERL_NIF_TERM result = enif_make_resource(env, h);
    enif_release_resource(h);
    return result;
}

ERL_NIF_TERM
scmi_env_extend3(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void) argc;

    ERL_NIF_TERM vars;
    unsigned vars_len;
    ERL_NIF_TERM vals;
    unsigned vals_len;
    scmi_env_handle* base;
    ERL_NIF_TERM head, tail;
    ErlNifVec* ivar;
    ErlNifVec* ival;

    if (!enif_get_list_length(env, argv[0], &vars_len)) {
        return MAKEBADARG(env);
    }
    vars = argv[0];

    if (!enif_get_list_length(env, argv[1], &vals_len)) {
        return MAKEBADARG(env);
    }
    vals = argv[1];

    if (vars_len != vals_len) {
        return MAKEBADARG(env);
    }

    if (!enif_get_resource(env, argv[2], scmi_env_RESOURCE, (void**)&base)) {
        return MAKEBADARG(env);
    }

    ivar = new ErlNifVec();
    ival = new ErlNifVec();
    if (!ivar || !ival) {
        delete ivar;
        delete ival;
        return MAKEBADARG(env);
    }

    scmi_env_handle* h = (scmi_env_handle*) enif_alloc_resource(scmi_env_RESOURCE, scmi_env_RESOURCE_SIZE);
    if (!h) {
        delete ivar;
        delete ival;
        return MAKEBADARG(env);
    }

    ivar->reserve(vars_len);
    ival->reserve(vals_len);

    enif_keep_resource(base);
    h->notifiees = NULL;
    h->base = base;
    h->immutable = false;
    h->env = enif_alloc_env();
    h->vals = enif_alloc_env();
    h->ivar = ivar;
    h->ival = ival;

    vars = enif_make_copy(h->env, vars);
    while (enif_get_list_cell(h->env, vars, &head, &tail)) {
        h->ivar->push_back(head);
        vars = tail;
    }

    bool unassigned = true;
    vals = enif_make_copy(h->vals, vals);
    while (enif_get_list_cell(h->vals, vals, &head, &tail)) {
        if (enif_is_identical(head, scmi_env_atom_)) {
            // special case when value is unassigned
            head = scmi_env_atom_;
        } else {
            unassigned = false;
        }

        h->ival->push_back(head);
        vals = tail;
    }
    // handle special case when all values are unassigned
    if (unassigned) {
        enif_clear_env(h->vals);
    }

    ERL_NIF_TERM result = enif_make_resource(env, h);
    enif_release_resource(h);
    return result;
}

ERL_NIF_TERM
scmi_env_make_immutable1(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void) argc;

    scmi_env_handle* h;

    if (!enif_get_resource(env, argv[0], scmi_env_RESOURCE, (void**)&h)) {
        return MAKEBADARG(env);
    }

    h->immutable = true;
    return scmi_env_atom_true;
}

ERL_NIF_TERM
scmi_env_is_immutable1(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void) argc;

    scmi_env_handle* h;

    if (!enif_get_resource(env, argv[0], scmi_env_RESOURCE, (void**)&h)) {
        return MAKEBADARG(env);
    }

    if (h->immutable) {
        return scmi_env_atom_true;
    } else {
        return scmi_env_atom_false;
    }
}

ERL_NIF_TERM
scmi_env_safe_lookup_variable2(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void) argc;

    ERL_NIF_TERM var;
    scmi_env_handle* h;

    var = argv[0];

    if (!enif_get_resource(env, argv[1], scmi_env_RESOURCE, (void**)&h)) {
        return MAKEBADARG(env);
    }

    // if macro identifier is given, find environment that contains
    // marker and then continue with normal processing from that
    // environment.
    {
        int arity;
        const ERL_NIF_TERM* tuple;
        ERL_NIF_TERM marker;

        if (enif_get_tuple(env, var, &arity, &tuple) && arity == 3) {
            if (enif_is_identical(tuple[0], scmi_env_atom_mid)) {
                marker = tuple[1];
                var = tuple[2];

            mid_env_loop:
                ErlNifVec::size_type size = h->ivar->size();
                for (ErlNifVec::size_type i=0; i < size; i++) {
                    if (enif_is_identical(marker, (*(h->ivar))[i])) {
                        goto env_loop;
                    }
                }

                if (h->base) {
                    h = h->base;
                    goto mid_env_loop;
                }

                return scmi_env_atom_;
            }
        }
    }

 env_loop:
    ErlNifVec::size_type size = h->ivar->size();
    for (ErlNifVec::size_type i=0; i < size; i++) {
        if (enif_is_identical(var, (*(h->ivar))[i])) {
            return enif_make_copy(env, (*(h->ival))[i]);
        }
    }

    if (h->base) {
        h = h->base;
        goto env_loop;
    }

    return scmi_env_atom_;
}

ERL_NIF_TERM
scmi_env_set_variable3(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void) argc;

    ERL_NIF_TERM var;
    ERL_NIF_TERM val;
    scmi_env_handle* h;

    var = argv[0];
    val = argv[1];

    if (!enif_get_resource(env, argv[2], scmi_env_RESOURCE, (void**)&h)) {
        return MAKEBADARG(env);
    }

 env_loop:
    if (h->immutable) {
        return MAKEBADARG(env);
    }

    ErlNifVec::size_type size = h->ivar->size();
    for (ErlNifVec::size_type i=0; i < size; i++) {
        if (enif_is_identical(var, (*(h->ivar))[i])) {
            if (enif_is_identical(val, scmi_env_atom_)) {
                // special case when value is unassigned
                val = scmi_env_atom_;
            } else {
                val = enif_make_copy(h->vals, val);
            }
            (*(h->ival))[i] = val;

            return scmi_env_atom_true;
        }
    }

    if (h->base) {
        h = h->base;
        goto env_loop;
    }

    return MAKEBADARG(env);
}

ERL_NIF_TERM
scmi_env_define_variable3(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void) argc;

    ERL_NIF_TERM var;
    ERL_NIF_TERM val;
    scmi_env_handle* h;

    var = argv[0];
    val = argv[1];

    if (!enif_get_resource(env, argv[2], scmi_env_RESOURCE, (void**)&h)) {
        return MAKEBADARG(env);
    }

    if (h->immutable) {
        return MAKEBADARG(env);
    }

    ErlNifVec::size_type size = h->ivar->size();
    for (ErlNifVec::size_type i=0; i < size; i++) {
        if (enif_is_identical(var, (*(h->ivar))[i])) {
            return MAKEBADARG(env);
        }
    }

    var = enif_make_copy(h->env, var);
    h->ivar->push_back(var);

    if (enif_is_identical(val, scmi_env_atom_)) {
        // special case when value is unassigned
        val = scmi_env_atom_;
    } else {
        val = enif_make_copy(h->vals, val);
    }
    h->ival->push_back(val);

    return scmi_env_atom_true;
}
