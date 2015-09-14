// The MIT License
//
// Copyright (C) 2013-2015 by Joseph Wayne Norton <norton@alum.mit.edu>
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
//

#include "scmi_iodev.h"
#include "scmi_iodev_lib.h"

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
    ErlNifEnv* env;
} scmi_iodev_handle;

static ErlNifResourceType* scmi_iodev_RESOURCE = NULL;
static unsigned scmi_iodev_RESOURCE_SIZE = sizeof(scmi_iodev_handle);
static ErlNifFunc nif_funcs[] =
    {
        {"$notify_when_destroyed", 3, scmi_iodev_notify_when_destroyed3},
        {"$is_resource", 1, scmi_iodev_is_resource1},
        {"$new", 0, scmi_iodev_new0},
    };

static void
scmi_iodev_resource_dtor(ErlNifEnv* env, void* arg)
{
    (void) env;
    scmi_iodev_handle* h = (scmi_iodev_handle*) arg;

    if (h->notifiees) {
        // notify in reverse order
        ErlNifPidVec::reverse_iterator rbegin = h->notifiees->rbegin();
        ErlNifPidVec::reverse_iterator rend = h->notifiees->rend();
        for (ErlNifPidVec::reverse_iterator i = rbegin; i != rend; ++i) {
            enif_send(NULL, &(i->first), h->env, i->second);
        }

        delete h->notifiees;
    }

    enif_free_env(h->env);
}

static int
on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    (void) priv_data;
    (void) load_info;

    if (!scmi_iodev_lib_init(env)) {
        return -1;
    }

    ErlNifResourceFlags flags = (ErlNifResourceFlags) (ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);
    scmi_iodev_RESOURCE = enif_open_resource_type(env, NULL,
                                                 "scmi_iodev_resource",
                                                 &scmi_iodev_resource_dtor,
                                                 flags, NULL);
    if (scmi_iodev_RESOURCE == NULL) {
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

ERL_NIF_INIT(scmi_iodev, nif_funcs, &on_load, &on_reload, &on_upgrade, &on_unload)

ERL_NIF_TERM
scmi_iodev_notify_when_destroyed3(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void) argc;

    scmi_iodev_handle* h;
    ErlNifPid pid;
    ERL_NIF_TERM msg;

    if (!enif_get_local_pid(env, argv[0], &pid)) {
        return MAKEBADARG(env);
    }

    if (!enif_get_resource(env, argv[2], scmi_iodev_RESOURCE, (void**)&h)) {
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

    return scmi_iodev_atom_true;
}

ERL_NIF_TERM
scmi_iodev_is_resource1(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void) argc;

    scmi_iodev_handle* h;

    if (!enif_get_resource(env, argv[0], scmi_iodev_RESOURCE, (void**)&h)) {
        return scmi_iodev_atom_false;
    }

    return scmi_iodev_atom_true;
}

ERL_NIF_TERM
scmi_iodev_new0(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void) argc;
    (void) argv;

    scmi_iodev_handle* h = (scmi_iodev_handle*) enif_alloc_resource(scmi_iodev_RESOURCE, scmi_iodev_RESOURCE_SIZE);
    if (!h) {
        return MAKEBADARG(env);
    }
    h->notifiees = NULL;
    h->env = enif_alloc_env();

    ERL_NIF_TERM result = enif_make_resource(env, h);
    enif_release_resource(h);
    return result;
}
