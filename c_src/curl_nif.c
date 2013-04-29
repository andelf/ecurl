/*  FileName    : curl_nif.c  */
/*  Author      : Wang ShuYu <andelf@gmail.com>  */
/*  Created     : Mon Apr 29 00:16:13 2013 by Wang ShuYu  */
/*  Copyright   : Feather Workshop (c) 2013  */
/*  Description : curl nif  */
/*  Time-stamp: <2013-04-29 15:41:58 andelf>  */

#include <stdio.h>
#include <curl/curl.h>
#include "erl_nif.h"


typedef struct ErlCURL_t
{
    CURL *handle;
} ErlCURL;

static ERL_NIF_TERM atom_ok;
static ERL_NIF_TERM atom_error;

static ERL_NIF_TERM atom_verbose;
static ERL_NIF_TERM atom_url;

static ErlNifResourceType *CURL_RESOURCE;

/* forward reference */
void cleanup(ErlNifEnv *env, void *p);

/* nif load */
static int load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
    atom_ok = enif_make_atom(env, "ok");
    atom_error = enif_make_atom(env, "error");

    atom_verbose = enif_make_atom(env, "verbose");
    atom_url = enif_make_atom(env, "url");

    if ( (CURL_RESOURCE =
          enif_open_resource_type(env, NULL,
                                  "curl_resource", cleanup,
                                  ERL_NIF_RT_CREATE, NULL)) == NULL)
        return -1;

    return 0;
}

static ERL_NIF_TERM new_curl_resource(ErlNifEnv* env, CURL *handle)
{
    ErlCURL *p = NULL;
    ERL_NIF_TERM res = {0};

    if (handle == NULL) {
        enif_release_resource(p);
        return atom_error;
    }

    p = enif_alloc_resource(CURL_RESOURCE, sizeof(ErlCURL));

    if (p == NULL)
        return atom_error;

    p->handle = handle;

    res = enif_make_resource(env, p);
    enif_release_resource(p);

    return enif_make_tuple(env, 2,
        atom_ok,
        res);
}


static ERL_NIF_TERM easy_init_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlCURL *p = NULL;
    ERL_NIF_TERM res = {0};

    p = enif_alloc_resource(CURL_RESOURCE, sizeof(ErlCURL));

    if (p == NULL)
        return atom_error;

    p->handle = curl_easy_init();

    if (p->handle == NULL) {
        enif_release_resource(p);
        return atom_error;
    }

    (void)fprintf(stderr, "alloc: p=%p/%p\r\n", p->handle, p);

    res = enif_make_resource(env, p);
    //enif_release_resource(p);

    return enif_make_tuple(env, 2,
        atom_ok,
        res);
}

static ERL_NIF_TERM easy_cleanup_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{

    ErlCURL *p = NULL;

    if (!enif_get_resource(env, argv[0], CURL_RESOURCE, (void **)&p))
        return enif_make_badarg(env);

    (void)fprintf(stderr, "free: p=%p/%p\n", p->handle, p);

    curl_easy_cleanup(p->handle);
    p->handle = NULL;

    return atom_ok;
}

static ERL_NIF_TERM easy_duphandle_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlCURL *p = NULL;

    if (!enif_get_resource(env, argv[0], CURL_RESOURCE, (void **)&p))
        return enif_make_badarg(env);

    return new_curl_resource(env, curl_easy_duphandle(p->handle));
}

static ERL_NIF_TERM easy_setopt_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlCURL *p = NULL;
    CURLoption option;
    unsigned long opt_val;
    char buf[1024];

    if (!enif_get_resource(env, argv[0], CURL_RESOURCE, (void **)&p))
        return enif_make_badarg(env);

    if (argv[1] == atom_verbose)
        option = CURLOPT_VERBOSE;
    else if (argv[1] == atom_url)
        option = CURLOPT_URL;
    else
        option = 0;

    fprintf(stderr, "option=%d\r\n", option);

    if (option > CURLOPTTYPE_OFF_T) {
        if (!enif_get_ulong(env, argv[2], &opt_val))
            return enif_make_badarg(env);
        curl_easy_setopt(p->handle, option, opt_val);
    } else if (option > CURLOPTTYPE_FUNCTIONPOINT) {
        fprintf(stderr, "blah~~~ \r\n");
    } else if (option > CURLOPTTYPE_OBJECTPOINT) {
        if (!enif_get_string(env, argv[2], buf, 1024, ERL_NIF_LATIN1))
            return enif_make_badarg(env);
        curl_easy_setopt(p->handle, option, buf);
    } else if (option > CURLOPTTYPE_LONG) {
        if (!enif_get_ulong(env, argv[2], &opt_val))
            return enif_make_badarg(env);
        curl_easy_setopt(p->handle, option, opt_val);
    } else {
        fprintf(stderr, "blah~~~ fuck\r\n");
    }

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM easy_perform_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlCURL *p = NULL;
    CURLcode ret;

    if (!enif_get_resource(env, argv[0], CURL_RESOURCE, (void **)&p))
        return enif_make_badarg(env);

    ret = curl_easy_perform(p->handle);

    if (CURLE_OK != ret) {
        return enif_make_tuple(env, 2, atom_error,
                               enif_make_int(env, ret));
    }

    return atom_ok;
}


static ERL_NIF_TERM debug_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlCURL *p = NULL;

    if (!enif_get_resource(env, argv[0], CURL_RESOURCE, (void **)&p))
        return enif_make_badarg(env);

    (void)fprintf(stderr, "debug: p=%p/%p\n", p->handle, p);

    return enif_make_atom(env, "ok");
}


/* rescoure cleanup */
void cleanup(ErlNifEnv *env, void *obj)
{
    ErlCURL *p = obj;
    (void)fprintf(stderr, "cleanup: *p=%p/%p\n", p->handle, p);
    if (p->handle) curl_easy_cleanup(p->handle);
}

static ErlNifFunc nif_funcs[] = {
    {"easy_init", 0, easy_init_nif},
    {"easy_cleanup", 1, easy_cleanup_nif},
    {"easy_duphandle", 1, easy_duphandle_nif},
    {"easy_setopt", 3, easy_setopt_nif},
    {"easy_perform", 1, easy_perform_nif},
    {"debug", 1, debug_nif}
//    {"bar", 1, bar_nif}
};

ERL_NIF_INIT(ecurl, nif_funcs, load, NULL, NULL, NULL)
