#include "erl_nif.h"
#include <cdb.h>
#include <string.h>
#include <fcntl.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

static ErlNifResourceType* cdbnif_RESOURCE = NULL;
typedef struct
{
    int fd;
    struct cdb cdb;
    struct cdb_find cdbf; 
} cdbnif_handle;

cdbnif_handle * get_handle(ErlNifEnv* env, ERL_NIF_TERM term) {
    void * res;
    if(!enif_get_resource(env, term, cdbnif_RESOURCE, &res))
        return NULL;
    return (cdbnif_handle*)res;
}

char * get_str(ErlNifEnv* env, ERL_NIF_TERM term, int * sz) {
    ErlNifBinary binary;
    if(!enif_inspect_binary(env, term, &binary))
        return NULL;
    char * res = malloc(binary.size + 1);
    memmove(res, binary.data, binary.size);
    res[binary.size] = 0;
    if(sz)
        *sz = binary.size;
    return res;
}

// Prototypes
static ERL_NIF_TERM cdbnif_new(ErlNifEnv* env, int argc,
                            const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM cdbnif_next(ErlNifEnv* env, int argc,
                             const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM cdbnif_close(ErlNifEnv* env, int argc,
                              const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM cdbnif_seek(ErlNifEnv* env, int argc,
                             const ERL_NIF_TERM argv[]);

static ErlNifFunc nif_funcs[] =
{
    {"new", 1, cdbnif_new},
    {"next", 1, cdbnif_next},
    {"close", 1, cdbnif_close},
    {"seek", 2, cdbnif_seek}
};

static ERL_NIF_TERM cdbnif_new(ErlNifEnv* env, int argc,
                                 const ERL_NIF_TERM argv[])
{
    char * path = get_str(env, argv[0], NULL);
    if(path==NULL)
        return enif_make_atom(env, "open_error");
    cdbnif_handle* handle = enif_alloc_resource(cdbnif_RESOURCE,
                                                sizeof(cdbnif_handle));
    ERL_NIF_TERM result = enif_make_resource(env, handle);
    enif_release_resource(handle);
    handle->fd = open(path, O_RDONLY);
    free(path);
    if(handle->fd <0)
        return enif_make_atom(env, "open_error");
    if(cdb_init(&(handle->cdb), handle->fd) < 0 )
        return enif_make_atom(env, "invalid_file");
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), result);
}

static ERL_NIF_TERM cdbnif_seek(ErlNifEnv* env, int argc,
                                const ERL_NIF_TERM argv[])
{
    cdbnif_handle* handle = get_handle(env, argv[0]);
    if(!handle)
        return enif_make_atom(env, "invalid_handle");

    int key_len;
    char * key = get_str(env, argv[1], &key_len);
    if(!key)
        return enif_make_atom(env, "invalid_key");
    int fres = cdb_findinit(&(handle->cdbf), &(handle->cdb), key, key_len);
    free(key);
    if(fres<0) {
        printf("failed seek %i\n", fres);
        return enif_make_atom(env, "failed_seek");
    }
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM cdbnif_next(ErlNifEnv* env, int argc,
                                const ERL_NIF_TERM argv[])
{
    unsigned vpos, vlen;
    void * data;
    ERL_NIF_TERM res;
    cdbnif_handle* handle = get_handle(env, argv[0]);
    if(!handle)
        return enif_make_atom(env, "invalid_handle");
    if(cdb_findnext(&(handle->cdbf))>0) {
        vpos = cdb_datapos(&(handle->cdb));
        vlen = cdb_datalen(&(handle->cdb));
        data = enif_make_new_binary(env, vlen, &res);
        cdb_read(&(handle->cdb), data, vlen, vpos);
        return enif_make_tuple2(env, enif_make_atom(env, "ok"), res);
    } else {
        return enif_make_atom(env, "eof");
    }
}

static ERL_NIF_TERM cdbnif_close(ErlNifEnv* env, int argc,
                                const ERL_NIF_TERM argv[])
{
    cdbnif_handle* handle = get_handle(env, argv[0]);
    if(!handle)
        return enif_make_atom(env, "invalid_handle");
    if(handle->fd >=0 ) {
        close(handle->fd);
        handle->fd = -1;
    }
    return enif_make_atom(env, "ok");
}
static void cdbnif_resource_cleanup(ErlNifEnv* env, void* arg)
{
    /* Delete any dynamically allocated memory stored in cdbnif_handle */
    cdbnif_handle* handle = (cdbnif_handle*)arg; 
    close(handle->fd);
}

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    ErlNifResourceFlags flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
    ErlNifResourceType* rt = enif_open_resource_type(env, NULL,
                                                     "cdbref",
                                                     &cdbnif_resource_cleanup,
                                                     flags, NULL);
    if (rt == NULL)
        return -1;

    cdbnif_RESOURCE = rt;

    return 0;
}

ERL_NIF_INIT(cdberl_nif, nif_funcs, &on_load, NULL, NULL, NULL);
