#include "erl_nif.h"
#include <cdb.h>
#include <string.h>
#include <fcntl.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

static ErlNifResourceType* cdbwriter_RESOURCE = NULL;
typedef struct
{
    int fd;
    struct cdb_make cdbm;
    char *target;
    char *tmpfile;
} cdbwriter_handle;

cdbwriter_handle * get_handle(ErlNifEnv* env, ERL_NIF_TERM term) {
    void * res;
    if(!enif_get_resource(env, term, cdbwriter_RESOURCE, &res))
        return NULL;
    return (cdbwriter_handle*)res;
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
static ERL_NIF_TERM cdbwriter_new(ErlNifEnv* env, int argc,
                            const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM cdbwriter_add(ErlNifEnv* env, int argc,
                             const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM cdbwriter_finish(ErlNifEnv* env, int argc,
                              const ERL_NIF_TERM argv[]);

static ErlNifFunc nif_funcs[] =
{
    {"new", 2, cdbwriter_new},
    {"add", 3, cdbwriter_add},
    {"finish", 1, cdbwriter_finish}
};

static ERL_NIF_TERM cdbwriter_new(ErlNifEnv* env, int argc,
                                 const ERL_NIF_TERM argv[])
{
    char * path = get_str(env, argv[0], NULL);
    int tmp_len;
    ERL_NIF_TERM result;
    if(path==NULL)
        return enif_make_atom(env, "invalid_path");
    cdbwriter_handle* handle = enif_alloc_resource(cdbwriter_RESOURCE,
                                                sizeof(cdbwriter_handle));
    handle->fd = -1;
    handle->target = path;
    handle->tmpfile = get_str(env, argv[1], &tmp_len);
    if(!handle->tmpfile || tmp_len<6)
        return enif_make_atom(env, "invalid_tmp_path");
    result = enif_make_resource(env, handle);
    enif_release_resource(handle);
    handle->fd = mkstemp(handle->tmpfile);
    if(handle->fd <0)
        return enif_make_atom(env, "open_error");
    if(cdb_make_start(&(handle->cdbm), handle->fd) < 0 )
        return enif_make_atom(env, "cant_start_write");
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), result);
}

static ERL_NIF_TERM cdbwriter_add(  ErlNifEnv* env, int argc,
                                    const ERL_NIF_TERM argv[])
{
    cdbwriter_handle* handle = get_handle(env, argv[0]);
    if(!handle || (handle->fd < 0))
        return enif_make_atom(env, "invalid_handle");

    int key_len;
    char * key = get_str(env, argv[1], &key_len);
    int val_len;
    if(!key)
        return enif_make_atom(env, "invalid_key");
    char * val = get_str(env, argv[2], &val_len);
    if(!val)
        return enif_make_atom(env, "invalid_val");
    int fres = cdb_make_add(&(handle->cdbm), key, key_len, val, val_len);
    free(key);
    free(val);
    if(fres!=0)
        return enif_make_atom(env, "failed_add");
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM cdbwriter_finish(ErlNifEnv* env, int argc,
                                const ERL_NIF_TERM argv[])
{
    cdbwriter_handle* handle = get_handle(env, argv[0]);
    if(!handle || (handle->fd < 0))
        return enif_make_atom(env, "invalid_handle");

    if(cdb_make_finish(&(handle->cdbm))<0) 
        return enif_make_atom(env, "could_not_finalize");
    close(handle->fd);
    handle->fd = -1;
    if(rename(handle->tmpfile, handle->target)!=0)
        return enif_make_atom(env, "move_error");
    return enif_make_atom(env, "ok");
}

static void cdbwriter_resource_cleanup(ErlNifEnv* env, void* arg)
{
    /* Delete any dynamically allocated memory stored in cdbwriter_handle */
    cdbwriter_handle* handle = (cdbwriter_handle*)arg; 
    if(handle->fd < 0)
        return;
    unlink(handle->tmpfile);
    close(handle->fd);
}

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    ErlNifResourceFlags flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
    ErlNifResourceType* rt = enif_open_resource_type(env, NULL,
                                                     "cdbmake_ref",
                                                     &cdbwriter_resource_cleanup,
                                                     flags, NULL);
    if (rt == NULL)
        return -1;

    cdbwriter_RESOURCE = rt;

    return 0;
}

ERL_NIF_INIT(cdberlwriter_nif, nif_funcs, &on_load, NULL, NULL, NULL);
