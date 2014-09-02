#include <string.h>
#include <ctype.h>
#include <erl_nif.h>
#include <math.h>
#include "geohash.h"

static ErlNifResourceType *hashes_type;
static ErlNifResourceType *index_type;
static ErlNifResourceType *index_builder_type;

static ERL_NIF_TERM atom_error;
static ERL_NIF_TERM atom_ok;
static ERL_NIF_TERM atom_undefined;
static ERL_NIF_TERM atom_true;
static ERL_NIF_TERM atom_false;

static void
hashes_type_destructor(ErlNifEnv* env, void* obj)
{
    void **wrapper = (void**)obj;
    destroy_vector(*wrapper);
}

static void
index_type_destructor(ErlNifEnv* env, void* obj)
{
    void **wrapper = (void**)obj;
    destroy_index(*wrapper);

}

static ERL_NIF_TERM
make_atom(ErlNifEnv *env, const char *name)
{
    ERL_NIF_TERM ret;

    if (enif_make_existing_atom(env, name, &ret, ERL_NIF_LATIN1)) {
        return ret;
    }
    return enif_make_atom(env, name);
}

static int
on_load(ErlNifEnv *env, void **priv, ERL_NIF_TERM info)
{

    atom_ok = make_atom(env, "ok");
    atom_error = make_atom(env, "error");
    atom_undefined = make_atom(env, "undefined");
    atom_true = make_atom(env, "true");
    atom_false = make_atom(env, "false");

    hashes_type = enif_open_resource_type(env, NULL, "hashes_type", hashes_type_destructor, ERL_NIF_RT_CREATE, NULL);
    index_type = enif_open_resource_type(env, NULL, "index_type", index_type_destructor, ERL_NIF_RT_CREATE, NULL);
    index_builder_type = enif_open_resource_type(env, NULL, "index_builder_type", NULL, ERL_NIF_RT_CREATE, NULL);

    return 0;
}

static ERL_NIF_TERM
nif_circle_to_bounding_box(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    const ERL_NIF_TERM *tuple;
    int arity;
    enif_get_tuple(env, argv[0], &arity, &tuple);

    if (arity != 3)
        return enif_make_badarg(env);

    double lat;
    double lon;
    double radius;

    if (!enif_get_double(env, tuple[0], &lat))
        return enif_make_badarg(env);

    if (!enif_get_double(env, tuple[1], &lon))
        return enif_make_badarg(env);

    if (!enif_get_double(env, tuple[2], &radius))
        return enif_make_badarg(env);

    return circle_to_bounding_box(env, lat, lon, radius);
}

static ERL_NIF_TERM
internal_radius_list_to_hashes(ErlNifEnv *env, ERL_NIF_TERM lst, ERL_NIF_TERM iterations)
{
    unsigned len;
    int it;

    if (!enif_get_list_length(env, lst, &len))
        return atom_undefined;

    if (!enif_get_int(env, iterations, &it))
        return atom_undefined;

    void *vector_pointer = radius_list_to_hashes(env, lst, len, it);

    if (vector_pointer == NULL)
        return atom_undefined;

    void **pointer_wrapper = (void**)enif_alloc_resource(hashes_type, sizeof(void*));

    *pointer_wrapper = vector_pointer;

    ERL_NIF_TERM retval = enif_make_resource(env, (void*)pointer_wrapper);
    enif_release_resource((void*)pointer_wrapper);

    return retval;
}

static ERL_NIF_TERM
nif_radius_list_to_hashes(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{

    ERL_NIF_TERM result = internal_radius_list_to_hashes(env, argv[0], argv[1]);

    if (result == atom_undefined)
        return enif_make_badarg(env);
    else
        return result;
}

static ERL_NIF_TERM
nif_build_index(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    unsigned len;
    int *values = NULL;
    void **vectors = NULL;
    ERL_NIF_TERM *errors = NULL;
    unsigned n_errors = 0;
    int successful = 0;
    ERL_NIF_TERM retval;

    if (!enif_get_list_length(env, argv[0], &len))
        goto cleanup;

    values = (int *)malloc(len * sizeof(int));
    vectors = (void **)malloc(len * sizeof(void*));
    errors = (ERL_NIF_TERM *)malloc(len * sizeof(ERL_NIF_TERM));

    unsigned i;
    ERL_NIF_TERM lst = argv[0];
    for (i = 0; i < len; i++)
    {
        ERL_NIF_TERM current;
        const ERL_NIF_TERM *tuple;
        int arity;

        enif_get_list_cell(env, lst, &current, &lst);
        enif_get_tuple(env, current, &arity, &tuple);

        if (arity != 2)
            goto cleanup;

        int value;
        void **wrapper;

        if (!enif_get_int(env, tuple[0], &value))
            goto cleanup;

        ERL_NIF_TERM hashes = internal_radius_list_to_hashes(env, tuple[1], argv[1]);

        if (!enif_get_resource(env, hashes, hashes_type, (void**)(&wrapper)))
            errors[n_errors++] = tuple[0];
        else
        {
            values[i] = value;
            vectors[i] = *wrapper;
        }
    }

    void *index_pointer = build_index(vectors, values, len - n_errors);
    void **pointer_wrapper = (void**)enif_alloc_resource(index_type, sizeof(void*));

    *pointer_wrapper = index_pointer;

    retval = enif_make_tuple2(env, enif_make_resource(env, (void*)pointer_wrapper), enif_make_list_from_array(env, errors, n_errors));
    successful = 1;

 cleanup:

    if (!successful)
        retval = enif_make_badarg(env);

    if (values != NULL)
        free(values);
    if (vectors != NULL)
        free(vectors);
    if (errors != NULL)
        free(errors);

    return retval;
}

struct index_env {
    ErlNifEnv *env;
    ERL_NIF_TERM ref;
    ErlNifPid pid;
    ERL_NIF_TERM argv[2];
};

static void *
async_build_index_thread(void *args)
{
    struct index_env *ie = (struct index_env*)args;

    ERL_NIF_TERM result = nif_build_index(ie->env, 2, ie->argv);
    ERL_NIF_TERM msg;

    if (enif_is_exception(ie->env, result))
        msg = enif_make_tuple2(ie->env, ie->ref, atom_error);
    else
        msg = enif_make_tuple2(ie->env, ie->ref, result);

    enif_send(NULL, &(ie->pid), ie->env, msg);

    enif_free_env(ie->env);
    free(ie);
    return NULL;
}

static ERL_NIF_TERM
nif_async_start_build_index(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM ref = enif_make_ref(env);
    ErlNifTid *tid = enif_alloc_resource(index_builder_type, sizeof(ErlNifTid));
    ERL_NIF_TERM retval;

    struct index_env *ie = malloc(sizeof(struct index_env));
    ie->env = enif_alloc_env();
    ie->argv[0] = enif_make_copy(ie->env, argv[0]);
    ie->argv[1] = enif_make_copy(ie->env, argv[1]);
    ie->ref = enif_make_copy(ie->env, ref);
    enif_self(env, &(ie->pid));

    if (enif_thread_create("geo_radius_index_builder", tid, async_build_index_thread, (void*)ie, NULL) == 0)
        retval = enif_make_tuple2(env, ref, enif_make_resource(env, tid));
    else
    {
        free(ie);
        retval = enif_make_badarg(env);
    }

    enif_release_resource(tid);
    return retval;
}

static ERL_NIF_TERM
nif_async_finish_build_index(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifTid *tid;
    if (!enif_get_resource(env, argv[0], index_builder_type, (void*)&tid))
        return enif_make_badarg(env);

    enif_thread_join(*tid, NULL);

    return atom_ok;
}

static ERL_NIF_TERM
nif_index_to_term(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    void **wrapper;

    if (!enif_get_resource(env, argv[0], index_type, (void**)(&wrapper)))
        return enif_make_badarg(env);

    ERL_NIF_TERM retval = index_to_term(env, *wrapper);

    return retval;
}

static ERL_NIF_TERM
nif_hashes_to_term(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    void **wrapper;

    if (!enif_get_resource(env, argv[0], hashes_type, (void**)(&wrapper)))
        return enif_make_badarg(env);

    ERL_NIF_TERM retval = hashes_to_term(env, *wrapper);

    return retval;
}

static ERL_NIF_TERM
nif_hashes_to_rectangles(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    void **wrapper;

    if (!enif_get_resource(env, argv[0], hashes_type, (void**)(&wrapper)))
        return enif_make_badarg(env);

    ERL_NIF_TERM retval = hashes_to_rectangles(env, *wrapper);

    return retval;
}

static ERL_NIF_TERM
nif_point_in_hashes(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    double lat;
    double lon;
    void **wrapper;

    if (!enif_get_double(env, argv[0], &lat))
        return enif_make_badarg(env);

    if (!enif_get_double(env, argv[1], &lon))
        return enif_make_badarg(env);

    if (!enif_get_resource(env, argv[2], hashes_type, (void**)(&wrapper)))
        return enif_make_badarg(env);

    return point_in_hashes(lat, lon, *wrapper) ? atom_true : atom_false;
}

static ERL_NIF_TERM
nif_point_index_values(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    double lat;
    double lon;
    void **wrapper;

    if (!enif_get_double(env, argv[0], &lat))
        return enif_make_badarg(env);

    if (!enif_get_double(env, argv[1], &lon))
        return enif_make_badarg(env);

    if (!enif_get_resource(env, argv[2], index_type, (void**)(&wrapper)))
        return enif_make_badarg(env);

    return point_index_values(env, lat, lon, *wrapper);
}

double geo_distance(double latitude1, double longitude1, double latitude2, double longitude2) {
    double dx, dy, dz;
    longitude1 -= longitude2;
    longitude1 = D2R(longitude1), latitude1 = D2R(latitude1), latitude2 = D2R(latitude2);

    dz = sin(latitude1) - sin(latitude2);
    dx = cos(longitude1) * cos(latitude1) - cos(latitude2);
    dy = sin(longitude1) * cos(latitude1);

    return asin(sqrt(dx*dx + dy*dy + dz*dz) / 2) * 2 * EARTH_RADIUS;
}

static ERL_NIF_TERM
nif_point_in_circle(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    double point_lat;
    double point_lon;

    double circle_lat;
    double circle_lon;
    double circle_rad;

    if (!enif_get_double(env, argv[0], &point_lat))
        return enif_make_badarg(env);

    if (!enif_get_double(env, argv[1], &point_lon))
        return enif_make_badarg(env);

    if (!enif_get_double(env, argv[2], &circle_lat))
        return enif_make_badarg(env);

    if (!enif_get_double(env, argv[3], &circle_lon))
        return enif_make_badarg(env);

    if (!enif_get_double(env, argv[4], &circle_rad))
        return enif_make_badarg(env);

    double distance = geo_distance(point_lat, point_lon, circle_lat, circle_lon);

    return distance <= circle_rad ? atom_true : atom_false;
}

static ErlNifFunc nif_functions[] = {
    {"circle_to_bounding_box", 1, nif_circle_to_bounding_box},
    {"radius_list_to_hashes", 2, nif_radius_list_to_hashes},
    {"hashes_to_term", 1, nif_hashes_to_term},
    {"index_to_term", 1, nif_index_to_term},
    {"hashes_to_rectangles", 1, nif_hashes_to_rectangles},
    {"point_in_hashes", 3, nif_point_in_hashes},
    {"point_in_circle", 5, nif_point_in_circle},
    {"point_index_values", 3, nif_point_index_values},
    {"build_index", 2, nif_build_index},
    {"nif_async_start_build_index", 2, nif_async_start_build_index},
    {"nif_async_finish_build_index", 1, nif_async_finish_build_index}
};

ERL_NIF_INIT(erl_geohash, nif_functions, &on_load, NULL, NULL, NULL);
