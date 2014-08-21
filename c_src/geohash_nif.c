#include <string.h>
#include <ctype.h>
#include <erl_nif.h>
#include <math.h>
#include "geohash.h"

static ErlNifResourceType *hashes_type;
static ErlNifResourceType *index_type;

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

    return 0;
}

static ERL_NIF_TERM
nif_geo_radius_hashes(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    double lat;
    double lon;
    double distance;
    int iterations;

    if (!enif_get_double(env, argv[0], &lat))
        return enif_make_badarg(env);

    if (!enif_get_double(env, argv[1], &lon))
        return enif_make_badarg(env);

    if (!enif_get_double(env, argv[2], &distance))
        return enif_make_badarg(env);

    if (!enif_get_int(env, argv[3], &iterations))
        return enif_make_badarg(env);

    void *vector_pointer = geo_radius_hashes(lat, lon, distance, iterations);
    void **pointer_wrapper = (void**)enif_alloc_resource(hashes_type, sizeof(void*));

    *pointer_wrapper = vector_pointer;

    ERL_NIF_TERM retval = enif_make_resource(env, (void*)pointer_wrapper);
    enif_release_resource((void*)pointer_wrapper);

    return retval;
}

static ERL_NIF_TERM
nif_geo_radiuses_hashes(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    unsigned len;
    int iterations;

    if (!enif_get_list_length(env, argv[0], &len))
        return enif_make_badarg(env);

    if (!enif_get_int(env, argv[1], &iterations))
        return enif_make_badarg(env);

    void *vector_pointer = geo_radiuses_hashes(env, argv[0], len, iterations);

    if (vector_pointer == NULL)
        return enif_make_badarg(env);

    void **pointer_wrapper = (void**)enif_alloc_resource(hashes_type, sizeof(void*));

    *pointer_wrapper = vector_pointer;

    ERL_NIF_TERM retval = enif_make_resource(env, (void*)pointer_wrapper);
    enif_release_resource((void*)pointer_wrapper);

    return retval;
}

// Once again there's no proper cleanup here. Shopping for memleaks
static ERL_NIF_TERM
nif_build_index(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    unsigned len;
    int *values;
    void **vectors;

    if (!enif_get_list_length(env, argv[0], &len))
        return enif_make_badarg(env);

    values = (int *)malloc(len * sizeof(int));
    vectors = (void **)malloc(len * sizeof(void*));

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
            // Clean yourself here
            return enif_make_badarg(env);

        int value;
        void **wrapper;

        if (!enif_get_int(env, tuple[0], &value))
            return enif_make_badarg(env);

        if (!enif_get_resource(env, tuple[1], hashes_type, (void**)(&wrapper)))
            return enif_make_badarg(env);

        values[i] = value;
        vectors[i] = *wrapper;
    }

    void *index_pointer = build_index(vectors, values, len);
    void **pointer_wrapper = (void**)enif_alloc_resource(index_type, sizeof(void*));

    *pointer_wrapper = index_pointer;

    ERL_NIF_TERM retval = enif_make_resource(env, (void*)pointer_wrapper);

    enif_release_resource((void*)pointer_wrapper);
    free(values);
    free(vectors);

    return retval;
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
    {"geo_radius_hashes", 4, nif_geo_radius_hashes},
    {"geo_radiuses_hashes", 2, nif_geo_radiuses_hashes},
    {"nif_hashes_to_term", 1, nif_hashes_to_term},
    {"nif_index_to_term", 1, nif_index_to_term},
    {"hashes_to_rectangles", 1, nif_hashes_to_rectangles},
    {"point_in_hashes", 3, nif_point_in_hashes},
    {"point_in_circle", 5, nif_point_in_circle},
    {"point_index_values", 3, nif_point_index_values},
    {"build_index", 1, nif_build_index}
};

ERL_NIF_INIT(erl_geohash, nif_functions, &on_load, NULL, NULL, NULL);
