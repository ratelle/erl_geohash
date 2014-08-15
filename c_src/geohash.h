#include <erl_nif.h>

#define EARTH_RADIUS 6371.01

#define D2R(d) ((d / 360.0) * 2.0 * M_PI)
#define R2D(r) (r / M_PI / 2.0 * 360.0)

#ifdef __cplusplus
extern "C" {
#endif

    void *geo_radius_hashes(double lat, double lon, double distance, int iterations);
    void *geo_radiuses_hashes(ErlNifEnv *env, ERL_NIF_TERM lst, unsigned length, int iterations);
    int point_in_hashes(double lat, double lon, void *vec);
    void destroy_vector(void *vec);
    ERL_NIF_TERM hashes_to_term(ErlNifEnv *env, void *vec);

#ifdef __cplusplus
}
#endif
