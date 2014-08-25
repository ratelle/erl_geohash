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
    void destroy_index(void *index);
    ERL_NIF_TERM hashes_to_term(ErlNifEnv *env, void *vec);
    ERL_NIF_TERM hashes_to_rectangles(ErlNifEnv *env, void *vec);
    void *build_index(void **vectors, int *values, unsigned length);
    ERL_NIF_TERM index_to_term(ErlNifEnv *env, void *index);
    ERL_NIF_TERM point_index_values(ErlNifEnv *env, double lat, double lon, void *index);

#ifdef __cplusplus
}
#endif

#ifdef __cplusplus

typedef GeoRectangle<GeoRadianCoordinateSystem> GeoRadianRectangle;
typedef GeoPoint<GeoRadianCoordinateSystem> GeoRadianPoint;

typedef GeoRectangle<GeoDegreeCoordinateSystem> GeoDegreeRectangle;
typedef GeoPoint<GeoDegreeCoordinateSystem> GeoDegreePoint;

typedef uint64_t HashType;

typedef GeoPrefix<GeoDegreeCoordinateSystem,HashType> Prefix;
typedef GeoHash<GeoDegreeCoordinateSystem,HashType> Hash;
typedef std::vector<Prefix> PrefixVector;

ERL_NIF_TERM prefix_to_term(ErlNifEnv *env, Prefix& prefix);

class GeoIndexNode {

 public:
    GeoIndexNode() {}

    // Duplicates should never happen because of the unicity property of each prefix list
    // This is flaky at best but good enough for now
    GeoIndexNode(int val) { values.push_back(val); }
    GeoIndexNode(int val, Prefix child_key, GeoIndexNode *child)
    {
        values.push_back(val);
        keys.push_back(child_key);
        children.push_back(child);
    }

    ~GeoIndexNode()
    {
        unsigned size = children.size();
        for (unsigned i = 0; i < size; i++)
        {
            delete children[i];
        }
    }

    void insert(Prefix& new_key, int new_value);
    void add(int new_value) { values.push_back(new_value); }

    void point_values(std::vector<int>& results, Hash& point);

    ERL_NIF_TERM to_term(ErlNifEnv *env);

 private:
    std::vector<int> values;
    std::vector<Prefix> keys;
    std::vector<GeoIndexNode *> children;

    ERL_NIF_TERM values_to_term(ErlNifEnv *env);
    ERL_NIF_TERM children_to_term(ErlNifEnv *env);

};

void
GeoIndexNode::insert(Prefix& new_key, int new_value)
{
    int min = 0;
    int max = keys.size()-1;

    while (max >= min)
    {
        int mid = min + ((max - min) / 2);
        Prefix current_key = keys[mid];
        if (current_key == new_key)
        {
            children[mid]->add(new_value);
            return;
        }
        else if (current_key.applies_to(new_key))
        {
            children[mid]->insert(new_key, new_value);
            return;
        }
        // Should never happen if insertion order is respected
        else if (new_key.applies_to(current_key))
        {
            GeoIndexNode *current_node = children[mid];
            GeoIndexNode *new_node = new GeoIndexNode(new_value, current_key, current_node);
            children[mid] = new_node;
            keys[mid] = new_key;
            return;
        }
        else if(new_key.value() < current_key.value())
            max = mid - 1;
        else
            min = mid + 1;
    }

    GeoIndexNode *new_node = new GeoIndexNode(new_value);
    keys.insert(keys.begin()+min, new_key);
    children.insert(children.begin()+min, new_node);
}

class GeoIndex {

 public:

    GeoIndex(void **vectors, int *values, unsigned length)
    {
        n_values = length;
        for (int offset = 64; offset >= 0; offset--) {
            for (unsigned i = 0; i < length; i++)
            {
                PrefixVector *prefixes_ptr = static_cast<PrefixVector*>(vectors[i]);
                unsigned vector_len = prefixes_ptr->size();
                PrefixVector prefixes = *prefixes_ptr;
                int value = values[i];

                for(unsigned j = 0; j < vector_len; j++) {
                    if (prefixes[j].offset() == offset)
                        root.insert(prefixes[j], value);
                }
            }
        }
    }

    ERL_NIF_TERM to_term(ErlNifEnv *env);
    void point_values(std::vector<int>&, double, double);

 private:
    int n_values;
    GeoIndexNode root;

};

#endif
