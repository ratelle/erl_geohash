#include "geoprimitives.h"
#include "geohash.h"
#include <erl_nif.h>
#include <iostream>
#include <cmath>
#include <iterator>
#include <algorithm>
#include <vector>

using namespace std;

GeoDegreeRectangle bounding_coordinates(GeoDegreePoint& point, double distance, double radius)
{

    double xrange[2];
    double yrange[2];
    GeoRadianCoordinateSystem::GetRanges(xrange, yrange);

    // Should check for <0 double args

    double radLat = D2R(point.y());
    double radLon = D2R(point.x());

    double radDist = distance / radius;
    double minLat = radLat - radDist;
    double maxLat = radLat + radDist;
    double minLon;
    double maxLon;

    if (minLat > yrange[0] && maxLat < yrange[1]) {
        double deltaLon = asin(sin(radDist) / cos(radLat));
        minLon = radLon - deltaLon;
        if (minLon < xrange[0]) minLon += 2.0 * M_PI;
        maxLon = radLon + deltaLon;
        if (maxLon > xrange[1]) maxLon -= 2.0 * M_PI;
    } else {
        // a pole is within the distance
        minLat = max(minLat, yrange[0]);
        maxLat = min(maxLat, yrange[1]);
        minLon = xrange[0];
        maxLon = xrange[1];
    }

    return GeoDegreeRectangle(R2D(minLon), R2D(minLat), R2D(maxLon), R2D(maxLat));
}

PrefixVector
internal_geo_radius_hashes(double lat, double lon, double distance, int iterations)
{
    GeoDegreePoint point (lon, lat);
    GeoDegreeRectangle box = bounding_coordinates(point, distance, EARTH_RADIUS);
    PrefixVector prefixes;
    Prefix::search_prefixes(box, prefixes, iterations);

    return prefixes;
}

void *
geo_radius_hashes(double lat, double lon, double distance, int iterations)
{
    PrefixVector prefixes = internal_geo_radius_hashes(lat, lon, distance, iterations);
    PrefixVector *prefixes_ptr = new PrefixVector(prefixes);

    return static_cast<void*>(prefixes_ptr);
}

PrefixVector
merge_prefixes(PrefixVector& prefixes)
{
    unsigned size = prefixes.size();
    PrefixVector workspace;

    if (size == 0)
        return workspace;

    workspace.reserve(size);

    Prefix current = prefixes[0];
    workspace.push_back(current);

    for (unsigned i = 1; i < size; i++)
    {
        if (!current.applies_to(prefixes[i]))
        {
            current = prefixes[i];
            workspace.push_back(current);
        }
    }

    return workspace;
}

void *
geo_radiuses_hashes(ErlNifEnv *env, ERL_NIF_TERM lst, unsigned length, int iterations)
{
    PrefixVector all_prefixes;

    for (unsigned i = 0; i < length; i++)
    {
        ERL_NIF_TERM current;
        const ERL_NIF_TERM *tuple;
        int arity;
        enif_get_list_cell(env, lst, &current, &lst);
        enif_get_tuple(env, current, &arity, &tuple);
        if (arity != 3)
            return NULL;

        double lat;
        double lon;
        double distance;

        // And clean yourself here
        if (!enif_get_double(env, tuple[0], &lat))
            return NULL;

        if (!enif_get_double(env, tuple[1], &lon))
            return NULL;

        if (!enif_get_double(env, tuple[2], &distance))
            return NULL;

        PrefixVector prefixes = internal_geo_radius_hashes(lat, lon, distance, iterations);
        all_prefixes.insert(all_prefixes.end(), prefixes.begin(), prefixes.end());
    }

    sort (all_prefixes.begin(), all_prefixes.end());

    PrefixVector merged = merge_prefixes(all_prefixes);

    PrefixVector *merged_ptr = new PrefixVector(merged);

    return static_cast<void*>(merged_ptr);
}

int
point_in_hashes(double lat, double lon, void *vec)
{
    PrefixVector *prefixes = static_cast<PrefixVector*>(vec);
    GeoHasher<HashType> hasher;
    GeoDegreePoint point(lon, lat);
    Hash hash = hasher.hash(point);

    int min = 0;
    int max = prefixes->size()-1;

    while (max >= min)
    {
        int mid = min + ((max - min) / 2);
        Prefix prefix = (*prefixes)[mid];
        if (prefix.applies_to(hash))
            return 1;
        else if (hash.value() < prefix.value())
            max = mid - 1;
        else
            min = mid + 1;
    }

    return 0;
}

void destroy_vector(void *vec)
{
    delete static_cast<PrefixVector*>(vec);
}

void destroy_index(void *index)
{
    delete static_cast<GeoIndex*>(index);
}

ERL_NIF_TERM
GeoIndexNode::values_to_term(ErlNifEnv *env)
{
    unsigned size = values.size();
    ERL_NIF_TERM *array = new ERL_NIF_TERM[size];
    for (unsigned i = 0; i < size; i++)
        array[i] = enif_make_int(env, values[i]);

    ERL_NIF_TERM list = enif_make_list_from_array(env, array, size);

    delete [] array;

    return list;
}

ERL_NIF_TERM
GeoIndexNode::children_to_term(ErlNifEnv *env)
{
    unsigned size = keys.size();
    ERL_NIF_TERM *array = new ERL_NIF_TERM[size];
    for (unsigned i = 0; i < size; i++)
        array[i] = enif_make_tuple2(env, prefix_to_term(env, keys[i]), children[i]->to_term(env));

    ERL_NIF_TERM list = enif_make_list_from_array(env, array, size);

    delete [] array;

    return list;
}

ERL_NIF_TERM
GeoIndexNode::to_term(ErlNifEnv *env)
{
    ERL_NIF_TERM values = values_to_term(env);
    ERL_NIF_TERM children = children_to_term(env);

    return enif_make_tuple2(env, values, children);
}

void
GeoIndexNode::point_values(vector<int>& results, Hash& point)
{

    results.insert(results.end(), values.begin(), values.end());

    int min = 0;
    int max = keys.size()-1;

    while (max >= min)
    {
        int mid = min + ((max - min) / 2);
        Prefix current_key = keys[mid];
        if (current_key.applies_to(point))
        {
            children[mid]->point_values(results, point);
            return;
        }
        else if(point.value() < current_key.value())
            max = mid - 1;
        else
            min = mid + 1;
    }

}

ERL_NIF_TERM
GeoIndex::to_term(ErlNifEnv *env)
{
    return root.to_term(env);
}

void
GeoIndex::point_values(vector<int>& results, double lat, double lon)
{
    GeoHasher<HashType> hasher;
    GeoDegreePoint point(lon, lat);
    Hash hash = hasher.hash(point);

    results.reserve(n_values);

    root.point_values(results, hash);

}


ERL_NIF_TERM
index_to_term(ErlNifEnv *env, void *index_ptr)
{
    GeoIndex *index = static_cast<GeoIndex*>(index_ptr);

    return index->to_term(env);
}

ERL_NIF_TERM
point_index_values(ErlNifEnv *env, double lat, double lon, void *index_ptr)
{
    GeoIndex *index = static_cast<GeoIndex*>(index_ptr);
    vector<int> results;

    index->point_values(results, lat, lon);

    unsigned size = results.size();

    ERL_NIF_TERM *array = new ERL_NIF_TERM[size];

    for (unsigned i = 0; i < size; i++)
        array[i] = enif_make_int(env, results[i]);

    ERL_NIF_TERM list = enif_make_list_from_array(env, array, size);

    delete [] array;

    return list;
}


ERL_NIF_TERM prefix_to_term(ErlNifEnv *env, Prefix& prefix)
{
    return enif_make_tuple2(env, enif_make_uint64(env, prefix.value()), enif_make_int(env, prefix.offset()));
}

ERL_NIF_TERM prefix_to_rectangle(ErlNifEnv *env, Prefix& prefix)
{
    GeoRectangle<GeoDegreeCoordinateSystem> bounds = prefix.bounds();
    GeoPoint<GeoDegreeCoordinateSystem> ll = bounds.ll();
    GeoPoint<GeoDegreeCoordinateSystem> ur = bounds.ur();

    ERL_NIF_TERM ll_term = enif_make_tuple2(env, enif_make_double(env, ll.x()), enif_make_double(env, ll.y()));
    ERL_NIF_TERM ur_term = enif_make_tuple2(env, enif_make_double(env, ur.x()), enif_make_double(env, ur.y()));

    return enif_make_tuple2(env, ll_term, ur_term);
}

ERL_NIF_TERM hashes_to_term(ErlNifEnv *env, void *vec)
{
    PrefixVector *prefixes = static_cast<PrefixVector*>(vec);

    unsigned size = prefixes->size();

    ERL_NIF_TERM *prefix_list = new ERL_NIF_TERM[size];

    for (unsigned i = 0; i < size; i++)
    {
        Prefix prefix = (*prefixes)[i];
        prefix_list[i] = prefix_to_term(env, prefix);
    }

    ERL_NIF_TERM lst = enif_make_list_from_array(env, prefix_list, size);

    delete [] prefix_list;

    return lst;
}

ERL_NIF_TERM hashes_to_rectangles(ErlNifEnv *env, void *vec)
{
    PrefixVector *prefixes = static_cast<PrefixVector*>(vec);

    unsigned size = prefixes->size();

    ERL_NIF_TERM *prefix_list = new ERL_NIF_TERM[size];

    for (unsigned i = 0; i < size; i++)
    {
        Prefix prefix = (*prefixes)[i];
        prefix_list[i] = prefix_to_rectangle(env, prefix);
    }

    ERL_NIF_TERM lst = enif_make_list_from_array(env, prefix_list, size);

    delete [] prefix_list;

    return lst;
}

void *
build_index(void **vectors, int *values, unsigned length)
{
    GeoIndex *index = new GeoIndex(vectors, values, length);
    return static_cast<void*>(index);
}
