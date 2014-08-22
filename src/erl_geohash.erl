-module(erl_geohash).

-export([
    init/0,
    build_index/1,
    geo_radius_hashes/4,
    geo_radiuses_hashes/2,
    hashes_to_term/1,
    hashes_to_rectangles/1,
    index_to_term/1,
    point_in_hashes/2,
    point_in_hashes/3,
    point_in_circle/5,
    point_index_values/2,
    point_index_values/3
]).

-define(N, 500).
-define(P, 1).

-on_load(init/0).

-spec init() -> ok.
init() ->
    SoName = filename:join(priv_dir(), "geohash_nif"),
    case catch erlang:load_nif(SoName,[]) of
        ok -> ok;
        LoadError -> error_logger:error_msg("erl_geohash: error loading NIF (~p): ~p",
                                            [SoName, LoadError])
    end.

priv_dir() ->
    case code:priv_dir(?MODULE) of
        {error, _} ->
            EbinDir = filename:dirname(code:which(?MODULE)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "priv");
        Dir -> Dir
    end.

geo_radius_hashes(_Lat, _Long, _Distance, _Iterations) ->
    {error, geohash_nif_not_loaded}.

geo_radiuses_hashes(_Radiuses, _Iterations) ->
    {error, geohash_nif_not_loaded}.

build_index(_HashLists) ->
    {error, geohash_nif_not_loaded}.

hashes_to_term(_Hashes) ->
    {error, geohash_nif_not_loaded}.

index_to_term(_Index) ->
    {error, geohash_nif_not_loaded}.

hashes_to_rectangles(_Hashes) ->
    {error, geohash_nif_not_loaded}.

point_in_hashes({Lat, Lon}, Hashes) ->
    point_in_hashes(Lat, Lon, Hashes).

point_in_hashes(_Lat, _Lon, _Hashes) ->
    {error, geohash_nif_not_loaded}.

point_in_circle(_PointLat, _PointLon, _CircleLat, _CircleLon, _CircleRadius) ->
    {error, geohash_nif_not_loaded}.

point_index_values({Lat, Lon}, Index) ->
    point_index_values(Lat, Lon, Index).

point_index_values(_Lat, _Lon, _Index) ->
    {error, geohash_nif_not_loaded}.
