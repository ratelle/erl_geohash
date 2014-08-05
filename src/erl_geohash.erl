-module(erl_geohash).

-export([
    init/0,
    geo_radius_hashes/4,
    geo_radiuses_hashes/2,
    hashes_to_term/1,
    point_in_hashes/3,
    debug_radius/4,
    debug_radiuses/2
]).

-on_load(init/0).

-spec init() -> ok.
init() ->
    PrivDir = case code:priv_dir(?MODULE) of
                  {error, _} ->
                      EbinDir = filename:dirname(code:which(?MODULE)),
                      AppPath = filename:dirname(EbinDir),
                      filename:join(AppPath, "priv");
                  Dir -> Dir
              end,
    SoName = filename:join(PrivDir, "geohash_nif"),
    case catch erlang:load_nif(SoName,[]) of
        ok -> ok;
        LoadError -> error_logger:error_msg("erl_geohash: error loading NIF (~p): ~p",
                                            [SoName, LoadError])
    end.

geo_radius_hashes(_Lat, _Long, _Distance, _Iterations) ->
    {error, geohash_nif_not_loaded}.

geo_radiuses_hashes(_Radiuses, _Iterations) ->
    {error, geohash_nif_not_loaded}.

nif_hashes_to_term(_Hashes) ->
    {error, geohash_nif_not_loaded}.

point_in_hashes(_Lat, _Lon, _Hashes) ->
    {error, geohash_nif_not_loaded}.

hashes_to_term(Hashes) ->
    Lst = nif_hashes_to_term(Hashes),
    [{<<Value:64>>,Offset,Lat,Long} || {Value,Offset,Lat,Long} <- Lst].

debug_radius(Lat, Long, Distance, Iterations) ->
    hashes_to_term(geo_radius_hashes(Lat, Long, Distance, Iterations)).

debug_radiuses(Radiuses, Iterations) ->
    hashes_to_term(geo_radiuses_hashes(Radiuses, Iterations)).
