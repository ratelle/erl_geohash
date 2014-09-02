-module(erl_geohash).

-export([
    init/0,
    build_index/2,
    circle_to_bounding_box/1,
    async_build_index/2,
    hashes_to_term/1,
    hashes_to_rectangles/1,
    index_to_term/1,
    json_radius_list_to_rectangles/2,
    point_in_hashes/2,
    point_in_hashes/3,
    point_in_circle/5,
    point_index_values/2,
    point_index_values/3,
    radius_list_to_hashes/2,
    radius_list_to_rectangles/2
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

circle_to_bounding_box(_Circle) ->
    {error, geohash_nif_not_loaded}.

radius_list_to_hashes(_RadiusList, _Iterations) ->
    {error, geohash_nif_not_loaded}.

radius_list_to_rectangles(RadiusList, Iterations) ->
    Hashes = radius_list_to_hashes(RadiusList, Iterations),
    hashes_to_rectangles(Hashes).

json_radius_list_to_rectangles(JsonList, Iterations) ->
    EJsonList = jiffy:decode(JsonList),
    Circles = [ejson_to_circle(Elem) || Elem <- EJsonList],
    Rectangles = radius_list_to_rectangles(Circles, Iterations),
    jiffy:encode([rectangle_to_ejson(Rectangle) || Rectangle <- Rectangles]).

rectangle_to_ejson({{LlLon, LlLat}, {UrLon, UrLat}}) ->
    {[{<<"ll">>, {[{<<"lat">>, LlLat}, {<<"lon">>, LlLon}]}},
      {<<"ur">>, {[{<<"lat">>, UrLat}, {<<"lon">>, UrLon}]}}]}.

ejson_to_circle({EJsonCircle}) ->
    {_, Latitude} = lists:keyfind(<<"lat">>, 1, EJsonCircle),
    {_, Longitude} = lists:keyfind(<<"lon">>, 1, EJsonCircle),
    {_, Radius} = lists:keyfind(<<"radius">>, 1, EJsonCircle),
    {float(Latitude), float(Longitude), float(Radius)}.

build_index(_Lists, _Iterations) ->
    {error, geohash_nif_not_loaded}.

nif_async_start_build_index(_Lists, _Iterations) ->
    {error, geohash_nif_not_loaded}.

nif_async_finish_build_index(_Tid) ->
    {error, geohash_nif_not_loaded}.

async_build_index(Lists, Iterations) ->
    {Ref, Tid} = nif_async_start_build_index(Lists, Iterations),
    receive
        {Ref, undefined} ->
            nif_async_finish_build_index(Tid),
            error(badarg);
        {Ref, Result} ->
            nif_async_finish_build_index(Tid),
            Result
    end.

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
