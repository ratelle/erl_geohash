-module(erl_geohash).

-export([
    init/0,
    geo_radius_hashes/4,
    geo_radiuses_hashes/2,
    hashes_to_term/1,
    point_in_hashes/3,
    debug_radius/4,
    debug_radiuses/2,
    hashes_to_rectangles/1,
    random_circles/7,
    visualize/2,
    visualize_hashes/2
]).

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

nif_hashes_to_term(_Hashes) ->
    {error, geohash_nif_not_loaded}.

point_in_hashes(_Lat, _Lon, _Hashes) ->
    {error, geohash_nif_not_loaded}.

hashes_to_term(Hashes) ->
    Lst = nif_hashes_to_term(Hashes),
    [{<<Value:64>>,Offset,LL,UR} || {Value,Offset,LL,UR} <- Lst].

hashes_to_rectangles(Hashes) ->
    Terms = hashes_to_term(Hashes),
    [{LL, UR} || {_, _, LL, UR} <- Terms].

debug_radius(Lat, Long, Distance, Iterations) ->
    hashes_to_term(geo_radius_hashes(Lat, Long, Distance, Iterations)).

debug_radiuses(Radiuses, Iterations) ->
    hashes_to_term(geo_radiuses_hashes(Radiuses, Iterations)).

rectangles_to_ejson(Rectangles) ->
    [{[{<<"ll">>, {[{<<"lat">>, LlLat}, {<<"lon">>, LlLon}]}},
       {<<"ur">>, {[{<<"lat">>, UrLat}, {<<"lon">>, UrLon}]}}]} || {{LlLon, LlLat}, {UrLon, UrLat}} <- Rectangles].

circles_to_ejson(Circles) ->
    [{[{<<"lat">>, Lat}, {<<"lon">>, Lon}, {<<"radius">>, Radius}]} || {Lat, Lon, Radius} <- Circles].

ejson_struct(Circles, Rectangles) ->
    {[{<<"circles">>, circles_to_ejson(Circles)}, {<<"rectangles">>, rectangles_to_ejson(Rectangles)}]}.

visualize(Circles, Rectangles) ->
    EJson = ejson_struct(Circles, Rectangles),
    Json = jiffy:encode(EJson),
    Priv = priv_dir(),
    {ok, Content} = file:read_file(filename:join(Priv, "visualize.html.template")),
    NewContent = binary:replace(Content, <<"__GEOHASH_ELEMS__">>, Json),
    file:write_file(filename:join(Priv, "visualize.html"), NewContent).

visualize_hashes(Circles, Iterations) ->
    Hashes = geo_radiuses_hashes(Circles, Iterations),
    Rectangles = hashes_to_rectangles(Hashes),
    visualize(Circles, Rectangles).

random_circles(N, MinLat, MaxLat, MinLong, MaxLong, MinRadius, MaxRadius) ->
    random_circles(N, MinLat, MaxLat, MinLong, MaxLong, MinRadius, MaxRadius, []).

random_circles(0, _, _, _, _, _, _, Accum) ->
    Accum;
random_circles(N, MinLat, MaxLat, MinLong, MaxLong, MinRadius, MaxRadius, Accum) ->
    Radius = {random_float(MinLat, MaxLat),
              random_float(MinLong, MaxLong),
              random_float(MinRadius, MaxRadius)},
    random_circles(N-1, MinLat, MaxLat, MinLong, MaxLong, MinRadius, MaxRadius, [Radius | Accum]).

random_float(Min, Max) ->
    Range = Max - Min,
    Add = Range * random:uniform(),
    Min + Add.
