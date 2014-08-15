-module(erl_geohash).

-export([
    init/0,
    geo_radius_hashes/4,
    geo_radiuses_hashes/2,
    hashes_to_term/1,
    points_in_hashes/2,
    point_in_hashes/2,
    point_in_hashes/3,
    point_in_circle/5,
    point_in_circles/2,
    debug_radius/4,
    debug_radiuses/2,
    hashes_to_rectangles/1,
    random_circles/7,
    random_points/5,
    visualize/2,
    visualize_hashes/2,
    benchmark/9
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

nif_hashes_to_term(_Hashes) ->
    {error, geohash_nif_not_loaded}.

points_in_hashes(Points, Hashes) ->
    [point_in_hashes(Point, Hashes) || Point <- Points].

point_in_hashes({Lat, Lon}, Hashes) ->
    point_in_hashes(Lat, Lon, Hashes).

point_in_hashes(_Lat, _Lon, _Hashes) ->
    {error, geohash_nif_not_loaded}.

point_in_circle(_PointLat, _PointLon, _CircleLat, _CircleLon, _CircleRadius) ->
    {error, geohash_nif_not_loaded}.

point_in_circles({PointLat, PointLon}, Circles) ->
    point_in_circles(PointLat, PointLon, Circles).

point_in_circles(PointLat, PointLon, [{CircleLat, CircleLon, CircleRadius} | Rest]) ->
    case point_in_circle(PointLat, PointLon, CircleLat, CircleLon, CircleRadius) of
        false -> point_in_circles(PointLat, PointLon, Rest);
        true -> true
    end;
point_in_circles(_, _, []) ->
    false.

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
    Func = fun () -> random_circle(MinLat, MaxLat, MinLong, MaxLong, MinRadius, MaxRadius) end,
    make_n(Func, N).

random_points(N, MinLat, MaxLat, MinLong, MaxLong) ->
    Func = fun () -> random_point(MinLat, MaxLat, MinLong, MaxLong) end,
    make_n(Func, N).

make_n(Func, N) ->
    make_n(Func, N, []).

make_n(_, 0, Accum) ->
    Accum;
make_n(Func, N, Accum) ->
    make_n(Func, N-1, [Func() | Accum]).

random_point(MinLat, MaxLat, MinLong, MaxLong) ->
    {random_float(MinLat, MaxLat),
     random_float(MinLong, MaxLong)}.

random_circle(MinLat, MaxLat, MinLong, MaxLong, MinRadius, MaxRadius) ->
    {random_float(MinLat, MaxLat),
     random_float(MinLong, MaxLong),
     random_float(MinRadius, MaxRadius)}.

random_float(Min, Max) ->
    Range = Max - Min,
    Add = Range * random:uniform(),
    Min + Add.

benchmark(NLists, NCircles, NPoints, MinLat, MaxLat, MinLong, MaxLong, MinRadius, MaxRadius) ->
    CirclesLists = make_n(fun () -> random_circles(NCircles, MinLat, MaxLat, MinLong, MaxLong, MinRadius, MaxRadius) end, NLists),
    Points = random_points(NPoints, MinLat, MaxLat, MinLong, MaxLong),
    HashesLists = [geo_radiuses_hashes(Circles, 20) || Circles <- CirclesLists],
    HashesResults = timing:function(fun () -> points_in_all_hashes(Points, HashesLists) end, ?N, ?P),
    CirclesResults = timing:function(fun () -> points_in_all_circles(Points, CirclesLists) end, ?N, ?P),
    {HashesResults, CirclesResults}.

point_in_all_hashes_no_accum(Point, [Hashes | Rest]) ->
    point_in_hashes(Point, Hashes),
    point_in_all_hashes_no_accum(Point, Rest);
point_in_all_hashes_no_accum(_, []) ->
    ok.

point_in_all_circles_no_accum(Point, [Circles | Rest]) ->
    point_in_circles(Point, Circles),
    point_in_all_circles_no_accum(Point, Rest);
point_in_all_circles_no_accum(_, []) ->
    ok.

points_in_all_hashes([Point | Points], AllHashes) ->
    point_in_all_hashes_no_accum(Point, AllHashes),
    points_in_all_hashes(Points, AllHashes);
points_in_all_hashes([], _) ->
    ok.

points_in_all_circles([Point | Points], AllCircles) ->
    point_in_all_circles_no_accum(Point, AllCircles),
    points_in_all_circles(Points, AllCircles);
points_in_all_circles([], _) ->
    ok.
