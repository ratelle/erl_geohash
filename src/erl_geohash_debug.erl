-module(erl_geohash_debug).

-export([
    prefix_to_binary/1,
    debug_radius/4,
    debug_radiuses/2,
    random_circles/1,
    random_circles/7,
    random_points/5,
    visualize/2,
    visualize_hashes/2,
    benchmark/9,
    test_index/2,
    verify/4
]).

priv_dir() ->
    case code:priv_dir(?MODULE) of
        {error, _} ->
            EbinDir = filename:dirname(code:which(?MODULE)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "priv");
        Dir -> Dir
    end.

visualize_hashes(Circles, Iterations) ->
    Hashes = erl_geohash:geo_radiuses_hashes(Circles, Iterations),
    Rectangles = erl_geohash:hashes_to_rectangles(Hashes),
    visualize([Circles], [Rectangles]).

random_circles(N) ->
    random_circles(N, 30.0, 60.0, -45.0, -75.0, 500.0, 1000.0).

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

visualize(CirclesList, RectanglesList) ->
    Structs = lists:zip(CirclesList, RectanglesList),
    EJson = [ejson_struct(Circles, Rectangles) || {Circles, Rectangles} <- Structs],
    Json = jiffy:encode(EJson),
    Priv = priv_dir(),
    {ok, Content} = file:read_file(filename:join(Priv, "visualize.html.template")),
    NewContent = binary:replace(Content, <<"__GEOHASH_ELEMS__">>, Json),
    file:write_file(filename:join(Priv, "visualize.html"), NewContent).

debug_radius(Lat, Long, Distance, Iterations) ->
    hashes_to_term(erl_geohash:geo_radius_hashes(Lat, Long, Distance, Iterations)).

debug_radiuses(Radiuses, Iterations) ->
    hashes_to_term(erl_geohash:geo_radiuses_hashes(Radiuses, Iterations)).

rectangles_to_ejson(Rectangles) ->
    [{[{<<"ll">>, {[{<<"lat">>, LlLat}, {<<"lon">>, LlLon}]}},
       {<<"ur">>, {[{<<"lat">>, UrLat}, {<<"lon">>, UrLon}]}}]} || {{LlLon, LlLat}, {UrLon, UrLat}} <- Rectangles].

circles_to_ejson(Circles) ->
    [{[{<<"lat">>, Lat}, {<<"lon">>, Lon}, {<<"radius">>, Radius}]} || {Lat, Lon, Radius} <- Circles].

ejson_struct(Circles, Rectangles) ->
    {[{<<"circles">>, circles_to_ejson(Circles)}, {<<"rectangles">>, rectangles_to_ejson(Rectangles)}]}.

verify(NLists, NCircles, NPoints, Precision) ->
    MinLat = 40.0,
    MaxLat = 45.0,
    MinLon = -75.0,
    MaxLon = -70.0,
    MinRadius = 1.0,
    MaxRadius = 20.0,
    CirclesLists = lists:zip(lists:seq(1,NLists), make_n(fun () -> random_circles(NCircles, MinLat, MaxLat, MinLon, MaxLon, MinRadius, MaxRadius) end, NLists)),
    HashesLists = [{I, erl_geohash:geo_radiuses_hashes(Circles, Precision)} || {I, Circles} <- CirclesLists],
    Index = erl_geohash:build_index(HashesLists),
    Points = random_points(NPoints, MinLat, MaxLat, MinLon, MaxLon),
    check(CirclesLists, HashesLists, Index, Points).

check(_CirclesLists, _HashesLists, _Index, []) ->
    ok;
check(CirclesLists, HashesLists, Index, [Point | Points]) ->
    CirclesResults = check_circles(Point, CirclesLists, []),
    HashesResults = check_hashes(Point, HashesLists, []),
    IndexResults = lists:sort(erl_geohash:point_index_values(Point, Index)),
    io:format("~p ~p~n", [length(CirclesResults), length(HashesResults)]),
    case HashesResults == IndexResults of
        true -> case lists:subtract(CirclesResults, HashesResults) of
                    [] -> check(CirclesLists, HashesLists, Index, Points);
                    _ -> error(circles_not_a_subset)
                end;
        false ->
             error(hashes_index_not_equal)
    end.

check_hashes(_, [], Accum) ->
    lists:sort(Accum);
check_hashes(Point, [{Id, Hashes} | Rest], Accum) ->
    case erl_geohash:point_in_hashes(Point, Hashes) of
        true -> check_hashes(Point, Rest, [Id | Accum]);
        false-> check_hashes(Point, Rest, Accum)
    end.

check_circles(_, [], Accum) ->
    lists:sort(Accum);
check_circles(Point, [{Id, Circles} | Rest], Accum) ->
    case point_in_circles(Point, Circles) of
        true -> check_circles(Point, Rest, [Id | Accum]);
        false-> check_circles(Point, Rest, Accum)
    end.

benchmark(NLists, NCircles, _NPoints, MinLat, MaxLat, MinLong, MaxLong, MinRadius, MaxRadius) ->
    CirclesLists = make_n(fun () -> random_circles(NCircles, MinLat, MaxLat, MinLong, MaxLong, MinRadius, MaxRadius) end, NLists),
    %Points = random_points(NPoints, MinLat, MaxLat, MinLong, MaxLong),
    Timestamp = os:timestamp(),
    HashesLists = [erl_geohash:geo_radiuses_hashes(Circles, 20) || Circles <- CirclesLists],
    io:format("~p~n",[timer:now_diff(os:timestamp(), Timestamp)]),
    Timestamp2 = os:timestamp(),
    erl_geohash:build_index(lists:zip(lists:seq(1, NLists), HashesLists)),
    io:format("~p~n",[timer:now_diff(os:timestamp(), Timestamp2)]).

    %timing:function(fun() -> points_in_index(Points, Index) end, ?N, ?P).

    %% HashesResults = timing:function(fun () -> points_in_all_hashes(Points, HashesLists) end, ?N, ?P),
    %% CirclesResults = timing:function(fun () -> points_in_all_circles(Points, CirclesLists) end, ?N, ?P),
    %% {IndexResults, HashesResults, CirclesResults}.

point_in_all_hashes_no_accum(Point, [Hashes | Rest]) ->
    erl_geohash:point_in_hashes(Point, Hashes),
    point_in_all_hashes_no_accum(Point, Rest);
point_in_all_hashes_no_accum(_, []) ->
    ok.

point_in_all_circles_no_accum(Point, [Circles | Rest]) ->
    erl_geohash:point_in_circles(Point, Circles),
    point_in_all_circles_no_accum(Point, Rest);
point_in_all_circles_no_accum(_, []) ->
    ok.

points_in_index([Point | Points], Index) ->
    erl_geohash:point_index_values(Point, Index),
    points_in_index(Points, Index);
points_in_index([], _) ->
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

test_index(CircleN, ListN) ->
    CirclesList = [{I, random_circles(CircleN)} || I <- lists:seq(1, ListN)],
    GeoHashes = [{I, erl_geohash:geo_radiuses_hashes(Circles, 20)} || {I, Circles} <- CirclesList],
    Index = erl_geohash:async_build_index(CirclesList, 20),
    Term = erl_geohash:index_to_term(Index),

    visualize(
        [Circles || {_, Circles} <- CirclesList],
        [erl_geohash:hashes_to_rectangles(Hashes) || {_, Hashes} <- GeoHashes]
    ),

    print_index(Term),
    ok.

print_index({[], Keys}) ->
    lists:foreach(fun (Key) -> print_key(Key, 0) end, Keys).

print_key({Prefix, {Values, NextKeys}}, N) ->
    Binary = prefix_to_binary(Prefix),
    Padding = [32 || _X <- lists:seq(1, N)],
    io:format("~s~s ~p ~p~n",[Padding, Binary, element(2, Prefix), Values]),
    lists:foreach(fun (Key) -> print_key(Key, N+1) end, NextKeys).

point_in_circles({PointLat, PointLon}, Circles) ->
    point_in_circles(PointLat, PointLon, Circles).

point_in_circles(PointLat, PointLon, [{CircleLat, CircleLon, CircleRadius} | Rest]) ->
    case erl_geohash:point_in_circle(PointLat, PointLon, CircleLat, CircleLon, CircleRadius) of
        false -> point_in_circles(PointLat, PointLon, Rest);
        true -> true
    end;
point_in_circles(_, _, []) ->
    false.

points_in_hashes(Points, Hashes) ->
    [erl_geohash:point_in_hashes(Point, Hashes) || Point <- Points].

hashes_to_term(Hashes) ->
    Lst = erl_geohash:hashes_to_term(Hashes),
    [prefix_to_binary(Prefix) || Prefix <- Lst].

prefix_to_binary({Value, Offset}) ->
    prefix_to_binary(Value bsr Offset, 64 - Offset, []).

prefix_to_binary(_, 0, Result) ->
    Result;
prefix_to_binary(Value, I, Result) ->
    Char = case Value band 1 of
               0 -> $0;
               1 -> $1
           end,
    prefix_to_binary(Value bsr 1, I - 1, [Char | Result]).
