%
% httpgps.erl
%

-module(httpgps).

-include("../../include/header.hrl").

-export([http_gps_deamon/7]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
%       Convert GPS to address
% Parameter :
%       InitialIPPort   : 
%       State           : 
%       Count           : 
%       ACount          : 
%       FCount          : 
%       FACount         : 
%       DispLog         : 
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
http_gps_deamon(InitialIPPort, State, Count, ACount, FCount, FACount, DispLog) ->
    receive
        {Pid, normal, Request} ->
            [LonReq, LatReq] = Request,
            case convert_request(Request) of
                {ok, SRequest} ->
                    FullRequest = lists:append(["http://", 
                                               InitialIPPort, 
                                               "/coordinate/simple?sid=15001&xys=", 
                                               SRequest, 
                                               "&resType=xml&rid=123&key=1831beb01605f760589221fdd6f2cdfb7412a767dbc0f004854457f59fb16ab863a3a1722cef553f"]),
                    log:loginfo("Normal Position : ~p", [FullRequest], DispLog)
                    case State of
                        inited ->
                            try
                                case httpc:request(FullRequest) of
                                    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
                                        BodyB = list_to_binary(Body),
                                        BinParts = binary:split(BodyB, [<<"<xys>">>, <<"</xys>">>], [global]),
                                        case length(BinParts) of
                                            3 ->
                                                [_, LonLatBinS, _] = BinParts,
                                                LonLatBinL = binary:split(LonLatBinS, [<<",">>], [global]),
                                                case length(LonLatBinL) of
                                                    2 ->
                                                        [LonBin, LatBin] = LonLatBinL,
                                                        try
                                                            Lon = erlang:binary_to_float(LonBin),
                                                            Lat = erlang:binary_to_float(LatBin),
                                                            Request2 = [Lon, Lat],
                                                            case convertrequest(Request2) of
                                                                {ok, SRequest2} ->
                                                                    FullRequest2 = lists:append(["http://",
                                                                                                InitialIPPort, 
                                                                                                "/rgeocode/simple?sid=7001&region=", 
                                                                                                SRequest2, 
                                                                                                "&poinum=1&range=3000&encode=UTF-8&resType=json&rid=$rid&roadnum=1&crossnum=0&show_near_districts=true&key=1831beb01605f760589221fdd6f2cdfb7412a767dbc0f004854457f59fb16ab863a3a1722cef553f"]),
                                                                    log:loginfo("Normal Address : ~p", [FullRequest2], DispLog)
                                                                    try
                                                                        case httpc:request(FullRequest2) of
                                                                            {ok, {{_Version2, 200, _ReasonPhrase2}, _Headers2, Body2}} ->
                                                                                BodyB2 = list_to_binary(Body2),
                                                                                FullAddrBin2 = get_full_address(BodyB2),
                                                                                case FullAddrBin2 of
                                                                                    <<>> ->
                                                                                        Pid ! [Lon, Lat, []];
                                                                                    _ ->
                                                                                        FullAddr = binary_to_list(FullAddrBin2),
                                                                                        Pid ! [Lon, Lat, FullAddr]
                                                                                end,
                                                                                http_gps_deamon(InitialIPPort, State, Count+1, ACount, FCount, FACount, DispLog);
                                                                            {error, Reason2} ->
                                                                                log:loginfo("HTTP GPS address request fails : ~p", [Reason2], DispLog),
                                                                                Pid ! [Lon, Lat, []],
                                                                                http_gps_deamon(InitialIPPort, State, Count, ACount, FCount+1, FACount, DispLog)
                                                                        end
                                                                    catch
                                                                        Oper2:ExReason2 ->
                                                                            log:loginfo("HTTP GPS address request exception : (~p) ~p", [Oper2, ExReason2], DispLog),
                                                                            Pid ! [Lon, Lat, []],
                                                                            http_gps_deamon(InitialIPPort, State, Count, ACount, FCount+1, FACount, DispLog)
                                                                    end;
                                                                error ->
                                                                    log:loginfo("HTTP GPS address request fails because of conversion error", DispLog),
                                                                    Pid ! [Lon, Lat, []],
                                                                    http_gps_deamon(InitialIPPort, State, Count, ACount, FCount+1, FACount, DispLog)
                                                            end
                                                        catch
                                                            _:_ ->
                                                                log:loginfo("HTTP GPS request fails : cannot convert longitude and latitude ~p", [Body], DispLog),
                                                                Pid ! [LonReq, LatReq, []],
                                                                http_gps_deamon(InitialIPPort, State, Count, ACount, FCount+1, FACount, DispLog)
                                                        end;
                                                    _ ->
                                                        log:loginfo("HTTP GPS request fails : cannot convert longitude/latitude ~p", [Body], DispLog),
                                                        Pid ! [LonReq, LatReq, []],
                                                        http_gps_deamon(InitialIPPort, State, Count, ACount, FCount+1, FACount, DispLog)
                                                end;
                                            _ ->
                                                log:loginfo("HTTP GPS request fails : response error ~p", [Body], DispLog),
                                                Pid ! [LonReq, LatReq, []],
                                                http_gps_deamon(InitialIPPort, State, Count, ACount, FCount+1, FACount, DispLog)
                                        end;
                                    {error, Reason} ->
                                        log:loginfo("HTTP GPS request fails : ~p", [Reason], DispLog),
                                        Pid ! [LonReq, LatReq, []],
                                        http_gps_deamon(InitialIPPort, State, Count, ACount, FCount+1, FACount, DispLog)
                                end
                            catch
                                Oper:ExReason ->
                                    log:loginfo("HTTP GPS request exception : (~p) ~p", [Oper, ExReason], DispLog),
                                    Pid ! [LonReq, LatReq, []],
                                    http_gps_deamon(InitialIPPort, State, Count, ACount, FCount+1, FACount, DispLog)
                            end;
                        uninit ->
                            log:loginfo("HTTP GPS request fails because of uninit state", DispLog),
                            Pid ! [LonReq, LatReq, []],
                            http_gps_deamon(InitialIPPort, State, Count, ACount, FCount+1, FACount, DispLog);
                        _ ->
                            log:loginfo("HTTP GPS request fails because of unknown state", DispLog),
                            Pid ! [LonReq, LatReq, []],
                            http_gps_deamon(InitialIPPort, State, Count, ACount, FCount+1, FACount, DispLog)
                    end;
                error ->
                    log:loginfo("HTTP GPS request fails because of unknown state", DispLog),
                    Pid ! [LonReq, LatReq, []],
                    http_gps_deamon(InitialIPPort, State, Count, ACount, FCount+1, FACount, DispLog)
            end;
        {Pid, abnormal, Request} ->
            [LonReq, LatReq] = Request,
            case convertrequest(Request) of
                {ok, SRequest} ->
                    FullRequest = lists:append(["http://", 
                                               InitialIPPort, 
                                               "/rgeocode/simple?sid=7001&region=", 
                                               SRequest, 
                                               "&poinum=1&range=3000&encode=UTF-8&resType=json&rid=$rid&roadnum=1&crossnum=0&show_near_districts=true&key=1831beb01605f760589221fdd6f2cdfb7412a767dbc0f004854457f59fb16ab863a3a1722cef553f"]),
                    log:loginfo("Abnormal Address : ~p", [FullRequest], DispLog),
                    case State of
                        inited ->
                            try
                                case httpc:request(FullRequest) of
                                    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
                                        BodyB = list_to_binary(Body),
                                        FullAddrBin = get_full_address(BodyB),
                                        case FullAddrBin of
                                            <<>> ->
                                                Pid ! [LonReq, LatReq, []];
                                            _ ->
                                                FullAddr = binary_to_list(FullAddrBin),
                                                Pid ! [LonReq, LatReq, FullAddr]
                                        end,
                                        http_gps_deamon(InitialIPPort, State, Count, ACount+1, FCount, FACount, DispLog);
                                    {error, Reason} ->
                                        log:loginfo("HTTP GPS address request fails : ~p", [Reason], DispLog),
                                        Pid ! [LonReq, LatReq, []],
                                        http_gps_deamon(InitialIPPort, State, Count, ACount, FCount, FACount+1, DispLog)
                                end
                            catch
                                Oper:ExReason ->
                                    log:loginfo("HTTP GPS address request exception : (~p) ~p", [Oper, ExReason]),
                                    Pid ! [LonReq, LatReq, []],
                                    http_gps_deamon(InitialIPPort, State, Count, ACount, FCount, FACount+1, DispLog)
                            end;
                        uninit ->
                            log:loginfo("HTTP GPS address request fails because of uninit state", DispLog),
                            Pid ! [LonReq, LatReq, []],
                            http_gps_deamon(InitialIPPort, State, Count, ACount, FCount, FACount+1, DispLog);
                        _ ->
                            log:loginfo("HTTP GPS request fails because of unknown state", DispLog),
                            Pid ! [LonReq, LatReq, []],
                            http_gps_deamon(InitialIPPort, State, Count, ACount, FCount, FACount+1, DispLog)
                    end;
                error ->
                    log:loginfo("HTTP GPS address request fails because of conversion error", DispLog),
                    Pid ! [LonReq, LatReq, []],
                    http_gps_deamon(InitialIPPort, State, Count, ACount, FCount, FACount+1, DispLog)
            end;
        {server, IPPort} ->
            % Need restart?
            http_gps_deamon(IPPort, State, Count, ACount, FCount, FACount, DispLog, DispLog);
        enablelog ->
            http_gps_deamon(InitialIPPort, State, Count, ACount, FCount, FACount, DispLog, 1);
        disablelog ->
            http_gps_deamon(InitialIPPort, State, Count, ACount, FCount, FACount, DispLog, 0);
        init ->
            case State of
                uninit ->
                    case inets:start() of
                        ok ->
                            http_gps_deamon(InitialIPPort, inited, 0, 0, 0, 0);
                        {error, Reason} ->
                            log:loginfo("Cannot start HTTP GPS inets : ~p", [Reason], DispLog),
                            http_gps_deamon(InitialIPPort, State, Count, ACount, FCount, FACount, DispLog)
                    end;
                inited ->
                    log:loginfo("HTTP GPS inets already inited for init command", DispLog),
                    http_gps_deamon(InitialIPPort, State, Count, ACount, FCount, FACount, DispLog);
                _ ->
                    log:loginfo("HTTP GPS inets unknown state for init command", DispLog),
                    http_gps_deamon(InitialIPPort, State, Count, ACount, FCount, FACount, DispLog)
            end;
        release ->
            case State of
                uninit ->
                    log:loginfo("HTTP GPS inets already uninit for release command", DispLog),
                    http_gps_deamon(InitialIPPort, State, Count, ACount, FCount, FACount, DispLog);
                inited ->
                    inets:stop(),
                    http_gps_deamon(InitialIPPort, uninit, Count, ACount, FCount, FACount, DispLog);
                _ ->
                    log:loginfo("HTTP GPS inets unknwon state for release command", DispLog),
                    http_gps_deamon(InitialIPPort, State, Count, ACount, FCount, FACount, DispLog)
            end;
        stop ->
            case State of
                uninit ->
                    log:loginfo("HTTP GPS inets already uninit for stop command", DispLog);
                inited ->
                    inets:stop();
                _ ->
                    log:loginfo("HTTP GPS inets unknwon state for stop command", DispLog)
            end;
        {Pid, get} ->
            Pid ! {InitialIPPort, State, Count, ACount, FCount, FACount},
            http_gps_deamon(InitialIPPort, State, Count, ACount, FCount, FACount, DispLog);
        _ ->
            log:loginfo("HTTP GPS process receive unknown msg.", DispLog),
            http_gps_deamon(InitialIPPort, State, Count, ACount, FCount, FACount, DispLog)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Body is a binary list
% Return binary address
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_full_address(Body, DispLog) ->
    log:loginfo("Body : ~p", [Body], DispLog)
    Province = get_province_name(Body),
    log:loginfo("Province : ~p", [Province], DispLog),
    City = get_city_name(Body),
    log:loginfo("City : ~p", [City], DispLog),
    District = get_district_name(Body),
    log:loginfo("District : ~p", [District], DispLog),
    Road = get_road_name(Body),
    log:loginfo("Road : ~p", [Road], DispLog),
    BuildingAddress = get_building_address(Body),
    log:loginfo("BuildingAddress : ~p", [BuildingAddress], DispLog),
    BuildingName = get_building_name(Body),
    log:loginfo("BuildingName : ~p", [BuildingName], DispLog),
    BuildingDirection = get_building_direction(Body),
    log:loginfo("BuildingDirection : ~p", [BuildingDirection], DispLog),
    BuildingDistance = get_building_distance(Body),
    log:loginfo("BuildingDistance : ~p", [BuildingDistance], DispLog),
    if
        BuildingDirection == <<"">> orelse BuildingDistance == <<"">> ->
            Address = list_to_binary([Province, City, District, Road, BuildingAddress, BuildingName]),
            Address1 = binary:replace(Address, <<"(">>, <<"[">>, [global]),
            Address2 = binary:replace(Address1, <<")">>, <<"]">>, [global]),
            log:loginfo("Address : (Binary ~p, List ~p) ~p", [is_binary(Address2), is_list(Address2), Address2], DispLog)
            Address2;
        true ->
            Address = list_to_binary([Province, City, District, Road, BuildingAddress, BuildingName, BuildingDirection, BuildingDistance]),
            Address1 = binary:replace(Address, <<"(">>, <<"[">>, [global]),
            Address2 = binary:replace(Address1, <<")">>, <<"]">>, [global]),
            log:loginfo("Address : (Binary ~p, List ~p) ~p", [is_binary(Address2), is_list(Address2), Address2], DispLog),
            Address2
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
% Parameter :
%       Body    :
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_province_name(Body) ->
    try
        ProvinceBinsCheck = binary:split(Body, [<<"\"province\":{\"name\":\"\"">>], [global]),
        LenCheck = length(ProvinceBinsCheck),
        if
            LenCheck =/= 1 ->
                <<"">>;
            true ->
                ProvinceBins = binary:split(Body, [<<"\"province\":{\"name\":\"">>], [global]),
                Len1 = length(ProvinceBins),
                if
                    Len1 > 1 ->
                        ProvinceInfo = lists:nth(2, ProvinceBins),
                        ProvinceInfoBins = binary:split(ProvinceInfo, [<<"\",\"ename\":\"">>], [global]),
                        Len2 = length(ProvinceInfoBins),
                        if
                            Len2 > 0 ->
                                lists:nth(1, ProvinceInfoBins);
                            true ->
                                <<"">>
                        end;
                    true ->
                        <<"">>
                end
        end
    catch
        _:_ ->
            <<"">>
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
% Parameter :
%       Body    :
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_city_name(Body) ->
    try
        CityBins = binary:split(Body, [<<"\"city\":{\"citycode\":\"">>], [global]),
        Len1 = length(CityBins),
        if
            Len1 > 1 ->
                CityInfo = lists:nth(2, CityBins),
                CityInfoBody = binary:split(CityInfo, [<<"}">>], [global]),
                CityInfoPureBody = lists:nth(1, CityInfoBody),
                CityInfoBinsCheck = binary:split(CityInfoPureBody, [<<"\"name\":\"\"">>], [global]),
                LenCheck = length(CityInfoBinsCheck),
                if
                    LenCheck =/= 1 ->
                        <<"">>;
                    true ->
                        CityInfoBins = binary:split(CityInfo, [<<"\"name\":\"">>, <<"\",\"ename\":\"">>], [global]),
                        Len2 = length(CityInfoBins),
                        if
                            Len2 > 1 ->
                                lists:nth(2, CityInfoBins);
                            true ->
                                <<"">>
                        end
                end;
            true ->
                <<"">>
        end
    catch
        _:_ ->
            <<"">>
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
% Parameter :
%       Body    :
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_district_name(Body) ->
    try
        DistrictBinsCheck = binary:split(Body, [<<"\"district\":{\"name\":\"\"">>], [global]),
        LenCheck = length(DistrictBinsCheck),
        if
            LenCheck =/= 1 ->
                <<"">>;
            true ->
                DistrictBins = binary:split(Body, [<<"\"district\":{\"name\":\"">>], [global]),
                Len1 = length(DistrictBins),
                if
                    Len1 > 1 ->
                        DistrictInfo = lists:nth(2, DistrictBins),
                        DistrictInfoBins = binary:split(DistrictInfo, [<<"\",\"ename\":\"">>], [global]),
                        Len2 = length(DistrictInfoBins),
                        if
                            Len2 > 0 ->
                                lists:nth(1, DistrictInfoBins);
                            true ->
                                <<"">>
                        end;
                    true ->
                        <<"">>
                end
        end
    catch
        _:_ ->
            <<"">>
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
% Parameter :
%       Body    :
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_road_name(Body) ->
    try
        RoadBins = binary:split(Body, [<<"\"roadlist\":[{\"id\":\"">>], [global]),
        Len1 = length(RoadBins),
        if
            Len1 > 1 ->
                RoadInfo = lists:nth(2, RoadBins),
                RoadInfoBody = binary:split(RoadInfo, [<<"}]">>], [global]),
                RoadInfoPureBody = lists:nth(1, RoadInfoBody),
                RoadInfoBinsCheck = binary:split(RoadInfoPureBody, [<<"\"name\":\"\"">>], [global]),
                LenCheck = length(RoadInfoBinsCheck),
                if
                    LenCheck =/= 1->
                        <<"">>;
                    true ->
                        RoadInfoBins = binary:split(RoadInfo, [<<"\"name\":\"">>, <<"\",\"ename\":\"">>], [global]),
                        Len = length(RoadInfoBins),
                        if
                            Len > 1 ->
                                lists:nth(2, RoadInfoBins);
                            true ->
                                <<"">>
                        end
                end;
            true ->
                <<"">>
        end
    catch
        _:_ ->
            <<"">>
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
% Parameter :
%       Body    :
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_building_address(Body) ->
    try
        RoadBins = binary:split(Body, [<<"\"list\":[{\"poilist\":[{\"distance\":\"">>], [global]),
        Len1 = length(RoadBins),
        if
            Len1 > 1 ->
                RoadInfo = lists:nth(2, RoadBins),
                RoadInfoBody = binary:split(RoadInfo, [<<"}]">>], [global]),
                RoadInfoPureBody = lists:nth(1, RoadInfoBody),
                RoadInfoBinsCheck = binary:split(RoadInfoPureBody, [<<"\"address\":\"">>, <<"\",\"direction\":\"">>], [global]),
                LenCheck = length(RoadInfoBinsCheck),
                if
                    LenCheck =/= 3->
                        <<"">>;
                    true ->
                        lists:nth(2, RoadInfoBinsCheck)
                end;
            true ->
                <<"">>
        end
    catch
        _:_ ->
            <<"">>
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
% Parameter :
%       Body    :
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_building_name(Body) ->
    try
        RoadBins = binary:split(Body, [<<"\"list\":[{\"poilist\":[{\"distance\":\"">>], [global]),
        Len1 = length(RoadBins),
        if
            Len1 > 1 ->
                RoadInfo = lists:nth(2, RoadBins),
                RoadInfoBody = binary:split(RoadInfo, [<<"}]">>], [global]),
                RoadInfoPureBody = lists:nth(1, RoadInfoBody),
                RoadInfoBinsCheck = binary:split(RoadInfoPureBody, [<<"\"name\":\"">>, <<"\",\"type\":\"">>], [global]),
                LenCheck = length(RoadInfoBinsCheck),
                if
                    LenCheck =/= 3 ->
                        <<"">>;
                    true ->
                        lists:nth(2, RoadInfoBinsCheck)
                end;
            true ->
                <<"">>
        end
    catch
        _:_ ->
            <<"">>
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
% Parameter :
%       Body    :
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_building_direction(Body) ->
    try
        RoadBins = binary:split(Body, [<<"\"list\":[{\"poilist\":[{\"distance\":\"">>], [global]),
        Len1 = length(RoadBins),
        if
            Len1 > 1 ->
                RoadInfo = lists:nth(2, RoadBins),
                RoadInfoBody = binary:split(RoadInfo, [<<"}]">>], [global]),
                RoadInfoPureBody = lists:nth(1, RoadInfoBody),
                RoadInfoBinsCheck = binary:split(RoadInfoPureBody, [<<"\"direction\":\"">>, <<"\",\"tel\":\"">>], [global]),
                LenCheck = length(RoadInfoBinsCheck),
                if
                    LenCheck =/= 3->
                        <<"">>;
                    true ->
                        Direction = lists:nth(2, RoadInfoBinsCheck),
                        case Direction of
                            <<"East">> ->
                                <<"东">>;
                            <<"South">> ->
                                <<"南">>;
                            <<"West">> ->
                                <<"西">>;
                            <<"North">> ->
                                <<"北">>;
                            <<"SouthEast">> ->
                                <<"东南">>;
                            <<"EastSouth">> ->
                                <<"东南">>;
                            <<"SouthWest">> ->
                                <<"西南">>;
                            <<"WestSouth">> ->
                                <<"西南">>;
                            <<"NorthEast">> ->
                                <<"东北">>;
                            <<"EastNorth">> ->
                                <<"东北">>;
                            <<"NorthWest">> ->
                                <<"西北">>;
                            <<"WestNorth">> ->
                                <<"西北">>;
                            _ ->
                                Direction
                        end
                end;
            true ->
                <<"">>
        end
    catch
        _:_ ->
            <<"">>
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
% Parameter :
%       Body    :
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_building_distance(Body) ->
    try
        RoadBins = binary:split(Body, [<<"\"list\":[{\"poilist\":[{\"distance\":\"">>], [global]),
        Len1 = length(RoadBins),
        if
            Len1 > 1 ->
                RoadInfo = lists:nth(2, RoadBins),
                RoadInfoBody = binary:split(RoadInfo, [<<"}]">>], [global]),
                RoadInfoPureBody = lists:nth(1, RoadInfoBody),
                RoadInfoBinsCheck = binary:split(RoadInfoPureBody, [<<"\",\"typecode\":\"">>], [global]),
                LenCheck = length(RoadInfoBinsCheck),
                if
                    LenCheck =/= 2->
                        <<"">>;
                    true ->
                        Distance = lists:nth(1, RoadInfoBinsCheck),
                        DistanceBins = binary:split(Distance, [<<".">>], [global]),
                        LenDistanceBins = length(DistanceBins),
                        if
                            LenDistanceBins > 1 ->
                                erlang:list_to_binary([lists:nth(1, DistanceBins), <<"米">>]);
                            true ->
                                erlang:list_to_binary([Distance, <<"米">>])
                        end
                end;
            true ->
                <<"">>
        end
    catch
        _:_ ->
            <<"">>
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
% Parameter :
%       Lon : longitude - Jingdu(Chinese)
%       Lat : latitude - Weidu(Chinese)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
convert_request(Request) when is_list(Request),
                             length(Request) == 2 ->
    [Lon, Lat] = Request,
    if
        is_float(SLon) and is_float(SLat) ->
            SLon = erlang:float_to_list(Lon, [{decimals, 6}, compact]),
            SLat = erlang:float_to_list(Lat, [{decimals, 6}, compact]),
            {ok, lists:append([SLon, ",", SLat])};
        true ->
            {ok, "0.0,0.0"};
convertrequest(_Request) ->
    error.
