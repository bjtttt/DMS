%
% httpgps.erl
%

-module(log).

-include("../../include/header.hrl").

-export([http_gps_deamon/7]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
%       Convert GPS
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
            case convertrequest(Request) of
                {ok, SRequest} ->
                    FullRequest = lists:append(["http://", 
                                               InitialIPPort, 
                                               "/coordinate/simple?sid=15001&xys=", 
                                               SRequest, 
                                               "&resType=xml&rid=123&key=1831beb01605f760589221fdd6f2cdfb7412a767dbc0f004854457f59fb16ab863a3a1722cef553f"]),
                    %log:loginfo("Normal Position : ~p", [FullRequest]),
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
                                                                    %log:loginfo("Normal Address : ~p", [FullRequest2]),
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
                                                                                http_gps_deamon(InitialIPPort, State, Count+1, ACount, FCount, FACount);
                                                                            {error, Reason2} ->
                                                                                log:loginfo("HTTP GPS address request fails : ~p", [Reason2]),
                                                                                Pid ! [Lon, Lat, []],
                                                                                http_gps_deamon(InitialIPPort, State, Count, ACount, FCount+1, FACount)
                                                                        end
                                                                    catch
                                                                        Oper2:ExReason2 ->
                                                                            log:loginfo("HTTP GPS address request exception : (~p) ~p", [Oper2, ExReason2]),
                                                                            Pid ! [Lon, Lat, []],
                                                                            http_gps_deamon(InitialIPPort, State, Count, ACount, FCount+1, FACount)
                                                                    end;
                                                                error ->
                                                                    log:loginfo("HTTP GPS address request fails because of conversion error"),
                                                                    Pid ! [Lon, Lat, []],
                                                                    http_gps_deamon(InitialIPPort, State, Count, ACount, FCount+1, FACount)
                                                            end
                                                        catch
                                                            _:_ ->
                                                                log:loginfo("HTTP GPS request fails : cannot convert longitude and latitude ~p", [Body]),
                                                                Pid ! [LonReq, LatReq, []],
                                                                http_gps_deamon(InitialIPPort, State, Count, ACount, FCount+1, FACount)
                                                        end;
                                                    _ ->
                                                        log:loginfo("HTTP GPS request fails : cannot convert longitude/latitude ~p", [Body]),
                                                        Pid ! [LonReq, LatReq, []],
                                                        http_gps_deamon(InitialIPPort, State, Count, ACount, FCount+1, FACount)
                                                end;
                                            _ ->
                                                log:loginfo("HTTP GPS request fails : response error ~p", [Body]),
                                                Pid ! [LonReq, LatReq, []],
                                                http_gps_deamon(InitialIPPort, State, Count, ACount, FCount+1, FACount)
                                        end;
                                    {error, Reason} ->
                                        log:loginfo("HTTP GPS request fails : ~p", [Reason]),
                                        Pid ! [LonReq, LatReq, []],
                                        http_gps_deamon(InitialIPPort, State, Count, ACount, FCount+1, FACount)
                                end
                            catch
                                Oper:ExReason ->
                                    log:loginfo("HTTP GPS request exception : (~p) ~p", [Oper, ExReason]),
                                    Pid ! [LonReq, LatReq, []],
                                    http_gps_deamon(InitialIPPort, State, Count, ACount, FCount+1, FACount)
                            end;
                        uninit ->
                            %log:loginfo("HTTP GPS request fails because of uninit state"),
                            Pid ! [LonReq, LatReq, []],
                            http_gps_deamon(InitialIPPort, State, Count, ACount, FCount+1, FACount);
                        _ ->
                            log:loginfo("HTTP GPS request fails because of unknown state"),
                            Pid ! [LonReq, LatReq, []],
                            http_gps_deamon(InitialIPPort, State, Count, ACount, FCount+1, FACount)
                    end;
                error ->
                    log:loginfo("HTTP GPS request fails because of unknown state"),
                    Pid ! [LonReq, LatReq, []],
                    http_gps_deamon(InitialIPPort, State, Count, ACount, FCount+1, FACount)
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
                    %log:loginfo("Abnormal Address : ~p", [FullRequest]),
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
                                        http_gps_deamon(InitialIPPort, State, Count, ACount+1, FCount, FACount);
                                    {error, Reason} ->
                                        log:loginfo("HTTP GPS address request fails : ~p", [Reason]),
                                        Pid ! [LonReq, LatReq, []],
                                        http_gps_deamon(InitialIPPort, State, Count, ACount, FCount, FACount+1)
                                end
                            catch
                                Oper:ExReason ->
                                    log:loginfo("HTTP GPS address request exception : (~p) ~p", [Oper, ExReason]),
                                    Pid ! [LonReq, LatReq, []],
                                    http_gps_deamon(InitialIPPort, State, Count, ACount, FCount, FACount+1)
                            end;
                        uninit ->
                            %log:loginfo("HTTP GPS address request fails because of uninit state"),
                            Pid ! [LonReq, LatReq, []],
                            http_gps_deamon(InitialIPPort, State, Count, ACount, FCount, FACount+1);
                        _ ->
                            log:loginfo("HTTP GPS request fails because of unknown state"),
                            Pid ! [LonReq, LatReq, []],
                            http_gps_deamon(InitialIPPort, State, Count, ACount, FCount, FACount+1)
                    end;
                error ->
                    log:loginfo("HTTP GPS address request fails because of conversion error"),
                    Pid ! [LonReq, LatReq, []],
                    http_gps_deamon(InitialIPPort, State, Count, ACount, FCount, FACount+1)
            end;
        {server, IPPort} ->
            http_gps_deamon(IPPort, State, Count, ACount, FCount, FACount);
        init ->
            case State of
                uninit ->
                    case inets:start() of
                        ok ->
                            http_gps_deamon(InitialIPPort, inited, 0, 0, 0, 0);
                        {error, Reason} ->
                            log:loginfo("Cannot start HTTP GPS inets : ~p", [Reason]),
                            http_gps_deamon(InitialIPPort, State, Count, ACount, FCount, FACount)
                    end;
                inited ->
                    log:loginfo("HTTP GPS inets already inited for init command"),
                    http_gps_deamon(InitialIPPort, State, Count, ACount, FCount, FACount);
                _ ->
                    log:loginfo("HTTP GPS inets unknown state for init command"),
                    http_gps_deamon(InitialIPPort, State, Count, ACount, FCount, FACount)
            end;
        release ->
            case State of
                uninit ->
                    log:loginfo("HTTP GPS inets already uninit for release command"),
                    http_gps_deamon(InitialIPPort, State, Count, ACount, FCount, FACount);
                inited ->
                    inets:stop(),
                    http_gps_deamon(InitialIPPort, uninit, Count, ACount, FCount, FACount);
                _ ->
                    log:loginfo("HTTP GPS inets unknwon state for release command"),
                    http_gps_deamon(InitialIPPort, State, Count, ACount, FCount, FACount)
            end;
        stop ->
            case State of
                uninit ->
                    log:loginfo("HTTP GPS inets already uninit for stop command");
                inited ->
                    inets:stop();
                _ ->
                    log:loginfo("HTTP GPS inets unknwon state for stop command")
            end;
        {Pid, get} ->
            Pid ! {InitialIPPort, State, Count, ACount, FCount, FACount},
            http_gps_deamon(InitialIPPort, State, Count, ACount, FCount, FACount);
        _ ->
            log:loginfo("HTTP GPS process receive unknown msg."),
            http_gps_deamon(InitialIPPort, State, Count, ACount, FCount, FACount)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Body is a binary list
% Return binary address
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_full_address(Body) ->
    get_full_address(Body, 0).

get_full_address(Body, DispLog) ->
    %log:loginfo("Body : ~p", [Body]),
    Province = get_province_name(Body),
    %log:loginfo("Province : ~p", [Province]),
    City = get_city_name(Body),
    %log:loginfo("City : ~p", [City]),
    District = get_district_name(Body),
    %log:loginfo("District : ~p", [District]),
    Road = get_road_name(Body),
    %log:loginfo("Road : ~p", [Road]),
    BuildingAddress = get_building_address(Body),
    %log:loginfo("BuildingAddress : ~p", [BuildingAddress]),
    BuildingName = get_building_name(Body),
    %log:loginfo("BuildingName : ~p", [BuildingName]),
    BuildingDirection = get_building_direction(Body),
    %log:loginfo("BuildingDirection : ~p", [BuildingDirection]),
    BuildingDistance = get_building_distance(Body),
    %log:loginfo("BuildingDistance : ~p", [BuildingDistance]),
    %list_to_binary([Province, City, District, Road]).
    if
        BuildingDirection == <<"">> orelse BuildingDistance == <<"">> ->
            Address = list_to_binary([Province, City, District, Road, BuildingAddress, BuildingName]),
            Address1 = binary:replace(Address, <<"(">>, <<"[">>, [global]),
            Address2 = binary:replace(Address1, <<")">>, <<"]">>, [global]),
            %log:loginfo("Address : (Binary ~p, List ~p) ~p", [is_binary(Address2), is_list(Address2), Address2]),
            Address2;
        true ->
            Address = list_to_binary([Province, City, District, Road, BuildingAddress, BuildingName, BuildingDirection, BuildingDistance]),
            Address1 = binary:replace(Address, <<"(">>, <<"[">>, [global]),
            Address2 = binary:replace(Address1, <<")">>, <<"]">>, [global]),
            %log:loginfo("Address : (Binary ~p, List ~p) ~p", [is_binary(Address2), is_list(Address2), Address2]),
            Address2
    end.
    %log:loginfo("Address : (Binary ~p, List ~p) ~p", [is_binary(Address), is_list(Address), Address]),
    %Address.
    %CCPid ! {Pid, utf8togbk, Address},
    %receive
    %   AddressNew ->
    %       log:loginfo("New Address : (Binary ~p, List ~p) ~p", [is_binary(AddressNew), is_list(AddressNew), AddressNew]),
    %       list_to_binary(AddressNew)
    %after ?TIMEOUT_CC_PROCESS ->
    %       log:loginfo("Address : (Binary ~p, List ~p) ~p", [is_binary(Address), is_list(Address), Address]),
    %       list_to_binary(Address)
    %end.

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
                                %Province = lists:nth(1, ProvinceInfoBins),
                                %CCPid ! {Pid, utf8togbk, Province},
                                %receive
                                %   ProvinceNew ->
                                %       log:loginfo("Province New : (Binary ~p, List ~p) ~p", [is_binary(ProvinceNew), is_list(ProvinceNew), ProvinceNew]),
                                %       list_to_binary(ProvinceNew)
                                %after ?TIMEOUT_CC_PROCESS ->
                                %       log:loginfo("Province : (Binary ~p, List ~p) ~p", [is_binary(Province), is_list(Province), Province]),
                                %       list_to_binary(Province)
                                %end;
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
                        %log:loginfo("CityInfoBins : (~p)~n~p", [length(CityInfoBins), CityInfoBins]),
                        Len2 = length(CityInfoBins),
                        if
                            Len2 > 1 ->
                                lists:nth(2, CityInfoBins);
                                %City = lists:nth(2, CityInfoBins),
                                %CCPid ! {Pid, utf8togbk, City},
                                %receive
                                %   CityNew ->
                                %       log:loginfo("City New : (Binary ~p, List ~p) ~p", [is_binary(CityNew), is_list(CityNew), CityNew]),
                                %       list_to_binary(CityNew)
                                %after ?TIMEOUT_CC_PROCESS ->
                                %       log:loginfo("City : (Binary ~p, List ~p) ~p", [is_binary(City), is_list(City), City]),
                                %       list_to_binary(City)
                                %end;
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
                                %Dictrict = lists:nth(1, DistrictInfoBins),
                                %CCPid ! {Pid, utf8togbk, Dictrict},
                                %receive
                                %   DictrictNew ->
                                %       log:loginfo("Dictrict New : (Binary ~p, List ~p) ~p", [is_binary(DictrictNew), is_list(DictrictNew), DictrictNew]),
                                %       list_to_binary(DictrictNew)
                                %after ?TIMEOUT_CC_PROCESS ->
                                %       log:loginfo("Dictrict : (Binary ~p, List ~p) ~p", [is_binary(Dictrict), is_list(Dictrict), Dictrict]),
                                %       list_to_binary(Dictrict)
                                %end;
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
                        %log:loginfo("RoadInfoBins : (~p)~n~p", [length(RoadInfoBins), RoadInfoBins]),
                        Len = length(RoadInfoBins),
                        if
                            Len > 1 ->
                                lists:nth(2, RoadInfoBins);
                                %Road = lists:nth(2, RoadInfoBins),
                                %CCPid ! {Pid, utf8togbk, Road},
                                %receive
                                %   RoadNew ->
                                %       log:loginfo("Road New : (Binary ~p, List ~p) ~p", [is_binary(RoadNew), is_list(RoadNew), RoadNew]),
                                %       list_to_binary(RoadNew)
                                %after ?TIMEOUT_CC_PROCESS ->
                                %       log:loginfo("Road : (Binary ~p, List ~p) ~p", [is_binary(Road), is_list(Road), Road]),
                                %       list_to_binary(Road)
                                %end;
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
%       Lon : longitude - Jingdu(Chinese)
%       Lat : latitude - Weidu(Chinese)
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
