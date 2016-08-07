%
% msapp.erl
%

-module(msapp).

-behaviour(application).

-include("../include/header.hrl").

-export([start/2, stop/1]).

%host: 42.96.146.34
%user: optimus
%passwd: opt123450
%port:3306
%dbname: gps_database

%cd /home/optimus/innovbackend/gateway/src
%make
%cd ../ebin
%erl -P 655350 -Q 655350
%application:start(sasl).
%application:start(msapp).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start(_StartType, StartArgs) ->
    Len = length(StartArgs),
    if
        Len =:= 2 ->
            startserver(StartArgs);
        true ->
            log:loginfo("Parameter count error : ~p", [Len])
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Parameters:
%   redis   : 1         -> use Redis
%             others    -> doesn't use Redis, for the capacity test
%   HttpGps : 1         -> use HttpGps server
%             others    -> doesn't use HttpGps server
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
startserver(StartArgs) ->
    [UseRedisFlag, UseHttpGpsFlag] = StartArgs,
    ets:new(msgservertable, [set, public, named_table, {keypos, 1}, {read_concurrency, true}, {write_concurrency, true}]),
    ets:insert(msgservertable, {displevel, ?DISP_LEVEL_HINT}),
    ets:insert(msgservertable, {displog, 1}),
    if
        UseRedisFlag =:= 1 ->
            ets:insert(msgservertable, {useredis, 1});
        true ->
            ets:insert(msgservertable, {useredis, 0})
    end,
    [{useredis, UseRedis}] = ets:lookup(msgservertable, useredis),
    if
        UseHttpGpsFlag =:= 1 ->
            ets:insert(msgservertable, {usehttpgps, 1});
        true ->
            ets:insert(msgservertable, {usehttpgps, 0})
    end,
    [{usehttpgps, UseHttpGps}] = ets:lookup(msgservertable, usehttpgps),
    ets:insert(msgservertable, {redispid, undefined}),
    ets:insert(msgservertable, {redisoperationpid, undefined}),
    ets:insert(msgservertable, {apppid, self()}),
    
    % Need revisit
    ets:new(alarmtable,   [bag,         public, named_table, {keypos, #alarmitem.vehicleid},   {read_concurrency, true}, {write_concurrency, true}]),
    ets:new(vdrdbtable,   [ordered_set, public, named_table, {keypos, #vdrdbitem.authencode},  {read_concurrency, true}, {write_concurrency, true}]),
    ets:new(vdrtable,     [ordered_set, public, named_table, {keypos, #vdritem.socket},        {read_concurrency, true}, {write_concurrency, true}]),
    ets:new(mantable,     [set,         public, named_table, {keypos, #manitem.socket},        {read_concurrency, true}, {write_concurrency, true}]),
    ets:new(usertable,    [set,         public, named_table, {keypos, #user.id},               {read_concurrency, true}, {write_concurrency, true}]),
    ets:new(drivertable,  [set,         public, named_table, {keypos, #driverinfo.driverid},   {read_concurrency, true}, {write_concurrency, true}]),
    ets:new(lastpostable, [set,         public, named_table, {keypos, #lastposinfo.vehicleid}, {read_concurrency, true}, {write_concurrency, true}]),
    ets:new(montable,     [set,         public, named_table, {keypos, #monitem.socket},        {read_concurrency, true}, {write_concurrency, true}]),
    log:loginfo("Tables are initialized."),
    
    case file:make_dir(?DEF_LOG_PATH ++ "/log") of
        ok ->
            log:loghint("Successfully create directory ~p", [?DEF_LOG_PATH ++ "/log"]);
        {error, DirEx0} ->
            log:logerr("Cannot create directory ~p : ~p", [?DEF_LOG_PATH ++ "/log", DirEx0])
    end,
    case file:make_dir(?DEF_LOG_PATH ++ "/log/vdr") of
        ok ->
            log:loghint("Successfully create directory ~p", [?DEF_LOG_PATH ++ "/log/vdr"]);
        {error, DirEx1} ->
            log:logerr("Cannot create directory ~p : ~p", [?DEF_LOG_PATH ++ "/log/vdr", DirEx1])
    end,
    case file:make_dir(?DEF_LOG_PATH ++ "/log/redis") of
        ok ->
            log:loghint("Successfully create directory ~p", [?DEF_LOG_PATH ++ "/log/redis"]);
        {error, DirEx2} ->
            log:logerr("Cannot create directory ~p : ~p", [?DEF_LOG_PATH ++ "/log/redis", DirEx2])
    end,
    case file:make_dir(?DEF_LOG_PATH ++ "/media") of
        ok ->
            log:loghint("Successfully create directory ~p", [?DEF_LOG_PATH ++ "/media"]);
        {error, DirEx3} ->
            log:logerr("Cannot create directory ~p : ~p", [?DEF_LOG_PATH ++ "/media", DirEx3])
    end,
    case file:make_dir(?DEF_LOG_PATH ++ "/upgrade") of
        ok ->
            log:loghint("Successfully create directory ~p", [?DEF_LOG_PATH ++ "/upgrade"]);
        {error, DirEx4} ->
            log:logerr("Cannot create directory ~p : ~p", [?DEF_LOG_PATH ++ "/upgrade", DirEx4])
    end,
    log:loginfo("Directories are initialized."),
    
    case supervisor:start_link(mssup, []) of
        {ok, SupPid} ->
            ets:insert(msgservertable, {suppid, SupPid}),
            log:loghint("DMS starts initializing data structures."),
            case receive_redis_init_msg(UseRedis, 0) of
                ok ->
                    LinkInfoPid = spawn(fun() -> connection_info_process(lists:duplicate(?CONN_STAT_INFO_COUNT, 0)) end),
                    ets:insert(msgservertable, {linkinfopid, LinkInfoPid}),
                    
                    CCPid = spawn(fun() -> ccprocessor:code_convertor_process() end),
                    ets:insert(msgservertable, {ccpid, CCPid}),
                                        
                    VdrTablePid = spawn(fun() -> vdrtable_insert_delete_process() end),
                    ets:insert(msgservertable, {vdrtablepid, VdrTablePid}),
                    
                    DriverTablePid = spawn(fun() -> drivertable_insert_delete_process() end),
                    ets:insert(msgservertable, {drivertablepid, DriverTablePid}),

                    LastPosTablePid = spawn(fun() -> lastpostable_insert_delete_process() end),
                    ets:insert(msgservertable, {lastpostablepid, LastPosTablePid}),

                    VDRLogPid = spawn(fun() -> vdr_log_process([]) end),
                    ets:insert(msgservertable, {vdrlogpid, VDRLogPid}),

                    VDROnlinePid = spawn(fun() -> vdr_online_table_process([], []) end),
                    ets:insert(msgservertable, {vdronlinepid, VDROnlinePid}),
                    
                    case init_vdr_db_table() of
                        {error, Msg0} ->
                            {error, Msg0};
                        ok ->
                            case init_last_pos_table() of
                                {error, Msg1} ->
                                    {error, Msg1};
                                ok ->
                                    case init_driver_table() of
                                        {error, Msg2} ->
                                            {error, Msg2};
                                        ok ->
                                            CCPid ! {self(), create},
                                            receive
                                                created ->
                                                    log:loghint("Code convertor table is created"),
                                                    HttpGpsPid = spawn(fun() -> httpgps:http_gps_deamon(?DEF_HTTPGPS_SERVER, uninit, 0, 0, 0, 0, 1) end),
                                                    ets:insert(msgservertable, {httpgpspid, HttpGpsPid}),
                                                    log:loghint("HTTP GPS process PID is ~p", [HttpGpsPid]),
                                                    case UseHttpGps of
                                                        1 ->
                                                            HttpGpsPid ! init;
                                                        _ ->
                                                            ok
                                                    end,
                                                    {ok, self()}
                                                after ?TIMEOUT_CC_INIT ->
                                                    {error, "ERROR : code convertor table is timeout"}
                                            end
                                    end
                            end
                    end;
                {error, RedisError} ->
                    {error, "Message server fails to start : " ++ RedisError}                  
            end;
        ignore ->
            log:logerr("Message server fails to start : ignore"),
            ignore;
        {error, Error} ->
            log:logerr("Message server fails to start : ~p", [Error]),
            {error, Error}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Not Implemented Yet!
%
% Description :
%        Initialize VDR table from Redis.
%        Should we also use local cache here even if we use Redis? 
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init_vdr_db_table() ->
    ets:delete_all_objects(vdrdbtable),
    log:loghint("Init vdr db table.").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Not Implemented Yet!
%
% Description :
%        Initialize driver table from Redis.
%        Should we also use local cache here even if we use Redis? 
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init_driver_table() ->
    ets:delete_all_objects(drivertable),
    log:loghint("Init driver table.").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Not Implemented Yet!
%
% Description :
%        Initialize last position table from Redis.
%        Should we also use local cache here even if we use Redis? 
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init_last_pos_table() ->
    ets:delete_all_objects(lastpostable),
    log:loghint("Init last position table.").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
vdr_log_process(VDRList) ->
    receive
        stop ->
            log:loghint("VDR log process stops.");
        reset ->
            vdr_log_process([]);
        {set, VID} ->
            VDRExclList = [C || C <- VDRList, C =/= VID],
            NewVDRList = lists:merge([VDRExclList, [VID]]),
            vdr_log_process(NewVDRList);
        {clear, VID} ->
            VDRExclList = [C || C <- VDRList, C =/= VID],
            vdr_log_process(VDRExclList);
        {Pid, count} ->
            Count = length(VDRList),
            Pid ! {Pid, Count},
            vdr_log_process(VDRList);
        {save, VDRID, FromVDR, MsgBin, DateTime} ->
            VDRInclList = [C || C <- VDRList, C =:= VDRID],
            Len = length(VDRInclList),
            if
                Len =:= 1 ->
                    save_msg_4_vdr(VDRID, FromVDR, MsgBin, DateTime)
            end,
            vdr_log_process(VDRList);
        _ ->
            log:loghint("VDR log process : unknown message"),
            vdr_log_process(VDRList)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
% Parameter :
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
save_msg_4_vdr(VDRID, FromVDR, MsgBin, DateTime) ->
    if
        VDRID =/= undefined ->
            File = "/tmp/log/vdr/VDR" ++ integer_to_list(VDRID) ++ ".log",
            case file:open(File, [append]) of
                {ok, IOFile} ->
                    {Year,Month,Day,Hour,Min,Second} = DateTime,
                    case FromVDR of
                        true ->
                            io:format(IOFile, "(~p ~p ~p, ~p:~p:~p) VDR=> ~p~n", [Year,Month,Day,Hour,Min,Second,MsgBin]);
                        _ ->
                            io:format(IOFile, "(~p ~p ~p, ~p:~p:~p) =>VDR ~p~n", [Year,Month,Day,Hour,Min,Second,MsgBin])
                    end,
                    file:close(IOFile);
                {error, Reason} ->
                    log:logerr("Cannot open ~p : ~p", [File, Reason]);
                _ ->
                    log:logerr("Cannot open ~p : unknown", [File])
            end;
        true ->
            ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
%       This table records the VDR online/offline time
% Parameters :
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
vdr_online_table_process(VDROnlineList, VDROfflineList) ->
    receive
        stop ->
            log:loghint("VDR online process stops.");
        reset ->
            vdr_online_table_process([], []);
        {add, VID, DateTime} ->
            ListExclVID = [{VDRID, DTList}      || {VDRID, DTList}      <- VDROnlineList, VDRID =/= VID],
            ListInclVID = [{VDRID0, DTList0}    || {VDRID0, DTList0}    <- VDROnlineList, VDRID0 =:= VID],
            Length = length(ListInclVID),
            if
                Length =:= 1 ->
                    [{VDRID1, DTList1}] = ListInclVID,
                    NewDTList = lists:merge([DTList1, [DateTime]]),
                    ListNew = lists:merge([ListExclVID, [{VDRID1, NewDTList}]]),
                    vdr_online_table_process(ListNew, VDROfflineList);
                true ->
                    ListNew = lists:merge([ListExclVID, [{VID, [DateTime]}]]),
                    vdr_online_table_process(ListNew, VDROfflineList)
            end;
        {addoff, VID, DateTime} ->
            ListExclVID = [{VDRID, DTList}      || {VDRID, DTList}      <- VDROfflineList, VDRID =/= VID],
            ListInclVID = [{VDRID0, DTList0}    || {VDRID0, DTList0}    <- VDROfflineList, VDRID0 =:= VID],
            Length = length(ListInclVID),
            if
                Length =:= 1 ->
                    [{VDRID1, DTList1}] = ListInclVID,
                    NewDTList = lists:merge([DTList1, [DateTime]]),
                    ListNew = lists:merge([ListExclVID, [{VDRID1, NewDTList}]]),
                    vdr_online_table_process(VDROnlineList, ListNew);
                true ->
                    ListNew = lists:merge([ListExclVID, [{VID, [DateTime]}]]),
                    vdr_online_table_process(VDROnlineList, ListNew)
            end;
        {Pid, count} ->
            CountOn = length(VDROnlineList),
            CountOff = length(VDROfflineList),
            Pid ! {Pid, {CountOn, CountOff}};
        {Pid, count, VID} ->
            ListInclVIDOn = [{VDRID, DTList} || {VDRID, DTList} <- VDROnlineList, VDRID =:= VID],
            ListInclVIDOff = [{VDRID0, DTList0} || {VDRID0, DTList0} <- VDROfflineList, VDRID0 =:= VID],
            CountOn = length(ListInclVIDOn),
            CountOff = length(ListInclVIDOff),
            Pid ! {Pid, {CountOn, CountOff}},
            vdr_online_table_process(VDROnlineList, VDROfflineList);
        {Pid, get, VID} ->
            ListInclVIDOn = [{VDRID0, DTList0} || {VDRID0, DTList0} <- VDROnlineList, VDRID0 =:= VID],
            LengthOn = length(ListInclVIDOn),
            ListInclVIDOff = [{VDRID0, DTList0} || {VDRID0, DTList0} <- VDROnlineList, VDRID0 =:= VID],
            LengthOff = length(ListInclVIDOff),
            if
                LengthOn =:= 1, LengthOff =:= 1 ->
                    [{_VDRID1, DTList1}] = ListInclVIDOn,
                    [{_VDRID2, DTList2}] = ListInclVIDOff,
                    Pid ! {Pid, {DTList1, DTList2}},
                    vdr_online_table_process(VDROnlineList, VDROfflineList);
                LengthOn =:= 1, LengthOff =/= 1 ->
                    [{_VDRID1, DTList1}] = ListInclVIDOn,
                    ListExclVIDOff = [{VDRID, DTList} || {VDRID, DTList} <- VDROfflineList, VDRID =/= VID],
                    Pid ! {Pid, {DTList1, []}},
                    vdr_online_table_process(VDROnlineList, ListExclVIDOff);
                LengthOn =/= 1, LengthOff =:= 1 ->
                    ListExclVIDOn = [{VDRID, DTList} || {VDRID, DTList} <- VDROnlineList, VDRID =/= VID],
                    [{_VDRID2, DTList2}] = ListInclVIDOff,
                    Pid ! {Pid, {[], DTList2}},
                    vdr_online_table_process(ListExclVIDOn, VDROfflineList);
                true ->
                    ListExclVIDOn = [{VDRID, DTList} || {VDRID, DTList} <- VDROnlineList, VDRID =/= VID],
                    ListExclVIDOff = [{VDRID, DTList} || {VDRID, DTList} <- VDROfflineList, VDRID =/= VID],
                    Pid ! {Pid, {[], []}},
                    vdr_online_table_process(ListExclVIDOn, ListExclVIDOff)
            end;
        {clear, VID} ->
            ListExclVIDOn = [{VDRID, DTList} || {VDRID, DTList} <- VDROnlineList, VDRID =/= VID],
            ListExclVIDOff = [{VDRID, DTList} || {VDRID, DTList} <- VDROfflineList, VDRID =/= VID],
            vdr_online_table_process(ListExclVIDOn, ListExclVIDOff);
        _ ->
            log:loghint("VDR online process : unknown message"),
            vdr_online_table_process(VDROnlineList, VDROfflineList)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
% Parameters :
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
vdrtable_insert_delete_process() ->
    receive
        stop ->
            log:loghint("VDR table insert/delete process stops.");
        {insert, Object} ->
            ets:insert(vdrtable, Object),
            vdrtable_insert_delete_process();
        {delete, Key} ->
            ets:delete(vdrtable, Key),
            vdrtable_insert_delete_process();
        {Pid, count} ->
            Count = ets:info(vdrtable, size),
            Pid ! {Pid, Count},
            vdrtable_insert_delete_process();
        {Pid, lookup, Key} ->
            Res = ets:lookup(vdrtable, Key),
            Pid ! {Pid, Res},
            vdrtable_insert_delete_process();
        _ ->
            log:loghint("VDR table insert/delete process receive unknown msg."),
            vdrtable_insert_delete_process()
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
% Parameters :
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
drivertable_insert_delete_process() ->
    receive
        stop ->
            log:loghint("Driver table insert/delete process stops.");
        {chkinsdriverinfo, {DriverID, LicNo, CertCode, VDRAuthCode}} ->        % Check and insert
            if
                DriverID =:= undefined ->
                    log:loghint("Cannot get driver item by driver undefined id");
                true ->
                    DriverInfos = ets:match(drivertable, {'$1', DriverID, '_', '_', '_'}),
                    DriverInfosCount = length(DriverInfos),
                    if
                        DriverInfosCount =:= 0 orelse DriverInfosCount =:= 1 ->
                            DriverInfoItem = #driverinfo{driverid=DriverID, licno=LicNo, certcode=CertCode, vdrauthcode=VDRAuthCode},
                            log:lognone("Insert new driver item : ~p", [DriverInfoItem]),
                            ets:insert(drivertable, DriverInfoItem);
                        true ->
                            ets:delete(drivertable, DriverID),
                            DriverInfoItem = #driverinfo{driverid=DriverID, licno=LicNo, certcode=CertCode, vdrauthcode=VDRAuthCode},
                            log:lognone("Get ~p driver item by driver id ~p and re-create a new driver item : ~p", [DriverInfosCount, DriverID, DriverInfoItem]),
                            ets:insert(drivertable, DriverInfoItem)
                    end
            end,
            drivertable_insert_delete_process();
        {offwork, CertCode} ->
            DriverInfos = ets:match(drivertable, {'_', '$1', '$2', CertCode, '_'}),
            DriverInfosCount = length(DriverInfos),
            if
                DriverInfosCount =:= 1 ->
                    [[DriverID, LicNo]] = DriverInfos,
                    DriverInfoItem = #driverinfo{driverid=DriverID, licno=LicNo, certcode=CertCode},
                    ets:insert(drivertable, DriverInfoItem);
                true ->
                    log:lognone("Get ~p driver item by certificate_code ~p", [DriverInfosCount, CertCode])
            end,                    
            drivertable_insert_delete_process();
        {Pid, checkcc, {CertCode, VDRAuthCode}} ->        % CertCode must be binary
            DriverInfos = ets:match(drivertable, {'_', '$1', '$2', CertCode, '_'}),
            DriverInfosCount = length(DriverInfos),
            if
                DriverInfosCount =:= 1 ->
                    [[DriverID, LicNoRec]] = DriverInfos,
                    DriverInfoItem = #driverinfo{driverid=DriverID, licno=LicNoRec, certcode=CertCode, vdrauthcode=VDRAuthCode},
                    %log:loginfo("Change driver item online state : ~p", [DriverInfoItem]),
                    ets:insert(drivertable, DriverInfoItem),
                    Pid ! {Pid, {DriverInfosCount, DriverID}};
                true ->
                    log:lognone("Get ~p driver item by certificate_code ~p", [DriverInfosCount, CertCode]),
                    Pid ! {Pid, {DriverInfosCount, undefined}}
            end,
            drivertable_insert_delete_process();
        {Pid, getccbyvdr, VDRAuthCode} ->
            DriverInfos = ets:match(drivertable, {'_', '_', '_', '$1', VDRAuthCode}),
            DriverInfosCount = length(DriverInfos),
            if
                DriverInfosCount =:= 1 ->
                    [[CertCodeBin]] = DriverInfos,
                    if
                        CertCodeBin =:= undefined ->
                            Pid ! {Pid, <<"">>};
                        true ->
                            Pid ! {Pid, CertCodeBin}
                    end;
                true ->
                    log:lognone("Get ~p certificate code by vdr_auth_code ~p", [DriverInfosCount, VDRAuthCode]),
                    Pid ! {Pid, <<"">>}
            end,
            drivertable_insert_delete_process();
        {Pid, count} ->
            Count = ets:info(drivertable,size),
            Pid ! {Pid, Count},
            drivertable_insert_delete_process();
        _ ->
            log:loghint("Driver table insert/delete process receive unknown msg."),
            drivertable_insert_delete_process()
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
% Parameters :
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lastpostable_insert_delete_process() ->
    receive
        stop ->
            log:loghint("Last pos table insert/delete process stops.");
        {Pid, get, VID} ->
            Infos = ets:match(lastpostable, {'_', VID, '$1', '$2'}),
            InfoCount = length(Infos),
            if
                InfoCount =:= 1 ->    
                    [[Lon, Lat]] = Infos,
                    Pid ! {Pid, [Lon, Lat]};
                true ->
                    Pid ! {Pid, [0.0, 0.0]}
            end,
            lastpostable_insert_delete_process();
        {_Pid, set, Info} ->
            [VID, Lon, Lat] = Info,
            Infos = ets:match(lastpostable, {'$1', VID, '_', '_'}),
            InfoCount = length(Infos),
            if
                InfoCount =:= 0 ->    
                    LastPosItem = #lastposinfo{vehicleid=VID,longitude=Lon,latitude=Lat},
                    ets:insert(lastpostable, LastPosItem);
                true ->
                    ok
            end,
            lastpostable_insert_delete_process();
        {Pid, count} ->
            Count = ets:info(lastpostable,size),
            Pid ! {Pid, Count},
            lastpostable_insert_delete_process();
        _ ->
            log:loghint("Last pos table insert/delete process receive unknown msg."),
            lastpostable_insert_delete_process()
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
% Parameters :
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
stop(_State) ->
    [{vdrtablepid, VdrTablePid}] = ets:lookup(msgservertable, vdrtablepid),
    case VdrTablePid of
        undefined ->
            ok;
        _ ->
            VdrTablePid ! stop
    end,
    [{linkinfopid, LinkInfoPid}] = ets:lookup(msgservertable, linkinfopid),
    case LinkInfoPid of
        undefined ->
            ok;
        _ ->
            LinkInfoPid ! stop
    end,
    [{ccpid, CCPid}] = ets:lookup(msgservertable, ccpid),
    case CCPid of
        undefined ->
            ok;
        _ ->
            CCPid ! stop
    end,
    [{drivertablepid, DriverTablePid}] = ets:lookup(msgservertable, drivertablepid),
    case DriverTablePid of
        undefined ->
            ok;
        _ ->
            DriverTablePid ! stop
    end,
    [{lastpostablepid, LastPosTablePid}] = ets:lookup(msgservertable, lastpostablepid),
    case LastPosTablePid of
        undefined ->
            ok;
        _ ->
            DriverTablePid ! stop
    end,
    [{httpgpspid, HttpGpsPid}] = ets:lookup(msgservertable, httpgpspid),
    case HttpGpsPid of
        undefined ->
            ok;
        _ ->
            HttpGpsPid ! stop
    end,
    [{vdrlogpid, VDRLogPid}] = ets:lookup(msgservertable, vdrlogpid),
    case VDRLogPid of
        undefined ->
            ok;
        _ ->
            VDRLogPid ! stop
    end,
    [{vdronlinepid, VDROnlinePid}] = ets:lookup(msgservertable, vdronlinepid),
    case VDROnlinePid of
        undefined ->
            ok;
        _ ->
            VDROnlinePid ! stop
    end,
    log:loghint("Message server stops.").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
%        Maintain the connection status information
%       Please refer to include\header.hrl for details
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
connection_info_process(List) ->
    Len = length(List),
    if
        Len =:= ?CONN_STAT_INFO_COUNT ->
            receive
                stop ->
                    ok;
                clear ->
                    connection_info_process(lists:duplicate(?CONN_STAT_INFO_COUNT, 0));
                {clear, ClearIndex} ->
                    connection_info_process(lists:sublist(List, ClearIndex - 1) ++ [0] ++ lists:nthtail(ClearIndex + 1, List));
                {Pid, count} ->
                    Pid ! List;
                {record, Index} ->
                    connection_info_process(lists:sublist(List, Index - 1) ++ [lists:nth(Index, List) + 1] ++ lists:nthtail(Index + 1, List));
                _ ->
                    connection_info_process(List)
            end;
        true ->
            connection_info_process(lists:duplicate(?CONN_STAT_INFO_COUNT, 0))
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
%       Will wait 20s for establishing the Redis connection
% Parameter :
%       UseRedis    : 1 -> use Redis
%                     0 -> not use Redis
%       Count       : 
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
receive_redis_init_msg(UseRedis, Count) ->
    if
        UseRedis =:= 1 ->
            if
                Count > 20 ->
                    {error, "Redis is not ready"};
                true ->
                    receive
                        {"Redis", redisok} ->
                            ok
                    after ?WAIT_LOOP_INTERVAL ->
                            receive_redis_init_msg(UseRedis, Count+1)
                    end
            end;
        true ->
            ok
    end.

