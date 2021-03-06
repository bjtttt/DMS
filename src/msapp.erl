%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% msapp.erl
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(msapp).

-behaviour(application).

% This header file is included in "../include/header_struct.hrl"
%-include("../include/header_const.hrl").
-include("../include/header_struct.hrl").

-export([start/2, stop/1]).

%host: 42.96.146.34
%user: optimus
%passwd: opt@123450
%port:3306
%dbname: gps_database

%cd /home/optimus/innovbackend/gateway/src
%make
%cd ../ebin
%erl -P 655350 -Q 655350
%application:start(sasl).
%application:start(msapp).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description:
%
% Parameter:
%       _StartType  :
%       StartArgs   :
% Return:
%       ok.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start(_StartType, StartArgs) ->
    Len = length(StartArgs),
    if
        Len =:= ?START_PARAM_COUN ->
            start_server(StartArgs);
        true ->
            mslog:logerror("Parameter count error : ~p", [Len])
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description:
%
% Parameter:
%       StartArgs   :   [Log, Redis, HttpGps]
% Return:
%       ok.
%
%-----------------------------------------------------------------------------------------
%
%   Log         : 1     -> enable log
%                       -> disable log
%   Redis       : 1     -> use Redis
%                 0     -> doesn't use Redis, for the capacity test
%   HttpGps     : 1     -> use HttpGps server
%                 0     -> doesn't use HttpGps server
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_server(StartArgs) ->
    [Log, Redis, HttpGps] = StartArgs,

    % Log process needs to be created first seperatedly.
    LogPid = spawn(fun() -> log:log_process(#logstate{logenabled=Log}) end),
    ets:insert(msgservertable, {logpid, LogPid}),

    log:log_force_info(LogPid, "Log process () is created.")
    
    ets:new(msgservertable, [set, public, named_table, {keypos, 1}, {read_concurrency, true}, {write_concurrency, true}]),
    if
        Redis =:= 1 ->
            ets:insert(msgservertable, {useredis, true});
        true ->
            ets:insert(msgservertable, {useredis, false})
    end,
    if
        HttpGps =:= 1 ->
            ets:insert(msgservertable, {usehttpgps, true}),
        true ->
            ets:insert(msgservertable, {usehttpgps, false})
    end,
    ets:insert(msgservertable, {redispid, undefined}),
    ets:insert(msgservertable, {redisoperationpid, undefined}),
    ets:insert(msgservertable, {apppid, self()}),
    
    create_tables(LogPid),

    create_directories(LogPid),
    
    create_processes(LogPid, Redis),

    {eredispid, EredisPid} = ets:lookup(msgservertable, eredispid);
    
    case supervisor:start_link(mssup, []) of
        {ok, SupPid} ->
            ets:insert(msgservertable, {suppid, SupPid}),
            mslog:loghint("DMS starts initializing data structures."),
            EredisPid ! {self(), init},
            case receive_redis_init_msg(Redis, EredisPid, 0) of
                {error, RedisError} ->
                    stop(self()),
                    {error, "Message server fails to start : " ++ RedisError};
                Other ->
                    case Other of
                        {EredisPid, error_ok} ->
                            EredisPid ! ok,
                            mslog:loghint("Message server force redis process to be switched to ok.")
                    end,
                    CCPid ! {self(), create},
                    receive
                        created ->
                            mslog:loghint("Code convertor table is created"),
                            HttpGpsPid = spawn(fun() -> httpgps:http_gps_deamon(?DEF_HTTPGPS_SERVER, uninit, 0, 0, 0, 0, 1) end),
                            ets:insert(msgservertable, {httpgpspid, HttpGpsPid}),
                            mslog:loghint("HTTP GPS process PID is ~p", [HttpGpsPid]),
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
            end;
        ignore ->
            mslog:logerr("Message server fails to start : ignore"),
            ignore;
        {error, Error} ->
            mslog:logerr("Message server fails to start : ~p", [Error]),
            {error, Error}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description:
%
% Parameter:
%       LogPid  :
% Return:
%       ok
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create_tables(LogPid) ->
    ets:new(alarmtable,   [bag,         public, named_table, {keypos, #alarmitem.vehicleid},   {read_concurrency, true}, {write_concurrency, true}]),
    ets:new(vdrdbtable,   [ordered_set, public, named_table, {keypos, #vdrdbitem.authencode},  {read_concurrency, true}, {write_concurrency, true}]),
    ets:new(vdrtable,     [ordered_set, public, named_table, {keypos, #vdritem.socket},        {read_concurrency, true}, {write_concurrency, true}]),
    ets:new(mantable,     [set,         public, named_table, {keypos, #manitem.socket},        {read_concurrency, true}, {write_concurrency, true}]),
    ets:new(usertable,    [set,         public, named_table, {keypos, #user.id},               {read_concurrency, true}, {write_concurrency, true}]),
    ets:new(drivertable,  [set,         public, named_table, {keypos, #driverinfo.driverid},   {read_concurrency, true}, {write_concurrency, true}]),
    ets:new(lastpostable, [set,         public, named_table, {keypos, #lastposinfo.vehicleid}, {read_concurrency, true}, {write_concurrency, true}]),
    ets:new(montable,     [set,         public, named_table, {keypos, #monitem.socket},        {read_concurrency, true}, {write_concurrency, true}]),
    log:log_info(LogPid, "Tables are initialized.").    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description:
%
% Parameters:
%       LogPid  :
% Return:
%       ok
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create_directories(LogPid) ->
    case file:make_dir(?DEF_LOG_PATH ++ "/log") of
        ok ->
            log:log_info(LogPid, "Successfully create directory ~p", [?DEF_LOG_PATH ++ "/log"]);
        {error, Reason0} ->
            log:log_err(LogPid, "Cannot create directory ~p : ~p", [?DEF_LOG_PATH ++ "/log", Reason0])
    end,
    
    case file:make_dir(?DEF_LOG_PATH ++ "/log/vdr") of
        ok ->
            log:log_info(LogPid, "Successfully create directory ~p", [?DEF_LOG_PATH ++ "/log/vdr"]);
        {error, Reason1} ->
            log:log_err(LogPid, "Cannot create directory ~p : ~p", [?DEF_LOG_PATH ++ "/log/vdr", Reason1])
    end,
    
    case file:make_dir(?DEF_LOG_PATH ++ "/log/redis") of
        ok ->
            log:log_info(LogPid, "Successfully create directory ~p", [?DEF_LOG_PATH ++ "/log/redis"]);
        {error, Reason2} ->
            log:log_err(LogPid, "Cannot create directory ~p : ~p", [?DEF_LOG_PATH ++ "/log/redis", Reason2])
    end,
    
    case file:make_dir(?DEF_LOG_PATH ++ "/media") of
        ok ->
            log:log_info(LogPid, "Successfully create directory ~p", [?DEF_LOG_PATH ++ "/media"]);
        {error, Reason3} ->
            log:log_err(LogPid, "Cannot create directory ~p : ~p", [?DEF_LOG_PATH ++ "/media", Reason3])
    end,
    
    case file:make_dir(?DEF_LOG_PATH ++ "/upgrade") of
        ok ->
            log:log_info(LogPid, "Successfully create directory ~p", [?DEF_LOG_PATH ++ "/upgrade"]);
        {error, Reason4} ->
            log:log_err(LogPid, "Cannot create directory ~p : ~p", [?DEF_LOG_PATH ++ "/upgrade", Reason4])
    end,
    
    log:loginfo(LogPid, "Directories are initialized.").    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description:
%
% Parameters:
%       LogPid  :   
%       Redis   :   Whether to use redis or not
%                   1       -   use redis
%                   others  -   not use redis
% Return:
%       ok
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create_processes(LogPid, Redis) ->
    ConnInfoPid = spawn(fun() -> conn_info:connection_info_process(lists:duplicate(?CONN_STAT_INFO_COUNT, 0)) end),
    ets:insert(msgservertable, {conninfopid, ConnInfoPid}),
    
    if
        Redis =:= 1->
            EredisPid = spawn(fun() -> eredis_processor:eredis_process(ConnInfoPid) end),
            ets:insert(msgservertable, {eredispid, EredisPid});
        true ->
            ets:insert(msgservertable, {eredispid, undefined})
    end,
    
    CCPid = spawn(fun() -> cc_helper:code_convertor_process(LogPid) end),
    ets:insert(msgservertable, {ccpid, CCPid}),                        
    
    VDRLogPid = spawn(fun() -> log_vdr:vdr_log_process(LogPid, []) end),
    ets:insert(msgservertable, {vdrlogpid, VDRLogPid}).

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
            mslog:loghint("VDR online process stops.");
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
            mslog:loghint("VDR online process : unknown message"),
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
            mslog:loghint("VDR table insert/delete process stops.");
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
            mslog:loghint("VDR table insert/delete process receive unknown msg."),
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
            mslog:loghint("Driver table insert/delete process stops.");
        {chkinsdriverinfo, {DriverID, LicNo, CertCode, VDRAuthCode}} ->        % Check and insert
            if
                DriverID =:= undefined ->
                    mslog:loghint("Cannot get driver item by driver undefined id");
                true ->
                    DriverInfos = ets:match(drivertable, {'$1', DriverID, '_', '_', '_'}),
                    DriverInfosCount = length(DriverInfos),
                    if
                        DriverInfosCount =:= 0 orelse DriverInfosCount =:= 1 ->
                            DriverInfoItem = #driverinfo{driverid=DriverID, licno=LicNo, certcode=CertCode, vdrauthcode=VDRAuthCode},
                            mslog:lognone("Insert new driver item : ~p", [DriverInfoItem]),
                            ets:insert(drivertable, DriverInfoItem);
                        true ->
                            ets:delete(drivertable, DriverID),
                            DriverInfoItem = #driverinfo{driverid=DriverID, licno=LicNo, certcode=CertCode, vdrauthcode=VDRAuthCode},
                            mslog:lognone("Get ~p driver item by driver id ~p and re-create a new driver item : ~p", [DriverInfosCount, DriverID, DriverInfoItem]),
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
                    mslog:lognone("Get ~p driver item by certificate_code ~p", [DriverInfosCount, CertCode])
            end,                    
            drivertable_insert_delete_process();
        {Pid, checkcc, {CertCode, VDRAuthCode}} ->        % CertCode must be binary
            DriverInfos = ets:match(drivertable, {'_', '$1', '$2', CertCode, '_'}),
            DriverInfosCount = length(DriverInfos),
            if
                DriverInfosCount =:= 1 ->
                    [[DriverID, LicNoRec]] = DriverInfos,
                    DriverInfoItem = #driverinfo{driverid=DriverID, licno=LicNoRec, certcode=CertCode, vdrauthcode=VDRAuthCode},
                    %mslog:loginfo("Change driver item online state : ~p", [DriverInfoItem]),
                    ets:insert(drivertable, DriverInfoItem),
                    Pid ! {Pid, {DriverInfosCount, DriverID}};
                true ->
                    mslog:lognone("Get ~p driver item by certificate_code ~p", [DriverInfosCount, CertCode]),
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
                    mslog:lognone("Get ~p certificate code by vdr_auth_code ~p", [DriverInfosCount, VDRAuthCode]),
                    Pid ! {Pid, <<"">>}
            end,
            drivertable_insert_delete_process();
        {Pid, count} ->
            Count = ets:info(drivertable,size),
            Pid ! {Pid, Count},
            drivertable_insert_delete_process();
        _ ->
            mslog:loghint("Driver table insert/delete process receive unknown msg."),
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
            mslog:loghint("Last pos table insert/delete process stops.");
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
            mslog:loghint("Last pos table insert/delete process receive unknown msg."),
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
    [{eredispid, EredisPid}] = ets:lookup(msgservertable, eredispid),
    case EredisPid of
        undefined ->
            ok;
        _ ->
            EredisPid ! stop
    end,
    [{conninfopid, ConnInfoPid}] = ets:lookup(msgservertable, conninfopid),
    case ConnInfoPid of
        undefined ->
            ok;
        _ ->
            ConnInfoPid ! stop
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
    mslog:loghint("Message server stops.").

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
receive_redis_init_msg(UseRedis, EredisPid, Count) ->
    %mslog:loghint("msapp:receive_redis_init_msg(UseRedis ~p, EredisPid ~p, Count ~p)", [UseRedis, EredisPid, Count]),
    if
        UseRedis =:= 1 ->
            if
                Count > ?REDIS_INIT_TIME ->
                    {error, "Redis is not ready"};
                true ->
                    receive
                        {EredisPid, redisok} ->
                            ok
                    after ?WAIT_LOOP_INTERVAL ->
                        receive_redis_init_msg(UseRedis, EredisPid, Count+1)
                    end
            end;
        true ->
            ok
    end.

