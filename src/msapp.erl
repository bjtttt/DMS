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
start(StartType, StartArgs) ->
    Len = length(StartArgs),
    if
        Len == 2 ->
            startserver(StartType, StartArgs);
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
startserver(StartType, StartArgs) ->
    [UseRedisFlag, UseHttpGpsFlag] = StartArgs,
    ets:new(msgservertable, [set, public, named_table, {keypos, 1}, {read_concurrency, true}, {write_concurrency, true}]),
    if
        UseRedisFlag == 1 ->
            ets:insert(msgservertable, {useredis, 1});
        true ->
            ets:insert(msgservertable, {useredis, 0})
    end,
    [{useredis, UseRedis}] = ets:lookup(msgservertable, useredis),
    if
        UseHttpGpsFlag == 1 ->
            ets:insert(msgservertable, {usehttpgps, 1});
        true ->
            ets:insert(msgservertable, {usehttpgps, 0})
    end,
    [{usehttpgps, UseHttpGps}] = ets:lookup(msgservertable, usehttpgps),
    ets:insert(msgservertable, {redispid, undefined}),
	ets:insert(msgservertable, {redisoperationpid, undefined}),
    ets:insert(msgservertable, {apppid, self()}),
    ets:insert(msgservertable, {dblog, []}),
    
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
			log:loginfo("Successfully create directory ~p", [?DEF_LOG_PATH ++ "/log"]);
		{error, DirEx0} ->
			log:loginfo("Cannot create directory ~p : ~p", [?DEF_LOG_PATH ++ "/log", DirEx0])
	end,
	case file:make_dir(?DEF_LOG_PATH ++ "/log/vdr") of
		ok ->
			log:loginfo("Successfully create directory ~p", [?DEF_LOG_PATH ++ "/log/vdr"]);
		{error, DirEx1} ->
			log:loginfo("Cannot create directory ~p : ~p", [?DEF_LOG_PATH ++ "/log/vdr", DirEx1])
	end,
	case file:make_dir(?DEF_LOG_PATH ++ "/log/redis") of
		ok ->
			log:loginfo("Successfully create directory ~p", [?DEF_LOG_PATH ++ "/log/redis"]);
		{error, DirEx2} ->
			log:loginfo("Cannot create directory ~p : ~p", [?DEF_LOG_PATH ++ "/log/redis", DirEx2])
	end,
	case file:make_dir(?DEF_LOG_PATH ++ "/media") of
		ok ->
			log:loginfo("Successfully create directory ~p", [?DEF_LOG_PATH ++ "/media"]);
		{error, DirEx3} ->
			log:loginfo("Cannot create directory ~p : ~p", [?DEF_LOG_PATH ++ "/media", DirEx3])
	end,
	case file:make_dir(?DEF_LOG_PATH ++ "/upgrade") of
		ok ->
			log:loginfo("Successfully create directory ~p", [?DEF_LOG_PATH ++ "/upgrade"]);
		{error, DirEx4} ->
			log:loginfo("Cannot create directory ~p : ~p", [?DEF_LOG_PATH ++ "/upgrade", DirEx4])
	end,
    log:loginfo("Directories are initialized."),
    
    case supervisor:start_link(mssup, []) of
        {ok, SupPid} ->
            ets:insert(msgservertable, {suppid, SupPid}),
            log:loginfo("Dynamic Message Server starts initializing data structures."),
            case receive_redis_init_msg(UseRedis, 0) of
                ok ->
                    LinkInfoPid = spawn(fun() -> connection_info_process(lists:duplicate(?CONN_STAT_INFO_COUNT, 0)) end),
                    ets:insert(msgservertable, {linkinfopid, LinkInfoPid}),
                    
                    CCPid = spawn(fun() -> code_convertor_process(0) end),
                    ets:insert(msgservertable, {ccpid, CCPid}),
                                        
                    VdrTablePid = spawn(fun() -> vdrtable_insert_delete_process() end),
                    ets:insert(msgservertable, {vdrtablepid, VdrTablePid}),
					
                    DriverTablePid = spawn(fun() -> drivertable_insert_delete_process() end),
                    ets:insert(msgservertable, {drivertablepid, DriverTablePid}),

					LastPosTablePid = spawn(fun() -> lastpostable_insert_delete_process() end),
                    ets:insert(msgservertable, {lastpostablepid, LastPosTablePid}),

					VDRLogPid = spawn(fun() -> vdr_log_process([]) end),
    				ets:insert(msgservertable, {vdrlogpid, VDRLogPid}),

					VDROnlinePid = spawn(fun() -> vdr_online_process([]) end),
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
						                            log:loginfo("Code convertor table is created"),
													HttpGpsPid = spawn(fun() -> http_gps_deamon(?DEF_HTTPGPS_SERVER, uninit, 0, 0, 0, 0, 1) end),
													ets:insert(msgservertable, {httpgpspid, HttpGpsPid}),
													log:loginfo("HTTP GPS process PID is ~p", [HttpGpsPid]),
													case HttpGps of
														1 ->
															HttpGpsPid ! init;
														_ ->
															ok
													end,
						                            {ok, AppPid}
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
            log:loginfo("Message server fails to start : ignore"),
            ignore;
        {error, Error} ->
            log:loginfo("Message server fails to start : ~p", [Error]),
            {error, Error}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Not Implemented Yet!
%
% Description :
%		Initialize VDR table from Redis.
%		Should we also use local cache here even if we use Redis? 
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init_vdr_db_table() ->
	ets:delete_all_objects(vdrdbtable),
	log:loginfo("Init vdr db table.").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Not Implemented Yet!
%
% Description :
%		Initialize driver table from Redis.
%		Should we also use local cache here even if we use Redis? 
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init_driver_table() ->
	ets:delete_all_objects(drivertable),
	log:loginfo("Init driver table.").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Not Implemented Yet!
%
% Description :
%		Initialize last position table from Redis.
%		Should we also use local cache here even if we use Redis? 
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init_last_pos_table() ->
	ets:delete_all_objects(lastpostable),
	log:loginfo("Init last position table.").

vdr_log_process(VDRList) ->
	receive
		stop ->
			log:loginfo("VDR log process stops.");
		reset ->
			vdr_log_process([]);
		{set, VID} ->
			MidVDRList = [C || C <- VDRList, C =/= VID],
			NewVDRList = lists:merge([MidVDRList, [VID]]),
			%log:loginfo("SET : VDRList ~p, MidVDRList ~p, NewVDRList ~p", [VDRList, MidVDRList, NewVDRList]),
			vdr_log_process(NewVDRList);
		{clear, VID} ->
			MidVDRList = [C || C <- VDRList, C =/= VID],
			%log:loginfo("CLEAR : VDRList ~p, MidVDRList ~pp", [VDRList, MidVDRList]),
			vdr_log_process(MidVDRList);
		{Pid, get} ->
			Pid ! {Pid, VDRList},
			vdr_log_process(VDRList);
		{_Pid, save, VDRID, FromVDR, MsgBin, DateTime} ->
			MidVDRList = [C || C <- VDRList, C == VDRID],
			Len = length(MidVDRList),
			if
				Len < 1 ->
					ok;
				true ->
					save_msg_4_vdr(VDRID, FromVDR, MsgBin, DateTime)
			end,
			vdr_log_process(VDRList);
		_ ->
			log:loginfo("VDR log process : unknown message"),
			vdr_log_process(VDRList)
	end.

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
					log:loginfo("Cannot open ~p : ~p", [File, Reason]);
				_ ->
					log:loginfo("Cannot open ~p : unknown", [File])
			end;
		true ->
			ok
	end.

vdr_online_process(VDROnlineList) ->
	receive
		stop ->
			log:loginfo("VDR Online process stops.");
		reset ->
			vdr_online_process([]);
		{_Pid, add, VID, DateTime} ->
			MidVDROnlineList = [{VDRID, DTList} || {VDRID, DTList} <- VDROnlineList, VDRID =/= VID],
			VIDVDROnlineList = [{VDRID0, DTList0} || {VDRID0, DTList0} <- VDROnlineList, VDRID0 == VID],
			Length = length(VIDVDROnlineList),
			if
				Length == 1 ->
					[{VDRID1, DTList1}] = VIDVDROnlineList,
					NewDTList1 = lists:merge([DTList1, [DateTime]]),
					NewVDROnlineList = lists:merge([MidVDROnlineList, [{VDRID1, NewDTList1}]]),
					vdr_online_process(NewVDROnlineList);
				true ->
					vdr_online_process(MidVDROnlineList)
			end;
		{Pid, count} ->
			CountList = get_vdr_online_count(VDROnlineList),
			Pid ! {Pid, CountList},
			vdr_online_process(VDROnlineList);
		{Pid, get, VID} ->
			VIDVDROnlineList = [{VDRID0, DTList0} || {VDRID0, DTList0} <- VDROnlineList, VDRID0 == VID],
			Length = length(VIDVDROnlineList),
			if
				Length == 1 ->
					[{_VDRID1, DTList1}] = VIDVDROnlineList,
					Pid ! {Pid, DTList1},
					vdr_online_process(VDROnlineList);
				true ->
					MidVDROnlineList = [{VDRID, DTList} || {VDRID, DTList} <- VDROnlineList, VDRID =/= VID],
					Pid ! {Pid, []},
					vdr_online_process(MidVDROnlineList)
			end;
		{_Pid, clear, VID} ->
			MidVDROnlineList = [{VDRID, DTList} || {VDRID, DTList} <- VDROnlineList, VDRID =/= VID],
			vdr_online_process(MidVDROnlineList);
		_ ->
			log:loginfo("VDR Online process : unknown message"),
			vdr_online_process(VDROnlineList)
	end.

get_vdr_online_count(VDROnlineList) when is_list(VDROnlineList),
										 length(VDROnlineList) > 0 ->
	[H|T] = VDROnlineList,
	{VDRID, DTList} = H,
	DTCount = length(DTList),
	if
		DTCount > 1 ->
			NewH = {VDRID, DTCount},
			NewT = get_vdr_online_count(T),
			lists:merge([NewH], NewT);
		true ->
			get_vdr_online_count(T)
	end;
get_vdr_online_count(_VDROnlineList) ->
	[].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% This process will operate device\vehicle table and alarm table
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
db_data_operation_process(DBPid) ->
	receive
		stop ->
			log:loginfo("DB operation process stops.");
		%{Pid, update, devicevehicle} ->
		%	log:loginfo("DB operation process update device/vehicle."),
		%	ProcPid = self(),
		%	init_vdrdbtable(ProcPid, DBPid),
		%	Pid ! {Pid, updateok},
		%	db_data_operation_process(DBPid);
		%{Pid, update, alarm} ->
		%	log:loginfo("DB operation process update alarm."),
		%	ProcPid = self(),
		%	init_alarmtable(ProcPid, DBPid),
		%	Pid ! {Pid, updateok},
		%	db_data_operation_process(DBPid);
		{_Pid, replace, alarm, VehicleID, AlarmList} ->
			ets:delete(alarmtable, VehicleID),
			ets:insert(alarmtable, AlarmList),
			%Pid ! {Pid, relpaceok},
			db_data_operation_process(DBPid);
		{Pid, lookup, Table, Key} ->
			Res = ets:lookup(Table, Key),
			Pid ! {Pid, Res},
			db_data_operation_process(DBPid);
		{Pid, insert, Table, Object} ->
			ets:insert(Table, Object),
			Pid ! {Pid, insertok},
			db_data_operation_process(DBPid);
		{_Pid, insert, Table, Object, noresp} ->
			ets:insert(Table, Object),
			db_data_operation_process(DBPid);
		{Pid, delete, Table, Key} ->
			ets:delete(Table, Key),
			Pid ! {Pid, deleteok},
			db_data_operation_process(DBPid);
		{_Pid, delete, Table, Key, noresp} ->
			ets:delete(Table, Key),
			db_data_operation_process(DBPid);
		{Pid, clear, Table} ->
			ets:delete_all_objects(Table),
			Pid ! {Pid, clearok},
			db_data_operation_process(DBPid);
		{_Pid, clear, Table, noresp} ->
			ets:delete_all_objects(Table),
			db_data_operation_process(DBPid);
		Msg ->
			log:loginfo("DB operation process receive unknown msg : ~p", [Msg]),
			db_data_operation_process(DBPid)
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
vdrtable_insert_delete_process() ->
	receive
		stop ->
			log:loginfo("VDR table insert/delete process stops.");
		{Pid, insert, Object} ->
			ets:insert(vdrtable, Object),
			Pid ! {Pid, ok},
			vdrtable_insert_delete_process();
		{_Pid, insert, Object, noresp} ->
			ets:insert(vdrtable, Object),
			vdrtable_insert_delete_process();
		{Pid, delete, Key} ->
			ets:delete(vdrtable, Key),
			Pid ! {Pid, ok},
			vdrtable_insert_delete_process();
		{_Pid, delete, Key, noresp} ->
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
			log:loginfo("VDR table insert/delete process receive unknown msg."),
			vdrtable_insert_delete_process()
	end.

discremove_vdr_by_socket(Scks, Sock) when is_list(Scks),
									      length(Scks) > 0 ->
	[[H]|T] = Scks,
	ets:delete(vdrtable, H),
	if
		Sock =/= H ->
			try
				gen_tcp:close(H)
			catch
				_:_ ->
					ok
			end;
		true ->
			ok
	end,
	discremove_vdr_by_socket(T, Sock);
discremove_vdr_by_socket(_Scks, _Sock) ->
	ok.

drivertable_insert_delete_process() ->
	receive
		stop ->
			log:loginfo("Driver table insert/delete process stops.");
		{_Pid, chkinsdriverinfo, {DriverID, LicNo, CertCode, VDRAuthCode}} ->		% Check and insert
			if
				DriverID == undefined ->
					log:loginfo("Cannot get driver item by driver undefined id");
				true ->
					DriverInfos = ets:match(drivertable, {'$1',
														  DriverID, '_', '_', '_'}),
					DriverInfosCount = length(DriverInfos),
					if
						DriverInfosCount == 0 orelse DriverInfosCount == 1 ->
							DriverInfoItem = #driverinfo{driverid=DriverID, licno=LicNo, certcode=CertCode, vdrauthcode=VDRAuthCode},
							log:loginfo("Insert new driver item : ~p", [DriverInfoItem]),
							ets:insert(drivertable, DriverInfoItem);
						true ->
							ets:delete(drivertable, DriverID),
							DriverInfoItem = #driverinfo{driverid=DriverID, licno=LicNo, certcode=CertCode, vdrauthcode=VDRAuthCode},
							log:loginfo("Get ~p driver item by driver id ~p and re-create a new driver item : ~p", [DriverInfosCount, DriverID, DriverInfoItem]),
							ets:insert(drivertable, DriverInfoItem)
					end
			end,
			drivertable_insert_delete_process();
		{_Pid, offwork, CertCode} ->
		    DriverInfos = ets:match(drivertable, {'_',
												  '$1', '$2', CertCode, '_'}),
    		DriverInfosCount = length(DriverInfos),
			if
				DriverInfosCount == 1 ->
					[[DriverID, LicNo]] = DriverInfos,
					DriverInfoItem = #driverinfo{driverid=DriverID, licno=LicNo, certcode=CertCode},
					ets:insert(drivertable, DriverInfoItem);
				true ->
					log:loginfo("Get ~p driver item by certificate_code ~p", [DriverInfosCount, CertCode])
			end,					
			drivertable_insert_delete_process();
		{Pid, checkcc, {CertCode, VDRAuthCode}} ->		% CertCode must be binary
		    DriverInfos = ets:match(drivertable, {'_',
												  '$1', '$2', CertCode, '_'}),
    		DriverInfosCount = length(DriverInfos),
			if
				DriverInfosCount == 1 ->
					[[DriverID, LicNoRec]] = DriverInfos,
					DriverInfoItem = #driverinfo{driverid=DriverID, licno=LicNoRec, certcode=CertCode, vdrauthcode=VDRAuthCode},
					%log:loginfo("Change driver item online state : ~p", [DriverInfoItem]),
					ets:insert(drivertable, DriverInfoItem),
					Pid ! {Pid, {DriverInfosCount, DriverID}};
				true ->
					log:loginfo("Get ~p driver item by certificate_code ~p", [DriverInfosCount, CertCode]),
					Pid ! {Pid, {DriverInfosCount, undefined}}
			end,
			drivertable_insert_delete_process();
		{Pid, getccbyvdr, VDRAuthCode} ->
		    DriverInfos = ets:match(drivertable, {'_',
												  '_', '_', '$1', VDRAuthCode}),
    		DriverInfosCount = length(DriverInfos),
			if
				DriverInfosCount == 1 ->
					[[CertCodeBin]] = DriverInfos,
					if
						CertCodeBin == undefined ->
							Pid ! {Pid, <<"">>};
						true ->
							Pid ! {Pid, CertCodeBin}
					end;
				true ->
					log:loginfo("Get ~p certificate code by vdr_auth_code ~p", [DriverInfosCount, VDRAuthCode]),
					Pid ! {Pid, <<"">>}
			end,
			drivertable_insert_delete_process();
		{Pid, count} ->
			Count = ets:info(drivertable,size),
			Pid ! {Pid, Count},
			drivertable_insert_delete_process();
		_ ->
			log:loginfo("Driver table insert/delete process receive unknown msg."),
			drivertable_insert_delete_process()
	end.

lastpostable_insert_delete_process() ->
	receive
		stop ->
			log:loginfo("Last pos table insert/delete process stops.");
		{Pid, get, VID} ->
		    Infos = ets:match(lastpostable, {'_', 
											VID, '$1', '$2'}),
			InfoCount = length(Infos),
			if
				InfoCount == 1 ->	
					[[Lon, Lat]] = Infos,
					Pid ! {Pid, [Lon, Lat]};
				true ->
					Pid ! {Pid, [0.0, 0.0]}
			end,
			lastpostable_insert_delete_process();
		{_Pid, set, Info} ->
			[VID, Lon, Lat] = Info,
		    Infos = ets:match(lastpostable, {'$1', 
											VID, '_', '_'}),
			InfoCount = length(Infos),
			if
				InfoCount == 0 ->	
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
			log:loginfo("Last pos table insert/delete process receive unknown msg."),
			lastpostable_insert_delete_process()
	end.

%vdr_resp_process() ->
%	receive
%		stop ->
%			log:loginfo("VDR response process stops.");
%		{Pid, Socket, Msg} ->
%			vdr_resp_process(),
%			%log:loginfo("Msg from VDR ~p to VDRPid ~p : ~p", [Pid, self(), Msg]),
%			try gen_tcp:send(Socket, Msg)
%		    catch
%		        _:Ex ->
%		            log:loginfo("Exception when gen_tcps:send ~p : ~p", [Msg, Ex])
%		    end,
%			Pid ! {Pid, ok};
%		{_Pid, Socket, Msg, noresp} ->
%			vdr_resp_process(),
%			%log:loginfo("Msg from VDR ~p to VDRPid ~p : ~p", [Pid, self(), Msg]),
%			try gen_tcp:send(Socket, Msg)
%		    catch
%		        _:Ex ->
%		            log:loginfo("Exception when gen_tcps:send ~p : ~p", [Msg, Ex])
%		    end;
%		Unknown ->
%			log:loginfo("Msg from VDR to VDRPid unknwon : ~p", [Unknown]),
%			vdr_resp_process()
%	end.

%%%
%%%
%%%
stop(_State) ->
    [{dbpid, DBPid}] = ets:lookup(msgservertable, dbpid),
    case DBPid of
        undefined ->
            ok;
        _ ->
            DBPid ! stop
    end,
    [{vdrtablepid, VdrTablePid}] = ets:lookup(msgservertable, vdrtablepid),
    case VdrTablePid of
        undefined ->
            ok;
        _ ->
            VdrTablePid ! stop
    end,
    [{dbmaintainpid, DBMaintainPid}] = ets:lookup(msgservertable, dbmaintainpid),
    case DBMaintainPid of
        undefined ->
            ok;
        _ ->
            DBMaintainPid ! stop
    end,
    [{dboperationpid, DBOperationPid}] = ets:lookup(msgservertable, dboperationpid),
    case DBOperationPid of
        undefined ->
            ok;
        _ ->
            DBOperationPid ! stop
    end,
    [{mysqlactivepid, MysqlActivePid}] = ets:lookup(msgservertable, mysqlactivepid),
    case MysqlActivePid of
        undefined ->
            ok;
        _ ->
            MysqlActivePid ! stop
    end,
    [{vdrresppid, VdrRespPid}] = ets:lookup(msgservertable, vdrresppid),
    case VdrRespPid of
        undefined ->
            ok;
        _ ->
            VdrRespPid ! stop
    end,
    [{wspid, WSPid}] = ets:lookup(msgservertable, wspid),
    case WSPid of
        undefined ->
            ok;
        _ ->
            WSPid ! stop
    end,
    [{linkpid, LinkPid}] = ets:lookup(msgservertable, linkpid),
    case LinkPid of
        undefined ->
            ok;
        _ ->
            LinkPid ! stop
    end,
    [{dbtablepid, DBTablePid}] = ets:lookup(msgservertable, dbtablepid),
    case DBTablePid of
        undefined ->
            ok;
        _ ->
            DBTablePid ! stop
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
            VDRLogPid ! stop
    end,
    error_logger:info_msg("Message server stops.").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
%		Convert characters between UTF8 and GBK
% Parameters :
%   AppPid  :
%   DispLog : 1         -> display log message
%             others    -> doesn't display log message
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
code_convertor_process(DispLog) ->
    receive
        {Pid, create} ->			
            code_convertor:init_code_table(),
            log:loginfo("Code table is initialized."),
            Pid ! created,
            code_convertor_process(DispLog);
        stop ->
            ok;
        displaylog ->
            code_convertor_process(1);
        hidelog ->
            code_convertor_process(0);
        {Pid, gbk2utf8, Source} ->
            try
                Destination = code_convertor:to_utf8(Source),
				if
					DispLog == 1 ->
						log:loginfo("code_convertor_process : source GBK : ~p, dest UTF8 : ~p", [Source, Destination])
				end,
                Pid ! Destination
            catch
                _:Reason ->
					if
						DispLog == 1 ->
							log:loginfo("code_convertor_process : source ~p, dest UTF8 Exception : ~p", [Source, Reason])
					end,
                    Pid ! Source
            end,
            code_convertor_process(DispLog);
        {Pid, utf82gbk, Source} ->
            try
                Destination = code_convertor:to_gbk(Source),
				if
					DispLog == 1 ->
						log:loginfo("code_convertor_process : source UTF8 : ~p, dest GBK : ~p", [Source, Destination])
				end,
                Pid ! Destination
            catch
                _:Reason ->
					if
						DispLog == 1 ->
							log:loginfo("code_convertor_process : source ~p, dest GBK Exception : ~p", [Source, Reason])
					end,
                    Pid ! Source
            end,
            code_convertor_process(DispLog);
		{Pid, Msg} ->
			if
				DispLog == 1 ->
					log:loginfo("code_convertor_process : unknown request : ~p", [Msg])
			end,
			Pid ! Msg,
			code_convertor_process(DispLog);
		_ ->
			if
				DispLog == 1 ->
					log:loginfo("code_convertor_process : unknown message")
			end,
			code_convertor_process(DispLog)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
%		Maintain the connection status information
%       Please refer to include\header.hrl for details
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
connection_info_process(List) ->
    Len = length(List),
    if
        Len == ?CONN_STAT_INFO_COUNT ->
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
        UseRedis == 1 ->
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
db_table_deamon() ->
    Today = erlang:localtime(),%today(),
    Tomorrow = add(Today, 1),
    {{Year, Month, Day}, _} = Today,
    {{Year1, Month1, Day1}, _} = Tomorrow,
    YearS = vdr_data_processor:get_2_number_integer_from_oct_string(integer_to_list(Year)),
    MonthS = vdr_data_processor:get_2_number_integer_from_oct_string(integer_to_list(Month)),
    DayS = vdr_data_processor:get_2_number_integer_from_oct_string(integer_to_list(Day)),
    Year1S = vdr_data_processor:get_2_number_integer_from_oct_string(integer_to_list(Year1)),
    Month1S = vdr_data_processor:get_2_number_integer_from_oct_string(integer_to_list(Month1)),
    Day1S = vdr_data_processor:get_2_number_integer_from_oct_string(integer_to_list(Day1)),
    Bin = list_to_binary([common:integer_to_2byte_binary(YearS),
              common:integer_to_2byte_binary(MonthS),
              common:integer_to_2byte_binary(DayS)]),
    Bin1 = list_to_binary([common:integer_to_2byte_binary(Year1S),
               common:integer_to_2byte_binary(Month1S),
               common:integer_to_2byte_binary(Day1S)]),
    [{dbpid, DBPid}] = ets:lookup(msgservertable, dbpid),
    [{dbname, DBName}] = ets:lookup(msgservertable, dbname),
	DBNameBin = list_to_binary(DBName),
    case DBPid of
        undefined ->
            ok;
        _ ->
            Pid = self(),
            error_logger:info_msg("Check table ~p.vehicle_position_~p", [DBName, binary_to_list(Bin)]),
            DBPid ! {Pid, conn, list_to_binary([<<"CREATE TABLE IF NOT EXISTS ">>,
												DBNameBin,
												<<".vehicle_position_">>,
												Bin,
												<<" LIKE vehicle_position">>])},
            receive
                {Pid, Result1} ->
                    Result1
            end,
            error_logger:info_msg("Check table ~p.vehicle_position_~p", [DBName, binary_to_list(Bin1)]),
            DBPid ! {Pid, conn, list_to_binary([<<"CREATE TABLE IF NOT EXISTS ">>,
												DBNameBin,
												<<".vehicle_position_">>,
												Bin1,
												<<" LIKE vehicle_position">>])},
            receive
                {Pid, Result2} ->
                    Result2
            end
    end,
	receive
		stop ->
			ok;
		_ ->
			db_table_deamon()
	after 23*60*60*1000 ->
			db_table_deamon()
	end.			

%today() -> erlang:localtime().
%tomorrow() -> add(today(), 1).

add(Date, second) ->
    add(Date, 1, seconds);
add(Date, minute) ->
    add(Date, 1, minutes);
add(Date, hour) ->
    add(Date, 1, hours);
add(Date, day) ->
    add(Date, 1);
add(Date, week) ->
    add(Date, 1, weeks);
add(Date, month) ->
    add(Date, 1, months);
add(Date, year) ->
    add(Date, 1, years);
add(Date, N)  ->
    add(Date, N, days).

add(DateTime, N, seconds) ->
    T1 = calendar:datetime_to_gregorian_seconds(DateTime),
    T2 = T1 + N,
    calendar:gregorian_seconds_to_datetime(T2);
add(DateTime, N, minutes) ->
    add(DateTime, 60*N, seconds);
add(DateTime, N, hours) ->
    add(DateTime, 60*N, minutes);
add(DateTime, N, days) ->
    add(DateTime, 24*N, hours);
add(DateTime, N, weeks) ->
    add(DateTime, 7*N, days);
% Adding months is a bit tricky.
add({{YYYY, MM, DD}=Date, Time}, 0, months) ->
    case calendar:valid_date(Date) of
	true  -> {Date, Time};
	false -> add({{YYYY, MM, DD-1}, Time}, 0, months) % Oops, too many days in this month,
                                                          % Remove a day and try again.
    end;
add({{YYYY, MM, DD}, Time}, N, months) when N > 0 andalso MM < 12 ->
    add({{YYYY, MM+1, DD}, Time}, N-1, months);
add({{YYYY, MM, DD}, Time}, N, months) when N > 0 andalso MM =:= 12 ->
    add({{YYYY+1, 1, DD}, Time}, N-1, months); 
add({{YYYY, MM, DD}, Time}, N, months) when N < 0 andalso MM > 1 ->
    add({{YYYY, MM-1, DD}, Time}, N+1, months);
add({{YYYY, MM, DD}, Time}, N, months) when N < 0 andalso MM =:= 1 ->
    add({{YYYY-1, 12, DD}, Time}, N+1, months);
add(Date, N, years) ->
    add(Date, 12*N, months).
     
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File END.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

