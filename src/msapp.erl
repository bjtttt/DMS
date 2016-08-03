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
            common:loginfo("Parameter count error : ~p", [Len])
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
    common:loginfo("Tables are initialized."),
    
	case file:make_dir(?DEF_LOG_PATH ++ "/log") of
		ok ->
			common:loginfo("Successfully create directory ~p", [?DEF_LOG_PATH ++ "/log"]);
		{error, DirEx0} ->
			common:loginfo("Cannot create directory ~p : ~p", [?DEF_LOG_PATH ++ "/log", DirEx0])
	end,
	case file:make_dir(?DEF_LOG_PATH ++ "/log/vdr") of
		ok ->
			common:loginfo("Successfully create directory ~p", [?DEF_LOG_PATH ++ "/log/vdr"]);
		{error, DirEx1} ->
			common:loginfo("Cannot create directory ~p : ~p", [?DEF_LOG_PATH ++ "/log/vdr", DirEx1])
	end,
	case file:make_dir(?DEF_LOG_PATH ++ "/log/redis") of
		ok ->
			common:loginfo("Successfully create directory ~p", [?DEF_LOG_PATH ++ "/log/redis"]);
		{error, DirEx2} ->
			common:loginfo("Cannot create directory ~p : ~p", [?DEF_LOG_PATH ++ "/log/redis", DirEx2])
	end,
	case file:make_dir(?DEF_LOG_PATH ++ "/media") of
		ok ->
			common:loginfo("Successfully create directory ~p", [?DEF_LOG_PATH ++ "/media"]);
		{error, DirEx3} ->
			common:loginfo("Cannot create directory ~p : ~p", [?DEF_LOG_PATH ++ "/media", DirEx3])
	end,
	case file:make_dir(?DEF_LOG_PATH ++ "/upgrade") of
		ok ->
			common:loginfo("Successfully create directory ~p", [?DEF_LOG_PATH ++ "/upgrade"]);
		{error, DirEx4} ->
			common:loginfo("Cannot create directory ~p : ~p", [?DEF_LOG_PATH ++ "/upgrade", DirEx4])
	end,
    common:loginfo("Directories are initialized."),
    
    case supervisor:start_link(mssup, []) of
        {ok, SupPid} ->
            ets:insert(msgservertable, {suppid, SupPid}),
            common:loginfo("Dynamic Message Server starts initializing data structures."),
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
						                            common:loginfo("Code convertor table is created"),
													HttpGpsPid = spawn(fun() -> http_gps_deamon(HttpGpsServer, uninit, 0, 0, 0, 0) end),
													ets:insert(msgservertable, {httpgpspid, HttpGpsPid}),
													common:loginfo("HTTP GPS process PID is ~p", [HttpGpsPid]),
													case HttpGps of
														1 ->
															HttpGpsPid ! init;
														_ ->
															ok
													end,
						                            {ok, AppPid}
						                        after ?TIMEOUT_CC_INIT_PROCESS ->
						                            {error, "ERROR : code convertor table is timeout"}
											end
									end
							end
                    end;
                {error, RedisError} ->
                    {error, "Message server fails to start : " ++ RedisError}                  
            end;
        ignore ->
            common:loginfo("Message server fails to start : ignore"),
            ignore;
        {error, Error} ->
            common:loginfo("Message server fails to start : ~p", [Error]),
            {error, Error}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
%		Initialize VDR table from Redis.
%		Should we also use local cache here even if we use Redis? 
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init_vdr_db_table() ->
	ets:delete_all_objects(vdrdbtable),
	common:loginfo("Init vdr db table.").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
%		Initialize driver table from Redis.
%		Should we also use local cache here even if we use Redis? 
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init_driver_table() ->
	ets:delete_all_objects(drivertable),
	common:loginfo("Init driver table.").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
%		Initialize last position table from Redis.
%		Should we also use local cache here even if we use Redis? 
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init_last_pos_table() ->
	ets:delete_all_objects(lastpostable),
	common:loginfo("Init last position table.").

vdr_log_process(VDRList) ->
	receive
		stop ->
			common:loginfo("VDR log process stops.");
		reset ->
			vdr_log_process([]);
		{set, VID} ->
			MidVDRList = [C || C <- VDRList, C =/= VID],
			NewVDRList = lists:merge([MidVDRList, [VID]]),
			%common:loginfo("SET : VDRList ~p, MidVDRList ~p, NewVDRList ~p", [VDRList, MidVDRList, NewVDRList]),
			vdr_log_process(NewVDRList);
		{clear, VID} ->
			MidVDRList = [C || C <- VDRList, C =/= VID],
			%common:loginfo("CLEAR : VDRList ~p, MidVDRList ~pp", [VDRList, MidVDRList]),
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
			common:loginfo("VDR log process : unknown message"),
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
					common:loginfo("Cannot open ~p : ~p", [File, Reason]);
				_ ->
					common:loginfo("Cannot open ~p : unknown", [File])
			end;
		true ->
			ok
	end.

vdr_online_process(VDROnlineList) ->
	receive
		stop ->
			common:loginfo("VDR Online process stops.");
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
			common:loginfo("VDR Online process : unknown message"),
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
			common:loginfo("DB operation process stops.");
		%{Pid, update, devicevehicle} ->
		%	common:loginfo("DB operation process update device/vehicle."),
		%	ProcPid = self(),
		%	init_vdrdbtable(ProcPid, DBPid),
		%	Pid ! {Pid, updateok},
		%	db_data_operation_process(DBPid);
		%{Pid, update, alarm} ->
		%	common:loginfo("DB operation process update alarm."),
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
			common:loginfo("DB operation process receive unknown msg : ~p", [Msg]),
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
			common:loginfo("VDR table insert/delete process stops.");
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
			common:loginfo("VDR table insert/delete process receive unknown msg."),
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
			common:loginfo("Driver table insert/delete process stops.");
		{_Pid, chkinsdriverinfo, {DriverID, LicNo, CertCode, VDRAuthCode}} ->		% Check and insert
			if
				DriverID == undefined ->
					common:loginfo("Cannot get driver item by driver undefined id");
				true ->
					DriverInfos = ets:match(drivertable, {'$1',
														  DriverID, '_', '_', '_'}),
					DriverInfosCount = length(DriverInfos),
					if
						DriverInfosCount == 0 orelse DriverInfosCount == 1 ->
							DriverInfoItem = #driverinfo{driverid=DriverID, licno=LicNo, certcode=CertCode, vdrauthcode=VDRAuthCode},
							common:loginfo("Insert new driver item : ~p", [DriverInfoItem]),
							ets:insert(drivertable, DriverInfoItem);
						true ->
							ets:delete(drivertable, DriverID),
							DriverInfoItem = #driverinfo{driverid=DriverID, licno=LicNo, certcode=CertCode, vdrauthcode=VDRAuthCode},
							common:loginfo("Get ~p driver item by driver id ~p and re-create a new driver item : ~p", [DriverInfosCount, DriverID, DriverInfoItem]),
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
					common:loginfo("Get ~p driver item by certificate_code ~p", [DriverInfosCount, CertCode])
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
					%common:loginfo("Change driver item online state : ~p", [DriverInfoItem]),
					ets:insert(drivertable, DriverInfoItem),
					Pid ! {Pid, {DriverInfosCount, DriverID}};
				true ->
					common:loginfo("Get ~p driver item by certificate_code ~p", [DriverInfosCount, CertCode]),
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
					common:loginfo("Get ~p certificate code by vdr_auth_code ~p", [DriverInfosCount, VDRAuthCode]),
					Pid ! {Pid, <<"">>}
			end,
			drivertable_insert_delete_process();
		{Pid, count} ->
			Count = ets:info(drivertable,size),
			Pid ! {Pid, Count},
			drivertable_insert_delete_process();
		_ ->
			common:loginfo("Driver table insert/delete process receive unknown msg."),
			drivertable_insert_delete_process()
	end.

lastpostable_insert_delete_process() ->
	receive
		stop ->
			common:loginfo("Last pos table insert/delete process stops.");
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
			common:loginfo("Last pos table insert/delete process receive unknown msg."),
			lastpostable_insert_delete_process()
	end.

%vdr_resp_process() ->
%	receive
%		stop ->
%			common:loginfo("VDR response process stops.");
%		{Pid, Socket, Msg} ->
%			vdr_resp_process(),
%			%common:loginfo("Msg from VDR ~p to VDRPid ~p : ~p", [Pid, self(), Msg]),
%			try gen_tcp:send(Socket, Msg)
%		    catch
%		        _:Ex ->
%		            common:loginfo("Exception when gen_tcps:send ~p : ~p", [Msg, Ex])
%		    end,
%			Pid ! {Pid, ok};
%		{_Pid, Socket, Msg, noresp} ->
%			vdr_resp_process(),
%			%common:loginfo("Msg from VDR ~p to VDRPid ~p : ~p", [Pid, self(), Msg]),
%			try gen_tcp:send(Socket, Msg)
%		    catch
%		        _:Ex ->
%		            common:loginfo("Exception when gen_tcps:send ~p : ~p", [Msg, Ex])
%		    end;
%		Unknown ->
%			common:loginfo("Msg from VDR to VDRPid unknwon : ~p", [Unknown]),
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
            common:loginfo("Code table is initialized."),
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
						common:loginfo("code_convertor_process : source GBK : ~p, dest UTF8 : ~p", [Source, Destination])
				end,
                Pid ! Destination
            catch
                _:Reason ->
					if
						DispLog == 1 ->
							common:loginfo("code_convertor_process : source ~p, dest UTF8 Exception : ~p", [Source, Reason])
					end,
                    Pid ! Source
            end,
            code_convertor_process(DispLog);
        {Pid, utf82gbk, Source} ->
            try
                Destination = code_convertor:to_gbk(Source),
				if
					DispLog == 1 ->
						common:loginfo("code_convertor_process : source UTF8 : ~p, dest GBK : ~p", [Source, Destination])
				end,
                Pid ! Destination
            catch
                _:Reason ->
					if
						DispLog == 1 ->
							common:loginfo("code_convertor_process : source ~p, dest GBK Exception : ~p", [Source, Reason])
					end,
                    Pid ! Source
            end,
            code_convertor_process(DispLog);
		{Pid, Msg} ->
			if
				DispLog == 1 ->
					common:loginfo("code_convertor_process : unknown request : ~p", [Msg])
			end,
			Pid ! Msg,
			code_convertor_process(DispLog);
		_ ->
			if
				DispLog == 1 ->
					common:loginfo("code_convertor_process : unknown message")
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
     

http_gps_deamon(InitialIPPort, State, Count, ACount, FCount, FACount) ->
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
					%common:loginfo("Normal Position : ~p", [FullRequest]),
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
																	%common:loginfo("Normal Address : ~p", [FullRequest2]),
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
																				common:loginfo("HTTP GPS address request fails : ~p", [Reason2]),
																				Pid ! [Lon, Lat, []],
																				http_gps_deamon(InitialIPPort, State, Count, ACount, FCount+1, FACount)
																		end
																	catch
																		Oper2:ExReason2 ->
																			common:loginfo("HTTP GPS address request exception : (~p) ~p", [Oper2, ExReason2]),
																			Pid ! [Lon, Lat, []],
																			http_gps_deamon(InitialIPPort, State, Count, ACount, FCount+1, FACount)
																	end;
																error ->
																	common:loginfo("HTTP GPS address request fails because of conversion error"),
																	Pid ! [Lon, Lat, []],
																	http_gps_deamon(InitialIPPort, State, Count, ACount, FCount+1, FACount)
															end
														catch
															_:_ ->
																common:loginfo("HTTP GPS request fails : cannot convert longitude and latitude ~p", [Body]),
																Pid ! [LonReq, LatReq, []],
																http_gps_deamon(InitialIPPort, State, Count, ACount, FCount+1, FACount)
														end;
													_ ->
														common:loginfo("HTTP GPS request fails : cannot convert longitude/latitude ~p", [Body]),
														Pid ! [LonReq, LatReq, []],
														http_gps_deamon(InitialIPPort, State, Count, ACount, FCount+1, FACount)
												end;
											_ ->
												common:loginfo("HTTP GPS request fails : response error ~p", [Body]),
												Pid ! [LonReq, LatReq, []],
												http_gps_deamon(InitialIPPort, State, Count, ACount, FCount+1, FACount)
										end;
									{error, Reason} ->
										common:loginfo("HTTP GPS request fails : ~p", [Reason]),
										Pid ! [LonReq, LatReq, []],
										http_gps_deamon(InitialIPPort, State, Count, ACount, FCount+1, FACount)
								end
							catch
								Oper:ExReason ->
									common:loginfo("HTTP GPS request exception : (~p) ~p", [Oper, ExReason]),
									Pid ! [LonReq, LatReq, []],
									http_gps_deamon(InitialIPPort, State, Count, ACount, FCount+1, FACount)
							end;
						uninit ->
							%common:loginfo("HTTP GPS request fails because of uninit state"),
							Pid ! [LonReq, LatReq, []],
							http_gps_deamon(InitialIPPort, State, Count, ACount, FCount+1, FACount);
						_ ->
							common:loginfo("HTTP GPS request fails because of unknown state"),
							Pid ! [LonReq, LatReq, []],
							http_gps_deamon(InitialIPPort, State, Count, ACount, FCount+1, FACount)
					end;
				error ->
					common:loginfo("HTTP GPS request fails because of unknown state"),
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
					%common:loginfo("Abnormal Address : ~p", [FullRequest]),
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
										common:loginfo("HTTP GPS address request fails : ~p", [Reason]),
										Pid ! [LonReq, LatReq, []],
										http_gps_deamon(InitialIPPort, State, Count, ACount, FCount, FACount+1)
								end
							catch
								Oper:ExReason ->
									common:loginfo("HTTP GPS address request exception : (~p) ~p", [Oper, ExReason]),
									Pid ! [LonReq, LatReq, []],
									http_gps_deamon(InitialIPPort, State, Count, ACount, FCount, FACount+1)
							end;
						uninit ->
							%common:loginfo("HTTP GPS address request fails because of uninit state"),
							Pid ! [LonReq, LatReq, []],
							http_gps_deamon(InitialIPPort, State, Count, ACount, FCount, FACount+1);
						_ ->
							common:loginfo("HTTP GPS request fails because of unknown state"),
							Pid ! [LonReq, LatReq, []],
							http_gps_deamon(InitialIPPort, State, Count, ACount, FCount, FACount+1)
					end;
				error ->
					common:loginfo("HTTP GPS address request fails because of conversion error"),
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
							common:loginfo("Cannot start HTTP GPS inets : ~p", [Reason]),
							http_gps_deamon(InitialIPPort, State, Count, ACount, FCount, FACount)
					end;
				inited ->
					common:loginfo("HTTP GPS inets already inited for init command"),
					http_gps_deamon(InitialIPPort, State, Count, ACount, FCount, FACount);
				_ ->
					common:loginfo("HTTP GPS inets unknown state for init command"),
					http_gps_deamon(InitialIPPort, State, Count, ACount, FCount, FACount)
			end;
		release ->
			case State of
				uninit ->
					common:loginfo("HTTP GPS inets already uninit for release command"),
					http_gps_deamon(InitialIPPort, State, Count, ACount, FCount, FACount);
				inited ->
					inets:stop(),
					http_gps_deamon(InitialIPPort, uninit, Count, ACount, FCount, FACount);
				_ ->
					common:loginfo("HTTP GPS inets unknwon state for release command"),
					http_gps_deamon(InitialIPPort, State, Count, ACount, FCount, FACount)
			end;
		stop ->
			case State of
				uninit ->
					common:loginfo("HTTP GPS inets already uninit for stop command");
				inited ->
					inets:stop();
				_ ->
					common:loginfo("HTTP GPS inets unknwon state for stop command")
			end;
		{Pid, get} ->
			Pid ! {InitialIPPort, State, Count, ACount, FCount, FACount},
			http_gps_deamon(InitialIPPort, State, Count, ACount, FCount, FACount);
		_ ->
			common:loginfo("HTTP GPS process receive unknown msg."),
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
	%common:loginfo("Body : ~p", [Body]),
	Province = get_province_name(Body),
	%common:loginfo("Province : ~p", [Province]),
	City = get_city_name(Body),
	%common:loginfo("City : ~p", [City]),
	District = get_district_name(Body),
	%common:loginfo("District : ~p", [District]),
	Road = get_road_name(Body),
	%common:loginfo("Road : ~p", [Road]),
	BuildingAddress = get_building_address(Body),
	%common:loginfo("BuildingAddress : ~p", [BuildingAddress]),
	BuildingName = get_building_name(Body),
	%common:loginfo("BuildingName : ~p", [BuildingName]),
	BuildingDirection = get_building_direction(Body),
	%common:loginfo("BuildingDirection : ~p", [BuildingDirection]),
	BuildingDistance = get_building_distance(Body),
	%common:loginfo("BuildingDistance : ~p", [BuildingDistance]),
	%list_to_binary([Province, City, District, Road]).
	if
		BuildingDirection == <<"">> orelse BuildingDistance == <<"">> ->
			Address = list_to_binary([Province, City, District, Road, BuildingAddress, BuildingName]),
			Address1 = binary:replace(Address, <<"(">>, <<"[">>, [global]),
			Address2 = binary:replace(Address1, <<")">>, <<"]">>, [global]),
			%common:loginfo("Address : (Binary ~p, List ~p) ~p", [is_binary(Address2), is_list(Address2), Address2]),
			Address2;
		true ->
			Address = list_to_binary([Province, City, District, Road, BuildingAddress, BuildingName, BuildingDirection, BuildingDistance]),
			Address1 = binary:replace(Address, <<"(">>, <<"[">>, [global]),
			Address2 = binary:replace(Address1, <<")">>, <<"]">>, [global]),
			%common:loginfo("Address : (Binary ~p, List ~p) ~p", [is_binary(Address2), is_list(Address2), Address2]),
			Address2
	end.
	%common:loginfo("Address : (Binary ~p, List ~p) ~p", [is_binary(Address), is_list(Address), Address]),
	%Address.
	%CCPid ! {Pid, utf8togbk, Address},
	%receive
	%	AddressNew ->
	%		common:loginfo("New Address : (Binary ~p, List ~p) ~p", [is_binary(AddressNew), is_list(AddressNew), AddressNew]),
	%		list_to_binary(AddressNew)
	%after ?TIMEOUT_CC_PROCESS ->
	%		common:loginfo("Address : (Binary ~p, List ~p) ~p", [is_binary(Address), is_list(Address), Address]),
	%		list_to_binary(Address)
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
								%	ProvinceNew ->
								%		common:loginfo("Province New : (Binary ~p, List ~p) ~p", [is_binary(ProvinceNew), is_list(ProvinceNew), ProvinceNew]),
								%		list_to_binary(ProvinceNew)
								%after ?TIMEOUT_CC_PROCESS ->
								%		common:loginfo("Province : (Binary ~p, List ~p) ~p", [is_binary(Province), is_list(Province), Province]),
								%		list_to_binary(Province)
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
						%common:loginfo("CityInfoBins : (~p)~n~p", [length(CityInfoBins), CityInfoBins]),
						Len2 = length(CityInfoBins),
						if
							Len2 > 1 ->
								lists:nth(2, CityInfoBins);
								%City = lists:nth(2, CityInfoBins),
								%CCPid ! {Pid, utf8togbk, City},
								%receive
								%	CityNew ->
								%		common:loginfo("City New : (Binary ~p, List ~p) ~p", [is_binary(CityNew), is_list(CityNew), CityNew]),
								%		list_to_binary(CityNew)
								%after ?TIMEOUT_CC_PROCESS ->
								%		common:loginfo("City : (Binary ~p, List ~p) ~p", [is_binary(City), is_list(City), City]),
								%		list_to_binary(City)
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
								%	DictrictNew ->
								%		common:loginfo("Dictrict New : (Binary ~p, List ~p) ~p", [is_binary(DictrictNew), is_list(DictrictNew), DictrictNew]),
								%		list_to_binary(DictrictNew)
								%after ?TIMEOUT_CC_PROCESS ->
								%		common:loginfo("Dictrict : (Binary ~p, List ~p) ~p", [is_binary(Dictrict), is_list(Dictrict), Dictrict]),
								%		list_to_binary(Dictrict)
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
						%common:loginfo("RoadInfoBins : (~p)~n~p", [length(RoadInfoBins), RoadInfoBins]),
						Len = length(RoadInfoBins),
						if
							Len > 1 ->
								lists:nth(2, RoadInfoBins);
								%Road = lists:nth(2, RoadInfoBins),
								%CCPid ! {Pid, utf8togbk, Road},
								%receive
								%	RoadNew ->
								%		common:loginfo("Road New : (Binary ~p, List ~p) ~p", [is_binary(RoadNew), is_list(RoadNew), RoadNew]),
								%		list_to_binary(RoadNew)
								%after ?TIMEOUT_CC_PROCESS ->
								%		common:loginfo("Road : (Binary ~p, List ~p) ~p", [is_binary(Road), is_list(Road), Road]),
								%		list_to_binary(Road)
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Lon : Jingdu
% Lat : Weidu
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
convertrequest(Request) when is_list(Request),
							 length(Request) == 2 ->
	[Lon, Lat] = Request,
	SLon = erlang:float_to_list(Lon, [{decimals, 6}, compact]),
	SLat = erlang:float_to_list(Lat, [{decimals, 6}, compact]),
	{ok, lists:append([SLon, ",", SLat])};
convertrequest(_Request) ->
	error.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File END.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

