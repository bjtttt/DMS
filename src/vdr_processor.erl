%
% vdr_processor.erl
%

-module(vdr_processor).

-include("../include/header.hrl").

-export([process_vdr_msges/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
% Parameter :
% Return :
%     {ok, State}
%     {error, ?, State}                 - ? means predefined integer  
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
process_vdr_msges(Socket, Msges, State) ->
    [H|T] = Msges,
    Result = safe_process_vdr_msg(Socket, H, State),
    case T of
        [] ->
            Result;
        _ ->
            case Result of
                {ok, NewState} ->
                    process_vdr_msges(Socket, T, NewState);
                {error, ErrorType, NewState} ->
                    {error, ErrorType, NewState}
            end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
% Parameter :
% Return :
%     {ok, State}
%     {error, ?, State}                 - ? means predefined integer  
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
safe_process_vdr_msg(Socket, Msg, State) ->
    try process_vdr_data(Socket, Msg, State)
    catch
        _:Ex ->
            [ST] = erlang:get_stacktrace(),
            mslog:log_vdr_info(error, State, "vdr_handler:safe_process_vdr_msg(...) excption ~p~nstack trace ~p~ndata ~p", [Ex, ST, Msg]),
            {error, ?CONN_STAT_DISC_MSGEX, State}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% This function should refer to the document on the mechanism
%
% Return :
%     {ok, State}
%     {warning, State}
%     {error, ?, State}  
%
% MsgIdx  : VDR message index
% FlowIdx : Gateway message flow index
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
process_vdr_data(Socket, Data, State) -> 
    mslog:log_vdr_info(all, State, "data ~p", Data),
    case vdr_data_parser:safe_parse_data(State, Data) of
        {ok, HeadInfo, Msg, NewState} ->
            process_pasred_messages(HeadInfo, Msg, NewState, Socket, Data);
        {error, ErrorType, NewState} ->
            % Only when message has parity error
            {error, ErrorType, NewState};
        {warning, HeaderInfo, ErrorType, NewState} ->
            {ID, MsgIdx, _Tel, _CryptoType} = HeaderInfo,
            FlowIdx = NewState#vdritem.msgflownum,
            MsgBody = vdr_data_processor:create_gen_resp(ID, MsgIdx, ErrorType),
            NewFlowIdx = send_data_to_vdr(16#8001, NewState#vdritem.tel, FlowIdx, MsgBody, NewState),
            {warning, NewState#vdritem{msgflownum=NewFlowIdx}};
        {ignore, HeaderInfo, NewState} ->
            {ID, MsgIdx, _Tel, _CryptoType} = HeaderInfo,
            FlowIdx = NewState#vdritem.msgflownum,
            MsgBody = vdr_data_processor:create_gen_resp(ID, MsgIdx, ?T_GEN_RESP_OK),
            NewFlowIdx = send_data_to_vdr(16#8001, NewState#vdritem.tel, FlowIdx, MsgBody, NewState),
            {RequiredId, MsgPackages} = NewState#vdritem.msgpackages,
            if
                RequiredId > -1 ->
                    common:loginfo("~p starts sending VDR (~p) request for resend : required msg id ~p", 
                                   [NewState#vdritem.pid, 
                                    NewState#vdritem.addr, 
                                    RequiredId]),
                    MissingMsgIdx = find_missing_msgidx(RequiredId, MsgPackages),
                    common:loginfo("~p starts sending VDR (~p) request for resend : finding missing msg index ~p", 
                                   [NewState#vdritem.pid, 
                                    NewState#vdritem.addr, 
                                    MissingMsgIdx]),
                    case MissingMsgIdx of
                        none ->
                            [VDRItem] = ets:lookup(vdrtable, Socket),
                            VDRTablePid = VDRItem#vdritem.vdrtablepid,
                            NewVDRItem = VDRItem#vdritem{msg=NewState#vdritem.msg},
                            common:send_vdr_table_operation(VDRTablePid, {self(), insert, NewVDRItem, noresp}),
                            
                            {ok, NewState#vdritem{msgflownum=NewFlowIdx}};
                        {FirstmsgIdxID, MsgIdxsID} ->
                            MsgBody1 = vdr_data_processor:create_resend_subpack_req(FirstmsgIdxID, length(MsgIdxsID), MsgIdxsID),
                            common:loginfo("~p sends VDR (~p) request for resend : fisrt msg id ~p, msg indexes ~p~n~p", 
                                           [NewState#vdritem.pid, 
                                            NewState#vdritem.addr, 
                                            FirstmsgIdxID, 
                                            MsgIdxsID, 
                                            MsgBody1]),
                            NewFlowIdx1 = send_data_to_vdr(16#8003, NewState#vdritem.tel, FlowIdx, MsgBody1, NewState),

                            [VDRItem] = ets:lookup(vdrtable, Socket),
                            VDRTablePid = VDRItem#vdritem.vdrtablepid,
                            NewVDRItem = VDRItem#vdritem{msg=NewState#vdritem.msg},
                            common:send_vdr_table_operation(VDRTablePid, {self(), insert, NewVDRItem, noresp}),
                            
                            {ok, NewState#vdritem{msgflownum=NewFlowIdx1}}
                    end;
                true ->
                    [VDRItem] = ets:lookup(vdrtable, Socket),
                    VDRTablePid = VDRItem#vdritem.vdrtablepid,
                    NewVDRItem = VDRItem#vdritem{msg=NewState#vdritem.msg},
                    common:send_vdr_table_operation(VDRTablePid, {self(), insert, NewVDRItem, noresp}),
                    
                    {ok, NewState#vdritem{msgflownum=NewFlowIdx}}
            end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
process_pasred_messages(HeadInfo, Msg, State, Socket, Data) ->
    {ID, MsgIdx, Tel, CryptoType} = HeadInfo,
    NewState = State#vdritem{encrypt=convert_crytotype(CryptoType)},
    if
        NewState#vdritem.id == undefined ->
            case ID of
                16#100 ->
                    % Registration
                    {ok, NewState};
                16#102 ->
                    % authorization
                    {ok, NewState};
                true ->
                    mslog:log_vdr_info(error, NewState, "unregistered/unauthorized VDR"),
                    % Unauthorized/Unregistered VDR can only accept 16#100/16#102
                    {error, ?CONN_STAT_DISC_AUTH, NewState}
            end;
        true ->
            if
                ID =/= 16#2 orelse ID =/= 16#200 ->
                    mslog:log_vdr_info(all, "msg id ~p, msg index ~p, msg tel ~p~ndata ~p", [ID, MsgIdx, Tel, Data])
            end,
            case ID of
                16#1 ->     % VDR general response
                    {RespFlowIdx, RespID, Resp} = Msg,                            
                    % Process reponse from VDR here
                    mslog:log_vdr_info(all, "general response (16#1) : Response Flow Index (~p), Response ID (~p), Response (~p)", [RespFlowIdx, RespID, Resp]),                            
                    if
                        %RespID == 16#8003 orelse
                            RespID == 16#8103 orelse
                            RespID == 16#8203 orelse    % has issue
                            RespID == 16#8600 orelse    % need further tested
                            RespID == 16#8601 orelse
                            RespID == 16#8602 orelse    % need further tested
                            RespID == 16#8603 orelse
                            RespID == 16#8604 orelse    % need further tested
                            RespID == 16#8605 orelse
                            RespID == 16#8606 orelse    % need further tested
                            RespID == 16#8607 orelse
                            RespID == 16#8105 orelse
                            RespID == 16#8108 orelse
                            RespID == 16#8202 orelse
                            RespID == 16#8300 orelse
                            RespID == 16#8302 orelse
                            RespID == 16#8400 orelse
                            RespID == 16#8401 orelse
                            RespID == 16#8500 orelse
                            RespID == 16#8801 orelse
                            RespID == 16#8804
                          ->
                            {ok, NewState};
                        true ->
                            {ok, NewState}
                    end;
                16#2 ->     % VDR pulse
                    FlowIdx = NewState#vdritem.msgflownum,
                    MsgBody = vdr_msg_processor:create_gen_resp(ID, MsgIdx, ?T_GEN_RESP_OK),
                    mslog:log_vdr_info(all, NewState, "general response ~p", [MsgBody]),
                    NewFlowIdx = send_data_to_vdr(16#8001, Tel, FlowIdx, MsgBody, NewState),
                    {ok, NewState#vdritem{msgflownum=NewFlowIdx}};
                16#3 ->     % VDR unregistration
                    FlowIdx = NewState#vdritem.msgflownum,
                    MsgBody = vdr_data_processor:create_gen_resp(ID, MsgIdx, ?T_GEN_RESP_OK),
                    mslog:log_vdr_info(all, NewState, "unregistration response ~p", [MsgBody]),
                    NewFlowIdx = send_data_to_vdr(16#8001, Tel, FlowIdx, MsgBody, NewState),
                    % return error to terminate connection with VDR
                    {error, ?CONN_STAT_DISC_UNREG, NewState#vdritem{msgflownum=NewFlowIdx}};
                16#104 ->   % VDR parameter query
                    {_RespIdx, _ActLen, _List} = Msg,
                    % Process response from VDR here
                    {ok, NewState};
                16#107 ->   % VDR property query
                    {_Type, _ProId, _Model, _TerId, _ICCID, _HwVerLen, _HwVer, _FwVerLen, _FwVer, _GNSS, _Prop} = Msg,
                    % Process response from VDR here
                    {ok, NewState};
                16#108 ->
                    {_Type, _Res} = Msg,
                    % Process response from VDR here
                    {ok, NewState};
                16#200 ->
                    %[Msg, Address] = adjust_http_gps_position(Msg, NewState),
                    FlowIdx = NewState#vdritem.msgflownum,
                    MsgBody = vdr_data_processor:create_gen_resp(ID, MsgIdx, ?T_GEN_RESP_OK),
                    NewFlowIdx = send_data_to_vdr(16#8001, NewState#vdritem.tel, FlowIdx, MsgBody, NewState),
                    {ok, NewState#vdritem{msgflownum=NewFlowIdx}};
                16#201 ->
                    FlowIdx = NewState#vdritem.msgflownum,
                    MsgBody = vdr_data_processor:create_gen_resp(ID, MsgIdx, ?T_GEN_RESP_OK),
                    NewFlowIdx = send_data_to_vdr(16#8001, NewState#vdritem.tel, FlowIdx, MsgBody, NewState),
                    {ok, NewState#vdritem{msgflownum=NewFlowIdx}};
                16#301 ->
                    {_Id} = Msg,
                    
                    {ok, NewState};
                16#302 ->
                    {ok, NewState};
                16#303 ->
                    {_MsgType, _POC} = Msg,
                    
                    {ok, NewState};
                16#500 ->
                    {ok, NewState};
                16#700 ->
                    {_Number, _OrderWord, _DB} = Msg,
                    
                    {ok, NewState};
                16#701 ->
                    {_Length, _Content} = Msg,
                    
                    {ok, NewState};
                16#702 ->
                    {ok, NewState};                            
                16#704 ->
                    {_Len, _Type, _Positions} = Msg,
                    
                    {ok, NewState};
                16#705 ->
                    {_Count, _Time, _Data} = Msg,
                    
                    {ok, NewState};
                16#800 ->
                    {_MsgId, _MsgType, _MsgCode, _MsgEICode, _MsgPipeId} = Msg,
                    
                    FlowIdx = NewState#vdritem.msgflownum,
                    MsgBody = vdr_data_processor:create_gen_resp(ID, MsgIdx, ?T_GEN_RESP_OK),
                    NewFlowIdx = send_data_to_vdr(16#8001, Tel, FlowIdx, MsgBody, NewState),

                    {ok, NewState#vdritem{msgflownum=NewFlowIdx}};
                16#801 ->
                    {MediaId, _Type, _Code, _EICode, _PipeId, _PosInfo, _Pack} = Msg,

                    FlowIdx = NewState#vdritem.msgflownum,
                    MsgBody = vdr_data_processor:create_multimedia_data_reply(MediaId),
                    NewFlowIdx = send_data_to_vdr(16#8800, Tel, FlowIdx, MsgBody, NewState),
                    
                    [VDRItem] = ets:lookup(vdrtable, Socket),
                    VDRTablePid = VDRItem#vdritem.vdrtablepid,
                    NewVDRItem = VDRItem#vdritem{msg=NewState#vdritem.msg},
                    common:send_vdr_table_operation(VDRTablePid, {insert, NewVDRItem}),
                    
                    {ok, NewState#vdritem{msgflownum=NewFlowIdx}};
               16#805 ->
                    {ok, NewState};
                16#802 ->
                    {_FlowNum, _Len, _RespData} = Msg,
                    
                    {ok, NewState};
                16#900 ->
                    {_Type, _Con} = Msg,
                    
                    {ok, NewState};
                16#901 ->
                    {_Len, _Body} = Msg,
                    
                    {ok, NewState};
                16#A00 ->
                    {_E, _N} = Msg,
                    
                    {ok, NewState};
                16#100 ->
                    FlowIdx = NewState#vdritem.msgflownum,
                    MsgBody = vdr_data_processor:create_reg_resp(MsgIdx, 0, list_to_binary(NewState#vdritem.auth)),
                    NewFlowIdx = send_data_to_vdr(16#8100, Tel, FlowIdx, MsgBody, NewState),

                    {ok, NewState#vdritem{msgflownum=NewFlowIdx}};
                16#102 ->
                    FlowIdx = NewState#vdritem.msgflownum,
                    MsgBody = vdr_data_processor:create_gen_resp(ID, MsgIdx, ?T_GEN_RESP_OK),
                    NewFlowIdx = send_data_to_vdr(16#8001, Tel, FlowIdx, MsgBody, NewState),
                    
                    {ok, NewState#vdritem{msgflownum=NewFlowIdx}};
                _ ->
                    mslog:log_vdr_info(error, NewState, "unknown message id ~p", [ID]),
                    {error, ?CONN_STAT_DISC_UNK_MSG_ERR, NewState}
            end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
%   Save the VDR online time locally
% Parameter :
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
save_online_msg(VID, State) ->
    VDROnlinePid = State#vdritem.vdronlinepid,
    if
        VDROnlinePid =/= undefined ->
            {Year,Month,Day} = erlang:date(),
            {Hour,Min,Second} = erlang:time(),
            VDROnlinePid ! {add, VID, {Year,Month,Day,Hour,Min,Second}};
        true ->
            mslog:log_vdr_info(error, State, "vdr_processor:save_online_msg(...) : no VDR online process id")
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
% Parameter :
% Return :
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
convert_crytotype(CryptoType) ->
    BVal = CryptoType band 1,
    case BVal of
        1 ->
            true;
        _ ->
            false
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
% Parameter :
% Return :
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
find_missing_msgidx(RequiredId, MsgPackages) when is_integer(RequiredId),
                                                  RequiredId > -1,
                                                  is_list(MsgPackages),
                                                  length(MsgPackages) > 0 ->
    [H|T] = MsgPackages,
    [HId, HFirstmsgIdx, HMsgIdxs] = H,
    if
        HId == RequiredId ->
            {HFirstmsgIdx, HMsgIdxs};
        true ->
            find_missing_msgidx(RequiredId, T)
    end;
find_missing_msgidx(_RequiredId, _MsgPackages) ->
    none.

create_time_list_and_binary(Time) when is_integer(Time) ->
    <<Year:8, Month:8, Day:8, Hour:8, Minute:8, Second:8>> = <<Time:48>>,
    YearBin = common:integer_to_binary(common:convert_bcd_integer(Year)),
    MonthBin = common:integer_to_binary(common:convert_bcd_integer(Month)),
    DayBin = common:integer_to_binary(common:convert_bcd_integer(Day)),
    HourBin = common:integer_to_binary(common:convert_bcd_integer(Hour)),
    MinuteBin = common:integer_to_binary(common:convert_bcd_integer(Minute)),
    SecondBin = common:integer_to_binary(common:convert_bcd_integer(Second)),
    TimeBin = list_to_binary([YearBin, <<"-">>, MonthBin, <<"-">>, DayBin, <<" ">>, HourBin, <<":">>, MinuteBin, <<":">>, SecondBin]),
    TimeList = binary_to_list(TimeBin),
    {TimeBin, TimeList, {{Year,Month,Day},{Hour,Minute,Second}}};
create_time_list_and_binary(_Time) ->
    {<<"2000-01-01 00:00:00">>, "2000-01-01 00:00:00", {{2000,1,1},{0,0,0}}}.

adjust_http_gps_position(Msg, NewState) ->
    case Msg of
        {H, AppInfo} ->
            [AlarmSym, StateFlag, Lat, Lon, Height, Speed, Direction, Time]= H,
            [NewLat, NewLon, Address] = get_http_gps_lon_lat(Lat, Lon, NewState),
            [{[AlarmSym, StateFlag, NewLat, NewLon, Height, Speed, Direction, Time], AppInfo}, Address];
        _ ->
            [Msg, []]
    end.

get_appinfo_area_line_alarm(AppInfo) when is_list(AppInfo),
                                          length(AppInfo) > 0 ->
    [H|T] = AppInfo,
    case H of
        [ID, _Res1, _Res2, _Res3] ->
            if
                ID == 16#12 ->
                    H;
                    %lists:merge([H], get_appinfo_area_line_alarm(T));
                true ->
                    get_appinfo_area_line_alarm(T)
            end;
        _ ->
            [0,0,0]
    end;
get_appinfo_area_line_alarm(_AppInfo) ->
    [0,0,0].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% SetClear : 1 or 0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
send_masg_to_ws_alarm(FlowIdx, AlarmList, SetClear, Lat, Lon, TimeBinS, State) when is_list(AlarmList),
                                                                                    length(AlarmList) > 0,
                                                                                    is_binary(TimeBinS) ->
    [H|T] = AlarmList,
    LenT = length(T),
    {alarmitem, _VehicleID, ID, _Time} = H,
    case SetClear of
        1 ->
            {ok, WSUpdate} = wsock_data_parser:create_term_alarm([State#vdritem.vehicleid],
                                                                 FlowIdx,
                                                                 common:combine_strings(["\"", State#vdritem.vehiclecode, "\""], false),
                                                                 ID,
                                                                 1,
                                                                 Lat, 
                                                                 Lon,
                                                                 binary_to_list(TimeBinS)),
            send_msg_to_ws_nowait(WSUpdate, State), %wsock_client:send(WSUpdate)
            if
                LenT > 0 ->
                    send_masg_to_ws_alarm(FlowIdx, T, SetClear, Lat, Lon, TimeBinS, State);
                true ->
                    ok
            end;
        0 ->
            {ok, WSUpdate} = wsock_data_parser:create_term_alarm([State#vdritem.vehicleid],
                                                                 FlowIdx,
                                                                 common:combine_strings(["\"", State#vdritem.vehiclecode, "\""], false),
                                                                 ID,
                                                                 0,
                                                                 Lat, 
                                                                 Lon,
                                                                 binary_to_list(TimeBinS)),
            send_msg_to_ws_nowait(WSUpdate, State), %wsock_client:send(WSUpdate)
            if
                LenT > 0 ->
                    send_masg_to_ws_alarm(FlowIdx, T, SetClear, Lat, Lon, TimeBinS, State);
                true ->
                    ok
            end;
        _ ->
            ok
    end;
send_masg_to_ws_alarm(_FlowIdx, _AlarmList, _SetClear, _Lat, _Lon, _TimeBinS, _State) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
find_alarm_in_lista_not_in_listb(ListA, ListB) when is_list(ListA),
                                                    is_list(ListB)->
    LenA = length(ListA),
    LenB = length(ListB),
    if
        LenA < 1 ->
            [];
        true ->
            if
                LenB < 1 ->
                    ListA;
                true ->
                    [H|T] = ListA,
                    case find_a_in_lista(ListB, H) of
                        true ->
                            find_alarm_in_lista_not_in_listb(T, ListB);
                        false ->
                            lists:merge([[H], find_alarm_in_lista_not_in_listb(T, ListB)])
                    end
            end
    end;
find_alarm_in_lista_not_in_listb(_ListA, _ListB) ->
    [].
                    
find_a_in_lista(ListA, A) when is_list(ListA),
                               length(ListA) > 0 ->
    [H|T] = ListA,
    {alarmitem, _VehicleID, ID, _Time} = H,
    {alarmitem, _VehicleIDA, IDA, _TimeA} = A,
    if
        ID == IDA ->
            true;
        true ->
            LenT = length(T),
            if
                LenT < 1 ->
                    false;
                true ->
                    find_a_in_lista(T, A)
            end
    end;
find_a_in_lista(_ListA, _A) ->
    false.

get_alarm_item(Index, AlarmList) when is_integer(Index),
                                      is_list(AlarmList),
                                      length(AlarmList) > 0,
                                      Index >= 0,
                                      Index =< 31 ->
    [H|T] = AlarmList,
    {alarmitem, _VehicleID, Idx, _Time} = H,
    if
        Index == Idx ->
            H;
        true ->
            get_alarm_item(Index, T)
    end;
get_alarm_item(_Index, _AlarmList) ->
    empty.

remove_alarm_item(Index, AlarmList) when is_integer(Index),
                                         is_list(AlarmList),
                                         length(AlarmList) > 0,
                                         Index >= 0,
                                         Index =< 31 ->
    [H|T] = AlarmList,
    {alarmitem, _VehicleID, Idx, _Time} = H,
    if
        Index == Idx ->
            case T of
                [] ->
                    [];
                _ ->
                    remove_alarm_item(Index, T)
            end;
        true ->
            case T of
                [] ->
                    [H];
                _ ->
                    lists:merge([H], remove_alarm_item(Index, T))
            end
    end;
remove_alarm_item(_Index, AlarmList) ->
    AlarmList.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Diconnect socket and remove related entries from vdrtable
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
disconn_socket_by_id(SockList) when is_list(SockList),
                                    length(SockList) > 0 ->
    case SockList of
        [] ->
            ok;
        _ ->
            [H|T] = SockList,
            [Sock] = H,
            try
                gen_tcp:close(Sock)
            catch
                _:_ ->
                    ok
            end,
            disconn_socket_by_id(T)
    end;
disconn_socket_by_id(_SockList) ->
    ok.

disconn_socket_by_vehicle_id(VehicleID) ->
    SockList = ets:match(vdrtable, {'_', 
                         '$1', '_', '_', '_', VehicleID,
                         '_', '_', '_', '_', '_',
                         '_', '_', '_', '_', '_',
                         '_', '_', '_', '_', '_',
                         '_', '_', '_', '_', '_',
                         '_', '_', '_', '_', '_',
                         '_', '_', '_', '_', '_',
                         '_', '_', '_', '_', '_',
                         '_'}),
    disconn_socket_by_id(SockList).

check_alarm(State, VehicleID) ->
    Pid = State#vdritem.pid,
    DBOperationPid = State#vdritem.dboperid,
    DBOperationPid ! {Pid, lookup, vdrdbtable, VehicleID},
    receive
        {Pid, Res} ->
            case Res of
                [] ->
                    empty;
                Alarms ->
                    {ok, Alarms}
            end
    after ?DB_RESP_TIMEOUT * 3 ->
            empty
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
update_reg_install_time(DeviceID, DeviceRegTime, VehicleID, VehicleDeviceInstallTime, State) ->
    {Year, Month, Day} = erlang:date(),
    {Hour, Minute, Second} = erlang:time(),
    DateTime = integer_to_list(Year) ++ "-" ++ 
                   integer_to_list(Month) ++ "-" ++ 
                   integer_to_list(Day) ++ " " ++ 
                   integer_to_list(Hour) ++ ":" ++ 
                   integer_to_list(Minute) ++ ":" ++ 
                   integer_to_list(Second),
    if
        DeviceRegTime == undefined ->
            DevInstallTimeSql = list_to_binary([<<"update vehicle set dev_install_time='">>,
                                                list_to_binary(DateTime),
                                                <<"' where id=">>,
                                                common:integer_to_binary(VehicleID)]),
            % Should we check the update result?
            %end_sql_to_db(regauth, DevInstallTimeSql, State);
            send_sql_to_db_nowait(conn, DevInstallTimeSql, State);
        true ->
            ok
    end,
    if
        VehicleDeviceInstallTime == undefined ->
            VDRRegTimeSql = list_to_binary([<<"update device set reg_time='">>,
                                            list_to_binary(DateTime),
                                            <<"' where id=">>,
                                            common:integer_to_binary(DeviceID)]),
            % Should we check the update result?
            %send_sql_to_db(regauth, VDRRegTimeSql, State);
            send_sql_to_db_nowait(conn, VDRRegTimeSql, State);
        true ->
            ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Parameters :
%       VehicleID   : Integer
%       DriverID    : Integer
%       TimeS       : String
%       Alarm       : Integer
%       Index       : Integer 0 - 31
%       AlarmList   : List
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
update_vehicle_alarm(VehicleID, DriverID, TimeS, TimeTuple, Alarm, Index, MsgIdx, State, AreaLineAlarm) when is_integer(VehicleID),
                                                                                              is_integer(DriverID),
                                                                                              is_integer(Index),
                                                                                              is_list(TimeS),
                                                                                              is_integer(MsgIdx),
                                                                                              MsgIdx >= 0,
                                                                                              is_integer(Alarm),
                                                                                              Index >= 0,
                                                                                              Index =< 31 ->
    AlarmList = State#vdritem.alarmlist,
    Flag = 1 bsl Index,
    BitState = Alarm band Flag,
    [_ID, AreaLineType, AreaLineID, AreaLineOper] = AreaLineAlarm,
    if
        BitState == 1 ->
            AlarmEntry = get_alarm_item(Index, AlarmList),
            if
                AlarmEntry == empty ->
                    UpdateSql = list_to_binary([<<"insert into vehicle_alarm(vehicle_id,driver_id,alarm_time,clear_time,type_id,sn,inout_area_line_id,inout_area_line_type,inout_area_line_oper) values(">>,
                                                common:integer_to_binary(VehicleID), <<",">>,
                                                common:integer_to_binary(DriverID), <<",'">>,
                                                list_to_binary(TimeS), <<"',NULL,">>,
                                                common:integer_to_binary(Index), <<",">>,
                                                common:integer_to_binary(MsgIdx), <<",">>, 
                                                common:integer_to_binary(AreaLineID), <<",">>,
                                                common:integer_to_binary(AreaLineType), <<",">>,
                                                common:integer_to_binary(AreaLineOper), <<")">>]),
                    send_sql_to_db_nowait(conn, UpdateSql, State),
                    NewAlarmList = lists:merge(AlarmList,[{alarmitem, VehicleID, Index, TimeS}]),%TimeTuple}]),
                    update_vehicle_alarm(VehicleID, DriverID, TimeS, TimeTuple, Alarm, Index+1, MsgIdx, State#vdritem{alarmlist=NewAlarmList}, AreaLineAlarm);
                true ->
                    update_vehicle_alarm(VehicleID, DriverID, TimeS, TimeTuple, Alarm, Index+1, MsgIdx, State, AreaLineAlarm)
            end;
        true ->
            AlarmEntry = get_alarm_item(Index, AlarmList),
            if
                AlarmEntry == empty ->
                    update_vehicle_alarm(VehicleID, DriverID, TimeS, TimeTuple, Alarm, Index+1, MsgIdx, State, AreaLineAlarm);
                true ->
                    {alarmitem, _VehicleID, Index, SetTime} = AlarmEntry,
                        UpdateSql = list_to_binary([<<"update vehicle_alarm set clear_time='">>, list_to_binary(TimeS),
                                                    <<"' where vehicle_id=">>, common:integer_to_binary(VehicleID),
                                                    <<" and driver_id=">>, common:integer_to_binary(DriverID),
                                                    <<" and alarm_time='">>, list_to_binary(SetTime),
                                                    <<"' and type_id=">>, common:integer_to_binary(Index)]),
                        send_sql_to_db_nowait(conn, UpdateSql, State),
                        NewAlarmList = remove_alarm_item(Index, AlarmList),
                        update_vehicle_alarm(VehicleID, DriverID, TimeS, TimeTuple, Alarm, Index+1, MsgIdx, State#vdritem{alarmlist=NewAlarmList}, AreaLineAlarm)
            end
    end;
update_vehicle_alarm(VehicleID, _DriverID, TimeS, TimeTuple, Alarm, Index, MsgIdx, State, AreaLineAlarm) when is_integer(VehicleID),
                                                                                               is_integer(Alarm),
                                                                                               is_list(TimeS),
                                                                                               is_integer(MsgIdx),                                                                                         
                                                                                               MsgIdx >= 0,
                                                                                               is_integer(Alarm),
                                                                                               Index >= 0,
                                                                                               Index =< 31 ->
    AlarmList = State#vdritem.alarmlist,
    Flag = 1 bsl Index,
    BitState = Alarm band Flag,
    [AreaLineType, AreaLineID, AreaLineOper] = AreaLineAlarm,
    if
        BitState == Flag ->
            AlarmEntry = get_alarm_item(Index, AlarmList),
            if
                AlarmEntry == empty ->
                    UpdateSql = list_to_binary([<<"insert into vehicle_alarm(vehicle_id,driver_id,alarm_time,clear_time,type_id,sn,inout_area_line_id,inout_area_line_type,inout_area_line_oper) values(">>,
                                                common:integer_to_binary(VehicleID), <<",0,'">>,
                                                list_to_binary(TimeS), <<"',NULL,">>,
                                                common:integer_to_binary(Index), <<",">>,
                                                common:integer_to_binary(MsgIdx), <<",">>, 
                                                common:integer_to_binary(AreaLineID), <<",">>,
                                                common:integer_to_binary(AreaLineType), <<",">>,
                                                common:integer_to_binary(AreaLineOper), <<")">>]),
                    send_sql_to_db_nowait(conn, UpdateSql, State),
                    NewAlarmList = lists:merge(AlarmList,[{alarmitem, VehicleID, Index, TimeS}]),%TimeTuple}]),
                    update_vehicle_alarm(VehicleID, _DriverID, TimeS, TimeTuple, Alarm, Index+1, MsgIdx, State#vdritem{alarmlist=NewAlarmList}, AreaLineAlarm);
                true ->
                    update_vehicle_alarm(VehicleID, _DriverID, TimeS, TimeTuple, Alarm, Index+1, MsgIdx, State, AreaLineAlarm)
            end;
        true ->
            AlarmEntry = get_alarm_item(Index, AlarmList),
            if
                AlarmEntry == empty ->
                    update_vehicle_alarm(VehicleID, _DriverID, TimeS, TimeTuple, Alarm, Index+1, MsgIdx, State, AreaLineAlarm);
                true ->
                    {alarmitem, _VehicleID, Index, SetTime} = AlarmEntry,
                    UpdateSql = list_to_binary([<<"update vehicle_alarm set clear_time='">>, list_to_binary(TimeS),
                                                <<"' where vehicle_id=">>, common:integer_to_binary(VehicleID),
                                                <<" and driver_id=0 and alarm_time='">>, list_to_binary(SetTime),
                                                <<"' and type_id=">>, common:integer_to_binary(Index)]),
                    send_sql_to_db_nowait(conn, UpdateSql, State),
                    NewAlarmList = remove_alarm_item(Index, AlarmList),
                    update_vehicle_alarm(VehicleID, _DriverID, TimeS, TimeTuple, Alarm, Index+1, MsgIdx, State#vdritem{alarmlist=NewAlarmList}, AreaLineAlarm)
            end
    end;
update_vehicle_alarm(_VehicleID, _DriverID, _TimeS, _TimeTuple, _Alarm, _Index, _MsgIdx, State, _AreaLineAlarm) ->
    State#vdritem.alarmlist.

send_sqls_to_db_nowait(PoolId, Msgs, State) when is_list(Msgs),
                                                 length(Msgs) > 0 ->
    [H|T] = Msgs,
    send_sql_to_db_nowait(PoolId, H, State),
    send_sqls_to_db_nowait(PoolId, T, State);
send_sqls_to_db_nowait(_PoolId, _Msgs, _State) ->
    ok.

get_certcode_for_sql(CertCode) ->
    if
        CertCode == undefined ->
            <<"">>;
        true ->
            CertCode
    end.
 
get_driver_cert_code(State) ->
    CertCode = State#vdritem.drivercertcode,
    case CertCode of
        undefined ->
            "";
        _ ->
            CertCode
    end.

get_driver_cc_by_vdr_auth_code(State, VDRAuthCode) ->
    DriverTablePid = State#vdritem.drivertablepid,
    Pid = State#vdritem.pid,
    DriverTablePid ! {Pid, getccbyvdr, VDRAuthCode},
    receive
        {Pid, CertCode} ->
            case CertCode of
                <<"">> ->
                    undefined;
                _ ->
                    CertCode
            end
    after ?PROC_RESP_TIMEOUT ->
            undefined
    end.

replace_pos_app_list(Init, ID, Item) when is_list(Init),
                                          length(Init) > 0 ->
    [H|T] = Init,
    {HID, _HKey, _HValue, _HKeyVal} = H,
    if
        HID == ID ->
            {NewKey, NewValue, NewKeyValue} = Item,
            lists:append([[{HID, NewKey, NewValue, NewKeyValue}], T]);
        true ->
            lists:append([[H], replace_pos_app_list(T, ID, Item)])          
    end;
replace_pos_app_list(Init, _ID, _Item) ->
    Init.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
% Parameter :
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_http_gps_lon_lat(Lat, Lon, State) ->
    {MidLat, MidLon} = get_not_0_lat_lon(Lat, Lon, State),
    if
        MidLat > 54000000.0 orelse MidLat < 4000000.0 orelse MidLon > 135000000.0 orelse MidLon < 73000000.0 ->
            [MidLat, MidLon, []];
        true ->
            Pid = State#vdritem.pid,
            HttpGpsPid = State#vdritem.httpgpspid,
            Encrypt = State#vdritem.encrypt,
            case Encrypt of
                1 ->
                    HttpGpsPid ! {Pid, abnormal, [MidLon/1000000.0, MidLat/1000000.0]},
                    receive
                        [FinalLon, FinalLat, Address] ->
                            [FinalLat*1000000.0, FinalLon*1000000.0, Address]
                    after ?PROC_RESP_TIMEOUT ->
                            [Lat, Lon, []]
                    end;
                _ ->
                    HttpGpsPid ! {Pid, normal, [MidLon/1000000.0, MidLat/1000000.0]},
                    receive
                        [FinalLon, FinalLat, Address] ->
                            [FinalLat*1000000.0, FinalLon*1000000.0, Address]
                    after ?PROC_RESP_TIMEOUT ->
                            [Lat, Lon, []]
                    end
            end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
%   Sometimes, VDR GPS doesn't work well. So we should use the previous position.
% Parameter :
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_not_0_lat_lon(Lat, Lon, State) ->
    if
        Lat == 0 orelse Lat == 0.0 orelse Lon == 0 orelse Lon == 0.0 ->
            {State#vdritem.lastlat, State#vdritem.lastlon};
        true ->
            {Lat, Lon}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% ID        :
% FlowIdx   :
% MsgBody   :
% Pid       :
% VDRPid    :
%
% Return    : the next message index
%       10,20,30,40,... is for the index of the message from WS to VDR
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
send_data_to_vdr(ID, Tel, FlowIdx, MsgBody, State) ->
    Socket = State#vdritem.socket,
    Pid = State#vdritem.pid,
    LinkPid = State#vdritem.linkpid,    
    case is_binary(MsgBody) of
        true ->
            MsgLen = byte_size(MsgBody),
            try
                if
                    ID == 16#8001 ->
                        MsgBodyTypeBytes = binary:part(MsgBody, MsgLen-3, 2),
                        if
                            MsgBodyTypeBytes =/= <<2,0>> andalso MsgBodyTypeBytes =/= <<0,2>> andalso MsgBodyTypeBytes =/= <<1,2>> ->
                                common:loginfo("Msg2VDR : ID ~p, Tel ~p, FlowIdx ~p, MsgType ~p, Msgbody ~p", [ID, Tel, FlowIdx, MsgBodyTypeBytes, MsgBody]);
                            true ->
                                ok
                        end;
                    true ->
                        common:loginfo("Msg2VDR : ID ~p, Tel ~p, FlowIdx ~p, Msgbody ~p", [ID, Tel, FlowIdx, MsgBody])
                end
            catch
                _:_ ->
                    ok
            end,
            if
                MsgLen > 24 ->
                    Header = binary:part(MsgBody, 0, 20),
                    if
                        Header == ?SUB_PACK_INDI_HEADER ->
                            Total = binary:part(MsgBody, 20, 2),
                            <<TotalInt:16>> = Total,
                            Index = binary:part(MsgBody, 22, 2),
                            <<IndexInt:16>> = Index,
                            Tail = binary:part(MsgBody, 24, MsgLen-24),
                            Msg = vdr_data_processor:create_final_msg(ID, Tel, FlowIdx, Tail, TotalInt, IndexInt),
                            do_send_data_to_vdr(Pid, Socket, Msg, ID, FlowIdx, LinkPid, State);
                        true ->
                            Msg = vdr_data_processor:create_final_msg(ID, Tel, FlowIdx, MsgBody),
                            do_send_data_to_vdr(Pid, Socket, Msg, ID, FlowIdx, LinkPid, State)
                    end;
                true ->
                    %common:loginfo("2"),
                    Msg = vdr_data_processor:create_final_msg(ID, Tel, FlowIdx, MsgBody),
                    %common:loginfo("4"),
                    do_send_data_to_vdr(Pid, Socket, Msg, ID, FlowIdx, LinkPid, State)
            end;
        _ ->
            try
                MsgBodyBin = list_to_binary(MsgBody),
                MsgLen = byte_size(MsgBody),
                MsgBodyTypeBytes = binary:part(MsgBodyBin, MsgLen-3, 2),
                if
                    ID == 16#8001 ->
                        if
                            MsgBodyTypeBytes =/= <<2,0>> andalso MsgBodyTypeBytes =/= <<0,2>> andalso MsgBodyTypeBytes =/= <<1,2>> ->
                                common:loginfo("Msg2VDR : ID ~p, Tel ~p, FlowIdx ~p, MsgType ~p, Msgbody ~p", [ID, Tel, FlowIdx, MsgBodyTypeBytes, MsgBody]);
                            true ->
                                ok
                        end;
                    true ->
                        common:loginfo("Msg2VDR : ID ~p, Tel ~p, FlowIdx ~p, Msgbody ~p", [ID, Tel, FlowIdx, MsgBody])
                end
            catch
                _:_ ->
                    ok
            end,
            Msg = vdr_data_processor:create_final_msg(ID, Tel, FlowIdx, MsgBody),
            do_send_data_to_vdr(Pid, Socket, Msg, ID, FlowIdx, LinkPid, State)
    end.

do_send_data_to_vdr(Pid, Socket, Msg, ID, FlowIdx, LinkPid, State) ->
    case is_list(Msg) of
        true ->
            do_send_msg2vdr(Pid, Socket, Msg, LinkPid, State);
        _ ->
            case is_binary(Msg) of
                true ->
                    if
                        Msg == <<>> andalso ID =/= 16#8702 ->
                            common:loginfo("~p send_data_to_vdr NULL final message : ID (~p), FlowIdx (~p), Msg (~p)", [Pid, ID, FlowIdx, Msg]);
                        Msg == <<>> andalso ID == 16#8702 ->
                            do_send_msg2vdr(Pid, Socket, Msg, LinkPid, State),
                            get_new_flow_index(FlowIdx);
                        Msg =/= <<>> ->
                            do_send_msg2vdr(Pid, Socket, Msg, LinkPid, State),
                            get_new_flow_index(FlowIdx)
                    end;
                _ ->
                    FlowIdx
            end
    end.

do_send_msg2vdr(Pid, Socket, Msg, LinkPid, State) when is_binary(Msg),
                                                        byte_size(Msg) > 0 ->
    LinkPid ! {Pid, vdrmsgsent},
    try
        MsgResult1 = binary:replace(Msg, <<125,1>>, <<255,254,253,252,251,250,251,252,253,254,255>>, [global]),
        FinalMsgResult1 = binary:replace(MsgResult1, <<125,2>>, <<245,244,243,242,241,240,241,242,243,244,245>>, [global]),
        MsgResult = binary:replace(FinalMsgResult1, <<255,254,253,252,251,250,251,252,253,254,255>>, <<125>>, [global]),
        FinalMsgResult = binary:replace(MsgResult, <<245,244,243,242,241,240,241,242,243,244,245>>, <<126>>, [global]),
        FinalMsgResultLen = byte_size(FinalMsgResult),
        MsgTypeBytes = binary:part(FinalMsgResult, 1, 2),
        if
            MsgTypeBytes == <<128, 1>> ->
                MsgRespTypeBytes = binary:part(FinalMsgResult, FinalMsgResultLen-5, 2),
                if
                    MsgRespTypeBytes =/= <<2,0>> andalso MsgRespTypeBytes =/= <<0,2>> andalso MsgRespTypeBytes =/= <<1,2>> ->
                        common:loginfo("Msg2VDR(~p, ~p) : ~p", [MsgTypeBytes, MsgRespTypeBytes, Msg]);
                    true ->
                        ok
                end;
            true ->
                common:loginfo("Msg2VDR(~p) : ~p", [MsgTypeBytes, Msg])
        end
    catch
        _:_ ->
            ok
    end,
    %common:loginfo("=>VDR : begin"),
    %safe_save_msg_4_vdr(Msg, State, false),
    %common:loginfo("=>VDR : ~p", [Msg]),
    save_msg_4_vdr(State, false, Msg),
    try
        %common:loginfo("Socket : ~p", [Socket]),
        gen_tcp:send(Socket, Msg)
    catch
        _:_ ->
            try
                gen_tcp:close(Socket)
            catch
                _:_ ->
                    ok
            end
    end;
    %VDRPid ! {Pid, Socket, Msg, noresp};
do_send_msg2vdr(_Pid, _Socket, Msg, _LinkPid, _State) when is_binary(Msg),
                                                          byte_size(Msg) < 1 ->
    ok;
do_send_msg2vdr(Pid, Socket, Msg, LinkPid, State) when is_list(Msg),
                                                       length(Msg) > 0 ->
    [H|T] = Msg,
    LinkPid ! {Pid, vdrmsgsent},
    try
        HResult1 = binary:replace(H, <<125,1>>, <<255,254,253,252,251,250,251,252,253,254,255>>, [global]),
        FinalHResult1 = binary:replace(HResult1, <<125,2>>, <<245,244,243,242,241,240,241,242,243,244,245>>, [global]),
        HResult = binary:replace(FinalHResult1, <<255,254,253,252,251,250,251,252,253,254,255>>, <<125>>, [global]),
        FinalHResult = binary:replace(HResult, <<245,244,243,242,241,240,241,242,243,244,245>>, <<126>>, [global]),
        FinalHResultLen = byte_size(FinalHResult),
        HTypeBytes = binary:part(FinalHResult, 1, 2),
        if
            HTypeBytes == <<128, 1>> ->
                HRespTypeBytes = binary:part(H, FinalHResultLen-5, 2),
                if
                    HRespTypeBytes =/= <<2,0>> andalso HRespTypeBytes =/= <<0,2>> andalso HRespTypeBytes =/= <<1,2>> ->
                        common:loginfo("Msg2VDR(~p, ~p) : ~p", [HTypeBytes, HRespTypeBytes, H]);
                    true ->
                        ok
                end;
            true ->
                common:loginfo("Msg2VDR(~p) : ~p", [HTypeBytes, H])
        end
    catch
        _:_ ->
            ok
    end,
    %common:loginfo("=>VDR : begin"),
    %safe_save_msg_4_vdr(H, State, false),
    %common:loginfo("=>VDR : ~p", [H]),
    save_msg_4_vdr(State, false, Msg),
    try
        %common:loginfo("Socket : ~p", [Socket]),
        gen_tcp:send(Socket, H)
    catch
        _:_ ->
            try
                gen_tcp:close(Socket)
            catch
                _:_ ->
                    ok
            end
    end,
    %VDRPid ! {Pid, Socket, H, noresp},
    do_send_msg2vdr(Pid, Socket, T, LinkPid, State);
do_send_msg2vdr(_Pid, _Socket, Msg, _LinkPid, _State) when is_list(Msg),
                                                          length(Msg) < 1 ->
    ok;
do_send_msg2vdr(_Pid, _Socket, _Msg, _LinkPid, _State) ->
    ok.

get_new_flow_index(FlowIdx) ->
    NewFlowIdx = FlowIdx + 1,
    NewFlowIdxRem = NewFlowIdx rem ?WS2VDRFREQ,
    case NewFlowIdxRem of
        0 ->
            NewFlowIdx + 1;
        _ ->
            FlowIdxRem = FlowIdx rem ?WS2VDRFREQ,
            case FlowIdxRem of
                0 ->
                    FlowIdx + ?WS2VDRFREQ;
                _ ->
                    NewFlowIdx
            end
    end.

