%
% vdr_data_processor.erl
%

-module(vdr_data_processor).

-include("../../include/header.hrl").

-export([process_vdr_data/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% This function should refer to the document on the mechanism
%
% Return :
%     {ok, State}
%     {warning, State}
%     {error, systemerror/vdrerror/invaliderror/exception, State}  
%
% MsgIdx  : VDR message index
% FlowIdx : Gateway message flow index
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
process_vdr_data(Socket, Data, State) -> 
    vdr_handler:logvdr(all, State, "data ~p", Data),
    case vdr_data_parser:process_data(State, Data) of
        {ok, HeadInfo, Msg, NewStateOrigin} ->
            %common:loginfo("New Driver ID when MSG : ~p", [NewState#vdritem.driverid]),
            %common:loginfo("DEBUG : vdr_data_parser:process_data is OK"),
            {ID, MsgIdx, Tel, CryptoType} = HeadInfo,
            NewState = NewStateOrigin#vdritem{encrypt=convert_crytotype(CryptoType)},
            if
                State#vdritem.id == undefined ->
                    %common:loginfo("Unknown VDR (~p) MSG ID (~p), MSG Index (~p), MSG Tel (~p)", [NewState#vdritem.addr, ID, MsgIdx, Tel]),
                    case ID of
                        16#100 ->
                            % Not complete
                            % Register VDR
                            %{Province, City, Producer, TermModel, TermID, LicColor, LicID} = Msg,
                            VehicleID = undefined,
                            DeviceID = undefined,
                            if
                                VehicleID == undefined andalso DeviceID == undefined -> % No vehicle or no VDR
                                    if
                                        VehicleID == undefined ->
                                            FlowIdx = NewState#vdritem.msgflownum,
                                            MsgBody = vdr_data_processor:create_reg_resp(MsgIdx, 2, empty),
                                            common:loginfo("~p sends VDR (~p) registration response (no such vechile in DB) : ~p", [NewState#vdritem.pid, NewState#vdritem.addr, MsgBody]),
                                            NewFlowIdx = send_data_to_vdr(16#8100, Tel, FlowIdx, MsgBody, NewState),
                                            
                                            % return error to terminate VDR connection
                                            {error, regerror, NewState#vdritem{msgflownum=NewFlowIdx}};
                                        true -> % DeviceID == undefined ->
                                            FlowIdx = NewState#vdritem.msgflownum,
                                            MsgBody = vdr_data_processor:create_reg_resp(MsgIdx, 4, empty),
                                            common:loginfo("~p sends VDR (~p) registration response (no such VDR in DB) : ~p", [NewState#vdritem.pid, NewState#vdritem.addr, MsgBody]),
                                            NewFlowIdx = send_data_to_vdr(16#8100, Tel, FlowIdx, MsgBody, NewState),
                                            
                                            % return error to terminate VDR connection
                                            {error, regerror, NewState#vdritem{msgflownum=NewFlowIdx}}
                                    end;
                                VehicleID == undefined orelse DeviceID == undefined -> % No vehicle or no VDR
                                    if
                                        VehicleID == undefined ->
                                            FlowIdx = NewState#vdritem.msgflownum,
                                            MsgBody = vdr_data_processor:create_reg_resp(MsgIdx, 2, empty),
                                            common:loginfo("~p sends VDR (~p) registration response (no such vechile in DB) : ~p", [NewState#vdritem.pid, NewState#vdritem.addr, MsgBody]),
                                            NewFlowIdx = send_data_to_vdr(16#8100, Tel, FlowIdx, MsgBody, NewState),
                                            
                                            % return error to terminate VDR connection
                                            {error, regerror, NewState#vdritem{msgflownum=NewFlowIdx}};
                                        true -> % DeviceID == undefined ->
                                            FlowIdx = NewState#vdritem.msgflownum,
                                            MsgBody = vdr_data_processor:create_reg_resp(MsgIdx, 4, empty),
                                            common:loginfo("~p sends VDR (~p) registration response (no such VDR in DB) : ~p", [NewState#vdritem.pid, NewState#vdritem.addr, MsgBody]),
                                            NewFlowIdx = send_data_to_vdr(16#8100, Tel, FlowIdx, MsgBody, NewState),
                                            
                                            % return error to terminate VDR connection
                                            {error, regerror, NewState#vdritem{msgflownum=NewFlowIdx}}
                                    end;
                                VehicleID =/= undefined andalso DeviceID =/= undefined -> % Vehicle registered and VDR registered
                                    if
                                        VehicleDeviceID =/= DeviceID ->
                                            FlowIdx = NewState#vdritem.msgflownum,
                                            MsgBody = vdr_data_processor:create_reg_resp(MsgIdx, 1, empty),
                                            common:loginfo("~p sends VDR (~p) registration response (vehicle registered) : ~p", [NewState#vdritem.pid, NewState#vdritem.addr, MsgBody]),
                                            NewFlowIdx = send_data_to_vdr(16#8100, Tel, FlowIdx, MsgBody, NewState),
                                            
                                            % return error to terminate VDR connection
                                            {error, regerror, NewState#vdritem{msgflownum=NewFlowIdx}};
                                        DeviceVehicleID =/= VehicleID ->
                                            FlowIdx = NewState#vdritem.msgflownum,
                                            MsgBody = vdr_data_processor:create_reg_resp(MsgIdx, 3, empty),
                                            common:loginfo("~p sends VDR (~p) registration response (VDR registered) : ~p", [NewState#vdritem.pid, NewState#vdritem.addr, MsgBody]),
                                            NewFlowIdx = send_data_to_vdr(16#8100, Tel, FlowIdx, MsgBody, NewState),
                                            
                                            % return error to terminate VDR connection
                                            {error, regerror, NewState#vdritem{msgflownum=NewFlowIdx}};
                                        true ->
                                            FlowIdx = NewState#vdritem.msgflownum,
                                            case is_binary(DeviceAuthenCode) of
                                                true ->
                                                    MsgBody = vdr_data_processor:create_reg_resp(MsgIdx, 0, DeviceAuthenCode),
                                                    %common:loginfo("~p sends VDR registration response (ok) (vehicle code : ~p) : ~p", [NewState#vdritem.pid, VehicleCode, MsgBody]),
                                                    NewFlowIdx = send_data_to_vdr(16#8100, Tel, FlowIdx, MsgBody, NewState),
                                                    
                                                    update_reg_install_time(DeviceID, DeviceRegTime, VehicleID, VehicleDeviceInstallTime, NewState),        
                                                    
                                                    % return error to terminate VDR connection
                                                    {ok, NewState#vdritem{msgflownum=NewFlowIdx, msg2vdr=[], msg=[], req=[], alarm=0, alarmlist=[], state=0, statelist=[], tel=Tel}};
                                                false ->
                                                    MsgBody = vdr_data_processor:create_reg_resp(MsgIdx, 0, list_to_binary(DeviceAuthenCode)),
                                                    %common:loginfo("~p sends VDR registration response (ok) (vehicle code : ~p) : ~p", [NewState#vdritem.pid, VehicleCode, MsgBody]),
                                                    NewFlowIdx = send_data_to_vdr(16#8100, Tel, FlowIdx, MsgBody, NewState),
                                                    
                                                    update_reg_install_time(DeviceID, DeviceRegTime, VehicleID, VehicleDeviceInstallTime, NewState),        
                                                    
                                                    % return error to terminate VDR connection
                                                    {ok, NewState#vdritem{msgflownum=NewFlowIdx, msg2vdr=[], msg=[], req=[], tel=Tel}}
                                            end
                                    end;
                                VehiclID =/= undefined andalso DeviceID == undefined -> % Vehicle registered
                                    if
                                        VehicleDeviceID =/= DeviceID ->
                                            FlowIdx = NewState#vdritem.msgflownum,
                                            MsgBody = vdr_data_processor:create_reg_resp(MsgIdx, 1, empty),
                                            common:loginfo("~p sends VDR (~p) registration response (vehicle registered) : ~p", [NewState#vdritem.pid, NewState#vdritem.addr, MsgBody]),
                                            NewFlowIdx = send_data_to_vdr(16#8100, Tel, FlowIdx, MsgBody, NewState),
                                            
                                            % return error to terminate VDR connection
                                            {error, regerror, NewState#vdritem{msgflownum=NewFlowIdx}};
                                        true ->
                                            VDRVehicleIDSql = list_to_binary([<<"update device set vehicle_id='">>,
                                                                              common:integer_to_binary(VehicleID),
                                                                              <<"' where id=">>,
                                                                              common:integer_to_binary(DeviceID)]),
                                            % Should we check the update result?
                                            send_sql_to_db_nowait(conn, VDRVehicleIDSql, NewState),
                                            
                                            update_reg_install_time(DeviceID, DeviceRegTime, VehicleID, VehicleDeviceInstallTime, NewState),        

                                            FlowIdx = NewState#vdritem.msgflownum,
                                            MsgBody = vdr_data_processor:create_reg_resp(MsgIdx, 0, list_to_binary(DeviceAuthenCode)),
                                            %common:loginfo("~p sends VDR (~p) registration response (ok) (vehicle code : ~p) : ~p", [NewState#vdritem.pid, NewState#vdritem.addr, VehicleCode, MsgBody]),
                                            NewFlowIdx = send_data_to_vdr(16#8100, Tel, FlowIdx, MsgBody, NewState),
                                            
                                            % return error to terminate VDR connection
                                            {ok, NewState#vdritem{msgflownum=NewFlowIdx, msg2vdr=[], msg=[], req=[], alarm=0, alarmlist=[], state=0, statelist=[], tel=Tel}}
                                    end;
                                VehicleID == undefined andalso VehicleID =/= undefined -> % Vehicle registered
                                    if
                                        DeviceVehicleID =/= VehicleID ->
                                            FlowIdx = NewState#vdritem.msgflownum,
                                            MsgBody = vdr_data_processor:create_reg_resp(MsgIdx, 3, empty),
                                            common:loginfo("~p sends VDR (~p) registration response (VDR registered) : ~p", [NewState#vdritem.pid, NewState#vdritem.addr, MsgBody]),
                                            NewFlowIdx = send_data_to_vdr(16#8100, Tel, FlowIdx, MsgBody, NewState),
                                            
                                            % return error to terminate VDR connection
                                            {error, regerror, NewState#vdritem{msgflownum=NewFlowIdx}};
                                        true ->
                                            VehicleVDRIDSql = list_to_binary([<<"update vehicle set device_id='">>,
                                                                              common:integer_to_binary(DeviceID),
                                                                              <<"' where id=">>,
                                                                              common:integer_to_binary(VehicleID)]),
                                            send_sql_to_db_nowait(conn, VehicleVDRIDSql, NewState),
                                            
                                            update_reg_install_time(DeviceID, DeviceRegTime, VehicleID, VehicleDeviceInstallTime, NewState),        

                                            FlowIdx = NewState#vdritem.msgflownum,
                                            MsgBody = vdr_data_processor:create_reg_resp(MsgIdx, 0, list_to_binary(DeviceAuthenCode)),
                                            %common:loginfo("~p sends VDR (~p) registration response (ok) (vehicle code : ~p) : ~p", [NewState#vdritem.pid, NewState#vdritem.addr, VehicleCode, MsgBody]),
                                            NewFlowIdx = send_data_to_vdr(16#8100, Tel, FlowIdx, MsgBody, NewState),

                                            % return error to terminate VDR connection
                                            {ok, NewState#vdritem{msgflownum=NewFlowIdx, msg2vdr=[], msg=[], req=[], alarm=0, alarmlist=[], state=0, statelist=[], tel=Tel}}
                                    end;
                                VehicleID == undefined andalso VehicleID == undefined ->
                                    VDRVehicleIDSql = list_to_binary([<<"update device set vehicle_id='">>,
                                                                      common:integer_to_binary(VehicleID),
                                                                      <<"' where id=">>,
                                                                      common:integer_to_binary(DeviceID)]),
                                    send_sql_to_db_nowait(conn, VDRVehicleIDSql, NewState),
                                    
                                    VehicleVDRIDSql = list_to_binary([<<"update vehicle set device_id='">>,
                                                                      common:integer_to_binary(DeviceID),
                                                                      <<"' where id=">>,
                                                                      common:integer_to_binary(VehicleID)]),
                                    send_sql_to_db_nowait(conn, VehicleVDRIDSql, NewState),

                                    update_reg_install_time(DeviceID, DeviceRegTime, VehicleID, VehicleDeviceInstallTime, NewState),      

                                    FlowIdx = NewState#vdritem.msgflownum,
                                    MsgBody = vdr_data_processor:create_reg_resp(MsgIdx, 0, list_to_binary(DeviceAuthenCode)),
                                    %common:loginfo("~p sends VDR (~p) registration response (ok) (vehicle code : ~p) : ~p", [NewState#vdritem.pid, NewState#vdritem.addr, VehicleCode, MsgBody]),
                                    NewFlowIdx = send_data_to_vdr(16#8100, Tel, FlowIdx, MsgBody, NewState),
                                    
                                    {ok, NewState#vdritem{msgflownum=NewFlowIdx, msg2vdr=[], msg=[], req=[], alarm=0, alarmlist=[], state=0, statelist=[], tel=Tel}};
                                true -> % Impossible condition
                                    {error, regerror, NewState}
                            end;
                        16#102 ->
                            % VDR Authentication
                            {Auth} = Msg,
                            case check_vdrdbtable_auth(NewState, Auth) of
                                {ok, VDRDBItem} ->
                                    %common:loginfo("Local authen passes"),
                                    %common:loginfo("VDR DB Item : ~p", [VDRDBItem]),
                                    % DriverID is undefined because DB is designed in thisway
                                    {vdrdbitem, VDRAuthenCode, VDRID, VDRSerialNo, VehicleCode, VehicleID, DriverID} = VDRDBItem,
                                    save_online_msg(VDRID, NewState),
                                    if
                                        VehicleID == undefined orelse VehicleCode==undefined ->
                                            {error, autherror, NewState};
                                        true ->
                                            %common:loginfo("VDR Auth Code : ~p", [VDRAuthenCode]),
                                            CertCode = get_driver_cc_by_vdr_auth_code(NewState, VDRAuthenCode),
                                            %CertCode = common:get_list_from_binary(CertCodeBin),
                                            %common:loginfo("Get certificate code ~p by driver id ~p", [CertCode, DriverID]),
                                            disconn_socket_by_vehicle_id(VehicleID),
                                            SockVdrList = ets:lookup(vdrtable, Socket),
                                            case length(SockVdrList) of
                                                1 ->
                                                    case check_alarm(NewState, VehicleID) of
                                                        {ok, Alarms} ->
                                                            case wsock_data_parser:create_term_online([VehicleID]) of
                                                                {ok, WSUpdate} ->
                                                                    %common:loginfo("VDR (~p) WS : ~p~n~p", [State#vdritem.addr, WSUpdate, list_to_binary(WSUpdate)]),
                                                                    send_msg_to_ws_nowait(WSUpdate, NewState),
                                                                    
                                                                    FlowIdx = NewState#vdritem.msgflownum,
                                                                    MsgBody = vdr_data_processor:create_gen_resp(ID, MsgIdx, ?T_GEN_RESP_OK),
                                                                    %common:loginfo("~p sends VDR (~p) response for 16#102 (ok) : ~p", [NewState#vdritem.pid, NewState#vdritem.addr, MsgBody]),
                                                                    MidState = NewState#vdritem{id=VDRID,
                                                                                                  serialno=binary_to_list(VDRSerialNo),
                                                                                                  auth=binary_to_list(VDRAuthenCode),
                                                                                                  vehicleid=VehicleID,
                                                                                                  vehiclecode=binary_to_list(VehicleCode),
                                                                                                  driverid=DriverID,
                                                                                                  drivercertcode=CertCode,
                                                                                                  msg2vdr=[], msg=[], req=[],
                                                                                                  alarm=0, alarmlist=Alarms, state=0, statelist=[], tel=Tel},
                                                                    NewFlowIdx = send_data_to_vdr(16#8001, Tel, FlowIdx, MsgBody, MidState),
                                        
                                                                    FinalState = MidState#vdritem{msgflownum=NewFlowIdx},
                                                                    VDRTablePid = FinalState#vdritem.vdrtablepid,
                                                                    common:send_vdr_table_operation(VDRTablePid, {self(), insert, FinalState, noresp}),
                                                                    
                                                                    {ok, FinalState};
                                                                _ ->
                                                                    {error, autherror, NewState}
                                                            end;
                                                        empty ->
                                                            case wsock_data_parser:create_term_online([VehicleID]) of
                                                                {ok, WSUpdate} ->
                                                                    %common:loginfo("VDR (~p) WS : ~p~n~p", [State#vdritem.addr, WSUpdate, list_to_binary(WSUpdate)]),
                                                                    send_msg_to_ws_nowait(WSUpdate, NewState),
                                                            
                                                                    FlowIdx = NewState#vdritem.msgflownum,
                                                                    MsgBody = vdr_data_processor:create_gen_resp(ID, MsgIdx, ?T_GEN_RESP_OK),
                                                                    %common:loginfo("~p sends VDR (~p) response for 16#102 (ok) : ~p", [State#vdritem.pid, State#vdritem.addr, MsgBody]),
                                                                    MidState = NewState#vdritem{id=VDRID, 
                                                                                                  serialno=binary_to_list(VDRSerialNo),
                                                                                                  auth=binary_to_list(VDRAuthenCode),
                                                                                                  vehicleid=VehicleID,
                                                                                                  vehiclecode=binary_to_list(VehicleCode),
                                                                                                  driverid=DriverID,
                                                                                                  drivercertcode=CertCode,
                                                                                                  msg2vdr=[], msg=[], req=[],
                                                                                                  alarm=0, alarmlist=[], state=0, statelist=[], tel=Tel},
                                                                    NewFlowIdx = send_data_to_vdr(16#8001, Tel, FlowIdx, MsgBody, MidState),
                                                                
                                                                    FinalState = MidState#vdritem{msgflownum=NewFlowIdx},
                                                                    VDRTablePid = FinalState#vdritem.vdrtablepid,
                                                                    common:send_vdr_table_operation(VDRTablePid, {self(), insert, FinalState, noresp}),
                                        
                                                                    {ok, FinalState};
                                                                _ ->
                                                                    {error, autherror, NewState}
                                                            end
                                                    end;
                                                _ ->
                                                    % vdrtable error
                                                    {error, autherror, NewState}
                                            end
                                    end;
                                error ->
                                    %common:loginfo("Local authen fails"),
                                    case create_sql_from_vdr(HeadInfo, Msg, NewState) of
                                        {ok, Sql} ->
                                            SqlResp = send_sql_to_db(conn, Sql, NewState),
                                            case extract_db_resp(SqlResp) of
                                                {ok, empty} ->
                                                    {error, autherror, NewState};
                                                {ok, [Rec]} ->
                                                    % "id" is NOT NULL & UNIQUE, so it cannot be null. However it can be undefined because vehicle table device_id may don't be euqual to device table id 
                                                    {<<"vehicle">>, <<"code">>, VehicleCode} = get_record_field(<<"vehicle">>, Rec, <<"code">>),
                                                    if
                                                        VehicleCode =/= undefined andalso binary_part(VehicleCode, 0, 1) == <<"?">> ->
                                                            common:loginfo("VDR (~p) Vehicle Code has invalid character \"?\" and will be disconnected : ~p", [State#vdritem.addr, VehicleCode]),
                                                            {error, charerror, NewState};
                                                        true ->
                                                            {VDRID, VDRSerialNo, VDRAuthenCode, VehicleCode, VehicleID, DriverID} = get_record_column_info(Rec),
                                                            if
                                                                VehicleID == undefined orelse VehicleCode==undefined ->
                                                                    {error, autherror, NewState};
                                                                true ->
                                                                    % Update VDR DB hash
                                                                    Pid = State#vdritem.pid,
                                                                    DBOperationPid = State#vdritem.dboperid,
                                                                    VDRDBItem = #vdrdbitem{authencode=VDRAuthenCode, 
                                                                                           vdrid=VDRID, 
                                                                                           vdrserialno=VDRSerialNo,
                                                                                           vehiclecode=VehicleCode,
                                                                                           vehicleid=VehicleID,
                                                                                           driverid=DriverID},
                                                                    DBOperationPid ! {Pid, insert, vdrdbtable, VDRDBItem, noresp},
                                                                    
                                                                    save_online_msg(VDRID, NewState),
                                                                    
                                                                    disconn_socket_by_vehicle_id(VehicleID),
                                                                    SockVdrList = ets:lookup(vdrtable, Socket),
                                                                    case length(SockVdrList) of
                                                                        1 ->
                                                                            case check_alarm(NewState, VehicleID) of
                                                                                {ok, Alarms} ->
                                                                                    case wsock_data_parser:create_term_online([VehicleID]) of
                                                                                        {ok, WSUpdate} ->
                                                                                            %common:loginfo("VDR (~p) WS : ~p~n~p", [State#vdritem.addr, WSUpdate, list_to_binary(WSUpdate)]),
                                                                                            send_msg_to_ws_nowait(WSUpdate, NewState),
                                                                                            
                                                                                            FlowIdx = NewState#vdritem.msgflownum,
                                                                                            MsgBody = vdr_data_processor:create_gen_resp(ID, MsgIdx, ?T_GEN_RESP_OK),
                                                                                            %common:loginfo("~p sends VDR (~p) response for 16#102 (ok) : ~p", [NewState#vdritem.pid, NewState#vdritem.addr, MsgBody]),
                                                                                            MidState = NewState#vdritem{id=VDRID,
                                                                                                                          serialno=binary_to_list(VDRSerialNo),
                                                                                                                          auth=binary_to_list(VDRAuthenCode),
                                                                                                                          vehicleid=VehicleID,
                                                                                                                          vehiclecode=binary_to_list(VehicleCode),
                                                                                                                          driverid=DriverID,
                                                                                                                          msg2vdr=[], msg=[], req=[],
                                                                                                                          alarm=0, alarmlist=Alarms, state=0, statelist=[], tel=Tel},
                                                                                            NewFlowIdx = send_data_to_vdr(16#8001, Tel, FlowIdx, MsgBody, MidState),
                                                                
                                                                                            FinalState = MidState#vdritem{msgflownum=NewFlowIdx},
                                                                                            VDRTablePid = FinalState#vdritem.vdrtablepid,
                                                                                            common:send_vdr_table_operation(VDRTablePid, {self(), insert, FinalState, noresp}),
                                                                                            
                                                                                            {ok, FinalState};
                                                                                        _ ->
                                                                                            {error, autherror, NewState}
                                                                                    %end
                                                                                    end;
                                                                                empty ->
                                                                                    case wsock_data_parser:create_term_online([VehicleID]) of
                                                                                        {ok, WSUpdate} ->
                                                                                            %common:loginfo("VDR (~p) WS : ~p~n~p", [State#vdritem.addr, WSUpdate, list_to_binary(WSUpdate)]),
                                                                                            send_msg_to_ws_nowait(WSUpdate, NewState),
                                                                                    
                                                                                            FlowIdx = NewState#vdritem.msgflownum,
                                                                                            MsgBody = vdr_data_processor:create_gen_resp(ID, MsgIdx, ?T_GEN_RESP_OK),
                                                                                            %common:loginfo("~p sends VDR (~p) response for 16#102 (ok) : ~p", [State#vdritem.pid, State#vdritem.addr, MsgBody]),
                                                                                            MidState = NewState#vdritem{id=VDRID, 
                                                                                                                          serialno=binary_to_list(VDRSerialNo),
                                                                                                                          auth=binary_to_list(VDRAuthenCode),
                                                                                                                          vehicleid=VehicleID,
                                                                                                                          vehiclecode=binary_to_list(VehicleCode),
                                                                                                                          driverid=DriverID,
                                                                                                                          msg2vdr=[], msg=[], req=[],
                                                                                                                          alarm=0, alarmlist=[], state=0, statelist=[], tel=Tel},
                                                                                            NewFlowIdx = send_data_to_vdr(16#8001, Tel, FlowIdx, MsgBody, MidState),
                                                                                        
                                                                                            FinalState = MidState#vdritem{msgflownum=NewFlowIdx},
                                                                                            VDRTablePid = FinalState#vdritem.vdrtablepid,
                                                                                            common:send_vdr_table_operation(VDRTablePid, {self(), insert, FinalState, noresp}),
                                                                
                                                                                            {ok, FinalState};
                                                                                        _ ->
                                                                                            {error, autherror, NewState}
                                                                                    end
                                                                            end;
                                                                        _ ->
                                                                            % vdrtable error
                                                                            {error, autherror, NewState}
                                                                    end
                                                            end
                                                    end;
                                                _ ->
                                                    % DB includes no record with the given authen_code
                                                    {error, autherror, NewState}
                                            end;
                                        _ ->
                                            % Authentication fails
                                            {error, autherror, NewState}
                                    end
                            end;
                        true ->
                            common:loginfo("Invalid common message from unknown/unregistered/unauthenticated VDR (~p) (id:~p, serialno:~p, authen_code:~p, vehicleid:~p, vehiclecode:~p) MSG ID : ~p", [NewState#vdritem.addr, NewState#vdritem.id, NewState#vdritem.serialno, NewState#vdritem.auth, NewState#vdritem.vehicleid, NewState#vdritem.vehiclecode, ID]),
                            % Unauthorized/Unregistered VDR can only accept 16#100/16#102
                            {error, unautherror, State}
                    end;
                true ->
                    if
                        ID =/= 16#2 andalso ID =/= 16#200 ->
                            common:loginfo("VDR (~p) (id:~p, serialno:~p, authen_code:~p, vehicleid:~p, vehiclecode:~p) MSG ID (~p), MSG Index (~p), MSG Tel (~p)~n~p",
                                           [NewState#vdritem.addr, 
                                            NewState#vdritem.id, 
                                            NewState#vdritem.serialno, 
                                            NewState#vdritem.auth, 
                                            NewState#vdritem.vehicleid, 
                                            NewState#vdritem.vehiclecode, 
                                            ID, MsgIdx, Tel, Data]);
                        true ->
                            ok
                    end,
                    case ID of
                        16#1 ->     % VDR general response
                            {RespFlowIdx, RespID, Res} = Msg,
                            
                            % Process reponse from VDR here
                            common:loginfo("Gateway (~p) receives VDR (~p) general response (16#1) : RespFlowIdx (~p), RespID (~p), Res (~p)", [State#vdritem.pid, State#vdritem.addr, RespFlowIdx, RespID, Res]),
                            
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
                                    VehicleID = NewState#vdritem.vehicleid,

                                    [VDRItem] = ets:lookup(vdrtable, Socket),
                                    MsgList = VDRItem#vdritem.msgws2vdr,
                                    common:loginfo("Stored MSG from WS to VDR stored in GW : ~p", [MsgList]),
                                    
                                    TargetList = [{WSID, WSFlowIdx, WSValue} || {WSID, WSFlowIdx, WSValue} <- MsgList, WSID == RespID],
                                    case length(TargetList) of
                                        1 ->
                                            [{TargetWSID, TargetWSFlowIdx, _WSValue}] = TargetList,
                                            {ok, WSUpdate} = wsock_data_parser:create_gen_resp(TargetWSFlowIdx,
                                                                                               TargetWSID,
                                                                                               [VehicleID],
                                                                                               Res),
                                            %common:loginfo("Gateway receives VDR (~p) response to WS request ~p : ~p", [NewState#vdritem.addr, RespID, WSUpdate]),
                                            send_msg_to_ws_nowait(WSUpdate, NewState);
                                        ItemCount ->
                                            common:loginfo("(FATAL) vdritem.msgws2vdr has ~p item(s) for wsid ~p", [ItemCount, RespID])
                                    end,
                                    
                                    NewMsgList = [{WSID, WSFlowIdx, WSValue} || {WSID, WSFlowIdx, WSValue} <- MsgList, WSID =/= RespID],
                                    common:loginfo("New stored MSG from WS to VDR stored in GW : ~p", [NewMsgList]),
                                    VDRTablePid = VDRItem#vdritem.vdrtablepid,
                                    NewVDRItem = VDRItem#vdritem{msgws2vdr=NewMsgList},
                                    common:send_vdr_table_operation(VDRTablePid, {self(), insert, NewVDRItem, noresp}),
                                    
                                    {ok, NewState#vdritem{msgws2vdr=NewMsgList}};
                                true ->
                                    {ok, NewState}
                            end;
                        16#2 ->     % VDR pulse
                            % Nothing to do here
                            %{} = Msg,
                            FlowIdx = NewState#vdritem.msgflownum,
                            MsgBody = vdr_data_processor:create_gen_resp(ID, MsgIdx, ?T_GEN_RESP_OK),
                            %common:loginfo("Gateway (~p) sends VDR (~p) response for 16#2 (Pulse) : ~p", [State#vdritem.pid, State#vdritem.addr, MsgBody]),
                            NewFlowIdx = send_data_to_vdr(16#8001, Tel, FlowIdx, MsgBody, NewState),

                            {ok, NewState#vdritem{msgflownum=NewFlowIdx}};
                        16#3 ->     % VDR unregistration
                            %{} = Msg,
                            Auth = NewState#vdritem.auth,
                            ID = NewState#vdritem.id,
                            case create_sql_from_vdr(HeadInfo, {ID, Auth}, NewState) of
                                {ok, Sql} ->
                                    send_sql_to_db_nowait(conn, Sql, NewState),
        
                                    FlowIdx = NewState#vdritem.msgflownum,
                                    MsgBody = vdr_data_processor:create_gen_resp(ID, MsgIdx, ?T_GEN_RESP_OK),
                                    %common:loginfo("~p sends VDR (~p) response for 16#3 (Position) : ~p", [State#vdritem.pid, NewState#vdritem.addr, MsgBody]),
                                    NewFlowIdx = send_data_to_vdr(16#8001, Tel, FlowIdx, MsgBody, NewState),
        
                                    % return error to terminate connection with VDR
                                    {error, exiterror, NewState#vdritem{msgflownum=NewFlowIdx}};
                                _ ->
                                    {error, vdrerror, NewState}
                            end;
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
                            %common:loginfo("New Driver ID when MSG : ~p", [NewState#vdritem.driverid]),
                            process_pos_info(ID, MsgIdx, HeadInfo, Msg, NewState);
                        16#201 ->
                            case Msg of
                                {_RespFlowIdx, PosInfo} ->
                                    VehicleID = NewState#vdritem.vehicleid,                            
        
                                    [VDRItem] = ets:lookup(vdrtable, Socket),
                                    MsgList = VDRItem#vdritem.msgws2vdr,
                                    common:loginfo("Stored MSG from WS to VDR stored in GW : ~p", [MsgList]),
        
                                    TargetList = [{WSID, WSFlowIdx, WSValue} || {WSID, WSFlowIdx, WSValue} <- MsgList, WSID == 16#8201],
                                    case length(TargetList) of
                                        1 ->
                                            [{16#8201, TargetWSFlowIdx, _WSValue}] = TargetList,
                                            {ok, WSUpdate} = wsock_data_parser:create_gen_resp(TargetWSFlowIdx,
                                                                                               16#8201,
                                                                                               [VehicleID],
                                                                                               0),
                                            %common:loginfo("Gateway receives VDR (~p) response to WS request ~p : ~p", [NewState#vdritem.addr, RespID, WSUpdate]),
                                            send_msg_to_ws_nowait(WSUpdate, NewState);
                                        ItemCount ->
                                            common:loginfo("(FATAL) vdritem.msgws2vdr has ~p item(s) for wsid ~p", [ItemCount, 16#8201])
                                    end,
                                            
                                    NewMsgList = [{WSID, WSFlowIdx, WSValue} || {WSID, WSFlowIdx, WSValue} <- MsgList, WSID =/= 16#8201],
                                    common:loginfo("New stored MSG from WS to VDR stored in GW : ~p", [NewMsgList]),
                                    VDRTablePid = VDRItem#vdritem.vdrtablepid,
                                    NewVDRItem = VDRItem#vdritem{msgws2vdr=NewMsgList},
                                    common:send_vdr_table_operation(VDRTablePid, {self(), insert, NewVDRItem, noresp}),

                                    process_pos_info(ID, MsgIdx, HeadInfo, PosInfo, NewState#vdritem{msgws2vdr=NewMsgList});
                                _ ->
                                    {error, vdrerror, NewState}
                            end;
                        16#301 ->
                            {_Id} = Msg,
                            
                            {ok, NewState};
                        16#302 ->
                            {_AnswerFlowIdx, AnswerID} = Msg,

                            VehicleID = NewState#vdritem.vehicleid,                            

                            [VDRItem] = ets:lookup(vdrtable, Socket),
                            MsgList = VDRItem#vdritem.msgws2vdr,
                            common:loginfo("Stored MSG from WS to VDR stored in GW : ~p", [MsgList]),

                            TargetList = [{WSID, WSFlowIdx, WSValue} || {WSID, WSFlowIdx, WSValue} <- MsgList, WSID == 16#8302],
                            case length(TargetList) of
                                1 ->
                                    [{16#8302, TargetWSFlowIdx, _WSValue}] = TargetList,
                                    {ok, WSUpdate} = wsock_data_parser:create_term_answer(TargetWSFlowIdx,
                                                                                          [VehicleID],
                                                                                          [AnswerID]),
                                    %common:loginfo("Gateway receives VDR (~p) response to WS request ~p : ~p", [NewState#vdritem.addr, 16#8302, WSUpdate]),
                                    send_msg_to_ws_nowait(WSUpdate, NewState);
                                ItemCount ->
                                    common:loginfo("(FATAL) vdritem.msgws2vdr has ~p item(s) for wsid ~p", [ItemCount, 16#8302])
                            end,
                                    
                            NewMsgList = [{WSID, WSFlowIdx, WSValue} || {WSID, WSFlowIdx, WSValue} <- MsgList, WSID =/= 16#8302],
                            common:loginfo("New stored MSG from WS to VDR stored in GW : ~p", [NewMsgList]),
                            VDRTablePid = VDRItem#vdritem.vdrtablepid,
                            NewVDRItem = VDRItem#vdritem{msgws2vdr=NewMsgList},
                            common:send_vdr_table_operation(VDRTablePid, {self(), insert, NewVDRItem, noresp}),
                            
                            {ok, NewState#vdritem{msgws2vdr=NewMsgList}};
                        16#303 ->
                            {_MsgType, _POC} = Msg,
                            
                            {ok, NewState};
                        16#500 ->
                            {_FlowNum, Resp} = Msg,
                            {Info, _AppInfo} = Resp,
                            [_AlarmSym, InfoState, _Lat, _Lon, _Height, _Speed, _Direction, _Time] = Info,

                            VehicleID = NewState#vdritem.vehicleid,                            

                            [VDRItem] = ets:lookup(vdrtable, Socket),
                            MsgList = VDRItem#vdritem.msgws2vdr,
                            common:loginfo("Stored MSG from WS to VDR stored in GW : ~p", [MsgList]),

                            TargetList = [{WSID, WSFlowIdx, WSValue} || {WSID, WSFlowIdx, WSValue} <- MsgList, WSID == 16#8500],
                            case length(TargetList) of
                                1 ->
                                    [{16#8500, TargetWSFlowIdx, WSValue}] = TargetList,
                                    FlagBit = WSValue band 1,
                                    ResBit = InfoState band 16#1000,
                                    NewResBit = ResBit bsr 12,
                                    % Not very clear about the latest parameter
                                    {ok, WSUpdate} = wsock_data_parser:create_vehicle_ctrl_answer(TargetWSFlowIdx,
                                                                                                  FlagBit bxor NewResBit,
                                                                                                  [VehicleID],
                                                                                                  [InfoState]),
                                    %common:loginfo("Gateway receives VDR (~p) response to WS request ~p : ~p", [NewState#vdritem.addr, 16#8500, WSUpdate]),
                                    send_msg_to_ws_nowait(WSUpdate, NewState);
                                ItemCount ->
                                    common:loginfo("(FATAL) vdritem.msgws2vdr has ~p item(s) for wsid ~p", [ItemCount, 16#8500])
                            end,
                                    
                            NewMsgList = [{WSID, WSFlowIdx, WSValue} || {WSID, WSFlowIdx, WSValue} <- MsgList, WSID =/= 16#8500],
                            common:loginfo("New stored MSG from WS to VDR stored in GW : ~p", [NewMsgList]),
                            VDRTablePid = VDRItem#vdritem.vdrtablepid,
                            NewVDRItem = VDRItem#vdritem{msgws2vdr=NewMsgList},
                            common:send_vdr_table_operation(VDRTablePid, {self(), insert, NewVDRItem, noresp}),
                            
                            {ok, NewState#vdritem{msgws2vdr=NewMsgList}};
                        16#700 ->
                            {_Number, _OrderWord, _DB} = Msg,
                            
                            {ok, NewState};
                        16#701 ->
                            {_Length, _Content} = Msg,
                            
                            {ok, NewState};
                        16#702 ->
                            %common:loginfo("Driver Cert Code : ~p", [NewState#vdritem.drivercertcode]),
                            case create_sql_from_vdr(HeadInfo, Msg, NewState) of
                                {ok, Sql} ->
                                    send_sql_to_db_nowait(conn, Sql, NewState),

                                    FlowIdx = NewState#vdritem.msgflownum,
                                    MsgBody = vdr_data_processor:create_gen_resp(ID, MsgIdx, ?T_GEN_RESP_OK),
                                    %common:loginfo("~p sends VDR driver info update response (ok) : ~p", [NewState#vdritem.pid, MsgBody]),
                                    NewFlowIdx = send_data_to_vdr(16#8001, Tel, FlowIdx, MsgBody, NewState),
                                    
                                    MsgLength = tuple_size(Msg),
                                    DriverTablePid = NewState#vdritem.drivertablepid,
                                    VDRAuthCode = common:get_binary_from_list(NewState#vdritem.auth),
                                    SelfPid = NewState#vdritem.pid,
                                    %common:loginfo("0x702 message body (~p) : ~p", [MsgLength, Msg]),
                                    if
                                        MsgLength == 9 ->
                                            {_DrvState, _Time, _IcReadResult, _NameLen, _N, C, _OrgLen, _O, _Validity} = Msg,
                                            %common:loginfo("Certificate Code : ~p", [C]),
                                            CBin = common:get_binary_from_list(C),
                                            DriverTablePid ! {SelfPid, checkcc, {CBin, VDRAuthCode}},
                                            receive
                                                {_Pid, {DriverInfoCount, DriverIDCC}} ->
                                                    %common:loginfo("Driver item count : ~p", [DriverInfoCount]),
                                                    if
                                                        DriverInfoCount == 1 ->
                                                            {ok, NewState#vdritem{msgflownum=NewFlowIdx,driverid=DriverIDCC,drivercertcode=CBin}};
                                                        true ->
                                                            DriverSql = list_to_binary([<<"select * from driver where certificate_code='">>, common:get_binary_from_list(C), <<"'">>]),
                                                            %common:loginfo("0x702 - Driver table query SQL : ~p", [DriverSql]),
                                                            DriverSqlResult = send_sql_to_db(conn, DriverSql , NewState),
                                                            case vdr_handler:extract_db_resp(DriverSqlResult) of
                                                                error ->
                                                                    common:loginfo("Message server cannot read driver table"),
                                                                    {ok, NewState#vdritem{msgflownum=NewFlowIdx,driverid=DriverIDCC,drivercertcode=CBin}};
                                                                {ok, empty} ->
                                                                    common:loginfo("Message server get no driver info from driver table for certificate_code : ~p", [C]),
                                                                    {ok, NewState#vdritem{msgflownum=NewFlowIdx,driverid=DriverIDCC,drivercertcode=CBin}};
                                                                {ok, Records} ->
                                                                    RecordCount = length(Records),
                                                                    if
                                                                        RecordCount == 1 ->
                                                                            [Record] = Records,
                                                                            {<<"driver">>, <<"id">>, DriverID} = vdr_handler:get_record_field(<<"driver">>, Record, <<"id">>),
                                                                            {<<"driver">>, <<"license_no">>, LicNo} = vdr_handler:get_record_field(<<"driver">>, Record, <<"license_no">>),
                                                                            {<<"driver">>, <<"certificate_code">>, CertCode} = vdr_handler:get_record_field(<<"driver">>, Record, <<"certificate_code">>),
                                                                            DriverTablePid ! {SelfPid, chkinsdriverinfo, {DriverID, LicNo, CertCode, VDRAuthCode}},
                                                                            {ok, NewState#vdritem{msgflownum=NewFlowIdx,driverid=DriverID,drivercertcode=CertCode}};
                                                                        true ->
                                                                            common:loginfo("Message server get ~p driver info from driver table for certificate_code : ~p", [RecordCount, C]),
                                                                            {ok, NewState#vdritem{msgflownum=NewFlowIdx,driverid=DriverIDCC,drivercertcode=CBin}}
                                                                    end
                                                            end
                                                    end
                                            after ?PROC_RESP_TIMEOUT ->
                                                    common:loginfo("Message server fails to get driver info from driver table for certificate_code (~p) : timeout", [C]),
                                                    {ok, NewState#vdritem{msgflownum=NewFlowIdx,driverid=undefined,drivercertcode=CBin}}
                                            end;
                                        MsgLength == 2 ->
                                            DriverTablePid ! {SelfPid, offwork, NewState#vdritem.drivercertcode},
                                            {ok, NewState#vdritem{msgflownum=NewFlowIdx,driverid=undefined,drivercertcode=undefined}};
                                        true ->
                                            {ok, NewState#vdritem{msgflownum=NewFlowIdx}}
                                    end;                                    
                                _ ->
                                    {error, vdrerror, NewState}
                            end;                                
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
                            {_RespIdx, Res, _ActLen, List} = Msg,

                            VehicleID = NewState#vdritem.vehicleid,                            

                            [VDRItem] = ets:lookup(vdrtable, Socket),
                            MsgList = VDRItem#vdritem.msgws2vdr,
                            common:loginfo("Stored MSG from WS to VDR stored in GW : ~p", [MsgList]),

                            TargetList = [{WSID, WSFlowIdx, WSValue} || {WSID, WSFlowIdx, WSValue} <- MsgList, WSID == 16#8801],
                            case length(TargetList) of
                                1 ->
                                    [{16#8801, TargetWSFlowIdx, _WSValue}] = TargetList,
                                    {ok, WSUpdate} = wsock_data_parser:create_shot_resp(TargetWSFlowIdx,
                                                                                        [VehicleID],
                                                                                        Res,
                                                                                        List),
                                    %common:loginfo("Gateway receives VDR (~p) response to WS request ~p : ~p", [NewState#vdritem.addr, 16#8801, WSUpdate]),
                                    send_msg_to_ws_nowait(WSUpdate, NewState);
                                ItemCount ->
                                    common:loginfo("(FATAL) vdritem.msgws2vdr has ~p item(s) for wsid ~p", [ItemCount, 16#8801])
                            end,
                                    
                            NewMsgList = [{WSID, WSFlowIdx, WSValue} || {WSID, WSFlowIdx, WSValue} <- MsgList, WSID =/= 16#8801],
                            common:loginfo("New stored MSG from WS to VDR stored in GW : ~p", [NewMsgList]),
                            VDRTablePid = VDRItem#vdritem.vdrtablepid,
                            NewVDRItem = VDRItem#vdritem{msgws2vdr=NewMsgList},
                            common:send_vdr_table_operation(VDRTablePid, {self(), insert, NewVDRItem, noresp}),
                            
                            {ok, NewState#vdritem{msgws2vdr=NewMsgList}};
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
                            common:loginfo("Invalid message from registered/authenticated VDR (~p) (id:~p, serialno:~p, authen_code:~p, vehicleid:~p, vehiclecode:~p) MSG ID : ~p", 
                                           [NewState#vdritem.addr, 
                                            NewState#vdritem.id, 
                                            NewState#vdritem.serialno, 
                                            NewState#vdritem.auth, 
                                            NewState#vdritem.vehicleid, 
                                            NewState#vdritem.vehiclecode, 
                                            ID]),
                            {error, invalidmsgerror, NewState}
                    end
            end;
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
            end;
        {warning, HeaderInfo, ErrorType, NewState} ->
            {ID, MsgIdx, _Tel, _CryptoType} = HeaderInfo,
            FlowIdx = NewState#vdritem.msgflownum,
            MsgBody = vdr_data_processor:create_gen_resp(ID, MsgIdx, ErrorType),
            if
                NewState#vdritem.dboperid == undefined ->
                    NewFlowIdx = send_data_to_vdr(16#8001, NewState#vdritem.tel, FlowIdx, MsgBody, NewState),
                    {warning, NewState#vdritem{msgflownum=NewFlowIdx}};
                true ->
                    NewFlowIdx = send_data_to_vdr(16#8001, NewState#vdritem.tel, FlowIdx, MsgBody, NewState),
                    {warning, NewState#vdritem{msgflownum=NewFlowIdx}}
            end;
        {error, _ErrorType, NewState} ->    % exception/parityerror/formaterror
            {error, unvdrerror, NewState}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
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
            logvdr(error, State, "save_online_msg(...) : no VDR online process id", [])
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

check_vdrdbtable_auth(State, Auth) when is_binary(Auth) ->
    Pid = State#vdritem.pid,
    DBOperationPid = State#vdritem.dboperid,
    DBOperationPid ! {Pid, lookup, vdrdbtable, Auth},
    receive
        {Pid, Res} ->
            %Res = ets:lookup(vdrdbtable, Auth),
            %common:loginfo("Res for Auth: ~p, ~p", [Res, Auth]),
            case Res of
                [] ->
                    error;
                [VDRDBItem] ->
                    {ok, VDRDBItem}
            end
    after ?DB_RESP_TIMEOUT * 3 ->
            error
    end;
check_vdrdbtable_auth(State, Auth) when is_list(Auth) ->
    Pid = State#vdritem.pid,
    DBOperationPid = State#vdritem.dboperid,
    DBOperationPid ! {Pid, lookup, vdrdbtable, list_to_binary(Auth)},
    receive
        {Pid, Res} ->
            %Res = ets:lookup(vdrdbtable, list_to_binary(Auth)),
            %common:loginfo("Res for Auth: ~p, ~p", [Res, Auth]),
            case Res of
                [] ->
                    error;
                [VDRDBItem] ->
                    {ok, VDRDBItem}
            end
    after ?DB_RESP_TIMEOUT * 3 ->
            error
    end;
check_vdrdbtable_auth(_State, _Auth) ->
    error.

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

process_pos_info(ID, MsgIdx, HeadInfo, MsgOrigin, NewState) ->
    %common:loginfo("NewState : ~p", [NewState]),
    %common:loginfo("MsgOrigin : ~p", [MsgOrigin]),
    [Msg, Address] = adjust_http_gps_position(MsgOrigin, NewState),
    %common:loginfo("Msg : ~p", [Msg]),
    DBPid = NewState#vdritem.dbpid,
    if
        DBPid == unused -> % For no db mode
            FlowIdx = NewState#vdritem.msgflownum,
            MsgBody = vdr_data_processor:create_gen_resp(ID, MsgIdx, ?T_GEN_RESP_OK),
            NewFlowIdx = send_data_to_vdr(16#8001, NewState#vdritem.tel, FlowIdx, MsgBody, NewState),
            
            {ok, NewState#vdritem{msgflownum=NewFlowIdx}};
        true ->
            case create_sql_from_vdr(HeadInfo, Msg, Address, NewState) of
                {ok, Sqls} ->
                    FlowIdx = NewState#vdritem.msgflownum,
                    PreviousAlarm = NewState#vdritem.alarm,
                    
                    {H, AppInfo} = Msg,
                    AreaLineAlarmInfo = get_appinfo_area_line_alarm(AppInfo),
                    [AlarmSym, StateFlag, Lat, Lon, _Height, _Speed, _Direction, Time]= H,
                    LonStored = NewState#vdritem.lastlon,
                    LatStored = NewState#vdritem.lastlat,
                    if
                        LonStored == 0 orelse LonStored == 0.0 orelse LatStored == 0.0 orelse LatStored == 0.0 ->
                            if
                                Lat =/= 0 andalso Lat =/= 0.0 andalso Lon =/= 0 andalso Lon =/= 0.0 ->
                                    LastPosTablePid = NewState#vdritem.lastpostablepid,
                                    VIDKey = NewState#vdritem.vehicleid,
                                    SelfPid = NewState#vdritem.pid,
                                    % Last Position table should be encrypted data because when it is initialized,
                                    % it uses the data from the DB
                                    LastPosTablePid ! {SelfPid, set, [VIDKey, Lon, Lat]};   
                                true ->
                                    ok
                            end;
                        true ->
                            ok
                    end,
                    send_sqls_to_db_nowait(conn, Sqls, NewState#vdritem{lastlat=Lat, lastlon=Lon}),
                    if
                        AlarmSym == PreviousAlarm ->
                            MsgBody = vdr_data_processor:create_gen_resp(ID, MsgIdx, ?T_GEN_RESP_OK),
                            NewFlowIdx = send_data_to_vdr(16#8001, NewState#vdritem.tel, FlowIdx, MsgBody, NewState),
                            {ok, NewState#vdritem{msgflownum=NewFlowIdx, alarm=AlarmSym, state=StateFlag, lastlat=Lat, lastlon=Lon}};
                        true ->
                            {TimeBin, TimeS, TimeTuple} = create_time_list_and_binary(Time),
                            TimeBinS = list_to_binary([<<"\"">>, TimeBin, <<"\"">>]),
                            AlarmList = update_vehicle_alarm(NewState#vdritem.vehicleid, NewState#vdritem.driverid, TimeS, TimeTuple, AlarmSym, 0, MsgIdx, NewState, AreaLineAlarmInfo),
                            if
                                AlarmList == NewState#vdritem.alarmlist ->
                                    ok;
                                AlarmList =/= NewState#vdritem.alarmlist ->
                                    NewSetAlarmList = find_alarm_in_lista_not_in_listb(AlarmList, NewState#vdritem.alarmlist),
                                    NewClearAlarmList = find_alarm_in_lista_not_in_listb(NewState#vdritem.alarmlist, AlarmList),
                                    if
                                        NewState#vdritem.dboperid =/= undefined ->
                                            NewState#vdritem.dboperid ! {NewState#vdritem.pid, replace, alarm, NewState#vdritem.vehicleid, AlarmList};
                                        true ->
                                            ok
                                    end,
                                    send_masg_to_ws_alarm(FlowIdx, NewSetAlarmList, 1, Lat, Lon, TimeBinS, NewState),
                                    send_masg_to_ws_alarm(FlowIdx, NewClearAlarmList, 0, Lat, Lon, TimeBinS, NewState)
                            end,
                            MsgBody = vdr_data_processor:create_gen_resp(ID, MsgIdx, ?T_GEN_RESP_OK),
                            NewFlowIdx = send_data_to_vdr(16#8001, NewState#vdritem.tel, FlowIdx, MsgBody, NewState),
                            {ok, NewState#vdritem{msgflownum=NewFlowIdx, alarm=AlarmSym, alarmlist=AlarmList, state=StateFlag, lastlat=Lat, lastlon=Lon}}
                    end;
                _ ->
                    {error, invaliderror, NewState}
            end
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

get_http_gps_lon_lat(Lat, Lon, State) ->
    {MidLat, MidLon} = get_not_0_lat_lon(Lat, Lon, State),
    %common:loginfo("MidLat ~p, MidLon ~p, Lat ~p, Lon ~p", [MidLat, MidLon, Lat, Lon]),
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

get_not_0_lat_lon(Lat, Lon, State) ->
    if
        Lat == 0 orelse Lat == 0.0 orelse Lon == 0 orelse Lon == 0.0 ->
            {State#vdritem.lastlat, State#vdritem.lastlon};
            %if
            %   Lon == 0 orelse Lon == 0.0 ->
            %        {State#vdritem.lastlat, State#vdritem.lastlon};
            %    true ->
            %        {State#vdritem.lastlat, Lon}
            %end;
        true ->
            {Lat, Lon}
            %if
            %   Lon == 0 orelse Lon == 0.0 ->
            %        {Lat, State#vdritem.lastlon};
            %    true ->
            %        {Lat, Lon}
            %end
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

