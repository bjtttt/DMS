%
% vdr_handler.erl
%

-module(vdr_handler).

-behaviour(gen_server).

-export([start_link/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("../include/header.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link(CSock, Addr, LinkInfoPid) ->
    log:lognone("vdr_handler:start_link(CSock ~p, Addr ~p, LinkInfoPid ~p)", [CSock, Addr, LinkInfoPid]),
	gen_server:start_link(?MODULE, [CSock, Addr, LinkInfoPid], []). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([CSock, Addr, LinkInfoPid]) ->
    log:lognone("vdr_handler:init(CSock ~p, Addr ~p, LinkInfoPid ~p)", [CSock, Addr, LinkInfoPid]),
    [{ccpid, CCPid}] = ets:lookup(msgservertable, ccpid),
    [{vdrtablepid, VDRTablePid}] = ets:lookup(msgservertable, vdrtablepid),
    [{drivertablepid, DriverTablePid}] = ets:lookup(msgservertable, drivertablepid),
    [{lastpostablepid, LastPosTablePid}] = ets:lookup(msgservertable, lastpostablepid),
    [{httpgpspid, HttpGpsPid}] = ets:lookup(msgservertable, httpgpspid),
    [{vdrlogpid, VDRLogPid}] = ets:lookup(msgservertable, vdrlogpid),
    [{vdronlinepid, VDROnlinePid}] = ets:lookup(msgservertable, vdronlinepid),
    State = #vdritem{socket=CSock, 
                     pid=self(), 
                     addr=Addr, 
                     msgflownum=1, 
					 errorcount=0, 
                     dbpid=unused,
                     ccpid=CCPid, 
                     linkpid=LinkInfoPid, 
					 vdrtablepid=VDRTablePid, 
                     drivertablepid=DriverTablePid, 
                     lastpostablepid=LastPosTablePid,
					 httpgpspid=HttpGpsPid, 
                     vdrlogpid=VDRLogPid, 
                     vdronlinepid=VDROnlinePid},
	log:log_vdr_statistics_info(State, ?CONN_STAT_CONN),
    set_sock_opts(CSock),
    {ok, State, ?VDR_MSG_TIMEOUT}.       

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Parameter :
%       CSock   :
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
set_sock_opts(CSock) ->
    inet:setopts(CSock, [binary, {active, once}, {send_timeout, ?VDR_MSG_TIMEOUT}, {send_timeout_close, true}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_call(_Request, _From, State) ->
    {noreply, ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_cast(_Msg, State) ->    
	{noreply, State}. 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
%   Debug function. Save messages between the VDR and the gateway to disk.
% Parameter :
%       State   :
%       FromVDR :
%       Msg     :
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
save_msg_4_vdr(State, FromVDR, Msg) ->
    VDRID = State#vdritem.id,
	StoredMsg = State#vdritem.storedmsg4save,
	VDRLogPid = State#vdritem.vdrlogpid,
	if
		VDRID == undefined ->
			{Year,Month,Day} = erlang:date(),
			{Hour,Min,Second} = erlang:time(),
			NewMsg = [{FromVDR, Msg, Year, Month, Day, Hour, Min, Second}],
			NewStoredMsg = lists:merge([StoredMsg, NewMsg]),
            log:logvdr(none, State, "vdr_handler:save_msg_4_vdr(...) store data : ~p", NewStoredMsg),
			State#vdritem{storedmsg4save=NewStoredMsg};
		true ->
            if
                StoredMsg =/= [] ->
                    log:logvdr(none, State, "vdr_handler:save_msg_4_vdr(...) send stored data : ~p", StoredMsg),
                    save_stored_msg_4_vdr(VDRID, StoredMsg, VDRLogPid);
                true ->
                    ok
            end,
			{Year,Month,Day} = erlang:date(),
			{Hour,Min,Second} = erlang:time(),
			DateTime = {Year, Month, Day, Hour, Min, Second},
            log:logvdr(none, State, "vdr_handler:save_msg_4_vdr(...) send data : ~p", Msg),
			VDRLogPid ! {save, VDRID, FromVDR, Msg, DateTime},
			State#vdritem{storedmsg4save=[]}
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
%   Send messages to the message saving process.
% Parameter :
%       Pid, 
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
save_stored_msg_4_vdr(VDRID, StoredMsg, VDRLogPid) when VDRLogPid =/= undefined,
                                                        is_list(StoredMsg),
                                                        length(StoredMsg) > 0 ->
	[H|T] = StoredMsg,
	{FromVDR, MsgBin, Year, Month, Day, Hour, Min, Second} = H,
	DateTime = {Year, Month, Day, Hour, Min, Second},
	VDRLogPid ! {save, VDRID, FromVDR, MsgBin, DateTime},
	save_stored_msg_4_vdr(VDRID, T, VDRLogPid);
save_stored_msg_4_vdr(_VDRID, _StoredMsg, _VDRLogPid) ->
	ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
% Parameter :
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_info({tcp, Socket, Data}, PrevState) ->
	LinkInfoPid = PrevState#vdritem.linkpid,
	Pid = PrevState#vdritem.pid,
	LinkInfoPid ! {Pid, ?CONN_STAT_FROM_GW},
	MidState = save_msg_4_vdr(PrevState, true, Data),
    log:logvdr(all, MidState, "vdr_handler:handle_info(...) data ~p", [Data]),
    % Update active time for VDR
	DateTime = {erlang:date(), erlang:time()},
    State = MidState#vdritem{acttime=DateTime},
    %DataDebug = <<126,1,2,0,2,1,86,121,16,51,112,0,14,81,82,113,126,126,1,2,0,2,1,86,121,16,51,112,123,14,81,82,144,126>>,
    %DataDebug = <<126,1,2,0,2,1,86,121,16,51,112,44,40,81,82,123,126>>,
    %DataDebug = <<126,2,0,0,46,1,86,121,16,51,112,0,2,0,0,0,0,0,0,0,17,0,0,0,0,0,0,0,0,0,0,0,0,0,0,19,3,36,25,18,68,1,4,0,0,0,0,2,2,0,0,3,2,0,0,4,2,0,0,59,126>>,
    %DataDebug = <<126,2,0,0,46,1,86,121,16,51,112,3,44,0,8,0,0,0,0,0,17,0,0,0,0,0,0,0,0,0,0,0,0,0,0,19,3,36,35,85,35,1,4,0,0,0,0,2,2,0,0,3,2,0,0,4,2,0,0,4,126>>,
	%DataDebug = <<126,1,0,0,45,1,86,0,71,2,5,0,55,0,11,0,114,55,48,51,49,57,74,76,57,48,49,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,48,52,55,48,50,48,53,1,190,169,66,55,48,50,48,53,39,126>>,
	%DataDebug = <<126,2,0,0,49,1,86,151,146,84,84,0,115,0,0,0,0,0,0,0,3,2,97,110,120,6,239,82,248,0,47,0,30,0,253,19,7,4,19,86,18,1,4,0,0,0,125,1,2,2,0,0,3,2,0,0,4,2,0,0,17,1,0,195,126>>,
	%DataDebug = <<126,8,0,0,8,1,52,1,8,18,33,46,94,81,234,104,178,1,3,0,0,28,126>>,
	%DataDebug = <<126,8,1,34,36,1,52,1,8,18,33,54,69,0,34,0,1,81,234,120,125,1,1,3,0,0,0,8,0,0,0,0,0,19,2,97,189,24,6,238,86,48,0,95,0,86,0,0,19,7,32,17,70,3,82,73,70,70,160,66,0,0,87,65,86,69,102,109,116,32,16,0,0,0,1,0,1,0,64,31,0,0,128,62,0,0,2,0,16,0,100,97,116,97,124,66,0,0,199,255,216,255,218,255,200,255,194,255,178,255,169,255,169,255,155,255,145,255,213,255,227,255,198,255,237,255,244,255,242,255,21,0,52,0,68,0,68,0,68,0,54,0,54,0,44,0,66,0,45,0,65,0,59,0,50,0,12,0,246,255,246,255,216,255,215,255,202,255,172,255,144,255,132,255,157,255,175,255,154,255,147,255,152,255,186,255,172,255,151,255,162,255,172,255,153,255,136,255,138,255,180,255,150,255,128,255,91,255,97,255,82,255,50,255,87,255,81,255,109,255,144,255,162,255,156,255,141,255,152,255,165,255,209,255,227,255,213,255,202,255,210,255,189,255,178,255,160,255,188,255,186,255,242,255,250,255,33,0,28,0,32,0,15,0,46,0,52,0,82,0,66,0,55,0,44,0,33,0,249,255,225,255,188,255,217,255,228,255,243,255,2,0,16,0,225,255,203,255,212,255,198,255,184,255,168,255,182,255,143,255,125,1,255,186,255,212,255,206,255,251,255,255,255,18,0,17,0,28,0,12,0,19,0,49,0,58,0,35,0,50,0,87,0,104,0,64,0,23,0,224,255,221,255,239,255,11,0,24,0,42,0,41,0,24,0,18,0,6,0,29,0,48,0,19,0,250,255,10,0,20,0,238,255,239,255,179,255,167,255,168,255,171,255,164,255,202,255,200,255,181,255,220,255,228,255,225,255,228,255,170,255,171,255,206,255,222,255,229,255,235,255,245,255,235,255,4,0,247,255,231,255,228,255,2,0,231,255,244,255,232,255,245,255,4,0,37,0,33,0,81,0,55,0,69,0,47,0,53,0,67,0,56,0,42,0,46,0,58,0,35,0,255,255,230,255,196,255,186,255,209,255,193,255,194,255,227,255,231,255,230,255,214,255,189,255,154,255,147,255,153,255,157,255,168,255,168,255,191,255,181,255,204,255,252,255,240,255,1,0,235,255,243,255,238,255,241,255,251,255,229,255,215,255,231,255,224,255,248,255,236,255,248,255,45,0,25,0,12,0,21,0,5,0,243,255,226,255,185,255,151,255,162,255,173,255,225,255,231,126>>,
	%DataDebug = <<126,8,5,0,9,1,52,1,8,18,33,5,59,0,0,0,0,1,0,0,0,2,54,126>>,
	%DataDebug = <<126,8,5,0,9,1,52,1,8,18,33,2,125,2,0,10,0,0,1,0,0,0,2,125,2,126>>,
	%DataDebug = <<126,7,2,0,7,1,52,1,8,18,33,0,13,2,19,8,48,22,41,22,0,126>>,
	%DataDebug = <<126,7,2,0,50,1,52,1,8,18,33,1,18,1,19,9,3,18,54,9,0,8,53,54,185,220,192,237,212,177,49,50,51,52,53,54,55,56,56,56,57,48,49,50,51,52,57,57,0,0,8,183,162,214,164,187,250,185,185,0,24,7,5,9,126>>,
	%DataDebug = <<126,7,2,0,50,1,52,1,8,18,33,0,11,1,19,9,3,21,8,87,0,8,53,54,185,220,192,237,212,177,49,50,51,52,53,54,55,56,56,56,57,48,49,50,51,52,57,57,0,0,8,183,162,214,164,187,250,185,185,0,24,7,5,118,126>>,
	%DataDebug = <<126,2,0,0,64,1,50,97,51,36,129,0,120,0,0,0,0,0,12,0,19,2,97,0,56,6,241,103,104,0,48,0,100,0,1,19,16,24,21,73,2,1,4,0,0,0,160,2,2,0,0,3,2,0,0,37,4,0,0,0,0,42,2,0,0,43,4,0,0,0,0,48,1,0,49,1,17,192,126>>,
	%DataDebug = <<126,02,00,00,64,01,50,97,51,66,17,00,05,00,00,64,00,00,12,00,03,01,102,244,156,06,197,41,112,00,30,26,134,00,76,19,18,09,24,35,40,01,04,00,00,00,146,02,02,00,00,03,02,01,234,37,04,00,00,00,00,42,02,00,00,43,04,00,00,00,00,48,01,00,49,01,00,32,126>>,
	%DataDebug = <<126,7,2,0,48,1,56,16,89,23,38,0,82,1,18,1,39,5,34,0,0,6,185,220,192,237,212,177,50,51,52,53,54,55,56,57,48,53,48,53,53,51,52,53,54,55,56,57,8,183,162,214,164,187,250,185,185,0,20,1,1,34,126>>,
    %DataDebug = <<126,2,0,0,60,1,50,97,51,36,129,0,4,0,0,0,0,0,12,1,19,2,94,215,124,6,239,184,194,0,27,0,0,1,98,21,3,18,16,6,68,1,4,0,0,0,10,2,2,0,0,3,2,0,0,37,4,0,0,0,0,43,4,0,0,0,255,48,1,99,49,1,10,61,126,126,0,2,0,0,1,50,97,51,36,129,0,5,195,126,126,2,0,0,60,1,50,97,51,36,129,0,6,0,0,0,0,0,0,1,17,0,0,0,0,0,0,0,0,0,0,0,0,0,0,21,3,18,16,6,72,1,4,0,0,0,10,2,2,0,0,3,2,0,0,37,4,0,0,0,0,43,4,0,0,0,0,48,1,99,49,1,0,212,126>>,
    %Msgs = msghelper:split_msg_to_single(DataDebug, 16#7e),
	Msgs = msghelper:split_msg_to_single(Data, 16#7e),
    case Msgs of
        [] ->
			common:send_stat_err(State, ?CONN_STAT_SPLIT_ERR),
            ErrCount = State#vdritem.errorcount + 1,
            log:logvdr(error, State, "vdr_handler:handle_info(...) empty splitted data from ~p", [Msgs]),
            if
                ErrCount >= ?MAX_VDR_ERR_COUNT ->
					log:log_vdr_statistics_info(State, ?CONN_STAT_DISC_ERR_CNT),
					log:log_vdr_statistics_info(State, ?CONN_STAT_DISC_GW),
                    {stop, vdrerror, State#vdritem{errorcount=ErrCount}};
                true ->
                    set_sock_opts(Socket),
                    {noreply, State#vdritem{errorcount=ErrCount}, ?VDR_MSG_TIMEOUT}
            end;    
        NewMsgs ->
            case process_vdr_msges(Socket, Msgs, State) of
                %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                % Should revisit here for error message definitions
                %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                {error, vdrerror, NewState} ->
                    ErrCount = NewState#vdritem.errorcount + 1,
                    log:logvdr(error, State, "vdr_handler:handle_info(...) wrong splitted data : ~p", [NewMsgs]),
                    common:send_stat_err(State, ?CONN_STAT_UNK_ERR),
                    if
                        ErrCount >= ?MAX_VDR_ERR_COUNT ->
                            common:send_stat_err(State, ?CONN_STAT_DISC_ERR_CNT),
							common:send_stat_err(State, ?CONN_STAT_DISC_GW),
                            {stop, vdrerror, NewState#vdritem{errorcount=ErrCount}};
                        true ->
                            set_sock_opts(Socket),
                            {noreply, NewState#vdritem{errorcount=ErrCount}, ?VDR_MSG_TIMEOUT}
                    end;
                {error, ErrType, NewState} ->
					if
						ErrType == charerror ->
							log:log_vdr_statistics_info(State, ?CONN_STAT_DISC_CHAR);
						ErrType == regerror ->
							log:log_vdr_statistics_info(State, ?CONN_STAT_DISC_REG);
						ErrType == autherror ->
							log:log_vdr_statistics_info(State, ?CONN_STAT_DISC_AUTH);
						ErrType == unautherror ->
							log:log_vdr_statistics_info(State, ?CONN_STAT_DISC_UNAUTH);
						ErrType == invalidmsgerror ->
							log:log_vdr_statistics_info(State, ?CONN_STAT_DISC_INVALID_MSG);
						ErrType == exiterror ->
							log:log_vdr_statistics_info(State, ?CONN_STAT_DISC_UNREG);
						ErrType == vdrerror ->
							log:log_vdr_statistics_info(State, ?CONN_STAT_DISC_MSG_ERR);
						ErrType == unvdrerror ->
							log:log_vdr_statistics_info(State, ?CONN_STAT_DISC_UNK_MSG_ERR);
						ErrType == msg_process_exception ->
							log:log_vdr_statistics_info(State, ?CONN_STAT_DISC_MSGEX);
						true ->
							log:log_vdr_statistics_info(State, ?CONN_STAT_DISC_UNK_ERR)
					end,
					log:log_vdr_statistics_info(State, ?CONN_STAT_DISC_GW),
                    {stop, ErrType, NewState};
                {warning, NewState} ->
                    set_sock_opts(Socket),
                    {noreply, NewState#vdritem{errorcount=0}, ?VDR_MSG_TIMEOUT};
                {ok, NewState} ->
                    set_sock_opts(Socket),
                    {noreply, NewState#vdritem{errorcount=0}, ?VDR_MSG_TIMEOUT}
            end
    end;
handle_info({tcp_closed, _Socket}, State) -> 
    log:logvdr(error, State, "vdr_handler:handle_info(...) tcp_closed", []),
	log:log_vdr_statistics_info(State, ?CONN_STAT_DISC_CLI),
	{stop, tcp_closed, State};
handle_info(timeout, State) ->
    log:logvdr(error, State, "vdr_handler:handle_info(...) timeout", []),
	log:log_vdr_statistics_info(State, ?CONN_STAT_DISC_TIMEOUT),
	{stop, vdrtimeout, State};
handle_info(Info, State) ->   
    log:logvdr(error, State, "vdr_handler:handle_info(...) unknown ~p", [Info]),
	{stop, unknown, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% When VDR handler process is terminated, do the clean jobs here
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
terminate(Reason, State) ->
    log:logvdr(info, State, "terminate(Reason ~p, State)", [Reason]),
    Socket = State#vdritem.socket,
    VID = State#vdritem.id,
    VDRTablePid = State#vdritem.vdrtablepid,
    VDROnlinePid = State#vdritem.vdronlinepid,
    case Socket of
        undefined ->
            log:logvdr(error, State, "vdr_handler:terminate(...) undefined socket", []);
        _ ->
            if 
                VDRTablePid =/= undefined ->
                   common:send_vdr_table_operation(VDRTablePid, {delete, Socket});
                true ->
                    log:logvdr(error, State, "vdr_handler:terminate(...) undefined VDR table processor id", [])
            end
    end,
    if
        VDROnlinePid =/= undefined ->
            {Year,Month,Day} = erlang:date(),
            {Hour,Min,Second} = erlang:time(),
            if
                VID =/= undefined ->
                    VDROnlinePid ! {addoff, VID, {Year,Month,Day,Hour,Min,Second}};
                true ->
                    log:logvdr(error, State, "vdr_handler:terminate(...) undefined VDR id", [])
            end;
        true ->
            log:logvdr(error, State, "vdr_handler:terminate(...) undefined VDR  online process id", [])
    end,
	try gen_tcp:close(State#vdritem.socket)
    catch
        _:Ex ->
            log:logvdr(error, State, "vdr_handler:terminate(...) gen_tcp:close(...) exception : ~p", [Ex])
    end.

code_change(_OldVsn, State, _Extra) ->    
	{ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
% Parameter :
% Return :
%     {ok, State}
%     {warning, State}
%     {error, vdrerror/invaliderror/systemerror/exception/unknown, State}  
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
                {warning, NewState} ->
                    process_vdr_msges(Socket, T, NewState);
                {error, ErrorType, NewState} ->
                    {error, ErrorType, NewState};
                _ ->
					log:logvdr(error, State, "vdr_handler:process_vdr_msges(...) unknown state : ~p", [Result]),
                    {error, unknown, State}
            end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
% Parameter :
% Return :
%     {ok, State}
%     {warning, State}
%     {error, systemerror/vdrerror/invaliderror/exception, State}  
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
safe_process_vdr_msg(Socket, Msg, State) ->
    try vdr_data_processor:process_vdr_data(Socket, Msg, State)
    catch
        _:Ex ->
			[ST] = erlang:get_stacktrace(),
            log:logvdr(error, State, "vdr_handler:safe_process_vdr_msg(...) Msg ~p~nexcption ~p~nStack trace ~p", [Msg, Ex, ST]),
            {error, msg_process_exception, State}
    end.
