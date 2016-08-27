-module(mon_handler).

-behaviour(gen_server).

-export([start_link/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("../include/header.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link(CSock, Addr, ConnInfoPid) ->   
    mslog:logall("mon_handler:start_link(CSock ~p, Addr ~p, ConnInfoPid ~p)", [CSock, Addr, ConnInfoPid]),
    gen_server:start_link(?MODULE, [CSock, Addr, ConnInfoPid], []). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([CSock, Addr, ConnInfoPid]) ->
	process_flag(trap_exit, true),
	[{drivertablepid, DriverTablePid}] = ets:lookup(msgservertable, drivertablepid),
	[{vdrlogpid, VDRLogPid}] = ets:lookup(msgservertable, vdrlogpid),
	[{vdronlinepid, VDROnlinePid}] = ets:lookup(msgservertable, vdronlinepid),
    State=#monitem{socket=CSock, 
                   pid=self(), 
                   addr=Addr, 
                   driverpid=DriverTablePid, 
                   vdrlogpid=VDRLogPid, 
                   vdronlinepid=VDROnlinePid, 
                   conninfopid=ConnInfoPid},
    ets:insert(montable, State), 
    common:set_sock_opts(CSock),
    logger:log_vdr_info(all, State, "Initilized."),
    {ok, State, ?MON_MSG_TIMEOUT}.       

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

handle_info({tcp, Socket, Data}, State) ->    
    Resp = mon_data_parser:parse_data(Data, State),
    gen_tcp:send(Socket, Resp),
    common:set_sock_opts(Socket),
    {noreply, State}; 
handle_info({tcp_closed, _Socket}, State) ->    
    mslog:logvdr(error, State, "mon_handler:handle_info(...) tcp_closed", []),
    {stop, normal, State}; 
handle_info(timeout, State) ->
    mslog:logvdr(error, State, "mon_handler:handle_info(...) timeout", []),
    {stop, vdrtimeout, State};
handle_info(Info, State) ->   
    mslog:logvdr(error, State, "mon_handler:handle_info(...) unknown ~p", [Info]),
    {stop, unknown, State}.

terminate(Reason, State) ->
    mslog:logvdr(info, State, "mon_handler:terminate(Reason ~p, State)", [Reason]),
	ets:delete(montable, State#monitem.socket),
    try
		gen_tcp:close(State#monitem.socket)
	catch
		_:Ex ->
			mslog:logerr("Monitor (~p) : exception when gen_tcp:close : ~p", [State#monitem.addr, Ex])
	end.

code_change(_OldVsn, State, _Extra) ->    
    {ok, State}.




