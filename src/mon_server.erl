%%%
%%% Need considering how management server sends message to Management
%%%

-module(mon_server).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]). 

-include("../include/header.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Result = {ok,Pid} | ignore | {error,Error}
%    Pid = pid()
%  Error = {already_started,Pid} | term()
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link(LinkInfoPid) ->
    mslog:loghint("mon_server:start_link(LinkInfoPid : ~p)", [LinkInfoPid]),
    case gen_server:start_link({local, ?MODULE}, ?MODULE, [LinkInfoPid], []) of
        {ok, Pid} ->
            mslog:loginfo("mon_server:start_link(LinkInfoPid : ~p) ok", [LinkInfoPid]),
            {ok, Pid};
        ignore ->
            mslog:loghint("mon_server:start_link(LinkInfoPid : ~p) fails : ignore", [LinkInfoPid]),
            ignore;
        {already_started, Pid} ->
            mslog:loghint("mon_server:start_link(LinkInfoPid : ~p) fails : already_started : ~p", [LinkInfoPid, Pid]),
            {already_started, Pid}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([LinkInfoPid]) ->
    mslog:loghint("mon_server:init(LinkInfoPid : ~p)", [LinkInfoPid]),
    Opts = [binary, {packet, 0}, {reuseaddr, true}, {keepalive, true}, {active, once}],    
    case gen_tcp:listen(?DEF_PORT_MON, Opts) of       
        {ok, LSock} -> 
           mslog:loginfo("mon_server:init(LinkInfoPid : ~p) : gen_tcp:listen ok", [LinkInfoPid]),
           case prim_inet:async_accept(LSock, -1) of
                {ok, Ref} ->
                    mslog:lognone("mon_server:init(LinkInfoPid : ~p) : prim_inet:async_accept(LSock : ~p, -1) ok", [LinkInfoPid, LSock]),
                    {ok, #serverstate{lsock=LSock, acceptor=Ref, linkinfopid=LinkInfoPid}};
                Error ->
                    mslog:logerr("mon_server:init(LinkInfoPid : ~p) : prim_inet:async_accept(LSock : ~p, -1) fails : ~p", [LinkInfoPid, LSock, Error]),
                    {stop, Error}
            end;
        {error, Reason} ->          
            mslog:logerr("mon_server:init(LinkInfoPid : ~p) : gen_tcp:listen fails : ~p", [LinkInfoPid, Reason]),
            {stop, Reason}    
    end. 

handle_call(Request, _From, State) ->    
    {stop, {unknown_call, Request}, State}.

handle_cast(_Msg, State) ->    
    {noreply, State}. 

handle_info({inet_async, LSock, Ref, {ok, CSock}},
            #serverstate{lsock=LSock, acceptor=Ref, linkinfopid=LinkInfoPid}=State) ->
    common:printsocketinfo(CSock, "Accepted a monitor from"),
    try        
        case common:set_sockopt(LSock, CSock, "mon_server:handle_info({inet_async...):set_sockopt") of            
            ok -> 
                ok;         
            {error, Reason} -> 
                mslog:logerr("mon_server:handle_info({inet_async...) : common:set_sockopt(LSock : ~p, CSock : ~p, ...) fails : ~p", [LSock, CSock, Reason]),
                % Why use exit here?
                % {stop, set_sockpt, Reason}
                % Please consider it in the future
                exit({set_sockopt, Reason})       
        end,
        % New client connected
        % Spawn a new process using the simple_one_for_one supervisor.
        % Why it is "the simple_one_for_one supervisor"?
        case common:safepeername(CSock) of
            {error, Err} ->
                mslog:logerr("mon_server:handle_info(...) : common:safepeername(CSock : ~p) fails : ~p", [CSock, Err]);
            {ok, {Addr, _Port}} ->
                case mssup:start_child_mon(CSock, Addr, LinkInfoPid) of
                    {ok, Pid} ->
                        case gen_tcp:controlling_process(CSock, Pid) of
                            ok ->
                                ok;
                            {error, Reason1} ->
                                mslog:logerr("mon_server:handle_info(...) : gen_server:controlling_process(Socket, ~p) fails : ~p", [Pid, Reason1]),
                                case mssup:stop_child_monr(Pid) of
                                    ok ->
                                        ok;
                                    {error, Reason2} ->
                                        mslog:logerr("mon_server:handle_info(...) : mssup:stop_child_vdr(~p) fails : ~p", [Pid, Reason2])
                                end
                        end;
                    {ok, Pid, _Info} ->
                        case gen_tcp:controlling_process(CSock, Pid) of
                            ok ->
                                ok;
                            {error, Reason1} ->
                                mslog:logerr("mon_server:handle_info(...) : gen_server:controlling_process(Socket, ~p) fails: ~p", [Pid, Reason1]),
                                 case mssup:stop_child_mon(Pid) of
                                    ok ->
                                        ok;
                                    {error, Reason2} ->
                                        mslog:logerr("mon_server:handle_info(...) : mssup:stop_child_vdr(~p) fails : ~p", [Pid, Reason2])
                                end
                        end;
                    {error, already_present} ->
                        mslog:logerr("mon_server:handle_info(...) : mssup:start_child_vdr fails : already_present");
                    {error, {already_started, Pid}} ->
                        mslog:logerr("mon_server:handle_info(...) : mssup:start_child_vdr fails : already_started PID : ~p", [Pid]);
                    {error, Msg} ->
                        mslog:logerr("mon_server:handle_info(...) : mssup:start_child_vdr fails : ~p", [Msg])
                end
        end,
        %% Signal the network driver that we are ready to accept another connection        
        case prim_inet:async_accept(LSock, -1) of           
            {ok, NewRef} -> 
                {noreply, State#serverstate{acceptor=NewRef}};
            Error ->
                mslog:logerr("mon_server:handle_info(...) : prim_inet:async_accept fails : ~p", [inet:format_error(Error)]),
                % Why use exit here?
                % {stop, Error, State}
                % Please consider it in the future
                exit({async_accept, inet:format_error(Error)})        
        end
    catch 
        exit:Why ->        
            [ST] = erlang:get_stacktrace(),
            mslog:loginfo("mon_server:handle_info(...) : inet_async exception : ~p~nStack trace :~n~p", [Why, ST]),         
            {stop, Why, State}    
    end;

%%%
%%% Data should not be received here because it is a listening socket process
%%%
handle_info({tcp, Socket, Data}, State) ->  
    common:printsocketinfo(Socket, "MON server receives STRANGE data from"),
    mslog:logerr("ERROR : MON server receives STRANGE data : ~p", [Data]),
    inet:setopts(Socket, [{active, once}]),
    {noreply, State}; 
handle_info({inet_async, LSock, Ref, Error}, #serverstate{lsock=LSock, acceptor=Ref}=State) ->    
    mslog:logerr("mon_server:handle_info(...) : error : ~p", [Error]),
    {stop, Error, State}; 
handle_info(_Info, State) ->    
    {noreply, State}. 

terminate(Reason, State) ->    
    mslog:loghint("mon_server:terminate(...) : ~p", [Reason]),
    gen_tcp:close(State#serverstate.lsock),    
    ok. 

code_change(_OldVsn, State, _Extra) ->    
    {ok, State}. 
    

                                