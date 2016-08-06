%
% Need considering how management server sends message to VDR
% https://erlangcentral.org/wiki/index.php?title=Building_a_Non-blocking_TCP_server_using_OTP_principles
% http://blog.chinaunix.net/uid-429659-id-3540652.html
%
-module(vdr_server).

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
    log:loginfo("vdr_server:start_link(LinkInfoPid : ~p)", [LinkInfoPid]),
	case gen_server:start_link({local, ?MODULE}, ?MODULE, [LinkInfoPid], []) of
        {ok, Pid} ->
            log:loginfo("vdr_server:start_link(LinkInfoPid : ~p) ok", [LinkInfoPid]),
            {ok, Pid};
        ignore ->
            log:loginfo("vdr_server:start_link(LinkInfoPid : ~p) fails : ignore", [LinkInfoPid]),
            ignore;
        {already_started, Pid} ->
            log:loginfo("vdr_server:start_link(LinkInfoPid : ~p) fails : already_started : ~p", [LinkInfoPid, Pid]),
            {already_started, Pid}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% {active, true}            : 套接字设置为主动模式。所有套接字接收到的消息都作为 Erlang 消息转发到拥有这个套接字进程上。当开启一个套接字时，默认是主动模式。
% {active, false}           : 设置套接字为被动模式。套接字收到的消息被缓存起来，进程必须通过调用函数 gen_tcp:recv/2或 gen_tcp:recv/3来读取这些消息。
% {active, once}            : 将设置套接字为主动模式，但是一旦收到第一条消息，就将其设置为被动模式，并使用 gen_tcp:recv/2 或 gen_tcp:recv/3函数来读取后续消息。
% {keepalive, true}         : 当没有转移数据时，确保所连接的套接字发送保持活跃（keepalive）的消息。因为关闭套接字消息可能会丢失，如果没有接收到保持活跃消息的响应，那么该选项可确保这个套接字能被关闭。默认情况下，该标签是关闭的。
% {nodelay, true}           : 数据包直接发送到套接字，不过它多么小。在默认情况下，此选项处于关闭状态，并且与之相反，数据被聚集而以更大的数据块进行发送。
% {packet_size, Size}       : 设置数据包允许的最大长度。如果数据包比 Size还大，那么将认为这个数据包无效。
% {packet, 0}               : 表示 Erlang系统会把 TCP 数据原封不动地直接传送给应用程序
% {reuseaddr, true}         : 允许本地重复使用端口号
% {nodelay, true}           : 意味着很少的数据也会被马上被发送出去
% {delay_send, true}        : 数据不是立即发送，而是存到发送队列里，等 socket可写的时候再发送
% {backlog, 1024}           : 缓冲区的长度
% {exit_on_close, false}    : 设置为 flase，那么 socket 被关闭之后还能将缓冲区中的数据发送出去
% {send_timeout, 15000}     : 设置一个时间去等待操作系统发送数据，如果底层在这个时间段后还没发出数据，那么就会返回 {error,timeout}
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([LinkInfoPid]) ->
    log:loginfo("vdr_server:init(LinkInfoPid : ~p)", [LinkInfoPid]),
	Opts = [binary, {packet, 0}, {reuseaddr, true}, {keepalive, true}, {active, once}],    
    case gen_tcp:listen(?DEF_PORT_VDR, Opts) of	    
		{ok, LSock} -> 
            log:loginfo("vdr_server:init(LinkInfoPid : ~p) : gen_tcp:listen ok", [LinkInfoPid]),
			case prim_inet:async_accept(LSock, -1) of
                {ok, Ref} ->
                    log:loginfo("vdr_server:init(LinkInfoPid : ~p) : prim_inet:async_accept(LSock : ~p, -1) ok", [LinkInfoPid, LSock]),
                    {ok, #serverstate{lsock=LSock, acceptor=Ref, linkinfopid=LinkInfoPid}};
                Error ->
                    common:logerr("vdr_server:init(LinkInfoPid : ~p) : prim_inet:async_accept(LSock : ~p, -1) fails : ~p", [LinkInfoPid, LSock, Error]),
                    {stop, Error}
            end;
		{error, Reason} ->	        
            common:loginfo("vdr_server:init(LinkInfoPid : ~p) : gen_tcp:listen fails : ~p", [LinkInfoPid, Reason]),
			{stop, Reason}    
	end. 

%%-------------------------------------------------------------------------
%% @spec (Request, From, State) -> {reply, Reply, State}          |
%%                                 {reply, Reply, State, Timeout} |
%%                                 {noreply, State}               |
%%                                 {noreply, State, Timeout}      |
%%                                 {stop, Reason, Reply, State}   |
%%                                 {stop, Reason, State}
%% @doc Callback for synchronous server calls.  If `{stop, ...}' tuple
%%      is returned, the server is stopped and `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_call(Request, _From, State) ->    
	{stop, {unknown_call, Request}, State}.

%%-------------------------------------------------------------------------
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc Callback for asyncrous server calls.  If `{stop, ...}' tuple
%%      is returned, the server is stopped and `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_cast(_Msg, State) ->    
	{noreply, State}. 

%%-------------------------------------------------------------------------
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc Callback for messages sent directly to server's mailbox.
%%      If `{stop, ...}' tuple is returned, the server is stopped and
%%      `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_info({inet_async, LSock, Ref, {ok, CSock}},
            #serverstate{lsock=LSock, acceptor=Ref, linkinfopid=_LinkInfoPid}=State) ->
    try        
		case common:set_sockopt(LSock, CSock, "vdr_server:handle_info({inet_async...)") of	        
			ok -> 
				ok;	        
			{error, Reason} -> 
                common:loginfo("vdr_server:handle_info({inet_async...) : common:set_sockopt(LSock : ~p, CSock : ~p, ...) fails : ~p", [LSock, CSock, Reason]),
                exit({set_sockopt, Reason})       
		end,
		% New client connected
        % Spawn a new process using the simple_one_for_one supervisor.
        case common:safepeername(CSock) of
            {ok, {Addr, _Port}} ->
                case mssup:start_child_vdr(CSock, Addr) of
                    {ok, Pid} ->
                        case gen_tcp:controlling_process(CSock, Pid) of
                            ok ->
                                ok;
                            {error, Reason1} ->
                                common:loginfo("vdr_server:handle_info(...) : gen_server:controlling_process(Socket, ~p) fails : ~p", [Pid, Reason1]),
                                case mssup:stop_child_vdr(Pid) of
                                    ok ->
                                        ok;
                                    {error, Reason2} ->
                                        common:loginfo("vdr_server:handle_info(...) : mssup:stop_child_vdr(~p) fails : ~p", [Pid, Reason2])
                                end
                        end;
                    {ok, Pid, _Info} ->
                        case gen_tcp:controlling_process(CSock, Pid) of
                            ok ->
                                ok;
                            {error, Reason1} ->
                                common:loginfo("vdr_server:handle_info(...) : gen_server:controlling_process(Socket, ~p) fails: ~p", [Pid, Reason1]),
                                case mssup:stop_child_vdr(Pid) of
                                    ok ->
                                        ok;
                                    {error, Reason2} ->
                                        common:loginfo("vdr_server:handle_info(...) : mssup:stop_child_vdr(~p) fails : ~p", [Pid, Reason2])
                                end
                        end;
                    {error, already_present} ->
                        common:loginfo("vdr_server:handle_info(...) : mssup:start_child_vdr fails : already_present");
                    {error, {already_started, Pid}} ->
                        common:loginfo("vdr_server:handle_info(...) : mssup:start_child_vdr fails : already_started PID : ~p", [Pid]);
                    {error, Msg} ->
                        common:loginfo("vdr_server:handle_info(...) : mssup:start_child_vdr fails : ~p", [Msg])
                end;
            {error, Err} ->
                common:loginfo("vdr_server:handle_info(...) : cannot start new process for new connection because common:safepeername(...) fails : ~p", [Err])
        end,
        %% Signal the network driver that we are ready to accept another connection        
		case prim_inet:async_accept(LSock, -1) of	        
			{ok, NewRef} -> 
                {noreply, State#serverstate{acceptor=NewRef}};
			Error ->
                common:loginfo("vdr_server:handle_info(...) : prim_inet:async_accept fails : ~p", [inet:format_error(Error)]),
                % Why use exit here?
                % {stop, Error, State}
                % Please consider it in the future
                exit({async_accept, inet:format_error(Error)})        
		end
	catch 
		exit:Why ->    
			[ST] = erlang:get_stacktrace(),
            common:loginfo("vdr_server:handle_info(...) : inet_async exception : ~p~nStack trace :~n~p", [Why, ST]),			
            {stop, Why, State}    
	end;

%%%
%%% Data should not be received here because it is a listening socket process
%%%
handle_info({tcp, Socket, _Data}, State) ->  
	common:send_stat_err_server(State, servermsg),
	inet:setopts(Socket, [{active, once}]),
    {noreply, State}; 
handle_info({inet_async, LSock, Ref, Error}, #serverstate{lsock=LSock, acceptor=Ref}=OriState) ->    
    [{linkpid, LinkPid}] = ets:lookup(msgservertable, linkpid),
	State = OriState#serverstate{linkpid=LinkPid},
	{stop, Error, State}; 
handle_info(_Info, State) ->    
	{noreply, State}. 

terminate(Reason, State) ->    
    common:loginfo("vdr_server:terminate(...) : ~p", [Reason]),
	gen_tcp:close(State#serverstate.lsock),    
	ok. 

code_change(_OldVsn, State, _Extra) ->    
	{ok, State}. 
    
								