%%%
%%%
%%%

-module(common).

-include("../include/header.hrl").

-export([send_vdr_table_operation/2]).

-export([set_sockopt/3, set_sock_opts/1]).

-export([safepeername/1, forcesafepeername/1, printsocketinfo/2, forceprintsocketinfo/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Parameter :
%       CSock   :
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
set_sock_opts(CSock) ->
    inet:setopts(CSock, [binary, {active, once}, {send_timeout, ?VDR_MSG_TIMEOUT}, {send_timeout_close, true}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Parameter :
%       LSock   : Listen socket
%       CSock   : Client socket
% Return :
%       ok
%       {error, Error}
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
set_sockopt(LSock, CSock, Msg) ->    
    true = inet_db:register_socket(CSock, inet_tcp),    
    case prim_inet:getopts(LSock, [active, nodelay, keepalive, delay_send, priority, tos]) of       
        {ok, Opts} ->           
            case prim_inet:setopts(CSock, Opts) of              
                ok ->
                    %log:loginfo("prim_inet:setopts(CSock : ~p, Opts : ~p) ok.", [CSock, Opts]);
                    ok;             
                Error -> 
                    log:loginfo(string:concat(Msg, " : prim_inet:setopts(CSock : ~p, Opts : ~p) fails : ~p"), [CSock, Opts, Error]),    
                    gen_tcp:close(CSock),
                    {error, Error}
            end;       
        Error ->           
            mslog:loginfo(string:concat(Msg, " : prim_inet:setopts(...) fails : ~p"), [Error]),
            gen_tcp:close(CSock),
            {error, Error}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% {ok {Address, Port}}
% {error, Reason|Why}
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
safepeername(Socket) ->
	try inet:peername(Socket) of
		{ok, {Address, Port}} ->
			{ok, {inet_parse:ntoa(Address), Port}};
		{error, Error} ->
			{error, Error}
	catch
		_:Reason ->
			{error, Reason}
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% {ok, {Address, Port}}
% {ok, {"0.0.0.0", 0}}
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
forcesafepeername(Socket) ->
	try inet:peername(Socket) of
		{ok, {Address, Port}} ->
			{ok, {inet_parse:ntoa(Address), Port}};
		{error, _Error} ->
			{ok, {"0.0.0.0", 0}}
	catch
		_:_Reason ->
			{ok, {"0.0.0.0", 0}}
	end.

printsocketinfo(Socket, Msg) ->
    case common:safepeername(Socket) of
        {ok, {Address, _Port}} ->
            common:loginfo(string:concat(Msg, " from IP : ~p"), [Address]);
        {error, Error} ->
            common:loginfo(string:concat(Msg, " from unknown IP : ~p"), [Error])
    end.

forceprintsocketinfo(Socket, Msg) ->
    {ok, {Address, _Port}} = common:forcesafepeername(Socket),
    common:loginfo(string:concat(Msg, " IP : ~p"), [Address]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
%   VDR sends information to vdr table process.
% Parameter :
%       VDRTablePid :
%       Oper        :
% Return :
%       ok | {modified, ConnInfoPid}
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
send_vdr_table_operation(VDRTablePid, Oper) ->
    case VDRTablePid of
        undefined ->
            [Result] = ets:lookup(msgservertable, vdrtablepid),
			case Result of
				{vdrtablepid, VDRTablePid1} ->
		            case VDRTablePid1 of
		                undefined ->
		                    nofoundpid;
		                _ ->
		                    case Oper of
		                        {insert, _Object} ->
		                            VDRTablePid1 ! Oper;
		                        {delete, _Key} ->
		                            VDRTablePid1 ! Oper;
								{count} ->
									VDRTablePid1 ! Oper,
		                            receive
		                                Count -> Count
		                            end;
								{lookup, _Key} ->
									VDRTablePid1 ! Oper,
		                            receive
		                                Res -> Res
		                            end
		                  end
		            end,
                    {modified, VDRTablePid1};
				_ ->
					ok
			end;
        _ ->
            case Oper of
                {insert, _Object} ->
                    VDRTablePid ! Oper;
                {delete, _Key} ->
                    VDRTablePid ! Oper;
                {count} ->
                    VDRTablePid ! Oper,
                    receive
                        Count -> Count
                    end;
                {lookup, _Key} ->
                    VDRTablePid ! Oper,
                    receive
                        Res -> Res
                    end
            end
    end.








