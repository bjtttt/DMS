%%%
%%%
%%%

-module(common).

-include("../include/header.hrl").

-export([split_msg_to_packages/2,
		 number_list_to_binary/2,
		 make_sure_n_byte_binary/2,
		 convert_integer_to_binary_string_list/1,
         convert_bcd_integer/1,
		 convert_integer_bcd/1,
         removemsgfromlistbyflownum/2,
         is_integer_string/1,
         is_dec_integer_string/1,
         is_hex_integer_string/1,
         convert_word_hex_string_to_integer/1,
		 integer_to_binary/1,
         integer_to_2byte_binary/1,
		 integer_to_size_binary/2,
		 integer_list_to_size_binary_list/2,
         float_to_binary/1,
		 send_vdr_table_operation/2,
		 get_binary_from_list/1,
		 get_list_from_binary/1]).

-export([set_sockopt/3]).

-export([safepeername/1, forcesafepeername/1, printsocketinfo/2, forceprintsocketinfo/2]).

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
            common:loginfo(string:concat(Msg, " : prim_inet:setopts(CSock : ~p, Opts : ~p) fails : ~p"), [CSock, Opts, Error]),
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
            common:loginfo(string:concat(Msg, " from IP : ~p~n"), [Address]);
        {error, Error} ->
            common:loginfo(string:concat(Msg, " from unknown IP : ~p~n"), [Error])
    end.

forceprintsocketinfo(Socket, Msg) ->
    {ok, {Address, _Port}} = common:forcesafepeername(Socket),
    common:loginfo(string:concat(Msg, " IP : ~p~n"), [Address]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
%   VDR sends information to vdr table process.
% Parameter :
%       VDRTablePid :
%       Oper        :
% Return :
%       ok | {modified, LinkInfoPid}
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
		            end;
				_ ->
					ok
			end,
            {modified, VDRTablePid1};
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
            end,
            ok
    end.








