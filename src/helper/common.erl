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
         is_dec_list/1,
         convert_word_hex_string_to_integer/1,
		 integer_to_binary/1,
         integer_to_2byte_binary/1,
		 integer_to_size_binary/2,
		 integer_list_to_size_binary_list/2,
         float_to_binary/1,
		 get_str_bin_to_bin_list/1,
		 send_vdr_table_operation/2,
		 get_binary_from_list/1,
		 get_list_from_binary/1]).

-export([set_sockopt/3]).

-export([safepeername/1, forcesafepeername/1, printsocketinfo/2, forceprintsocketinfo/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
split_msg_to_packages(Data, PackLen) when is_integer(PackLen),
							    		  PackLen > 0,
										  is_binary(Data),
										  PackLen =< byte_size(Data) ->
	Bins = do_split_msg_to_packages(Data, PackLen),
	Len = length(Bins),
	if
		Len > 1 ->
			add_sub_pack_suffix_to_bin_list(Bins, [], Len);%,
		true ->
			Bins
	end;
split_msg_to_packages(Data, PackLen) when is_integer(PackLen),
							    		  PackLen > 0,
										  is_binary(Data),
										  PackLen >= byte_size(Data) ->
	[Data];
split_msg_to_packages(_Data, _PackLen) ->
	[].

do_split_msg_to_packages(Data, PackLen) when is_integer(PackLen),
							    		     PackLen > 0,
										     is_binary(Data),
											 PackLen < byte_size(Data) ->
	Len = byte_size(Data),
	if
		PackLen >= Len ->
			[Data];
		true ->
			H = binary:part(Data, 0, PackLen),
			T = binary:part(Data, PackLen, Len-PackLen),
			%common:loginfo("Total size ~p : New subpackage size ~p : Left size ~p~n~p", [Len, byte_size(H), byte_size(T), H]),
			[H|do_split_msg_to_packages(T, PackLen)]
	end;
do_split_msg_to_packages(Data, PackLen) when is_integer(PackLen),
							    		     PackLen > 0,
										     is_binary(Data),
											 PackLen >= byte_size(Data)->
	[Data];
do_split_msg_to_packages(_Data, _PackLen) ->
	[].

add_sub_pack_suffix_to_bin_list(SrcList, DestList, TotalLen) when is_list(SrcList),
																  length(SrcList) > 0,
																  is_list(DestList) ->
	DestLen = length(DestList),
	[H|T] = SrcList,
	HNew = list_to_binary([?SUB_PACK_INDI_HEADER,
					       <<TotalLen:16>>,
					       <<(DestLen+1):16>>,
						   H]),
	DestListNew = lists:merge(DestList, [HNew]),
	add_sub_pack_suffix_to_bin_list(T, DestListNew, TotalLen);
add_sub_pack_suffix_to_bin_list(_SrcList, DestList, _TotalLen) when is_list(DestList) ->
	DestList;
add_sub_pack_suffix_to_bin_list(_SrcList, _DestList, _TotalLen) ->
	[].

make_sure_n_byte_binary(Bin, N) when is_binary(Bin),
									 N > 0 ->
	Size = bit_size(Bin),
	<<Int:Size>> = Bin,
	<<Int:(N*?LEN_BYTE)>>;
make_sure_n_byte_binary(_Bin, _N) ->
	<<>>.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Parameter 	: Integer = 48
% Output		: <<"48">>
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
integer_to_binary(Integer) when is_integer(Integer) ->
    list_to_binary(integer_to_list(Integer));
integer_to_binary(_Integer) ->
    <<>>.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Parameter 	: Integer = 48, ByteSize = 2
%%% Output		: <<0, 48>>
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
integer_to_size_binary(Integer, ByteSize) when is_integer(Integer),
										       is_integer(ByteSize),
										       ByteSize >= 0 ->
    <<Integer:(ByteSize*8)>>;
integer_to_size_binary(_Integer, _ByteSize) ->
    <<>>.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Input : 5
% 	Must >= 0
% Output : 101
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
convert_integer_to_binary_string_list(Int) when is_integer(Int),
												Int >= 0 ->
	Bit = Int band 1,
	BitChar = integer_to_list(Bit),
	NewInt = Int bsr 1,
	if
		NewInt > 0 ->
			lists:merge([convert_integer_to_binary_string_list(NewInt), BitChar]);
		true ->
			BitChar
	end;				
convert_integer_to_binary_string_list(Int) ->
	integer_to_list(Int).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Parameter 	: IDs = [48, 32], ByteSize = 2
%%% Output		: <<0, 48, 0, 32>>
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
integer_list_to_size_binary_list(IDs, ByteSize) when is_list(IDs),
						          		             length(IDs) > 0,
												     is_integer(ByteSize),
												     ByteSize >= 0 ->
	[H|T] = IDs,
	case T of
		[] ->
			common:integer_to_size_binary(H, ByteSize);
		_ ->
			list_to_binary([common:integer_to_size_binary(H, ByteSize), integer_list_to_size_binary_list(T, ByteSize)])
	end;
integer_list_to_size_binary_list(_IDs, _ByteSize) ->
	<<>>.

%%%
%%% It is only for this case : 10 -> <<"10">>
%%% Not for 10 -> <<0, 10>> which should use <<10:16>>
%%%
integer_to_2byte_binary(Integer) when is_integer(Integer) ->
    List = integer_to_list(Integer),
    Len = length(List),
    if
        Len > 1 ->
            list_to_binary(List);
        true ->
            list_to_binary(["0", List])
    end;
integer_to_2byte_binary(_Integer) ->
    <<>>.

%%%
%%%
%%%
float_to_binary(Float) ->
    list_to_binary(float_to_list(Float)).

%%%
%%%
%%%
convert_bcd_integer(Number) ->
    (Number div 16) * 10 + (Number rem 16).

convert_integer_bcd(Number) ->
	(Number div 10) * 16 + (Number rem 10).

%%%
%%% Convert number list to binary.
%%% List    : [Num0, Num1, Num2, ...]
%%% NumLen  : Number length
%%% Return  : [<<Num0:NumLen>>, <<Num1:NumLen>>, <<Num2:NumLen>>, ...]
%%%
number_list_to_binary(List, NumLen) ->
    [H|T] = List,
    case T of
        [] ->
            <<H:NumLen>>;
        _ ->
            [<<H:NumLen>>|number_list_to_binary(T, NumLen)]
    end.

%%%
%%% Remove [FlowNumN, MsgN] according to FlowNum
%%% Msg : [[FlowNum0, Msg0], [FlowNum1, Msg1], [FlowNum2, Msg2], ...]
%%%
removemsgfromlistbyflownum(FlowNum, Msg) ->
    case Msg of
        [] ->
            [];
        _ ->
            [H|T] = Msg,
            [FN, _M] = H,
            if
                FN == FlowNum ->
                    removemsgfromlistbyflownum(FlowNum, T);
                FN =/= FlowNum ->
                    [H|removemsgfromlistbyflownum(FlowNum, T)]
            end
    end.

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

%%%
%%% {ok {Address, Port}}
%%% {error, Reason|Why}
%%%
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

%%%
%%% {ok, {Address, Port}}
%%% {ok, {"0.0.0.0", 0}}
%%%
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% true only when Value is NOT an empty integer string
% For example, "81", "8A" and "0x8B" is true
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
is_integer_string(Value) when is_list(Value) ->
    Len = length(Value),
    if
        Len < 1 ->
            false;
        true ->
            Fun = fun(X) ->
                          if X >= 48 orelse X =< 57 -> 
								 true;
							 X == 88 orelse X == 120 ->
								 true;
							 X >= 65 orelse X =< 70 ->
								 true;
							 X >= 97 orelse X =< 102 ->
								 true;
                             true -> 
								 false
                          end
                  end,
            lists:all(Fun, Value)
    end;
is_integer_string(_Value) ->
    false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
is_dec_list(List) when is_list(List),
                       length(List) > 0 ->
    
    Fun = fun(X) ->
                  case is_integer(X) of
                      true -> true;
                      _ -> false
                  end
          end,
    lists:all(Fun, List).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Check whether "NNNN", "NN" or any string like this format to be a valid decimal string or not.
% For example, "81" is true while "8A" is false
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
is_dec_integer_string(Value) when is_list(Value) ->
    Len = length(Value),
    if
        Len < 1 ->
            false;
        true ->
            Fun = fun(X) ->
                          if 
                              X < 48 orelse X > 57 -> 
                                false;
                              true -> true
                          end
                  end,
            lists:all(Fun, Value)
    end;
is_dec_integer_string(_Value) ->
    false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Check whether "NNNN", "NN" or any string like this format to be a valid hex string or not.
% For example, "81" and "8A" are both true
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
is_hex_integer_string(Value) when is_list(Value) ->
    case is_integer_string(Value) of
        true ->
            Len = length(string:tokens(Value, "xX")),
            if
                Len == 2 ->
                    true;
                true ->
                    false
            end;
        _ ->
            false
    end;
is_hex_integer_string(_Value) ->
    false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% WordHexString : 32 bit
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
convert_word_hex_string_to_integer(WordHexStr) when is_list(WordHexStr) ->
    case is_hex_integer_string(WordHexStr) of
        true ->
            [_Pre, Value] = string:tokens(WordHexStr, "xX"),
			try
				list_to_integer(Value, 16)
			catch
				_:_ ->
					16#FFFF
			end;
        _ ->
            0
    end;
convert_word_hex_string_to_integer(_WordHexStr) ->
    0.

get_str_bin_to_bin_list(S) when is_list(S),
							    length(S) > 0 ->
	B = list_to_binary(S),
	get_str_bin_to_bin_list(B);
get_str_bin_to_bin_list(S) when is_binary(S),
							    byte_size(S) > 0 ->
	H = binary:part(S, 0, 1),
	T = binary:part(S, 1, byte_size(S)-1),
	<<HInt:8>> = H,
	[integer_to_list(HInt)|get_str_bin_to_bin_list(T)];
get_str_bin_to_bin_list(_S) ->
	[].

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

get_binary_from_list(List) when is_list(List) ->
	try
		erlang:list_to_binary(List)
	catch
		_:_ ->
			<<"">>
	end;
get_binary_from_list(List) when is_binary(List) ->
	List;
get_binary_from_list(_List) ->
	<<"">>.

get_list_from_binary(Binary) when is_binary(Binary) ->
	try
		erlang:binary_to_list(Binary)
	catch
		_:_ ->
			[]
	end;
get_list_from_binary(Binary) when is_list(Binary) ->
	Binary;
get_list_from_binary(_Binary) ->
	[].








