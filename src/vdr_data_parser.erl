%
% This file is use to parse the data from VDR
% Need considering the case when > 1 packages.
% In this case, we need to keep the previous package.
%

-module(vdr_data_parser).

-include("../include/header.hrl").

-export([safe_parse_data/2,
         bxorbytelist/1]).

%%%
%%% check 0x7d
%%%
%restore_data(Data) ->
%    Data.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Parse the data from VDR
% Return :
%     {ok, HeadInfo, Res, State}
%     {ignore, HeadInfo, State}
%     {warning, HeadInfo, ErrorType, State}
%     {error, exception/formaterror/msg_parity_error, State}
%
%     formaterror : Head/Tail is not 16#7e
%     msg_parity_error :
%     warning     : error message/not supported/fail
%     ignore      : not complete message (maybe this state is not necessary)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
safe_parse_data(State, Data) ->
    %DataDebug = <<126,2,0,0,46,1,86,121,16,51,112,0,14,0,0,0,0,0,0,0,17,0,0,0,0,0,0,0,0,0,0,0,0,0,0,19,3,20,0,64,34,1,4,0,0,0,0,2,2,0,0,3,2,0,0,4,2,0,0,42,126>>,
    NewData = get_data_binary(Data),
    try do_parse_data(State, NewData)
    catch
        _:Why ->
            [ST] = erlang:get_stacktrace(),
            log:logvdr(error, State, "vdr_data_processor:safe_parse_data(...) exception : ~p~ndata : ~p~nstack trace : ~p", [Why, Data, ST]),
            {error, ?CONN_STAT_DISC_MSGEX, State}
    end.

get_data_binary(Data) when is_list(Data)->
    [BinData] = Data,
    BinData;
get_data_binary(Data) ->
    Data.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Internal usage for parse_data(Socket, State, Data)
% Return :
%     {ok, HeadInfo, Res, State}
%     {ignore, HeadInfo, State}
%     {warning, HeadInfo, ErrorType, State}
%     {error, formaterror/parityerror, State}
%
%     CONN_STAT_DISC_RESTORE    : Head/Tail is not 16#7e
%     CONN_STAT_DISC_PARITY     :
%     warning     : error message/not supported/fail
%     ignore      : not complete message (maybe this state is not necessary)
%
% HeadInfo = {ID, MsgIdx, Tel, CryptoType}
%
% What is Decoded, still in design
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
do_parse_data(State, Data) ->
    case restore_7d_7e_msg(State, Data) of
        error ->
            % restore_7d_7e_msg will display error message
            {error, ?CONN_STAT_DISC_RESTORE, State};
        {ok, RawData} ->
            NoParityLen = byte_size(RawData) - 1,
            {HeaderBody, Parity} = split_binary(RawData, NoParityLen),
            CalcParity = vdr_helper:bxorbytelist(HeaderBody),
            if
                CalcParity =/= Parity ->
                    mslog:log_vdr_info(error, State, "parity error : calculated ~p, data ~p", [CalcParity, Parity]),
                    {error, ?CONN_STAT_DISC_PARSE_PARITY, State};
                true ->
                    <<ID:16, Property:16, Tel:48, MsgIdx:16, Tail/binary>> = HeaderBody,
                    <<_Reserved:2, Pack:1, CryptoType:3, BodyLen:10>> = <<Property:16>>,
                    HeadInfo = {ID, MsgIdx, Tel, CryptoType},
                    case Pack of
                        0 ->
                            % Single package message
                            Body = Tail,
                            ActBodyLen = byte_size(Body),
                            if
                                BodyLen == ActBodyLen ->
                                    case vdr_msg_processor:parse_msg_body(ID, Body) of
                                        {ok, Result} ->
                                            {ok, HeadInfo, Result, State};
                                        {error, parseerror} ->
                                            mslog:log_vdr_statistics_info(State, ?CONN_STAT_PARSE_ERROR),
                                            mslog:log_vdr_info(error, State, "vdr_data_processor:do_parse_data(...) : parsing message (id ~p) fails, data ~p", [ID, Data]),
                                            {warning, HeadInfo, parseerror, State};
                                        {error, parseexception} ->
                                            mslog:log_vdr_statistics_info(State, ?CONN_STAT_PARSE_EXCEPTION),
                                            % Don't need display message here because vdr_msg_processor:parse_msg_body(...) has already done it.
                                            {warning, HeadInfo, ?P_GENRESP_ERRMSG, State};
                                        {error, unsupported} ->
                                            mslog:log_vdr_statistics_info(State, ?CONN_STAT_PARSE_UNSUPPORTED_ID),
                                            % Don't need display message here because vdr_msg_processor:parse_msg_body(...) has already done it.
                                            {warning, HeadInfo, ?P_GENRESP_ERRMSG, State}
                                    end;
                                BodyLen =/= ActBodyLen ->
									mslog:log_vdr_statistics_info(State, ?CONN_STAT_PARSE_LEN_MISMATCH),
                                    mslog:log_vdr_info(error, State, "vdr_data_processor:do_parse_data(...) : message id ~p, calculated length ~p =/= actual length ~p, data ~p", [ID, BodyLen, ActBodyLen, Data]),
                                    {warning, HeadInfo, ?P_GENRESP_ERRMSG, State}
                            end;
                        1 ->
                            % Multi package message
                            <<PackInfo:32,Body/binary>> = Tail,
                            ActBodyLen = byte_size(Body),
                            <<Total:16,Index:16>> = <<PackInfo:32>>,
                            if
                                Total =< 1 ->
									mslog:log_vdr_statistics_info(State, ?CONN_STAT_PARSE_TOTAL_ERROR),
                                    mslog:log_vdr_info(error, State, "wrong total number ~p for msg id ~p : data ~p", [Total, ID, Data]),
                                    {warning, HeadInfo, ?P_GENRESP_ERRMSG, State};
                                Total > 1 ->
                                    if
                                        Index < 1 ->
											mslog:log_vdr_statistics_info(State, ?CONN_STAT_PARSE_INDEX_SMALL),
                                            common:loginfo("wrong index ~p for msg id ~p : data ~p", [Index, ID, Data]),
                                            {warning, HeadInfo, ?P_GENRESP_ERRMSG, State};
                                        Index > Total ->
											mslog:log_vdr_statistics_info(State, ?CONN_STAT_PARSE_INDEX_LARGE),
                                            common:loginfo("wrong index ~p is larger than total number ~p for msg id ~p : data ~p", [Index, Total, ID, Data]),
                                            {warning, HeadInfo, ?P_GENRESP_ERRMSG, State};
                                        Index =< Total ->
                                            if
                                                BodyLen == ActBodyLen ->
                                                    case combine_msg_packs(State, ID, MsgIdx, Total, Index, Body) of
                                                        {complete, Msg, NewState} ->
                                                            vdr_handler:logvdr(info, State, "combined MSG (ID ~p): ~p", [ID, Msg]),
                                                            case vdr_data_processor:parse_msg_body(ID, Msg) of
                                                                {ok, Result} ->
                                                                    {ok, HeadInfo, Result, NewState};
                                                                {warning, msgerror} ->
                                                                    {warning, HeadInfo, ?P_GENRESP_ERRMSG, NewState};
                                                                {warning, unsupported} ->
                                                                    {warning, HeadInfo, ?P_GENRESP_NOTSUPPORT, NewState}
                                                            end;
                                                        {notcomplete, NewState} ->
                                                            {ignore, HeadInfo, NewState}
                                                    end;
                                                BodyLen =/= ActBodyLen ->
													common:send_stat_err(State, lenerr),
                                                    common:loginfo("Length error for msg (~p) from (~p) : (Field)~p:(Actual)~p", [MsgIdx, State#vdritem.addr, BodyLen, ActBodyLen]),
                                                    {warning, HeadInfo, ?P_GENRESP_ERRMSG, State}
                                            end
                                    end
                            end
                    end
            end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
%   0x7d and 0x7e are taken as 0x7d0x1 and 0x7d0x2 in transfer. This method is used to restore 0x7d0x1 and 0x7d0x2 to 0x7d and 0x7e
%   0x7d0x1 -> 0x7d & 0x7d0x2 -> 0x7e
% Parameter :
%       State   :
%       Data    :
% Return :
%       {ok, FinalResult}
%       error
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
restore_7d_7e_msg(State, Data) ->
    Len = byte_size(Data),
    {Head, Remain} = split_binary(Data, 1),
    {Body, Tail} = split_binary(Remain, Len-2),
    if
        Head == <<16#7e>> andalso Tail == <<16#7e>> ->
		    Result1 = binary:replace(Body, <<125,1>>, <<255,254,253,252,251,250,251,252,253,254,255>>, [global]),
		    FinalResult1 = binary:replace(Result1, <<125,2>>, <<245,244,243,242,241,240,241,242,243,244,245>>, [global]),
		    Result = binary:replace(FinalResult1, <<255,254,253,252,251,250,251,252,253,254,255>>, <<125>>, [global]),
		    FinalResult = binary:replace(Result, <<245,244,243,242,241,240,241,242,243,244,245>>, <<126>>, [global]),
            {ok, FinalResult};
        true ->
            mslog:log_vdr_info(error, State, "wrong data head/tail : ~p / ~p",[Head, Tail]),
            error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Check whether received a complete msg packages
% State#vdritem.msg : [[ID0,MsgIdx0,Total0,Index0,Data0],[ID1,MsgIdx1,Total1,Index1,Data1],[ID2,MsgIdx2,Total2,Index2,Data2],..
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
combine_msg_packs(State, ID, MsgIdx, Total, Idx, Body) ->
    NewMsg = [ID, MsgIdx, Total, Idx, Body],
    StoredMsges = State#vdritem.msg,
    % Get all msg packages with the same ID
    % [E || E <- State#vdritem.msg, [HID,_HFlowNum,_HTotal,_HIdx,_HBody] = E, HID == ID ]
    MsgesWithID = get_msg_with_id(StoredMsges, ID),
    % Get all msg packages without the same ID
    % [E || E <- State#vdritem.msg, [HID,_HFlowNum,_HTotal,_HIdx,_HBody] = E, HID =/= ID ]
    MsgesWithoutID = get_msg_without_id(StoredMsges, ID),
    % What is LastID for?
    {_LastID, MsgPackages} = State#vdritem.msgpackages,
    case MsgesWithID of
        [] ->
			NewStoredMsges = lists:merge(StoredMsges, [NewMsg]),
            NewMsgPackages = update_msg_packs(MsgPackages, ID, get_missing_pack_msgidxs([NewMsg])),
			mslog:log_vdr_info(info, State, "new MSG packages : ~p", [NewMsgPackages]),
    		NewState = State#vdritem{msg=NewStoredMsges, msgpackages={ID, NewMsgPackages}},
			{notcomplete, NewState};
    	_ ->
			case check_ignored_msg(State, MsgesWithID, MsgIdx, Total, Idx) of
                replace ->
                    
				new ->
					NewMsgWithID = [[ID, MsgIdx, Total, Idx, Body]],
		            case check_msg(NewMsgWithID, Total) of
		                ok ->
		                    Msg = compose_msg(NewMsgWithID, Total),
		                    [H|_T] = Msg,
		                    [_ID, FirstMsgIdx, _Total, _Idx, _Body] = H,
		                    case check_msg_idx(Msg, FirstMsgIdx) of
		                        ok ->
		                            NewState = State#vdritem{msg=MsgesWithoutID},
		                            BinMsg = compose_real_msg(Msg),
                                    NewMsgPackages = remove_msgidx_with_id(MsgPackages, ID),
									common:loginfo("VDR (~p) (id:~p, serialno:~p, authen_code:~p, vehicleid:~p, vehiclecode:~p)~nNew msg packages 1 : ~p", 
												   [State#vdritem.addr,
													State#vdritem.id,
													State#vdritem.serialno,
													State#vdritem.auth,
													State#vdritem.vehicleid,
													State#vdritem.vehiclecode,
													NewMsgPackages]),
		                            {complete, BinMsg, NewState#vdritem{msgpackages={-1, NewMsgPackages}}};
		                        error ->
									MergedList = lists:merge(NewMsgWithID, MsgWithoutID),
                                    NewMsgPackages = update_msg_packs(MsgPackages, ID, get_missing_pack_msgidxs(NewMsgWithID)),
									common:loginfo("VDR (~p) (id:~p, serialno:~p, authen_code:~p, vehicleid:~p, vehiclecode:~p)~nNew msg packages 2 : ~p", 
												   [State#vdritem.addr,
													State#vdritem.id,
													State#vdritem.serialno,
													State#vdritem.auth,
													State#vdritem.vehicleid,
													State#vdritem.vehiclecode,
													NewMsgPackages]),
		                            NewState = State#vdritem{msg=MergedList, msgpackages={ID, NewMsgPackages}},
		                            {notcomplete, NewState}
		                    end;
		                error ->
							MergedList = lists:merge(NewMsgWithID, MsgWithoutID),
                            NewMsgPackages = update_msg_packs(MsgPackages, ID, get_missing_pack_msgidxs(NewMsgWithID)),
							common:loginfo("VDR (~p) (id:~p, serialno:~p, authen_code:~p, vehicleid:~p, vehiclecode:~p)~nNew msg packages 3 : ~p", 
										   [State#vdritem.addr,
											State#vdritem.id,
											State#vdritem.serialno,
											State#vdritem.auth,
											State#vdritem.vehicleid,
											State#vdritem.vehiclecode,
											NewMsgPackages]),
		                    NewState = State#vdritem{msg=MergedList, msgpackages={ID, NewMsgPackages}},
		                    {notcomplete, NewState}
		            end;
				false ->
					DelPack = del_pack_with_idx(MsgWithID, MsgIdx, Total, Idx),
				    NewMsgWithID = lists:merge([[ID, MsgIdx, Total, Idx, Body]], DelPack),
		            case check_msg(NewMsgWithID, Total) of
		                ok ->
		                    Msg = compose_msg(NewMsgWithID, Total),
		                    [H|_T] = Msg,
		                    [_ID, FirstMsgIdx, _Total, _Idx, _Body] = H,
		                    case check_msg_idx(Msg, FirstMsgIdx) of
		                        ok ->
		                            NewState = State#vdritem{msg=MsgWithoutID},
		                            BinMsg = compose_real_msg(Msg),
                                    NewMsgPackages = remove_msgidx_with_id(MsgPackages, ID),
									common:loginfo("VDR (~p) (id:~p, serialno:~p, authen_code:~p, vehicleid:~p, vehiclecode:~p)~nNew msg packages 4 : ~p", 
												   [State#vdritem.addr,
													State#vdritem.id,
													State#vdritem.serialno,
													State#vdritem.auth,
													State#vdritem.vehicleid,
													State#vdritem.vehiclecode,
													NewMsgPackages]),
		                            {complete, BinMsg, NewState#vdritem{msgpackages={-1, NewMsgPackages}}};
		                        error ->
									MergedList = lists:merge(NewMsgWithID, MsgWithoutID),
                                    NewMsgPackages = update_msg_packs(MsgPackages, ID, get_missing_pack_msgidxs(NewMsgWithID)),
									common:loginfo("VDR (~p) (id:~p, serialno:~p, authen_code:~p, vehicleid:~p, vehiclecode:~p)~nNew msg packages 5 : ~p", 
												   [State#vdritem.addr,
													State#vdritem.id,
													State#vdritem.serialno,
													State#vdritem.auth,
													State#vdritem.vehicleid,
													State#vdritem.vehiclecode,
													NewMsgPackages]),
		                            NewState = State#vdritem{msg=MergedList, msgpackages={ID, NewMsgPackages}},
		                            {notcomplete, NewState}
		                    end;
		                error ->
							MergedList = lists:merge(NewMsgWithID, MsgWithoutID),
                            NewMsgPackages = update_msg_packs(MsgPackages, ID, get_missing_pack_msgidxs(NewMsgWithID)),
							common:loginfo("VDR (~p) (id:~p, serialno:~p, authen_code:~p, vehicleid:~p, vehiclecode:~p)~nNew msg packages 6 : ~p", 
										   [State#vdritem.addr,
											State#vdritem.id,
											State#vdritem.serialno,
											State#vdritem.auth,
											State#vdritem.vehicleid,
											State#vdritem.vehiclecode,
											NewMsgPackages]),
		                    NewState = State#vdritem{msg=MergedList, msgpackages={ID, NewMsgPackages}},
		                    {notcomplete, NewState}
		            end;
				_ ->
					ok
			end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% MsgPacks 	: [[ID0, FirstMsgIdx0, MsgIdxList0], [ID0, FirstMsgIdx0, MsgIdxList0], [ID0, FirstMsgIdx0, MsgIdxList0], ...]
% ID		: 
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
remove_msgidx_with_id(MsgPacks, ID) when is_list(MsgPacks),
									     length(MsgPacks),
									     is_integer(ID),
                                         ID > 0 ->
    [H|T] = MsgPacks,
    [HID, _HFirstMsgIdx, _HMsgIdxs] = H,
    if
        HID == ID ->
            T;
        true ->
			case T of
				[] ->
					[H];
				_ ->
            		lists:merge([H], remove_msgidx_with_id(T, ID))
			end
    end;
remove_msgidx_with_id(_MsgPacks, _ID) ->
    [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
%   Update the recieived MSG indexes after receiving a new MSG with the same ID
% Parameter :
%       MsgPacks	: [[ID0, FirstMsgIdx0, MsgIdxs0], [ID1, FirstMsgIdx1, MsgIdxs1], [ID2, FirstMsgIdx2, MsgIdxs2], ...]
%                       MsgIdexn is the indexes of all the received messages with the same ID 
%       ID		    :
%       MsgIdxs	    : [MsgIdx0, MsgIdx1, MsgIdx2, ...] --- It should be the output of get_missing_pack_msgidxs(MsgWithID)
%                       If MsgIdxs == [], the ID related item will be removed from MsgPacks
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
update_msg_packs(MsgPacks, ID, MsgIdxs) when is_list(MsgPacks),
                                             length(MsgPacks) > 0,
                                             is_integer(ID),
                                             ID > 0,
											 is_list(MsgIdxs) ->
    [H|T] = MsgPacks,
    [HID, HFirstMsgIdx, _HMsgIdxs] = H,
	if
		HID == ID ->
            case MsgIdxs of
                [] ->
                    MsgPacks;
                _ ->
                    lists:merge([[ID, HFirstMsgIdx, MsgIdxs]], T)
            end;
		true ->
			lists:merge([H], update_msg_packs(T, ID, MsgIdxs))
	end;
update_msg_packs(MsgPacks, _ID, _MsgIdxs) ->
	MsgPacks.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Parameter :
%       MsgWithID	: [[ID0, MsgIdx0, Total0, Idx0, Body0], [ID1, MsgIdx1, Total1, Idx1, Body1], [ID2, MsgIdx2, Total2, Idx2, Body2], ...]
%
% Output :
%       {FirstMsgIdx, [MsgIdxn0, MsgIdxn1, MsgIdxn2, ...]}
%           MsgIdxnn is not in MsgWithID
%       none
%           it means it is the 1st message package
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_missing_pack_msgidxs(MsgesWithID) when is_list(MsgesWithID),
                                           length(MsgesWithID) > 0 ->
    Last = lists:last(MsgesWithID),
    [_ID, LMsgIdx, LTotal, LIdx, _Body] = Last,
    FirstMsgIdx = LMsgIdx - (LTotal - LIdx),
    if
        LIdx == 1 ->
            none;
        true ->
            MissingIdxs = del_num_from_num_list([E || E <- lists:seq(1, LIdx)], MsgesWithID),
            {FirstMsgIdx, calc_missing_pack_msgidxs(MissingIdxs, LMsgIdx, LIdx)}
    end;
get_missing_pack_msgidxs(_MsgesWithID) ->
    none.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
%   Convert message package index to message index. For example, the 1st message index is 2123, and the indexes are [1,3,4,5].
%   The final list will be [2123,2125,2126,2127]
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
calc_missing_pack_msgidxs(MissingIdxs, LastMsgIdx, LastIdx) when is_list(MissingIdxs),
                                                                 length(MissingIdxs) > 0,
                                                                 is_integer(LastMsgIdx),
                                                                 LastMsgIdx > 0,
                                                                 is_integer(LastIdx),
                                                                 LastIdx > 0 ->
    [H|T] = MissingIdxs,
    MissingMsgIdx = LastMsgIdx - (LastIdx - H),
    lists:merge([MissingMsgIdx], calc_missing_pack_msgidxs(T, LastMsgIdx, LastIdx));
calc_missing_pack_msgidxs(_MissingIdxs, _LastMsgIdx, _LastIdx) ->
    [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
%   Check whether this message should be ignored or not.
% Parameter :
% Return :
%       true    : the current message should be ignored
%       false   :
%       replace : the previous message should be replaced due to the same message index
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
check_ignored_msg(State, MsgesWithID, MsgIdx, Total, Idx) when is_list(MsgesWithID),
                                                               length(MsgesWithID) > 0,
                                                               is_integer(MsgIdx),
                                                               is_integer(Total),
                                                               is_integer(Idx),
                                                               Total >= Idx ->
	[H|_T] = MsgesWithID,
	[_ID, HMsgIdx, HTotal, HIdx, _Body] = H,
	if
        HTotal =/= Total ->
            true;
        true ->
            if
                HMsgIdx == MsgIdx ->
                    replace;
                true ->
                    Diff0 = HMsgIdx - MsgIdx,
                    Diff1 = HIdx - Idx,
                    if
                        Diff0 == Diff1 ->
                            false;
                        true ->
                            true
                    end
            end
	end;
check_ignored_msg(_State, _MsgesWithID, _MsgIdx, _Total, _Idx) ->
    true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Msg : [[ID0,FlowNum0,Total0,Index0,Data0],[ID1,FlowNum1,Total1,Index1,Data1],[ID2,FlowNum2,Total2,Index2,Data2],...
%   This function is to created a new list with the ones whose IDn is the same as ID.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_msg_with_id(Msg, ID) when is_list(Msg),
                              length(Msg) > 0 ->
    [H|T] = Msg,
    [HID, _HMsgIdx, _HTotal ,_HIdx, _HBody] = H,
    TWithID = get_msg_with_id(T, ID),
    if
        HID == ID ->
            if
                TWithID == [] ->
                    [H];
                true ->
                    lists:merge([H], TWithID)
            end;
        HID =/= ID ->
            TWithID
    end;
get_msg_with_id(_Msg, _ID) ->
    [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Msg : [[ID0,FlowNum0,Total0,Index0,Data0],[ID1,FlowNum1,Total1,Index1,Data1],[ID2,FlowNum2,Total2,Index2,Data2],...
% This function is to created a new list with the ones whose IDn is NOT the same as ID.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_msg_without_id(Msg, ID) when is_list(Msg),
                                 length(Msg) > 0 ->
    [H|T] = Msg,
    [HID, _HMsgIdx, _HTotal, _HIdx, _HBody] = H,
    TWithoutID = get_msg_without_id(T, ID),
    if
        HID == ID ->
            TWithoutID;
        HID =/= ID ->
            if
                TWithoutID == [] ->
                    [H];
                true ->
                    lists:merge([H], TWithoutID)
            end
    end;
get_msg_without_id(_Msg, _ID) ->
    [].

%%%
%%% Remove msg package with the same Index from the msg packages
%%% Before calling this method, please first call get_msg_with_id(Msg, ID) to get the msg packages with the ID.
%%%
del_pack_with_idx(Msg, MsgIdx, Total, Idx) ->
    case Msg of
        [] ->
            [];
        _ ->
            [H|T] = Msg,
            [_HID, HMsgIdx, HTotal, HIdx, _HBody] = H,
            if
                HMsgIdx + (HTotal - HIdx) < MsgIdx ->
                    % This is the 2nd msg and discard all the 1st ones
                    del_pack_with_idx(T, MsgIdx, Total, Idx);
                HMsgIdx + (HTotal - HIdx) >= MsgIdx ->
                    DiffTotal = HTotal - Total,
                    if
                        DiffTotal == 0 ->
                            if
                                Idx == HIdx ->
                                    del_pack_with_idx(T, MsgIdx, Total, Idx);
                                Idx =/= HIdx ->
                                    lists:merge([H], del_pack_with_idx(T, MsgIdx, Total, Idx))
                            end;
                        DiffTotal =/= 0 ->
                            % Take the new msg package as the standard
                            del_pack_with_idx(T, MsgIdx, Total, Idx)
                    end
            end
    end.

%%%
%%% Internal usage for combinemsgpacks(State, ID, FlowNum, Total, Idx, Body)
%%% Check whether Packages includes all packages by checking the package index
%%%
check_msg(Packages, Total) ->
    Len = length(Packages),
    if
        Len == Total ->
            case del_num_from_num_list([E || E <- lists:seq(1, Total)], Packages) of
                [] ->
                    ok;
                _ ->
                    error
            end;
        Len =/= Total ->
			common:loginfo("Check message error : Packages length ~p =/= Total ~p", [Len, Total]),
            error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Internal usage for checkmsg(Packages, Total)
% Remove the package index from the complete package index list
% Return the missing package index list
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
del_num_from_num_list(NumList, Packages) ->
    case Packages of
        [] ->
            NumList;
        _ ->
            [H|T] = Packages,
            [_ID, _MsgIdx, _Total, Idx, _Body] = H,
            NewNumList = [E || E <- NumList, E =/= Idx],
            del_num_from_num_list(NewNumList, T)
    end.

%%%
%%% Internal usage for combinemsgpacks(State, ID, FlowNum, Total, Idx, Body)
%%% The caller will check the length of Packages first
%%%
compose_msg(Packages, Total) ->
    if
        Total < 1 ->
            [];
        Total >= 1 ->
            lists:merge([get_package_by_idx(Packages, Total)], compose_msg(Packages, Total-1))
    end.

%%%
%%% Internal usag for composemsg(Packages, Total)
%%%
get_package_by_idx(Packages, Idx) ->
    case Packages of
        [] ->
            [];
        _ ->
            [H|T] = Packages,
            [_ID, _MsgIdx, _Total, HIdx, _Body] = H,
            if
                HIdx == Idx ->
                    H;
                HIdx =/= Idx ->
                    get_package_by_idx(T, Idx)
            end
    end.

%%%
%%% Internal usage for combine_msg_packs(State, ID, FlowNum, Total, Idx, Body)
%%% Caller has make sure that Msg is not []
%%%
check_msg_idx(Msg, MsgIdx) ->
    case Msg of
        [] ->
            ok;
        _ ->
            [H|T] = Msg,
            [_ID, HMsgIdx, _Total, _HIdx, _Body] = H,
            DiffMsgIdx = HMsgIdx - MsgIdx,
            if
                DiffMsgIdx == 0 ->
                    check_msg_idx(T, MsgIdx+1);
                DiffMsgIdx =/= 0 ->
                    error
            end
    end.

%%%
%%%
%%%
compose_real_msg(Msges) when is_list(Msges),
							 length(Msges) > 0 ->
    [H|T] = Msges,
    [_ID,_MsgIdx,_Total,_HIdx,Body] = H,
    list_to_binary([Body, compose_real_msg(T)]);
compose_real_msg(_Msges) ->
	<<>>.


