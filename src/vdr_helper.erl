%
% vdr_helper.erl
%

-module(vdr_helper).

-include("../include/header.hrl").

-export([split_msg_to_single/1,
       bxorbytelist/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
%   Sometimes, VDR will send more than one messages in one time. So we need to split it to a single message list
% Msg structure :
%     Flag    : == 1 byte
%     Head    : 11 or 15 bytes
%     Body    : >= 0 bytes
%     Parity  : == 1 byte
%     Flag    : == 1 byte
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
split_msg_to_single(Msg) ->
    List = binary_to_list(Msg),
    Len = length(List),
    if
        Len < 15 ->
            [];
        true ->
            HeadFlag = lists:nth(1, List),
            HeadFlag1 = lists:nth(2, List),
            TailFlag = lists:nth(Len, List),
            TailFlag1 = lists:nth(Len-1, List),
            if
                HeadFlag =:= 16#7e andalso TailFlag =:= 16#7e andalso HeadFlag1 =/= 16#7e andalso TailFlag1 =/= 16#7e ->
                    Mid = lists:sublist(List, 2, Len-2),
                    BinMid = list_to_binary(Mid),
                    BinMids = binary:split(BinMid, <<16#7e, 16#7e>>, [global]),
                    convert_bin_array_to_list_array(BinMids);
                true ->
                    []
            end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
convert_bin_array_to_list_array(Bins) when is_list(Bins),
                                           length(Bins) > 0 ->
    [H|T] = Bins,
    HList = binary_to_list(H),
    HListAll = lists:append([[126],HList,[126]]),
    HListAllBin = list_to_binary(HListAll),
    TList = convert_bin_array_to_list_array(T),
    case TList of
        [] ->
            [HListAllBin];
        _ ->
            lists:merge([HListAllBin], TList)
    end;
convert_bin_array_to_list_array(_Bins) ->
    [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% XOR a binary list
% The caller must make sure of the length of data must be larger than or equal to 1
% Input : Data is a binary list
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
bxorbytelist(Data) ->
    Len = byte_size(Data),
    case Len of
        1 ->
            Data;
        2 ->
            <<HInt:8,TInt:8>> = Data,
            Res = HInt bxor TInt,
            <<Res>>;
        _ ->
            <<HInt:8, T/binary>> = Data,
            <<TInt:8>> = bxorbytelist(T),
            Res = HInt bxor TInt,
            <<Res>>
    end.

