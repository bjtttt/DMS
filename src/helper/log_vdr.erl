%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% log_vdr.erl
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(log_vdr).

-include("../../include/header_struct.hrl").

-export([vdr_log_process/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description:
%
% Parameter:
%       LogPid  :
% Return:
%       ok
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
vdr_log_process(VDRList) ->
    receive
        stop ->
            mslog:loghint("VDR log process stops.");
        reset ->
            vdr_log_process([]);
        {set, VID} ->
            VDRExclList = [C || C <- VDRList, C =/= VID],
            NewVDRList = lists:merge([VDRExclList, [VID]]),
            vdr_log_process(NewVDRList);
        {clear, VID} ->
            VDRExclList = [C || C <- VDRList, C =/= VID],
            vdr_log_process(VDRExclList);
        {Pid, count} ->
            Count = length(VDRList),
            Pid ! {Pid, Count},
            vdr_log_process(VDRList);
        {save, VDRID, FromVDR, MsgBin, DateTime} ->
            VDRInclList = [C || C <- VDRList, C =:= VDRID],
            Len = length(VDRInclList),
            if
                Len =:= 1 ->
                    save_msg_4_vdr(VDRID, FromVDR, MsgBin, DateTime),
                    vdr_log_process(VDRList);
                Len > 1 ->
                    VDRExclList = [C || C <- VDRList, C =/= VDRID],
                    NewVDRList = lists:merge([VDRExclList, [VDRID]]),
                    save_msg_4_vdr(VDRID, FromVDR, MsgBin, DateTime),
                    vdr_log_process(NewVDRList);
                true ->
                    vdr_log_process(VDRList)
            end;
        _ ->
            mslog:loghint("VDR log process : unknown message"),
            vdr_log_process(VDRList)
    end.
