%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% log_vdr.erl
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(log_vdr).

-include("../../include/header_struct.hrl").

-export([vdr_log_process/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description:
%
% Parameter:
%   LogPid  :
%   VDRList :
% Return:
%       ok
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
vdr_log_process(LogPid, VDRList) ->
    receive
        stop ->
            log:log_info(LogPid, "VDR log process stops.");
        reset ->
            vdr_log_process([]);
        {check, Pid, VID} ->
            VDRInclList = [C || C <- VDRList, C =:= VID],
            if
                Len =:= 1 ->
                    Pid ! true,
                    vdr_log_process(LogPid, VDRList);
                Len > 1 ->
                    Pid ! true,
                    VDRExclList = [C || C <- VDRList, C =/= VDRID],
                    NewVDRList = lists:merge([VDRExclList, [VDRID]]),
                    vdr_log_process(LogPid, NewVDRList);
                true ->
                    vdr_log_process(LogPid, VDRList)
            end;
        {set, VID} ->
            VDRExclList = [C || C <- VDRList, C =/= VID],
            NewVDRList = lists:merge([VDRExclList, [VID]]),
            vdr_log_process(LogPid, NewVDRList);
        {clear, VID} ->
            VDRExclList = [C || C <- VDRList, C =/= VID],
            vdr_log_process(LogPid, VDRExclList);
        {count, Pid} ->
            Count = length(VDRList),
            Pid ! Count,
            vdr_log_process(LogPid, VDRList);
        {save, VDRID, FromVDR, MsgBin, DateTime} ->
            VDRInclList = [C || C <- VDRList, C =:= VDRID],
            Len = length(VDRInclList),
            if
                Len =:= 1 ->
                    save_msg_4_vdr(LogPid, VDRID, FromVDR, MsgBin, DateTime),
                    vdr_log_process(LogPid, VDRList);
                Len > 1 ->
                    VDRExclList = [C || C <- VDRList, C =/= VDRID],
                    NewVDRList = lists:merge([VDRExclList, [VDRID]]),
                    save_msg_4_vdr(LogPid, VDRID, FromVDR, MsgBin, DateTime),
                    vdr_log_process(LogPid, NewVDRList);
                true ->
                    log:log_warn(LogPid, "VDR log process : VID - ~p, fromVDR - ~p, DateTime - ~p, unsaved message - ~p", [VDRID, FromVDR, DateTime, MsgBin]),
                    vdr_log_process(LogPid, VDRList)
            end;
        UnknownMsg ->
            log:log_err(LogPid, "VDR log process : unknown message - ~p", UnknownMsg),
            vdr_log_process(LogPid, VDRList)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description:
%
% Parameter:
%   LogPid      :
%   VDRID       :
%   FromVDR     :
%   MsgBin      :
%   DateTime    :
% Return:
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
save_msg_4_vdr(LogPid, VDRID, FromVDR, MsgBin, DateTime) ->
    if
        VDRID =/= undefined ->
            File = ?DEF_LOG_PATH ++ "/log/vdr/VDR" ++ integer_to_list(VDRID) ++ ".log",
            case file:open(File, [append]) of
                {ok, IOFile} ->
                    {Year,Month,Day,Hour,Min,Second} = DateTime,
                    case FromVDR of
                        true ->
                            io:format(IOFile, "(~p ~p ~p, ~p:~p:~p) VDR=> ~p~n", [Year,Month,Day,Hour,Min,Second,MsgBin]);
                        _ ->
                            io:format(IOFile, "(~p ~p ~p, ~p:~p:~p) =>VDR ~p~n", [Year,Month,Day,Hour,Min,Second,MsgBin])
                    end,
                    file:close(IOFile);
                {error, Reason} ->
                    log:log_err(LogPid, "Cannot open ~p : ~p", [File, Reason]);
                UnknownMsg ->
                    log:log_err(LogPid, "Cannot open ~p : unknown - ~p", [File, UnknownMsg])
            end;
        true ->
            ok
    end.
