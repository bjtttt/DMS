%
% mslog.erl
%

-module(mslog).

-include("../include/header.hrl").

-export([logall/1,
         logall/2,
         lognone/1,
         lognone/2,
         loginfo/1,
         loginfo/2,
         loghint/1,
         loghint/2,
         logerr/1,
         logerr/2,
         log_vdr_statistics_info/2,
         log_vdr_info/3,
         log_vdr_info/4,
         save_msg_4_vdr/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
%   Debug function. Save messages between the VDR and the gateway to disk.
% Parameter :
%       State   :
%       FromVDR :
%       Msg     :
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
save_msg_4_vdr(State, FromVDR, Msg) ->
    VDRID = State#vdritem.id,
    StoredMsg = State#vdritem.storedmsg4save,
    VDRLogPid = State#vdritem.vdrlogpid,
    if
        VDRID == undefined ->
            {Year,Month,Day} = erlang:date(),
            {Hour,Min,Second} = erlang:time(),
            NewMsg = [{FromVDR, Msg, Year, Month, Day, Hour, Min, Second}],
            NewStoredMsg = lists:merge([StoredMsg, NewMsg]),
            mslog:log_vdr_info(none, State, "vdr_handler:save_msg_4_vdr(...) store data : ~p", NewStoredMsg),
            State#vdritem{storedmsg4save=NewStoredMsg};
        true ->
            if
                StoredMsg =/= [] ->
                    mslog:log_vdr_info(none, State, "vdr_handler:save_msg_4_vdr(...) send stored data : ~p", StoredMsg),
                    save_stored_msg_4_vdr(VDRID, StoredMsg, VDRLogPid);
                true ->
                    ok
            end,
            {Year,Month,Day} = erlang:date(),
            {Hour,Min,Second} = erlang:time(),
            DateTime = {Year, Month, Day, Hour, Min, Second},
            mslog:log_vdr_info(none, State, "vdr_handler:save_msg_4_vdr(...) send data : ~p", Msg),
            VDRLogPid ! {save, VDRID, FromVDR, Msg, DateTime},
            State#vdritem{storedmsg4save=[]}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
%   Send messages to the message saving process.
% Parameter :
%       Pid, 
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
save_stored_msg_4_vdr(VDRID, StoredMsg, VDRLogPid) when VDRLogPid =/= undefined,
                                                        is_list(StoredMsg),
                                                        length(StoredMsg) > 0 ->
    [H|T] = StoredMsg,
    {FromVDR, MsgBin, Year, Month, Day, Hour, Min, Second} = H,
    DateTime = {Year, Month, Day, Hour, Min, Second},
    VDRLogPid ! {save, VDRID, FromVDR, MsgBin, DateTime},
    save_stored_msg_4_vdr(VDRID, T, VDRLogPid);
save_stored_msg_4_vdr(_VDRID, _StoredMsg, _VDRLogPid) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description:
%   Log all messages.
% Parameter :
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
logall(Format) ->
    do_log(Format, ?DISP_LEVEL_ALL, 0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description:
%   Log maybe useful messages.
% Parameter :
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lognone(Format) ->
    do_log(Format, ?DISP_LEVEL_NONE, 0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description:
%   Log common messages.
% Parameter :
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
loginfo(Format) ->
    do_log(Format, ?DISP_LEVEL_INFO, 0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description:
%   Log important messages, such as some operation related messages
% Parameter :
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
loghint(Format) ->
    do_log(Format, ?DISP_LEVEL_HINT, 0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description:
%   Log errors.
% Parameter :
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
logerr(Format) ->
    do_log(Format, ?DISP_LEVEL_ERR, 1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Parameter :
%       Format      :
%       CurLevel    : Only when current level is larger than or equal to the display level, can the message be displayed
%       DispErr     :
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
do_log(Format, CurLevel, DispErr) when is_binary(Format),
                                       CurLevel =< ?DISP_LEVEL_ERR,
                                       CurLevel >= ?DISP_LEVEL_ALL,
                                       DispErr =< 1,
                                       DispErr >= 0 ->
    [{displog, DispLog}] = ets:lookup(msgservertable, displog),
    [{displevel, DispLevel}] = ets:lookup(msgservertable, displevel),
    if
        DispLevel =< CurLevel ->
            try
                if
                    DispLog =:= 1 ->
                        if
                            DispErr == 0 ->
                                error_logger:info_msg(binary_to_list(Format));
                            true ->
                                error_logger:error_msg(binary_to_list(Format))
                        end
                end
            catch
                Oper:Msg ->
                    if
                        DispLog =:= 1 ->
                            error_logger:error_msg("do_log(...) exception : ~p : ~p", [Oper, Msg])
                    end
            end
    end;
do_log(Format, CurLevel, DispErr) when is_list(Format),
                                       CurLevel =< ?DISP_LEVEL_ERR,
                                       CurLevel >= ?DISP_LEVEL_ALL,
                                       DispErr =< 1,
                                       DispErr >= 0 ->
    [{displog, DispLog}] = ets:lookup(msgservertable, displog),
    [{displevel, DispLevel}] = ets:lookup(msgservertable, displevel),
    if
        DispLevel =< CurLevel ->
            try
                if
                    DispLog =:= 1 ->
                        if
                            DispErr == 0 ->
                                error_logger:info_msg(Format);
                            true ->
                                error_logger:error_msg(Format)
                        end
                end
            catch
                Oper:Msg ->
                    if
                        DispLog =:= 1 ->
                            error_logger:error_msg("do_log(...) exception : ~p : ~p", [Oper, Msg])
                    end
            end
    end;
do_log(_Format, _CurLevel, _DispErr) ->
    ok.

%
% Description:
%   Log all messages.
% Parameter :
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
logall(Format, Data) ->
    do_log(Format, Data, ?DISP_LEVEL_ALL, 0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description:
%   Log maybe useful messages.
% Parameter :
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lognone(Format, Data) ->
    do_log(Format, Data, ?DISP_LEVEL_NONE, 0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description:
%   Log common messages.
% Parameter :
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
loginfo(Format, Data) ->
    do_log(Format, Data, ?DISP_LEVEL_INFO, 0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description:
%   Log important messages.
% Parameter :
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
loghint(Format, Data) ->
    do_log(Format, Data, ?DISP_LEVEL_HINT, 0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description:
%   Log errors.
% Parameter :
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
logerr(Format, Data) ->
    do_log(Format, Data, ?DISP_LEVEL_ERR, 1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Parameter :
%       Format      :
%       Data        :
%       CurLevel    : Only when current level is larger than or equal to the display level, can the message be displayed
%       DispErr     :
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
do_log(Format, Data, CurLevel, DispErr) when is_binary(Data),
                                             CurLevel =< ?DISP_LEVEL_ERR,
                                             CurLevel >= ?DISP_LEVEL_ALL,
                                             DispErr =< 1,
                                             DispErr >= 0 ->
    [{displog, DispLog}] = ets:lookup(msgservertable, displog),
    [{displevel, DispLevel}] = ets:lookup(msgservertable, displevel),
    if
        DispLevel =< CurLevel ->
            try
                if
                    DispLog =:= 1 ->
                        if
                            DispErr =:= 0 ->
                                error_logger:info_msg(Format, binary_to_list(Data));
                            true ->
                                error_logger:error_msg(Format, binary_to_list(Data))
                        end
                end
            catch
                Oper:Msg ->
                    if
                        DispLog =:= 1 ->
                            error_logger:error_msg("do_log(...) exception : ~p : ~p", [Oper, Msg])
                    end
            end
    end;
do_log(Format, Data, CurLevel, DispErr) when is_list(Data),
                                             CurLevel =< ?DISP_LEVEL_ERR,
                                             CurLevel >= ?DISP_LEVEL_ALL,
                                             DispErr =< 1,
                                             DispErr >= 0 ->
    [{displog, DispLog}] = ets:lookup(msgservertable, displog),
    [{displevel, DispLevel}] = ets:lookup(msgservertable, displevel),
    if
        DispLevel =< CurLevel ->
            try
                if
                    DispLog =:= 1 ->
                        if
                            DispErr == 0 ->
                                error_logger:info_msg(Format, Data);
                            true ->
                                error_logger:error_msg(Format, Data)
                        end
                end
            catch
                Oper:Msg ->
                    if
                        DispLog =:= 1 ->
                            error_logger:error_msg("do_log(...) exception : ~p : ~p", [Oper, Msg])
                    end
            end
    end;
do_log(_Format, _Data, _CurLevel, _DispErr) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
%   VDR sends information to link information process.
% Parameter :
%       State   :
%       Type    :
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
log_vdr_statistics_info(State, Type) ->
    if
        State#vdritem.linkpid =/= undefined ->
            State#vdritem.linkpid ! {self(), Type},
            ok;
        true ->
            log_vdr_info(error, State, "undefined link info pid")
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
% Parameter :
%       Type        : all|none|info|hint|error
%       State       :
%       FormatEx    :
%       DataEx      :
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
log_vdr_info(Type, State, FormatEx) when is_list(FormatEx) ->
    log_vdr_info(Type, State, FormatEx).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
% Parameter :
%       Type        : all|none|info|hint|error
%       State       :
%       FormatEx    :
%       DataEx      :
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
log_vdr_info(Type, State, FormatEx, DataEx) when is_list(FormatEx),
                                                 is_list(DataEx) ->
    Format = "(PID ~p, id ~p, addr ~p, serialno ~p, authcode ~p, vehicleid ~p, vehiclecode ~p, driverid ~p)~n",
    NewFormat = string:concat(Format, FormatEx),
    Data = [self(),
            State#vdritem.id, 
            State#vdritem.addr,
            State#vdritem.serialno,
            State#vdritem.auth,
            State#vdritem.vehicleid,
            State#vdritem.vehiclecode,
            State#vdritem.driverid],
    NewData = lists:append(Data, DataEx),
    if
        Type == error ->
            log:logerr(NewFormat, NewData);
        Type == hint ->
            log:loghint(NewFormat, NewData);
        Type == info ->
            log:loginfo(NewFormat, NewData);
        Type == none ->
            log:lognone(NewFormat, NewData);
        true ->
            log:logall(NewFormat, NewData)
    end;
log_vdr_info(_Type, _State, _FormatEx, _DataEx) ->
    ok.
