%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% mslog.erl
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(mslog).

-include("../include/header_const.hrl").
-include("../include/header_struct.hrl").

-export([log_all/1,
         log_all/3,
         log_info/1,
         log_info/3,
         log_hint/1,
         log_hint/3,
         log_err/1,
         log_err/3]).
%         log_vdr_statistics_info/2,
%         log_vdr_info/3,
%         log_vdr_info/4,
%         save_msg_4_vdr/3]).

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
%       Data        : binary/list to be displayed
% Return :
%       ok
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
log_all(Data) when is_binary(Data) ->
    do_log(binary_to_list(Data), ?DISP_LEVEL_ALL);
log_all(Data) when is_list(Data) ->
    do_log(Data, ?DISP_LEVEL_ALL);
log_all(_Data) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description:
%   Log all messages.
% Parameter :
%       Data        : binary/list to be displayed
%       Log         : YES/NO, should be the system defined display log enabled
%       LogLevel    : DISP_LEVEL_ALL/DISP_LEVEL_INFO/DISP_LEVEL_IMP/DISP_LEVEL_ERR, should be the system defined display log level
% Return :
%       ok
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
log_all(Data, Log, LogLevel) when is_binary(Data),
                                 Log >= ?NO,
                                 Log =< ?YES,
                                 LogLevel >= ?DISP_LEVEL_ALL,
                                 LogLevel =< ?DISP_LEVEL_ERR ->
    do_log(Data, ?DISP_LEVEL_ALL, Log, LogLevel);
log_all(Data, Log, LogLevel) when is_list(Data),
                                 Log >= ?NO,
                                 Log =< ?YES,
                                 LogLevel >= ?DISP_LEVEL_ALL,
                                 LogLevel =< ?DISP_LEVEL_ERR ->
    do_log(Data, ?DISP_LEVEL_ALL, Log, LogLevel);
log_all(_Data, _Log, _LogLevel) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description:
%   Log information messages.
% Parameter :
%       Data        : binary/list to be displayed
% Return :
%       ok
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
log_info(Data) when is_binary(Data) ->
    do_log(Data, ?DISP_LEVEL_INFO);
log_info(Data) when is_list(Data) ->
    do_log(Data, ?DISP_LEVEL_INFO);
log_info(_Data) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description:
%   Log information messages.
% Parameter :
%       Data        :
%       Log         : YES/NO, should be the system defined display log enabled
%       LogLevel    : DISP_LEVEL_ALL/DISP_LEVEL_INFO/DISP_LEVEL_IMP/DISP_LEVEL_ERR, should be the system defined display log level
% Return :
%       ok
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
log_info(Data, Log, LogLevel) when is_binary(Data),
                                  Log >= ?NO,
                                  Log =< ?YES,
                                  LogLevel >= ?DISP_LEVEL_ALL,
                                  LogLevel =< ?DISP_LEVEL_ERR ->
    do_log(Data, ?DISP_LEVEL_INFO, Log, LogLevel);
log_info(Data, Log, LogLevel) when is_list(Data),
                                  Log >= ?NO,
                                  Log =< ?YES,
                                  LogLevel >= ?DISP_LEVEL_ALL,
                                  LogLevel =< ?DISP_LEVEL_ERR ->
    do_log(Data, ?DISP_LEVEL_INFO, Log, LogLevel);
log_info(_Data, _Log, _LogLevel) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description:
%   Log important messages, such as some operation related messages
% Parameter :
%       Data        : binary/list to be displayed
% Return :
%       ok
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
log_hint(Data) when is_binary(Data) ->
    do_log(Data, ?DISP_LEVEL_IMP);
log_hint(Data) when is_list(Data) ->
    do_log(Data, ?DISP_LEVEL_IMP);
log_hint(_Data) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description:
%   Log important messages, such as some operation related messages
% Parameter :
%       Data        : binary/list to be displayed
%       Log         : YES/NO, should be the system defined display log enabled
%       LogLevel    : DISP_LEVEL_ALL/DISP_LEVEL_INFO/DISP_LEVEL_IMP/DISP_LEVEL_ERR, should be the system defined display log level
% Return :
%       ok
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
log_hint(Data, Log, LogLevel) when is_binary(Data),
                                  Log >= ?NO,
                                  Log =< ?YES,
                                  LogLevel >= ?DISP_LEVEL_ALL,
                                  LogLevel =< ?DISP_LEVEL_ERR ->
    do_log(Data, ?DISP_LEVEL_IMP, Log, LogLevel);
log_hint(Data, Log, LogLevel) when is_list(Data),
                                  Log >= ?NO,
                                  Log =< ?YES,
                                  LogLevel >= ?DISP_LEVEL_ALL,
                                  LogLevel =< ?DISP_LEVEL_ERR ->
    do_log(Data, ?DISP_LEVEL_IMP, Log, LogLevel);
log_hint(_Data, _Log, _LogLevel) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description:
%   Log errors.
% Parameter :
%       Data        : binary/list to be displayed
% Return :
%       ok
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
log_err(Data) when is_binary(Data) ->
    do_log(Data, ?DISP_LEVEL_ERR);
log_err(Data) when is_list(Data) ->
    do_log(Data, ?DISP_LEVEL_ERR);
log_err(_Data) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description:
%   Log errors.
% Parameter :
%       Data        : binary/list to be displayed
%       Log         : YES/NO, should be the system defined display log enabled
%       LogLevel    : DISP_LEVEL_ALL/DISP_LEVEL_INFO/DISP_LEVEL_IMP/DISP_LEVEL_ERR, should be the system defined display log level
% Return :
%       ok
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
log_err(Data, Log, LogLevel) when is_binary(Data),
                                  Log >= ?NO,
                                  Log =< ?YES,
                                  LogLevel >= ?DISP_LEVEL_ALL,
                                  LogLevel =< ?DISP_LEVEL_ERR ->
    do_log(Data, ?DISP_LEVEL_ERR, Log, LogLevel);
log_err(Data, Log, LogLevel) when is_list(Data),
                                  Log >= ?NO,
                                  Log =< ?YES,
                                  LogLevel >= ?DISP_LEVEL_ALL,
                                  LogLevel =< ?DISP_LEVEL_ERR ->
    do_log(Data, ?DISP_LEVEL_ERR, Log, LogLevel);
log_err(_Data, _Log, _LogLevel) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
%    When the system defined display log is false, the message won't be displayed.
% Parameter :
%       Data        :
%       Level       : when current level is smaller than the system defined display log level, the message won't be displayed
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
do_log(Data, Level) when is_binary(Data),
                         Level =< ?DISP_LEVEL_ERR,
                         Level >= ?DISP_LEVEL_ALL ->
    try
        [{displog, Log}] = ets:lookup(msgservertable, displog),
        [{displevel, LogLevel}] = ets:lookup(msgservertable, displevel),
        
        do_log(Data, Level, Log, LogLevel)
    catch
        Oper:Msg ->
            error_logger:error_msg("do_log(Data, Level) exception : ~p : ~p", [Oper, Msg])
    end;
do_log(_Data, _Level) ->
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Parameter :
%       Data        :
%       Level       : when current level is smaller than the system defined display log level, the message won't be displayed
%       Log         : should be the system defined display log
%       LogLevel    : should be the system defined dislay log level
%
% When Log is false, the message won't be displayed.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
do_log(Data, Level, Log, LogLevel) when is_binary(Data),
                                        Level =< ?DISP_LEVEL_ERR,
                                        Level >= ?DISP_LEVEL_ALL,
                                        Log =< 1,
                                        Log >= 0,
                                        LogLevel =< ?DISP_LEVEL_ERR,
                                        LogLevel >= ?DISP_LEVEL_ALL ->
    try
        if
            LogLevel =< Level ->
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
                        error_logger:error_msg("do_log(...) exception : ~p : ~p", [Oper, Msg])
                end
        end
    catch
        Oper1:Msg1 ->
            error_logger:error_msg("do_log(...) exception : ~p : ~p", [Oper1, Msg1])
    end;
do_log(Format, CurLevel, DispErr) when is_list(Format),
                                       CurLevel =< ?DISP_LEVEL_ERR,
                                       CurLevel >= ?DISP_LEVEL_ALL,
                                       DispErr =< 1,
                                       DispErr >= 0 ->
    try
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
                end;
            true ->
                ok
        end
    catch
        Oper1:Msg1 ->
            error_logger:error_msg("do_log(...) exception : ~p : ~p", [Oper1, Msg1])
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
do_log(Format, Data, CurLevel, DispErr) when is_string(Format),
                                             is_binary(Data),
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
        State#vdritem.conninfopid =/= undefined ->
            State#vdritem.conninfopid ! {self(), Type},
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
