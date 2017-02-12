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
         log_warn/1,
         log_warn/3,
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
%       Format      : binary/list to be displayed
% Return :
%       ok
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
log_all(Format) when is_binary(Format) ->
    do_log(Format, ?DISP_LEVEL_ALL);
log_all(Format) when is_list(Format) ->
    do_log(Format, ?DISP_LEVEL_ALL);
log_all(_Format) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description:
%   Log all messages.
% Parameter :
%       Format      : binary/list to be displayed
%       Log         : YES/NO, should be the system defined display log enabled
%       LogLevel    : DISP_LEVEL_ALL/DISP_LEVEL_INFO/DISP_LEVEL_WARN/DISP_LEVEL_ERR, should be the system defined display log level
% Return :
%       ok
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
log_all(Format, Log, LogLevel) when is_binary(Format),
                                    is_boolean(Log),
                                    LogLevel >= ?DISP_LEVEL_ALL,
                                    LogLevel =< ?DISP_LEVEL_ERR ->
    do_log(Format, ?DISP_LEVEL_ALL, Log, LogLevel);
log_all(Format, Log, LogLevel) when is_list(Format),
                                    is_boolean(Log),
                                    LogLevel >= ?DISP_LEVEL_ALL,
                                    LogLevel =< ?DISP_LEVEL_ERR ->
    do_log(Format, ?DISP_LEVEL_ALL, Log, LogLevel);
log_all(_Format, _Log, _LogLevel) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description:
%   Log information messages.
% Parameter :
%       Format      : binary/list to be displayed
% Return :
%       ok
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
log_info(Format) when is_binary(Format) ->
    do_log(Format, ?DISP_LEVEL_INFO);
log_info(Format) when is_list(Format) ->
    do_log(Format, ?DISP_LEVEL_INFO);
log_info(_Format) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description:
%   Log information messages.
% Parameter :
%       Format      : binary/list to be displayed
%       Log         : boolean true/false, should be the system defined display log enabled
%       LogLevel    : DISP_LEVEL_ALL/DISP_LEVEL_INFO/DISP_LEVEL_WARN/DISP_LEVEL_ERR, should be the system defined display log level
% Return :
%       ok
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
log_info(Format, Log, LogLevel) when is_binary(Format),
                                     is_boolean(Log),
                                     LogLevel >= ?DISP_LEVEL_ALL,
                                     LogLevel =< ?DISP_LEVEL_ERR ->
    do_log(Format, ?DISP_LEVEL_INFO, Log, LogLevel);
log_info(Format, Log, LogLevel) when is_list(Format),
                                     is_boolean(Log),
                                     LogLevel >= ?DISP_LEVEL_ALL,
                                     LogLevel =< ?DISP_LEVEL_ERR ->
    do_log(Format, ?DISP_LEVEL_INFO, Log, LogLevel);
log_info(_Format, _Log, _LogLevel) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description:
%   Log warning messages, such as some operation related messages
% Parameter :
%       Format      : binary/list to be displayed
% Return :
%       ok
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
log_warn(Format) when is_binaryFormat ->
    do_log(Format, ?DISP_LEVEL_WARN);
log_warn(Format) when is_list(Format) ->
    do_log(Format, ?DISP_LEVEL_WARN);
log_warn(_Format) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description:
%   Log warning messages, such as some operation related messages
% Parameter :
%       Format      : binary/list to be displayed
%       Log         : boolean true/false, should be the system defined display log enabled
%       LogLevel    : DISP_LEVEL_ALL/DISP_LEVEL_INFO/DISP_LEVEL_WARN/DISP_LEVEL_ERR, should be the system defined display log level
% Return :
%       ok
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
log_warn(Format, Log, LogLevel) when is_binary(Format),
                                     is_boolean(Log),
                                     LogLevel >= ?DISP_LEVEL_ALL,
                                     LogLevel =< ?DISP_LEVEL_ERR ->
    do_log(Format, ?DISP_LEVEL_WARN, Log, LogLevel);
log_warn(Format, Log, LogLevel) when is_list(Format),
                                     is_boolean(Log),
                                     LogLevel >= ?DISP_LEVEL_ALL,
                                     LogLevel =< ?DISP_LEVEL_ERR ->
    do_log(Format, ?DISP_LEVEL_WARN, Log, LogLevel);
log_warn(_Format, _Log, _LogLevel) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description:
%   Log errors.
% Parameter :
%       Format      : binary/list to be displayed
% Return :
%       ok
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
log_err(Format) when is_binary(Format) ->
    do_log(Format, ?DISP_LEVEL_ERR);
log_err(Format) when is_list(Format) ->
    do_log(Format, ?DISP_LEVEL_ERR);
log_err(_Format) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description:
%   Log errors.
% Parameter :
%       Format      : binary/list to be displayed
%       Log         : boolean true/false, should be the system defined display log enabled
%       LogLevel    : DISP_LEVEL_ALL/DISP_LEVEL_INFO/DISP_LEVEL_WARN/DISP_LEVEL_ERR, should be the system defined display log level
% Return :
%       ok
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
log_err(Format, Log, LogLevel) when is_binary(Format),
                                    is_boolean(Log),
                                    LogLevel >= ?DISP_LEVEL_ALL,
                                    LogLevel =< ?DISP_LEVEL_ERR ->
    do_log(Format, ?DISP_LEVEL_ERR, Log, LogLevel);
log_err(Format, Log, LogLevel) when is_list(Format),
                                    is_boolean(Log),
                                    LogLevel >= ?DISP_LEVEL_ALL,
                                    LogLevel =< ?DISP_LEVEL_ERR ->
    do_log(Format, ?DISP_LEVEL_ERR, Log, LogLevel);
log_err(_Format, _Log, _LogLevel) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
%    Display the messages.
%    When the system defined display log enabled is NO, the message won't be displayed.
%    When Level is smaller than the system defined display log level, the message won't be displayed
% Parameter :
%       Format      : binary/list to be displayed
%       Level       : DISP_LEVEL_ALL/DISP_LEVEL_INFO/DISP_LEVEL_IMP/DISP_LEVEL_ERR
% Return :
%       ok
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
do_log(Format, Level) when is_binary(Format),
                         Level =< ?DISP_LEVEL_ERR,
                         Level >= ?DISP_LEVEL_ALL ->
    try
        do_log(binary_to_list(Format), Level, Log, LogLevel);
    catch
        Oper:Msg ->
            error_logger:error_msg("do_log(Format - binary, Level) exception : ~p : ~p", [Oper, Msg])
    end;
do_log(Format, Level) when is_list(Format),
                         Level =< ?DISP_LEVEL_ERR,
                         Level >= ?DISP_LEVEL_ALL ->
    try
        [{displog, Log}] = ets:lookup(msgservertable, displog),
        [{displevel, LogLevel}] = ets:lookup(msgservertable, displevel),
        
        do_log(Format, Level, Log, LogLevel)
    catch
        Oper:Msg ->
            error_logger:error_msg("do_log(Format - list, Level) exception : ~p : ~p", [Oper, Msg])
    end;
do_log(_Format, _Level) ->
    error_logger:error_msg("do_log(Format, Level) parameter error : " ++ erlang:get_stacktrace()).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
%    Display the messages.
%    When the system defined display log enabled is NO, the message won't be displayed.
%    When Level is smaller than the system defined display log level, the message won't be displayed
% Parameter :
%       Format      : binary/list to be displayed
%       Level       : DISP_LEVEL_ALL/DISP_LEVEL_INFO/DISP_LEVEL_IMP/DISP_LEVEL_ERR
%       Log         : boolean true/false, should be the system defined display log
%       LogLevel    : DISP_LEVEL_ALL/DISP_LEVEL_INFO/DISP_LEVEL_IMP/DISP_LEVEL_ERR, should be the system defined dislay log level
% Return :
%       ok
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
do_log(Format, Level, Log, LogLevel) when is_binary(Format),
                                          Level =< ?DISP_LEVEL_ERR,
                                          Level >= ?DISP_LEVEL_ALL,
                                          is_boolean(Log),
                                          LogLevel =< ?DISP_LEVEL_ERR,
                                          LogLevel >= ?DISP_LEVEL_ALL ->
    try
        do_log(binary_to_list(Format), Level, Log, LogLevel)
    catch
        Oper:Msg ->
            error_logger:error_msg("do_log(Data, Level, Log, LogLevel) exception : ~p : ~p", [Oper, Msg])
    end;
do_log(Format, Level, Log, LogLevel) when is_list(Format),
                                          Level =< ?DISP_LEVEL_ERR,
                                          Level >= ?DISP_LEVEL_ALL,
                                          is_boolean(Log),
                                          LogLevel =< ?DISP_LEVEL_ERR,
                                          LogLevel >= ?DISP_LEVEL_ALL ->
    if
        LogLevel =< Level and Log =:= true ->
            try
                case Level of
                    ?DISP_LOG_ALL ->
                        error_logger:info_msg(Format);
                    ?DISP_LOG_INFO ->
                        error_logger:info_msg(Format);
                    ?DISP_LEVEL_WARN ->
                        error_logger:warning_msg(Format);
                    ?DISP_LEVEL_ERR ->
                        error_logger:error_msg(Format);
                    _ ->
                        ok
                end
            catch
                Oper:Msg ->
                    error_logger:error_msg("do_log(Data, Level, Log, LogLevel) exception : ~p : ~p", [Oper, Msg])
            end
    end;
do_log(_Format, _Level, _Log, _LogLevel) ->
    error_logger:error_msg("do_log(Format, Level) parameter error : " ++ erlang:get_stacktrace()).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description:
%   Log all messages.
% Parameter :
%       Format + Data   : binary/list to be displayed
% Return :
%       ok
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
log_all(Format, Data) when is_binary(Data) ->
    if
        data_helper:is_string(Format) =:= ?YES
    do_log(Data, ?DISP_LEVEL_ALL);
log_all(Format, Data) when is_list(Data) ->
    do_log(Format, Data, ?DISP_LEVEL_ALL);
log_all(_Format, _Data) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description:
%   Log all messages.
% Parameter :
%       Data        : binary/list to be displayed
%       Log         : boolean true/false, should be the system defined display log enabled
%       LogLevel    : DISP_LEVEL_ALL/DISP_LEVEL_INFO/DISP_LEVEL_WARN/DISP_LEVEL_ERR, should be the system defined display log level
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
%       LogLevel    : DISP_LEVEL_ALL/DISP_LEVEL_INFO/DISP_LEVEL_WARN/DISP_LEVEL_ERR, should be the system defined display log level
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
%   Log warning messages, such as some operation related messages
% Parameter :
%       Data        : binary/list to be displayed
% Return :
%       ok
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
log_warn(Data) when is_binary(Data) ->
    do_log(Data, ?DISP_LEVEL_WARN);
log_warn(Data) when is_list(Data) ->
    do_log(Data, ?DISP_LEVEL_WARN);
log_warn(_Data) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description:
%   Log warning messages, such as some operation related messages
% Parameter :
%       Data        : binary/list to be displayed
%       Log         : YES/NO, should be the system defined display log enabled
%       LogLevel    : DISP_LEVEL_ALL/DISP_LEVEL_INFO/DISP_LEVEL_WARN/DISP_LEVEL_ERR, should be the system defined display log level
% Return :
%       ok
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
log_warn(Data, Log, LogLevel) when is_binary(Data),
                                   Log >= ?NO,
                                   Log =< ?YES,
                                   LogLevel >= ?DISP_LEVEL_ALL,
                                   LogLevel =< ?DISP_LEVEL_ERR ->
    do_log(Data, ?DISP_LEVEL_WARN, Log, LogLevel);
log_warn(Data, Log, LogLevel) when is_list(Data),
                                   Log >= ?NO,
                                   Log =< ?YES,
                                   LogLevel >= ?DISP_LEVEL_ALL,
                                   LogLevel =< ?DISP_LEVEL_ERR ->
    do_log(Data, ?DISP_LEVEL_WARN, Log, LogLevel);
log_warn(_Data, _Log, _LogLevel) ->
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
%       LogLevel    : DISP_LEVEL_ALL/DISP_LEVEL_INFO/DISP_LEVEL_WARN/DISP_LEVEL_ERR, should be the system defined display log level
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
