%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% log.erl
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(log).

-include("../include/header_const.hrl").
-include("../include/header_struct.hrl").

-export([log_process/9,
         log_all/1,
         log_info/1,
         log_warn/1,
         log_err/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description:
%   Log process will receive all log request and log the messages.
% Parameter :
%       Log         : YES/NO, should be the system defined display log enabled
%       LogLevel    : DISP_LEVEL_ALL/DISP_LEVEL_INFO/DISP_LEVEL_WARN/DISP_LEVEL_ERR, should be the system defined display log level
% Return :
%       ok
% Note  :
%   UnknwonCount    : unknown display level
%   MissedCount     : when Log is false
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
log_process(Level, Log, LogLevel, AllCount, InfoCount, WarnCount, ErrCount, UnknwonCount, MissedCount) ->
    receive
        {Format} ->
            if
                Log =:= ?YES ->
                    case Level of
                        ?DISP_LEVEL_ALL ->
                            log_all(Format, Log, LogLevel),
                            log_process(Level, Log, LogLevel, AllCount+1, InfoCount, WarnCount, ErrCount, UnknwonCount, MissedCount);
                        ?DISP_LEVEL_INFO ->
                            log_info(Format, Log, LogLevel);
                            log_process(Level, Log, LogLevel, AllCount, InfoCount+1, WarnCount, ErrCount, UnknwonCount, MissedCount);
                        ?DISP_LEVEL_INFO ->
                            log_warn(Format, Log, LogLevel);
                            log_process(Level, Log, LogLevel, AllCount, InfoCount, WarnCount+1, ErrCount, UnknwonCount, MissedCount);
                        ?DISP_LEVEL_INFO ->
                            log_err(Format, Log, LogLevel);
                            log_process(Level, Log, LogLevel, AllCount, InfoCount, WarnCount, ErrCount+1, UnknwonCount, MissedCount);
                        _ ->
                            log_process(Level, Log, LogLevel, AllCount, InfoCount, WarnCount, ErrCount, UnknwonCount+1, MissedCount)
                    end;
                true ->
                    log_process(Level, Log, LogLevel, AllCount, InfoCount, WarnCount, ErrCount, UnknwonCount, MissedCount+1)
           end,
        {Format, Data} ->
            if
                Log =:= ?YES ->
                    case Level of
                        ?DISP_LEVEL_ALL ->
                            log_all(Format, Data, Log, LogLevel);
                            log_process(Level, Log, LogLevel, AllCount+1, InfoCount, WarnCount, ErrCount, UnknwonCount, MissedCount);
                        ?DISP_LEVEL_INFO ->
                            log_info(Format, Data, Log, LogLevel);
                            log_process(Level, Log, LogLevel, AllCount, InfoCount+1, WarnCount, ErrCount, UnknwonCount, MissedCount);
                        ?DISP_LEVEL_INFO ->
                            log_warn(Format, Data, Log, LogLevel);
                            log_process(Level, Log, LogLevel, AllCount, InfoCount, WarnCount+1, ErrCount, UnknwonCount, MissedCount);
                        ?DISP_LEVEL_INFO ->
                            log_err(Format, Data, Log, LogLevel);
                            log_process(Level, Log, LogLevel, AllCount, InfoCount, WarnCount, ErrCount+1, UnknwonCount, MissedCount);
                        _ ->
                            log_process(Level, Log, LogLevel, AllCount, InfoCount, WarnCount, ErrCount, UnknwonCount+1, MissedCount)
                    end;
                true ->
                    log_process(Level, Log, LogLevel, AllCount, InfoCount, WarnCount, ErrCount, UnknwonCount, MissedCount+1)
            end;
        {log, Value} ->
            log_process(Value, Log, LogLevel, AllCount, InfoCount, WarnCount, ErrCount, UnknwonCount, MissedCount);
        {displog, Value} ->
            log_process(Level, Value, LogLevel, AllCount, InfoCount, WarnCount, ErrCount, UnknwonCount, MissedCount);
        {displevel, Value} ->
            log_process(Level, Log, Value, AllCount, InfoCount, WarnCount, ErrCount, UnknwonCount, MissedCount);
        reset ->
            log_process(?DISP_LEVEL_ERR, ?NO, ?DISP_LEVEL_ERR, 0, 0, 0, 0, 0, 0);
        {Pid, query} ->
            Pid ! {Level, Log, LogLevel, AllCount, InfoCount, WarnCount, ErrCount, UnknwonCount, MissedCount},
            log_process(Level, Log, LogLevel, AllCount, InfoCount, WarnCount, ErrCount, UnknwonCount, MissedCount);
        stop ->
            ok
    end.

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
            error_logger:error_msg("do_log(Format - binary, Level, Log, LogLevel) exception : ~p : ~p", [Oper, Msg])
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
                    error_logger:error_msg("do_log(Format - list, Level, Log, LogLevel) exception : ~p : ~p", [Oper, Msg])
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
        string_helper:is_string(Format) =:= ?YES ->
            do_log(Format, Data, ?DISP_LEVEL_ALL);
        true ->
            error_logger:error_msg("log_all(Format - string, Data - binary) parameter error : " ++ erlang:get_stacktrace())
    end;
log_all(Format, Data) when is_list(Data) ->
    if
        string_helper:is_string(Format) =:= ?YES ->
            do_log(Format, Data, ?DISP_LEVEL_ALL);
        true ->
            error_logger:error_msg("log_all(Format - string, Data - binary) parameter error : " ++ erlang:get_stacktrace())
    end;
log_all(_Format, _Data) ->
    error_logger:error_msg("do_log(Format, Data) parameter error : " ++ erlang:get_stacktrace()).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description:
%   Log all messages.
% Parameter :
%       Format + Data   : binary/list to be displayed
%       Log         : boolean true/false, should be the system defined display log enabled
%       LogLevel    : DISP_LEVEL_ALL/DISP_LEVEL_INFO/DISP_LEVEL_WARN/DISP_LEVEL_ERR, should be the system defined display log level
% Return :
%       ok
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
log_all(Format, Data, Log, LogLevel) when is_binary(Data),
                                          Log >= ?NO,
                                          Log =< ?YES,
                                          LogLevel >= ?DISP_LEVEL_ALL,
                                          LogLevel =< ?DISP_LEVEL_ERR ->
    if
        string_helper:is_string(Format) =:= ?YES ->
            do_log(Format, Data, ?DISP_LEVEL_ALL, Log, LogLevel);
        true ->
            error_logger:error_msg("log_all(Format - string, Data - binary, Log, LogLevel) parameter error : " ++ erlang:get_stacktrace())
    end;
log_all(Format, Data, Log, LogLevel) when is_list(Data),
                                          Log >= ?NO,
                                          Log =< ?YES,
                                          LogLevel >= ?DISP_LEVEL_ALL,
                                          LogLevel =< ?DISP_LEVEL_ERR ->
    do_log(Format, Data, ?DISP_LEVEL_ALL, Log, LogLevel);
log_all(_Format, _Data, _Log, _LogLevel) ->
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
%       Data        : binary/list to be displayed
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

