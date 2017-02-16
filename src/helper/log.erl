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
%       LogState    : struct logstate
% Return :
%       ok
% Note  :
%   unknowncount    : unknown display level
%   missedcount     : when Log is false
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
log_process(LogState#logstate) ->
    receive
        {info, Format} ->
            if
                LogState#logstate.logenabled =:= ?YES and LogState#logstate.loglevel >= ?DISP_LEVEL_INFO  ->
                    log_info(Format),
                    log_process(LogState#logstate{infocount=LogState#logstate.infocount+1});
                true ->
                    log_process(LogState#logstate{missedcount=LogState#logstate.missedcount+1})
            end;
        {info, Format, Data} ->
            if
                LogState#logstate.logenabled =:= ?YES and LogState#logstate.loglevel >= ?DISP_LEVEL_INFO  ->
                    log_info(Format, Data),
                    log_process(LogState#logstate{infocount=LogState#logstate.infocount+1});
                true ->
                    log_process(LogState#logstate{missedcount=LogState#logstate.missedcount+1})
            end;
        {warn, Format} ->
            if
                LogState#logstate.logenabled =:= ?YES and LogState#logstate.loglevel >= ?DISP_LEVEL_WARN  ->
                    log_warn(Format),
                    log_process(LogState#logstate{warncount=LogState#logstate.warncount+1});
                true ->
                    log_process(LogState#logstate{missedcount=LogState#logstate.missedcount+1})
            end;
        {warn, Format, Data} ->
            if
                LogState#logstate.logenabled =:= ?YES and LogState#logstate.loglevel >= ?DISP_LEVEL_WARN  ->
                    log_warn(Format, Data),
                    log_process(LogState#logstate{warncount=LogState#logstate.warncount+1});
                true ->
                    log_process(LogState#logstate{missedcount=LogState#logstate.missedcount+1})
            end;
        {err, Format} ->
            if
                LogState#logstate.logenabled =:= ?YES and LogState#logstate.loglevel >= ?DISP_LEVEL_ERR  ->
                    log_err(Format),
                    log_process(LogState#logstate{errcount=LogState#logstate.errcount+1});
                true ->
                    log_process(LogState#logstate{missedcount=LogState#logstate.missedcount+1})
            end;
        {err, Format, Data} ->
            if
                LogState#logstate.logenabled =:= ?YES and LogState#logstate.loglevel >= ?DISP_LEVEL_ERR  ->
                    log_err(Format, Data),
                    log_process(LogState#logstate{errcount=LogState#logstate.errcount+1});
                true ->
                    log_process(LogState#logstate{missedcount=LogState#logstate.missedcount+1})
            end;
        {log, Value} ->
            log_process(LogState#logstate{curlevel=Value});
        {displog, Value} ->
            log_process(LogState#logstate{logenabled=Value});
        {displevel, Value} ->
            log_process(LogState#logstate{loglevel=Value});
        reset ->
            log_process(NewLogState#logstate);
        {Pid, query} ->
            Pid ! LogState#logstate,
            log_process(LogStatet#logstate);
        stop ->
            ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description:
%   Log information messages.
% Parameter :
%       LogPid      :
%       Format      : binary/list to be displayed
% Return :
%       ok
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
log_info(LogPid, Format) ->
    LogPid ! {info, Format}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description:
%   Log warning messages, such as some operation related messages
% Parameter :
%       LogPid      :
%       Format      : binary/list to be displayed
% Return :
%       ok
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
log_warn(LogPid, Format) ->
    LogPid ! {Format}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description:
%   Log errors.
% Parameter :
%       LogPid      :
%       Format      : binary/list to be displayed
% Return :
%       ok
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
log_err(LogPid, Format) ->
    LogPid ! {Format}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description:
%   Log information messages.
% Parameter :
%       LogPid      :
%       Format + Data   : binary/list to be displayed
% Return :
%       ok
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
log_info(LogPid, Format, Data) ->
    LogPid ! {Format, Data}.

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
log_warn(LogPid, Format, Data) when is_binary(Data) ->
    LogPid ! {Format, Data}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description:
%   Log information messages.
% Parameter :
%       Format      : binary/list to be displayed
%       LogState#logstate   :
% Return :
%       LogState#logstate
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
log_info(Format, LogState#logstate) when is_binary(Format) ->
    do_log(Format, ?DISP_LEVEL_INFO, LogState#logstate);
log_info(Format, LogState#logstate) when is_list(Format) ->
    do_log(Format, ?DISP_LEVEL_INFO, LogState#logstate);
log_info(_Format, LogState#logstate) ->
    LogState#logstate{unknowncount=LogState#logstate.unknowncount+1}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description:
%   Log information messages.
% Parameter :
%       Format      : binary/list to be displayed
%       LogState#logstate   :
% Return :
%       LogState#logstate
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
log_info(Format, Data, LogState#logstate) when is_binary(Format) ->
    do_log(Format, Data, ?DISP_LEVEL_INFO, LogState#logstate);
log_info(Format, Data, LogState#logstate) when is_list(Format) ->
    do_log(Format, Data, ?DISP_LEVEL_INFO, LogState#logstate);
log_info(_Format, Data, LogState#logstate) ->
    LogState#logstate{unknowncount=LogState#logstate.unknowncount+1}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description:
%   Log warning messages, such as some operation related messages
% Parameter :
%       Format      : binary/list to be displayed
%       LogState#logstate   :
% Return :
%       LogState#logstate
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
log_warn(Format, LogState#logstate) when is_binary(Format) ->
    do_log(Format, LogState#logstate);
log_warn(Format, LogState#logstate) when is_list(Format) ->
    do_log(Format, LogState#logstate);
log_warn(_Format, LogState#logstate) ->
    LogState#logstate{unknowncount=LogState#logstate.unknowncount+1}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description:
%   Log errors.
% Parameter :
%       Format      : binary/list to be displayed
%       LogState#logstate   :
% Return :
%       LogState#logstate
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
log_err(Format, LogState#logstate) when is_binary(Format) ->
    do_log(Format, LogState#logstate);
log_err(Format, LogState#logstate) when is_list(Format) ->
    do_log(Format, LogState#logstate);
log_err(_Format, LogState#logstate) ->
    LogState#logstate{unknowncount=LogState#logstate.unknowncount+1}.

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
do_log(Format, Level, LogState#logstate) when is_binary(Format),
                                              Level >= ?DISP_LEVEL_INFO,
                                              Level =< ?DISP_LEVEL_ERR ->
    try
        do_log(binary_to_list(Format), Level),
        LogState#logstate
    catch
        Oper:Msg ->
            error_logger:error_msg("do_log(Format - binary, LogState#logstate) exception : ~p : ~p", [Oper, Msg]),
            LogState#logstate{expcount=LogState#logstate.expcount+1}
    end;
do_log(Format, Level, LogState#logstate) when is_list(Format),
                                              Level >= ?DISP_LEVEL_INFO,
                                              Level =< ?DISP_LEVEL_ERR ->
    try
        case Level of
            ?DISP_LOG_INFO ->
                error_logger:info_msg(Format);
            ?DISP_LEVEL_WARN ->
                error_logger:warning_msg(Format);
            ?DISP_LEVEL_ERR ->
                error_logger:error_msg(Format)
        end,
        LogState#logstate
    catch
        Oper:Msg ->
            error_logger:error_msg("do_log(Format - list, LogState#logstate) exception : ~p : ~p", [Oper, Msg]),
            LogState#logstate{expcount=LogState#logstate.expcount+1}
    end
do_log(_Format, _Level) ->
    LogState#logstate{unknowncount=LogState#logstate.unknowncount+1}.
