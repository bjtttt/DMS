%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% log.erl
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(log).

-include("../include/header_const.hrl").
-include("../include/header_struct.hrl").

-export([log_process_dummy/1,
         log_info/2,
         log_info/3,
         log_force_info/2,
         log_force_info/3,
         log_warn/2,
         log_warn/3,
         log_err/2,
         log_err/3,
         log_force_err/2,
         log_force_err/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Property:
%   private
% Description:
%   Log process will receive all log request and log the messages.
%   When receiving pause, will turn to log_process_dummy
% Parameter:
%   LogState#logstate   : struct logstate
% Return:
%   ok
% Note:
%   unknowncount    : unknown display level
%   missedcount     : when Log is false
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
log_process(LogState#logstate) ->
    receive
        {info, Format} ->
            if
                LogState#logstate.logenabled =:= ?YES and LogState#logstate.loglevel >= ?DISP_LEVEL_INFO  ->
                    do_log_info(Format);
                true ->
                    log_process(LogState#logstate{missedcount=LogState#logstate.missedcount+1})
            end;
        {info, Format, Data} ->
            if
                LogState#logstate.logenabled =:= ?YES and LogState#logstate.loglevel >= ?DISP_LEVEL_INFO  ->
                    do_log_info(Format, Data);
                true ->
                    log_process(LogState#logstate{missedcount=LogState#logstate.missedcount+1})
            end;
        {warn, Format} ->
            if
                LogState#logstate.logenabled =:= ?YES and LogState#logstate.loglevel >= ?DISP_LEVEL_WARN  ->
                    do_log_warn(Format);
                true ->
                    log_process(LogState#logstate{missedcount=LogState#logstate.missedcount+1})
            end;
        {warn, Format, Data} ->
            if
                LogState#logstate.logenabled =:= ?YES and LogState#logstate.loglevel >= ?DISP_LEVEL_WARN  ->
                    do_log_warn(Format, Data);
                true ->
                    log_process(LogState#logstate{missedcount=LogState#logstate.missedcount+1})
            end;
        {err, Format} ->
            if
                LogState#logstate.logenabled =:= ?YES and LogState#logstate.loglevel >= ?DISP_LEVEL_ERR  ->
                    do_log_err(Format);
                true ->
                    log_process(LogState#logstate{missedcount=LogState#logstate.missedcount+1})
            end;
        {err, Format, Data} ->
            if
                LogState#logstate.logenabled =:= ?YES and LogState#logstate.loglevel >= ?DISP_LEVEL_ERR  ->
                    do_log_err(Format, Data);
                true ->
                    log_process(LogState#logstate{missedcount=LogState#logstate.missedcount+1})
            end;
        {forceinfo, Format} ->
            do_log_info(Format);
        {forceinfo, Format, Data} ->
            do_log_info(Format, Data);
        {forceerr, Format} ->
            do_log_err(Format);
        {forceerr, Format, Data} ->
            do_log_err(Format, Data);
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
        pause ->
            log_process_dummy(LogState#logstate);
        stop ->
            ok;
        Unknown ->
            do_log_err("Log unknown log message : " ++ Unknown),
            log_process_dummy(LogState#logstate{unknowncount=LogState#logstate.unknowncount+1})
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Property:
%   public
% Description:
%   Log process dummy will receive all log request but doesn't do any log operation.
%   Each log request will be taken as a dummy log request and increase dummycount by 1.
%   When receiving start, will turn to log_process
% Parameter:
%   LogState#logstate   : struct logstate
% Return:
%   ok
% Note:
%   dummycount  : dummy log request count
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
log_process_dummy(LogState#logstate) ->
    receive
        start ->
            log_process(LogState#logstate);
        stop ->
            ok;
        _ ->
            log_process_dummy(LogState#logstate{dummycount=LogState#logstate.dummycount+1})
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Property:
%   public
% Description:
%   Log information messages.
% Parameter:
%       LogPid      :
%       Format      : binary/list to be displayed
% Return:
%       ok
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
log_info(LogPid, Format) ->
    LogPid ! {info, Format}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Property:
%   public
% Description:
%   Log information messages.
% Parameter :
%   LogPid          :
%   Format + Data   : binary/list to be displayed
% Return :
%   ok
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
log_info(LogPid, Format, Data) ->
    LogPid ! {info, Format, Data}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Property:
%   public
% Description:
%   Log information messages regardless of whether Log is enabled or not.
% Parameter:
%   LogPid      :
%   Format      : binary/list to be displayed
% Return:
%   ok
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
log_force_info(LogPid, Format) ->
    LogPid ! {forceinfo, Format}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Property:
%   public
% Description:
%   Log information messages regardless of whether Log is enabled or not.
% Parameter:
%   LogPid          :
%   Format + Data   : binary/list to be displayed
% Return:
%   ok
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
log_force_info(LogPid, Format, Data) ->
    LogPid ! {forceinfo, Format, Data}.

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
    LogPid ! {warn, Format}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Property:
%   public
% Description:
%   Log warning messages, such as some operation related messages
% Parameter :
%       LogPid          :
%       Format + Data   : binary/list to be displayed
% Return :
%       ok
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
log_warn(LogPid, Format, Data) ->
    LogPid ! {warn, Format, Data}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description:
%   Log errors regardless of whether Log is enabled or not.
% Parameter :
%       LogPid      :
%       Format      : binary/list to be displayed
% Return :
%       ok
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
log_err(LogPid, Format) ->
    LogPid ! {err, Format}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Property:
%   public
% Description:
%   Log warning messages, such as some operation related messages
% Parameter :
%       LogPid          :
%       Format + Data   : binary/list to be displayed
% Return :
%       ok
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
log_err(LogPid, Format, Data) ->
    LogPid ! {err, Format, Data}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description:
%   Log errors regardless of whether Log is enabled or not.
% Parameter :
%       LogPid      :
%       Format      : binary/list to be displayed
% Return :
%       ok
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
log_force_err(LogPid, Format) ->
    LogPid ! {forceerr, Format}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Property:
%   public
% Description:
%   Log warning messages, such as some operation related messages
% Parameter :
%       LogPid          :
%       Format + Data   : binary/list to be displayed
% Return :
%       ok
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
log_force_err(LogPid, Format, Data) ->
    LogPid ! {forceerr, Format, Data}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Property:
%   private
% Description:
%   Log information messages.
% Parameter:
%   Format              : binary/list to be displayed
%   LogState#logstate   :
% Return:
%   LogState#logstate
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
do_log_info(Format, LogState#logstate) when is_binary(Format) ->
    do_log(Format, ?DISP_LEVEL_INFO, LogState#logstate);
do_log_info(Format, LogState#logstate) when is_list(Format) ->
    do_log(Format, ?DISP_LEVEL_INFO, LogState#logstate);
do_log_info(_Format, LogState#logstate) ->
    LogState#logstate{formatcount=LogState#logstate.formatcount+1}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Property:
%   private
% Description:
%   Log information messages.
% Parameter:
%   Format + Data       : binary/list to be displayed
%   LogState#logstate   :
% Return:
%   LogState#logstate
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
do_log_info(Format, Data, LogState#logstate) when is_binary(Format),
                                                  is_binary(Data) ->
    do_log(Format, Data, ?DISP_LEVEL_INFO, LogState#logstate);
do_log_info(Format, Data, LogState#logstate) when is_binary(Format),
                                                  is_list(Data) ->
    do_log(Format, Data, ?DISP_LEVEL_INFO, LogState#logstate);
do_log_info(Format, Data, LogState#logstate) when is_list(Format),
                                                  is_binary(Data) ->
    do_log(Format, Data, ?DISP_LEVEL_INFO, LogState#logstate);
do_log_info(Format, Data, LogState#logstate) when is_list(Format),
                                                  is_list(Data) ->
    do_log(Format, Data, ?DISP_LEVEL_INFO, LogState#logstate);
do_log_info(_Format, _Data, LogState#logstate) ->
    LogState#logstate{formatcount=LogState#logstate.formatcount+1}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Property:
%   private
% Description:
%   Log warning messages, such as some operation related messages
% Parameter:
%   Format              : binary/list to be displayed
%   LogState#logstate   :
% Return:
%   LogState#logstate
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
do_log_warn(Format, LogState#logstate) when is_binary(Format) ->
    do_log(Format, ?DISP_LEVEL_WARN, LogState#logstate);
do_log_warn(Format, LogState#logstate) when is_list(Format) ->
    do_log(Format, ?DISP_LEVEL_WARN, LogState#logstate);
do_log_warn(_Format, LogState#logstate) ->
    LogState#logstate{formatcount=LogState#logstate.formatcount+1}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Property:
%   private
% Description:
%   Log warning messages, such as some operation related messages
% Parameter:
%   Format              : binary/list to be displayed
%   LogState#logstate   :
% Return:
%   LogState#logstate
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
do_log_warn(Format, Data, LogState#logstate) when is_binary(Format),
                                                  is_list(Data) ->
    do_log(Format, Data, ?DISP_LEVEL_WARN, LogState#logstate);
do_log_warn(Format, Data, LogState#logstate) when is_binary(Format),
                                                  is_binary(Data) ->
    do_log(Format, Data, ?DISP_LEVEL_WARN, LogState#logstate);
do_log_warn(Format, Data, LogState#logstate) when is_list(Format),
                                                  is_binary(Data) ->
    do_log(Format, Data, ?DISP_LEVEL_WARN, LogState#logstate);
do_log_warn(Format, Data, LogState#logstate) when is_list(Format),
                                                  is_list(Data) ->
    do_log(Format, Data, ?DISP_LEVEL_WARN, LogState#logstate);
do_log_warn(_Format, _Data, LogState#logstate) ->
    LogState#logstate{formatcount=LogState#logstate.formatcount+1}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description:
%   Log errors.
% Parameter :
%       Format              : binary/list to be displayed
%       LogState#logstate   :
% Return :
%       LogState#logstate
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
do_log_err(Format, LogState#logstate) when is_binary(Format) ->
    do_log(Format, ?DISP_LEVEL_ERR, LogState#logstate);
do_log_err(Format, LogState#logstate) when is_list(Format) ->
    do_log(Format, ?DISP_LEVEL_ERR, LogState#logstate);
do_log_err(_Format, LogState#logstate) ->
    LogState#logstate{formatcount=LogState#logstate.formatcount+1}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description:
%   Log errors.
% Parameter :
%       Format + Data       : binary/list to be displayed
%       LogState#logstate   :
% Return :
%       LogState#logstate
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
do_log_err(Format, Data, LogState#logstate) when is_binary(Format),
                                                 is_binary(Data) ->
    do_log(Format, Data, ?DISP_LEVEL_ERR, LogState#logstate);
do_log_err(Format, Data, LogState#logstate) when is_binary(Format),
                                                 is_list(Data) ->
    do_log(Format, Data, ?DISP_LEVEL_ERR, LogState#logstate);
do_log_err(Format, Data, LogState#logstate) when is_list(Format),
                                                 is_binary(Data) ->
    do_log(Format, Data, ?DISP_LEVEL_ERR, LogState#logstate);
do_log_err(Format, Data, LogState#logstate) when is_list(Format),
                                                 is_list(Data) ->
    do_log(Format, Data, ?DISP_LEVEL_ERR, LogState#logstate);
do_log_err(_Format, _Data, LogState#logstate) ->
    LogState#logstate{formatcount=LogState#logstate.formatcount+1}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
%    Display the messages.
%    When the system defined display log enabled, which is LogState#logstate.logenabled, is NO, the message won't be displayed.
%    When Level is smaller than the system defined display log level, which is LogState#logstate.loglevel, the message won't be displayed
%    This method should not be called except log_info/log_warn/log_err
% Parameter :
%       Format              : binary/list to be displayed
%       Level               : DISP_LEVEL_ALL/DISP_LEVEL_INFO/DISP_LEVEL_IMP/DISP_LEVEL_ERR
%       LogState#logstate   :
% Return :
%       LogState#logstate
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
do_log(Format, Level, LogState#logstate) when is_binary(Format),
                                              Level >= ?DISP_LEVEL_INFO,
                                              Level =< ?DISP_LEVEL_ERR ->
    try
        do_log(binary_to_list(Format), Level),
        case Level of
            ?DISP_LOG_INFO ->
                LogState#logstate{infocount=LogState#logstate.infocount+1};
            ?DISP_LEVEL_WARN ->
                LogState#logstate{infocount=LogState#logstate.warncount+1};
            ?DISP_LEVEL_ERR ->
                LogState#logstate{infocount=LogState#logstate.errcount+1}
        end
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
                error_logger:info_msg(Format),
                LogState#logstate{infocount=LogState#logstate.infocount+1};
            ?DISP_LEVEL_WARN ->
                error_logger:warning_msg(Format),
                LogState#logstate{infocount=LogState#logstate.warncount+1};
            ?DISP_LEVEL_ERR ->
                error_logger:error_msg(Format),
                LogState#logstate{infocount=LogState#logstate.errcount+1}
        end
    catch
        Oper:Msg ->
            error_logger:error_msg("do_log(Format - list, LogState#logstate) exception : ~p : ~p", [Oper, Msg]),
            LogState#logstate{expcount=LogState#logstate.expcount+1}
    end;
do_log(_Format, _Level) ->
    LogState#logstate{formatcount=LogState#logstate.formatcount+1}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
%    Display the messages.
%    When the system defined display log enabled, which is LogState#logstate.logenabled, is NO, the message won't be displayed.
%    When Level is smaller than the system defined display log level, which is LogState#logstate.loglevel, the message won't be displayed
%    This method should not be called except log_info/log_warn/log_err
% Parameter :
%       Format + Data       : binary/list to be displayed
%       Level               : DISP_LEVEL_ALL/DISP_LEVEL_INFO/DISP_LEVEL_IMP/DISP_LEVEL_ERR
%       LogState#logstate   :
% Return :
%       LogState#logstate
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
do_log(Format, Data, Level, LogState#logstate) when is_binary(Format),
                                                    is_binary(Data),
                                                    Level >= ?DISP_LEVEL_INFO,
                                                    Level =< ?DISP_LEVEL_ERR ->
    try
        do_log(binary_to_list(Format), binary_to_list(Data), Level),
        case Level of
            ?DISP_LOG_INFO ->
                LogState#logstate{infocount=LogState#logstate.infocount+1};
            ?DISP_LEVEL_WARN ->
                LogState#logstate{infocount=LogState#logstate.warncount+1};
            ?DISP_LEVEL_ERR ->
                LogState#logstate{infocount=LogState#logstate.errcount+1}
        end
    catch
        Oper:Msg ->
            error_logger:error_msg("do_log(Format - binary, Data - binary, LogState#logstate) exception : ~p : ~p", [Oper, Msg]),
            LogState#logstate{expcount=LogState#logstate.expcount+1}
    end;
do_log(Format, Data, Level, LogState#logstate) when is_list(Format),
                                                    is_binary(Data),
                                                    Level >= ?DISP_LEVEL_INFO,
                                                    Level =< ?DISP_LEVEL_ERR ->
    try
        do_log(Format, binary_to_list(Data), Level),
        case Level of
            ?DISP_LOG_INFO ->
                LogState#logstate{infocount=LogState#logstate.infocount+1};
            ?DISP_LEVEL_WARN ->
                LogState#logstate{infocount=LogState#logstate.warncount+1};
            ?DISP_LEVEL_ERR ->
                LogState#logstate{infocount=LogState#logstate.errcount+1}
        end
    catch
        Oper:Msg ->
            error_logger:error_msg("do_log(Format - list, Data - binary, LogState#logstate) exception : ~p : ~p", [Oper, Msg]),
            LogState#logstate{expcount=LogState#logstate.expcount+1}
    end;
do_log(Format, Data, Level, LogState#logstate) when is_binary(Format),
                                                    is_list(Data),
                                                    Level >= ?DISP_LEVEL_INFO,
                                                    Level =< ?DISP_LEVEL_ERR ->
    try
        do_log(binary_to_list(Format), Data, Level),
        case Level of
            ?DISP_LOG_INFO ->
                LogState#logstate{infocount=LogState#logstate.infocount+1};
            ?DISP_LEVEL_WARN ->
                LogState#logstate{infocount=LogState#logstate.warncount+1};
            ?DISP_LEVEL_ERR ->
                LogState#logstate{infocount=LogState#logstate.errcount+1}
        end
    catch
        Oper:Msg ->
            error_logger:error_msg("do_log(Format - binary, Data - list, LogState#logstate) exception : ~p : ~p", [Oper, Msg]),
            LogState#logstate{expcount=LogState#logstate.expcount+1}
    end;
do_log(Format, Data, Level, LogState#logstate) when is_list(Format),
                                                    is_list(Data),
                                                    Level >= ?DISP_LEVEL_INFO,
                                                    Level =< ?DISP_LEVEL_ERR ->
    try
        case Level of
            ?DISP_LOG_INFO ->
                error_logger:info_msg(Format, Data),
                LogState#logstate{infocount=LogState#logstate.infocount+1};
            ?DISP_LEVEL_WARN ->
                error_logger:warning_msg(Format, Data),
                LogState#logstate{infocount=LogState#logstate.warncount+1};
            ?DISP_LEVEL_ERR ->
                error_logger:error_msg(Format, Data),
                LogState#logstate{infocount=LogState#logstate.errcount+1}
        end
    catch
        Oper:Msg ->
            error_logger:error_msg("do_log(Format - list, Data - list, LogState#logstate) exception : ~p : ~p", [Oper, Msg]),
            LogState#logstate{expcount=LogState#logstate.expcount+1}
    end;
do_log(_Format, _Data, _Level, LogState#logstate) ->
    LogState#logstate{formatcount=LogState#logstate.formatcount+1}.
