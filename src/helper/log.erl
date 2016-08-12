%
% log.erl
%

-module(log).

-include("../../include/header.hrl").

-export([lognone/1,
         lognone/2,
         loginfo/1,
         loginfo/2,
         loghint/1,
         loghint/2,
         logerr/1,
         logerr/2,
         log_vdr_statistics_info]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Parameter :
%       Format       : a list, for example : [], [Msg] or [Msg1, Msg2]
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
logall(Format) ->
    do_log(Format, ?DISP_LEVEL_ALL, 0).

lognone(Format) ->
    do_log(Format, ?DISP_LEVEL_NONE, 0).

loginfo(Format) ->
    do_log(Format, ?DISP_LEVEL_INFO, 0).

loghint(Format) ->
    do_log(Format, ?DISP_LEVEL_HINT, 0).

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Parameter :
%       Data        : a list, for example : [], [Msg] or [Msg1, Msg2]
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
logall(Format, Data) ->
    do_log(Format, Data, ?DISP_LEVEL_ALL, 0).

lognone(Format, Data) ->
    do_log(Format, Data, ?DISP_LEVEL_NONE, 0).

loginfo(Format, Data) ->
    do_log(Format, Data, ?DISP_LEVEL_INFO, 0).

loghint(Format, Data) ->
    do_log(Format, Data, ?DISP_LEVEL_HINT, 0).

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
                            DispErr == 0 ->
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
            vdr_handler:logvdr(error, State, "undefined link info pid", [])
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
logvdr(Type, State, FormatEx, DataEx) when is_list(FormatEx),
                                           is_list(DataEx) ->
    Format = "(Pid ~p) vdr_handler (id ~p, addr ~p, serialno ~p, auth ~p, vehicleid ~p, vehiclecode ~p, driverid ~p) : ",
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
    end.
