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
