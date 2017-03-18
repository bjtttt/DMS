%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% cc_helper.erl
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(cc_helper).

-include("../include/header_struct.hrl").

-export([convert_utf8_to_gbk/1,
         convert_utf8_to_gbk/2,
         convert_gbk_to_utf8/1,
         convert_gbk_to_utf8/2,
         code_convertor_process/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description:
%
% Parameter:
%       CCPid   :
%       Src     :
% Return:
%       ok.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
convert_utf8_to_gbk(CCPid, Src) when is_binary(Src) orelse is_list(Src) ->
    CCPid ! {self(), utf82gbk, Src},
    receive
        undefined ->
            Src;
        Value ->
            Value
    after ?TIMEOUT_CC_PROCESS ->
            Src
    end;
convert_utf8_to_gbk(_CCPid, Src) ->
    Src.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description:
%
% Parameter:
%       Src     :
% Return:
%       ok.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
convert_utf8_to_gbk(Src) when is_binary(Src) orelse is_list(Src) ->
    [{ccpid, CCPid}] = ets:lookup(msgservertable, ccpid),
    CCPid ! {self(), utf82gbk, Src},
    receive
        undefined ->
            Src;
        Value ->
            Value
    after ?TIMEOUT_CC_PROCESS ->
            Src
    end;
convert_utf8_to_gbk(Src) ->
    Src.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description:
%
% Parameter:
%       CCPid   :
%       Src     :
% Return:
%       ok.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
convert_gbk_to_utf8(CCPid, Src) when is_binary(Src) orelse is_list(Src) ->
    CCPid ! {self(), gbk2utf8, Src},
    receive
        undefined ->
            Src;
        Value ->
            Value
    after ?TIMEOUT_CC_PROCESS ->
            Src
    end;
convert_gbk_to_utf8(_CCPid, Src) ->
    Src.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description:
%
% Parameter:
%       Src     :
% Return:
%       ok.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
convert_gbk_to_utf8(Src) when is_binary(Src) orelse is_list(Src) ->
    [{ccpid, CCPid}] = ets:lookup(msgservertable, ccpid),
    CCPid ! {self(), gbk2utf8, Src},
    receive
        undefined ->
            Src;
        Value ->
            Value
    after ?TIMEOUT_CC_PROCESS ->
            Src
    end;
convert_gbk_to_utf8(Src) ->
    Src.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
%        Convert characters between UTF8 and GBK
% Parameter:
%   LogPid  :
% Return:
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
code_convertor_process(LogPid) ->
    receive
        {Pid, create} ->            
            ccprocessor:init_code_table(),
            log:log_info(LogPid, "Code convertor table is initialized."),
            Pid ! created,
            code_convertor_process(LogPid);
        stop ->
            ok;
        {Pid, gbk2utf8, Source} ->
            try
                Destination = ccprocessor:to_utf8(Source),
                log:log_info("code_convertor_process : source GBK : ~p, dest UTF8 : ~p", [Source, Destination]),
                Pid ! Destination
            catch
                _:Reason ->
                    log:log_err("code_convertor_process : source ~p, dest UTF8 Exception : ~p", [Source, Reason]),
                    Pid ! Source
            end,
            code_convertor_process();
        {Pid, utf82gbk, Source} ->
            try
                Destination = ccprocessor:to_gbk(Source),
                log:log_info("code_convertor_process : source UTF8 : ~p, dest GBK : ~p", [Source, Destination]),
                Pid ! Destination
            catch
                _:Reason ->
                    log:logerr("code_convertor_process : source ~p, dest GBK Exception : ~p", [Source, Reason]),
                    Pid ! Source
            end,
            code_convertor_process();
        {Pid, Msg} ->
            log:loghint("code_convertor_process : unknown request : ~p", [Msg]),
            Pid ! Msg,
            code_convertor_process();
        _ ->
            log:loghint("code_convertor_process : unknown message"),
            code_convertor_process()
    end.

