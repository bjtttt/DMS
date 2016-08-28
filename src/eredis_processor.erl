%
% eredis_processor.erl
%

-module(eredis_processor).

-export([eredis_process/1,
         eredis_error_process/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
eredis_process(ConnInfoPid) ->
    receive
        {Pid, error} ->
            Pid ! ok,
            eredis_error_process(ConnInfoPid);
        stop ->
            mslog:loghint("Eredis process stops.");
        _ ->
            mslog:loghint("Eredis process receive unknown msg."),
            ConnInfoPid ! {self(), redis_unknown_request},
            eredis_process(ConnInfoPid)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
eredis_error_process(ConnInfoPid) ->
    receive
        {Pid, ok} ->
            Pid ! ok,
            eredis_process(ConnInfoPid);
        stop ->
            mslog:loghint("Eredis error process stops.");
        _ ->
            mslog:loghint("Eredis error process process receive unknown msg."),
            ConnInfoPid ! {self(), redis_unknown_request},
            eredis_error_process(ConnInfoPid)
    end.

