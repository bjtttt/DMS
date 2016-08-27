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
eredis_process(ConnLinkPid) ->
    receive
        {Pid, error} ->
            Pid ! ok,
            eredis_error_process(ConnLinkPid);
        stop ->
            ok;
        _ ->
            ConnLinkPid ! {self(), redis_unknown_request},
            eredis_process(ConnLinkPid)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
eredis_error_process(ConnLinkPid) ->
    receive
        {Pid, ok} ->
            Pid ! ok,
            eredis_process(ConnLinkPid);
        stop ->
            ok;
        _ ->
            ConnLinkPid ! {self(), redis_unknown_request},
            eredis_error_process(ConnLinkPid)
    end.

