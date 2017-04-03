%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% conn_info.erl
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(conn_info).

-include("../../include/header_struct.hrl").

-export([connection_info_process/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description:
%   Maintain the connection status information
%   Please refer to include\header.hrl for details
% Parameter:
%   List    :
% Return:
%   ok
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
connection_info_process(List) ->
    Len = length(List),
    if
        Len =:= ?CONN_STAT_INFO_COUNT ->
            receive
                stop ->
                    ok;
                clear ->
                    connection_info_process(lists:duplicate(?CONN_STAT_INFO_COUNT, 0));
                {clear, ClearIndex} ->
                    connection_info_process(lists:sublist(List, ClearIndex - 1) ++ [0] ++ lists:nthtail(ClearIndex + 1, List));
                {Pid, count} ->
                    Pid ! List;
                {record, Index} ->
                    connection_info_process(lists:sublist(List, Index - 1) ++ [lists:nth(Index, List) + 1] ++ lists:nthtail(Index + 1, List));
                _ ->
                    connection_info_process(List)
            end;
        true ->
            connection_info_process(lists:duplicate(?CONN_STAT_INFO_COUNT, 0))
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description:
%   VDR sends information to link information process.
% Parameter:
%   State   :
%   Type    :
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
log_conn_info(Pid, Type) ->
    Pid ! Type.
