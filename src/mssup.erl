%
% This is the root supervisor 
%

-module(mssup).

-behaviour(supervisor).

-include("../include/header.hrl").

-export([start_link/0]).

-export([start_child_vdr/2, start_child_mon/1]).

-export([stop_child_vdr/1, stop_child_mon/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% startchild_ret() = {ok, Child :: child()}
%                  | {ok, Child :: child(), Info :: term()}
%                  | {error, startchild_err()}
% startchild_err() = already_present
%                  | {already_started, Child :: child()}
%                  | term()
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_child_vdr(Socket, Addr) ->
    case supervisor:start_child(sup_vdr_handler, [Socket, Addr]) of
        {ok, Pid} ->
            {ok, Pid};
        {ok, Pid, Info} ->
            {ok, Pid, Info};
        {error, Reason} ->
            case Reason of
                already_present ->
                    common:loginfo("mssup:start_child_vdr fails : already_present~n");
                {already_strated, CPid} ->
                    common:loginfo("mssup:start_child_vdr fails : already_started PID : ~p~n", [CPid]);
                Msg ->
                    common:loginfo("mssup:start_child_vdr fails : ~p~n", [Msg])
            end,
            {error, Reason}
    end.         

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% startchild_ret() = {ok, Child :: child()}
%                  | {ok, Child :: child(), Info :: term()}
%                  | {error, startchild_err()}
% startchild_err() = already_present
%                  | {already_started, Child :: child()}
%                  | term()
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_child_mon(Socket) ->
    case supervisor:start_child(sup_mon_handler, [Socket]) of
        {ok, Pid} ->
            {ok, Pid};
        {ok, Pid, Info} ->
            {ok, Pid, Info};
        {error, Reason} ->
            case Reason of
                already_present ->
                    common:loginfo("mssup:start_child_mon fails : already_present~n");
                {already_strated, CPid} ->
                    common:loginfo("mssup:start_child_mon fails : already_started PID : ~p~n", [CPid]);
                Msg ->
                    common:loginfo("mssup:start_child_mon fails : ~p~n", [Msg])
            end,
            {error, Reason}
    end.          

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% ok
% {error, Error} : Error = not_found | simple_one_for_one
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
stop_child_vdr(Pid) ->
    case supervisor:terminate_child(sup_vdr_handler, Pid) of
        ok ->
            ok;
        {error, Reason} ->
            common:loginfo("mssup:stop_child_vdr(PID : ~p) fails : ~p~n", [Reason, Pid]),
            {error, Reason}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% ok
% {error, Error} : Error = not_found | simple_one_for_one
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
stop_child_mon(Pid) ->
    case supervisor:terminate_child(sup_mon_handler, Pid) of
        ok ->
            ok;
        {error, Reason} ->
            common:loginfo("mssup:stop_child_mon fails(PID : ~p) : ~p~n", [Reason, Pid]),
            {error, Reason}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% startlink_ret() = {ok, pid()}
%                 | ignore
%                 | {error, startlink_err()}
% startlink_err() = {already_started, pid()}
%                 | {shutdown, term()}
%                 | term()
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link() ->
    case supervisor:start_link({local, ?SERVER}, ?MODULE, []) of
        {ok, Pid} ->
            {ok, Pid};
        ignore ->
            common:loginfo("mssup:start_link : ignore~n"),
            ignore;
        {error, Reason} ->
            case Reason of
                {shutdown, Info} ->
                    common:loginfo("mssup:start_link fails : shutdown : ~p~n", [Info]);
                {already_strated, CPid} ->
                    common:loginfo("mssup:start_link fails : already_started PID : ~p~n", [CPid]);
                Msg ->
                    common:loginfo("mssup:start_link fails : ~p~n", [Msg])
            end,
            {error, Reason}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([]) ->
	log:loginfo("mssup:init([])"),
    % Listen VDR connection
    VDRServer = {
				 vdr_server,                            		% Id       = internal id
				 {vdr_server, start_link, [?DEF_PORT_VDR]},    	% StartFun = {M, F, A}
				 permanent,                              		% Restart  = permanent | transient | temporary
				 brutal_kill,                               	% Shutdown = brutal_kill | int() >= 0 | infinity
				 worker,                                    	% Type     = worker | supervisor
				 [vdr_server]                            		% Modules  = [Module] | dynamic
				},
    % Process VDR communication
    VDRHandler = {
				  sup_vdr_handler,               				% Id       = internal id
				  {supervisor, start_link, [{local, sup_vdr_handler}, ?MODULE, [vdr_handler]]},
				  permanent, 									% Restart  = permanent | transient | temporary
				  ?TIME_TERMINATE_VDR, 							% Shutdown = brutal_kill | int() >= 0 | infinity
				  supervisor, 				    				% Type     = worker | supervisor
				  []											% Modules  = [Module] | dynamic
				 },
    % Listen Monitor connection
    MonServer = {
                 mon_server,                             		% Id       = internal id
                 {mon_server, start_link, [?DEF_PORT_MON]},   	% StartFun = {M, F, A}
                 permanent,                                 	% Restart  = permanent | transient | temporary
                 brutal_kill,                               	% Shutdown = brutal_kill | int() >= 0 | infinity
                 worker,                                    	% Type     = worker | supervisor
                 [mon_server]                            		% Modules  = [Module] | dynamic
                },
    % Process Monitor communication
    MonHandler = {
                  sup_mon_handler,               				% Id       = internal id
                  {supervisor, start_link, [{local, sup_mon_handler}, ?MODULE, [mon_handler]]},
                  permanent,                        			% Restart  = permanent | transient | temporary
                  ?TIME_TERMINATE_MON,              			% Shutdown = brutal_kill | int() >= 0 | infinity
                  supervisor,                       			% Type     = worker | supervisor
                  []                                			% Modules  = [Module] | dynamic
                 },
	Children = [VDRServer, VDRHandler, MonServer, MonHandler],
	% Assuming the values MaxR for intensity and MaxT for period, then, if more than MaxR restarts occur within MaxT seconds, 
	% the supervisor terminates all child processes and then itself. intensity defaults to 1 and period defaults to 5.
    RestartStrategy = {one_for_one, ?SUP_MAXR, ?SUP_MAXT},
    {ok, {RestartStrategy, Children}};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([Module]) ->
	log:loginfo("mssup:init([Module]) : ~p", [Module]),
    Instance = {
             	undefined,                 % Id       = internal id
                {Module, start_link, []},  % StartFun = {M, F, A}
                temporary,                 % Restart  = permanent | transient | temporary
                brutal_kill,               % Shutdown = brutal_kill | int() >= 0 | infinity
                worker,                    % Type     = worker | supervisor
                []                         % Modules  = [Module] | dynamic
               },
    Children = [Instance],
    RestartStrategy = {simple_one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.

