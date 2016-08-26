%
% This is the root supervisor 
%

-module(mssup).

-behaviour(supervisor).

-include("../include/header.hrl").

-export([start_link/0]).

-export([start_child_vdr/3, start_child_mon/3]).

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
start_child_vdr(Socket, Addr, LinkInfoPid) ->
    mslog:loginfo("mssup:start_child_vdr(Socket : ~p, Addr : ~p)", [Socket, Addr]),
    case supervisor:start_child(sup_vdr_handler, [Socket, Addr, LinkInfoPid]) of
        {ok, Pid} ->
            {ok, Pid};
        {ok, Pid, Info} ->
            {ok, Pid, Info};
        {error, Reason} ->
            case Reason of
                already_present ->
                    mslog:loghint("mssup:start_child_vdr fails : already_present");
                {already_strated, CPid} ->
                    mslog:logerr("mssup:start_child_vdr fails : already_started PID : ~p", [CPid]);
                Msg ->
                    mslog:logerr("mssup:start_child_vdr fails : ~p", [Msg])
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
start_child_mon(Socket, Addr, LinkInfoPid) ->
    mslog:loginfo("mssup:start_child_mon(Socket : ~p, Addr : ~p)", [Socket, Addr]),
    case supervisor:start_child(sup_mon_handler, [Socket, Addr, LinkInfoPid]) of
        {ok, Pid} ->
            {ok, Pid};
        {ok, Pid, Info} ->
            {ok, Pid, Info};
        {error, Reason} ->
            case Reason of
                already_present ->
                    mslog:loghint("mssup:start_child_mon fails : already_present");
                {already_strated, CPid} ->
                    mslog:logerr("mssup:start_child_mon fails : already_started PID : ~p", [CPid]);
                Msg ->
                    mslog:logerr("mssup:start_child_mon fails : ~p", [Msg])
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
    mslog:loginfo("mssup:stop_child_vdr(PID : ~p)", [Pid]),
    case supervisor:terminate_child(sup_vdr_handler, Pid) of
        ok ->
            ok;
        {error, Reason} ->
            mslog:logerr("mssup:stop_child_vdr(Pid : ~p) fails : ~p", [Pid, Reason]),
            {error, Reason}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% ok
% {error, Error} : Error = not_found | simple_one_for_one
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
stop_child_mon(Pid) ->
    mslog:loginfo("mssup:stop_child_mon(PID : ~p)", [Pid]),
    case supervisor:terminate_child(sup_mon_handler, Pid) of
        ok ->
            ok;
        {error, Reason} ->
            mslog:logerr("mssup:stop_child_mon fails(PID : ~p) : ~p", [Pid, Reason]),
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
            common:loghint("mssup:start_link : ignore"),
            ignore;
        {error, Reason} ->
            case Reason of
                {shutdown, Info} ->
                    common:logerr("mssup:start_link fails : shutdown : ~p", [Info]);
                {already_strated, CPid} ->
                    common:logerr("mssup:start_link fails : already_started PID : ~p", [CPid]);
                Msg ->
                    common:logerr("mssup:start_link fails : ~p", [Msg])
            end,
            {error, Reason}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([]) ->
    mslog:loghint("mssup:init([])"),
    [{linkinfopid, LinkInfoPid}] = ets:lookup(msgservertable, linkinfopid),
    % Id        : used to identify the child specification internally by the supervisor.
    %             Notice that this identifier on occations has been called "name". 
    %             As far as possible, the terms "identifier" or "id" are now used but to keep backward compatibility, 
    %               some occurences of "name" can still be found, for example in error messages.
    % StartFun  : defines the function call used to start the child process. It must be a module-function-arguments tuple {M,F,A} used as apply(M,F,A).
    %             The start function must create and link to the child process, and must return {ok,Child} or {ok,Child,Info}, 
    %               where Child is the pid of the child process and Info any term that is ignored by the supervisor.
    %             The start function can also return ignore if the child process for some reason cannot be started, 
    %               in which case the child specification is kept by the supervisor (unless it is a temporary child) but the non-existing child process is ignored.
    %             If something goes wrong, the function can also return an error tuple {error,Error}.
    %             Notice that the start_link functions of the different behavior modules fulfill the above requirements.
    % Restart   : defines when a terminated child process must be restarted. A permanent child process is always restarted. 
    %             A temporary child process is never restarted (even when the supervisor's restart strategy is rest_for_one or one_for_all and a sibling's death causes the temporary process to be terminated).
    %             A transient child process is restarted only if it terminates abnormally, that is, with another exit reason than normal, shutdown, or {shutdown,Term}.
    %             If it is not specified, it defaults to permanent.
    % Shutdown  : defines how a child process must be terminated. 
    %               brutal_kill means that the child process is unconditionally terminated using exit(Child,kill). 
    %               An integer time-out value means that the supervisor tells the child process to terminate by calling exit(Child,shutdown) and then wait for an exit signal with reason shutdown back from the child process. 
    %               If no exit signal is received within the specified number of milliseconds, the child process is unconditionally terminated using exit(Child,kill).
    %             If the child process is another supervisor, the shutdown time is to be set to infinity to give the subtree ample time to shut down. It is also allowed to set it to infinity, if the child process is a worker.
    %             Be careful when setting the shutdown time to infinity when the child process is a worker. Because, in this situation, 
    %               the termination of the supervision tree depends on the child process, it must be implemented in a safe way and its cleanup procedure must always return.
    %             Notice that all child processes implemented using the standard OTP behavior modules automatically adhere to the shutdown protocol.
    %             The shutdown key is optional. If it is not specified, it defaults to 5000 if the child is of type worker and it defaults to infinity if the child is of type supervisor.
    % Type      : specifies if the child process is a supervisor or a worker.
    %             The type key is optional. If it is not specified, it defaults to worker.
    % Modules   : is used by the release handler during code replacement to determine which processes are using a certain module. 
    %               As a rule of thumb, if the child process is a supervisor, gen_server, gen_statem, or gen_fsm, this is to be a list with one element [Module], 
    %               where Module is the callback module. If the child process is an event manager (gen_event) with a dynamic set of callback modules, value dynamic must be used.
    %             The modules key is optional. If it is not specified, it defaults to [M], where M comes from the child's start {M,F,A}.
    % Listen VDR connection
    VDRServer = {
                 vdr_server,                                    % Id       = internal id
                 {vdr_server, start_link, [LinkInfoPid]},       % StartFun = {M, F, A}
                 permanent,                                     % Restart  = permanent | transient | temporary
                 brutal_kill,                                   % Shutdown = brutal_kill | int() >= 0 | infinity
                 worker,                                        % Type     = worker | supervisor
                 [vdr_server]                                   % Modules  = [Module] | dynamic
                },
    % Process VDR communication
    VDRHandler = {
                  sup_vdr_handler,                              % Id       = internal id
                  {supervisor, start_link, [{local, sup_vdr_handler}, ?MODULE, [vdr_handler]]},
                  permanent,                                    % Restart  = permanent | transient | temporary
                  ?TIME_TERMINATE_VDR,                          % Shutdown = brutal_kill | int() >= 0 | infinity
                  supervisor,                                   % Type     = worker | supervisor
                  []                                            % Modules  = [Module] | dynamic
                 },
    % Listen Monitor connection
    MonServer = {
                 mon_server,                                    % Id       = internal id
                 {mon_server, start_link, [LinkInfoPid]},       % StartFun = {M, F, A}
                 permanent,                                     % Restart  = permanent | transient | temporary
                 brutal_kill,                                   % Shutdown = brutal_kill | int() >= 0 | infinity
                 worker,                                        % Type     = worker | supervisor
                 [mon_server]                                   % Modules  = [Module] | dynamic
                },
    % Process Monitor communication
    MonHandler = {
                  sup_mon_handler,                              % Id       = internal id
                  {supervisor, start_link, [{local, sup_mon_handler}, ?MODULE, [mon_handler]]},
                  permanent,                                    % Restart  = permanent | transient | temporary
                  ?TIME_TERMINATE_MON,                          % Shutdown = brutal_kill | int() >= 0 | infinity
                  supervisor,                                   % Type     = worker | supervisor
                  []                                            % Modules  = [Module] | dynamic
                 },
    Children = [VDRServer, VDRHandler, MonServer, MonHandler],
    % one_for_one - If one child process terminates and is to be restarted, only that child process is affected.
    %   If a child process terminates, only that process is restarted. This is the default restart strategy.
    % Assuming the values MaxR for intensity and MaxT for period, then, if more than MaxR restarts occur within MaxT seconds, 
    % the supervisor terminates all child processes and then itself. intensity defaults to 1 and period defaults to 5.
    RestartStrategy = {one_for_one, ?SUP_MAX_RESTART, ?SUP_MAX_TIME},
    {ok, {RestartStrategy, Children}};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([Module]) ->
    mslog:loghint("mssup:init([Module]) : ~p", [Module]),
    Instance = {
             	undefined,                 % Id       = internal id
                {Module, start_link, []},  % StartFun = {M, F, A}
                temporary,                 % Restart  = permanent | transient | temporary
                brutal_kill,               % Shutdown = brutal_kill | int() >= 0 | infinity
                worker,                    % Type     = worker | supervisor
                []                         % Modules  = [Module] | dynamic
               },
    Children = [Instance],
    % A supervisor with restart strategy simple_one_for_one is a simplified one_for_one supervisor, where all child processes are dynamically added instances of the same process.
    % Details : http://erlang.org/doc/design_principles/sup_princ.html#simple
    RestartStrategy = {simple_one_for_one, ?SUP_MAX_RESTART, ?SUP_MAX_TIME},
    {ok, {RestartStrategy, Children}}.

