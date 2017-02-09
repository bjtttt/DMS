%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% header_struct.hrl
%
% Including STRUCT definitions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%
%%% There is only one super user, name is super, password is super.
%%% 
-record(user, {id=undefined, name=undefined, level=undefined, ip=undefined, time=undefined}).

-record(vdronlineitem, {id=undefined, online=[]}).

%%%
%%%
%%%
-record(vdritem, {  socket=undefined, 
                    id=undefined,               % DB VDR ID, only valid in DB table
                    serialno=undefined,         % Actual VDR ID
                    auth=undefined,             % Actual VDR authen code, is binary
                    vehicleid=undefined,        % DB vechile ID, only valid in DB table
                    vehiclecode=undefined,      % Actual vehicle code
                    driverid=undefined,         % DB driver ID, only valid in DB table
                    driverlicno=undefined,      % Actual driver license number
                    alarm=0,
                    alarmlist=[],
                    state=0,
                    statelist=[],
                    lastlat=0.0,
                    lastlon=0.0,
                    pid=undefined,
                    dboperid=undefined,            % db operation process
                    addr=undefined, 
                    acttime=undefined, 
                    timeout=undefined,
                    msgflownum=1,
                    errorcount=0,
                    wspid=undefined,
                    dbpid=undefined,
                    msg2vdr=[],
                    msg=[], 
                    req=[],
                    msgws2vdrflownum=?WS2VDRFREQ,
                    msgws2vdr=[],            % [{MsgID, WSFlowIdx}, ...] only one item for one MsgID
                    ccpid=undefined,
                    msgpackages={-1, []},
                    tel=0,
                    conninfopid=undefined,
                    vdrtablepid=undefined,
                    drivertablepid=undefined,
                    lastpostablepid=undefined,
                    drivercertcode=undefined,
                    httpgpspid=undefined,
                    encrypt=false,
                    vdrlogpid=undefined,
                    storedmsg4save=[],
                    vdronlinepid=undefined
                 }).

-record(vdrdbitem, {  authencode=undefined,        % VDRAuthenCode
                      vdrid=undefined,             % VDRID
                      vdrserialno=undefined,    % VDRSerialNo
                      vehiclecode=undefined,     % VehicleCode
                      vehicleid=undefined,        % VehicleID
                      driverid=undefined        % DriverID
                 }).

-record(alarmitem, {  vehicleid=undefined,        % VehicleID
                      type=undefined,             % Type
                      time=undefined%,            % Time
                      %sn=undefined                % msg flow index
                 }).

%%%
%%% pid     : VDR handler process id
%%% datapid : VDR handler send data to VDR process id
%%%
-record(manitem, {  socket=undefined, 
                    pid=undefined, 
                    manpid=undefined, 
                    addr=undefined, 
                    timeout=undefined
                 }).

-record(monitem, {  socket=undefined, 
                    pid=undefined, 
                    addr=undefined, 
                    timeout=undefined,
                    dbpid=undefined,
                    wspid=undefined,
                    driverpid=undefined,
                    vdrlogpid=undefined,
                    vdronlinepid=undefined,
                    conninfopid=undefined
                 }).

-record(mpitem, {  socket=undefined, 
                   pid=undefined, 
                   addr=undefined, 
                   timeout=undefined
                }).

-record(dbstate, {  db=undefined, 
                    dbport=undefined,
                    dbdsn=undefined,
                    dbname=undefined,
                    dbuid=undefined,
                    dbpwd=undefined,
                    dbconn=undefined,
                    dbref=undefined, 
                    dbpid=undefined,
                    timeout=undefined
                 }).

-record(wsstate, {  socket=undefined, 
                    state=?CONNECTING, 
                    headers=[], 
                    pid=undefined, 
                    wspid=undefined,
                    timeout=undefined,
                    wsacckey=undeifned
                 }).

-record(driverinfo, {   driverid=undefined,
                        licno=undefined,
                        certcode=undefined,
                        vdrauthcode=undefined
                     }).

-record(lastposinfo, {  vehicleid=undefined,
                        longitude=0.0,
                        latitude=0.0
                     }).

%
% lsock           : Listening socket
% acceptor        : Asynchronous acceptor's internal reference
% linkinfopid    :
%
-record(serverstate, { lsock = undefined, 
                       acceptor = undefined, 
                       conninfopid = undefined
                     }).

%% JSON - RFC 4627 - for Erlang
%%---------------------------------------------------------------------------
%% Copyright (c) 2007-2010, 2011, 2012 Tony Garnock-Jones <tonygarnockjones@gmail.com>
%% Copyright (c) 2007-2010 LShift Ltd. <query@lshift.net>
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use, copy,
%% modify, merge, publish, distribute, sublicense, and/or sell copies
%% of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
%% BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
%% ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
%% CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.
%%---------------------------------------------------------------------------
%%
%% Convenience macros for encoding and decoding record structures.
%%
%% Erlang's compile-time-only notion of record definitions means we
%% have to supply a constant record name in the source text.

%-define(RFC4627_FROM_RECORD(RName, R),
%    ti_rfc4627:from_record(R, RName, record_info(fields, RName))).

%-define(RFC4627_TO_RECORD(RName, R),
%    ti_rfc4627:to_record(R, #RName{}, record_info(fields, RName))).
