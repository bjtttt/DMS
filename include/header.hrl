%
% header.hrl
%

-define(DISP_LEVEL_ALL,     0).
-define(DISP_LEVEL_NONE,    1).
-define(DISP_LEVEL_INFO,    2).
-define(DISP_LEVEL_HINT,    3).
-define(DISP_LEVEL_ERR,     4).

-define(SUP_MAX_RESTART,    3).
-define(SUP_MAX_TIME,       1).

-define(DEF_PORT_MYSQL,    3306).
-define(DEF_PORT_VDR,    6000).
-define(DEF_PORT_MON,     6001).

-define(DEF_MYSQL_DB_NAME,    "gps_database").
-define(DEF_MYSQL_USERNAME, "optimus").
-define(DEF_MYSQL_PASSWORD, "opt123450").
-define(DEF_HTTPGPS_SERVER, "58.246.201.138:8081").

-define(DEF_LOG_PATH, "/tmp").


-define(CONN_STAT_TEST,                 0).     %
-define(CONN_STAT_CONN,                 1).     % Count for VDR connections
-define(CONN_STAT_DISC_CHAR,            2).     % Disconnection count due to invalid characters in the message from VDR to the gateway
-define(CONN_STAT_DISC_REG,             3).     % Disconnection count due to unregistered VDR
-define(CONN_STAT_DISC_AUTH,            4).     % Disconnection count due to error information from authorized VDR
-define(CONN_STAT_DISC_UNAUTH,          5).     % Disconnection count due to unanthorized VDR
-define(CONN_STAT_DISC_ERR_CNT,         6).     % Disconnection count due to VDR communication error count reaches the MAX
-define(CONN_STAT_DISC_CLI,             7).     % Disconnection count due to VDR requirement
-define(CONN_STAT_DISC_LEN,             8).     % Disconnection count due to MSG length error
-define(CONN_STAT_DISC_PARSE_PARITY,    9).     % Disconnection count due to MSG parity error
-define(CONN_STAT_DISC_RESTORE,        10).     % Disconnection count due to MSG restore error
-define(CONN_STAT_DISC_PACK,           11).     % Disconnection count due to sub MSGes, which can be combine to one big MSG, package index error
-define(CONN_STAT_DISC_TIMEOUT,        12).     % Disconnection count due to VDR timeout
-define(CONN_STAT_DISC_UNREG,          13).     % Disconnection count due to VDR unregistry
-define(CONN_STAT_DISC_MSGEX,          14).     % Disconnection count due to MSG parsing exception
-define(CONN_STAT_DISC_INVALID_MSG,    15).     % Disconnection count due to invalid MSG ID from VDR
-define(CONN_STAT_DISC_MSG_ERR,        16).     % Disconnection count due to VDR MSG parsing error
-define(CONN_STAT_DISC_UNK_MSG_ERR,    17).     % Disconnection count due to unknown VDR MSG error
-define(CONN_STAT_DISC_UNK_ERR,        18).     % Disconnection count due to unknown VDR error
-define(CONN_STAT_SPLIT_ERR,           19).     % Multi MSGes splitting error
-define(CONN_STAT_SERVER_MSG,          20).     % Count for MSGs from an unknown place instead of VDR
-define(CONN_STAT_INVALID_MSG,         21).     % Count for undefined-type MSGes
-define(CONN_STAT_TO_GW,               22).     % Count for MSGes to gateway
-define(CONN_STAT_FROM_GW,             23).     % Count for MSGes from gateway
-define(CONN_STAT_UNK_ERR,             24).     % Unknown VDR error
-define(CONN_STAT_PARSE_ERROR,         25).     % Parsing: error
-define(CONN_STAT_PARSE_EXCEPTION,     26).     % Parsing: exception
-define(CONN_STAT_PARSE_UNSUPPORTED_ID,27).     % Parsing: unsupported message id
-define(CONN_STAT_PARSE_LEN_MISMATCH,  28).     % Parsing: calculated length =/= actual length
-define(CONN_STAT_PARSE_TOTAL_ERROR,   29).     % Parsing: total package number =< 1 for multiple messages
-define(CONN_STAT_PARSE_INDEX_SMALL,   30).     % Parsing: current package index < 1 for multiple messages
-define(CONN_STAT_PARSE_INDEX_LARGE,   31).     % Parsing: current package index > total package number for multiple messages
-define(CONN_STAT_INFO_COUNT,          32).     % Count for connection status information, should be of the last one and for an indication of the length

%
% Timeout definitions
%
-define(TIMEOUT_CC_INIT,    5000). 
-define(TIMEOUT_CC_REQ,    10000). 

-define(SUP_WAIT_INTVL_MS, 5000).

-define(MAX_VDR_ERR_COUNT, 5).

-define(DB_HASH_UPDATE_INTERVAL, 3*60*60*1000).
-define(DB_HASH_UPDATE_ONCE_COUNT, 2000).

-define(MAX_DB_STORED_COUNT, 1000).
-define(MAX_DB_STORED_HALF_COUNT, 500).
-define(MAX_DB_STORED_URGENT_COUNT, 100).
-define(MAX_DB_PROC_WAIT_INTERVAL, 30000).

-define(DB_RESP_TIMEOUT, 30000).
-define(PROC_RESP_TIMEOUT, 10000).

%%% DB_SUP_MAX and DB_SUP_WITHIN are use in DB Supervisor for DB client restart mechanism
%%% In development, they are 0 and 1 to make debug more easy and efficient.
%%% When release, they should be 10 and 10 OR other better values
-define(DB_SUP_MAX, 0).
-define(DB_SUP_WITHIN, 1).

%-define(LOG_DEBUG_INFO_ERR, 2).
%-define(LOG_INFO_ERR, 1).
%-define(LOG_ERR, 0).

-define(WAIT_LOOP_INTERVAL, 1000).

-define(TIMEOUT_VDR, 60000). 
%-define(TIMEOUT_MAN, 30000). 
-define(TIMEOUT_MON, 10000). 
-define(TIMEOUT_DB, 30000). 

-define(TIMEOUT_CC_INIT_PROCESS, 5000). 
-define(TIMEOUT_CC_PROCESS, 10000). 

-define(TIMEOUT_DB_PROCESS, 1000). 
-define(DB_PROCESS_TRIAL_MAX, 10). 
-define(DB_PROCESS_FAILURE_MAX, 10). 

-define(TIMEOUT_DATA_MAN, 5). 
-define(TIMEOUT_DATA_VDR, 5). 
-define(TIMEOUT_DATA_DB, 1). 

-define(TIME_TERMINATE_VDR, 10000).
-define(TIME_TERMINATE_MAN, 5000).
-define(TIME_TERMINATE_MON, 5000).
-define(TIME_TERMINATE_MP, 5000).
-define(TIME_TERMINATE_DB, 5000).
-define(TIME_TERMINATE_ICONV, 5000).

-define(T_GEN_RESP_OK, 0).
-define(T_GEN_RESP_FAIL, 1).
-define(T_GEN_RESP_ERRMSG, 2).
-define(T_GEN_RESP_NOTSUPPORT, 3).

-define(P_GENRESP_OK, 0).
-define(P_GENRESP_FAIL, 1).
-define(P_GENRESP_ERRMSG, 2).
-define(P_GENRESP_NOTSUPPORT, 3).
-define(P_GENRESP_ALARMACK, 4).

-define(MAX_SINGLE_MSG_LEN, 512).
-define(MULTI_MSG_INTERVAL, 250).

-define(LEN_BYTE, 8).
-define(LEN_WORD, 16).
-define(LEN_DWORD, 32).
-define(LEN_BYTE_BYTE, 1).
-define(LEN_WORD_BYTE, 2).
-define(LEN_DWORD_BYTE, 4).

-define(CONNECTING, 0).
-define(OPEN, 1).
-define(CLOSED, 2).

-define(WS2VDRFREQ, 10).

-define(VDR_MSG_TIMEOUT,      300000).
-define(VDR_MSG_RESP_TIMEOUT,   5000).
-define(CC_PID_TIMEOUT,         5000).

-define(MON_MSG_TIMEOUT,      300000).

-define(SUB_PACK_INDI_HEADER, <<255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255>>).

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
                    linkpid=undefined,
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
                    linkinfopid=undefined
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

-record(driverinfo, {    driverid=undefined,
                        licno=undefined,
                        certcode=undefined,
                        vdrauthcode=undefined
                     }).

-record(lastposinfo, {    vehicleid=undefined,
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
                       linkinfopid = undefined
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
