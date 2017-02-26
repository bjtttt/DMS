%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% header_const.hrl
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(SUP_MAX_RESTART,    3).
-define(SUP_MAX_TIME,       1).

-define(DEF_PORT_VDR,   6000).
-define(DEF_PORT_MON,   6001).

-define(DEF_MYSQL_DB_NAME,  "gps_database").
-define(DEF_MYSQL_USERNAME, "optimus").
-define(DEF_MYSQL_PASSWORD, "opt123450").
-define(DEF_HTTPGPS_SERVER, "58.246.201.138:8081").

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

-define(PROC_RESP_TIMEOUT, 10000).

-define(TIMEOUT_VDR, 60000).
%-define(TIMEOUT_MAN, 30000). 
-define(TIMEOUT_MON, 10000).

-define(TIMEOUT_CC_INIT_PROCESS, 5000). 
-define(TIMEOUT_CC_PROCESS, 10000). 

-define(TIMEOUT_DATA_MAN, 5). 
-define(TIMEOUT_DATA_VDR, 5). 

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

%%%
%%% pid     : VDR handler process id
%%% datapid : VDR handler send data to VDR process id
%%%


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
