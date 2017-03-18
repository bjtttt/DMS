%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% header_const.hrl
%
% Including CONST definitions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(START_PARAM_COUN,   3).

-define(YES,                true).
-define(NO,                 false).

-define(DEF_LOG_PATH,       "/tmp").

-define(REDIS_INIT_TIME,    20).
-define(WAIT_LOOP_INTERVAL, 1000).

-define(WS2VDRFREQ,         10).        % Need investigation

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Display level definitions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(DISP_LEVEL_INFO,    0).         % Information
-define(DISP_LEVEL_WARN,    1).         % Warning
-define(DISP_LEVEL_ERR,     2).         % Error

-define(DISP_SPEC_NONE,     0).         % CC
-define(DISP_SPEC_CC,       1).         % CC

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Connection information definitions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Timeout definitions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(TIMEOUT_CC_INIT,    5000).      % 5s
-define(TIMEOUT_CC_REQ,    10000).      % 10s
