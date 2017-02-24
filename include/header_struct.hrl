%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% header_struct.hrl
%
% Including STRUCT definitions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include("./header_const.hrl").

-record(logstate, 
        {
            loglevel=?DISP_LEVEL_ERR,
            infocount=0,
            warncount=0,
            errcount=0,
            expcount=0,
            formatcount=0,
            unknowncount=0,
            missedcount=0,
            dummycount=0
        }).

-record(vdritem, 
        {  
            socket=undefined,
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
            vdronlinepid=undefined,
            logpid=undefined
        }).
