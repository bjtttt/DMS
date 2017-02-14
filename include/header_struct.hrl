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
            curlevel=?DISP_LEVEL_ERR,
            logenabled=?YES,
            loglevel=?DISP_LEVEL_ERR,
            allcount=0,
            infocount=0,
            warncount=0,
            errcount=0,
            unknowncount=0,
            missedcount=0
        }).
