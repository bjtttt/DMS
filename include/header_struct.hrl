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
            logenabled=?YES,
            loglevel=?DISP_LEVEL_ERR,
            infocount=0,
            warncount=0,
            errcount=0,
            expcount=0,
            unknowncount=0,
            missedcount=0
        }).
