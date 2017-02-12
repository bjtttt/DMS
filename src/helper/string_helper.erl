%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% string_helper.erl
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(string_helper).

-include("../include/header_const.hrl").
-include("../include/header_struct.hrl").

-export([is_string/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
%       Check whether a list is a pure string list or not.
%       Condition is each character should be within [32, 126]
% Parameter :
%       String  : source to be checked
% Return    :
%       true    : is a string
%       false   : not a string
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
is_string(String) when is_list(String) ->
    Fun = fun(X) -> 
            if 
                X < 32 -> 
                    false; 
                X > 126 -> 
                    false;
                true -> 
                    true
            end
    end,
    lists:all(Fun, String);
is_string(_String) ->
    false.

