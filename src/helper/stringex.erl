%%%
%%% stringex.erl
%%%

-module(stringex).

-include("../../include/header.hrl").

-export([combine_strings/1, 
        combine_strings/2,
        is_string/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
%       Combine a pure string list.
% Parameter :
%       List    : must be a pure string list. Otherwise, an empty string will be returned.
% Returns   :
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
combine_strings(List) when is_list(List)->
    combine_strings(List, true);
combine_strings(_List) ->
    [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
%       Combine a pure string list.
% Parameter :
%       List    : must be string list. Otherwise, an empty string will be returned.
%       Sep     : seperator between any two strings
% Returns   :
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
combine_strings(List, Sep) when is_list(List),
                                is_list(HasComma) ->
    case List of
        [] ->
            "";
        _ ->
            Fun = fun(X) ->
                    case is_string(X) of
                        true ->
                            true;
                        _ ->
                            false
                    end
            end,
            Bool = lists:all(Fun, List) and lists:all(Fun, Sep),
            case Bool of
                true ->
                    [H|T] = List,
                    case T of
                        [] ->
                            H;
                        _ ->
                            string:concat(string:concat(H, Sep), combine_strings(T, Sep));
                    end;
                _ ->
                    ""
            end
    end;
combine_strings(_List, _Sep) ->
    [].
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
%       Check whether a list is a pure string list or not.
%       Condition is each character should be within [32, 126]
% Parameter :
%       Source  :
% Return    :
%       true    : is a string
%       false   : not a string
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%   true only when all strings in List are NOT an empty one
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
is_string_list(List) when is_list(List) ->
    Len = length(List),
    if
        Len < 1 ->
            false;
        true ->
            Fun = fun(X) ->
                    case is_string(X) of
                        true -> true;
                        _ -> false
                    end
            end,
            lists:all(Fun, List)
    end;
is_string_list(_List) ->
    false.

