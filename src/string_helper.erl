%%%
%%% string_helper.erl
%%%

-module(string_helper).

-include("../include/header.hrl").

-export([combine_strings/1,
         combine_strings/2,
         is_string/1,
         is_dec_list/1]).

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
                                is_list(Sep) ->
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
                            string:concat(string:concat(H, Sep), combine_strings(T, Sep))
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
convert_list_to_bin(List) when is_list(List) ->
    try
        erlang:list_to_binary(List)
    catch
        _:_ ->
            <<"">>
    end;
convert_list_to_bin(List) when is_binary(List) ->
    List;
convert_list_to_bin(_List) ->
    <<"">>.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
convert_bin_to_list(Binary) when is_binary(Binary) ->
    try
        erlang:binary_to_list(Binary)
    catch
        _:_ ->
            []
    end;
convert_bin_to_list(Binary) when is_list(Binary) ->
    Binary;
convert_bin_to_list(_Binary) ->
    [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
%   Check whether "NNNN", "NN" or any string like this format to be a valid decimal string or not.
%   For example, "81" is true while "8A" is false
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
is_dec_integer_string(Value) when is_list(Value) ->
    Len = length(Value),
    if
        Len < 1 ->
            false;
        true ->
            Fun = fun(X) ->
                          if 
                              X < 48 orelse X > 57 -> 
                                false;
                              true -> true
                          end
                  end,
            lists:all(Fun, Value)
    end;
is_dec_integer_string(_Value) ->
    false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Check whether "NNNN", "NN" or any string like this format to be a valid hex string or not.
% For example, "81" and "8A" are both true
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
is_hex_integer_string(Value) when is_list(Value) ->
    case is_integer_string(Value) of
        true ->
            Len = length(string:tokens(Value, "xX")),
            if
                Len == 2 ->
                    true;
                true ->
                    false
            end;
        _ ->
            false
    end;
is_hex_integer_string(_Value) ->
    false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% WordHexString : 32 bit
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
convert_word_hex_string_to_integer(WordHexStr) when is_list(WordHexStr) ->
    case is_hex_integer_string(WordHexStr) of
        true ->
            [_Pre, Value] = string:tokens(WordHexStr, "xX"),
            try
                list_to_integer(Value, 16)
            catch
                _:_ ->
                    16#FFFF
            end;
        _ ->
            0
    end;
convert_word_hex_string_to_integer(_WordHexStr) ->
    0.

get_str_bin_to_bin_list(S) when is_list(S),
                                length(S) > 0 ->
    B = list_to_binary(S),
    get_str_bin_to_bin_list(B);
get_str_bin_to_bin_list(S) when is_binary(S),
                                byte_size(S) > 0 ->
    H = binary:part(S, 0, 1),
    T = binary:part(S, 1, byte_size(S)-1),
    <<HInt:8>> = H,
    [integer_to_list(HInt)|get_str_bin_to_bin_list(T)];
get_str_bin_to_bin_list(_S) ->
    [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% true only when Value is NOT an empty integer string
% For example, "81", "8A" and "0x8B" is true
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
is_integer_string(Value) when is_list(Value) ->
    Len = length(Value),
    if
        Len < 1 ->
            false;
        true ->
            Fun = fun(X) ->
                          if X >= 48 orelse X =< 57 -> 
                                 true;
                             X == 88 orelse X == 120 ->
                                 true;
                             X >= 65 orelse X =< 70 ->
                                 true;
                             X >= 97 orelse X =< 102 ->
                                 true;
                             true -> 
                                 false
                          end
                  end,
            lists:all(Fun, Value)
    end;
is_integer_string(_Value) ->
    false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
is_dec_list(List) when is_list(List),
                       length(List) > 0 ->
    
    Fun = fun(X) ->
                  case is_integer(X) of
                      true -> true;
                      _ -> false
                  end
          end,
    lists:all(Fun, List).

