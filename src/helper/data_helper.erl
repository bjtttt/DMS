%%%
%%% data_helper.erl
%%%

-module(data_helper).

-include("../include/header.hrl").

-export([combine_strings/1,
         combine_strings/2,
         is_string/1,
         is_string_list/1,
         is_dec_list/1,
         convert_list_to_bin/1,
         convert_bin_to_list/1,
         convert_string_to_integer_dec/1,
         force_convert_string_to_integer_dec/1,
         convert_string_to_integer_hex/1,
         force_convert_string_to_integer_hex/1,
         make_sure_n_byte_binary/2,
         integer_to_binary/1,
         integer_to_size_binary/2,
         convert_integer_to_binary_string_list/1,
         integer_list_to_size_binary_list/2,
         float_to_binary/1,
         integer_to_2byte_binary/1,
         convert_bcd_integer/1,
         convert_integer_bcd/1,
         number_list_to_binary/2]).

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
%   Check whether a string is an 10 based integer string or not and convert it
% Parameter :
%       String  : to be checked and converted, "81" is OK but "8A" and "0x8B" are NOT OK
% Return :
%       an 10 based integer :
%       error               :
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
convert_string_to_integer_dec(String) ->
    convert_string_to_integer(String, true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
%   Check whether a string is an 10 based integer string or not and convert it
% Parameter :
%       String  : to be checked and converted, "81" is OK but "8A" and "0x8B" are NOT OK
% Return :
%       an 10 based integer :
%       0                   : when error
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
force_convert_string_to_integer_dec(String) ->
    case convert_string_to_integer_dec(String) of
        error ->
            0;
        Value ->
            Value
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
%   Check whether a string is an 16 based integer string or not and convert it
% Parameter :
%       String  : to be checked and converted, "81", "8A" and "0x8B" are OK
% Return :
%       an 10 based integer :
%       error               :
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
convert_string_to_integer_hex(String) ->
    convert_string_to_integer(String, false).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
%   Check whether a string is an 16 based integer string or not and convert it
% Parameter :
%       String  : to be checked and converted, "81", "8A" and "0x8B" are OK
% Return :
%       an 10 based integer :
%       0                   : when error
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
force_convert_string_to_integer_hex(String) ->
    case convert_string_to_integer_hex(String) of
        error ->
            0;
        Value ->
            Value
    end.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
%   Check whether a string is an integer string or not and convert it
% Parameter :
%       String  : to be checked and converted, "81", "8A" and "0x8B" are OK
%       Base10  : true  -> 10 based
%                 false -> 16 based
% Return :
%       an 10 based integer :
%       error               :
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
convert_string_to_integer(String, Base10) when is_list(String) ->
    Len = length(String),
    if
        Len =< 2 ->
            try
                if
                    Base10 == true ->
                        erlang:list_to_integer(String, 10);
                    true ->
                        erlang:list_to_integer(String, 16)
                end
            catch
                _:_ ->
                    error
            end;
        true ->
            Prefix = lists:sublist(String, 1, 2),
            if
                Prefix == "0x" orelse Prefix == "0X" ->
                    try
                        if
                            Base10 == true ->
                                erlang:list_to_integer(lists:sublist(String, 3, Len - 2), 10);
                            true ->
                                erlang:list_to_integer(lists:sublist(String, 3, Len - 2), 16)
                        end
                    catch
                        _:_ ->
                            error
                    end;
                true ->
                    try
                        if
                            Base10 == true ->
                                erlang:list_to_integer(String, 10);
                            true ->
                                erlang:list_to_integer(String, 16)
                        end
                    catch
                        _:_ ->
                            error
                    end
            end            
    end;
convert_string_to_integer(_String, _Base10) ->
    error.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
%   Check whether a list is an integer list
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Description :
%   Use N bytes to store a binary.
%   For example, make_sure_n_byte_binary(<<"ABC">>, 2) will get <<"BC">>.
%   make_sure_n_byte_binary(<<"ABC">>, 40) will get <<0,0,65,66,67>>
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
make_sure_n_byte_binary(Bin, N) when is_binary(Bin),
                                     N > 0 ->
    Size = bit_size(Bin),
    <<Int:Size>> = Bin,
    <<Int:(N*?LEN_BYTE)>>;
make_sure_n_byte_binary(_Bin, _N) ->
    <<>>.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Parameter     : Integer = 48
% Output        : <<"48">>
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
integer_to_binary(Integer) when is_integer(Integer) ->
    list_to_binary(integer_to_list(Integer));
integer_to_binary(_Integer) ->
    <<>>.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Parameter   : Integer = 48, ByteSize = 2
%%% Output      : <<0, 48>>
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
integer_to_size_binary(Integer, ByteSize) when is_integer(Integer),
                                               is_integer(ByteSize),
                                               ByteSize >= 0 ->
    <<Integer:(ByteSize*8)>>;
integer_to_size_binary(_Integer, _ByteSize) ->
    <<>>.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Input : 5
%   Must >= 0
% Output : 101
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
convert_integer_to_binary_string_list(Int) when is_integer(Int),
                                                Int >= 0 ->
    Bit = Int band 1,
    BitChar = integer_to_list(Bit),
    NewInt = Int bsr 1,
    if
        NewInt > 0 ->
            lists:merge([convert_integer_to_binary_string_list(NewInt), BitChar]);
        true ->
            BitChar
    end;                
convert_integer_to_binary_string_list(Int) ->
    integer_to_list(Int).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Parameter   : IDs = [48, 32], ByteSize = 2
%%% Output      : <<0, 48, 0, 32>>
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
integer_list_to_size_binary_list(IDs, ByteSize) when is_list(IDs),
                                                     length(IDs) > 0,
                                                     is_integer(ByteSize),
                                                     ByteSize >= 0 ->
    [H|T] = IDs,
    case T of
        [] ->
            common:integer_to_size_binary(H, ByteSize);
        _ ->
            list_to_binary([common:integer_to_size_binary(H, ByteSize), integer_list_to_size_binary_list(T, ByteSize)])
    end;
integer_list_to_size_binary_list(_IDs, _ByteSize) ->
    <<>>.

%%%
%%% It is only for this case : 10 -> <<"10">>
%%% Not for 10 -> <<0, 10>> which should use <<10:16>>
%%%
integer_to_2byte_binary(Integer) when is_integer(Integer) ->
    List = integer_to_list(Integer),
    Len = length(List),
    if
        Len > 1 ->
            list_to_binary(List);
        true ->
            list_to_binary(["0", List])
    end;
integer_to_2byte_binary(_Integer) ->
    <<>>.

%%%
%%%
%%%
float_to_binary(Float) ->
    list_to_binary(float_to_list(Float)).

%%%
%%%
%%%
convert_bcd_integer(Number) ->
    (Number div 16) * 10 + (Number rem 16).

convert_integer_bcd(Number) ->
    (Number div 10) * 16 + (Number rem 10).

%%%
%%% Convert number list to binary.
%%% List    : [Num0, Num1, Num2, ...]
%%% NumLen  : Number length
%%% Return  : [<<Num0:NumLen>>, <<Num1:NumLen>>, <<Num2:NumLen>>, ...]
%%%
number_list_to_binary(List, NumLen) ->
    [H|T] = List,
    case T of
        [] ->
            <<H:NumLen>>;
        _ ->
            [<<H:NumLen>>|number_list_to_binary(T, NumLen)]
    end.

