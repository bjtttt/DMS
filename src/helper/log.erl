%%%
%%% log.erl
%%%

-module(log).

-include("../../include/header.hrl").

-export([loginfo/1, 
        loginfo/2, 
        loginfo/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
loginfo(Data) when is_binary(Data) ->
    loginfo(Data, 1);
loginfo(Data) when is_list(Data) ->
    loginfo(Data, 1);
loginfo(Data) ->
	loginfo(Data, 1).
loginfo(Data, DispLog) when is_binary(Data),
							is_integer(DispLog) ->
    try
		if
			DispLog =:= 1 ->
				error_logger:info_msg(binary_to_list(Data))
		end
    catch
        Oper:Msg ->
			if
				DispLog =:= 1 ->
					error_logger:error_msg("loginfo exception : ~p : ~p", [Oper, Msg])
			end
    end
loginfo(Data, DispLog) when is_list(Data),
							is_integer(DispLog) ->
    try
		if
			DispLog =:= 1 ->
				error_logger:info_msg(Data)
		end
    catch
        Oper:Msg ->
			if
				DispLog =:= 1 ->
					error_logger:error_msg("loginfo exception : ~p : ~p", [Oper, Msg])
			end
    end;
loginfo(_Data, DispLog) ->
	if
		DispLog =:= 1 ->
			error_logger:error_msg("loginfo fails : no binary or list")
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Data is a list, for example : [], [Msg] or [Msg1, Msg2]
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
loginfo(Format, Data) when is_string(Format),
						   is_binary(Data) ->
	loginfo(Format, Data, 1);
loginfo(Format, Data) when is_string(Format),
						   is_list(Data) ->
	loginfo(Format, Data, 1);
loginfo(_Format, _Data) ->
	loginfo(_Format, _Data, 1);
loginfo(Format, Data, DispLog) when is_string(Format),
									is_binary(Data) ->
    try
		if
			DispLog =:= 1 ->
				error_logger:info_msg(Format, binary_to_list(Data))
		end
    catch
        Oper:Msg ->
			if
				DispLog =:= 1 ->
					error_logger:error_msg("loginfo exception : ~p : ~p", [Oper, Msg])
			end
    end;
loginfo(Format, Data, DispLog) when is_string(Format),
									is_list(Data) ->
    try
		if
			DispLog =:= 1 ->
				error_logger:info_msg(Format, Data)
		end
    catch
        Oper:Msg ->
			if
				DispLog =:= 1 ->
					error_logger:error_msg("loginfo exception : ~p : ~p", [Oper, Msg])
			end
    end;
loginfo(_Format, _Data, DispLog) ->
	if
		DispLog =:= 1 ->
			error_logger:error_msg("loginfo fails : no binary or list")
	end

