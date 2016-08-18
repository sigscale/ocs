%%% ocs_eap_codec.erl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 SigScale Global Inc.
%%% @end
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc This library module implements the public API for the
%%% 	{@link //ocs. ocs} application.
%%%
%%% @reference <a href="http://tools.ietf.org/html/rfc3748">
%%% 	RFC3748 - Extensible Authentication Protocol (EAP)</a>
%%%
%%% @reference <a href="http://tools.ietf.org/html/rfc5931">
%%% 	RFC5931 - EAP Authentication Using Only a Password</a>
%%%
-module(ocs_eap_codec).
-copyright('Copyright (c) 2016 SigScale Global Inc.').

%% export the ocs public API
-export([packet/1, pwd/1, payload_id/1]).

-include("ocs_eap_codec.hrl").

-spec packet(Packet :: binary() | #eap_packet{}) -> #eap_packet{} | binary().
%% @doc Encode or decode an EAP packet transported in the RADIUS `EAP-Message'
%% attribute.
packet(<<Code, Identifier, Length:16, _/binary>> = Packet)
		when size(Packet) >= Length ->
	Data = binary:part(Packet, 4, Length - 4),
	#eap_packet{code = Code, identifier = Identifier, data = Data};	
packet(#eap_packet{code = Code, identifier = Identifier,
		data = Data} ) when is_integer(Code), is_integer(Identifier),
		is_binary(Data) ->
	Length = size(Data) + 32,
	<<Code, Identifier, Length:16, Data/binary>>.

-spec pwd(Packet :: binary()) -> #eap_pwd{}.
%% @doc Decode an EAP-PWD-Header packet transported in the RADIUS `EAP-Message'
%% attribute.
%%
%% RFC-5931 3.1
pwd(Packet) ->
	pwd(Packet, #eap_pwd{}).
%% @hidden
pwd(<<Code/binary, T>>, Acc) ->
	case Code of
		?Request ->
			pwd(T, Acc#eap_pwd{code = Code});
		?Response ->
			pwd(T, Acc#eap_pwd{code = Code});
		_ ->
			pwd(T, Acc)
	end;
pwd(<<Identifier/binary, T>>, Acc) ->
	pwd(T, Acc#eap_pwd{identifier = Identifier});
pwd(<<Length/binary, T>>, Acc) ->
	case size(Length) of
		S when S >= 32 ->
			pwd(T, Acc#eap_pwd{length = Length});
		_ ->
			pwd(T, Acc)
	end;
pwd(<<Type/binary, T>>, Acc) ->
	case Type of
		?PWD ->
			pwd(T, Acc#eap_pwd{type = Type});
		_ ->
			pwd(T, Acc)
	end;
pwd(<<LB/binary, T>>, Acc) ->
	pwd(T, Acc#eap_pwd{l_bit = LB});
pwd(<<MB/binary, T>>, Acc) ->
	pwd(T, Acc#eap_pwd{m_bit = MB});
pwd(<<PWDExch/binary, T>>, Acc) ->
	pwd(T, Acc#eap_pwd{pwd_exch = PWDExch});
pwd(<<TotLength/binary, T>>, Acc) ->
	case Acc#eap_pwd.l_bit of
		true ->
			pwd(T, Acc#eap_pwd{tot_length = TotLength});
		false ->
			pwd(T, Acc)
	end;
pwd(<<Payload, T>>, Acc) ->
	pwd(T, Acc#eap_pwd{data = Payload});
pwd(<<>>, Acc) ->
	Acc.

-spec pwd(#eap_pwd{}) -> binary().
%% @doc Encode an eap_pwd record into an EAP-PWD-Header packet transportedi
%% in the RADIUS `EAP-Message' attribute.
%%
%% RFC-5931 3.1
pwd(#eap_pwd{code = C, identifier = I, length = Len, type = Type,
	 l_bit = L,
	 m_bit = M, pwd_exch = P, data = D } = Packet) -> 
	Length = byte_size(<<C/binary, I/binary, Type/binary, 0:16, D/binary>>),
	case L of
		true ->
			#eap_pwd{tot_length = TLen},
			<<C, I, Length, _, L, M, P, TLen, D>>;
		false ->
			<<C, I, Length, _, L, M, P, D>>
	end.

-spec payload_id(Packet :: binary()) -> #eap_pwd_id{}.
%% @doc Decode `EAP-pwd-ID' 
%%
%% RFC-5931 3.2.1
%% Comprise the Ciphersuite included in the calculation of the
%% peer's and server's confirm messages
payload_id(<<GDesc, RanFun, PRF, Token, PWDPrep, Id>>) ->
	#eap_pwd_id{
		group_desc = GDesc,
		random_fun = RanFun,
		prf = PRF,
		token = Token,
		pwd_prep = PWDPrep,
		identity = Identity}.

-spec payload_id(#eap_pwd_id{}) -> binary().
%% @doc Encode `EAP-pwd-ID' 
%%
%% RFC-5931 3.2.1
%% Comprise the Ciphersuite included in the calculation of the
%% peer's and server's confirm messages
payload_id(#eap_pwd_id{groug_id = GID, random_fun = RanFun, prf = PRF,
		token = Token, pwd_prep = PWDPre, identity = ID}) ->
	<<GID, RanFun, PRF, Token, PWDPre, ID>>.
