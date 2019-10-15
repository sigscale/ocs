%%% ocs_eap_codec.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 - 2017 SigScale Global Inc.
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
%%% @doc This library module implements encoding and decoding (CODEC)
%%% 	functions for the Extensible Authentication Protocol (EAP) in the
%%% 	{@link //ocs. ocs} application.
%%%
%%% @reference <a href="http://tools.ietf.org/html/rfc3748">
%%% 	RFC3748 - Extensible Authentication Protocol (EAP)</a>
%%%
%%% @reference <a href="http://tools.ietf.org/html/rfc5931">
%%% 	RFC5931 - EAP Authentication Using Only a Password</a>
%%%
%%% @reference <a href="http://tools.ietf.org/html/rfc4187">
%%% 	RFC4187 - EAP Method for 3rd Generation
%%% 	Authentication and Key Agreement (EAP-AKA)</a>
%%%
%%% @reference <a href="http://tools.ietf.org/html/rfc5448">
%%% 	RFC5448 - Improved EAP Method for 3rd Generation
%%% 	Authentication and Key Agreement (EAP-AKA')</a>
%%%
-module(ocs_eap_codec).
-copyright('Copyright (c) 2016 - 2017 SigScale Global Inc.').

%% export the ocs public API
-export([eap_packet/1, eap_pwd/1, eap_pwd_id/1, eap_pwd_commit/1,
			eap_ttls/1, eap_aka/1]).
-export([aka_clear_mac/1, aka_set_mac/2]).

-include("ocs_eap_codec.hrl").

%%----------------------------------------------------------------------
%%  The ocs_eap_codec public API
%%----------------------------------------------------------------------

-spec eap_packet(Packet) -> Result 
	when
		Packet :: binary() | #eap_packet{},
		Result :: #eap_packet{} | binary().
%% @doc Encode or decode an EAP packet.
eap_packet(<<?EapSuccess, Identifier, 4:16>> = _Packet) ->
	#eap_packet{code = success, identifier = Identifier};
eap_packet(<<?EapFailure, Identifier, 4:16>>) ->
	#eap_packet{code = failure, identifier = Identifier};
eap_packet(<<?EapRequest, Identifier, Length:16, Type, _/binary>> = Packet)
		when size(Packet) >= Length ->
	Data = binary:part(Packet, 5, Length - 5),
	#eap_packet{code = request, type = Type, identifier = Identifier, data = Data};
eap_packet(<<?EapResponse, Identifier, Length:16, Type, _/binary>> = Packet)
		when size(Packet) >= Length ->
	Data = binary:part(Packet, 5, Length - 5),
	#eap_packet{code = response, type = Type, identifier = Identifier, data = Data};
eap_packet(#eap_packet{code = success, identifier = Identifier}) ->
	<<?EapSuccess, Identifier, 4:16>>;
eap_packet(#eap_packet{code = failure, identifier = Identifier}) ->
	<<?EapFailure, Identifier, 4:16>>;
eap_packet(#eap_packet{code = request, type = Type,
		identifier = Identifier, data = Data})
		when is_integer(Type), is_integer(Identifier), is_binary(Data) ->
	Length = size(Data) + 5,
	<<?EapRequest, Identifier, Length:16, Type, Data/binary>>;
eap_packet(#eap_packet{code = response, type = Type,
		identifier = Identifier, data = Data})
		when is_integer(Type), is_integer(Identifier), is_binary(Data) ->
	Length = size(Data) + 5,
	<<?EapResponse, Identifier, Length:16, Type, Data/binary>>.

-spec eap_pwd(Packet) -> Result
	when
		Packet :: binary() | #eap_pwd{},
		Result :: #eap_pwd{} | binary().
%% @doc Encode or Decode an EAP-PWD-Header packet transported in the
%% RADIUS `EAP-Message' attribute.
%%
%% RFC-5931 3.1
eap_pwd(#eap_pwd{length = true, more = true, pwd_exch = id, data = D } = Packet) ->
	TLen = Packet#eap_pwd.tot_length,
	<<1:1, 1:1, 1:6, TLen:16, D/binary>>;
eap_pwd(#eap_pwd{length = true, more = true, pwd_exch = commit, data = D } = Packet) ->
	TLen = Packet#eap_pwd.tot_length,
	<<1:1, 1:1, 2:6, TLen:16, D/binary>>;
eap_pwd(#eap_pwd{length = true, more = true, pwd_exch = confirm, data = D } = Packet) ->
	TLen = Packet#eap_pwd.tot_length,
	<<1:1, 1:1, 3:6, TLen:16, D/binary>>;
eap_pwd(#eap_pwd{length = true, more = false, pwd_exch = id, data = D } = Packet) ->
	TLen = Packet#eap_pwd.tot_length,
	<<1:1, 0:1, 1:6, TLen:16, D/binary>>;
eap_pwd(#eap_pwd{length = true, more = false, pwd_exch = commit, data = D } = Packet) ->
	TLen = Packet#eap_pwd.tot_length,
	<<1:1, 0:1, 2:6, TLen:16, D/binary>>;
eap_pwd(#eap_pwd{length = true, more = false, pwd_exch = confirm, data = D } = Packet) ->
	TLen = Packet#eap_pwd.tot_length,
	<<1:1, 0:1, 3:6, TLen:16, D/binary>>;
eap_pwd(#eap_pwd{length = false, more = true, pwd_exch = id, data = D } = _Packet) ->
	<<0:1, 1:1, 1:6, D/binary>>;
eap_pwd(#eap_pwd{length = false, more = true, pwd_exch = commit, data = D } = _Packet) ->
	<<0:1, 1:1, 2:6, D/binary>>;
eap_pwd(#eap_pwd{length = false, more = true, pwd_exch = confirm, data = D } = _Packet) ->
	<<0:1, 1:1, 3:6, D/binary>>;
eap_pwd(#eap_pwd{length = false, more = false, pwd_exch = id, data = D } = _Packet) ->
	<<0:1, 0:1, 1:6, D/binary>>;
eap_pwd(#eap_pwd{length = false, more = false, pwd_exch = commit, data = D } = _Packet) ->
	<<0:1, 0:1, 2:6, D/binary>>;
eap_pwd(#eap_pwd{length = false, more = false, pwd_exch = confirm, data = D } = _Packet) ->
	<<0:1, 0:1, 3:6, D/binary>>;
eap_pwd(<<1:1, 1:1, 1:6, TotLength:16, Payload/binary>>) ->
	#eap_pwd{length = true, more = true, pwd_exch = id,
			tot_length = TotLength, data = Payload};
eap_pwd(<<1:1, 1:1, 2:6, TotLength:16, Payload/binary>>) ->
	#eap_pwd{length = true, more = true, pwd_exch = commit,
			tot_length = TotLength, data = Payload};
eap_pwd(<<1:1, 1:1, 3:6, TotLength:16, Payload/binary>>) ->
	#eap_pwd{length = true, more = true, pwd_exch = confirm,
			tot_length = TotLength, data = Payload};
eap_pwd(<<0:1, 1:1, 1:6, Payload/binary>>) ->
	#eap_pwd{length = false, more = true, pwd_exch = id,
			data = Payload};
eap_pwd(<<0:1, 1:1, 2:6, Payload/binary>>) ->
	#eap_pwd{length = false, more = true, pwd_exch = confirm,
			data = Payload};
eap_pwd(<<0:1, 1:1, 3:6, Payload/binary>>) ->
	#eap_pwd{length = false, more = true, pwd_exch = confirm,
			data = Payload};
eap_pwd(<<0:1, 0:1, 1:6, Payload/binary>>) ->
	#eap_pwd{length = false, more = false, pwd_exch = id,
			data = Payload};
eap_pwd(<<0:1, 0:1, 2:6, Payload/binary>>) ->
	#eap_pwd{length = false, more = false, pwd_exch = commit,
			data = Payload};
eap_pwd(<<0:1, 0:1, 3:6, Payload/binary>>) ->
	#eap_pwd{length = false, more = false, pwd_exch = confirm,
			data = Payload}.

-spec eap_pwd_id(Packet) -> Result
	when
		Packet :: binary() | #eap_pwd_id{},
		Result :: #eap_pwd_id{} | binary().
%% @doc Encode or Decode `EAP-pwd-ID'
%%
%% RFC-5931 3.2.1
%% Comprise the Ciphersuite included in the calculation of the
%% peer's and server's confirm messages
eap_pwd_id(<<GDesc:16, RanFun, PRF, Token:4/binary, 0, Identity/binary>>) ->
	#eap_pwd_id{group_desc = GDesc, random_fun = RanFun, prf = PRF,
		token = Token, pwd_prep = none, identity = Identity};
eap_pwd_id(<<GDesc:16, RanFun, PRF, Token:4/binary, 1, Identity/binary>>) ->
	#eap_pwd_id{group_desc = GDesc, random_fun = RanFun, prf = PRF,
		token = Token, pwd_prep = rfc2759, identity = Identity};
eap_pwd_id(<<GDesc:16, RanFun, PRF, Token:4/binary, 2, Identity/binary>>) ->
	#eap_pwd_id{group_desc = GDesc, random_fun = RanFun, prf = PRF,
		token = Token, pwd_prep = saslprep, identity = Identity};
eap_pwd_id(#eap_pwd_id{group_desc = GDesc, random_fun = RanFun, prf = PRF,
		token = Token, pwd_prep = none, identity = Identity})
		when size(Token) == 4, is_binary(Identity) ->
	<<GDesc:16, RanFun, PRF, Token/binary, 0, Identity/binary>>;
eap_pwd_id(#eap_pwd_id{group_desc = GDesc, random_fun = RanFun, prf = PRF,
		token = Token, pwd_prep = rfc2759, identity = Identity})
		when size(Token) == 4, is_binary(Identity) ->
	<<GDesc:16, RanFun, PRF, Token/binary, 1, Identity/binary>>;
eap_pwd_id(#eap_pwd_id{group_desc = GDesc, random_fun = RanFun, prf = PRF,
		token = Token, pwd_prep = saslprep, identity = Identity})
		when size(Token) == 4, is_binary(Identity) ->
	<<GDesc:16, RanFun, PRF, Token/binary, 2, Identity/binary>>.

-spec eap_pwd_commit(Packet) -> Result 
	when
		Packet :: binary() | #eap_pwd_commit{},
		Result :: #eap_pwd_commit{} | binary().
%% @doc Encode or Decode `EAP-pwd-commit'
%%
%%RFC-5931 3.2.2
%% Element, Scalar are generated by server (in EAP-PWD-Commit/Request) and
%% peer (in EAP-PWD-Commit/Response)
eap_pwd_commit(<<Element:64/binary, Scalar:32/binary>>) ->
	#eap_pwd_commit{element = Element, scalar = Scalar};
eap_pwd_commit(#eap_pwd_commit{element = Element, scalar = Scalar}) ->
	<<Element:64/binary, Scalar:32/binary>>.

-spec eap_ttls(Packet) -> Result
	when
		Packet :: binary() | #eap_ttls{},
		Result :: #eap_ttls{} | binary().
%% @doc Encode or Decode `EAP-TTLS' packet
%%
%% RFC-5281 9.1
eap_ttls(#eap_ttls{message_len = undefined, more = false, start = false,
		version = Version, data = Data}) when is_integer(Version) ->
	<<0:1, 0:1, 0:1, 0:2, Version:3, Data/binary>>;
eap_ttls(#eap_ttls{message_len = undefined, more = false, start = true,
		version = Version, data = Data}) when is_integer(Version) ->
	<<0:1, 0:1, 1:1, 0:2, Version:3, Data/binary>>;
eap_ttls(#eap_ttls{message_len = undefined, more = true, start = false,
		version = Version, data = Data}) when is_integer(Version) ->
	<<0:1, 1:1, 0:1, 0:2, Version:3, Data/binary>>;
eap_ttls(#eap_ttls{message_len = undefined, more = true, start = true,
		version = Version, data = Data}) when is_integer(Version) ->
	<<0:1, 1:1, 1:1, 0:2, Version:3, Data/binary>>;
eap_ttls(#eap_ttls{message_len = Length, more = true, start = false,
		version = Version, data = Data})
		when is_integer(Version), is_integer(Length) ->
	<<1:1, 1:1, 0:1, 0:2, Version:3, Length:32, Data/binary>>;
eap_ttls(#eap_ttls{message_len = Length, more = true, start = true,
		version = Version, data = Data})
		when is_integer(Version), is_integer(Length) ->
	<<1:1, 1:1, 1:1, 0:2, Version:3, Length:32, Data/binary>>;
eap_ttls(#eap_ttls{message_len = Length, more = false, start = false,
		version = Version, data = Data})
		when is_integer(Version), is_integer(Length) ->
	<<1:1, 0:1, 0:1, 0:2, Version:3, Length:32, Data/binary>>;
eap_ttls(<<0:1, 0:1, 0:1, _:2, Version:3, Data/binary>>) ->
	#eap_ttls{version = Version, data = Data};
eap_ttls(<<0:1, 0:1, 1:1, _:2, Version:3, Data/binary>>) ->
	#eap_ttls{start = true, version = Version, data = Data};
eap_ttls(<<0:1, 1:1, 0:1, _:2, Version:3, Data/binary>>) ->
	#eap_ttls{more = true, version = Version, data = Data};
eap_ttls(<<0:1, 1:1, 1:1, _:2, Version:3, Data/binary>>) ->
	#eap_ttls{more = true, start = true, version = Version, data = Data};
eap_ttls(<<1:1, 0:1, 0:1, _:2, Version:3, Length:32, Data/binary>>) ->
	#eap_ttls{version = Version, message_len = Length, data = Data};
eap_ttls(<<1:1, 1:1, 0:1, _:2, Version:3, Length:32, Data/binary>>) ->
	#eap_ttls{more = true, version = Version, message_len = Length, data = Data};
eap_ttls(<<1:1, 1:1, 1:1, _:2, Version:3, Length:32, Data/binary>>) ->
	#eap_ttls{more = true, start = true, version = Version,
			message_len = Length, data = Data}.

-spec eap_aka(Message) -> Message
	when
		Message :: binary()
				| #eap_aka_identity{}
				| #eap_aka_challenge{}
				| #eap_aka_reauthentication{}
				| #eap_aka_notification{}
				| #eap_aka_authentication_reject{}
				| #eap_aka_synchronization_failure{}
				| #eap_aka_client_error{}.
%% @doc Encode or decode an EAP-AKA message.
%%
%% RFC4187 section 8.1
%%
eap_aka(#eap_aka_challenge{} = Message) ->
	eap_aka_challenge(Message);
eap_aka(#eap_aka_authentication_reject{} = Message) ->
	eap_aka_authentication_reject(Message);
eap_aka(#eap_aka_synchronization_failure{} = Message) ->
	eap_aka_synchronization_failure(Message);
eap_aka(#eap_aka_identity{} = Message) ->
	eap_aka_identity(Message);
eap_aka(#eap_aka_notification{} = Message) ->
	eap_aka_notification(Message);
eap_aka(#eap_aka_reauthentication{} = Message) ->
	eap_aka_reauthentication(Message);
eap_aka(#eap_aka_client_error{} = Message) ->
	eap_aka_client_error(Message);
eap_aka(<<1, _:16, Attributes/binary>>) ->
	F = fun(?AT_RAND, Rand, Acc) ->
				Acc#eap_aka_challenge{rand = Rand};
			(?AT_AUTN, Autn, Acc) ->
				Acc#eap_aka_challenge{autn = Autn};
			(?AT_KDF, KDFs, Acc) ->
				Acc#eap_aka_challenge{kdf = KDFs};
			(?AT_KDF_INPUT, Name, Acc) ->
				Acc#eap_aka_challenge{network = Name};
			(?AT_MAC, Mac, Acc) ->
				Acc#eap_aka_challenge{mac = Mac};
			(?AT_RESULT_IND, ResultInd, Acc) ->
				Acc#eap_aka_challenge{result_ind = ResultInd};
			(?AT_RES, Res, Acc) ->
				Acc#eap_aka_challenge{res = Res};
			(?AT_CHECKCODE, Code, Acc) ->
				Acc#eap_aka_challenge{checkcode = Code};
			(?AT_IV, Iv, Acc) ->
				Acc#eap_aka_challenge{iv = Iv};
			(?AT_ENCR_DATA, EncrData, Acc) ->
				Acc#eap_aka_challenge{encr_data = EncrData}
	end,
	maps:fold(F, #eap_aka_challenge{}, aka_attr(Attributes));
eap_aka(<<2, _:16, _Attributes/binary>>) ->
	#eap_aka_authentication_reject{};
eap_aka(<<4, _:16, Attributes/binary>>) ->
	F = fun(?AT_AUTS, Auts, Acc) ->
				Acc#eap_aka_synchronization_failure{auts = Auts}
	end,
	maps:fold(F, #eap_aka_synchronization_failure{}, aka_attr(Attributes));
eap_aka(<<5, _:16, Attributes/binary>>) ->
	F = fun(?AT_PERMANENT_ID_REQ, PermanentIdReq, Acc) ->
				Acc#eap_aka_identity{permanent_id_req = PermanentIdReq};
			(?AT_FULLAUTH_ID_REQ, FullAuthReq, Acc) ->
				Acc#eap_aka_identity{fullauth_id_req = FullAuthReq};
			(?AT_ANY_ID_REQ, AnyIdReq, Acc) ->
				Acc#eap_aka_identity{any_id_req = AnyIdReq};
			(?AT_IDENTITY, Identity, Acc) ->
				Acc#eap_aka_identity{identity = Identity}
	end,
	maps:fold(F, #eap_aka_identity{}, aka_attr(Attributes));
eap_aka(<<12, _:16, Attributes/binary>>) ->
	F = fun(?AT_NOTIFICATION, Notification, Acc) ->
				Acc#eap_aka_notification{notification = Notification};
			(?AT_MAC, Mac, Acc) ->
				Acc#eap_aka_notification{mac = Mac};
			(?AT_IV, Iv, Acc) ->
				Acc#eap_aka_notification{iv = Iv};
			(?AT_ENCR_DATA, EncrData, Acc) ->
				Acc#eap_aka_notification{encr_data = EncrData}
	end,
	maps:fold(F, #eap_aka_notification{}, aka_attr(Attributes));
eap_aka(<<13, _:16, Attributes/binary>>) ->
	F = fun(?AT_MAC, Mac, Acc) ->
				Acc#eap_aka_reauthentication{mac = Mac};
			(?AT_RESULT_IND, ResultInd, Acc) ->
				Acc#eap_aka_reauthentication{result_ind = ResultInd};
			(?AT_CHECKCODE, Code, Acc) ->
				Acc#eap_aka_reauthentication{checkcode = Code};
			(?AT_IV, Iv, Acc) ->
				Acc#eap_aka_reauthentication{iv = Iv};
			(?AT_ENCR_DATA, EncrData, Acc) ->
				Acc#eap_aka_reauthentication{encr_data = EncrData}
	end,
	maps:fold(F, #eap_aka_reauthentication{}, aka_attr(Attributes));
eap_aka(<<14, _:16, Attributes/binary>>) ->
	F = fun(?AT_AUTS, Auts, Acc) ->
				Acc#eap_aka_synchronization_failure{auts = Auts}
	end,
	maps:fold(F, #eap_aka_synchronization_failure{}, aka_attr(Attributes)).

-spec aka_clear_mac(EapMessage) -> EapMessage
	when
		EapMessage :: binary().
%% @doc Zero out the EAP-AKA' message authentication code (MAC).
aka_clear_mac(EapMessage) when is_binary(EapMessage) ->
	P1 = aka_find_mac(EapMessage),
	B1 = binary_part(EapMessage, 0, P1),
	P2 = P1 + 16,
	B2 = binary_part(EapMessage, P2, size(EapMessage) - P2),
	<<B1/bytes, 0:128, B2/bytes>>.

-spec aka_set_mac(MAC, EapMessage) -> EapMessage
	when
		MAC :: binary(),
		EapMessage :: binary().
%% @doc Overwrite value of the EAP-AKA' message authentication code (MAC).
aka_set_mac(MAC, EapMessage) when size(MAC) =:= 16, is_binary(EapMessage) ->
	P1 = aka_find_mac(EapMessage),
	B1 = binary_part(EapMessage, 0, P1),
	P2 = P1 + 16,
	B2 = binary_part(EapMessage, P2, size(EapMessage) - P2),
	<<B1/bytes, MAC/bytes, B2/bytes>>.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec aka_find_mac(EapMessage) -> Pos
	when
		EapMessage :: binary(),
		Pos :: pos_integer().
%% @doc Find start of EAP-AKA' message authentication code (MAC).
%% @hidden
aka_find_mac(EapMessage) ->
	P = 8,
	aka_find_mac(binary_part(EapMessage, P, 2), P, EapMessage).
%% @hidden
aka_find_mac(<<?AT_MAC, 5>>, P, _) ->
	P + 4;
aka_find_mac(<<_, L>>, P, S) ->
	P1 = P + (L * 4),
	aka_find_mac(binary_part(S, P1, 2), P1, S).

-spec aka_attr(Attributes) -> Attributes
	when
		Attributes :: map() | binary().
%% @doc Encode or decode EAP-AKA attributes.
%% @hidden
aka_attr(Attributes) when is_binary(Attributes)->
	aka_attr(Attributes, #{});
aka_attr(Attributes) when is_map(Attributes)->
	list_to_binary(maps:fold(fun aka_attr/3, <<>>, Attributes)).
%% @hidden
aka_attr(<<?AT_RAND, 5, _:16, Rand:16/bytes, Rest/bytes>>, Acc) ->
	aka_attr(Rest, Acc#{?AT_RAND => Rand});
aka_attr(<<?AT_AUTN, 5, _:16, Autn:16/bytes, Rest/bytes>>, Acc) ->
	aka_attr(Rest, Acc#{?AT_AUTN => Autn});
aka_attr(<<?AT_RES, L1, _/bytes>> = B, Acc) ->
	L2 = (L1 * 4) - 4,
	<<_:16, L3:16, Data:L2/bytes, Rest/bytes>> = B,
	<<Res:L3/bits, _/bits>> = Data,
	aka_attr(Rest, Acc#{?AT_RES => Res});
aka_attr(<<?AT_AUTS, 4, Auts:14/bytes, Rest/bytes>>, Acc) ->
	aka_attr(Rest, Acc#{?AT_AUTS => Auts});
aka_attr(<<?AT_PADDING, L1, _/bytes>> = B, Acc) ->
	L2 = (L1 * 4) - 2,
	<<_:16, _:L2/bytes, Rest/bytes>> = B,
	aka_attr(Rest, Acc);
aka_attr(<<?AT_PERMANENT_ID_REQ, 1, _:16, Rest/bytes>>, Acc) ->
	aka_attr(Rest, Acc#{?AT_PERMANENT_ID_REQ => true});
aka_attr(<<?AT_MAC, 5, _:16, Mac:16/bytes, Rest/bytes>>, Acc) ->
	aka_attr(Rest, Acc#{?AT_MAC => Mac});
aka_attr(<<?AT_NOTIFICATION, 1, Code:16, Rest/bytes>>, Acc) ->
	aka_attr(Rest, Acc#{?AT_NOTIFICATION => Code});
aka_attr(<<?AT_ANY_ID_REQ, 1, _:16, Rest/bytes>>, Acc) ->
	aka_attr(Rest, Acc#{?AT_ANY_ID_REQ => true});
aka_attr(<<?AT_IDENTITY, L1, _/bytes>> = B, Acc) ->
	L2 = (L1 * 4) - 4,
	<<_:16, L3:16, Data:L2/bytes, Rest/bytes>> = B,
	<<Identity:L3/bytes, _/bytes>> = Data,
	aka_attr(Rest, Acc#{?AT_IDENTITY => Identity});
aka_attr(<<?AT_FULLAUTH_ID_REQ, 1, _:16, Rest/bytes>>, Acc) ->
	aka_attr(Rest, Acc#{?AT_FULLAUTH_ID_REQ => true});
aka_attr(<<?AT_COUNTER, 1, Counter:16, Rest/bytes>>, Acc) ->
	aka_attr(Rest, Acc#{?AT_COUNTER => Counter});
aka_attr(<<?AT_COUNTER_TOO_SMALL, 1, _:16, Rest/bytes>>, Acc) ->
	aka_attr(Rest, Acc#{?AT_COUNTER_TOO_SMALL => true});
aka_attr(<<?AT_NONCE_S, 5, _:16, Nonce:16/bytes, Rest/bytes>>, Acc) ->
	aka_attr(Rest, Acc#{?AT_NONCE_S => Nonce});
aka_attr(<<?AT_CLIENT_ERROR_CODE, 1, Code:16, Rest/bytes>>, Acc) ->
	aka_attr(Rest, Acc#{?AT_CLIENT_ERROR_CODE => Code});
aka_attr(<<?AT_KDF_INPUT, L1, _/bytes>> = B, Acc) ->
	L2 = (L1 * 4) - 4,
	<<_:16, L3:16, Data:L2/bytes, Rest/bytes>> = B,
	<<Name:L3/bytes, _/bytes>> = Data,
	aka_attr(Rest, Acc#{?AT_KDF_INPUT => Name});
aka_attr(<<?AT_KDF, 1, KDF:16, Rest/bytes>>, #{?AT_KDF := KDFs} = Acc) ->
	aka_attr(Rest, Acc#{?AT_KDF => [KDF | KDFs]});
aka_attr(<<?AT_KDF, 1, KDF:16, Rest/bytes>>, Acc) ->
	aka_attr(Rest, Acc#{?AT_KDF => [KDF]});
aka_attr(<<?AT_IV, 5, _:16, Iv:16/bytes, Rest/bytes>>, Acc) ->
	aka_attr(Rest, Acc#{?AT_IV => Iv});
aka_attr(<<?AT_ENCR_DATA, L1, _/bytes>> = B, Acc) ->
	L2 = (L1 * 4) - 4,
	<<_:32, EncrData:L2/bytes, Rest/bytes>> = B,
	aka_attr(Rest, Acc#{?AT_ENCR_DATA => EncrData});
aka_attr(<<?AT_NEXT_PSEUDONYM, L1, _/bytes>> = B, Acc) ->
	L2 = (L1 * 4) - 4,
	<<_:16, L3:16, Data:L2/bytes, Rest/bytes>> = B,
	<<NextPseudonym:L3/bytes, _/bytes>> = Data,
	aka_attr(Rest, Acc#{?AT_NEXT_PSEUDONYM => NextPseudonym});
aka_attr(<<?AT_NEXT_REAUTH_ID, L1, _/bytes>> = B, Acc) ->
	L2 = (L1 * 4) - 4,
	<<_:16, L3:16, Data:L2/bytes, Rest/bytes>> = B,
	<<NextReauthId:L3/bytes, _/bytes>> = Data,
	aka_attr(Rest, Acc#{?AT_NEXT_REAUTH_ID => NextReauthId});
aka_attr(<<?AT_CHECKCODE, 1, _:16, Rest/bytes>>, Acc) ->
	aka_attr(Rest, Acc#{?AT_CHECKCODE => <<>>});
aka_attr(<<?AT_CHECKCODE, 9, _:16, CheckCode:32/bytes, Rest/bytes>>, Acc) ->
	aka_attr(Rest, Acc#{?AT_CHECKCODE => CheckCode});
aka_attr(<<?AT_RESULT_IND, 1, _:16, Rest/bytes>>, Acc) ->
	aka_attr(Rest, Acc#{?AT_RESULT_IND=> true});
aka_attr(<<?AT_BIDDING, 1, 1:15, Rest/bytes>>, Acc) ->
	aka_attr(Rest, Acc#{?AT_BIDDING => true});
aka_attr(<<?AT_BIDDING, 1, 0:15, Rest/bytes>>, Acc) ->
	aka_attr(Rest, Acc#{?AT_BIDDING => false});
aka_attr(<<Type, 1, _:16, Rest/bytes>>, Acc) when Type > 127 ->
	aka_attr(Rest, Acc);
aka_attr(<<>>, Acc) ->
	Acc.
%% @hidden
aka_attr(?AT_RAND, Rand, Acc) when size(Rand) == 16 ->
	[<<?AT_RAND, 5, 0:16, Rand/bytes>> | Acc];
aka_attr(?AT_AUTN, Autn, Acc) when size(Autn) == 16 ->
	[<<?AT_AUTN, 5, 0:16, Autn:16/bytes>> | Acc];
aka_attr(?AT_RES, Res, Acc) when bit_size(Res) >= 32, bit_size(Res) =< 128 ->
	L = bit_size(Res),
	Pad = case {L, L rem 32} of
		{L, 0} ->
			0;
		{L, R} ->
			L + 32 - R
	end,
	L1 = ((L + Pad) div 32) + 1,
	[<<?AT_RES, L1, L:16, Res/bits, 0:Pad>> | Acc];
aka_attr(?AT_AUTS, Auts, Acc) when size(Auts) == 14 ->
	[<<?AT_AUTS, 4, Auts/bytes>> |  Acc];
aka_attr(?AT_PADDING, Pad, Acc) when size(Pad) == 2 ->
	[<<?AT_PADDING, 1, Pad/bytes>> | Acc];
aka_attr(?AT_PADDING, Pad, Acc) when size(Pad) == 6 ->
	[<<?AT_PADDING, 2, Pad/bytes>> | Acc];
aka_attr(?AT_PADDING, Pad, Acc) when size(Pad) == 10 ->
	[<<?AT_PADDING, 3, Pad/bytes>> | Acc];
aka_attr(?AT_PERMANENT_ID_REQ, true, Acc) ->
	[<<?AT_PERMANENT_ID_REQ, 1, 0:16>> | Acc];
aka_attr(?AT_PERMANENT_ID_REQ, false, Acc) ->
	Acc;
aka_attr(?AT_MAC, Mac, Acc) when size(Mac) == 16 ->
	[<<?AT_MAC, 5, 0:16, Mac/bytes>> | Acc];
aka_attr(?AT_NOTIFICATION, Code, Acc) when is_integer(Code) ->
	[<<?AT_NOTIFICATION, 1, Code:16>> | Acc];
aka_attr(?AT_ANY_ID_REQ, true, Acc) ->
	[<<?AT_ANY_ID_REQ, 1, 0:16>> | Acc];
aka_attr(?AT_ANY_ID_REQ, false, Acc) ->
	Acc;
aka_attr(A, Identity, Acc) when is_binary(Identity),
		((A == ?AT_IDENTITY) or (A == ?AT_NEXT_PSEUDONYM)
		or (A == ?AT_NEXT_REAUTH_ID)) ->
	L = size(Identity),
	R = L rem 4,
	{L1, Pad} = case {L, R} of
		{L, 0} ->
			{(L div 4) + 1, 0};
		{L, R} ->
			N = 4 - R,
			{(L + N + 4) div 4, N * 8}
	end,
	[<<A, L1, L:16, Identity/bytes, 0:Pad>> | Acc];
aka_attr(?AT_FULLAUTH_ID_REQ, true, Acc) ->
	[<<?AT_FULLAUTH_ID_REQ, 1, 0:16>> | Acc];
aka_attr(?AT_FULLAUTH_ID_REQ, false, Acc) ->
	Acc;
aka_attr(?AT_COUNTER, Counter, Acc) when is_integer(Counter) ->
	[<<?AT_COUNTER, 1, Counter:16>>, Acc];
aka_attr(?AT_COUNTER_TOO_SMALL, true, Acc) ->
	[<<?AT_COUNTER_TOO_SMALL, 1, 0:16>> | Acc];
aka_attr(?AT_COUNTER_TOO_SMALL, false, Acc) ->
	Acc;
aka_attr(?AT_NONCE_S, Nonce, Acc) when size(Nonce) == 16 ->
	[<<?AT_NONCE_S, 5, 0:16, Nonce/bytes>> | Acc];
aka_attr(?AT_CLIENT_ERROR_CODE, Code, Acc) when is_integer(Code) ->
	[<<?AT_CLIENT_ERROR_CODE, 1, Code:16>> | Acc];
aka_attr(?AT_KDF_INPUT, Name, Acc) when is_binary(Name) ->
	L = size(Name),
	R = L rem 4,
	{L1, Pad} = case {L, R} of
		{L, 0} ->
			{(L div 4) + 1, 0};
		{L, R} ->
			N = 4 - R,
			{(L + N + 4) div 4, N * 8}
	end,
	[<<?AT_KDF_INPUT, L1, L:16, Name/bytes, 0:Pad>> | Acc];
aka_attr(?AT_KDF, KDFs, Acc) when is_list(KDFs) ->
	[[<<?AT_KDF, 1, KDF:16>> || KDF <- KDFs] | Acc];
aka_attr(?AT_IV, Iv, Acc) when size(Iv) == 16 ->
	[<<?AT_IV, 5, 0:16, Iv:16/bytes>> | Acc];
aka_attr(?AT_ENCR_DATA, EncrData, Acc) when (size(EncrData) rem 16) == 0 ->
	L = (size(EncrData) div 4) + 1,
	[<<?AT_ENCR_DATA, L, 0:16, EncrData/bytes>> | Acc];
aka_attr(?AT_CHECKCODE, <<>>, Acc) ->
	[<<?AT_CHECKCODE, 1, 0:16>> | Acc];
aka_attr(?AT_CHECKCODE, CheckCode, Acc) when size(CheckCode) == 32 ->
	[<<?AT_CHECKCODE, 9, 0:16, CheckCode/bytes>> | Acc];
aka_attr(?AT_RESULT_IND, true, Acc) ->
	[<<?AT_RESULT_IND, 1, 0:16>> | Acc];
aka_attr(?AT_RESULT_IND, false, Acc) ->
	Acc;
aka_attr(?AT_BIDDING, true, Acc) ->
	[<<?AT_BIDDING, 1, 1:1, 0:15>> | Acc];
aka_attr(?AT_BIDDING, false, Acc) ->
	[<<?AT_BIDDING, 1, 0:1, 0:15>> | Acc].

%% @hidden
eap_aka_challenge(R) ->
	AttrMap = eap_aka_challenge(record_info(fields, eap_aka_challenge), R, #{}),
	AttrBin = aka_attr(AttrMap),
	<<1, 0:16, AttrBin/binary>>.
%% @hidden
eap_aka_challenge([rand | T], #eap_aka_challenge{rand = Rand} = R, Acc)
		when Rand /= undefined ->
	eap_aka_challenge(T, R, Acc#{?AT_RAND => Rand});
eap_aka_challenge([autn | T], #eap_aka_challenge{autn = Autn} = R, Acc)
		when Autn /= undefined ->
	eap_aka_challenge(T, R, Acc#{?AT_AUTN => Autn});
eap_aka_challenge([res | T], #eap_aka_challenge{res = Res} = R, Acc)
		when Res /= undefined ->
	eap_aka_challenge(T, R, Acc#{?AT_RES => Res});
eap_aka_challenge([kdf | T], #eap_aka_challenge{kdf = KDFs} = R, Acc)
		when KDFs /= undefined ->
	eap_aka_challenge(T, R, Acc#{?AT_KDF => KDFs});
eap_aka_challenge([network | T], #eap_aka_challenge{network = Name} = R, Acc)
		when Name /= undefined ->
	eap_aka_challenge(T, R, Acc#{?AT_KDF_INPUT => Name});
eap_aka_challenge([next_pseudonym | T],
		#eap_aka_challenge{next_pseudonym = Identity} = R, Acc)
		when Identity /= undefined ->
	eap_aka_challenge(T, R, Acc#{?AT_NEXT_PSEUDONYM => Identity});
eap_aka_challenge([next_reauth_id | T],
		#eap_aka_challenge{next_reauth_id = Identity} = R, Acc)
		when Identity /= undefined ->
	eap_aka_challenge(T, R, Acc#{?AT_NEXT_REAUTH_ID => Identity});
eap_aka_challenge([iv| T], #eap_aka_challenge{iv = Iv} = R, Acc)
		when Iv /= undefined ->
	eap_aka_challenge(T, R, Acc#{?AT_IV => Iv});
eap_aka_challenge([encr_data | T],
		#eap_aka_challenge{encr_data = EncrData} = R, Acc)
		when EncrData/= undefined ->
	eap_aka_challenge(T, R, Acc#{?AT_ENCR_DATA => EncrData});
eap_aka_challenge([checkcode | T],
		#eap_aka_challenge{checkcode = CheckCode} = R, Acc)
		when CheckCode /= undefined ->
	eap_aka_challenge(T, R, Acc#{?AT_CHECKCODE => CheckCode});
eap_aka_challenge([result_ind | T],
		#eap_aka_challenge{result_ind = ResultInd} = R, Acc)
		when ResultInd /= undefined ->
	eap_aka_challenge(T, R, Acc#{?AT_RESULT_IND => ResultInd});
eap_aka_challenge([mac | T], #eap_aka_challenge{mac = Mac} = R, Acc)
		when Mac /= undefined ->
	eap_aka_challenge(T, R, Acc#{?AT_MAC => Mac});
eap_aka_challenge([_ | T], R, Acc) ->
	eap_aka_challenge(T, R, Acc);
eap_aka_challenge([], _, Acc) ->
	Acc.

%% @hidden
eap_aka_authentication_reject(_R) ->
	<<2, 0:16>>.

%% @hidden
eap_aka_synchronization_failure(#eap_aka_synchronization_failure{auts = Auts}) ->
	AttrBin = aka_attr(#{?AT_AUTS => Auts}),
	<<4, 0:16, AttrBin/binary>>.

%% @hidden
eap_aka_identity(R) ->
	AttrMap = eap_aka_identity(record_info(fields, eap_aka_identity), R, #{}),
	AttrBin = aka_attr(AttrMap),
	<<5, 0:16, AttrBin/binary>>.
%% @hidden
eap_aka_identity([permanent_id_req | T],
		#eap_aka_identity{permanent_id_req = true} = R, Acc) ->
	eap_aka_identity(T, R, Acc#{?AT_PERMANENT_ID_REQ => true});
eap_aka_identity([any_id_req | T],
		#eap_aka_identity{any_id_req = true} = R, Acc) ->
	eap_aka_identity(T, R, Acc#{?AT_PERMANENT_ID_REQ => true});
eap_aka_identity([fullauth_id_req | T],
		#eap_aka_identity{fullauth_id_req = true} = R, Acc) ->
	eap_aka_identity(T, R, Acc#{?AT_FULLAUTH_ID_REQ => true});
eap_aka_identity([identity | T],
		#eap_aka_identity{identity = Identity} = R, Acc)
		when Identity /= undefined ->
	eap_aka_identity(T, R, Acc#{?AT_IDENTITY => Identity});
eap_aka_identity([_ | T], R, Acc) ->
	eap_aka_identity(T, R, Acc);
eap_aka_identity([], _, Acc) ->
	Acc.

%% @hidden
eap_aka_notification(R) ->
	AttrMap = eap_aka_notification(record_info(fields, eap_aka_notification), R, #{}),
	AttrBin = aka_attr(AttrMap),
	<<12, 0:16, AttrBin/binary>>.
%% @hidden
eap_aka_notification([iv | T],
		#eap_aka_notification{iv = Iv} = R, Acc) when Iv /= undefined ->
	eap_aka_notification(T, R, Acc#{?AT_IV => Iv});
eap_aka_notification([encr_data | T],
		#eap_aka_notification{encr_data = EncrData} = R, Acc)
		when EncrData  /= undefined ->
	eap_aka_notification(T, R, Acc#{?AT_ENCR_DATA => EncrData});
eap_aka_notification([mac | T], #eap_aka_notification{mac = Mac} = R, Acc)
		when Mac /= undefined ->
	eap_aka_notification(T, R, Acc#{?AT_MAC => Mac});
eap_aka_notification([counter | T],
		#eap_aka_notification{counter = Counter} = R, Acc)
		when Counter /= undefined ->
	eap_aka_notification(T, R, Acc#{?AT_COUNTER => Counter});
eap_aka_notification([notification | T],
		#eap_aka_notification{notification = Notification} = R, Acc)
		when Notification /= undefined ->
	eap_aka_notification(T, R, Acc#{?AT_NOTIFICATION => Notification});
eap_aka_notification([_ | T], R, Acc) ->
	eap_aka_notification(T, R, Acc);
eap_aka_notification([], _, Acc) ->
	Acc.

%% @hidden
eap_aka_reauthentication(R) ->
	AttrMap = eap_aka_reauthentication(record_info(fields, eap_aka_reauthentication), R, #{}),
	AttrBin = aka_attr(AttrMap),
	<<13, 0:16, AttrBin/binary>>.
%% @hidden
eap_aka_reauthentication([next_reauth_id | T],
		#eap_aka_reauthentication{next_reauth_id = Identity} = R, Acc)
		when Identity /= undefined ->
	eap_aka_reauthentication(T, R, Acc#{?AT_NEXT_REAUTH_ID => Identity});
eap_aka_reauthentication([iv | T], #eap_aka_reauthentication{iv = Iv} = R, Acc)
		when Iv /= undefined ->
	eap_aka_reauthentication(T, R, Acc#{?AT_IV => Iv});
eap_aka_reauthentication([encr_data | T],
		#eap_aka_reauthentication{encr_data = EncrData} = R, Acc)
		when EncrData  /= undefined ->
	eap_aka_reauthentication(T, R, Acc#{?AT_ENCR_DATA => EncrData});
eap_aka_reauthentication([checkcode | T],
		#eap_aka_reauthentication{checkcode = CheckCode} = R, Acc)
		when CheckCode /= undefined ->
	eap_aka_reauthentication(T, R, Acc#{?AT_CHECKCODE => CheckCode});
eap_aka_reauthentication([result_ind | T],
		#eap_aka_reauthentication{result_ind = ResultInd} = R, Acc)
		when ResultInd /= undefined ->
	eap_aka_reauthentication(T, R, Acc#{?AT_RESULT_IND => ResultInd});
eap_aka_reauthentication([mac | T], #eap_aka_reauthentication{mac = Mac} = R, Acc)
		when Mac /= undefined ->
	eap_aka_reauthentication(T, R, Acc#{?AT_MAC => Mac});
eap_aka_reauthentication([counter | T],
		#eap_aka_reauthentication{counter = Counter} = R, Acc)
		when Counter /= undefined ->
	eap_aka_reauthentication(T, R, Acc#{?AT_COUNTER => Counter});
eap_aka_reauthentication([counter_too_small | T],
		#eap_aka_reauthentication{counter_too_small = true} = R, Acc) ->
	eap_aka_reauthentication(T, R, Acc#{?AT_COUNTER_TOO_SMALL => true});
eap_aka_reauthentication([nonce_s | T],
		#eap_aka_reauthentication{nonce_s = NonceS} = R, Acc)
		when NonceS /= undefined ->
	eap_aka_reauthentication(T, R, Acc#{?AT_NONCE_S => NonceS});
eap_aka_reauthentication([_ | T], R, Acc) ->
	eap_aka_reauthentication(T, R, Acc);
eap_aka_reauthentication([], _, Acc) ->
	Acc.

%% @hidden
eap_aka_client_error(#eap_aka_client_error{client_error_code = Code})
		when Code /= undefined ->
	AttrBin = aka_attr(#{?AT_CLIENT_ERROR_CODE => Code}),
	<<14, 0:16, AttrBin/binary>>.

