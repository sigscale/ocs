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
-export([eap_packet/1, eap_pwd/1, eap_pwd_id/1, eap_pwd_commit/1,
			eap_pwd_confirm/1]).

-include("ocs_eap_codec.hrl").

-spec eap_packet(Packet :: binary() | #eap_packet{}) -> #eap_packet{} | binary().
%% @doc Encode or decode an EAP packet transported in the RADIUS `EAP-Message'
%% attribute.
eap_packet(<<Code, Identifier, Length:16, Type, _/binary>> = Packet)
		when size(Packet) >= Length ->
	Data = binary:part(Packet, 5, Length - 5),
	#eap_packet{code = Code, type = Type, identifier = Identifier, data = Data};	
eap_packet(#eap_packet{code = Code, type = Type, identifier = Identifier,
		data = Data} ) when is_integer(Code), is_integer(Type),
			is_integer(Identifier), is_binary(Data) ->
	Length = size(Data) + 5,
	<<Code, Identifier, Length:16, Type, Data/binary>>.

-spec eap_pwd(Packet :: binary() | #eap_pwd{}) -> #eap_pwd{} | binary().
%% @doc Encode or Decode an EAP-PWD-Header packet transported in the
%% RADIUS `EAP-Message' attribute.
%%
%% RFC-5931 3.1
eap_pwd(#eap_pwd{length = true, more = true, pwd_exch = id, data = D } = Packet) ->
	TLen = Packet#eap_pwd.tot_length,
	<<1, 1, 1, TLen, D/binary>>;
eap_pwd(#eap_pwd{length = true, more = true, pwd_exch = commit, data = D } = Packet) ->
	TLen = Packet#eap_pwd.tot_length,
	<<1, 1, 2, TLen, D/binary>>;
eap_pwd(#eap_pwd{length = true, more = true, pwd_exch = confirm, data = D } = Packet) ->
	TLen = Packet#eap_pwd.tot_length,
	<<1, 1, 3, TLen, D/binary>>;
eap_pwd(#eap_pwd{length = true, more = false, pwd_exch = id, data = D } = Packet) ->
	TLen = Packet#eap_pwd.tot_length,
	<<1, 0, 1, TLen, D/binary>>;
eap_pwd(#eap_pwd{length = true, more = false, pwd_exch = commit, data = D } = Packet) ->
	TLen = Packet#eap_pwd.tot_length,
	<<1, 0, 2, TLen, D/binary>>;
eap_pwd(#eap_pwd{length = true, more = false, pwd_exch = confirm, data = D } = Packet) ->
	TLen = Packet#eap_pwd.tot_length,
	<<1, 0, 3, TLen, D/binary>>;
eap_pwd(#eap_pwd{length = false, more = true, pwd_exch = id, data = D } = _Packet) ->
	<<0, 1, 1, D/binary>>;
eap_pwd(#eap_pwd{length = false, more = true, pwd_exch = commit, data = D } = _Packet) ->
	<<0, 1, 2, D/binary>>;
eap_pwd(#eap_pwd{length = false, more = true, pwd_exch = confirm, data = D } = _Packet) ->
	<<0, 1, 3, D/binary>>;
eap_pwd(#eap_pwd{length = false, more = false, pwd_exch = id, data = D } = _Packet) ->
	<<0, 0, 1, D/binary>>;
eap_pwd(#eap_pwd{length = false, more = false, pwd_exch = commit, data = D } = _Packet) ->
	<<0, 0, 2, D/binary>>;
eap_pwd(#eap_pwd{length = false, more = false, pwd_exch = confirm, data = D } = _Packet) ->
	<<0, 0, 3, D/binary>>;
eap_pwd(<<1, 1, 1, TotLength, Payload/binary>>) ->
	#eap_pwd{length = true, more = true, pwd_exch = id,
			tot_length = TotLength, data = Payload};
eap_pwd(<<1, 1, 2, TotLength, Payload/binary>>) ->
	#eap_pwd{length = true, more = true, pwd_exch = commit,
			tot_length = TotLength, data = Payload};
eap_pwd(<<1, 1, 3, TotLength, Payload/binary>>) ->
	#eap_pwd{length = true, more = true, pwd_exch = confirm,
			tot_length = TotLength, data = Payload};
eap_pwd(<<0, 1, 1, Payload/binary>>) ->
	#eap_pwd{length = false, more = true, pwd_exch = id,
			data = Payload};
eap_pwd(<<0, 1, 2, Payload/binary>>) ->
	#eap_pwd{length = false, more = true, pwd_exch = confirm,
			data = Payload};
eap_pwd(<<0, 1, 3, Payload/binary>>) ->
	#eap_pwd{length = false, more = true, pwd_exch = confirm,
			data = Payload};
eap_pwd(<<0, 0, 1, Payload/binary>>) ->
	#eap_pwd{length = false, more = false, pwd_exch = id,
			data = Payload};
eap_pwd(<<0, 0, 2, Payload/binary>>) ->
	#eap_pwd{length = false, more = false, pwd_exch = commit,
			data = Payload};
eap_pwd(<<0, 0, 3, Payload/binary>>) ->
	#eap_pwd{length = false, more = false, pwd_exch = confirm,
			data = Payload}.

-spec eap_pwd_id(Packet :: binary() | #eap_pwd_id{}) -> #eap_pwd_id{} | binary().
%% @doc Encode or Decode `EAP-pwd-ID'
%%
%% RFC-5931 3.2.1
%% Comprise the Ciphersuite included in the calculation of the
%% peer's and server's confirm messages
eap_pwd_id(<<GDesc:16, RanFun, PRF, Token:4/binary, PWDPrep, BinId/binary>>) ->
	Prep = case PWDPrep of
		0 -> none;
		1 -> rfc2759;
		3 -> saslprep
	end,
	Id = binary_to_list(BinId),
	#eap_pwd_id{
		group_desc = GDesc,
		random_fun = RanFun,
		prf = PRF,
		token = Token,
		pwd_prep = Prep,
		identity = Id};
eap_pwd_id(#eap_pwd_id{group_desc = GDesc, random_fun = RanFun, prf = PRF,
		token = Token, pwd_prep = PWDPrep, identity = ListID}) ->
	Prep = case PWDPrep of
		none -> 0;
		rfc2759 -> 1;
		saslprep -> 3
	end,
	ID = list_to_binary(ListID),
	<<GDesc:16, RanFun, PRF, Token/binary, Prep, ID/binary>>.

-spec eap_pwd_commit(Packet :: binary() | #eap_pwd_commit{}) -> #eap_pwd_commit{} | binary().
%% @doc Encode or Decode `EAP-pwd-commit'
%%
%%RFC-5931 3.2.2
%% Element, Scalar are generated by server (in EAP-PWD-Commit/Request) and 
%% peer (in EAP-PWD-Commit/Response)
eap_pwd_commit(<<Element:64/binary, Scalar:32/binary>>) ->
	#eap_pwd_commit{element = Element, scalar = Scalar};
eap_pwd_commit(#eap_pwd_commit{element = Element, scalar = Scalar}) ->
	<<Element:64/binary, Scalar:32/binary>>.

-spec eap_pwd_confirm(Packet :: binary() | #eap_pwd_confirm{}) -> 
			#eap_pwd_confirm{} | binary().
%% @doc Encode or Decode `EAP-pwd-confirm'
%%
%% RFC-5931 3.2.3
eap_pwd_confirm(<<Confirm/binary>>) ->
	#eap_pwd_confirm{confirm = Confirm};
eap_pwd_confirm(#eap_pwd_confirm{confirm = Confirm}) ->
	<<Confirm/binary>>.
