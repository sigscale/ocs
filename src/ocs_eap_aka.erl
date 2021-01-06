%%% ocs_eap_aka.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2019 - 2021 SigScale Global Inc.
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
%%% @doc This library module implements functions for EAP authentication
%%% 	using 3GPP Authentication and Key Agreement (AKA) in the
%%% 	{@link //ocs. ocs} application.
%%%
%%% @reference <a href="http://tools.ietf.org/html/rfc4187">
%%% 	RFC4187 - EAP Method for 3GPP AKA (EAP-AKA)</a>
%%%
-module(ocs_eap_aka).
-copyright('Copyright (c) 2019 - 2021 SigScale Global Inc.').

% export public api
-export([prf/1, compressed_imsi/1, encrypt_imsi/3, decrypt_imsi/2,
		encrypt_key/4]).

-on_load(init/0).

-include("ocs_eap_codec.hrl").

%% 3GPP TS 23.003 19.3.2 Root NAI
-define(PERM_AKA,  $0).
-define(PERM_AKAp, $6).
%% 3GPP TS 23.003 19.3.4 Fast Re-auth
-define(FAST_AKA,  $4).
-define(FAST_AKAp, $8).
%% 3GPP TS 23.003 19.3.5 Pseudonym
-define(TEMP_AKA,  $2).
-define(TEMP_AKAp, $7).

-spec prf(MK) -> Result
	when 
		MK :: binary(),
		Result :: binary().
%% @doc Pseudo-Random number Function (PRF).
%%
%% 	Generates a digest indistinguisable from random
%% 	as described in RFC4187 section 7.
%%
prf(MK) when byte_size(MK) =:= 20 ->
	prf(MK, <<>>).
%% @hidden
prf(_XKEY, Acc) when byte_size(Acc) =:= 160 ->
	Acc;
prf(<<XKEYn:160>> = XKEY, Acc) ->
	Mod = 1461501637330902918203684832716283019655932542976, % 2^b
	W = g(XKEY),
	<<Wn:160>> = W,
	XKEY1n = (1 + XKEYn + Wn) rem Mod,
	prf(<<XKEY1n:160>>, <<Acc/binary, W/binary>>).

%%
%% internal functions
%% 

-spec g(XKEY) -> Digest
	when
		XKEY :: binary(),
		Digest:: binary().
%% @doc Calculate message digest.
%%
%% 	A modified SHA-1 as described in RFC4187 section 7.
g(XKEY) when bit_size(XKEY) =:= 160 ->
	erlang:nif_error(nif_library_not_loaded).

-spec compressed_imsi(IMSI) -> IMSI
	when
		IMSI :: binary().
%% @doc Compress or decompress an IMSI.
%%
%% 	See 3GPP 33.402 14.1 Temporary identity generation.
%% @private
compressed_imsi(<<15:4, _:60/bits>> = IMSI) ->
	B = << <<A, B>> || <<A:4, B:4>> <= IMSI >>,
	lists:flatten([integer_to_list(C) || <<C>> <= B, C /= 15]);
compressed_imsi(IMSI) when is_binary(IMSI) ->
	L1 = [list_to_integer([C]) || <<C>> <= IMSI],
	L2 = lists:duplicate(16 - length(L1), 15),
	L3 = L2 ++ L1,
	<< <<D:4>> || D <- L3 >>.

-dialyzer([{nowarn_function, [encrypt_imsi/3]}, no_missing_calls]). % temporary
-spec encrypt_imsi(Tag, CompressedIMSI, Key) -> Pseudonym
	when
		Tag :: 50 | 55, % ?TEMP_AKA | ?TEMP_AKAp, (compile error with 18)
		CompressedIMSI :: binary(),
		Key :: {N, Kpseu},
		N :: pos_integer(),
		Kpseu :: binary(),
		Pseudonym :: binary().
%% @doc Create an encrypted temporary identity.
%%
%% 	See 3GPP 33.402 14.1 Temporary identity generation.
%% @private
encrypt_imsi(Tag, CompressedIMSI, {N, Kpseu} = _Key)
		when ((Tag =:= ?TEMP_AKA) or (Tag =:= ?TEMP_AKAp)),
		size(CompressedIMSI) == 8, size(Kpseu) == 16 ->
	Pad = crypto:strong_rand_bytes(8),
	PaddedIMSI = <<CompressedIMSI/binary, Pad/binary>>,
	EncryptedIMSI = crypto:block_encrypt(aes_ecb, Kpseu, PaddedIMSI),
	TaggedIMSI = <<Tag:6, N:4, EncryptedIMSI/binary, 0:6>>,
	binary:part(base64:encode(TaggedIMSI), 0, 23).

-spec decrypt_imsi(Pseudonym, Keys) -> CompressedIMSI
	when
		Pseudonym :: binary(),
		Keys :: [Key],
		Key :: {N, Kpseu},
		N :: pos_integer(),
		Kpseu :: binary(),
		CompressedIMSI :: binary().
%% @doc Decrypt a temporary identity.
%%
%% 	See 3GPP 33.402 14.1 Temporary identity generation.
%% @private
decrypt_imsi(Pseudonym, Keys)
		when size(Pseudonym) == 23, is_list(Keys) ->
	TaggedIMSI = base64:decode(<<Pseudonym/binary, $A>>),
	<<_:6, N:4, EncryptedIMSI:16/binary, _:6>> = TaggedIMSI,
	{_, Kpseu} = lists:keyfind(N, 1, Keys),
	PaddedIMSI = crypto:block_decrypt(aes_ecb, Kpseu, EncryptedIMSI),
	binary:part(PaddedIMSI, 0, 8).

-spec encrypt_key(Secret, RequestAuthenticator, Salt, Key) -> Ciphertext
	when
		Secret :: binary(),
		RequestAuthenticator :: [byte()],
		Salt :: integer(),
		Key :: binary(),
		Ciphertext :: binary().
%% @doc Encrypt the Pairwise Master Key (PMK) according to RFC2548
%% 	section 2.4.2 for use as String in a MS-MPPE-Recv-Key
%% 	or MS-MPPE-Send-Key attribute.
%% @private
encrypt_key(Secret, RequestAuthenticator, Salt, Key)
		when (Salt bsr 15) == 1 ->
	KeyLength = size(Key),
	Plaintext = case (KeyLength + 1) rem 16 of
		0 ->
			<<KeyLength, Key/binary>>;
		N ->
			PadLength = (16 - N) * 8,
			<<KeyLength, Key/binary, 0:PadLength>>
	end,
	F = fun(P, [H | _] = Acc) ->
				B = crypto:hash(md5, [Secret, H]),
				C = crypto:exor(P, B),
				[C | Acc]
	end,
	AccIn = [[RequestAuthenticator, <<Salt:16>>]],
	AccOut = lists:foldl(F, AccIn, [P || <<P:16/binary>> <= Plaintext]),
	iolist_to_binary(tl(lists:reverse(AccOut))).

%%
%% internal functions
%%

-spec init() -> ok.
%% @doc When this module is loaded this function is called to load NIF library.
%% @hidden
init() ->
	{ok, Application} = application:get_application(?MODULE),
	PrivDir = case code:priv_dir(Application) of
		{error, bad_name} ->
			BEAM = atom_to_list(?MODULE) ++ ".beam",
			Ebin = filename:dirname(code:where_is_file(BEAM)),
			filename:dirname(Ebin) ++ "/priv";
		Path ->
			Path
	end,
	ok = erlang:load_nif(PrivDir ++ "/lib/ocs_eap_aka", 0).

