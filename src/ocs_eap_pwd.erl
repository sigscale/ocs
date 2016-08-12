%%% ocs_eap_pwd.erl
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
%%% @reference <a href="http://tools.ietf.org/html/rfc5931">
%%% 	RFC5931 - EAP Authentication Using Only a Password</a>
%%%
-module(ocs_eap_pwd).
-copyright('Copyright (c) 2016 SigScale Global Inc.').

-export([h/1, prf/2, kdf/3, isqrt/1, ecc_pwe/2, fix_pwe/4]).

-include("ocs_eap_codec.hrl").

-define(P, 16#ffffffff00000001000000000000000000000000ffffffffffffffffffffffff).
-define(A, 16#ffffffff00000001000000000000000000000000fffffffffffffffffffffffc).
-define(B, 16#5ac635d8aa3a93e7b3ebbd55769886bc651d06b0cc53b0f63bce3c3e27d2604b).

-spec h(Data :: binary()) -> binary().
%% @doc Implements a Random function, h which  maps a binary string of indeterminate
%% length onto a 32 bits fixed length binary.
h(Data) when is_binary(Data) ->
	crypto:hmac(sha256, <<0:256>>, Data).

-spec prf(Key :: binary(), Data :: binary()) -> binary().
%% @doc Implements a Pseudo-random function (PRF) which generates a random binary
%% string.
prf(Key, Data) when is_binary(Key), is_binary(Data) ->
	crypto:hmac(sha256, Key, Data).

 -spec kdf(Key :: binary(), Label :: string() | binary(), Length :: pos_integer())
		-> binary().
%% @doc Implements a Key derivation function (KDF) to stretch out a `Key' which is
%% bound with a `Label' to a desired `Length'.
kdf(Key, Label, Length) when is_list(Label) ->
	kdf(Key, list_to_binary(Label), Length);
kdf(Key, Label, Length) when is_binary(Key), is_binary(Label),is_integer(Length),
		(Length rem 8) =:= 0 ->
	Data = list_to_binary([<<1:16>>, Label, <<Length:16>>]),
	K = prf(Key, Data),
	kdf(Key, Label, Length, 1, K, K).
%% @hidden
kdf(Key, Label, Length, I, K, Res) when size(Res) < (Length div 8) ->
	I1 = I + 1,
	Data = list_to_binary([K, <<I1:16>>, Label, <<Length:16>>]),
	K1 = prf(Key, Data),
	kdf(Key, Label, Length, 11, K1, <<Res/binary, K1/binary>>);
kdf(_, _, Length, _, _, Res) when size(Res) >= (Length div 8) ->
	binary:part(Res, 0, Length div 8).

-spec fix_pwe(Token :: binary(), ServerIdentity :: binary(), PeerIdentity
		:: binary(),Password :: binary()) -> PasswordElement :: {integer(),
		integer()}.
%% @doc Fix the Password Element (PWE).
fix_pwe(Token, ServerIdentity, PeerIdentity, Password) ->
	fix_pwe(Token, ServerIdentity, PeerIdentity, Password, 1).
%% @hidden
fix_pwe(Token, ServerIdentity, PeerIdentity, Password, Counter) ->
	Data = list_to_binary([Token, PeerIdentity, ServerIdentity,
		Password, <<Counter>>]),
	PasswordSeed = h(Data),
	case kdf(PasswordSeed, "EAP-pwd Hunting And Pecking", 256) of
		<<PasswordValue:256>> when PasswordValue < ?P ->
			<<_:255, LSB:1>> = PasswordSeed,
			case ecc_pwe(PasswordValue, LSB) of
				{ok, PasswordElement} ->
					PasswordElement;
				{error, _Reason} ->
					fix_pwe(Token, ServerIdentity,
							PeerIdentity, Password, Counter +1)
			end;
		_PasswordValue ->
			fix_pwe(Token, ServerIdentity, PeerIdentity,
					Password, Counter +1)
	end.

-spec ecc_pwe(PasswordValue :: integer(), LSB :: 0..1) ->
		{ok, PasswordElement :: binary()} | {error, not_found}.
%% @doc ECC Operation for password element (PWE).
ecc_pwe(PasswordValue, LSB) when is_integer(PasswordValue),
		((LSB =:= 0) or (LSB =:= 1)) ->
	X = PasswordValue,
	case isqrt(((X * X * X) + (?A * X) + ?B) rem 256) of
		Y when (Y band 1) == LSB ->
			{PasswordValue, Y};
		Y when is_integer(Y) ->
			{PasswordValue, ?P- Y};
		error ->
			{error, not_found}
	end.

 -spec isqrt(X :: pos_integer()) -> pos_integer() | error.
%% @doc Returns integer square root of a given square number .
isqrt(X) when is_integer(X), X >= 0 ->
	isqrt(X, X).
%% @hidden
isqrt(X, Xk) ->
	Xk1 = (Xk + X div Xk) div 2,
	if
		Xk1 >= Xk ->
			if
				Xk * Xk == X ->
					Xk;
						true ->
						error
			end;
		Xk1 < Xk ->
			isqrt(X, Xk1)
end.

