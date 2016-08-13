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
%%% @reference <a href="http://tools.ietf.org/html/rfc5114">
%%% 	RFC5114 - Additional Diffie-Hellman Groups</a>
%%%
-module(ocs_eap_pwd).
-copyright('Copyright (c) 2016 SigScale Global Inc.').

-export([h/1, prf/2, kdf/3, isqrt/1, ecc_pwe/2, fix_pwe/4]).

-define(P,  16#ffffffff00000001000000000000000000000000ffffffffffffffffffffffff).
-define(A,  16#ffffffff00000001000000000000000000000000fffffffffffffffffffffffc).
-define(B,  16#5ac635d8aa3a93e7b3ebbd55769886bc651d06b0cc53b0f63bce3c3e27d2604b).
-define(Gx, 16#6b17d1f2e12c4247f8bce6e563a440f277037d812deb33a0f4a13945d898c296).
-define(Gy, 16#4fe342e2fe1a7f9b8ee7eb4a7c0f9e162bce33576b315ececbb6406837bf51f5).
-define(R,  16#ffffffff00000000ffffffffffffffffbce6faada7179e84f3b9cac2fc632551).

-spec h([Data :: binary()]) -> binary().
%% @doc Random function (H).
%% 	RFC5931 section 2.4.
h(Data) when is_list(Data) ->
	h(crypto:hmac_init(sha256, <<0:256>>), Data).
%% @hidden
h(Context, [H | T]) ->
	h(crypto:hmac_update(Context, H), T);
h(Context, []) ->
	crypto:hmac_final(Context).
	
-spec prf(Key :: binary(), Data :: [binary()]) -> binary().
%% @doc Pseudo-random function (PRF).
%% 	RFC5931 section 2.10
prf(Key, Data) when is_binary(Key), is_list(Data) ->
	prf(crypto:hmac_init(sha256, Key), data).
%% @hidden
prf1(Context, [H | T]) ->
	prf1(crypto:hmac_update(Context, H), T);
prf1(Context, []) ->
	crypto:hmac_final(Context).

-spec kdf(Key :: binary(), Label :: string() | binary(),
		Length :: pos_integer()) -> binary().
%% @doc Key derivation function (KDF).
%% 	RFC5931 section 2.5.
kdf(Key, Label, Length) when is_list(Label) ->
	kdf(Key, list_to_binary(Label), Length);
kdf(Key, Label, Length) when is_binary(Key), is_binary(Label),
		is_integer(Length), (Length rem 8) =:= 0 ->
	K = prf(Key, [<<1:16>>, Label, <<Length:16>>]),
	kdf(Key, Label, Length, 1, K, K).
%% @hidden
kdf(Key, Label, Length, I, K, Res) when size(Res) < (Length div 8) ->
	I1 = I + 1,
	K1 = prf(Key, [K, <<I1:16>>, Label, <<Length:16>>]),
	kdf(Key, Label, Length, I1, K1, <<Res/binary, K1/binary>>);
kdf(_, _, Length, _, _, Res) when size(Res) >= (Length div 8) ->
	binary:part(Res, 0, Length div 8).

-spec fix_pwe(Token :: binary(), ServerIdentity :: binary(), PeerIdentity
		:: binary(),Password :: binary()) -> PasswordElement :: binary().
%% @doc Fix the Password Element (PWE).
fix_pwe(Token, ServerIdentity, PeerIdentity, Password) ->
	fix_pwe(Token, ServerIdentity, PeerIdentity, Password, 1).
%% @hidden
fix_pwe(Token, ServerIdentity, PeerIdentity, Password, Counter) ->
	PasswordSeed = h([Token, PeerIdentity, ServerIdentity,
			Password, <<Counter>>]),
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
		_ ->
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
			{ok, <<X:256, Y:256>>};
		Y when is_integer(Y) ->
			Y1 = ?P - 1,
			{ok, <<X:256, Y1:256>>};
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

