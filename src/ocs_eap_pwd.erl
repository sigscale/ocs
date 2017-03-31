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
%%% @doc This library module implements functions for EAP authentication
%%% 	using only a password in the
%%% 	{@link //ocs. ocs} application.
%%%
%%% @reference <a href="http://tools.ietf.org/html/rfc5931">
%%% 	RFC5931 - EAP Authentication Using Only a Password</a>
%%% @reference <a href="http://tools.ietf.org/html/rfc5114">
%%% 	RFC5114 - Additional Diffie-Hellman Groups</a>
%%%
-module(ocs_eap_pwd).
-copyright('Copyright (c) 2016 SigScale Global Inc.').

-export([h/1, kdf/3]).
-export([compute_pwe/4, compute_scalar/2, compute_ks/4]).
-on_load(init/0).

-include("ocs_eap_codec.hrl").

-spec h(Data) -> Result
	when 
		Data :: [Data :: binary()],
		Result :: binary().
%% @doc Random function (H).
%% 	RFC5931 section 2.4
h(Data) when is_list(Data) ->
	h(crypto:hmac_init(sha256, <<0:256>>), Data).
%% @hidden
h(Context, [H | T]) ->
	h(crypto:hmac_update(Context, H), T);
h(Context, []) ->
	crypto:hmac_final(Context).
	
-spec kdf(Key, Label, Length) -> Result 
	when
		Key :: binary(),
		Label :: binary(),
		Length :: integer(),
		Result :: binary().
%% @doc Key Derivation Function (KDF).
%% 	RFC5931 section 2.5
kdf(_Key, _Label, _Length) ->
	erlang:nif_error(nif_library_not_loaded).

-spec compute_pwe(Token, PeerIdentity, ServerIdentity, Password) -> PWE 
	when
		Token :: binary(),
		PeerIdentity :: binary(),
		ServerIdentity :: binary(),
		Password :: binary(),
		PWE :: binary().
%% @doc Compute Password Element (PWE).
%% 	RFC5931 section 2.8.3
compute_pwe(_Token, _PeerIdentity, _ServerIdentity, _Password) ->
	erlang:nif_error(nif_library_not_loaded).

-spec compute_scalar(Random, PWE) -> Result
	when
		Random :: binary(),
		PWE :: binary(),
		Result :: {Scalar, Element},
		Scalar :: binary(),
		Element :: binary().
%% @doc Compute Scalar Element.
%% 	RFC5931 section 2.8.4.1
compute_scalar(_Random, _PWE) ->
	erlang:nif_error(nif_library_not_loaded).

-spec compute_ks(Random, PWE, Scalar , Element) -> Ks
	when
		Random :: binary(),
		PWE :: binary(),
		Scalar :: binary(),
		Element :: binary(),
		Ks :: binary().
%% @doc Compute Ks.
%% 	RFC5931 section 2.8.4.1
compute_ks(_Random, _PWE, _Scalar, _Element) ->
	erlang:nif_error(nif_library_not_loaded).

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
	ok = erlang:load_nif(PrivDir ++ "/lib/ocs_eap_pwd", 0).

