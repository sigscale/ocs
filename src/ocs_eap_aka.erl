%%% ocs_eap_aka.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2019 SigScale Global Inc.
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
-copyright('Copyright (c) 2019 SigScale Global Inc.').

% export public api
-export([prf/1]).

% export private api
-export([g/1]).

-on_load(init/0).

-include("ocs_eap_codec.hrl").

-spec prf(MK) -> Result
	when 
		MK :: binary(),
		Result :: binary().
%% @doc Pseudo-Random number Function (PRF).
%%
%% 	Generates a digest indistinguisable from random
%% 	as described in RFC4187 section 7.
prf(<<XKEY:160>> = MK) ->
	Mod = 1461501637330902918203684832716283019655932542976, % 2^b
	W0 = g(MK),
	<<W:160>> = W0,
	XVAL = (1 + XKEY + W) rem Mod,
	W1 = g(<<XVAL:160>>),
	<<W0/binary, W1/binary>>.

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

