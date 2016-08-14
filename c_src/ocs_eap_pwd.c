/* ocs_eap_pwd.c
*****************************************************************************
*** Copyright 2016 SigScale Global Inc.
*** 
*** Licensed under the Apache License, Version 2.0 (the "License");
*** you may not use this file except in compliance with the License.
*** You may obtain a copy of the License at
***
***     http://www.apache.org/licenses/LICENSE-2.0
***
*** Unless required by applicable law or agreed to in writing, software
*** distributed under the License is distributed on an "AS IS" BASIS,
*** WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
*** See the License for the specific language governing permissions and
*** limitations under the License.
*****************************************************************************
*** This module implements NIFs for the Elliptic Curve Cryptography (ECC)
*** group(s) used by EAP-pwd (RFC5931).
***/

#include "erl_nif.h"
#include <openssl/bn.h>
#include <openssl/ec.h>

static ERL_NIF_TERM compute_pwe_nif(ErlNifEnv* env, int argc,
		const ERL_NIF_TERM argv[])
{
	ErlNifBinary token, server_id, peer_id, password, pwe;
	ERL_NIF_TERM reason;

	if (!enif_inspect_binary(env, argv[0], &token))
		return enif_make_badarg(env);
	if (!enif_inspect_binary(env, argv[1], &server_id))
		return enif_make_badarg(env);
	if (!enif_inspect_binary(env, argv[2], &peer_id))
		return enif_make_badarg(env);
	if (!enif_inspect_binary(env, argv[3], &password))
		return enif_make_badarg(env);
	if (!enif_alloc_binary(256, &pwe)) {
		enif_make_existing_atom(env, "enomem", &reason, ERL_NIF_LATIN1);
		return enif_raise_exception(env, reason);
	}

	return enif_make_binary(env, &pwe);
}

static ERL_NIF_TERM compute_scalar_nif(ErlNifEnv* env, int argc,
		const ERL_NIF_TERM argv[])
{
	ErlNifBinary random, scalar, element;
	BIGNUM s_rand;
	ERL_NIF_TERM reason, scalar_ret, element_ret;;

	if (!enif_inspect_binary(env, argv[0], &random))
		return enif_make_badarg(env);
	if (!BN_bin2bn(random.data, random.size, &s_rand)) {
		return enif_make_badarg(env);
	}
	if (!enif_alloc_binary(256, &scalar)) {
		enif_make_existing_atom(env, "enomem", &reason, ERL_NIF_LATIN1);
		return enif_raise_exception(env, reason);
	}
	if (!enif_alloc_binary(256, &element)) {
		enif_make_existing_atom(env, "enomem", &reason, ERL_NIF_LATIN1);
		return enif_raise_exception(env, reason);
	}

	scalar_ret = enif_make_binary(env, &scalar);
	element_ret = enif_make_binary(env, &element);
	return enif_make_tuple2(env, scalar_ret, element_ret);
}

static ERL_NIF_TERM compute_ks_nif(ErlNifEnv* env, int argc,
		const ERL_NIF_TERM argv[])
{
	ErlNifBinary pwe, random, scalar, element, ks;
	ERL_NIF_TERM reason;

	if (!enif_inspect_binary(env, argv[0], &pwe))
		return enif_make_badarg(env);
	if (!enif_inspect_binary(env, argv[1], &random))
		return enif_make_badarg(env);
	if (!enif_inspect_binary(env, argv[2], &scalar))
		return enif_make_badarg(env);
	if (!enif_inspect_binary(env, argv[3], &element))
		return enif_make_badarg(env);
	if (!enif_alloc_binary(256, &ks)) {
		enif_make_existing_atom(env, "enomem", &reason, ERL_NIF_LATIN1);
		return enif_raise_exception(env, reason);
	}

	return enif_make_binary(env, &ks);
}

static ErlNifFunc nif_funcs[] = {
	{"compute_pwe", 4, compute_pwe_nif},
	{"compute_scalar", 1, compute_scalar_nif},
	{"compute_ks", 4, compute_ks_nif}
};

ERL_NIF_INIT(ocs_eap_pwd, nif_funcs, NULL, NULL, NULL, NULL)

