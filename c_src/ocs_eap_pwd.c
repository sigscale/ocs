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
#include <openssl/objects.h>
#include <openssl/ec.h>
#include <openssl/sha.h>
#include <openssl/hmac.h>
#include <openssl/evp.h>

static unsigned char zerokey[SHA256_DIGEST_LENGTH] = { 0x00 };

#if OPENSSL_VERSION_NUMBER < 0x10100000L
static HMAC_CTX *
HMAC_CTX_new(void)
{
	HMAC_CTX *context;

	if ((context = calloc(sizeof(*context), 1)))
		HMAC_CTX_init(context);
	return context;
}
#endif /* OpenSSL < v1.1.0 */

#if ERL_NIF_MAJOR_VERSION == 2 && ERL_NIF_MINOR_VERSION < 8 \
		|| ERL_NIF_MAJOR_VERSION < 2
ERL_NIF_TERM
enif_raise_exception(ErlNifEnv* env, ERL_NIF_TERM reason) {
	return enif_make_badarg(env);
}
#endif /* NIF < v2.8 */

static ERL_NIF_TERM
compute_pwe_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	ErlNifBinary token, server_id, peer_id, password, pwe_ret;
	ERL_NIF_TERM reason;
	EC_GROUP *group;
	EC_POINT *pwe;
	BIGNUM *prime, *order, *cofactor;
	unsigned char counter;
	HMAC_CTX *context;
	unsigned char seed[SHA256_DIGEST_LENGTH];

	if (!enif_inspect_binary(env, argv[0], &token)
			|| !enif_inspect_binary(env, argv[1], &server_id)
			|| !enif_inspect_binary(env, argv[2], &peer_id)
			|| !enif_inspect_binary(env, argv[3], &password))
		return enif_make_badarg(env);
	if (!(group = EC_GROUP_new_by_curve_name(NID_X9_62_prime256v1))
			|| !(pwe = EC_POINT_new(group))
			|| !(prime = BN_new())
			|| !(order = BN_new())
			|| !(cofactor = BN_new())
			|| !(context = HMAC_CTX_new())
			|| !enif_alloc_binary(256, &pwe_ret)) {
		enif_make_existing_atom(env, "enomem", &reason, ERL_NIF_LATIN1);
		return enif_raise_exception(env, reason);
	}
	if (!EC_GROUP_get_curve_GFp(group, prime, NULL, NULL, NULL)
			|| !EC_GROUP_get_order(group, order, NULL)
			|| !EC_GROUP_get_cofactor(group, cofactor, NULL)) {
		reason = enif_make_string(env, "failed to get curve", ERL_NIF_LATIN1);
		return enif_raise_exception(env, reason);
	}
	for (counter = 1; counter < 10; counter++) {
		HMAC_Init_ex(context, zerokey, SHA256_DIGEST_LENGTH, EVP_sha256(), NULL);
		HMAC_Update(context, token.data, token.size);
		HMAC_Update(context, peer_id.data, peer_id.size);
		HMAC_Update(context, server_id.data, server_id.size);
		HMAC_Update(context, password.data, password.size);
		HMAC_Update(context, &counter, sizeof(counter));
		HMAC_Final(context, seed, NULL);
	}

	return enif_make_binary(env, &pwe_ret);
}

static ERL_NIF_TERM
compute_scalar_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	ErlNifBinary random, scalar, element;
	BIGNUM s_rand;
	ERL_NIF_TERM reason, scalar_ret, element_ret;

	if (!enif_inspect_binary(env, argv[0], &random)
			|| !BN_bin2bn(random.data, random.size, &s_rand))
		return enif_make_badarg(env);
	if (!enif_alloc_binary(256, &scalar)
			|| !enif_alloc_binary(256, &element)) {
		enif_make_existing_atom(env, "enomem", &reason, ERL_NIF_LATIN1);
		return enif_raise_exception(env, reason);
	}

	scalar_ret = enif_make_binary(env, &scalar);
	element_ret = enif_make_binary(env, &element);
	return enif_make_tuple2(env, scalar_ret, element_ret);
}

static ERL_NIF_TERM
compute_ks_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	ErlNifBinary pwe, random, scalar, element, ks;
	ERL_NIF_TERM reason;

	if (!enif_inspect_binary(env, argv[0], &pwe)
			|| !enif_inspect_binary(env, argv[1], &random)
			|| !enif_inspect_binary(env, argv[2], &scalar)
			|| !enif_inspect_binary(env, argv[3], &element))
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

