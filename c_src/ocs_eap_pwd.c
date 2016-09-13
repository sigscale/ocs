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

#include <stdint.h>
#include <string.h>
#include <arpa/inet.h>
#include <openssl/bn.h>
#include <openssl/objects.h>
#include <openssl/ec.h>
#include <openssl/sha.h>
#include <openssl/hmac.h>
#include <openssl/evp.h>
#include "erl_nif.h"

static uint8_t zerokey[SHA256_DIGEST_LENGTH] = { 0x00 };

#if OPENSSL_VERSION_NUMBER < 0x10100000L
static HMAC_CTX *
HMAC_CTX_new(void)
{
	HMAC_CTX *context;

	if ((context = calloc(sizeof(*context), 1)))
		HMAC_CTX_init(context);
	return context;
}

void
HMAC_CTX_reset(HMAC_CTX *context)
{
	HMAC_CTX_cleanup(context);
}

void
HMAC_CTX_free(HMAC_CTX *context)
{
	HMAC_CTX_cleanup(context);
}
#endif /* OpenSSL < v1.1.0 */

#if ((ERL_NIF_MAJOR_VERSION == 2 && ERL_NIF_MINOR_VERSION < 8) \
		|| ERL_NIF_MAJOR_VERSION < 2)
ERL_NIF_TERM
enif_raise_exception(ErlNifEnv* env, ERL_NIF_TERM reason) {
	return enif_make_badarg(env);
}
#endif /* NIF < v2.8 */

/* Key Derivatin Function (KDF)
 * RFC5931 section 2.5
 */
static void
kdf(uint8_t *key, int key_len, char const *label,
		int label_len, uint8_t *result, int result_len)
{
	HMAC_CTX *context;
	uint16_t i, counter, L;
	uint8_t k[SHA256_DIGEST_LENGTH];
	int len;
	unsigned int k_len = SHA256_DIGEST_LENGTH;

	context = HMAC_CTX_new();
	len = 0;
	counter = 1;
	L = htons(result_len * 8);
	while (len < result_len) {
		i = htons(counter);
		HMAC_Init_ex(context, key, key_len, EVP_sha256(), NULL);
		if (counter > 1)
			HMAC_Update(context, k, k_len);
		HMAC_Update(context, (uint8_t *) &i, sizeof(uint16_t));
		HMAC_Update(context, (uint8_t const *) label, label_len);
		HMAC_Update(context, (uint8_t *) &L, sizeof(uint16_t));
		HMAC_Final(context, k, &k_len);
		if ((len + k_len) <= result_len)
			memcpy(&result[len], k, k_len);
		else
			memcpy(&result[len], k, result_len - len);
		len += k_len;
		counter++;
		HMAC_CTX_reset(context);
	}
	HMAC_CTX_free(context);
}

/*  Compute the Password Element (PWE)
 *  RFC5931 section 2.8.3.1
 */
static ERL_NIF_TERM
compute_pwe_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	ErlNifBinary token, server_id, peer_id, password, pwe_ret;
	ERL_NIF_TERM reason;
	EC_GROUP *group;
	EC_POINT *pwe;
	BIGNUM *prime, *x, *bn_seed;
	HMAC_CTX *context;
	uint8_t pwd_seed[SHA256_DIGEST_LENGTH], pwd_value[32], counter;
	const char *label = "EAP-pwd Hunting And Pecking";
	int label_len = 27;
	uint8_t point_uncompressed[65];

	if (!enif_inspect_binary(env, argv[0], &token)
			|| !enif_inspect_binary(env, argv[2], &peer_id)
			|| !enif_inspect_binary(env, argv[1], &server_id)
			|| !enif_inspect_binary(env, argv[3], &password))
		return enif_make_badarg(env);
	if (!(group = EC_GROUP_new_by_curve_name(NID_X9_62_prime256v1))
			|| !(pwe = EC_POINT_new(group))
			|| !(prime = BN_new())
			|| !(x = BN_new())
			|| !(bn_seed = BN_new())
			|| !(context = HMAC_CTX_new())
			|| !enif_alloc_binary(64, &pwe_ret)) {
		enif_make_existing_atom(env, "enomem", &reason, ERL_NIF_LATIN1);
		return enif_raise_exception(env, reason);
	}
	if (!EC_GROUP_get_curve_GFp(group, prime, NULL, NULL, NULL)) {
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
		HMAC_Final(context, pwd_seed, NULL);
		BN_bin2bn(pwd_seed, SHA256_DIGEST_LENGTH, bn_seed);
		kdf(pwd_seed, SHA256_DIGEST_LENGTH, label, label_len, pwd_value, 32);
		BN_bin2bn(pwd_value, 32, x);
		if (BN_ucmp(x, prime) >= 0)
			continue;
		if (!EC_POINT_set_compressed_coordinates_GFp(group, pwe, x,
				BN_is_bit_set(bn_seed, 0), NULL))
			continue;
		break;
	}
	if (counter >= 10) {
		reason = enif_make_string(env, "too many iterations", ERL_NIF_LATIN1);
		return enif_raise_exception(env, reason);
	}
	EC_POINT_point2oct(group, pwe, POINT_CONVERSION_UNCOMPRESSED,
			point_uncompressed, 65, NULL);
	memcpy(pwe_ret.data, &point_uncompressed[1], 64);
	HMAC_CTX_free(context);
	EC_GROUP_free(group);
	EC_POINT_free(pwe);
	BN_free(prime);
	BN_free(x);
	BN_free(bn_seed);
	return enif_make_binary(env, &pwe_ret);
}

static ERL_NIF_TERM
compute_scalar_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	ErlNifBinary rand_bin, pwe_bin, scalar_bin, element_bin;
	EC_GROUP *group;
	EC_POINT *pwe, *element;
	BN_CTX *context;
	BIGNUM *rand, *mask, *scalar, *order;
	ERL_NIF_TERM reason, scalar_ret, element_ret;
	uint8_t point_uncompressed[65];

	if (!enif_inspect_binary(env, argv[0], &rand_bin)
			|| !enif_inspect_binary(env, argv[1], &pwe_bin))
		return enif_make_badarg(env);
	point_uncompressed[0] = 4;
	memcpy(&point_uncompressed[1], pwe_bin.data, pwe_bin.size);
	if (!(group = EC_GROUP_new_by_curve_name(NID_X9_62_prime256v1))
			|| !(pwe = EC_POINT_new(group))
			|| !(element = EC_POINT_new(group))
			|| !(context = BN_CTX_new())
			|| !(rand = BN_new())
			|| !(mask = BN_new())
			|| !(scalar = BN_new())
			|| !(order = BN_new())
			|| !BN_bin2bn(rand_bin.data, rand_bin.size, rand)
			|| !EC_POINT_oct2point(group, pwe,
						(uint8_t *) point_uncompressed, 65, context)
			|| !enif_alloc_binary(32, &scalar_bin)
			|| !enif_alloc_binary(64, &element_bin)) {
		enif_make_existing_atom(env, "enomem", &reason, ERL_NIF_LATIN1);
		return enif_raise_exception(env, reason);
	}
	if (!EC_GROUP_get_order(group, order, NULL)
			|| !BN_bin2bn((uint8_t *)rand_bin.data, rand_bin.size, rand)
			|| !BN_rand_range(mask, order)
			|| !BN_mod_add(scalar, rand, mask, order, context)
			|| !BN_bn2bin(scalar, scalar_bin.data)) {
		reason = enif_make_string(env, "failed to compute scalar", ERL_NIF_LATIN1);
		return enif_raise_exception(env, reason);
	}
	if (!EC_POINT_mul(group, element, NULL, pwe, mask, context)
			|| !EC_POINT_invert(group, element, context)
			|| !EC_POINT_point2oct(group, element,
					POINT_CONVERSION_UNCOMPRESSED, point_uncompressed, 65, NULL)) {
		reason = enif_make_string(env, "failed to compute element", ERL_NIF_LATIN1);
		return enif_raise_exception(env, reason);
	}
	memcpy(element_bin.data, &point_uncompressed[1], 64);
	scalar_ret = enif_make_binary(env, &scalar_bin);
	element_ret = enif_make_binary(env, &element_bin);
	EC_GROUP_free(group);
	EC_POINT_free(pwe);
	EC_POINT_free(element);
	BN_CTX_free(context);
	BN_free(rand);
	BN_free(mask);
	BN_free(scalar);
	BN_free(order);
	return enif_make_tuple2(env, scalar_ret, element_ret);
}

static ERL_NIF_TERM
compute_ks_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	ErlNifBinary pwe_bin, rand_bin, scalar_bin, element_bin, ks_bin;
	ERL_NIF_TERM reason;
	EC_GROUP *group;
	EC_POINT *pwe, *element, *k;
	BN_CTX *context;
	BIGNUM *rand, *scalar, *ks;
	uint8_t upoint1[65], upoint2[65];

	if (!enif_inspect_binary(env, argv[0], &rand_bin)
			|| !enif_inspect_binary(env, argv[1], &pwe_bin)
			|| !enif_inspect_binary(env, argv[2], &scalar_bin)
			|| !enif_inspect_binary(env, argv[3], &element_bin))
		return enif_make_badarg(env);
	upoint1[0] = 4;
	memcpy(&upoint1[1], pwe_bin.data, pwe_bin.size);
	upoint2[0] = 4;
	memcpy(&upoint2[1], element_bin.data, element_bin.size);
	if (!(group = EC_GROUP_new_by_curve_name(NID_X9_62_prime256v1))
			|| !(pwe = EC_POINT_new(group))
			|| !(element = EC_POINT_new(group))
			|| !(k = EC_POINT_new(group))
			|| !(context = BN_CTX_new())
			|| !(rand = BN_new())
			|| !(scalar = BN_new())
			|| !(ks = BN_new())
			|| !BN_bin2bn(rand_bin.data, rand_bin.size, rand)
			|| !EC_POINT_oct2point(group, pwe,
						(uint8_t *) upoint1, 65, context)
			|| !BN_bin2bn(scalar_bin.data, scalar_bin.size, scalar)
			|| !EC_POINT_oct2point(group, element,
						(uint8_t *) upoint2, 65, context)
			|| !enif_alloc_binary(32, &ks_bin)) {
		enif_make_existing_atom(env, "enomem", &reason, ERL_NIF_LATIN1);
		return enif_raise_exception(env, reason);
	}
	if (!EC_POINT_is_on_curve(group, element, NULL)) {
		reason = enif_make_string(env, "point not on curve", ERL_NIF_LATIN1);
		return enif_raise_exception(env, reason);
	}
	if (!EC_POINT_mul(group, k, NULL, pwe, scalar, context)
			|| !EC_POINT_add(group, k, k, element, context)
			|| !EC_POINT_mul(group, k, NULL, k, rand, context)
			|| !EC_POINT_get_affine_coordinates_GFp(group, k, ks, NULL, context)
			|| !BN_bn2bin(ks, ks_bin.data)) {
		reason = enif_make_string(env, "failed to compute ks", ERL_NIF_LATIN1);
		return enif_raise_exception(env, reason);
   }
	EC_GROUP_free(group);
	EC_POINT_free(pwe);
	EC_POINT_free(element);
	EC_POINT_free(k);
	BN_CTX_free(context);
	BN_free(rand);
	BN_free(scalar);
	BN_free(ks);
	return enif_make_binary(env, &ks_bin);
}

static ErlNifFunc nif_funcs[] = {
	{"compute_pwe", 4, compute_pwe_nif},
	{"compute_scalar", 2, compute_scalar_nif},
	{"compute_ks", 4, compute_ks_nif}
};

ERL_NIF_INIT(ocs_eap_pwd, nif_funcs, NULL, NULL, NULL, NULL)

