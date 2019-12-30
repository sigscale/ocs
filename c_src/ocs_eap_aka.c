/* ocs_eap_aka.c
*** vim: ts=3
*****************************************************************************
*** Copyright 2016 - 2017 SigScale Global Inc.
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
*** This module implements NIFs for the modified SHA-1 used by
*** EAP-AKA (RFC4187).
***/

#include <stdint.h>
#include "erl_nif.h"

#if ((ERL_NIF_MAJOR_VERSION == 2 && ERL_NIF_MINOR_VERSION < 8) \
		|| ERL_NIF_MAJOR_VERSION < 2)
ERL_NIF_TERM
enif_raise_exception(ErlNifEnv* env, ERL_NIF_TERM reason) {
	return enif_make_badarg(env);
}
#endif /* NIF < v2.8 */

#define CLS(B, W) ((W << B) | (W >> (32 - B)))
#define F1(B, C, D) ((B & C) | ((~B) & D))
#define F2(B, C, D) (B ^ C ^ D)
#define F3(B, C, D) ((B & C) | (B & D) | (C & D))

static ERL_NIF_TERM
g_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
   ErlNifBinary xkey, res;
   ERL_NIF_TERM reason;
   int i;
   const uint32_t h[5] = {0x67452301,
			0xefcdab89, 0x98badcfe, 0x10325476, 0xc3d2e1f0};
   uint32_t a, b, c, d, e, temp;
   uint32_t m[16] = {0};
   uint32_t w[80] = {0};
   
   if (!enif_inspect_binary(env, argv[0], &xkey) || xkey.size != 20)
      return enif_make_badarg(env);
   for(i = 0; i < 5; i++) {
      m[i] = (xkey.data[i*4] << 24) | (xkey.data[i*4+1] << 16)
            | (xkey.data[i*4+2] << 8) | (xkey.data[i*4+3]);
   } 
	for(i = 0; i < 16; i++) {
		w[i] = m[i];
	}
	for(i = 16; i < 80; i++) {
		w[i] = CLS(1, w[i-3] ^ w[i-8] ^ w[i-14] ^ w[i-16]);
	}
	a = h[0];
	b = h[1];
	c = h[2];
	d = h[3];
	e = h[4];
	for(i = 0; i < 20; i++) {
		temp = CLS(5, a) + F1(a, b, c) + e + w[i] + 0x5aA827999;
		e = d;
		d = c;
		c = CLS(30, b);
		b = a;
		a = temp;

	}
	for(i = 20; i < 40; i++) {
		temp = CLS(5, a) + F2(a, b, c) + e + w[i] + 0x6ed9eba1;
		e = d;
		d = c;
		c = CLS(30, b);
		b = a;
		a = temp;
	}
	for(i = 40; i < 60; i++) {
		temp = CLS(5, a) + F3(a, b, c) + e + w[i] + 0x8f1bbcdc;
		e = d;
		d = c;
		c = CLS(30, b);
		b = a;
		a = temp;
	}
	for(i = 60; i < 80; i++) {
		temp = CLS(5, a) + F2(a, b, c) + e + w[i] + 0xca62c1d6;
		e = d;
		d = c;
		c = CLS(30, b);
		b = a;
		a = temp;
	}
	if (!enif_alloc_binary(20, &res)) {
		enif_make_existing_atom(env, "enomem", &reason, ERL_NIF_LATIN1);
		return enif_raise_exception(env, reason);
	}
	temp = h[0] + a;
	res.data[0] = temp >> 24;
	res.data[1] = (temp >> 16) & 0xff;
	res.data[2] = (temp >> 8) & 0xff;
	res.data[3] = temp & 0xff;
	temp = h[1] + b;
	res.data[4] = temp >> 24;
	res.data[5] = (temp >> 16) & 0xff;
	res.data[6] = (temp >> 8) & 0xff;
	res.data[7] = temp & 0xff;
	temp = h[2] + c;
	res.data[8] = temp >> 24;
	res.data[9] = (temp >> 16) & 0xff;
	res.data[10] = (temp >> 8) & 0xff;
	res.data[11] = temp & 0xff;
	temp = h[3] + d;
	res.data[12] = temp >> 24;
	res.data[13] = (temp >> 16) & 0xff;
	res.data[14] = (temp >> 8) & 0xff;
	res.data[15] = temp & 0xff;
	temp = h[4] + e;
	res.data[16] = temp >> 24;
	res.data[17] = (temp >> 16) & 0xff;
	res.data[18] = (temp >> 8) & 0xff;
	res.data[19] = temp & 0xff;
	return enif_make_binary(env, &res);
}

static ErlNifFunc
nif_funcs[] = {
   {"g", 1, g_nif}
};

ERL_NIF_INIT(ocs_eap_aka, nif_funcs, NULL, NULL, NULL, NULL)

