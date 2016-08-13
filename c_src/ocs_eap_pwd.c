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

static ERL_NIF_TERM compute_pwe_nif(ErlNifEnv* env, int argc,
		const ERL_NIF_TERM argv[])
{
	int x, ret;
	if (!enif_get_int(env, argv[0], &x)) {
		return enif_make_badarg(env);
	}
	ret = 0;
	return enif_make_int(env, ret);
}

static ErlNifFunc nif_funcs[] = {
	{"compute_pwe", 1, compute_pwe_nif},
};

ERL_NIF_INIT(ocs_eap_pwd, nif_funcs, NULL, NULL, NULL, NULL)

