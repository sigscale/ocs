{suites, ".",
		[ocs_api_SUITE,
		ocs_diameter_SUITE,
		ocs_rest_api_SUITE,
		ocs_rest_lib_SUITE,
		ocs_simple_auth_SUITE,
		ocs_accounting_SUITE,
		ocs_eap_pwd_SUITE,
		ocs_eap_aka_SUITE,
		ocs_eap_akap_SUITE,
		ocs_codec_diameter_SUITE,
		ocs_codec_eap_SUITE,
		ocs_eap_ttls_SUITE,
		ocs_milenage_SUITE,
		ocs_log_SUITE,
		ocs_rating_SUITE,
		ocs_product_SUITE,
		ocs_charging_SUITE,
		ocs_snmp_SUITE,
		ocs_re_interface_SUITE]}.
{skip_cases, ".",
		ocs_rest_api_SUITE, [get_ipdr_usage], "TODO"}.
