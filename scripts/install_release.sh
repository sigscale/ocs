#!/bin/bash
########################################################################
# Copyright (c) 2018 - 2025 SigScale Global Inc.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
########################################################################
# Install an Erlang/OTP release package

usage() {
	echo "usage: $0 [-d] [-p] RELEASE-NAME"
	exit 1;
}

while getopts ":pd" opt; do
  case ${opt} in
    p)
      PERMANENT=true
      ;;
    d)
      echo "$0 $*"
		set -x
      ;;
    ?)
      usage
      ;;
  esac
done
shift $(($OPTIND - 1))

if [ $# -ne 1 ];
then
	usage
else
	APP_NEW=$1
	APP_NAME="${APP_NEW%%-[0-9]*}"
fi

cd ${HOME}
if [ -f "releases/${APP_NEW}.tar.gz" ];
then
	tar -zxf releases/${APP_NEW}.tar.gz
else
	echo "Release package ${APP_NEW} not found."
	exit 1
fi

if [ -f "releases/RELEASES" ];
then
	if ! APP_OLD=$(erl -noinput -eval '{ok, [R]} = file:consult("releases/RELEASES"), {release, _, Vsn, _, _, permanent} = lists:keyfind(permanent, 6, R), io:fwrite("~s", [Vsn]), init:stop()' 2> /dev/null);
	then
		echo "No permanent release found."
		unset APP_OLD
	else
		if [[ ${APP_NEW} == ${APP_OLD} || ( -z "${PERMANENT}" && ( ${APP_OLD} != ${APP_NAME}-* )) ]];
		then
			echo "Unpacked (only) release ${APP_NEW}."
			exit 0
		fi
	fi
fi
INSTDIR="${HOME}/lib"
APPDIRS=$(sed -e 's|.*{\([a-z][a-zA-Z_0-9]*\),[[:blank:]]*\"\([0-9.]*\)\".*|{\1,\"\2\",\"'${INSTDIR}'\"},|' -e '1s|^.*$|[|' -e '$s|\,$|]|' releases/${APP_NEW}.rel | tr -d "\r\n")
SASLVER=$(erl -noinput -eval 'application:load(sasl), {ok, Vsn} = application:get_key(sasl, vsn), io:fwrite("~s", [Vsn]), init:stop()')

# Compare old and new release versions
if [ -n "${APP_OLD}" ] && [ "${APP_NEW}" != "${APP_OLD}" ] && [ -d "lib/${APP_OLD}" ];
then
	# Perform an OTP release upgrade
	OTP_NODE="${APP_NAME}@$(echo ${HOSTNAME} | sed -e 's/\..*//')"
	cp releases/${APP_NEW}/sys.config releases/${APP_NEW}/sys.config.dist
	if [ -f "releases/${APP_OLD}/sys.config.dist" ];
	then
		echo "Merging previous system configuration localizations (sys.config) ..."
		diff --text --unified releases/${APP_OLD}/sys.config.dist \
				releases/${APP_OLD}/sys.config \
				> releases/${APP_OLD}/sys.config.patch || :
		if patch releases/${APP_NEW}/sys.config releases/${APP_OLD}/sys.config.patch;
		then
			echo "... merge done."
		else
			echo "... merge failed."
			echo "Using previous system configuration without any newly distributed changes."
			sed -e "s/${APP_OLD}/${APP_NEW}/" \
					releases/${APP_OLD}/sys.config > releases/${APP_NEW}/sys.config
		fi
	else
		echo "Using previous system configuration localizations (sys.config) without any newly distributed changes."
		sed -e "s/${APP_OLD}/${APP_NEW}/" \
				releases/${APP_OLD}/sys.config > releases/${APP_NEW}/sys.config
	fi
	if epmd -names 2> /dev/null | grep -q "^name ${APP_NAME} at";
	then
		# Upgrade using rpc
		RPC_SNAME=$(id -un)
		echo "Performing an in-service upgrade to ${APP_NEW} ..."
		if echo -e "4.2\n${SASLVER}"  | sort --check=quiet --version-sort;
		then
			if erl -noshell -sname ${RPC_SNAME} \
					-eval "rpc:call('${OTP_NODE}', application, stop, [sasl])" \
					-eval "rpc:call('${OTP_NODE}', application, set_env, [sasl, releases_dir, \"${HOME}/releases\"])" \
					-eval "rpc:call('${OTP_NODE}', application, start, [sasl])" \
					-eval "rpc:call('${OTP_NODE}', systools, make_relup, [\"releases/${APP_NEW}\", [\"releases/${APP_OLD}\"], [\"releases/${APP_OLD}\"], [{path,[\"lib/*/ebin\"]}, {outdir, \"releases/${APP_NEW}\"}]])" \
					-eval "{ok, _} = rpc:call('${OTP_NODE}', release_handler, set_unpacked, [\"${HOME}/releases/${APP_NEW}.rel\", ${APPDIRS}])" \
					-eval "{ok, _, _} = rpc:call('${OTP_NODE}', release_handler, install_release, [\"${APP_NEW}\", [{update_paths, true}]])" \
					-eval "ok = rpc:call('${OTP_NODE}', release_handler, make_permanent, [\"${APP_NEW}\"])" \
					-s init stop;
			then
				echo "... done."
			else
				echo "... failed."
				exit 1
			fi
		else
			if erl -noshell -sname ${RPC_SNAME} \
					-eval "rpc:call('${OTP_NODE}', application, start, [sasl])" \
					-eval "rpc:call('${OTP_NODE}', systools, make_relup, [\"releases/${APP_NEW}\", [\"releases/${APP_OLD}\"], [\"releases/${APP_OLD}\"], [{path,[\"lib/*/ebin\"]}, {outdir, \"releases/${APP_NEW}\"}]])" \
					-eval "{ok, _} = rpc:call('${OTP_NODE}', release_handler, set_unpacked, [\"releases/${APP_NEW}.rel\", ${APPDIRS}])" \
					-eval "{ok, _, _} = rpc:call('${OTP_NODE}', release_handler, install_release, [\"${APP_NEW}\", [{update_paths, true}]])" \
					-eval "ok = rpc:call('${OTP_NODE}', release_handler, make_permanent, [\"${APP_NEW}\"])" \
					-s init stop;
			then
				echo "... done."
			else
				echo "... failed."
				exit 1
			fi
		fi
	else
		# Start sasl and mnesia, perform upgrade, stop started applications
		echo "Performing an out-of-service upgrade to ${APP_NEW} ..."
		if echo -e "4.2\n${SASLVER}"  | sort --check=quiet --version-sort;
		then
			if ERL_LIBS=lib RELDIR=${HOME}/releases erl -noshell \
					-sname ${OTP_NODE} -config releases/${APP_OLD}/sys \
					-s mnesia \
					-eval "application:start(sasl)" \
					-eval "application:load(${APP_NAME})" \
					-eval "systools:make_relup(\"releases/${APP_NEW}\", [\"releases/${APP_OLD}\"], [\"releases/${APP_OLD}\"], [{path,[\"lib/*/ebin\"]}, {outdir, \"releases/${APP_NEW}\"}])" \
					-eval "{ok, _} = release_handler:set_unpacked(\"${HOME}/releases/${APP_NEW}.rel\", ${APPDIRS})" \
					-eval "{ok, _, _} = release_handler:install_release(\"${APP_NEW}\", [{update_paths, true}])" \
					-eval "ok = release_handler:make_permanent(\"${APP_NEW}\")" \
					-s init stop;
			then
				echo "... done."
			else
				echo "... failed."
				exit 1
			fi
		else
			if ERL_LIBS=lib RELDIR=releases erl -noshell -sname ${OTP_NODE} -config releases/${APP_OLD}/sys \
					-s mnesia \
					-eval "application:start(sasl)" \
					-eval "application:load(${APP_NAME})" \
					-eval "systools:make_relup(\"releases/${APP_NEW}\", [\"releases/${APP_OLD}\"], [\"releases/${APP_OLD}\"], [{path,[\"lib/*/ebin\"]}, {outdir, \"releases/${APP_NEW}\"}])" \
					-eval "{ok, _} = release_handler:set_unpacked(\"releases/${APP_NEW}.rel\", ${APPDIRS})" \
					-eval "{ok, _, _} = release_handler:install_release(\"${APP_NEW}\", [{update_paths, true}])" \
					-eval "ok = release_handler:make_permanent(\"${APP_NEW}\")" \
					-s init stop;
			then
				echo "... done."
			else
				echo "... failed."
				exit 1
			fi
		fi
	fi
else
	# Install release via shell
	echo "Installing an initial release of ${APP_NEW} ..."
	if echo -e "4.2\n${SASLVER}"  | sort --check=quiet --version-sort;
	then
		if RELDIR=${HOME}/releases erl -noshell -eval "application:start(sasl)" \
				-eval "ok = release_handler:create_RELEASES(\"${HOME}/releases\", \"${HOME}/releases/${APP_NEW}.rel\", ${APPDIRS})" \
				-s init stop;
		then
			echo "... done."
		else
			echo "... failed."
			exit 1
		fi
	else
		if RELDIR=releases erl -noshell -eval "application:start(sasl)" \
				-eval "ok = release_handler:create_RELEASES(code:root_dir(), \"${HOME}/releases\", \"${HOME}/releases/${APP_NEW}.rel\", ${APPDIRS})" \
				-s init stop;
		then
			echo "... done."
		else
			echo "... failed."
			exit 1
		fi
	fi
	if ! test -f releases/RELEASES;
	then
		exit 1
	fi
	cp releases/${APP_NEW}/sys.config releases/${APP_NEW}/sys.config.dist
fi
ERTS=$(grep "^\[{release," releases/RELEASES | sed -e 's/^\[{release,[[:blank:]]*\"//' -e 's/^[^"]*\",[[:blank:]]*\"//' -e 's/^[^"]*\",[[:blank:]]*\"//' -e 's/^\([0-9.]*\).*/\1/')
echo "${ERTS} ${APP_NEW}" > releases/start_erl.data

