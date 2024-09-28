#!/bin/bash
# Install OTP SNMP Agent configuration

RAND=$(od -An -N27 -w27 -v -t u1 /dev/urandom | tr -s '\ ' ',')
ENGINEID="[128,0,196,210,5$RAND]"
if [ ! -f $HOME/snmp/conf/agent.conf ];
then
	echo "{intAgentIpAddress, {0,0,0,0}}." > $HOME/snmp/conf/agent.conf
	echo "{intAgentUDPPort, 4161}." >> $HOME/snmp/conf/agent.conf
	echo "{snmpEngineID, $ENGINEID}." >> $HOME/snmp/conf/agent.conf
	echo "{snmpEngineMaxMessageSize, 484}." >> $HOME/snmp/conf/agent.conf
fi
if [ ! -f $HOME/snmp/conf/standard.conf ];
then
	echo "{sysName, \"SigScale OCS\"}." > $HOME/snmp/conf/standard.conf
	echo "{sysDescr, \"Online Charging System (OCS)\"}." >> $HOME/snmp/conf/standard.conf
	echo "{sysContact, \"support@sigscale.com\"}." >> $HOME/snmp/conf/standard.conf
	echo "{sysObjectID, [3,6,1,4,1,50386,1,1]}." >> $HOME/snmp/conf/standard.conf
	echo "{sysServices, 72}." >> $HOME/snmp/conf/standard.conf
	echo "{snmpEnableAuthenTraps, enabled}." >> $HOME/snmp/conf/standard.conf
fi
if [ ! -f $HOME/snmp/conf/community.conf ];
then
	echo "{\"public\", \"public\", \"initial\", \"\", \"\"}." > $HOME/snmp/conf/community.conf

fi
if [ ! -f $HOME/snmp/conf/vacm.conf ];
then
	echo "{vacmSecurityToGroup, v2c, \"initial\", \"initial\"}." > $HOME/snmp/conf/vacm.conf
	echo "{vacmSecurityToGroup, usm, \"initial\", \"initial\"}." >> $HOME/snmp/conf/vacm.conf
	echo "{vacmAccess, \"initial\", \"\", any, noAuthNoPriv, exact, \"restricted\", \"\", \"restricted\"}." >> $HOME/snmp/conf/vacm.conf
	echo "{vacmAccess, \"initial\", \"\", usm, authNoPriv, exact, \"internet\", \"internet\", \"internet\"}." >> $HOME/snmp/conf/vacm.conf
	echo "{vacmAccess, \"initial\", \"\", usm, authPriv, exact, \"internet\", \"internet\", \"internet\"}." >> $HOME/snmp/conf/vacm.conf
	echo "{vacmViewTreeFamily, \"internet\", [1,3,6,1], included, null}." >> $HOME/snmp/conf/vacm.conf
	echo "{vacmViewTreeFamily, \"restricted\", [1,3,6,1], included, null}." >> $HOME/snmp/conf/vacm.conf
fi
if [ ! -f $HOME/snmp/conf/usm.conf ];
then
	touch $HOME/snmp/conf/usm.conf
fi
if [ ! -f $HOME/snmp/conf/context.conf ];
then
	touch $HOME/snmp/conf/context.conf
fi
if [ ! -f $HOME/snmp/conf/notify.conf ];
then
	touch $HOME/snmp/conf/notify.conf
fi

