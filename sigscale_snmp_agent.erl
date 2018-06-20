-module(sigscale_snmp_agent).
-export([sigscaleClientsTable/3]).

%%-----------------------------------------------------------------
%% Instrumentation Functions
%%-----------------------------------------------------------------

sigscaleClientsTable(get, RowIndex, Cols) ->
	get(sigscaleClientsTable, RowIndex, Cols);

sigscaleClientsTable(get_next, RowIndex, Cols) ->
	next(sigscaleClientsTable, RowIndex, Cols).

%--------------------------------------------------------------

fa(snmpNotifyTable) -> ?snmpNotifyTag.

foi(snmpNotifyTable) -> ?snmpNotifyName.

noc(snmpNotifyTable) -> 5.

stc(snmpNotifyTable) -> ?snmpNotifyStorageType.

next(Name, RowIndex, Cols) ->
    snmp_generic:handle_table_next(db(Name), RowIndex, Cols,
                                   fa(Name), foi(Name), noc(Name)).

get(Name, RowIndex, Cols) ->
    snmp_generic:handle_table_get(db(Name), RowIndex, Cols, foi(Name)).

