Header
"%%% vim: ts=3: "
"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
"%%% @copyright 2018 SigScale Global Inc."
"%%% @end"
"%%% Licensed under the Apache License, Version 2.0 (the \"License\");"
"%%% you may not use this file except in compliance with the License."
"%%% You may obtain a copy of the License at"
"%%%"
"%%%     http://www.apache.org/licenses/LICENSE-2.0"
"%%%"
"%%% Unless required by applicable law or agreed to in writing, software"
"%%% distributed under the License is distributed on an \"AS IS\" BASIS,"
"%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied."
"%%% See the License for the specific language governing permissions and"
"%%% limitations under the License."
"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
"%%% @doc This library module implements a parser for TM Forum REST API"
"%%% 	Advanced Attribute Filtering Pattern query format in the"
"%%% 	{@link //ocs. ocs} application."
"%%%"
"%%% 	This module is generated with {@link //parsetools/yecc. yecc}"
"%%% 	from `{@module}.yrl'."
"%%%"
"%%% @author Vance Shipley <vances@sigscale.org>"
"%%% @reference Advanced Attribute Filtering Pattern for REST APIs"
"%%% (<a href=\"https://projects.tmforum.org/jira/browse/AP-832\">AP-832</a>)."
"%%%  <h2><a name=\"functions\">Function Details</a></h2>"
"%%%"
"%%%  <h3 class=\"function\"><a name=\"parse-1\">parse/1</a></h3>"
"%%%  <div class=\"spec\">"
"%%%  <p><tt>parse(Tokens) -&gt; Result</tt>"
"%%%  <ul class=\"definitions\">"
"%%%    <li><tt>Tokens = [Token] </tt></li>"
"%%%    <li><tt>Token = {Category, LineNumber, Symbol}"
"%%%        | {Symbol, LineNumber}</tt></li>"
"%%%    <li><tt>Category = string | number</tt></li>"
"%%%    <li><tt>Symbol = '[' | ']' | '{' | '}' | '.' | ','"
"%%%        | Operator</tt></li>"
"%%%    <li><tt>Result = {ok, Filters}"
"%%%        | {error, {LineNumber, Module, Message}}</tt></li>"
"%%%    <li><tt>Filters = [Filter]</tt></li>"
"%%%    <li><tt>Filter = Simple | Complex | Array</tt></li>"
"%%%    <li><tt>Simple = {LHS, Operator, RHS}</tt></li>"
"%%%    <li><tt>LHS = string()</tt></li>"
"%%%    <li><tt>Operator = exact | notexact | lt | lte | gt | gte"
"%%%        | regex | like | notlike | in | notin"
"%%%        | contains | notcontain | containsall</tt></li>"
"%%%    <li><tt>RHS = integer() | string() | boolean()"
"%%%        | Complex | Filters</tt></li>"
"%%%    <li><tt>Complex = {complex, Filters}</tt></li>"
"%%%    <li><tt>Array = {array, Filters}</tt></li>"
"%%%    <li><tt>LineNumber = integer()</tt></li>"
"%%%    <li><tt>Module = atom()</tt></li>"
"%%%    <li><tt>Message = term()</tt></li>"
"%%%  </ul></p>"
"%%%  </div>"
"%%%  <p>Parse the input <tt>Tokens</tt> according to the grammar"
"%%%  of the advanced attribute filtering pattern.</p>"
"%%%"
.

Nonterminals filters filter array complex field value values simple.

Terminals '[' ']' '{' '}' ',' '.' number string
	exact notexact lt lte gt gte regex like notlike
	in notin contains notcontain containsall.

Rootsymbol filters.

Nonassoc 100 exact notexact lt lte gt gte regex like notlike
	in notin contains notcontain containsall.

filters -> filter :
	['$1'].
filters -> filter ',' filters :
	['$1' | '$3'].

filter -> simple :
	'$1'.
filter -> array :
	'$1'.
filter -> complex :
	'$1'.

simple -> field exact value :
	case '$3' of
		"true" ->
			{'$1', exact, true};
		"false" ->
			{'$1', exact, false};
		Value ->
			{'$1', exact, Value}
	end.
simple -> field notexact value :
	case '$3' of
		"true" ->
			{'$1', notexact, true};
		"false" ->
			{'$1', notexact, false};
		Value ->
			{'$1', notexact, Value}
	end.
simple -> field lt value :
	{'$1', lt, '$3'}.
simple -> field lte value :
	{'$1', lte, '$3'}.
simple -> field gt value :
	{'$1', gt, '$3'}.
simple -> field gte value :
	{'$1', gte, '$3'}.
simple -> field regex value :
	{'$1', regex, '$3'}.
simple -> field like '[' values ']' :
	{'$1', like, '$4'}.
simple -> field notlike '[' values ']' :
	{'$1', notlike, '$4'}.
simple -> field in '[' values ']' :
	{'$1', in, '$4'}.
simple -> field notin '[' values ']' :
	{'$1', notin, '$4'}.
simple -> field contains '[' filters ']' :
	{'$1', contains, '$4'}.
simple -> field notcontain '[' filters ']' :
	{'$1', notcontain, '$4'}.
simple -> field containsall '[' filters ']' :
	{'$1', containsall, '$4'}.

array -> '[' filters ']' :
	{array, '$2'}.
complex -> '{' filters '}' :
	{complex, '$2'}.

field -> string :
	element(3, '$1').
field -> string '.' field :
	element(3, '$1') ++ "." ++ '$3'.

value -> string :
	element(3, '$1').
value -> number :
	element(3, '$1').

values -> value :
	['$1'].
values -> value ',' values :
	['$1' | '$3'].

