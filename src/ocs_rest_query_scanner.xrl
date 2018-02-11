%%% vim: ts=3: 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2018 SigScale Global Inc.
%%% @end
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @author Vance Shipley <vances@sigscale.org>
%%% @doc This is an input file for leex, the lexical analyzer generator
%%% 	to be used in preprocessing input to
%%% 	{@link //ocs/ocs_rest_query_parser. ocs_rest_query_parser} in the
%%% 	{@link //ocs. ocs} application.
%%%
%%% @reference Advanced Attribute Filtering Pattern for REST APIs
%%% (<a href="https://projects.tmforum.org/jira/browse/AP-832">AP-832</a>).

Definitions.
String = [a-zA-Z]+[a-zA-Z0-9%]*
Number = [0-9]*

Rules.
\.exact= : {token, {exact, TokenLine}}.
\.notexact= : {token, {notexact, TokenLine}}.
\.gt= : {token, {gt, TokenLine}}.
\.gte= : {token, {gte, TokenLine}}.
\.lt= : {token, {lt, TokenLine}}.
\.lte= : {token, {lte, TokenLine}}.
\.regex= : {token, {regex, TokenLine}}.
\.like= : {token, {like, TokenLine}}.
\.notlike= : {token, {notlike, TokenLine}}.
\.in= : {token, {in, TokenLine}}.
\.notin= : {token, {notin, TokenLine}}.
\.contains= : {token, {contains, TokenLine}}.
\.notcontain= : {token, {notcontain, TokenLine}}.
\.containsall= : {token, {containsall, TokenLine}}.
\*= : {token, {regex, TokenLine}}.
<= : {token, {lte, TokenLine}}.
>= : {token, {gte, TokenLine}}.
> : {token, {gt, TokenLine}}.
<> : {token, {notexact, TokenLine}}.
= : {token, {exact, TokenLine}}.
{String} : {token, {string, TokenLine, TokenChars}}.
{Number} : {token, {number, TokenLine, list_to_integer(TokenChars)}}.
\. : {token, {'.', TokenLine}}.
\[ : {token, {'[', TokenLine}}.
\] : {token, {']', TokenLine}}.
\{ : {token, {'{', TokenLine}}.
\} : {token, {'}', TokenLine}}.
\, : {token, {',', TokenLine}}.
\" : {token, {'"', TokenLine}}.

Erlang code.
%%% @doc The lexical scanner.
%%%  <h2><a name="functions">Function Details</a></h2>
%%%
%%%  <h3 class="function"><a name="string-1">string/1</a></h3>
%%%  <div class="spec">
%%%  <p><tt>string(String) -&gt; Result</tt>
%%%  <ul class="definitions">
%%%    <li><tt>String = string()</tt></li>
%%%    <li><tt>Result = {ok, Tokens, EndLine} | ErrorInfo</tt></li>
%%%    <li><tt>Token = tuple()</tt></li>
%%%    <li><tt>ErrorLine = integer()</tt></li>
%%%    <li><tt>ErrorInfo = {ErrorLine, module(), error_descriptor()}</tt></li>
%%%  </ul></p>
%%%  </div>
%%%  <p>Scan the input <tt>String</tt> according to the grammar
%%%  of the advanced attribute filtering pattern. Create <tt>Tokens</tt>
%%%  suitable for input to {@link //ocs/ocs_rest_query_parser.
%%%  ocs_rest_query_parser}</p>
%%% @end

