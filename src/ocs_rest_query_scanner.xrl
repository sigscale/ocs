%%% vim: ts=3: 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2018 - 2024 SigScale Global Inc.
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
WORD = [^\.\[\]\{\}\,\"\*=<>]*

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
{WORD} : {token, {word, TokenLine, TokenChars}}.
\. : {token, {'.', TokenLine}}.
\[ : {token, {'[', TokenLine}}.
\] : {token, {']', TokenLine}}.
\{ : {token, {'{', TokenLine}}.
\} : {token, {'}', TokenLine}}.
\, : {token, {',', TokenLine}}.
\" : {token, {'"', TokenLine}}.

Erlang code.

