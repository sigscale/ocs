%%% ocs_eap_codec.erl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 SigScale Global Inc.
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
%%% @doc This library module implements the public API for the
%%% 	{@link //ocs. ocs} application.
%%%
-module(ocs_eap_codec).
-copyright('Copyright (c) 2016 SigScale Global Inc.').

%% export the ocs public API
-export([packet/1]).

-include("ocs_eap_codec.hrl").

-spec packet(Packet :: binary() | #eap_packet{}) -> #eap_packet{} | binary().
%% @doc Encode or decode an EAP packet transported in the RADIUS `EAP-Message'
%% attribute.
packet(<<Code, Identifier,_Length:16, Data/binary>> = Packet)
		when size(Packet) =:= Length ->
	#eap_packet{code = Code, identifier = Identifier, data = Data};	
packet(#eap_packet{code = Code, identifier = Identifier,
		data = Data} ) when is_integer(Code), is_integer(Identifier),
		is_binary(Data) ->
	Length = size(Data) + 32,
	<<Code, Identifier, Length:16, Data>>.

