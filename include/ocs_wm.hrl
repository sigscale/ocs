%%% osc_wm.hrl 

-include_lib("webmachine/include/webmachine.hrl").

-type rd() :: #wm_reqdata{}.
-type halt() :: {error, term()} | {halt, 200..599}.
-type state() :: term().
-type streambody() :: {iodata(), fun(() -> streambody()) | done}.

