-module(reqparser_vhost).

-export([analyse/2, ext_url/3]).

analyse(Req, Config) ->
    case Req:get_header_value(host) of
        undefined ->
            {error, 400, "Missing Host HTTP header"};
        MixedCaseHost ->
            Host = string:to_lower(MixedCaseHost),
            AccessHosts = reversehttp:lookup(access_point_hosts, Config, []),
            case lists:member(Host, AccessHosts) of
                true ->
                    reversehttp:match_access_point(Req, Config);
                false ->
                    {single_request, ext_url1(Host)}
            end
    end.

ext_url(Req, _Config, HostLabel) ->
    ext_url1(HostLabel ++ "." ++ string:to_lower(Req:get_header_value(host))).

ext_url1(LcHost) ->
    "http://" ++ LcHost ++ "/".
