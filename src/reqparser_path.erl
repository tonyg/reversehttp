-module(reqparser_path).

-export([analyse/2, ext_url/3]).

analyse(Req, Config) ->
    case reversehttp:match_access_point(Req, Config) of
        V = {access_point, _, _, _} ->
            V;
        {normal, Path} ->
            case extract_label(Path) of
                {ok, HostLabel} ->
                    {single_request, ext_url(Req, Config, HostLabel)};
                {error, no_slash} ->
                    {normal, Path}
            end
    end.

extract_label(Path) ->
    case string:chr(Path, $/) of
        0 ->
            {error, no_slash};
        N ->
            {ok, string:left(Path, N - 1)}
    end.

ext_url(Req, Config, HostLabel) ->
    "http://" ++ string:to_lower(reversehttp:host(Req, Config)) ++ "/" ++ HostLabel ++ "/".
