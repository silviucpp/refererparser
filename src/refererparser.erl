-module(refererparser).

-include("refererparser.hrl").

-export([

    start/0,
    start/1,
    stop/0,

    parse/2,
    match_hosts/2
]).

-spec start() ->
    ok  | {error, reason()}.

start() ->
    start(temporary).

-spec start(permanent | transient | temporary) ->
    ok | {error, reason()}.

start(Type) ->
    case application:ensure_all_started(refererparser, Type) of
        {ok, _} ->
            ok;
        Other ->
            Other
    end.

-spec stop() ->
    ok.

stop() ->
    application:stop(refererparser).

-spec parse(binary(), binary()) ->
    {ok, referer()} | {error, reason()}.

parse(RefUriBin, PageUriBin) ->
    UriRef = liburi:from_string(RefUriBin),
    PageUri = liburi:from_string(PageUriBin),
    parse(liburi:scheme(UriRef), liburi:host(UriRef), liburi:path(UriRef), liburi:q(UriRef), liburi:host(PageUri)).

-spec parse(binary(), binary(), binary(), list(), binary()) ->
    {ok, referer()} | {error, reason()}.

parse(Scheme, Host, Path, Query, PageHost) ->
    case validation(Scheme, Host) of
        ok ->
            case match_hosts(Host, PageHost) of
                true ->
                    {ok, #referer{medium = ?MEDIUM_INTERNAL}};
                _ ->
                    case get_referer(Host, Path) of
                        undefined ->
                            {ok, #referer{medium = ?MEDIUM_UNKNOWN}};
                        {ok, {Medium, Source, Params}} ->
                            {ok, #referer{medium = Medium, source = Source, term = get_search_term(Medium, Query, Params)}}
                    end
            end;
        Error ->
            Error
    end.

% internals

-spec validation(binary(), binary()) ->
    ok | {error, reason()}.

validation(Scheme, Host) ->
    case valid_scheme(Scheme) of
        true ->
            case valid_host(Host) of
                true ->
                    ok;
                _ ->
                    {error, <<"Invalid host">>}
            end;
        _ ->
            {error, <<"Invalid scheme">>}
    end.

-spec valid_host(binary()) ->
    boolean().

valid_host(<<>>) ->
    false;
valid_host(_) ->
    true.

-spec valid_scheme(binary()) ->
    boolean().

valid_scheme(<<"https">>) ->
    true;
valid_scheme(<<"http">>) ->
    true;
valid_scheme(_) ->
    false.

-spec get_referer(binary(), binary()) ->
    {ok, referer()} | undefined.

get_referer(Host, Path) ->
    % try to lookup our referer. First check with paths, then without.
    % this is the safest way of handling lookups
    case referer_cache:lookup(Host, Path, true) of
        undefined ->
            referer_cache:lookup(Host, Path, false);
        R ->
            R
    end.

-spec get_search_term(medium(), list(), list()) ->
    binary() | null.

get_search_term(?MEDIUM_SEARCH, Query, Params) ->
    find_term(Query, Params);
get_search_term(_Medium, _Query, _Params) ->
    null.

-spec find_term(list(), list()) ->
    binary() | null.

find_term([{K, V}|T], Params) ->
    case lists:member(K, Params) of
        true ->
            V;
        _ ->
            find_term(T, Params)
    end;
find_term([], _Params) ->
    null.

-spec match_hosts(binary(), binary()) ->
    boolean().

match_hosts(A0, B0) ->
    A = normalise_hosts(A0),
    B = normalise_hosts(B0),

    ALength = byte_size(A),
    BLength = byte_size(B),
    DiffLength = erlang:max(0, ALength - BLength),

    case A of
        <<_:DiffLength/binary, B/binary>> ->
            true;
        _ ->
            false
    end.

-spec normalise_hosts(binary()) ->
    binary().

normalise_hosts(<<"www.", Rest/binary>>) ->
    Rest;
normalise_hosts(H) ->
    H.
