-module(referer_cache).

-include("refererparser.hrl").

-define(REFERERS_FILE, <<"referers.json">>).

-export([
    load/0,
    lookup/3
]).

-define(REFERER_CACHE_TAB, referer_cache_tab).

load() ->
    {ok, FileBin} = file:read_file(referer_utils:get_priv_path(?REFERERS_FILE)),
    Refs = referer_json:decode(FileBin),
    Md = parse_mediums(Refs, []),
    ?REFERER_CACHE_TAB = ets:new(?REFERER_CACHE_TAB, [set, named_table, public, {read_concurrency, true}]),
    true = ets:insert(?REFERER_CACHE_TAB, Md).

lookup(Host, Path, IncludePath) ->
    %io:format("##search for: ~p ~n", [[Host, Path, IncludePath]]),

    case get_referer(Host, Path, IncludePath) of
        {ok, _Referer} = R ->
            R;
        undefined ->
            case remove_subdomain(Host) of
                {ok, NewHost} ->
                    lookup(NewHost, Path, IncludePath);
                undefined ->
                    undefined
            end
    end.

% internals

parse_mediums([{MediumBin, Sources}|T], Acc) ->

    Medium = referer_medium:from_string(MediumBin),

    FunSources = fun({SourceName, Args}, SrcAcc) ->
        Domains = referer_utils:lookup(<<"domains">>, Args),
        Parameters = referer_utils:lookup(<<"parameters">>, Args),

        ok = validate_params(Medium, SourceName, Parameters),
        ok = validate_domains(Domains, SourceName),

        lists:foldl(fun(Domain, DAcc) -> [{Domain, {Medium, SourceName, Parameters}} | DAcc] end, SrcAcc, Domains)
    end,

    parse_mediums(T, lists:foldl(FunSources, Acc, Sources));
parse_mediums([], Acc) ->
    Acc.

validate_params(?MEDIUM_SEARCH, Source, Params) ->
    case Params of
        undefined ->
            throw({error, <<"No parameters found for search referer: '", Source/binary,"'">>});
        _ ->
            ok
    end;
validate_params(_Medium, Source, Params) ->
    case Params of
        undefined ->
            ok;
        _ ->
            throw({error, <<"Parameters not supported for non-search referer: '", Source/binary, "'">>})
    end.

validate_domains(undefined, Source) ->
    throw({error, <<"No domains found for referer: '", Source/binary, "'">>});
validate_domains(_Domains, _Source) ->
    ok.

get_referer(Host, Path, IncludePath) ->
    % Check if domain+full path matches, e.g. for apollo.lv/portal/search/
    case ets_get(get_search_term(IncludePath, Host, Path)) of
        undefined ->
            % Check if domain+one-level path matches, e.g. for orange.fr/webmail/fr_FR/read.html
            % (in our JSON it's orange.fr/webmail)
            case get_path_level_one(Path) of
                undefined ->
                    undefined;
                Level1Path ->
                    ets_get(get_search_term(IncludePath, Host, Level1Path))
            end;
        Ref ->
            Ref
    end.

get_search_term(true, Host, Path) ->
    <<Host/binary, Path/binary>>;
get_search_term(false, Host, _Path) ->
    Host.

get_path_level_one(Path) ->
    case binary:split(Path, <<"/">>, [global]) of
        [<<>>, Level1 | _T] ->
            <<"/", Level1/binary>>;
        _ ->
            undefined
    end.

remove_subdomain(Host) ->
    case binary:split(Host, <<".">>, [global]) of
        [_H|T] ->
            case T of
                [] ->
                    undefined;
                <<>> ->
                    undefined;
                Tbin when is_binary(Tbin) ->
                    {ok, T};
                _ ->
                    {ok, referer_utils:join(T, <<".">>)}
            end;
        _ ->
            undefined
    end.

ets_get(Key) ->
    %io:format("##ets get for: ~p ~n", [[Key]]),
    case ets:lookup(?REFERER_CACHE_TAB, Key) of
        [{Key, Value}] ->
            {ok, Value};
        [] ->
            undefined
    end.