-module(referer_utils).

-export([
    get_priv_path/1,
    lookup/2,
    lookup/3,
    join/2
]).

get_priv_path(File) ->
    case code:priv_dir(refererparser) of
        {error, bad_name} ->
            Ebin = filename:dirname(code:which(?MODULE)),
            filename:join([filename:dirname(Ebin), "priv", File]);
        Dir ->
            filename:join(Dir, File)
    end.

lookup(Key, List) ->
    lookup(Key, List, undefined).

lookup(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
        {Key, Result}
            -> Result;
        false ->
            Default
    end.

join([Head | Tail], Sep) ->
    join_list_sep(Tail, Sep, [Head]);
join([], _Sep) ->
    <<>>.

join_list_sep([Head | Tail], Sep, Acc) ->
    join_list_sep(Tail, Sep, [Head, Sep | Acc]);
join_list_sep([], _Sep, Acc) ->
    list_to_binary(lists:reverse(Acc)).