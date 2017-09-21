-module(integrity_test_SUITE).

-include("refererparser.hrl").

-define(TEST_FILE, <<"../../test/referer-tests.json">>).

-compile(export_all).

all() -> [
    {group, referers}
].

groups() -> [
    {referers, [sequence], [
        test_referers
    ]}
].

init_per_suite(Config) ->
    refererparser:start(),
    Config.

end_per_suite(_Config) ->
    refererparser:stop().

test_referers(_Config) ->
    {ok, FileBin} = file:read_file(?TEST_FILE),
    Json = referer_json:decode(FileBin),
    test_referer(Json).

test_referer([H|T]) ->
    Uri = referer_utils:lookup(<<"uri">>, H),
    Medium = referer_medium:from_string(referer_utils:lookup(<<"medium">>, H)),
    Source = referer_utils:lookup(<<"source">>, H),
    Term = referer_utils:lookup(<<"term">>, H),
    %io:format("## ~p ~n", [[Uri, Medium, Source, Term]]),
    {ok, #referer{medium = Medium, source = Source, term = Term}} = refererparser:parse(Uri, <<"www.snowplowanalytics.com">>),
    test_referer(T);
test_referer([]) ->
    true.
