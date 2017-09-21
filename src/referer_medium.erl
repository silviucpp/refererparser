-module(referer_medium).

-include("refererparser.hrl").

-export([
    from_string/1,
    to_string/1
]).

from_string(<<"unknown">>) ->
    ?MEDIUM_UNKNOWN;
from_string(<<"search">>) ->
    ?MEDIUM_SEARCH;
from_string(<<"social">>) ->
    ?MEDIUM_SOCIAL;
from_string(<<"email">>) ->
    ?MEDIUM_EMAIL;
from_string(<<"internal">>) ->
    ?MEDIUM_INTERNAL.

to_string(?MEDIUM_UNKNOWN) ->
    <<"unknown">>;
to_string(?MEDIUM_SEARCH) ->
    <<"search">>;
to_string(?MEDIUM_SOCIAL) ->
    <<"social">>;
to_string(?MEDIUM_EMAIL) ->
    <<"email">>;
to_string(?MEDIUM_INTERNAL) ->
    <<"internal">>.
