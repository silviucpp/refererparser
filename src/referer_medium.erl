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
    <<"UNKNOWN">>;
to_string(?MEDIUM_SEARCH) ->
    <<"SEARCH">>;
to_string(?MEDIUM_SOCIAL) ->
    <<"SOCIAL">>;
to_string(?MEDIUM_EMAIL) ->
    <<"EMAIL">>;
to_string(?MEDIUM_INTERNAL) ->
    <<"INTERNAL">>.
