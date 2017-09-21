This is a Erlang library for extracting marketing attribution data from referrer URLs based on [referer-parser][1] database 

### Getting started:

##### Integration

The application is compatible with both `rebar` or `rebar3`. Add `erluap` as a rebar dependency to your project:

```
{deps, [
  {refererparser, ".*", {git, "https://github.com/silviucpp/refererparser.git", "master"}},
}.
```

##### Update database

The latest database of referers can be downloaded from [referer-parser][1] repo. This file should replace `priv/referers.json`.
Also it's ok to replace the `referer-tests.json` as well with the last file once you update the referers database and to rerun the tests
in order to check that nothing broken.

### API

All available mediums are inside `refererparser.hrl` as follow:

```erlang
-define(MEDIUM_UNKNOWN, unknown).
-define(MEDIUM_INTERNAL, internal).
-define(MEDIUM_SEARCH, search).
-define(MEDIUM_SOCIAL, social).
-define(MEDIUM_EMAIL, email).
```

The referer is returned in a record defined as follow:

```erlang

-record(referer, {
    medium ::medium(),
    source = null :: ref_source(),
    term = null :: ref_term()
}).

```

In order to get the referer you can use the `parse/2` method where you can specify The referrer URL to parse and 
the URL of the current page.

```
refererparser:start().
refererparser:parse(
    <<"http://www.google.com/search?q=gateway+cards&client=safari">>, <<"http://my-web.com">>).
{ok,{referer,search,<<"Google">>, <<"gateway cards">>}}
```

### Tests

In order to run the tests just use `make ct` from project root after you compiled and got the deps using `rebar`

[1]:https://github.com/snowplow/referer-parser
