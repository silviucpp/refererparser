
%mediums

-define(MEDIUM_UNKNOWN, unknown).
-define(MEDIUM_INTERNAL, internal).
-define(MEDIUM_SEARCH, search).
-define(MEDIUM_SOCIAL, social).
-define(MEDIUM_EMAIL, email).

-type medium() :: ?MEDIUM_UNKNOWN | ?MEDIUM_INTERNAL | ?MEDIUM_SEARCH | ?MEDIUM_SOCIAL | ?MEDIUM_EMAIL.
-type ref_source() :: binary() | null.
-type ref_term() :: binary() | null.
-type reason() :: any().

-record(referer, {
    medium ::medium(),
    source = null :: ref_source(),
    term = null :: ref_term()
}).

-type referer() :: #referer{}.