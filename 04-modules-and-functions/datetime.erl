-module(datetime).
-record(datetime, {year,
                   month,
                   day,
                   hour=0,
                   minute=0,
                   second=0,
                   microsecond=0}).
-export([datetime/3,
         datetime/4,
         datetime/5,
         datetime/6,
         datetime/7,
         fromtimestamp/1,
         today/0,
         utcfromtimestamp/1,
         utcnow/0]).

% Public API

datetime(Year, Month, Day) ->
    #datetime{year=Year, month=Month, day=Day}.

datetime(Year, Month, Day, Hour) ->
    T = datetime(Year, Month, Day),
    T#datetime{hour=Hour}.

datetime(Year, Month, Day, Hour, Minute) ->
    T = datetime(Year, Month, Day),
    T#datetime{hour=Hour, minute=Minute}.

datetime(Year, Month, Day, Hour, Minute, Second) ->
    T = datetime(Year, Month, Day),
    T#datetime{hour=Hour, minute=Minute, second=Second}.

datetime(Year, Month, Day, Hour, Minute, Second, Microsecond) ->
    T = datetime(Year, Month, Day),
    T#datetime{hour=Hour, minute=Minute, second=Second, microsecond=Microsecond}.

fromtimestamp(S) when is_float(S) ->
    datetime_from_timestamp_func(S, {calendar, now_to_local_time}).

today() ->
    fromtimestamp(erlang:system_time(micro_seconds) / 1000000).

utcnow() ->
    utcfromtimestamp(erlang:system_time(micro_seconds) / 1000000).

utcfromtimestamp(S) when is_float(S) ->
    datetime_from_timestamp_func(S, {calendar, now_to_universal_time}).

% Private functions

datetime({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    datetime(Year, Month, Day, Hour, Minute, Second).

datetime_from_timestamp_func(S, {M, F}) when is_float(S) ->
    Sec = trunc(S),
    Micro = round((S - Sec) * 1000000),
    D = datetime(M:F({0, Sec, 0})),
    D#datetime{microsecond=Micro}.
