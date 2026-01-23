
Margin is a human-readable file format to encode data containing a
timestamp, a textual description and a numerical value.

This repo contains parsers and a few commands related to the format.

The [exmargination](https://github.com/danse/exmargination/) project
provides visualisation commands for margin files.

##### dataframe

The time formatting string is a bit difficult to guess, so here it is:

```
dataframe> r = D.defaultReadOptions { D.dateFormat = "%FT%T%QZ" }
dataframe> D.readCsvWithOpts r "data.margin" 
```

Another way is parsing after reading (the formatting string here is
not the right one for margin):

```
parseUtc :: String -> Maybe UTCTime
parseUtc s =
  zonedTimeToUTC <$> parseTimeM True defaultTimeLocale fmt s
  where
    fmt = "%Y-%m-%dT%H:%M:%S%Q%Ez"

df |> D.derive "time" (F.lift (parseUtc . T.unpack) event_time)
```