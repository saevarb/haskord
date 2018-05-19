# haskord


Create `config.yaml` with the contents

```yaml
botToken: <your bot token>
```

Then just `stack build --fast --exec haskord`.

# TODO

* Give `Snowflake` a phantom type parameter to indicate which kind of ID we have for extra type safety.
* Come up with some kind of type safe plugin API
* Figure out what kind of plugins we need
  - Do we allow plugins for "raw" commands as well(actual gateway messages)
* Create some kind of persistence layer for plugins and their state for plugins that want to
  keep state
  - Maybe just use SQLite as a backend using a table as some sort of KV store?
* There are still large parts of the gateway protocol which we aren't parsing because the types for it haven't been defined. These can be found by searching for `Value` (see `Common.hs` line 124 for example). 
  1. Define a type corresponding to the one in the discord api docs
  2. Add the correct instances ToJSON/FromJSON
  3. Make sure everything compiles
  4. Run and watch for parse errors -- no errors and we're good(probably) (errors are written to `log`)
* Implement the HTTP API so the bot can actually do things other than send a message
  - See `Http.hs`
  - If we can make it easy to add an endpoint, that would be great. 
