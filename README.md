# haskord


Create `config.yaml` with the contents

```yaml
botToken: <your bot token>
```

Then just `stack build --fast --exec haskord`.

# Known issues

* Resuming *may* work, but probably not. 
* After updating the logging stuff and updating the renderer for it, it doesn't work properly
  - Pressing keys updates the rendered output, but it's not possible to select any element in the list for some reason.
* There are several empty or nearly empty modules because there are so many mutual dependencies between the types
  that it's nearly impossible to split them up without running into cyclic module imports.
* It only builds on linux or other systems that can use the `vty` library because of `brick`
  - This should probably be optional

# TODO

* There are large parts of the protocol that haven't been implemented, which results in the numerous "Raw parse error"s in the log. Adding support for the rest is fairly simple, see below
* Implement the HTTP API so the bot can actually do things other than send a message
  - See `Http.hs`
  - If we can make it easy to add an endpoint, that would be great. 
* Improve plugin type
  - Add a type level string parameter for the name
  - Add a help parameter for help info

# Adding support for more messages in the protocol

How this works is best explained by example.
Let's say we want to add support for the `PRESENCE_UPDATE` dispatch event.
We start by going to the `Payload` type in Plugins.hs. This is a indexed GADT which will have a constructor
for each different payload. We modify as follows:

We add:

```haskell
-- We start with this
data Payload :: GatewayOpcode -> Maybe EventType -> * where
    HelloPayload         :: Heartbeat' -> RawPayload 'Hello
    MessageCreatePayload :: Message -> DispatchPayload 'MESSAGE_CREATE
    ReadyPayload         :: Ready   -> DispatchPayload 'READY
    
    
-- We end with this

data Payload :: GatewayOpcode -> Maybe EventType -> * where
    HelloPayload          :: Heartbeat' -> RawPayload 'Hello
    MessageCreatePayload  :: Message -> DispatchPayload 'MESSAGE_CREATE
    ReadyPayload          :: Ready   -> DispatchPayload 'READY
    PresenceUpdatePayload :: PresenceUpdate -> DispatchPayload 'PRESENCE_UPDATE -- new line
```

Note that `PresenceUpdate` is already defined in `Types.Common` in this case, and that should be the case for most
of the JSON objects sent to us by discord. If it's not already defined, you should define one yourself, ensuring that
you derive the JSON instances as is done for the other types:

```haskell
data SomeType
     = SomeType
     { ...
     }

instance ToJSON SomeType where
    toJSON = genericToJSON decodingOptions
instance FromJSON SomeType where
    parseJSON = genericParseJSON decodingOptions
```

Now we need to be able to parse the payload, which we do by modifying `parseEventPayload`:

```haskell
parseEventPayload :: forall opcode event. Sing opcode -> Sing event -> Value -> Parser (Payload opcode event)
parseEventPayload SDispatch (SJust SMESSAGE_CREATE) val = MessageCreatePayload <$> parseJSON val
parseEventPayload SDispatch (SJust SREADY)          val = ReadyPayload <$> parseJSON val
parseEventPayload SHello    SNothing                val = HelloPayload <$> parseJSON val
parseEventPayload SDispatch (SJust SPRESENCE_UPDATE) val = PresenceUpdatePayload <$> parseJSON val
parseEventPayload _ _ _ = fail "Can't parse payload"
```

This is all that is needed. Try to compile and everything should type-check, and the bot should be able to parse presence update messages.

# Plugins
 
TODO -- see Plugins.Default and Plugins.Resources (the latter is horrible and buggy so beware)
