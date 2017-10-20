# scoper

Log metadata scoping Erlang library.

## About
From [Urban Dictionary](http://www.urbandictionary.com):

[Scoper](http://www.urbandictionary.com/define.php?term=Scoper) - another word for [spastic](http://www.urbandictionary.com/define.php?term=spastic). Came into use when the Spastic's Society changed it's name to Scope.

```
You Scoper!
You're such a Scoper!
Scoooper!
```

## Usage
* Scope log metadata along the process call stack, wrapping functions in [scoper](src/scoper.erl)`:scope/2,3`. _Scope name_ should be an atom, _metadata_ is a map with atom keys. The types:

    ```erlang
    -type key()   :: atom().
    -type value() :: any().
    -type meta()  :: #{key() => value()}.
    -type scope() :: key().
    ```

* Add/remove scope metadata via [scoper](src/scoper.erl)`:add_meta/1` and [scoper](src/scoper.erl)`:remove_meta/1`
* Use [lager](https://github.com/erlang-lager/lager) or just process dictionary (by default) as metadata storage.
* Implement your own storage as [scoper_storage](src/scoper_storage.erl) behaviour. Metadata storage is configured via application environment variable `storage`, which should name the module implementing `scoper_storage` behaviour. E.g.:

    ```erlang
    {scoper, [
        {storage, scoper_storage_lager}
    ]}.
    ```

* Enjoy tailored [scoper_woody_event_handler](src/scoper_woody_event_handler.erl):

    ```erlang
    %% woody client or server config
    event_handler => scoper_woody_event_handler,
    ```
    Scope names for woody client and server are: `rpc.client` and `rpc.server`.

> Note, if using `lager` metadata storage or/and `woody` event handler make sure to properly setup corresponding applications in your `app.src` file, since they are optional for `scoper` and not configured as it's application dependencies.
