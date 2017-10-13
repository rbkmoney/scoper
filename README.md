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
* Scope log metadata along the call stack, wrapping functions in [scoper](src/scoper.erl)`:scope/2,3`.
* Add/remove scope metadata via [scoper](src/scoper.erl)`:add_meta/1,2` and [scoper](src/scoper.erl)`:remove_meta/1,2`
* Use [lager](https://github.com/erlang-lager/lager) or just process dictionary (by default) as metadata storage.
* Implement you own storage as [scoper_logger](src/scoper_logger.erl) behaviour.
* Metadata storage is configured via apllication environment variale `logger`, which should name the module implementing `scoper_logger` behaviour. E.g.:

    ```erlang
    {scoper, [
        {logger, scoper_logger_lager}
    ]}.
    ```

* Enjoy tailored [scoper_woody_event_handler](src/scoper_woody_event_handler.erl). Configure sope names for woody client and server via _woody_event_handler_ options (note, scope name should be an _atom_):

    ```erlang
    #{
        client_scope => 'my_client_scope',
        server_scope => 'my_server_scope'
    }
    ```
    Default values are `woody.client` and `woody.server`.

> Note, if using `lager` metadata storage or/and `woody` event handler make sure to properly setup corresponding applications in your `app.src` file, since they are optional for `scoper` and not stated as it's dependencies.
