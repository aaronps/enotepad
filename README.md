# enotepad
Clone of MS Notepad using Erlang (wxWidgets)

![Enotepad screenshot](screenshot.png)

## Motivation

In the process of learning Erlang, I wanted to make some GUI application, so I
though *I will make a Notepad clone, after all it is just a text control...*
it wasn't.

Things learned:

* how to use wxWidgets, wx_object and Scintilla
  * using menus, status bar
  * custom dialogs and simple dialogs
  * printing on a printer
* how to make a wxWidgets application using OTP design principles.
* how to build _escripts_ and applications manually
* file handling, creating/removing directories
* dialyzer and types
* how to make releases with systools and with reltool

## Requirement

- Erlang >= 19.1

## Installation

This program is not intended to be installed, its a learning tool.

Anyway, on Windows there is an installer, but for other systems you must build
from source.

## Building

This project has been converted to use rebar3 to compile and build the releases.

Extra `rebar3` commands and their effects:

    reltool    - makes a release using reltool
    systools   - makes a release using systools
    release    - do not use, this is the default of rebar3 and uses relx which is not supported
    clean      - extended clean which deletes more files.
    escriptize - This is the default rebar3 escriptize but embeds the app icon within the escript.
    winescript - This extedns escriptize to generate special versions for windows

Get more details for `winescript` using `rebar3 help winescript`.
    
## Running

You can run enotepad in multiple ways: using Erlang shell, using the built
_escript_ and using a generated release.

### Using Erlang shell

Using `rebar3 shell` will start a shell with the required libraries on path, you have
multiple options to run it:
 
Using main like the _escript_:

```
Eshell V8.1  (abort with ^G)
1> enotepad:main().
ok
2> enotepad:main(["some_file_name.txt"]).
ok
```

Using `start_link` which is used when running under supervisor, but you can use
too:

```
Eshell V8.1  (abort with ^G)
1> enotepad:start_link().
ok
2> enotepad:start_link("some_file_name.txt").
ok
```

Starting like an application:

```
Eshell V8.1  (abort with ^G)
1> application:start(wx), application:start(enotepad).
ok
2> application:set_env(enotepad, file, "some_file_name.txt").
ok
3> application:start(enotepad).     <<-- this will load some_file_name.txt
ok
```

When running like an application, you can stop with
`application:stop(enotepad)` but this is not needed if you just close the
window.

### Using the _escript_

This way requires you to have a working installation of Erlang on the path, then
just run it by name with optional parameter:

```
> path/to/enotepad <file_name>
```

### Using a built release

This doesn't require Erlang as it already includes it, just run it by name like
the _escript_, the actual file you run is a facade to hide the erl parameters.
