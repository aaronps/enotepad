enotepad_build_plugin
=====

A rebar plugin

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        { enotepad_build_plugin, ".*", {git, "git@host:user/enotepad_build_plugin.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 enotepad_build_plugin
    ===> Fetching enotepad_build_plugin
    ===> Compiling enotepad_build_plugin
    <Plugin Output>
