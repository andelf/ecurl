%%%-------------------------------------------------------------------
%%% @author Wang ShuYu <andelf@gmail.com>
%%% @copyright (C) 2013, Wang ShuYu
%%% @doc
%%%
%%% @end
%%% Created : 29 Apr 2013 by Wang ShuYu <andelf@gmail.com>
%%%-------------------------------------------------------------------
-module(ecurl).
-on_load(init/0).

%% API
-export([easy_init/0, easy_cleanup/1]).

-compile([export_all]).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    ok = erlang:load_nif("./priv/ecurl_drv", 0).

easy_init() ->
    ok.

easy_cleanup(_) ->
    ok.

easy_duphandle(_) ->
    ok.

easy_setopt(Handle, Option, Value) ->
    ok.

easy_perform(_) ->
    ok.

debug(_) ->
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================
