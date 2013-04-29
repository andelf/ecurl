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

-define(LONG_OPT, 0).
-define(OBJECTPOINT_OPT, 10000).
-define(FUNCTIONPOINT_OPT, 20000).
-define(OFFT_OPT, 30000).

-define(OPTION_CODES, [
    {writedata, ?OBJECTPOINT_OPT + 1},
    {url, ?OBJECTPOINT_OPT + 2},
    {port, ?LONG_OPT + 3},
    {proxy, ?OBJECTPOINT_OPT + 4},
    {proxyuserpwd, ?OBJECTPOINT_OPT + 6},
    {range, ?OBJECTPOINT_OPT + 7},
    {infile, ?OBJECTPOINT_OPT + 9},
    {readdata, ?OBJECTPOINT_OPT + 9},
    {errorbuffer, ?OBJECTPOINT_OPT + 10},
    {writefunction, ?FUNCTIONPOINT_OPT + 11},
    {readfunction, ?FUNCTIONPOINT_OPT + 12},
    {timeout, ?LONG_OPT + 13},
    {infilesize, ?LONG_OPT + 14},
    {postfields, ?OBJECTPOINT_OPT + 15},
    {referer, ?OBJECTPOINT_OPT + 16},
    {ftpport, ?OBJECTPOINT_OPT + 17},
    {useragent, ?OBJECTPOINT_OPT + 18},
    {low_speed_limit, ?LONG_OPT + 19},
    {low_speed_time, ?LONG_OPT + 20},
    {resume_from, ?LONG_OPT + 21},
    {cookie, ?OBJECTPOINT_OPT + 22},
    {httpheader, ?OBJECTPOINT_OPT + 23},
    {httppost, ?OBJECTPOINT_OPT + 24},
    {sslcert, ?OBJECTPOINT_OPT + 25},
    {sslcertpasswd, ?OBJECTPOINT_OPT + 26},
    {sslkeypasswd, ?OBJECTPOINT_OPT + 26},
    {crlf, ?LONG_OPT + 27},
    {quote, ?OBJECTPOINT_OPT + 28},
    {writeheader, ?OBJECTPOINT_OPT + 29},
    {headerdata, ?OBJECTPOINT_OPT + 29},
    {cookiefile, ?OBJECTPOINT_OPT + 31},
    {sslversion, ?LONG_OPT + 32},
    {timecondition, ?LONG_OPT + 33},
    {timevalue, ?LONG_OPT + 34},
    {customrequest, ?OBJECTPOINT_OPT + 36},
    {stderr, ?OBJECTPOINT_OPT + 37},
    {postquote, ?OBJECTPOINT_OPT + 39},
    {writeinfo, ?OBJECTPOINT_OPT + 40},
    {verbose, ?LONG_OPT + 41},
    {header, ?LONG_OPT + 42},
    {noprogress, ?LONG_OPT + 43},
    {nobody, ?LONG_OPT + 44},
    {failonerror, ?LONG_OPT + 45},
    {upload, ?LONG_OPT + 46},
    {post, ?LONG_OPT + 47},
    {ftplistonly, ?LONG_OPT + 48},
    {ftpappend, ?LONG_OPT + 50},
    {netrc, ?LONG_OPT + 51},
    {followlocation, ?LONG_OPT + 52},
    {transfertext, ?LONG_OPT + 53},
    {put, ?LONG_OPT + 54},
    {progressfunction, ?FUNCTIONPOINT_OPT + 56},
    {progressdata, ?OBJECTPOINT_OPT + 57},
    {autoreferer, ?LONG_OPT + 58},
    {proxyport, ?LONG_OPT + 59},
    {postfieldsize, ?LONG_OPT + 60},
    {httpproxytunnel, ?LONG_OPT + 61},
    {interface, ?OBJECTPOINT_OPT + 62},
    {krb4level, ?OBJECTPOINT_OPT + 63},
    {ssl_verifypeer, ?LONG_OPT + 64},
    {cainfo, ?OBJECTPOINT_OPT + 65},
    {maxredirs, ?LONG_OPT + 68},
    {filetime, ?OBJECTPOINT_OPT + 69},
    {telnetoptions, ?OBJECTPOINT_OPT + 70},
    {maxconnects, ?LONG_OPT + 71},
    {closepolicy, ?LONG_OPT + 72},
    {fresh_connect, ?LONG_OPT + 74},
    {forbid_reuse, ?LONG_OPT + 75},
    {random_file, ?OBJECTPOINT_OPT + 76},
    {egdsocket, ?OBJECTPOINT_OPT + 77},
    {connecttimeout, ?LONG_OPT + 78},
    {headerfunction, ?FUNCTIONPOINT_OPT + 79},
    {httpget, ?LONG_OPT + 80},
    {ssl_verifyhost, ?LONG_OPT + 81},
    {cookiejar, ?OBJECTPOINT_OPT + 82},
    {ssl_cipher_list, ?OBJECTPOINT_OPT + 83},
    {http_version, ?LONG_OPT + 84},
    {ftp_use_epsv, ?LONG_OPT + 85},
    {sslcerttype, ?OBJECTPOINT_OPT + 86},
    {sslkey, ?OBJECTPOINT_OPT + 87},
    {sslkeytype, ?OBJECTPOINT_OPT + 88},
    {sslengine, ?OBJECTPOINT_OPT + 89},
    {sslengine_default, ?LONG_OPT + 90},
    {dns_use_global_cache, ?LONG_OPT + 91},
    {dns_cache_timeout, ?LONG_OPT + 92},
    {prequote, ?OBJECTPOINT_OPT + 93},
    {debugfunction, ?FUNCTIONPOINT_OPT + 94},
    {debugdata, ?OBJECTPOINT_OPT + 95},
    {cookiesession, ?LONG_OPT + 96},
    {capath, ?OBJECTPOINT_OPT + 97},
    {buffersize, ?LONG_OPT + 98},
    {nosignal, ?LONG_OPT + 99},
    {share, ?OBJECTPOINT_OPT + 100},
    {proxytype, ?LONG_OPT + 101},
    {encoding, ?OBJECTPOINT_OPT + 102},
    {private, ?OBJECTPOINT_OPT + 103},
    {http200aliases, ?OBJECTPOINT_OPT + 104},
    {unrestricted_auth, ?LONG_OPT + 105},
    {ftp_use_eprt, ?LONG_OPT + 106},
    {httpauth, ?LONG_OPT + 107},
    {ssl_ctx_function, ?FUNCTIONPOINT_OPT + 108},
    {ssl_ctx_data, ?OBJECTPOINT_OPT + 109},
    {ftp_create_missing_dirs, ?LONG_OPT + 110},
    {proxyauth, ?LONG_OPT + 111},
    {ftp_response_timeout, ?LONG_OPT + 112},
    {ipresolve, ?LONG_OPT + 113},
    {maxfilesize, ?LONG_OPT + 114},
    {infilesize_large, ?OFFT_OPT + 115},
    {resume_from_large, ?OFFT_OPT + 116},
    {maxfilesize_large, ?OFFT_OPT + 117},
    {netrc_file, ?OBJECTPOINT_OPT + 118},
    {ftp_ssl, ?LONG_OPT + 119},
    {postfieldsize_large, ?OFFT_OPT + 120},
    {tcp_nodelay, ?LONG_OPT + 121}]).

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
    case proplists:get_value(Option, ?OPTION_CODES) of
        undefined ->
            {error, bad_option_key};
        OptVal ->
            easy_setopt_wrapper(Handle, OptVal, Value)
    end.

easy_setopt_wrapper(_, _, _) ->
    ok.

easy_perform(_) ->
    ok.

debug(_) ->
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================
