%% @doc Unit Tests for jsonpath.erl
%% @author Gene Stevens <gene@triplenexus.org>
%% @copyright (c) 2012 Gene Stevens.  All Rights Reserved.
-module(jsonpath_tests).
-include_lib("eunit/include/eunit.hrl").
-export([bench/4]).

-define(JSON, <<"{\"menu\": {
  \"id\": \"file\",
  \"value\": \"File\",
  \"popup\": {
    \"menuitem\": [
      {\"value\": \"New\", \"onclick\": \"CreateNewDoc()\"},
      {\"value\": \"Open\", \"onclick\": \"OpenDoc()\"},
      {\"value\": \"Close\", \"onclick\": \"CloseDoc()\"}
    ]
  }
}}">>).




add_subnode_to_existing_nodes_test() ->
	?assertEqual(
		<<"{\"doc\":{\"key1\":\"oldvalue1\",\"key2\":\"oldvalue2\",\"newkey\":{\"newsubkey\":\"newvalue\"}}}">>,
		jiffy:encode(jsonpath:add(
			<<"doc.newkey.newsubkey">>, 
			<<"newvalue">>, 
			<<"{\"doc\":{\"key1\":\"oldvalue1\",\"key2\":\"oldvalue2\"}}">>))).

add_subnodes_to_empty_test() ->
	?assertEqual(
		<<"{\"doc\":{\"newkey\":{\"newsubkey\":\"newvalue\"}}}">>,
		jiffy:encode(jsonpath:add(
			<<"doc.newkey.newsubkey">>, 
			<<"newvalue">>, 
			<<"{}">>))).
	
add_subnodes_to_existing_root_test() ->
	?assertEqual(
		<<"{\"doc\":{\"newkey\":{\"newsubkey\":\"newvalue\"}}}">>,
		jiffy:encode(jsonpath:add(
			<<"doc.newkey.newsubkey">>, 
			<<"newvalue">>, 
			<<"{\"doc\":{}}">>))).

add_array_test() ->
	?assertEqual(
		<<"{\"doc\":{\"testkey\":\"testval\",\"chans\":[\"newvalue\"]}}">>,
		jiffy:encode(jsonpath:add(
			<<"doc.chans[100]">>,
			<<"newvalue">>,
			<<"{\"doc\":{\"testkey\":\"testval\"}}">>))).

add_to_top_with_existing_array0_test() ->
	?assertEqual(
		<<"{\"docs0\":[],\"last_replication\":\"2013\"}">>,
		jiffy:encode(jsonpath:add(
				<<"last_replication">>, 
				<<"2013">>,
				<<"{\"docs0\":[]}">>))).

add_to_top_with_existing_array1_test() ->
	?assertEqual(
		<<"{\"docs1\":[],\"test1\":\"test3\",\"last_replication\":\"2013\"}">>,
		jiffy:encode(jsonpath:add(
				<<"last_replication">>, 
				<<"2013">>,
				<<"{\"docs1\":[],\"test1\":\"test3\"}">>))).


add_to_top_with_existing_array2_test() ->
?assertEqual(
	<<"{\"docs2\":[\"test\"],\"last_replication\":\"2013\"}">>,
	jiffy:encode(jsonpath:add(
		<<"last_replication">>, 
		<<"2013">>,
		<<"{\"docs2\":[\"test\"]}">>))).


add_to_top_with_existing_array3_test() ->
?assertEqual(
	<<"{\"docs3\":[{\"_id\":\"http\"}],\"last\":\"2013\"}">>,
	jiffy:encode(jsonpath:add(
		<<"docs3[1000]">>,
		{[{<<"_id">>,<<"http">>}]},
		<<"{\"docs3\":[],\"last\":\"2013\"}">>))).



add_in_and_to_array_test() ->
	?assertEqual(
		<<"{\"doc\":{\"chans\":[{\"test\":\"newvalue\"}]}}">>,
		jiffy:encode(jsonpath:add(
			<<"doc.chans[100].test">>,
			<<"newvalue">>,
			<<"{\"doc\":{}}">>))).

add_to_end_of_array_test() ->
	?assertEqual(
		<<"{\"doc\":{\"chans\":[\"test\",\"newvalue\"]}}">>,
		jiffy:encode(jsonpath:add(
			<<"doc.chans[100]">>,
			<<"newvalue">>,
			<<"{\"doc\":{\"chans\":[\"test\"]}}">>))).



search_single_node_test() ->
	?assertEqual(<<"file">>, jsonpath:search(<<"menu.id">>, ?JSON)).

search_single_node_undefined_test() ->
	?assertEqual(undefined, jsonpath:search(<<"menu.id.noexist">>, ?JSON)).

search_array_result_test() ->
	?assertMatch([_H|_T], jsonpath:search(<<"menu.popup.menuitem">>, ?JSON)).

search_array_index_test() ->
	?assertEqual({[{<<"value">>,<<"Open">>},
			{<<"onclick">>,<<"OpenDoc()">>}]},
		jsonpath:search(<<"menu.popup.menuitem[1]">>, ?JSON)).

replace_single_node_test() ->
	?assertEqual(
		{[{<<"menu">>,
		   {[{<<"id">>,<<"foo">>},
			 {<<"value">>,<<"File">>},
			 {<<"popup">>,
			  {[{<<"menuitem">>,
				 [{[{<<"value">>,<<"New">>},
					{<<"onclick">>,<<"CreateNewDoc()">>}]},
				  {[{<<"value">>,<<"Open">>},{<<"onclick">>,<<"OpenDoc()">>}]},
				  {[{<<"value">>,<<"Close">>},
					{<<"onclick">>,<<"CloseDoc()">>}]}]}]}}]}}]},
		jsonpath:replace(<<"menu.id">>, <<"foo">>, ?JSON)).

replace_array_index_test() ->
	?assertEqual(
		{[{<<"menu">>,
		   {[{<<"id">>,<<"file">>},
			 {<<"value">>,<<"File">>},
			 {<<"popup">>,
			  {[{<<"menuitem">>,
				 [{[{<<"value">>,<<"New">>},
					{<<"onclick">>,<<"CreateNewDoc()">>}]},
				  {[{<<"value">>,<<"foo">>},
					{<<"onclick">>,<<"OpenDoc()">>}]},
				  {[{<<"value">>,<<"Close">>},
					{<<"onclick">>,<<"CloseDoc()">>}]}]}]}}]}}]},
	 	jsonpath:replace(<<"menu.popup.menuitem[1].value">>, <<"foo">>, ?JSON)).

bench(M, F, A, N) when N > 0 ->
        L = bench_loop(M, F, A, N, []),
        Length = length(L),
        Min = lists:min(L),
        Max = lists:max(L),
        Med = lists:nth(round((Length / 2)), lists:sort(L)),
        Avg = round(lists:foldl(fun(X, Sum) -> X + Sum end, 0, L) / Length),
        io:format("Range: ~b - ~b mics~n"
                  "Median: ~b mics~n"
                  "Average: ~b mics~n",
                  [Min, Max, Med, Avg]),
        Med.

bench_loop(_M, _F, _A, 0, List) ->
        List;
bench_loop(M, F, A, N, List) ->
        {T, _Result} = timer:tc(M, F, A),
        bench_loop(M, F, A, N - 1, [T|List]).
