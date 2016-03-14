all: *.erl *.hrl lex.xrl grm.yrl
	erl -compile *.erl
	erl -pa ebin -eval "lexgrm:start(), halt()" -noshell -detached

clean:
	rm -f *.beam grm.erl lex.erl
	rm -f *.dump

run_tests: all
	erl -noshell -eval "eunit:test(test_client), halt()"

run_ping_tests: all
	erl -noshell -eval "eunit:test({test,test_client,ping}), halt()"

run_robustness_tests: all
	erl -noshell -eval "eunit:test({timeout, 10, {test,test_client,robustness_channel}}), halt()"
	erl -noshell -eval "eunit:test({timeout, 10, {test,test_client,robustness_server}}), halt()"

run_concurrency_tests: all
	erl -noshell -eval "eunit:test({timeout, 10, {test,test_client,process_usage_test}}), halt()"

PERFTESTS = "[\
{timeout, 60, fun () -> test_client:many_users_one_channel(20, one) end},\
{timeout, 60, fun () -> test_client:many_users_one_channel(5, all) end},\
{timeout, 60, fun () -> test_client:many_users_many_channels(10) end}\
]"

run_perf_tests: all
	erl -noshell -eval "eunit:test("${PERFTESTS}"), halt()"

run_distributed_tests: all
	-killall beam.smp 2>/dev/null
	erl -noshell -name "testsuite@127.0.0.1" -setcookie dchat -eval "eunit:test(test_remote), halt()"

run_workers_tests: all
	erl -noshell -eval "eunit:test({timeout, 10, {test,test_client,workers}}), halt()"
