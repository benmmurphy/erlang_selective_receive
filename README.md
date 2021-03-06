Illustrates the erlang:port_command selective receive problem. You need >= R14B to see the difference because
earlier versions of erlang didn't optimise selective receive. The difference happens in >= R14B because 
erlang doesn't optimise selective receive for erlang:port_command in prim_file (https://github.com/erlang/otp/blob/maint/erts/preloaded/src/prim_file.erl#L1035).
This problem will only occur if you are using `raw` mode because without `raw` erlang uses a gen_server to handle 
file writing and reading.

The slave_performance version spawns a process that it uses for writing to the file. The process still blocks
for the write to 'complete' but because there is never any messages in the slaves queue it runs fast. When
the gen_server is doing a selective receive in file:write there is a 50,000 messages in its queue so it runs
slow.

The slave completes in ~ 2 seconds the normal version completes in 312 seconds. :(

    erlc *.erl

    erl +P 200000 

    slave_performance:run()

    1> slave_performance:run().
    Elapsed {2260206,ok}
    ok
    terminate!

    bad_performance:run()

    1> bad_performance:run().
    Elapsed {312106478,ok}
    terminate!
    ok

This also happens in gen_tcp:send & gen_tcp:recv (https://github.com/erlang/otp/blob/maint/erts/preloaded/src/prim_inet.erl#L374).
    
