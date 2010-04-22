CollectDB
=========

Quickstart
----------

CollectDB is written in Erlang and furthermore depends on couchbeam. If you're using a Mac, you can use [this homebrew formula]((http://github.com/aflatter/homebrew/commit/bd0b708ccd814dbf20649369150b41ca01863447).

    make all
    make run
    
Now configure a collectd instance to use your machine as a server. You should see some output on the console for every packet collectdb receives.

Caveats
-------
* Packets are currently NOT saved to database. This is still TODO!
* `make run` will fail if you're not running a couchdb instance on your local port 5984. Sorry, there currently are no user-friendly error messages.

Tests
-----

Running the test suite requires `common_test` which is part of the Erlang standard distribution. You still have to install it yourself.
Look for the `common_test` directory and run `./install.sh` there. You should now see a `run_test` file in `~/bin`

    make tests
    
License
-------

I did not yet choose a license. This is an early preview. ;-)