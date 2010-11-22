#!/usr/bin/env sh
erlc test_fnt.erl
erl -run test_fnt run -pa ../../ebin -run init stop -noshell
