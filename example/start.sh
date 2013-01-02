#!/bin/bash

erl -pa ebin ../ebin ../deps/*/ebin -boot start_sasl -s application start epgpb_example
