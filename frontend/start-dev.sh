#!/bin/sh
exec erl \
    -pa ebin deps/*/ebin \
    -boot start_sasl \
    -sname frontend_dev \
    -s frontend \
    -s reloader
