#!/bin/bash
cljsc ../src/cljs {:output-dir \"cljs/out\" :output-to \"cljs/tests.js\" :target :nodejs}
