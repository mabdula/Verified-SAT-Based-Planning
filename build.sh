#!/bin/bash
unzip sml-parse-comb-master.zip
mlton -mlb-path-var 'AFP_PATH '$1'' -cc-opt -O3 -cc-opt -g0 encode_problem.mlb
mlton -mlb-path-var 'AFP_PATH '$1'' -cc-opt -O3 -cc-opt -g0 decode_model.mlb
# unzip kissat-master.zip
# cd kissat-master/
# ./configure
# make
# cd ..
# tar xzf gratgen.tgz
# cd gratgen/
# cmake .
# make
# cd ..
# tar xzf gratchk.tgz
# cd gratchk/code/
# make
# cd ../../
