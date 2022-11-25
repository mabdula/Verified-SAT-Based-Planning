#!/bin/bash

function help_message(){
  echo "Usage ./compute_plan.sh horizon problem SAT_solver_executable"
  echo "Note: horizon is a number\n problem is a SAS+ problem in FastDownward's format\n SAT_solver_executable is a path to an executable that accepts DIMACS SAT formulae in its stdin, and it has to output a model as a sequence of integers with only spaces between them"
  exit
}

if [ -z "$1" ]; then
  help_message
fi

if [ -z "$2" ]; then
  help_message
fi

if [ -z "$3" ]; then
  help_message
fi

horizon=$1
problem=$2
SAT_Solver=$3

ENCODING=`cat $problem | sed -n '/begin_SG/q;p' | ./encode_problem $horizon | sed 's/~/-/g'`
SAT_OUT=$(echo "$ENCODING" | $SAT_Solver /dev/stdin - 2>&1)

if echo "$SAT_OUT" | grep -wq SATISFIABLE; then
 echo "$SAT_OUT" | grep -wv c | grep -wv s | grep -wv d | grep -w v | sed 's/v //g' | sed 's/ 0//g' | ./decode_model <(cat $problem | sed -n '/begin_SG/q;p') $horizon
 #gratchk/code/gratchk sat <(echo "$ENCODING") <(echo "$SAT_OUT" | grep -wv c | grep -wv s | grep -wv d | grep -w v | sed 's/v //g' | sed 's/ 0//g')
fi

ID=$RANDOM
if echo "$SAT_OUT" | grep -wq UNSATISFIABLE; then  
  gratgen/gratgen <(echo "$ENCODING") <(echo "$SAT_OUT" | grep -wv c | grep -wv s) -o /tmp/cert.grat_$ID > /dev/null 2> /dev/null
  gratchk/code/gratchk unsat <(echo "$ENCODING") /tmp/cert.grat_$ID | grep "s VERIFIED UNSAT" | sed 's/UNSAT/Unsolvable/g'
  rm /tmp/cert.grat_$ID
fi
