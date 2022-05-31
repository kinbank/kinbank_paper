#!/bin/bash

hypothesis="fXpermitted"

tree="./processed_data/$hypothesis.bttrees"
data="./processed_data/$hypothesis.btdata"

iterations=10010000
sample=1000
burnin=1000000
chain=1

HOME=$(pwd)

# Independent ML
BayesTraitsV3 $tree $data << ANSWERS
2
2
ScaleTrees
priorAll exp 10
Stones 100 1000
Burnin $burnin
Iterations $iterations
Sample $sample
LogFile ./results/$hypothesis-indep-$chain
run
ANSWERS

# Dependent ML
BayesTraitsV3 $tree $data  << ANSWERS
3
2
ScaleTrees
priorAll exp 10
Stones 100 1000
Burnin $burnin
Iterations $iterations
Sample $sample
LogFile ./results/$hypothesis-dep-$chain
run
ANSWERS