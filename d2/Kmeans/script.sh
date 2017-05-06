#!/bin/bash

make kmeans
echo "done: 2"

for i in `seq 3 20`; do
    make clean
    sed -i .bak "s/PARAMETER :: nk = $((i - 1))/PARAMETER :: nk = $i/g" tools.f03
    make kmeans
    echo "finished: $i"
done
