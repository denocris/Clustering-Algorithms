#!/bin/bash

#for i in `seq 2 10`; do
#    sed -i "s/9999/$i/g" kmeanspp.cpp
#done


rm -f ndata.txt
touch ndata.txt

for i in `seq 2 10`; do
    cat data_$i.txt >> ndata.txt;
done