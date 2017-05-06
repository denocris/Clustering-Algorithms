#!/bin/bash
#awk 'BEGIN {FS=","}{print $3}' data.md


num_of_rep=`grep -c republican data.md` # count (-c is the option) the lines with the word republican
num_of_dem=`grep -c democrat data.md`
len=435
#len=`grep -c data.md`
echo $num_of_rep
echo $num_of_dem
echo $len
# {print log(-$1)/log(10)}
i=2
for i in `seq 2 17`;
do
awk -v num_of_rep=${num_of_rep} -v num_of_dem=${num_of_dem} -v len=${len} -v i=$i 'BEGIN {
FS=","
rep_and_yes = 0
rep_and_no = 0
dem_and_yes = 0
dem_and_no = 0
num_yes = 0
num_no = 0
num_qm = 0}
{
if ($1=="republican" && $i=="y") {rep_and_yes = rep_and_yes + 1}
if ($1=="republican" && $i=="n") {rep_and_no = rep_and_no + 1}
if ($1=="democrat" && $i=="y") {dem_and_yes = dem_and_yes + 1}
if ($1=="democrat" && $i=="n") {dem_and_no = dem_and_no + 1}
if ($i=="y") {num_yes = num_yes + 1}
if ($i=="n") {num_no = num_no + 1}
if ($i=="?") {num_qm = num_qm + 1}
}
{
len_without_qm = len - num_qm
}
END {

#print len_without_qm

print ((rep_and_yes/len)*log((rep_and_yes/len)/((num_of_rep/len)*(num_yes/len))) + \
       (dem_and_yes/len)*log((dem_and_yes/len)/((num_of_dem/len)*(num_yes/len))) + \
       (rep_and_no/len)*log((rep_and_no/len)/((num_of_rep/len)*(num_no/len))) + \
       (dem_and_no/len)*log((dem_and_no/len)/((num_of_dem/len)*(num_no/len))) )

# print ((rep_and_yes/len)*log((rep_and_yes/len)/((num_of_rep/len)*(num_yes/len_without_qm))) + \
#        (dem_and_yes/len)*log((dem_and_yes/len)/((num_of_dem/len)*(num_yes/len_without_qm))) + \
#        (rep_and_no/len)*log((rep_and_no/len)/((num_of_rep/len)*(num_no/len_without_qm))) + \
#        (dem_and_no/len)*log((dem_and_no/len)/((num_of_dem/len)*(num_no/len_without_qm))) )
# print ((rep_and_yes/len_without_qm)*log((rep_and_yes/len_without_qm)/((num_of_rep/len_without_qm)*(num_yes/len_without_qm))) + \
#        (dem_and_yes/len_without_qm)*log((dem_and_yes/len_without_qm)/((num_of_dem/len_without_qm)*(num_yes/len_without_qm))) + \
#        (rep_and_no/len_without_qm)*log((rep_and_no/len_without_qm)/((num_of_rep/len_without_qm)*(num_no/len_without_qm))) + \
#        (dem_and_no/len_without_qm)*log((dem_and_no/len_without_qm)/((num_of_dem/len_without_qm)*(num_no/len_without_qm))) )
}' data.md
done
