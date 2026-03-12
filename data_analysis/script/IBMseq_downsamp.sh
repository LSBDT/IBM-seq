#!/bin/bash

##update 2026/03/04

FILE1=$1
FILE2=$2
FILE3=$3
FILEDIR=$4
$OUTDIR=$5
echo $OUTDIR

mkdir -p "$OUTDIR"
#downsampling sizeをiで指定して、試行回数をjで指定する

for i in $(seq 15000000 -5000000 1)
do
	echo $i
	for j in $(seq 1 10)
	do
        	echo $j

        	name1=$(echo $FILE1|awk -F '.mat2.gz' '{print $1}')
        	name2=$(echo $FILE2|awk -F '.mat2.gz' '{print $1}')
        	name3=$(echo $FILE3|awk -F '.mat2.gz' '{print $1}')

        	zcat $FILEDIR/$FILE1|tail -n +2|shuf -n $i|cut -f 3,4,5,6,9,10 | sort | uniq -c | awk '{OFS="\t"}{print $2"."$6".t1",$3".e1",$6".t1","UEI1",$1,"\n"$3".e1",$4".e2","UEI1","UEI2",$1,"\n"$4".e2",$5"."$7".t2","UEI2",$7".t2",$1}' | awk '{OFS="\t"}{print $1,$2,$3,$4,$5}'|gzip >$OUTDIR/${name1}_${i}_${j}.link.gz
        	zcat $FILEDIR/$FILE2|tail -n +2|shuf -n $i|cut -f 3,4,5,6,9,10 | sort | uniq -c | awk '{OFS="\t"}{print $2"."$6".t1",$3".e1",$6".t1","UEI1",$1,"\n"$3".e1",$4".e2","UEI1","UEI2",$1,"\n"$4".e2",$5"."$7".t2","UEI2",$7".t2",$1}' | awk '{OFS="\t"}{print $1,$2,$3,$4,$5}'|gzip >$OUTDIR/${name2}_${i}_${j}.link.gz
        	zcat $FILEDIR/$FILE3|tail -n +2|shuf -n $i|cut -f 3,4,5,6,9,10 | sort | uniq -c | awk '{OFS="\t"}{print $2"."$6".t1",$3".e1",$6".t1","UEI1",$1,"\n"$3".e1",$4".e2","UEI1","UEI2",$1,"\n"$4".e2",$5"."$7".t2","UEI2",$7".t2",$1}' | awk '{OFS="\t"}{print $1,$2,$3,$4,$5}'|gzip >$OUTDIR/${name3}_${i}_${j}.link.gz
        
		if [ $? -ne 0 ]; then
			echo "Error during compression or shuffling" >&2
			exit 1
		fi
	done
done

