#!/bin/bash
file=$1
echo $file

plot_cmd=`grep gnuplot $file | sed 's/.*gnuplot //;s/\s\+/#/g;s/#\*)//g' | awk -F: '{if (length($0) > 1) {print " \""file"\" using 1:"$1" title \""$2"\","}}' RS="#" file=${file}.csv`
plot_cmd=`echo $plot_cmd | sed 's/,$//'`

cat > ${file}.gplot <<_EOC
set datafile separator ","
set terminal png
set output "${file}.png"
plot $plot_cmd
_EOC

gnuplot ${file}.gplot
