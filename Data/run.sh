#!/bin/bash

FILES=./*.csv

for f in $FILES
do
	tr -d '<' < $f > new.csv
	tr -d '>' < new.csv > $f;
done