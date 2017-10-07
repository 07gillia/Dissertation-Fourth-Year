#!/bin/bash

FILES=./*.csv

for f in $FILES
do
	g=${f:2}
	h=${g/.csv/}
	echo "# read in the $g data and validate it is as expected"
	echo "\"$h\""
	echo "$h = read.csv(\"../Data/$h.csv\")"
	echo "sprintf(\"The data is in a dataframe? : %s\", is.data.frame($h))"
	echo "if(is.data.frame($h)) {"
	echo "	sprintf(\"what are the dimentions of the dataframe? : x = %d by y = %d\", ncol($h), nrow($h))"
	echo "}"
	echo "suppressWarnings($h\$TICKER <- as.character($h\$TICKER))"
	echo "suppressWarnings($h\$DATE <- as.character($h\$DATE))"
	echo "suppressWarnings($h\$TIME <- as.character($h\$TIME))"
	echo "$h\$PER <- NULL"
	echo "$h\$TICKER <- substring($h\$TICKER, 5)"
	echo "$h\$DATETIME = paste($h\$DATE,$h\$TIME, sep=\"-\")"
	echo "$h\$TIME <- NULL"
	echo "$h\$DATE <- NULL"
	echo "$h\$HIGH <- NULL"
	echo "$h\$LOW <- NULL"
	echo "$h\$CLOSE <- NULL"
	echo "names($h)[names($h) == 'OPEN'] <- '$h'"
	echo "$h\$TICKER <- NULL"
	echo ""
	echo "STOCK <- merge(STOCK,$h,by=\"DATETIME\",all=TRUE)"
	#echo "print(tail($h, 10))"
	echo ""
done