####################################################################
# Data Manipulation
####################################################################

stock_files = c("AA", "AAPL", "ADBE", "AIG", "AMAT", "AMT", "AXP", "BA", "BAC", "C", "CA", "CAT", "CSCO", "CVX", "DD", "DIS", "EMC", "FSLR", "GE", "GLW", "GOOG", "GS", "HD", "HPQ", "IBM", "INTC", "IP", "JNJ", "JPM", "KO", "MCD", "MMM", "MRK", "MSFT", "PFE", "PG", "T", "TRV", "UTX", "VZ", "WFC", "WMT", "XOM", "YAHOO", "YNDX")

start = "Data/"
end = ".csv"

counter = 1

ALL <- data.frame()

for (x in c(0:44)) {

	stock = stock_files[counter]

	string = paste(start, stock, end, sep="")

	print(string)

	current = read.csv(string)

	current$TICKER = NULL
	current$HIGH = NULL
	current$LOW = NULL
	current$CLOSE = NULL
	current$PER = NULL

	if(counter == 1){

		ALL = current
		colnames(ALL)[3] <- "AA"
	}
	else{

		ALL <- merge(ALL, current,by=c("DATE","TIME"), all = TRUE)

		colnames(ALL)[counter + 2] <- stock_files[counter]

		#print(tail(ALL,2))

	}

	#print(tail(current, 1))

	counter = counter + 1
}

ALL$DateTime = paste(ALL$DATE,"-",ALL$TIME)
ALL$DATE = NULL
ALL$TIME = NULL

ALL = ALL[,c(46,1:45)]

print(tail(ALL,10))

print(colnames(ALL))

write.csv(ALL, file = "ALLSTOCKS.csv")