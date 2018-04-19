####################################################################
# Graphs - Functions the show the data in graph form
####################################################################

# graphs.plot <- function(Ledger, available_stocks){
# 	# plot, for all stocks, the stock values and all the insights into the stocks
# 	# then plot the ledger etc

# 	for (stocks in available_stocks) {

# 		# show the overall trading performance
# 		layout(matrix(c(1), 1, 1, byrow = TRUE))

# 		xrange <- range(Data$DATE)
# 		yrange <- range(Data$)
# 		# X - Y Range

# 		plot(xrange, yrange, type="n", xlab="Date", ylab="Total Value (£)" )
# 		# set up the plot

# 		lines(Ledger$Date, Ledger$Value, type="l", lwd=1.5, lty=1, col="blue", pch=18)
# 		lines(Ledger$Date, Ledger$Stock_Value, type="l", lwd=1.5, lty=1, col="red", pch=18)
# 		lines(Ledger$Date, Ledger$Capital_Value, type="l", lwd=1.5, lty=1, col="green", pch=18)
# 		# plot the lines of the ledger

# 		title("Total Value, Stock Value, and Capital Value")
# 		# Add a title

# 	}
# }

# Stock_Names = c("AA", "AAPL", "ADBE", "AIG", "AMAT", "AMT", "AXP", "BA", "BAC", "C", "CA", "CAT", "CSCO", "CVX", "DD", "DIS", "EMC", "FSLR", "GE", "GLW", "GOOG", "GS", "HD", "HPQ", "IBM", "INTC", "IP", "JNJ", "JPM", "KO", "MCD", "MMM", "MRK", "MSFT", "PFE", "PG", "T", "TRV", "UTX", "VZ", "WFC", "WMT", "XOM", "YAHOO", "YNDX")

graphs.plot_this <- function(){
	# print whatever is hardcoded

	plot = data.frame(
		X = double(),
		Y = double(),
	    stringsAsFactors=FALSE
	)

	random_day = sample(unique(Data$DATE),1)

	Date_temp = as.double(unique(Data$DATE))
	Stock_temp <- subset(Data, DATE == random_day , select=c(YAHOO))

	for (i in c(1:length(Date_temp))) {
		plot[nrow(plot) + 1,] = c(Date_temp[i], Stock_temp[i,1])
	}

	print(plot)

	plot = na.omit(plot)

	layout(matrix(c(1), 1, 1, byrow = TRUE))

	xrange <- range(plot$X)
	yrange <- range(plot$Y)

	plot(xrange, yrange, type="n", xlab="Date", ylab="Value (£)" )

	lines(plot$X, plot$Y, type="l", lwd=1.5, lty=1, col="blue", pch=18)

	title("AAPL Stock Value Over Total Time Period")
}

####################################################################