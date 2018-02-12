####################################################################
# Graphs - Functions the show the data in graph form
####################################################################

graphs.plot <- function(Ledger, available_stocks){
	# plot, for all stocks, the stock values and all the insights into the stocks
	# then plot the ledger etc

	for (stocks in available_stocks) {

		# show the overall trading performance
		layout(matrix(c(1), 1, 1, byrow = TRUE))

		xrange <- range(Data$DATE)
		yrange <- range(Data$)
		# X - Y Range

		plot(xrange, yrange, type="n", xlab="Date", ylab="Total Value (£)" )
		# set up the plot

		lines(Ledger$Date, Ledger$Value, type="l", lwd=1.5, lty=1, col="blue", pch=18)
		lines(Ledger$Date, Ledger$Stock_Value, type="l", lwd=1.5, lty=1, col="red", pch=18)
		lines(Ledger$Date, Ledger$Capital_Value, type="l", lwd=1.5, lty=1, col="green", pch=18)
		# plot the lines of the ledger

		title("Total Value, Stock Value, and Capital Value")
		# Add a title

	}
}

####################################################################