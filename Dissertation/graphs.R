####################################################################
# Graphs - Functions the show the data in graph form
####################################################################

stock_insights_temp = data.frame(
    Date = as.Date(character()),
    Stock = character(),
    Stock_price = double(),
    bollBands_lower = double(),
    bollBands_middle = double(),
    bollBands_upper = double(),
    chandler_exit_1 = double(),
    chandler_exit_2 = double(),
    aroon_list = double(),
    ATR_list = double(),
    bandwidth_list = double(),
    B_indicator_list = double()
)

stock_insights_temp$Date <- strptime(stock_insights_temp$Date , format="%Y-%m-%d %H:%M:%S")
stock_insights_temp$Date <- as.POSIXct(stock_insights_temp$Date)
stock_insights_temp$Stock <- lapply(stock_insights_temp$Stock, as.character)

####################################################################

my_graphs.draw_graph <- function(numberCols, numberRows, availableStocks, stockInsights, ledger, STOCK){

	# show the overall trading performance

	layout(matrix(c(1), 1, 1, byrow = TRUE))

	# get the range for the x and y axis 
	xrange <- range(ledger$Date)
	yrange <- c(0,20000)

	# set up the plot 
	plot(xrange, yrange, type="n", xlab="Date",
	    ylab="Total Value (£)" )

	# add lines
	lines(ledger$Date, ledger$Value, type="l", lwd=1.5, lty=1, col="blue", pch=18)
	lines(ledger$Date, ledger$Stock_Value, type="l", lwd=1.5, lty=1, col="red", pch=18)
	lines(ledger$Date, ledger$Capital_Value, type="l", lwd=1.5, lty=1, col="green", pch=18)

	# add a title
	title("Total Value, Stock Value, and Capital Value")

	####################################################################

	# show the stock insights per stock

	for (stock in availableStocks) {

		# get the details of the stock that is being graphed currently

		stock_name = colnames(STOCK)[stock]

		for(row in c(1:nrow(stock_insights))){

			# iterate through each row of the big dataframe

			if(stock_insights[row,2] == stock_name){

				# test if this is a row that we want

				stock_insights_temp[nrow(stock_insights_temp) + 1,] = stock_insights[row,]
			}

		}

		####################################################################	

		# given a number of rows and columns format the layout

		layout(matrix(c(1:numberRows), numberRows, numberCols, byrow = TRUE))

		# draw the graphs given

		# get the range for the x and y axis
		xrange <- range(stock_insights_temp$Date)
		yrange <- range(min(stock_insights_temp$bollBands_lower), max(stock_insights$bollBands_upper))

		# set up the plot 
		plot(xrange, yrange, type="n", xlab="Date",
		    ylab="Bollinger Bands" )

		# add lines
		lines(stock_insights_temp$Date, stock_insights_temp$bollBands_lower, type="l", lwd=1.5, lty=1, col="blue", pch=18)
		lines(stock_insights_temp$Date, stock_insights_temp$bollBands_middle, type="l", lwd=1.5, lty=1, col="blue", pch=18)
		lines(stock_insights_temp$Date, stock_insights_temp$bollBands_upper, type="l", lwd=1.5, lty=1, col="blue", pch=18)
		lines(stock_insights_temp$Date, stock_insights_temp$Stock_price, type="l", lwd=1.5, lty=1, col="red", pch=18)

		title = paste("Bollinger Bands -", stock_name, sep = " ")

		# add a title
		title(title)

		####################################################################

		# get the range for the x and y axis
		xrange <- range(stock_insights_temp$Date)
		yrange <- range(min(min(stock_insights_temp$chandler_exit_1), min(stock_insights_temp$chandler_exit_2)), max(max(stock_insights$chandler_exit_1), max(stock_insights$chandler_exit_2)))

		# set up the plot 
		plot(xrange, yrange, type="n", xlab="Date",
		    ylab="Chandler Exit 1 and 2" )

		# add lines
		lines(stock_insights_temp$Date, stock_insights_temp$chandler_exit_1, type="l", lwd=1.5, lty=1, col="blue", pch=18)
		lines(stock_insights_temp$Date, stock_insights_temp$chandler_exit_2, type="l", lwd=1.5, lty=1, col="blue", pch=18)
		lines(stock_insights_temp$Date, stock_insights_temp$Stock_price, type="l", lwd=1.5, lty=1, col="red", pch=18)

		title = paste("Chandler Exit -", stock_name, sep = " ")

		# add a title
		title(title)

		####################################################################

		# get the range for the x and y axis 
		xrange <- range(stock_insights_temp$Date)
		yrange <- range(stock_insights_temp$aroon_list)

		# set up the plot 
		plot(xrange, yrange, type="n", xlab="Date",
		    ylab="Aroon" )

		# add lines
		lines(stock_insights_temp$Date, stock_insights_temp$aroon_list, type="l", lwd=1.5, lty=1, col="blue", pch=18)

		title = paste("Aroon -", stock_name, sep = " ")

		# add a title
		title(title)

	}

}