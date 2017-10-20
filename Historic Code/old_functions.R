####################################################################
# Useful Functions File
####################################################################

my_functions.buy <- function(stock, capital_amount) {

	# buy a stock
	# add the stock name to the portfolio
	# add the date that it was bought
	# add the value that was bought total
	# add the number of shares bought
	# add the current value of the stock
	# add the current ratio of the bought value and current value
	# set the variable sold to false
	# set the sold date to null
	# set the sold value to null

	# create a unique id for the transaction just choose a random number
	UID = runif(1, 0, 1000000000)

	# store the amount of capital spent
	value_when_bought = capital_amount

	# calculate the number of shares bought
	amount = value_when_bought / current_stock_value

	# calculate the ratio 
	ratio = value_when_bought / current_stock_value

	# amount that it could be sold for
	total_current_value = amount * current_stock_value

	# store the values in a list
	output = list(UID, stock, current_time, value_when_bought, amount, current_stock_value, ratio, FALSE, current_time, total_current_value)

	# add to the portfoliio
	portfolio[nrow(portfolio) + 1,] = output

	return(portfolio)
}

my_functions.sell <- function(UID) {

	# sell the specific stock

	total_owned_stocks = nrow(portfolio)

	row <- which(portfolio$UID == UID)

	# update the values in the portfolio
	portfolio[row,8] = TRUE
	portfolio[row,9] = current_time
	portfolio[row,10] = portfolio[row,5] * current_stock_value

	return(portfolio)
}

my_functions.update <- function(current_stock_value) {

	# update the value of all the stocks that are in the portfolio

	# iterate through the rows in the portfolio
	for (rows in c(1:nrow(portfolio))) {
		current_row = portfolio[,rows]

		# update the current value
		current_row[6] = current_row[5] * current_stock_value
		# update the current ratio
		current_row[7] = current_row[4] / current_stock_value

		portfolio[rows] = current_row
	}

	return(portfolio)
}

my_functions.update_ledger <- function() {

	# keep track of the amount of capital and stock that is available

	# set the initial values
	capital_value = capital
	stock_value = 0

	for (rows in c(1:nrow(portfolio))) {
		if(portfolio[8,rows]){
			# means that the stock has been sold

			# take off the amount spent buying the shares
			capital_value = capital_value - portfolio[4,rows] * portfolio[5,rows]
			# add on the amount earnt from selling the stock
			capital_value = capital_value + portfolio[10]

			# take off the value that the stock was sold for
			stock_value = stock_value - portfolio[10]
		}
		else{
			# means that the stock has not been sold yet

			# decrease the amoun that was spent to buy the stock
			capital_value = capital_value - portfolio[4,rows] * portfolio[5,rows]

			# add the value of shares bought
			stock_value = stock_value + portfolio[6]
		}
	}

	output = list(current_time, capital_value + stock_value, stock_value, capital_value)

	# add to the portfoliio
	ledger[nrow(ledger) + 1,] = output

	return(ledger)
}

####################################################################