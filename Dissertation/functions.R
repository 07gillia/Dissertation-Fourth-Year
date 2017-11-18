####################################################################
# Useful Functions File
####################################################################

my_functions.buy <- function(stock_name, current_stock_value, capital_amount_spent, current_date) {

	# given the stock and capital amount 
	# set those in the portfolio

	# create a unique id for the transaction just choose a random number
	UID = runif(1, 0, 1000000000)

	# calculate the amount of stock bought
	bought_amount = capital_amount_spent / current_stock_value

	# calculate the ratio that it was bought at, will be 1
	current_ratio = capital_amount_spent / capital_amount_spent

	# store the values in a list
	output = list(UID, stock_name, current_date, current_stock_value, bought_amount, current_stock_value, current_ratio, FALSE, current_date, current_stock_value)

	# add to the portfolio
	portfolio[nrow(portfolio) + 1,] = output

	return(portfolio)

	# this works
}

my_functions.sell <- function(UID, current_date, current_stock_value) {

	# given the UID stock value and date well the specified stock

	# get the row that is being changed
	current_row <- which(portfolio$Unique_ID == UID)

	# set the bool to sold
	portfolio[current_row,8] = TRUE
	# set the date that it was sold
	portfolio[current_row,9] = list(current_date)
	# set the value that it was sold for
	portfolio[current_row,10] = current_stock_value * portfolio[current_row,5]

	return(portfolio)

	# this works
}

my_functions.update <- function(current_stock, current_stock_price, current_time) {

	# iterate through all rows in the portfolio
	# update any that are related to this stock

	if(nrow(portfolio) > 0){
		# make sure that the portfolio has something in it

		for (row in 1:nrow(portfolio)) {
			# iterate through the rows that are in the portfolio
			if(portfolio[row,2] == current_stock && portfolio[row,8] == FALSE){
				# check that the current row is of the same stock

				portfolio[row,6] = current_stock_price
				portfolio[row,7] = portfolio[row,6] / portfolio[row,4]
				portfolio[row,9] = list(current_time)
			}
		}
	}
	return(portfolio)

	# this works
}

my_functions.update_ledger <- function(current_time) {

	# update the ledger for the current time with the updated
	# amount of stocks that have been bought or sold and the amount
	# of money that was spent or collected from these transactions
	# this will have a row for every time in the data meaning that 
	# even if nothing happens on a specific datetime then the row will
	# still be added just with the same values as the one above it

	# get the value of all the stock that is current owned
	total_stock_value = 0
	total_capital_value = 10000
	if(nrow(portfolio) > 0){
		# make sure that the portfolio has something in it
		for (row in 1:nrow(portfolio)) {
			# iterate through the rows that are in the portfolio
			if(portfolio[row,8] == FALSE){
				# check that the current stock has not been sold
				total_stock_value = total_stock_value + portfolio[row,6] * portfolio[row,5]
				# add the current value of all this stock
				total_capital_value = total_capital_value - portfolio[row,4] * portfolio[row,5]
				# take off the capital that was spent to buy the current stock
			}
			# iterate through the rows that are in the portfolio
			if(portfolio[row,8] == TRUE){
				# if the current stock has been sold
				total_capital_value = total_capital_value + portfolio[row,10]
				# add the amount of capital that was made by selling this stock
				total_capital_value = total_capital_value - portfolio[row,4] * portfolio[row,5]
				# take off the amount that was spent on the stock in the first place
			}
		}
	}

	# get the total value
	total_value = total_stock_value + total_capital_value

	# get the stock value
	liquidity_ratio = total_stock_value/total_value

	# add all the variables to the list
	output = list(current_time, total_value, total_stock_value, total_capital_value, liquidity_ratio)

	# add the list to the ledger
	ledger[nrow(ledger) + 1,] = output

	return(ledger)
}

####################################################################
# Other Functions
####################################################################


my_functions.get_average <- function(row, number_of_minutes, stock){

	# given a specific stock and a timeframe give the average over that time
	# starting from the current time
	# to the current time - the number of minutes

	start = row - number_of_minutes

	average = mean(STOCK[start:row, stock], na.rm=TRUE)

	return(average)
}


my_functions.get_standard_deviation <- function(range){

	# given some variables get the standard deviation over them

	result = sd(range, na.rm=TRUE)

	return(result)
}

my_functions.get_bollinger_bands <- function(row, number_of_minutes, stock){

	# Middle Band = 20-day simple moving average (SMA)
  	# Upper Band = 20-day SMA + (20-day standard deviation of price x 2) 
  	# Lower Band = 20-day SMA - (20-day standard deviation of price x 2)

  	# gives an indication of how the stock will behave and the expected bounds, upper and lower

  	range = STOCK[row-number_of_minutes:row,stock]

  	lower_band = my_functions.get_average(row, number_of_minutes, stock) - (my_functions.get_standard_deviation(range)) * 2

  	middle_band = my_functions.get_average(row, number_of_minutes, stock)

  	upper_band = my_functions.get_average(row, number_of_minutes, stock) + (my_functions.get_standard_deviation(range)) * 2

  	result = c(lower_band, middle_band, upper_band)

  	return(result)
}

my_functions.chandelier_exit <- function(){

	# implementation of chandelier exit
	# Chandelier Exit (long) = 22-day High - ATR(22) x 3 
	# Chandelier Exit (short) = 22-day Low + ATR(22) x 3
	# ATR is needed for this

	return(result)
}

my_functions.average_true_range <- function(method){

	# implementation of average true range
	# there are three different methods
	# Method 1: Current High less the current Low
	# Method 2: Current High less the previous Close (absolute value)
	# Method 3: Current Low less the previous Close (absolute value)



	return(result)
}


####################################################################