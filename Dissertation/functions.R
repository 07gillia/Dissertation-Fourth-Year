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
# Basic Functions
####################################################################


my_functions.get_average <- function(row, number_of_minutes, stock){

	# given a specific stock and a timeframe give the average over that time
	# starting from the current time
	# to the current time - the number of minutes

	start = row - number_of_minutes

	average = mean(STOCK[start:row, stock], na.rm=TRUE)

	return(average)
}

my_functions.get_hour <- function(the_row, the_stock, multiplication){

	# get the current hour
	# returns a list that are all values for the last hour, will wrap
	# will also remove all NAs form the list

	from_row = the_row - 59 * multiplication

	all_stock = STOCK[, the_stock]

	result = all_stock[from_row:the_row]

	result = result[!is.na(result)]

	return(result)
}

my_functions.get_day <- function(the_row, the_stock, multiplication){

	# get the current day
	# returns a list that are all values for the last day
	# will also remove all NAs form the list

	from_row = the_row - 389 * multiplication

	all_stock = STOCK[, the_stock]

	result = all_stock[from_row:the_row]

	result = result[!is.na(result)]

	return(result)
}

my_functions.get_month <- function(the_row, the_stock, multiplication){

	# get the current month
	# returns a list that are all the values for the last month
	# will also remove all NAs form the list

	from_row = the_row - 7780 * multiplication

	all_stock = STOCK[, the_stock]

	result = all_stock[from_row:the_row]

	result = result[!is.na(result)]

	return(result)
}

my_functions.get_max <- function(the_list){

	# given a list of numbers get the maximum
	# this will be used in conjunction with the get_timeframe functions

	return(max(the_list))
}

my_functions.get_min <- function(the_list){

	# given a list of numbers get the maximum
	# this will be used in conjunction with the get_timeframe functions

	return(min(the_list))
}

my_functions.get_previous_close <- function(the_row, the_stock){

	# this will give the previous days close
	# used in ATR
	# NOT FINISHED

	previous_day = row - 390

	date_string = STOCK[previous_day,1]

	date_string = substr(date_string, 0, 10)

	while(grepl(date_string , STOCK[previous_day,1], fixed=TRUE)){ # X in in Y

		# iterate through the day until we reach the last day

		previous_day = previous_day + 1
	}

	result = previous_day - 1

	return(STOCK[result, the_stock])
}

my_functions.get_standard_deviation <- function(range){

	# given some variables get the standard deviation over them

	result = sd(range, na.rm=TRUE)

	return(result)
}

my_functions.get_rows_since <- function(the_row, the_stock, the_price){

	# find the number of rows from the current row that a given price was present

	current_row = the_row

	stock_price = STOCK[current_row, the_stock]

	while(isTRUE(stock_price != the_price) | is.na(stock_price)){

		# iterate backwards from the current date until the stock price is a specific value

		current_row = current_row - 1

		stock_price = STOCK[current_row, the_stock]
	}

	result = the_row - current_row

	return(result)
}

####################################################################
# Advanced Functions
####################################################################

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

my_functions.chandelier_exit <- function(method, the_row, the_stock){

	# implementation of chandelier exit
	# Chandelier Exit (long) = 22-day High - (ATR(22) x 3)
	# Chandelier Exit (short) = 22-day Low + (ATR(22) x 3)
	# ATR is needed for this

	if(method == 1){
		# if the method is 1 then use the long method

		result = my_functions.get_max(my_functions.get_day(the_row, the_stock, 22)) - (3 * my_functions.average_true_range(the_row, the_stock, 22 * 6.5))
	}
	if(method == 2){
		# if the method is 2 the use the short method

		result = my_functions.get_min(my_functions.get_day(the_row, the_stock, 22)) + (3 * my_functions.average_true_range(the_row, the_stock, 22 * 6.5))
	}

	return(result)
}

####################################################################

my_functions.accumulation_distribution_line <- function(the_row, the_stock){

	# Implementation of ADL
	# Money Flow Multiplier = [(Close  -  Low) - (High - Close)] /(High - Low) 
	# Money Flow Volume = Money Flow Multiplier x Volume for the Period
	# ADL = Previous ADL + Current Period's Money Flow Volume
	# Volume-based indicator designed to measure the cumulative flow of money into and out of a security

	# Volume is not available so this might be quite hard to implement??

	result = 0

	return(result)	
}

my_functions.aroon <- function(the_row, the_stock, number_rows){

	# Aroon-Up = ((25 - Days Since 25-day High)/25) x 100
	# Aroon-Down = ((25 - Days Since 25-day Low)/25) x 100
	# an indicator system that determines whether a stock is trending or not and how strong the trend is

	# Aroon Oscillator = Aroon-Up - Aroon-Down

	high = my_functions.get_max(my_functions.get_day(the_row, the_stock, round(number_rows/390, 0)))

	low = my_functions.get_min(my_functions.get_day(the_row, the_stock, round(number_rows/390, 0)))

	aroon_up = ((number_rows - my_functions.get_rows_since(the_row, the_stock, high))/ number_rows) * 100

	aroon_down = ((number_rows - my_functions.get_rows_since(the_row, the_stock, low))/ number_rows) * 100

	aroon_oscillator = aroon_up - aroon_down

	return(c(aroon_up, aroon_down, aroon_oscillator))
}

my_functions.plus_directional_movement <- function(the_row, the_stock, timeframe, number_of_timeframes){

	# used as part of the average directional index
	# current high - previous high
	# given that this value is +ve 
	# if negative then the plus directional indicator is 0

	# using different timeframes 
	# 0 - hour
	# 1 - day
	# 2 - month

	result = 0

	if(timeframe == 0){
		# if the timeframe is hour

		current_high = my_functions.get_max(my_functions.get_hour(the_row, the_stock, number_of_timeframes))

		previous_high = my_functions.get_max(my_functions.get_hour(the_row - (60*number_of_timeframes), the_stock, number_of_timeframes))

		result = current_high - previous_high
	}
	else if(timeframe == 1){
		# if the timeframe is day

		current_high = my_functions.get_max(my_functions.get_day(the_row, the_stock, number_of_timeframes))

		previous_high = my_functions.get_max(my_functions.get_day(the_row - (390*number_of_timeframes), the_stock, number_of_timeframes))

		result = current_high - previous_high
	}
	else if(timeframe == 2){
		# if the timeframe is month

		current_high = my_functions.get_max(my_functions.get_month(the_row, the_stock, number_of_timeframes))

		previous_high = my_functions.get_max(my_functions.get_month(the_row - (7780*number_of_timeframes), the_stock, number_of_timeframes))

		result = current_high - previous_high
	}
	else{
		stop("Error in plus directional movement")
	}

	if(result > 0){
		return(result)
	}
	else{
		return(0)
	}
}

my_functions.minus_directional_movement <- function(the_row, the_stock, timeframe, number_of_timeframes){

	# used as part of the average directional index
	# previous low - current low
	# given that this value is +ve
	# if negative then the plus directional indicator is 0

	# using different timeframes 
	# 0 - hour
	# 1 - day
	# 2 - month

	result = 0

	if(timeframe == 0){
		# if the timeframe is hour

		current_low = my_functions.get_min(my_functions.get_hour(the_row, the_stock, number_of_timeframes))

		previous_low = my_functions.get_min(my_functions.get_hour(the_row - (60*number_of_timeframes), the_stock, number_of_timeframes))

		result = previous_low - current_low
	}
	else if(timeframe == 1){
		# if the timeframe is day

		current_low = my_functions.get_min(my_functions.get_day(the_row, the_stock, number_of_timeframes))

		previous_low = my_functions.get_min(my_functions.get_day(the_row - (390*number_of_timeframes), the_stock, number_of_timeframes))

		result = previous_low - current_low
	}
	else if(timeframe == 2){
		# if the timeframe is month

		current_low = my_functions.get_min(my_functions.get_month(the_row, the_stock, number_of_timeframes))

		previous_low = my_functions.get_min(my_functions.get_month(the_row - (7780*number_of_timeframes), the_stock, number_of_timeframes))

		result = previous_low - current_low
	}
	else{
		stop("Error in minus directional movement")
	}

	if(result > 0){
		return(result)
	}
	else{
		return(0)
	}
}

my_functions.average_directional_index <- function(the_row, the_stock, timeframe, number_of_timeframes){

	# Directional movement is calculated by comparing the difference 
	# between two consecutive lows with the difference between their 
	# respective highs.

	result = 0

	plus_directional_movement = my_functions.plus_directional_movement(the_row, the_stock, timeframe, number_of_timeframes)

	minus_directional_movement = my_functions.minus_directional_movement(the_row, the_stock, timeframe, number_of_timeframes)

	if(timeframe == 0){
		number_of_hours = number_of_timeframes
	} else if(timeframe == 1){
		number_of_hours = number_of_timeframes * 6.5
	} else if(timeframe == 2){
		number_of_hours = number_of_timeframes * 6.5 * 20
	}

	ATR = my_functions.average_true_range(the_row, the_stock, number_of_hours)

	# GETS COMPLEX

	return(result)
}

my_functions.average_true_range <- function(the_row, the_stock, number_of_hours){

	# implementation of average true range
	# there are three different methods
	# Method 1: Current High less the current Low
	# Method 2: Current High less the previous Close (absolute value)
	# Method 3: Current Low less the previous Close (absolute value)
	# the answer is the greatest of all three methods

	method_1 = abs(my_functions.get_max(my_functions.get_hour(the_row, the_stock, number_of_hours)) - my_functions.get_min(my_functions.get_hour(the_row, the_stock, number_of_hours)))

	method_2 = abs(my_functions.get_max(my_functions.get_hour(the_row, the_stock, number_of_hours)) - my_functions.get_previous_close(the_row - (60 * number_of_hours), the_stock))

	method_3 = abs(my_functions.get_min(my_functions.get_hour(the_row, the_stock, number_of_hours)) - my_functions.get_previous_close(the_row - (60 * number_of_hours), the_stock))

	result = my_functions.get_max(c(method_1, method_2, method_3))

	return(result)
}

my_function.get_bandwidth <- function(row, number_of_minutes, stock){

	# using bollenger bands get the band width
	# measures the percentage difference between the upper band and the lower band

	boll_bands = my_functions.get_bollinger_bands(row, number_of_minutes, stock)

	return(((boll_bands[3] - boll_bands[1]) / boll_bands[2]) * 100)
}

my_functions.get_B_indicator <- function(row, number_of_minutes, stock, stock_price){

	# %B indicator 
	# quantifies a security's price relative to the upper and lower Bollinger Band
	# %B equals 1 when price is at the upper band
	# %B equals 0 when price is at the lower band
	# %B is above 1 when price is above the upper band
	# %B is below 0 when price is below the lower band
	# %B is above .50 when price is above the middle band (20-day SMA)
	# %B is below .50 when price is below the middle band (20-day SMA)

	boll_bands = my_functions.get_bollinger_bands(row, number_of_minutes, stock)

	return((stock_price - boll_bands[1])/(boll_bands[3] - boll_bands[1]))	
}

####################################################################