####################################################################
# Useful Functions File
####################################################################

action.buy <- function(date, time, stock, amount){
	# The function that buys a stock given time, date, stock, and amount

	UID = round(runif(1, 0, 1000000000))
	# create a unique id for the transaction just choose a random number

	current_stock_value = Data[Data$DATE == date & Data$TIME == time,stock]
	# get the current stock value

	number_of_shares = amount / current_stock_value
	# get the number of shares that bought with amount

	output = list(UID,date,time,stock,number_of_shares,current_stock_value)
	# the variable that brings together all values

	Active[nrow(Active) + 1,] = output
	# add to the list of active shares

	return(Active)
}

action.sell <- function(UID, date, time, stock){
	# The function that sells given a UID

	output = list(UID, Active[Active$Unique_ID == UID,2], Active[Active$Unique_ID == UID,3], Active[Active$Unique_ID == UID,4], Active[Active$Unique_ID == UID,5], Active[Active$Unique_ID == UID,6], date, time, Data[Data$TIME == time & Data$DATE == date,stock])

	Sold[nrow(Sold) + 1,] = output

	return(Sold)
}

action.update <- function(date, capital){
	# The function that will update the ledger every minute

	value_stock = 0

	value_capital = capital

	if(nrow(Active) > 0){
		for (x in c(1:nrow(Active))) {
			value_stock = value_stock + Active[x,5] * Active[x,6]
			value_capital = value_capital - Active[x,5] * Active[x,6]
		}
	}

	if(nrow(Sold) > 0){
		for (x in c(1:nrow(Sold))) {
			value_capital = value_capital + Sold[x,9] * Sold[x,5] - Sold[x,5] * Sold[x,6]
		}
	}

	total_value = value_capital + value_stock

	output = list(date, total_value, value_stock, value_capital)

	Ledger[nrow(Ledger) + 1,] = output

	return(Ledger)
}

####################################################################
# Basic Functions
####################################################################

action.get_last_X_datapoints <- function(){
	# given a stock, time, and date get the previous X values of that stock

	return()
}

action.get_current_day_available <- function(){
	# get all values that are available for a specific day
	# given time and date, no looking into the future

	return()
}

action.get_previous_day <- function(){
	# get all values of the previous day given date

	return()
}

action.get_previous_date <- function(){
	# return the date of the day before the current date

	return()
}

action.get_max <- function(){
	# given a vector find the maximum value

	return()
}

action.get_min <- function(){
	# given a vector find the minimum value

	return()
}

action.get_total_gain <- function(){
	# get the total amount that a stock increased in value by

	return()
}

action.get_total_loss <- function(){
	# get the total amount that a stock fell in value by

	return()
}

####################################################################
# Advanced Functions
####################################################################

action.get_previous_day_close <- function(){
	# get the last value of the preious day given a stock, time, and date

	return()
}

