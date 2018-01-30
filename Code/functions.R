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

	print(UID)
	print(date)
	print(time)
	print(stock)

	stop()

	selling_row = Active[Active$Unique_ID == UID,]

	print(selling_row)

	Sold[nrow(Sold) + 1,] = output

	return(Sold)
}

Sold = data.frame(
	Unique_ID = character(),
	Date_Bought = character(),
	Time_Bought = character(),
	Stock = character(),
	Number_Shares = double(),
	Cost_Per_Share = double(),
	Date_Sold = character(),
	Time_Sold = character(),
	Price_Per_Share = double(),
	stringsAsFactors=FALSE
)

action.update <- function(){
	# The function that will update the ledger every minute

	return()
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

