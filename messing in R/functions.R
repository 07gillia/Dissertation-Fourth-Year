####################################################################
# Useful Functions File
####################################################################

my_functions.get_data_between <- function(start_date = '1989-08-11',end_date){
	data.all.BAB[data.all.BAB$Date >= start_date & data.all.BAB$Date <= end_date,]
}

my_functions.buy <- function(stock, percentage_amount) {

	capital_spent = as.numeric(capital * percentage_amount)
	shares_bought = as.numeric(capital_spent / tick.open)
	current_value = as.numeric(shares_bought * tick.open)
	ratio = as.numeric(current_value / capital_spent)
	sold = as.logical(FALSE)
	sold.date = as.Date(tick.date)
	sold.amount = as.numeric(current_value)
	portfolio[nrow(portfolio) + 1,] = c(stock, tick.date, percentage_amount, capital_spent, shares_bought, current_value, ratio, sold, sold.date, sold.amount)
}

####################################################################