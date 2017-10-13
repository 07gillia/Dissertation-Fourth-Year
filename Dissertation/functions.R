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
				portfolio[row,9] = list(current_time) # THERE IS AN ISSUE HERE!!!!
			}
		}
	}

	return(portfolio)
}

my_functions.update_ledger <- function() {


}

####################################################################