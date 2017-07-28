####################################################################
# Useful Functions File
####################################################################

my_functions.get_data_between <- function(start_date = '1989-08-11',end_date){
	data.all.BAB[data.all.BAB$Date >= start_date & data.all.BAB$Date <= end_date,]
}

my_functions.buy <- function(stock, capital_amount) {

	# create a list to hold the variables
	# add the stock
	# add the date that it was bought
	# add the percentage bought

	# calculate the amount bought and add it to the list
	value_when_bought = capital_amount

	# calculate the amount of a single stock that has been bought and add it to the list
	amount = value_when_bought / tick.open

	# get the current value of the stock and add it to the list

	# get the current ratio and add it to the list
	ratio = tick.open / (value_when_bought / amount)

	# set that it has yet to be sold
	# set the sold date as the day bought

	# get the amount sold
	value_when_sold = tick.open * amount

	output = list(stock, tick.date, value_when_bought, amount, tick.open, ratio, FALSE, tick.date, value_when_sold)

	#print(output)

	portfolio[nrow(portfolio) + 1,] = output

	return(portfolio)

}

my_functions.sell <- function(stock, ratio) {

	# stock is the string that each stock has for where it has been bought from
	# ratio is the minimum ratio at which to sell

	total_owned_stocks = nrow(portfolio)

	if(total_owned_stocks != 0) {

		# loop through each purchase in the portfolio
		for (purchase in c(1:total_owned_stocks)) {

		    # make sure we are only iterating through the purchases that can be sold
			if(portfolio[purchase, 7] == FALSE) {

				#set up all the variables specidfic to this purchase
				stock_name = portfolio[purchase, 1]
				current_ratio = portfolio[purchase, 6]

				# if the stock is the one we want
				if(stock_name == stock && current_ratio >= ratio) {

					# set it to sold
					portfolio[purchase, 7] = TRUE

					# set the date it was sold
					portfolio[purchase, 8] = tick.date

					# set the amount it sold for
					portfolio[purchase, 9] = tick.open * portfolio[purchase, 4]
				}
			}
		}
	}

	return(portfolio)
}

my_functions.update <- function() {

	# this function will update the current values in the portfolio
	# the current values will be used to decide if the stock should be sold or not

	total_purchases = nrow(portfolio)

	if(total_purchases != 0) {
	
		# loop through each item in the portfolio
		for (purchase in c(1:total_purchases)) {

			# update the values in the current fields
			portfolio[purchase, 5] = tick.open

			portfolio[purchase, 6] = portfolio[purchase, 5] / (portfolio[purchase, 3] / portfolio[purchase, 4])
		}
	}

	return(portfolio)
}

my_functions.update_ledger <- function() {

	# keep track of the amount of capital and stock that is available

	total_purchases = nrow(portfolio)
	current_stock_value = 0
	current_capital_value = 0

	# take into account the inital amount invested
	current_capital_value = current_capital_value + capital

	# make sure that the portfolio has something in it
	if(total_purchases != 0) {

		# this means that there is something in the list to iterate through, iterate through it
		for(purchase in c(1:total_purchases)) {

			# if the stock has not been sold
			if(portfolio[purchase, 7] == FALSE) {

				# add the amount that the purchase is worth to the current_stock_value
				current_stock_value = current_stock_value + portfolio[purchase, 4] * portfolio[purchase, 5]

				# update the capital value so as to remove the capital spent on the stock initally
				current_capital_value = current_capital_value - portfolio[purchase, 3]
			}
			# if the stock has been sold
			if(portfolio[purchase, 7]) {

				# increase the current capital value so as to account for money gained
				current_capital_value = current_capital_value + portfolio[purchase, 9] - portfolio[purchase, 3]
			}
		}
		output = list(tick.date, current_capital_value + current_stock_value, current_stock_value, current_capital_value)
	}
	# if the ledger is empty then add a new line with no changes
	else {

		# set a list as all the required values
		output = list(tick.date, capital, 0 ,capital)
	}

	# set the next row of the table as the list
	ledger[nrow(ledger) + 1, ] = output

	return(ledger)
}

####################################################################