####################################################################
# Useful Functions File
####################################################################

my_functions.get_data_between <- function(start_date = '1989-08-11',end_date){
	data.all.BAB[data.all.BAB$Date >= start_date & data.all.BAB$Date <= end_date,]
}

my_functions.buy <- function(stock, percentage_amount) {

	# create a list to hold the variables
	# add the stock
	# add the date that it was bought
	# add the percentage bought

	# calculate the amount bought and add it to the list
	value_when_bought = percentage_amount * capital / 100

	# calculate the amount of a single stock that has been bought and add it to the list
	amount = value_when_bought / tick.open

	# get the current value of the stock and add it to the list

	# get the current ratio and add it to the list
	ratio = tick.open / (value_when_bought / amount)

	# set that it has yet to be sold
	# set the sold date as the day bought

	# get the amount sold
	value_when_sold = tick.open * amount

	output = list(stock, tick.date, percentage_amount, value_when_bought, amount, tick.open, ratio, FALSE, tick.date, value_when_sold)

	#print(output)

	portfolio[nrow(portfolio) + 1,] = output

	return(portfolio)

}

my_functions.sell <- function(stock, ratio) {

	# stock is the string that each stock has for where it has been bought from
	# ratio is the minimum ratio at which to sell

	total_owned_stocks = nrow(portfolio)

	# loop through each purchase in the portfolio
	for (purchase in c(1:total_owned_stocks)) {

	    # make sure we are only iterating through the purchases that can be sold
		if(portfolio[purchase, 8] == FALSE) {

			#set up all the variables specidfic to this purchase
			stock_name = portfolio[purchase, 1]
			current_ratio = portfolio[purchase, 7]

			# if the stock is the one we want
			if(stock_name == stock && current_ratio >= ratio) {

				# set it to sold
				portfolio[purchase, 8] = TRUE

				# set the date it was sold
				portfolio[purchase, 9] = tick.date

				# set the amount it sold for
				portfolio[purchase, 10] = tick.open * portfolio[purchase, 5]
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
			portfolio[purchase, 6] = tick.open

			portfolio[purchase, 7] = portfolio[purchase, 6] / (portfolio[purchase, 4] / portfolio[purchase, 5])
		}
	}

	return(portfolio)
}

####################################################################