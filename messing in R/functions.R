####################################################################
# Useful Functions File
####################################################################

my_functions.get_data_between <- function(start_date = '1989-08-11',end_date){
	data.all.BAB[data.all.BAB$Date >= start_date & data.all.BAB$Date <= end_date,]
}

my_functions.buy <- function(stock, percentage_amount) {

	# create a list to hold the variables
	new_row = c()

	# add the stock
	new_row = c(new_row, stock)

	# add the date that it was bought
	new_row = c(new_row, tick.date)

	# add the percentage bought
	new_row = c(new_row, percentage_amount)

	# calculate the amount bought and add it to the list
	value_when_bought = percentage_amount * capital
	new_row = c(new_row, value_when_bought)

	# calculate the amount of a single stock that has been bought and add it to the list
	amount = value_when_bought / tick.open
	new_row = c(new_row, amount)

	# get the current value of the stock and add it to the list
	new_row = c(new_row, tick.open)

	# get the current ratio and add it to the list
	ratio = tick.open / (value_when_bought / amount)
	new_row = c(new_row, ratio)

	# set that it has yet to be sold
	new_row = c(new_row, FALSE)

	# set the sold date as the day bought
	new_row = c(new_row, tick.date)

	# get the amount sold
	value_when_sold = tick.open * amount
	new_row = c(new_row, value_when_sold)

	output = list(stock, tick.date, percentage_amount, value_when_bought, amount, tick.open, ratio, FALSE, tick.date, value_when_sold)

	#print(output)
	#print(new_row)

	print(portfolio[nrow(portfolio) + 1,1])
	portfolio[nrow(portfolio) + 1,1] = output[1]
	print(portfolio[nrow(portfolio),1])
	portfolio[nrow(portfolio),2] = output[2]
	portfolio[nrow(portfolio),3] = output[3]
	portfolio[nrow(portfolio),4] = output[4]
	portfolio[nrow(portfolio),5] = output[5]
	portfolio[nrow(portfolio),6] = output[6]
	portfolio[nrow(portfolio),7] = output[7]
	portfolio[nrow(portfolio),8] = output[8]
	portfolio[nrow(portfolio),9] = output[9]
	portfolio[nrow(portfolio),10] = output[10]

	#data.frame(lapply(output, function(x) t(data.frame(x))))

}

####################################################################