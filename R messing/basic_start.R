####################################################################
# Basic start to getting a framework with which to trade for a year
####################################################################

# start with only one index - Babcock
# we can then do the same when we get all the other data

# Get and print current working directory
message("The working directory is : ", getwd())

# Set current working directory
# This shouldn't be run anywhere else so not an issue
setwd("/users/ColossusMini/Documents/R")

source("functions.R")

####################################################################
# Reading in the data and messing with it
####################################################################

# we should read in from an SQL database
# at the moment we are using a CSV file
# this will be changed
# any extra columns that are needed will be made prodominantly through SQL

# read in the babcock data and validate that it is as expected
data.all = read.csv("BAB.csv")
message(sprintf("The data is in a dataframe? : %s", is.data.frame(data.all)))
if(is.data.frame(data.all)) {
	message(sprintf("what are the dimentions of the dataframe? : x = %d by y = %d", ncol(data.all), nrow(data.all)))
}

data.all$Date <- as.Date(data.all$Date , "%Y-%m-%d")

str(data.all)

####################################################################
# making sure the framework is in place for iterating through the data
####################################################################

# we should just have a start and end date, if there is data then we will trade
# if there isn't then we wont
# run from one date to another

# the date we start trading
start_date = as.Date('2016-04-25')
# the day we end trading
end_date = as.Date('2017-04-28')
# this is the budget we have
capital = 1000

# work in percentages?
# give a budget?
# probably easy to work out once we have a couple of strategies
# percentage would make the most sense
# give a value of our budget
# assign a set percentage of that to a specific price
# work out a percentage of that compared to its stock price
# then multiply at a good time to get actual money back

# Create the data frame for storing the stocks that we own
portfolio = data.frame(
   	Stock = c(),
   	Bought.date = c(), 
   	Bought.percentage = c(),
   	Bought.value = c(),
   	Bought.amount = c(),
   	Current.value = c(),
   	Current.ratio = c(),
   	Sold = c(),
   	Sold.date = c(),
   	Sold.amount = c()
)

####################################################################
# the actual doing things bit
####################################################################

message("#############################################################")

data.tradingPeriod = my_functions.get_data_between(start_date, end_date)

total_ticks = nrow(data.tradingPeriod)

for (tick_number in c(1:total_ticks)) {
	tick.date = as.Date(data.tradingPeriod[tick_number,1])
	tick.open = data.tradingPeriod[tick_number,2]
	data.usable = my_functions.get_data_between(,tick.date)

	# this is what we do at every tick
	# in the case of the current data, every day

	my_date = as.Date('2016-04-25')

	if(tick.date == my_date) {
		my_functions.buy('BAB', 100)
	}
}

print(portfolio)

####################################################################