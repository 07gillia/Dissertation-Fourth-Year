####################################################################
# Basic start to getting a framework with which to trade for a year
####################################################################

# run:
# export LANG=en_US.UTF-8
# export LC_ALL=en_US.UTF-8
# change tab width
# Rscript basic_start.R > out.txt

# start with only one index - Babcock
# we can then do the same when we get all the other data

# Get and print current working directory
message("The working directory is : ", getwd())

# Set current working directory
# This shouldn't be run anywhere else so not an issue
setwd("/Users/ColossusMini/Documents/GIT/Dissertation Fourth Year/messing in R")

source("functions.R")

####################################################################
# Reading in the data and messing with it
####################################################################

# we should read in from an SQL database
# at the moment we are using a CSV file
# this will be changed
# any extra columns that are needed will be made prodominantly through SQL

# the data contains NULLs these will introduce NAs when typecast

# read in the babcock data and validate that it is as expected
data.all.BAB = read.csv("../Data/BAB.csv")
message(sprintf("The data is in a dataframe? : %s", is.data.frame(data.all.BAB)))
if(is.data.frame(data.all.BAB)) {
	message(sprintf("what are the dimentions of the dataframe? : x = %d by y = %d", ncol(data.all.BAB), nrow(data.all.BAB)))
}

data.all.BAB$Date <- as.Date(data.all.BAB$Date , "%Y-%m-%d")

suppressWarnings(data.all.BAB$Open <- as.numeric(as.character(data.all.BAB$Open)))
suppressWarnings(data.all.BAB$High <- as.numeric(as.character(data.all.BAB$High)))
suppressWarnings(data.all.BAB$Low <- as.numeric(as.character(data.all.BAB$Low)))
suppressWarnings(data.all.BAB$Close <- as.numeric(as.character(data.all.BAB$Close)))
suppressWarnings(data.all.BAB$Adj.Close <- as.numeric(as.character(data.all.BAB$Adj.Close)))

#str(data.all.BAB)

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
capital = 10000

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
   	Stock = character(),
   	Bought.date = as.Date(character()), 
   	Bought.value = double(),
    Bought.amount = double(),
   	Current.value = double(),
   	Current.ratio = double(),
   	Sold = logical(),
   	Sold.date = as.Date(character()),
   	Sold.value = double(),
   	stringsAsFactors=FALSE
)

# create a dataframe to keep track of what the capital is doing
ledger = data.frame(
    Date = as.Date(character()),
    Value = double(),
    Stock_Value = double(),
    Capital_Value = double()
)

#portfolio[nrow(portfolio) + 1,] = c('BAB', '2011-07-07', 100, 100, 100, 100, FALSE, '2011-07-07', 100)

#ledger[nrow(ledger) + 1, ] = c('2016-04-24', capital, 0, capital)

#print(ledger)

####################################################################
# the actual doing things bit
####################################################################

print("#############################################################")

print("Pre-Processing")

data.tradingPeriod = my_functions.get_data_between(start_date, end_date)

total_ticks = nrow(data.tradingPeriod)

#sprintf("The amount of capital spent: %s" ,total_bought)
#sprintf("The amount of capital recieved: %s" ,total_sold)

#print(portfolio[])

print("#############################################################")

print("Iteration")

# loop through each row in the data
for (tick_number in c(1:total_ticks)) {
    # set up all the variables that will be used in this iteration
	tick.date = as.Date(data.tradingPeriod[tick_number,1])
	tick.open = data.tradingPeriod[tick_number,2]
	data.usable = my_functions.get_data_between(,tick.date)

	# this is what we do at every tick
	# in the case of the current data, every day

    # update the values in the portfolio
    portfolio = my_functions.update()

    "
	my_date = as.Date('2016-04-25')
	if(tick.date == my_date) {
		portfolio = my_functions.buy('BAB', 10000)
	}
    my_date = as.Date('2016-06-07')
    if(tick.date == my_date) {
        print(portfolio)
        portfolio = my_functions.sell('BAB', 1)
        print(portfolio)
    }
    " # dummy buy and sell to make sure it works

    average_stock_price_last_250 = mean(tail(data.usable[ , 2], 250))
    average_stock_price_last_50 = mean(tail(data.usable[ , 2], 50))
    average_stock_price_last_10 = mean(tail(data.usable[ , 2], 10))

    #print(average_stock_price_last_250)
    #print(average_stock_price_last_50)
    #print(average_stock_price_last_10)
    #print("####")

    if(tick.open < average_stock_price_last_250 * 0.95 ) {
        portfolio = my_functions.buy('BAB', 300)
    }

    portfolio = my_functions.sell('BAB', 1.05)

    # update the values in the ledger
    ledger = my_functions.update_ledger()
}

print("#############################################################")

print("Results")

#print(portfolio[])

#print(ledger[])

capital_return = tail(ledger[ , 2], 1)

result_percentage = (capital_return / capital) * 100

sprintf("Raw made over the timeframe: £%f", capital_return - capital)

sprintf("Percentage made over the timeframe: %f%%", result_percentage)

#############################################################

# Make the line chart to show how we did over the year

# convert factor to numeric for convenience 
ntrees <- 3
# get the range for the x and y axis 
xrange <- range(ledger[,1]) 
min = min(c(min(ledger[,2], ledger[,3], ledger[,4])))
max = max(c(max(ledger[,2], ledger[,3], ledger[,4])))
yrange <- c(min,max)

# set up the plot 
plot(xrange, yrange, type="n", xlab="Date", ylab="Amount (£)" ) 
colors <- rainbow(ntrees) 
linetype <- c(1,1,1)
plotchar <- seq(18,18+ntrees,1)

# add lines 
lines(ledger[,1], ledger[,2], type="l", lwd=1.5,
lty=linetype[1], col='Black', pch=plotchar[1]) 

lines(ledger[,1], ledger[,3], type="l", lwd=1.5,
lty=linetype[2], col='Red', pch=plotchar[2]) 

lines(ledger[,1], ledger[,4], type="l", lwd=1.5,
lty=linetype[3], col='Green', pch=plotchar[3]) 

# add a title and subtitle 
title("Algorithmic Success")

####################################################################