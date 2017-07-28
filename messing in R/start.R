####################################################################
# A restart that should lead to a better system of modularity and flexibility
####################################################################

####################################################################
# Setup File - set up this file, amke sure all the supports are set
####################################################################

# run:
# export LANG=en_US.UTF-8
# export LC_ALL=en_US.UTF-8
# change tab width
# Rscript start.R > out.txt

# Get and print current working directory
sprintf("The working directory is : %s", getwd())

# Set current working directory
# This shouldn't be run anywhere else so not an issue
setwd("/Users/ColossusMini/Documents/GIT/Dissertation Fourth Year/messing in R")

source("functions.R")

####################################################################
# Setup Data - read in all the data and set up all the dataframes
####################################################################

# the date we start trading
start_date = as.Date('2016-04-25')
# the day we end trading
end_date = as.Date('2017-04-28')
# this is the budget we have
capital = 10000

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

if(FALSE){
" the test part of the code, make sure that it works
str(portfolio)
str(ledger)
portfolio[nrow(portfolio) + 1,] = c('BAB', '2011-07-07', 100, 100, 100, 100, FALSE, '2011-07-07', 100)
ledger[nrow(ledger) + 1, ] = c('2016-04-24', capital, 0, capital)
str(portfolio)
print(portfolio)
str(ledger)
print(ledger)
"
}

# the data contains NULLs these will introduce NAs when typecast

# read in the babcock data and validate that it is as expected
BAB = read.csv("../Data/BAB.csv")
sprintf("The data is in a dataframe? : %s", is.data.frame(BAB))
if(is.data.frame(BAB)) {
	sprintf("what are the dimentions of the dataframe? : x = %d by y = %d", ncol(BAB), nrow(BAB))
}

BAB$Date <- as.Date(BAB$Date , "%Y-%m-%d")
suppressWarnings(BAB$Open <- as.numeric(as.character(BAB$Open)))
suppressWarnings(BAB$High <- as.numeric(as.character(BAB$High)))
suppressWarnings(BAB$Low <- as.numeric(as.character(BAB$Low)))
suppressWarnings(BAB$Close <- as.numeric(as.character(BAB$Close)))
suppressWarnings(BAB$Adj.Close <- as.numeric(as.character(BAB$Adj.Close)))
#str(BAB)
#print(BAB)

data = list(BAB)

number_of_stocks = 1

#print(data)

####################################################################
# Testing - test the given algorithm over 
####################################################################

# we will make a list of 5 pairs of dates for each of the stocks we have
# we will then trade between these two dates and return the amount of 
# of capital that has been made by each one, the average of this will be
# how well we have done on this specific stock
# the average of how well we did over all the stocks will be the result 
# for this algorithm

# the results will be output to a file
# the name of the stock
# the amount made
# the portfolio
# the ledger



# iterate through all the data available

for (stock in c(1:number_of_stocks)) {

# make the dates we will trade between for each stock

	for (date_int in c(1:5)) {

# make sure that the data reflects this

		the_data = tail(data[[stock]],10)
		print(the_data)
		print(date_int)

# iterate through all the trading dates and trade

	}

# output the results to a file

}


