####################################################################
# Modular Algorithmic Trading Platform
####################################################################



####################################################################
# Setup File - set up this file, make sure all the supports are set
####################################################################

# run:
# export LANG=en_US.UTF-8
# export LC_ALL=en_US.UTF-8
# Rscript main.R > out.txt

# Get and print current working directory
sprintf("The working directory is : %s", getwd())

# Set current working directory
setwd("/Users/ColossusMini/Documents/GIT/Dissertation Fourth Year/Dissertation")

# Set a source file for the functions
source("functions.R")



####################################################################
# Setup Data - read in all the data and set up all the dataframes
####################################################################

# This is the budget we have
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

####################################################################

# read in the AA.csv data and validate it is as expected
"STOCK"
STOCK = read.csv("../Data/STOCK.csv")
sprintf("The data is in a dataframe? : %s", is.data.frame(STOCK))
if(is.data.frame(STOCK)) {
  sprintf("what are the dimentions of the dataframe? : x = %d by y = %d", ncol(STOCK), nrow(STOCK))
}
STOCK$X <- NULL
STOCK$TICKER. <- NULL
STOCK$PER. <- NULL
STOCK$DATE. <- NULL
STOCK$TIME. <- NULL
STOCK$HIGH. <- NULL
STOCK$OPEN. <- NULL
STOCK$LOW. <- NULL
STOCK$CLOSE. <- NULL
STOCK$DATETIME <- strptime(STOCK$DATETIME , format="%Y-%m-%d %H:%M:%S")
positions <- order(STOCK$DATETIME)
STOCK = STOCK[positions, ]

for (date in 1:length(STOCK$DATETIME)) {
  print(STOCK[date,])
  break
}



####################################################################
# Testing - test the given algorithm over 
####################################################################

# start date and time is 01/09/16 at 09:30
# end date and time is 31/08/2017 at 15:59
# 252 whole days of trading
# 2 days of half trading closing at 1
# a total of 98446 data points





####################################################################
# Results - Show the results of the algorithm
####################################################################




