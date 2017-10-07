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
source("readData.R")



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
"AA"
AA = read.csv("../Data/AA.csv")
sprintf("The data is in a dataframe? : %s", is.data.frame(AA))
if(is.data.frame(AA)) {
  sprintf("what are the dimentions of the dataframe? : x = %d by y = %d", ncol(AA), nrow(AA))
}
suppressWarnings(AA$TICKER <- as.character(AA$TICKER))
suppressWarnings(AA$DATE <- as.character(AA$DATE))
suppressWarnings(AA$TIME <- as.character(AA$TIME))
AA$PER <- NULL
AA$TICKER <- substring(AA$TICKER, 5)
AA$DATETIME = paste(AA$DATE,AA$TIME, sep="-")
AA$TIME <- NULL
AA$DATE <- NULL

# read in the AA.csv data and validate it is as expected
"AAPL"
AAPL = read.csv("../Data/AAPL.csv")
sprintf("The data is in a dataframe? : %s", is.data.frame(AAPL))
if(is.data.frame(AAPL)) {
  sprintf("what are the dimentions of the dataframe? : x = %d by y = %d", ncol(AAPL), nrow(AAPL))
}
suppressWarnings(AAPL$TICKER <- as.character(AAPL$TICKER))
suppressWarnings(AAPL$DATE <- as.character(AAPL$DATE))
suppressWarnings(AAPL$TIME <- as.character(AAPL$TIME))
AAPL$PER <- NULL
AAPL$TICKER <- substring(AAPL$TICKER, 5)
AAPL$DATETIME = paste(AAPL$DATE,AAPL$TIME, sep="-")
AAPL$TIME <- NULL
AAPL$DATE <- NULL

STOCK <- merge(AA,AAPL,by="DATETIME")



STOCK$DATETIME <- strptime(STOCK$DATETIME , format="%d/%m/%y-%H:%M")

positions <- order(STOCK$DATETIME)
STOCK = STOCK[positions, ]

print(STOCK)

####################################################################

# merge all the dataframe



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




