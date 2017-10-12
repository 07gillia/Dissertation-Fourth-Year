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
    Unique_ID = character(),
    Stock = character(),
    Bought_date = as.Date(character()), 
    Bought_value = double(),
    Bought_amount = double(),
    Current_value = double(),
    Current_ratio = double(),
    Sold = logical(),
    Sold_date = as.Date(character()),
    Sold_value = double(),
    stringsAsFactors=FALSE
)

# create a dataframe to keep track of what the capital is doing
ledger = data.frame(
    Date = as.Date(character()),
    Value = double(),
    Stock_Value = double(),
    Capital_Value = double()
)

# create a dataframe to store different averages over timeframes
averages = data.frame(
    thirtyMinutes = double(),
    threeHours = double(),
    sixHours = double(),
    threeDays = double(),
    sixDays = double()
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

# Show a stock price graph
# attach(STOCK)
# plot(DATETIME, AAPL, type = "n") 
# lines(DATETIME, AAPL) 
# title("AAPL Stock Price")



####################################################################
# Testing - test the given algorithm over 
####################################################################

# start date and time is 01/09/16 at 09:30
# end date and time is 31/08/2017 at 15:59
# 252 whole days of trading
# 2 days of half trading closing at 1
# a total of 98446 data points

# 49022 is the start of trading

# iterate through the rows one at a time
for (time in c(1:nrow(STOCK))){
    #print(paste("The row", time))

    # only do anything inside the trading windows
    if(time >= 49022){
        #print(paste("Time:", STOCK[time,1]))

        # iterate through the columns one at a time for the current datetime
        for (column in names(STOCK)){
            #print(paste("The column", column))

            # don't do anything with the datetime column
            if(column != "DATETIME"){

                # set the useful variables
                current_time = STOCK[time,1]
                current_stock = column
                current_stock_price = STOCK[time,column]

                # set the rolling averages for each of the timeframes
                averages[current_stock,1] = mean(STOCK[time-30:time, current_stock], na.rm=TRUE)
                averages[current_stock,2] = mean(STOCK[time-180:time, current_stock], na.rm=TRUE)
                averages[current_stock,3] = mean(STOCK[time-360:time, current_stock], na.rm=TRUE)
                averages[current_stock,4] = mean(STOCK[time-1170:time, current_stock], na.rm=TRUE)
                averages[current_stock,5] = mean(STOCK[time-2340:time, current_stock], na.rm=TRUE)
                #print(averages)



            }
        }

        my_functions.update()
        my_functions.update_ledger()

    }
}

print(portfolio)
print(ledger)




####################################################################
# Results - Show the results of the algorithm
####################################################################




