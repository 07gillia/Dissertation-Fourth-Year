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

portfolio$Bought_date <- strptime(portfolio$Bought_date , format="%Y-%m-%d %H:%M:%S")
portfolio$Sold_date <- strptime(portfolio$Sold_date , format="%Y-%m-%d %H:%M:%S")

portfolio$Bought_date <- as.POSIXct(portfolio$Bought_date)
portfolio$Sold_date <- as.POSIXct(portfolio$Sold_date)

ledger$Date <- strptime(ledger$Date , format="%Y-%m-%d %H:%M:%S")
ledger$Date <- as.POSIXct(ledger$Date)

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



####################################################################
# Testing - test the given algorithm over 
####################################################################

# iterate through row 1 -> end
for (row in c(49490:nrow(STOCK))) {
    #print(paste("ROW:", row))

    # iterate through the stocks in the dataframe columns 2 -> end
    for (column in c(2:ncol(STOCK))) {
        #print(paste("COLUMN:", column))

        # the stock could be null, if it is not trading can be done in that minute
        if(!is.na(STOCK[row,column]) && row >= 49022){
            #print(STOCK[row,column])

            # set the useful variables
            current_time = STOCK[row,1]
            current_stock = colnames(STOCK)[column]
            current_stock_price = STOCK[row,column]

            # print(current_time)
            # print(current_stock)
            # print(current_stock_price)

            # set the rolling averages for each of the timeframes
            # averages[current_stock,1] = mean(STOCK[row-30:row, current_stock], na.rm=TRUE)
            # averages[current_stock,2] = mean(STOCK[row-180:row, current_stock], na.rm=TRUE)
            # averages[current_stock,3] = mean(STOCK[row-360:row, current_stock], na.rm=TRUE)
            # averages[current_stock,4] = mean(STOCK[row-1170:row, current_stock], na.rm=TRUE)
            # averages[current_stock,5] = mean(STOCK[row-2340:row, current_stock], na.rm=TRUE)
            # print(averages)

            portfolio = my_functions.update(current_stock, current_stock_price, current_time)

            if(row == 49500 && current_stock == "AAPL"){
                portfolio = my_functions.buy(current_stock, current_stock_price, 1000, current_time)
            }

            if(nrow(portfolio) > 0) {
                for (stock in 1:nrow(portfolio)) {
                    if(portfolio[stock,7] > 1.01 && portfolio[stock,8] == FALSE){
                        portfolio = my_functions.sell(portfolio[stock,1], current_time, current_stock_price)
                    }
                }
            }
        }
    }
    #ledger = my_functions.update_ledger(current_time)

    print(portfolio)
    print(ledger)
}


####################################################################
# Results - Show the results of the algorithm
####################################################################




