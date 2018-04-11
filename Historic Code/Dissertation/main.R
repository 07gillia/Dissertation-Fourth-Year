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
# setwd("/Users/ColossusMini/Documents/GIT/Dissertation Fourth Year/Dissertation")

# Set a source file for the functions
source("functions.R")
source("graphs.R")



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
    Capital_Value = double(),
    Stock_ratio = double()
)

stock_insights = data.frame(
    Date = as.Date(character()),
    Stock = character(),
    Stock_price = double(),
    bollBands_lower = double(),
    bollBands_middle = double(),
    bollBands_upper = double(),
    chandler_exit_1 = double(),
    chandler_exit_2 = double(),
    aroon_up = double(),
    aroon_down = double(),
    aroon_oscillator = double(),
    ATR_list = double(),
    bandwidth_list = double(),
    B_indicator_list = double()
)
stock_insights$Date <- strptime(stock_insights$Date , format="%Y-%m-%d %H:%M:%S")
stock_insights$Date <- as.POSIXct(stock_insights$Date)
stock_insights$Stock <- lapply(stock_insights$Stock, as.character)

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

total_data_points = (nrow(STOCK) - 49023) * (ncol(STOCK) - 2)

# 49490

####################################################################
# Testing - test the given algorithm over 
####################################################################

# Pre testing to set up the environment

available_columns = sample(2:46, 1, replace=F)

write.table(available_columns, "stocks_used.txt", sep="\t")

stock_variables = data.frame(
    Max_bollBands_lower = double(46),
    Max_bollBands_middle = double(46),
    Max_bollBands_upper = double(46),
    Max_chandler_exit_1 = double(46),
    Max_chandler_exit_2 = double(46),
    Max_aroon_up = double(46),
    Max_aroon_down = double(46),
    Max_aroon_oscillator = double(46),
    Max_ATR_list = double(46),
    Max_bandwidth_list = double(46),
    Max_B_indicator_list = double(46),
    Min_bollBands_lower = double(46),
    Min_bollBands_middle = double(46),
    Min_bollBands_upper = double(46),
    Min_chandler_exit_1 = double(46),
    Min_chandler_exit_2 = double(46),
    Min_aroon_up = double(46),
    Min_aroon_down = double(46),
    Min_aroon_oscillator = double(46),
    Min_ATR_list = double(46),
    Min_bandwidth_list = double(46),
    Min_B_indicator_list = double(46),
    Counter = integer(46)
)

####################################################################

start.time <- Sys.time()

current_time = STOCK[49021,1]
# typical value is 49021

ledger = my_functions.update_ledger(current_time)

start_row = 120000
# start - 49022
# will be using 0 for the main run

end_row = nrow(STOCK)-390
# end - nrow(STOCK)-390

edit_variable_stock_variables = 0

# iterate through row 1 -> end 
for (row in c(start_row:end_row)){
# start - 49022 end - nrow(STOCK)-390

    # iterate through the stocks in the dataframe columns 2 -> end (current = 4 end = 46)
    for (column in available_columns){
        

        # the stock could be null, if it is not trading can be done in that minute
        if(!is.na(STOCK[row,column])){
            

            # set the useful variables
            current_time = STOCK[row,1]
            current_stock = colnames(STOCK)[column]
            current_stock_price = STOCK[row,column]
            current_stock_ratio = ledger[nrow(ledger), 5]

            # get the longer variables done
            bollBands_list = my_functions.get_bollinger_bands(row, 120, current_stock)
            aroon_list = my_functions.aroon(row, current_stock, 120)

            # add to the stock insight 
            stock_insights[nrow(stock_insights) + 1,] = list(current_time,
            current_stock, current_stock_price,
            bollBands_list[1], bollBands_list[2], bollBands_list[3],
            my_functions.chandelier_exit(1, row, current_stock),
            my_functions.chandelier_exit(2, row, current_stock),
            aroon_list[1],aroon_list[2],aroon_list[3],
            my_functions.average_true_range(row, current_stock, 4),
            my_functions.get_bandwidth(row, 120, current_stock),
            my_functions.get_B_indicator(row, 120, current_stock, current_stock_price))

            # for each week get a max need to be changed based on the timeframe used
            if(row %% 390 == 0 & row > start_row + 400){
                hour_max = my_functions.get_max(my_functions.get_day(row, current_stock, 1))
                hour_min = my_functions.get_min(my_functions.get_day(row, current_stock, 1))
                max_row = row - my_functions.get_rows_since(row, current_stock, hour_max)
                min_row = row - my_functions.get_rows_since(row, current_stock, hour_min)

                hour_max_time = STOCK[max_row,1]
                hour_min_time = STOCK[min_row,1]

                max_details = stock_insights[(stock_insights$Date == hour_max_time & stock_insights$Stock == current_stock),]
                min_details = stock_insights[(stock_insights$Date == hour_min_time & stock_insights$Stock == current_stock),]

                max_details = max_details[-c(1,2,3)]
                min_details = min_details[-c(1,2,3)]

                all_details = c(max_details, min_details, 1)
                stock_variables[column,] = stock_variables[column,] + all_details

                edit_variable_stock_variables = edit_variable_stock_variables + 1
            }



            if(edit_variable_stock_variables > 2 & row >= 49022 & ledger[nrow(ledger),4] >= 100){

                if(my_functions.get_B_indicator(row, 120, current_stock, current_stock_price) > stock_variables[column,11] * 0.9){

                    portfolio = my_functions.buy(current_stock, current_stock_price, 100, current_time)

                }

            }

            if(nrow(portfolio) > 0){

                for (stock in 1:nrow(portfolio)) {

                    if(portfolio[stock,7] > 1.04 && portfolio[stock,8] == FALSE){

                        portfolio = my_functions.sell(portfolio[stock,1], current_time, current_stock_price)

                    }

                }

            }

            # update progress bar
            current_data_point = ((row - 48632) * (ncol(STOCK) - 2) + column)
            percentage = current_data_point / total_data_points * 100
            cat("\r",format(round(percentage, 3), nsmall = 3), "%")

        }

    }

    ledger = my_functions.update_ledger(current_time)

}


write.table(portfolio, "portfolio.txt", sep="\t")
write.table(ledger, "ledger.txt", sep="\t")

end.time <- Sys.time()

time.taken <- end.time - start.time
write.table(time.taken, "time.txt", sep="\t")

####################################################################

# Post run, make sure that all the results are dealt with correctly



####################################################################
# Results - Show the results of the algorithm
####################################################################

my_graphs.draw_graph(available_columns, stock_insights, ledger, STOCK)

####################################################################