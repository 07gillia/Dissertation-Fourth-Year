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

# Set a source file for the functions
source("functions.R")
source("graphs.R")

# Weird timezone settings
Sys.setenv(TZ="Europe/London")

####################################################################
# Setup Data - read in all the data and set up all the dataframes
####################################################################

# Read in the data file of all stocks
"Reading Stock Data"
Data = read.csv("../Data/ALLSTOCKS.csv")
sprintf("The data is in a dataframe? : %s", is.data.frame(Data))
if(is.data.frame(Data)) {
    sprintf("what are the dimentions of the dataframe? : x = %d by y = %d", ncol(Data), nrow(Data))
}
Data$X = NULL
"Data read in and formatted"

# Set up the dataframes to store tracking data

# Create the data frame for storing the stocks that we own
Active = data.frame(
	Unique_ID = character(),
	Date_Bought = character(),
	Time_Bought = character(),
	Stock = character(),
	Number_Shares = double(),
	Cost_Per_Share = double(),
    stringsAsFactors=FALSE
)

Sold = data.frame(
	Unique_ID = character(),
	Date_Bought = character(),
	Time_Bought = character(),
	Stock = character(),
	Number_Shares = double(),
	Cost_Per_Share = double(),
	Date_Sold = character(),
	Time_Sold = character(),
	Price_Per_Share = double(),
	stringsAsFactors=FALSE
)

# create a dataframe to keep track of what the capital is doing
ledger = data.frame(
    Date = character(),
    Time = character(),
    Value = double(),
    Stock_Value = double(),
    Capital_Value = double(),
    Stock_ratio = double()
)

Date_List = unique(as.Date(Data$DATE, format = "%d/%m/%y"))
Date_List = Date_List[order(Date_List)]
Date_List = as.character(Date_List, format = "%d/%m/%y")

Stock_Names = c("AA", "AAPL", "ADBE", "AIG", "AMAT", "AMT", "AXP", "BA", "BAC", "C", "CA", "CAT", "CSCO", "CVX", "DD", "DIS", "EMC", "FSLR", "GE", "GLW", "GOOG", "GS", "HD", "HPQ", "IBM", "INTC", "IP", "JNJ", "JPM", "KO", "MCD", "MMM", "MRK", "MSFT", "PFE", "PG", "T", "TRV", "UTX", "VZ", "WFC", "WMT", "XOM", "YAHOO", "YNDX")

####################################################################
# Testing - test the given algorithm over 
####################################################################

Start_Date = 1
# Initial value = 1 
# Start trading at X?

End_Date = 382
# Initial value = 382

Available_Stocks = sort(sample(Stock_Names, 4, replace=F))
# Initial value = 44

sprintf("Stocks that will be used : %s", paste(Available_Stocks, collapse = " "))
# Output the stocks that will be used

Available_Data = Data[ ,(which(names(Data) %in% union("TIME",union("DATE", Available_Stocks))))]

counter = 0

####################################################################

for (day_index in c(Start_Date:End_Date)) {
	# Iterate through every day of the data

	current_date = Date_List[day_index]
	current_date_data = subset(Available_Data, DATE == current_date,)
	# Set date variables and subset the data

	for (minute_index in c(1:nrow(current_date_data))) {
		# Iterate through the datapoints available

		current_time = as.character(current_date_data[minute_index,2])
		current_time_data = current_date_data[minute_index,3:ncol(current_date_data)]
		# Set the time data and subset the data further

		for (stock_index in c(1:ncol(current_time_data))) {
			# Iterate through the stocks available

			if(!is.na(current_time_data[,stock_index])){
				# make sure that the current value is not NA

				current_stock = names(current_time_data)[stock_index]
				current_stock_value = current_time_data[,stock_index]
				# Set the stock variables

				if(day_index == 4 & minute_index == 5 & stock_index == 1){
					Active = action.buy(current_date, current_time, current_stock, 100)
				}

				if(nrow(Active) > 0){

					for (owned_index in c(1:nrow(Active))) {
						# iterate through every stock that is currently owned

						if( (Active[owned_index,5] * current_stock_value) >= (1.02 * Active[owned_index,5] * Active[owned_index,6]) && current_stock == Active[owned_index,4] ){
							print(Active[owned_index,])
							print(current_stock_value)
							print(current_date)
							print(current_time)

							UID = Active[owned_index, 1]

							action.sell(UID, current_date, current_time, current_stock)

							stop()
						}
					}
				}
			}
		}
	}

    percentage = (day_index - Start_Date) / (End_Date - Start_Date) * 100
    cat("\r",format(round(percentage, 3), nsmall = 3), "%")
    # show a percentage in the terminal
}
cat("\n")

print(counter)

print(Active)
print(Sold)

####################################################################
# Results - Show the results of the algorithm
####################################################################

						# if( (Active[owned_index,5] * current_stock_value) >= (1.02 * Active[owned_index,5] * Active[owned_index,6]) ){
						# 	UID = Active[owned_index,1]
						# 	action.sell(UID, current_date, current_time, current_stock])

						# 	print(Active)
						# 	print(Sold)

						# 	stop()
						# }
