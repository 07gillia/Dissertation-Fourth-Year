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
#source("graphs.R")

# Weird timezone settings
Sys.setenv(TZ="Europe/London")

####################################################################
# Setup Data - read in all the data and set up all the dataframes
####################################################################

start.time <- Sys.time()

# Read in the data file of all stocks
"Reading Stock Data"
Data = read.csv("../../Data/ALLSTOCKS.csv")
sprintf("The data is in a dataframe? : %s", is.data.frame(Data))
if(is.data.frame(Data)) {
    sprintf("What are the dimentions of the dataframe? : x = %d by y = %d", ncol(Data), nrow(Data))
}
Data$DATE = as.character(Data$DATE)
Data$TIME = as.character(Data$TIME)
Data$DATE = unlist(lapply(Data$DATE, action.date))
Data$TIME = unlist(lapply(Data$TIME, action.time))
Data$DATE = as.double(Data$DATE)
Data$TIME = as.double(Data$TIME)
Data$X = NULL
Data = Data[order(Data$DATE, Data$TIME),]
"Data read in and formatted"

# Set up the dataframes to store tracking data

# Create the data frame for storing the stocks that we own
Active = data.frame(
	Unique_ID = double(),
	Date_Bought = double(),
	Time_Bought = double(),
	Stock = character(),
	Number_Shares = double(),
	Cost_Per_Share = double(),
    stringsAsFactors=FALSE
)

Sold = data.frame(
	Unique_ID = double(),
	Date_Bought = double(),
	Time_Bought = double(),
	Stock = character(),
	Number_Shares = double(),
	Cost_Per_Share = double(),
	Amount_bought = double(),
	Date_Sold = double(),
	Time_Sold = double(),
	Price_Per_Share = double(),
	Amount_sold = double(),
	stringsAsFactors=FALSE
)

# create a dataframe to keep track of what the capital is doing
Ledger = data.frame(
    Date = double(),
    Value = double(),
    Stock_Value = double(),
    Capital_Value = double()
)

Date_List = unique(Data$DATE)

Stock_Names = c("AA", "AAPL", "ADBE", "AIG", "AMAT", "AMT", "AXP", "BA", "BAC", "C", "CA", "CAT", "CSCO", "CVX", "DD", "DIS", "EMC", "FSLR", "GE", "GLW", "GOOG", "GS", "HD", "HPQ", "IBM", "INTC", "IP", "JNJ", "JPM", "KO", "MCD", "MMM", "MRK", "MSFT", "PFE", "PG", "T", "TRV", "UTX", "VZ", "WFC", "WMT", "XOM", "YAHOO", "YNDX")

# print(str(Data))
# print(str(Active))
# print(str(Sold))
# print(str(Ledger))

####################################################################
# Testing - test the given algorithm over 
####################################################################

Start_Date = 1
# Initial value = 1
# Start trading at 130

End_Date = 382
# Initial value = 382

Available_Capital = 100000

Available_Stocks = sort(sample(Stock_Names, 4, replace=F))
# Initial value = 44

Available_Dates = unique(Data$DATE)

sprintf("Stocks that will be used : %s", paste(Available_Stocks, collapse = " "))
# Output the stocks that will be used

Available_Data = Data[,c("DATE","TIME", Available_Stocks)]

####################################################################

# Testing the function.R file

random_day = sample(Date_List, 1)
random_stock = sample(Available_Stocks, 1)
time_vec = subset(Data, DATE==random_day, select=TIME)
random_time = sample(time_vec$TIME,1)
test_vec = subset(Data, DATE==random_day, select=random_stock)

print(random_day)
print(random_time)
print(random_stock)
print(tail(test_vec,10))
print("########")

print(adv.get_rate_of_change(random_day, random_time, random_stock, 4))

stop()

####################################################################
# Iterate through each row

for (date in c(Start_Date:End_Date)) {
	# Each number is a date in the available dates list

	current_date = Available_Dates[date]
	available_date_data = Available_Data[Available_Data$DATE == current_date,]
	# Get any data that is needed at this stage



	################################################################
	# Iterate through each minute

	for (minute in c(1:length(available_date_data$TIME))) {
		# Each number is a row in the current day

		current_time = available_date_data[minute,2]
		# Get any data that is needed at this stage



		############################################################
		# Iterate through each stock

		for (stock in Available_Stocks) {
			# Iterate through each of the stocks

			current_stock_price = available_date_data[available_date_data$DATE == current_date & available_date_data$TIME == current_time, stock]
			# Get any data that is needed at this stage



			########################################################
			# Make sure that the stock has a value

			if(!is.na(current_stock_price)){
				# This means that there is a current price for this stock, time and date

				#all_data_available = Data[Data$DATE <= current_date & Data$TIME <= current_time, c("DATE", "TIME", stock)]
				#all_data_available = na.omit(all_data_available)



				####################################################
				# Check if the stock should be bought

				if(action.should_buy(current_date, current_time, stock) & date >= 130){
					# Test critiera for buy and that trading is open

					Active = action.buy(current_date, current_time, stock, 1000)
				}

				if(nrow(Active) > 1){
					# If there is something to sell

					to_remove = c()

					for (row in c(1:nrow(Active))) {
						# Iterate through each row in active

						uid = Active[row, 1]
						
						if(action.should_sell(uid, current_date, current_time) & Active[row,4] == stock){
							# Test the sell critirea

							row_data = Active[row,]

							to_remove = union(to_remove, c(row))

							Sold = action.sell(row_data, current_date, current_time, stock, current_stock_price)
						}
					}

					if(length(to_remove != 0)){
						Active = Active[!to_remove, ]
					}
				}
			}
		}				
	}

	percentage = (date - Start_Date) / (End_Date - Start_Date) * 100
    cat("\r",format(round(percentage, 3), nsmall = 3), "%")
    # show a percentage in the terminal
}
cat("\n")

end.time <- Sys.time()

time.taken <- end.time - start.time
sprintf("The time taken: %f", time.taken)

####################################################################
# Results - Show the results of the algorithm
####################################################################

#graphs.plot(Ledger)

print(Active)
print(Sold)

####################################################################