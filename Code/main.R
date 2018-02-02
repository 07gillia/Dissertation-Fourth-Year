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

# Read in the data file of all stocks
"Reading Stock Data"
Data = read.csv("../Data/ALLSTOCKS.csv")
sprintf("The data is in a dataframe? : %s", is.data.frame(Data))
if(is.data.frame(Data)) {
    sprintf("what are the dimentions of the dataframe? : x = %d by y = %d", ncol(Data), nrow(Data))
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
	Unique_ID = character(),
	Date_Bought = double(),
	Time_Bought = double(),
	Stock = character(),
	Number_Shares = double(),
	Cost_Per_Share = double(),
    stringsAsFactors=FALSE
)

Sold = data.frame(
	Unique_ID = character(),
	Date_Bought = double(),
	Time_Bought = double(),
	Stock = character(),
	Number_Shares = double(),
	Cost_Per_Share = double(),
	Date_Sold = double(),
	Time_Sold = double(),
	Price_Per_Share = double(),
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
# Start trading at X?

End_Date = 382
# Initial value = 382

Available_Capital = 100000

Available_Stocks = sort(sample(Stock_Names, 4, replace=F))
# Initial value = 44

Available_Dates = unique(Data$DATE)

sprintf("Stocks that will be used : %s", paste(Available_Stocks, collapse = " "))
# Output the stocks that will be used

Available_Data = Data[,c("DATE","TIME", Available_Stocks)]

print(Available_Dates)
print(length(Available_Dates))

####################################################################
# Iterate through each row



####################################################################
# Results - Show the results of the algorithm
####################################################################

#graphs.plot(Ledger)

# print(str(Data))
# print(str(Active))
# print(str(Sold))
# print(str(Ledger))

####################################################################