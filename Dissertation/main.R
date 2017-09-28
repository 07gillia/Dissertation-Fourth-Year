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

# read in the AAl data and validate that it is as expected
"AAL"
AAL = read.csv("../Data/AAL.csv")
sprintf("The data is in a dataframe? : %s", is.data.frame(AAL))
if(is.data.frame(AAL)) {
	sprintf("what are the dimentions of the dataframe? : x = %d by y = %d", ncol(AAL), nrow(AAL))
}

AAL$Date <- as.Date(AAL$Date , "%Y-%m-%d")
suppressWarnings(AAL$Open <- as.numeric(as.character(AAL$Open)))
suppressWarnings(AAL$High <- as.numeric(as.character(AAL$High)))
suppressWarnings(AAL$Low <- as.numeric(as.character(AAL$Low)))
suppressWarnings(AAL$Close <- as.numeric(as.character(AAL$Close)))
suppressWarnings(AAL$Adj.Close <- as.numeric(as.character(AAL$Adj.Close)))

# read in the ABF data and validate that it is as expected
"ABF"
ABF = read.csv("../Data/ABF.csv")
sprintf("The data is in a dataframe? : %s", is.data.frame(ABF))
if(is.data.frame(ABF)) {
	sprintf("what are the dimentions of the dataframe? : x = %d by y = %d", ncol(ABF), nrow(ABF))
}

ABF$Date <- as.Date(ABF$Date , "%Y-%m-%d")
suppressWarnings(ABF$Open <- as.numeric(as.character(ABF$Open)))
suppressWarnings(ABF$High <- as.numeric(as.character(ABF$High)))
suppressWarnings(ABF$Low <- as.numeric(as.character(ABF$Low)))
suppressWarnings(ABF$Close <- as.numeric(as.character(ABF$Close)))
suppressWarnings(ABF$Adj.Close <- as.numeric(as.character(ABF$Adj.Close)))

# read in the ADM data and validate that it is as expected
"ADM"
ADM = read.csv("../Data/ADM.csv")
sprintf("The data is in a dataframe? : %s", is.data.frame(ADM))
if(is.data.frame(ADM)) {
	sprintf("what are the dimentions of the dataframe? : x = %d by y = %d", ncol(ADM), nrow(ADM))
}

ADM$Date <- as.Date(ADM$Date , "%Y-%m-%d")
suppressWarnings(ADM$Open <- as.numeric(as.character(ADM$Open)))
suppressWarnings(ADM$High <- as.numeric(as.character(ADM$High)))
suppressWarnings(ADM$Low <- as.numeric(as.character(ADM$Low)))
suppressWarnings(ADM$Close <- as.numeric(as.character(ADM$Close)))
suppressWarnings(ADM$Adj.Close <- as.numeric(as.character(ADM$Adj.Close)))

# read in the AAl data and validate that it is as expected
"AHT"
AHT = read.csv("../Data/AHT.csv")
sprintf("The data is in a dataframe? : %s", is.data.frame(AHT))
if(is.data.frame(AHT)) {
	sprintf("what are the dimentions of the dataframe? : x = %d by y = %d", ncol(AHT), nrow(AHT))
}

AHT$Date <- as.Date(AHT$Date , "%Y-%m-%d")
suppressWarnings(AHT$Open <- as.numeric(as.character(AHT$Open)))
suppressWarnings(AHT$High <- as.numeric(as.character(AHT$High)))
suppressWarnings(AHT$Low <- as.numeric(as.character(AHT$Low)))
suppressWarnings(AHT$Close <- as.numeric(as.character(AHT$Close)))
suppressWarnings(AHT$Adj.Close <- as.numeric(as.character(AHT$Adj.Close)))



# read in the BAB data and validate that it is as expected
"BAB"
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



# Do the same for all other stocks that are available

####################################################################

# a list of the stock that are being used
data_labels = list("AAL","ABF","ADM","AHT","BAB")
data = list(AAL,ABF,ADM,AHT,BAB)

# print(data_labels)
# print(data)



####################################################################
# Testing - test the given algorithm over 
####################################################################

# Get a start date
# Get the data for the correct timeframe
# Iterate through the data tickwise
# Execute the trading algorithm on each tick





####################################################################
# Results - Show the results of the algorithm
####################################################################




