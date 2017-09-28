####################################################################
# Read in the data
####################################################################

my_functions.readInData <- function() {

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

	data = list(AAL, ABF, ADM, AHT, BAB)

	return(data)

}

####################################################################