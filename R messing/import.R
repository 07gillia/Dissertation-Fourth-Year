# getting file import working

# Get and print current working directory
#print(getwd())

# Set current working directory
setwd("/users/ColossusMini/Documents/R")

# Get and print current working directory
#print(getwd())

input = read.csv("BAB.csv")
data = tail(input,285)
print(is.data.frame(data))
print(ncol(data))
print(nrow(data))

# this changed the data in the table from factor to numeric, this changed the number of trailing zeros 
data[,2:7] = lapply(data[,2:7], function(x) as.numeric(as.character(x)))

# add a column called mean which is the running average
data$Mean = rowMeans(data[,2:4])

####################################################################



####################################################################

print("-----------------------------")

for (day in c(35:40)){

	# make sure the data is fine for the day
  	daily_data = data[0:day, ]

  	daily_data$Mean <- rowMeans(daily_data[,2:4])

  	#print(ncol(daily_data))
  	#print(nrow(daily_data))

  	past_average = mean(daily_data$Mean)


  	print(past_average)
}