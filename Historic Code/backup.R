for (day_index in c(Start_Date:End_Date)) {
	# Iterate through every day of the data

	current_date = Date_List[day_index]
	current_date_data = subset(Available_Data, DATE == current_date,)
	# Set date variables and subset the data

	####################################################################
	# Minutes

	for (minute_index in c(1:nrow(current_date_data))) {
		# Iterate through the datapoints available

		current_time = as.character(current_date_data[minute_index,2])
		current_time_data = current_date_data[minute_index,3:ncol(current_date_data)]
		# Set the time data and subset the data further

		####################################################################
		# Stocks

		for (stock_index in c(1:ncol(current_time_data))) {
			# Iterate through the stocks available

			if(!is.na(current_time_data[,stock_index])){
				# make sure that the current value is not NA

				current_stock = names(current_time_data)[stock_index]
				current_stock_value = current_time_data[,stock_index]
				# Set the stock variables

				####################################################################
				# Data collection



				####################################################################
				# Buy

				if(day_index == 60 & minute_index == 50 & stock_index == 1){

					Active = action.buy(current_date, current_time, current_stock, 100000)
				}

				####################################################################
				# Sell

				if(nrow(Active) > 0){

					for (owned_index in c(1:nrow(Active))) {
						# iterate through every stock that is currently owned

						if( (Active[owned_index,5] * current_stock_value) >= (1.02 * Active[owned_index,5] * Active[owned_index,6]) && current_stock == Active[owned_index,4] ){

							UID = Active[owned_index, 1]

							Sold = action.sell(UID, current_date, current_time, current_stock)

							Active = Active[!(Active$Unique_ID == UID),]
						}
					}
				}
			}
		}
	}

	Ledger = action.update(current_date, Available_Capital)

    percentage = (day_index - Start_Date) / (End_Date - Start_Date) * 100
    cat("\r",format(round(percentage, 3), nsmall = 3), "%")
    # show a percentage in the terminal
}
cat("\n")