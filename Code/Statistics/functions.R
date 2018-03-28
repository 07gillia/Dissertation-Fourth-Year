####################################################################
# Useful Functions File
####################################################################

action.buy <- function(date, time, stock, amount){
	# The function that buys a stock given time, date, stock, and amount

	UID = round(runif(1, 0, 1000000000))
	# create a unique id for the transaction just choose a random number

	current_stock_value = Data[Data$DATE == date & Data$TIME == time, stock]
	# get the current stock value

	number_of_shares = amount / current_stock_value
	# get the number of shares that bought with amount

	output = list(UID,date,time,stock,number_of_shares,current_stock_value)
	# the variable that brings together all values

	Active[nrow(Active) + 1,] = output
	# add to the list of active shares

	return(Active)
}

action.sell <- function(row, date, time, stock, stock_price){
	# The function that sells given a UID

	output = list(row[1], row[2], row[3], row[4], row[5], row[6], row[5]*row[6], date, time, stock_price, row[5]*stock_price)

	Sold[nrow(Sold) + 1,] = output

	return(Sold)
}

action.update <- function(date, capital){
	# The function that will update the ledger every minute

	value_stock = 0

	value_capital = capital

	if(nrow(Active) > 0){
		for (x in c(1:nrow(Active))) {
			value_stock = value_stock + Active[x,5] * Active[x,6]
			value_capital = value_capital - Active[x,5] * Active[x,6]
		}
	}

	if(nrow(Sold) > 0){
		for (x in c(1:nrow(Sold))) {
			value_capital = value_capital + Sold[x,9] * Sold[x,5] - Sold[x,5] * Sold[x,6]
		}
	}

	total_value = value_capital + value_stock

	output = list(date, total_value, value_stock, value_capital)

	Ledger[nrow(Ledger) + 1,] = output

	return(Ledger)
}

action.date <- function(date){
	first = substr(date,7,8)
	second = substr(date,4,5)
	third = substr(date,1,2)
	result = paste(first,second,third, sep = "")
	return(result)
}

action.time <- function(time){
	first = substr(time,1,2)
	second = substr(time,4,5)
	result = paste(first,second,sep="")
	return(result)
}

####################################################################
# Decision Functions
####################################################################

action.should_buy <- function(current_date, current_time, current_stock){
	# Function that will decide if the current stock should be bought or not

	result = FALSE

	number = runif(1)

	if(number < 0.0001){
		result = TRUE
	}

	return(result)
}

action.should_sell <- function(uid, date, time){
	# Function that will decide if the current stock should be sold or not

	result = FALSE

	number = runif(1)

	if(number < 0.0005){
		result = TRUE
	}

	return(result)
}

####################################################################
# Basic Functions
####################################################################

use.get_max <- function(list){
	# given a list get the max value

	result = max(list, na.rm = TRUE)

	return(result)
}

use.get_min <- function(list){
	# given a list get the min value

	result = min(list, na.rm = TRUE)

	return(result)
}

use.get_average <- function(list){
	# given a list get the average value

	result = mean(list, na.rm = TRUE)

	return(result)
}

use.get_sd <- function(list){
	# given a list get the standard deviation

	result = sd(list, na.rm = TRUE)

	return(result)
}

use.get_x_date <- function(date, X){
	# given a date get the date X days ago

	index = match(date,Date_List)
	result = Date_List[index - X]

	return(result)
}

use.get_x_close <- function(date, stock, X){
	# given a date get the previous days close

	prev_date = use.get_x_date(date, X)
	prev_date_data = subset(Data, DATE==prev_date, select=stock)
	stock_data = na.omit(prev_date_data)
	result = tail(stock_data[, stock],1)

	return(result)
}

use.get_x_open <- function(date, stock, X){
	# given a date get the open X days ago

	prev_date = use.get_x_date(date, X)
	prev_date_data = subset(Data, DATE==prev_date, select=stock)
	stock_data = na.omit(prev_date_data)
	result = head(stock_data[, stock],1)

	return(result)
}

use.get_x_data_points <- function(date, time, stock, X){
	# given a date, time, and stock get the last X data points that are not NA

	stock_data = subset(Data, DATE<=date & TIME<=time, select=stock)
	stock_data = na.omit(stock_data)
	result = tail(stock_data[,stock], X)

	return(result)
}

use.get_x_day_data_points <- function(date, stock, X){
	# get the full day of data X days ago

	day_to_get = use.get_x_date(date, X)
	day_data = subset(Data, DATE==day_to_get, select=stock)
	result = na.omit(day_data)

	return(result)
}

####################################################################
# Advanced Functions - Technical Overlays
####################################################################

adv.get_bollinger_bands <- function(date, time, stock){
	# get the bollinger bands

	data_points = use.get_x_data_points(date, time, stock, 20)

	middle_band = use.get_average(data_points)
	upper_band = use.get_average(data_points) + (use.get_sd(data_points) * 2)
	lower_band = use.get_average(data_points) - (use.get_sd(data_points) * 2)

	result = c(middle_band, upper_band, lower_band)

	return(result)
}

adv.get_chandelier_exit <- function(date, time, stock){
	# get the chandelier exit

	data_points = use.get_x_data_points(date, time, stock, 22)

	long = use.get_max(data_points) - adv.get_average_true_range(date, time, stock, 22)
	short = use.get_min(data_points) + adv.get_average_true_range(date, time, stock, 22)

	result = c(long, short)

	return(result)
}

adv.get_ichimoku_cloud <- function(date, time, stock){
	# get the values of the ichimoku cloud

	data_points = use.get_x_data_points(date, time, stock, 9)
	line_1 = (use.get_max(data_points) + use.get_min(data_points)) / 2

	data_points = use.get_x_data_points(date, time, stock, 26)
	line_2 = (use.get_max(data_points) + use.get_min(data_points)) / 2

	line_3 = (line_1 + line_2) / 2

	data_points = use.get_x_data_points(date, time, stock, 52)
	line_4 = (use.get_max(data_points) + use.get_min(data_points)) / 2

	line_5 = use.get_x_close(date, stock, 26)

	result = c(line_1, line_2, line_3, line_4, line_5)

	return(result)
}

adv.get_kama <- function(date, time, stock){
	# get Kaufman's Adaptive Moving Average
	# SOMETIMES DOESNT WORK - NEEDS CHECKING

	change = abs(use.get_x_close(date, stock, 1) - use.get_x_close(date, stock, 10))
	volatility = 0
	for (i in c(1:10)) {
		volatility = volatility + abs(use.get_x_close(date, stock, i) - use.get_x_close(date, stock, i + 1))
	}
	ER = change / volatility

	SC = (ER * (2/(2+1) - 2/(30+1)) + 2/(30+1)) ^ 2

	data_points = use.get_x_data_points(date, time, stock, 15)
	prior_kama = use.get_average(data_points)

	result = prior_kama + SC * (use.get_x_data_points(date, time, stock, 1) - prior_kama)

	return(result)
}

adv.get_ketler_channels <- function(date, time, stock){
	# get the ketler channels

	data_points = use.get_x_data_points(date, time, stock, 20)
	middle_line = use.get_average(data_points)
	upper_channel_line = use.get_average(data_points) + (2 * adv.get_average_true_range(date, time, stock, 10))
	lower_channel_line = use.get_average(data_points) - (2 * adv.get_average_true_range(date, time, stock, 10))

	result = c(middle_line, upper_channel_line, lower_channel_line)

	return(result)
}

adv.get_ema <- function(date, time, stock, X){
	# get the exponential moving average over X number of iterations
	# IS BROKEN NEED CHECKING

	start_date = use.get_x_date(date, X)
	data_points = use.get_x_data_points(start_date, time, stock, 10)
	initial_value = use.get_average(data_points)
	multiplier = 2 / (X + 1)

	for (i in c(1:X)) {
		ema_close = use.get_x_close(date, stock, X - i)
		ema_previous_day = initial_value
		EMA = (ema_close - ema_previous_day) * multiplier + ema_previous_day

		initial_value = EMA
	}

	result = EMA

	return(result)
}

adv.get_moving_average_envelopes <- function(date, time, stock){
	# get MAE

	data_points = use.get_x_data_points(date, time, stock, 20)
	upper_envelope = use.get_average(data_points) + (use.get_average(data_points) * 0.025)
	lower_envelope = use.get_average(data_points) - (use.get_average(data_points) * 0.025)

	result = c(upper_envelope, lower_envelope)

	return(result)
}

adv.get_parabloic_sar <- function(date, time, stock){
	# calculate parabloic SAR
	# WHAT IS THE INITAL VALUE

	result = 0

	return(result)
}

adv.get_pivot_points_standard <- function(date, time, stock){
	# calculate privot points

	data_points = use.get_x_day_data_points(date, stock, 1)

	high = use.get_max(data_points)
	low = use.get_min(data_points)
	close = use.get_x_close(date, stock, 1)

	PP = (high + low + close) / 3
	support_1 = (PP * 2) - high
	support_2 = PP - (high - low)
	resistance_1 = (PP * 2) - low
	resistance_2 = PP + (high - low)

	result = c(PP, support_1, support_2, resistance_1, resistance_2)

	return(result)
}

adv.get_pivot_points_fibonacci <- function(date, time, stock){
	# calculate privot points

	data_points = use.get_x_day_data_points(date, stock, 1)

	high = use.get_max(data_points)
	low = use.get_min(data_points)
	close = use.get_x_close(date, stock, 1)

	PP = (high + low + close) / 3
	support_1 = PP - (0.382 * (high - low))
	support_2 = PP - (0.618 * (high - low))
	support_3 = PP - (1 * (high - low))
	resistance_1 = PP + (0.382 * (high - low))
	resistance_2 = PP + (0.618 * (high - low))
	resistance_3 = PP + (1 * (high - low))

	result = c(PP, support_1, support_2, support_3, resistance_1, resistance_2, resistance_3)

	return(result)
}

adv.get_pivot_points_demark <- function(date, time, stock){
	# calculate privot points

	data_points = use.get_x_day_data_points(date, stock, 1)

	high = use.get_max(data_points)
	low = use.get_min(data_points)
	close = use.get_x_close(date, stock, 1)
	open = use.get_x_open(date, stock, 0)

	if(close < open){
		X = high + (2 * low) + close
	}
	else if(close > open){
		X = (2 * high) + low + close
	}
	else if (close == open){
		X = high + low + (2 * close)
	}
	else{
		print("there is a problem with PPD")
		stop()
	}

	PP = X / 4
	support_1 = (X / 2) - high
	resistance_1 = (X / 2) - low

	result = c(PP, support_1, resistance_1)

	return(result)
}

adv.get_price_channels <- function(date, time, stock){
	# get the price channels

	data_points = use.get_x_data_points(date, time, stock, 20)

	upper_channel_line = use.get_max(data_points)
	lower_channel_line = use.get_min(data_points)
	center_channel_line = (upper_channel_line + lower_channel_line) / 2

	result = c(upper_channel_line, lower_channel_line, center_channel_line)

	return(result)
}



####################################################################
# Advanced Functions - Technical Indicators
####################################################################

adv.get_average_true_range <- function(date, time, stock, X){
	# get the average true range

	data_points = use.get_x_data_points(date, time, stock, X)

	method_1 = use.get_max(data_points) - use.get_min(data_points)
	method_2 = abs(use.get_max(data_points) - use.get_x_close(date, stock, 1))
	method_3 = abs(use.get_min(data_points) - use.get_x_close(date, stock, 1))

	result = use.get_max(c(method_1, method_2, method_3))

	return(result)
}

####################################################################