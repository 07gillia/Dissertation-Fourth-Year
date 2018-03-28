####################################################################
# Basic Functions
####################################################################

action.get_last_X_datapoints <- function(stock, time, date, x){
	# given a stock, time, and date get the previous X values of that stock

	current_index = Data[Data$DATE == date & Data$TIME == time,1]

	start_index = current_index - x

	rows = Data[c(start_index:current_index), stock]

	rows = rows[!is.na(rows)]

	return(rows)
}

action.get_last_X_days <- function(date_index, stock, x){
	# get the last X days not including the current day

	days = Date_List[(day_index-x):(date_index-1)]

	result = Data[(which(Data$DATE %in% days)), stock]

	result = result[!is.na(result)]

	return(result)
}

action.get_current_day_available <- function(time, date, stock){
	# get all values that are available for a specific day
	# given time and date, no looking into the future

	current_index = Data[Data$DATE == date & Data$TIME == time,1]

	day = Data[Data$DATE == date & Data$X <= current_index, stock]

	day = day[!is.na(day)]

	return(day)
}

action.get_previous_day <- function(day_index, stock){
	# get all values of the previous day given date

	date = Date_List[day_index - 1]

	day = Data[Data$DATE == date,stock]

	day = day[!is.na(day)]

	return(day)
}

action.get_previous_date <- function(day_index){
	# return the date of the day before the current date

	date = Date_List[day_index - 1]

	return(date)
}

action.get_max <- function(list){
	# given a vector find the maximum value

	result = max(list, na.rm = FALSE)

	return(result)
}

action.get_min <- function(list){
	# given a vector find the minimum value

	result = min(list, na.rm = FALSE)

	return(result)
}

action.get_average <- function(list){

	result = mean(list)

	return(list)
}

action.get_standard_deviation <- function(list){
	# given some variables get the standard deviation over them

	result = sd(list)

	return(result)
}

action.get_total_gain_loss <- function(list){
	# get the total amount that a stock increased in value by given a list

	total_gain = 0
	total_loss = 0

	for (i in c(2:length(list)-1)){

		current = list[i]
		next_value = list[i+1]

		if(current > next_value){
			total_loss = total_loss + current - next_value
		}
		else if(current < next_value){
			total_gain = total_gain + next_value - current
		}
	}
	return(c(total_gain, total_loss))
}

####################################################################
# Advanced Functions
####################################################################

action.get_previous_day_close <- function(day_index, stock){
	# get the last value of the preious day given a stock, time, and date
	# need to make sure that data exists for that day

	if(length(action.get_previous_day(day_index, stock)) < 1){
		previous_day = action.get_previous_day(day_index - 1, stock)
	}
	else{
		previous_day = action.get_previous_day(day_index, stock)
	}

	previous_day = previous_day[!is.na(previous_day)]

	result = previous_day[length(previous_day)]

	return(result)
}

action.get_moving_average <- function(day_index, stock, days){
	# a simple moving average calculation

	if(day_index < days){
		stop("Error with get moving average")
	}

	counter = 0

	for (day in c((day_index-days):(day_index-1))) {
		# iterate through each date

		counter = counter + action.get_previous_day_close(day, stock)
	}

	result = counter / days

	return(result)
}

action.get_average_true_range <- function(day_index, stock, list){
	# Method 1: Current High less the current Low
	# Method 2: Current High less the previous Close (absolute value)
	# Method 3: Current Low less the previous Close (absolute value)

	method_1 = max(list) - min(list)

	method_2 = abs(max(list) - action.get_previous_day_close(day_index, stock))

	method_3 = abs(max(list) - action.get_previous_day_close(day_index, stock))

	return(action.get_max(c(method_1, method_2, method_3)))
}

action.get_exponential_moving_average <- function(day_index, stock, days){
	# get the EMA over the set number of days for the specific stock

	data = action.get_last_X_days(day_index, stock, days)

	multiplier = (2/(days + 1))

	ema = 0 # hard
}

####################################################################
# Technical Indicators
####################################################################

action.get_bollinger_bands <- function(day_index, stock){
	# Middle Band = 20-day simple moving average (SMA)
  	# Upper Band = 20-day SMA + (20-day standard deviation of price x 2) 
	# Lower Band = 20-day SMA - (20-day standard deviation of price x 2)

	# SMA - simple moving average

	if(day_index < 20){
		stop("Error with Bollinger Bands")
	}

	SMA = action.get_moving_average(day_index, stock, 20)

	SD = action.get_standard_deviation(action.get_last_X_days(day_index, stock,20)) 

	middle_band = SMA
	upper_band = SMA + SD * 2
	lower_band = SMA - SD * 2

	return(c(lower_band, middle_band, upper_band))
}

action.get_chandelier_exit <- function(day_index, stock){
	# Chandelier Exit (long) = 22-day High - ATR(22) x 3 
	# Chandelier Exit (short) = 22-day Low + ATR(22) x 3

	x22_days = action.get_last_X_days(day_index, stock, 22)
	x22_day_max = action.get_max(x22_days)
	x22_day_min = action.get_min(x22_days)

	long = x22_day_max - (action.get_average_true_range(day_index, stock, x22_days) * 3)
	short = x22_day_min - (action.get_average_true_range(day_index, stock, x22_days) * 3)

	return(c(long, short))
}

action.get_ichimoku_cloud <- function(date_index, stock){
	# conversion line = (9-period high + 9-period low)/2))
	# base line = (26-period high + 26-period low)/2))
	# leading span A = (Conversion Line + Base Line)/2))
	# leading span B = (52-period high + 52-period low)/2))
	# lagging span = Close plotted 26 days in the past

	if(date_index < 52){
		stop("Error in Ichimoku Cloud")
	}

	conversion_line = (action.get_max(action.get_last_X_days(date_index, stock, 9)) + action.get_min(action.get_last_X_days(date_index, stock, 9)))/2

	base_line = (action.get_max(action.get_last_X_days(date_index, stock, 26)) + action.get_min(action.get_last_X_days(date_index, stock, 26)))/2

	leading_span_A = (conversion_line + base_line)/2

	leading_span_B = (action.get_max(action.get_last_X_days(date_index, stock, 52)) + action.get_min(action.get_last_X_days(date_index, stock, 52)))/2

	lagging_span = action.get_previous_day_close(day_index-26, stock)

	return(c(conversion_line, base_line, leading_span_A, leading_span_B, lagging_span))
}

KAMA = c()

action.get_kaufman_adaptive_moving_average <- function(day_index, stock, current_stock_price){ # DON'T KNOW HOW TO DO!!!!
	# Current KAMA = Prior KAMA + SC x (Price - Prior KAMA)

	# SC = [ER x (fastest SC - slowest SC) + slowest SC]2
	# SC = [ER x (2/(2+1) - 2/(30+1)) + 2/(30+1)]2

	# ER = Change/Volatility
	# Change = ABS(Close - Close (10 periods ago))
	# Volatility = Sum10(ABS(Close - Prior Close))
	# Volatility is the sum of the absolute value of the last ten price changes (Close - Prior Close).

	change = abs(action.get_previous_day_close(day_index, stock) - action.get_previous_day_close((day_index-10), stock))

	volatility = 0

	for (i in c(1:10)) {
		volatility = volatility + abs(action.get_previous_day_close(day_index - i + 1, stock) - action.get_previous_day_close(day_index - i, stock))
	}

	if(change == 0 | volatility == 0){
		ER = 0
	}
	else{
		ER = change / volatility
	}

	SC = (ER * (2/(2+1) - 2/(30+1)) + 2/(30+1))^2

	if(length(KAMA) < 1){
		# if this has not been calculated before
		result = SC * current_stock_price
	}
	else{
		result = tail(KAMA,1) + SC * (current_stock_price - tail(KAMA,1))
	}

	append(KAMA, result)

	return(result)
}

action.get_keltner_channels <- function(){
	# Middle Line: 20-day exponential moving average 
	# Upper Channel Line: 20-day EMA + (2 x ATR(10))
	# Lower Channel Line: 20-day EMA - (2 x ATR(10))


}

####################################################################