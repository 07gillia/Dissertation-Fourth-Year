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
			value_stock = value_stock + Active[[x,5]] * Active[[x,6]]
			value_capital = value_capital - Active[[x,5]] * Active[[x,6]]
		}
	}

	if(nrow(Sold) > 0){
		for (x in c(1:(nrow(Sold) - 1))) {
			value_capital = value_capital + (Sold[[x,10]] * Sold[[x,5]]) - (Sold[[x,5]] * Sold[[x,6]])
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

action.should_buy <- function(current_date, current_time, current_stock, stock_tracker){
	# Function that will decide if the current stock should be bought or not

	result = FALSE

	if(length(stock_tracker) > 1){

		range = range(tail(stock_tracker,1000))
		
		if(tail(stock_tracker,1) > (1.04 * range(1))){
			result = TRUE
		}
	}

	return(result)
}

action.should_sell <- function(uid, date, time){
	# Function that will decide if the current stock should be sold or not

	result = FALSE

	if(length(stock_tracker) > 1){

		range = range(tail(stock_tracker,1000))
		
		if(tail(stock_tracker,1) > (0.96 * range(2))){
			result = TRUE
		}
	}

	return(result)
}

action.update_tracker <- function(input, date, time, stock){
	# a function that will add whatever value to the stock tracker

	answer = adv.get_bollinger_bands(date, time, stock)[1]

	result = c(input, answer)

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

	if(index <= X){
		print("There has been an issue with get_x_date")
		stop()
	}

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

	if(length(stock_data[,stock]) < X){
		print("There has been an issue with get_x_data_points, not enough data")
		stop()
	}

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

use.get_x_since_price <- function(date, time, stock, price){
	# get the number of periods since the price specified

	data_available = subset(Data, DATE<=date & TIME<=time, select=stock)
	value_vec = data_available[,1]
	rev_value_vec = rev(value_vec)
	rev_value_vec = na.omit(rev_value_vec)
	rows_since = match(price,rev_value_vec)
	result = rows_since

	return(result)
}

use.get_typical_price <- function(date, stock){
	# calculate the typical price of that day for that stock

	day_data = use.get_x_day_data_points(date, stock, 0)
	high = use.get_max(day_data)
	low = use.get_min(day_data)
	close = use.get_x_close(date, stock, 0)

	result = (high + low + close) / 3 

	return(result)
}

use.get_total_gain_loss <- function(date, time, stock, X){
	# find the total amount that a stock went up and down in X periods

	data_points = use.get_x_data_points(date, time, stock, X)

	gain = 0
	loss = 0

	for (i in c(1:(length(data_points) - 1))) {

		first = data_points[0+i]
		second = data_points[1+i]

		if(first < second){
			gain = gain + (second - first)
		}
		else if(first > second){
			loss = loss + (first - second)
		}
	}

	result = c(gain, loss)

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

adv.get_aroon <- function(date, time, stock){
	# get the arron

	data_points = use.get_x_data_points(date, time, stock, 25)
	max = use.get_max(data_points)
	min = use.get_min(data_points)
	days_since_max = use.get_x_since_price(date, time, stock, max)
	days_since_min = use.get_x_since_price(date, time, stock, min)

	aroon_up = ((25.0 - days_since_max) / 25.0) * 100.0
	aroon_down = ((25.0 - days_since_min) / 25.0) * 100.0

	result = c(aroon_up, aroon_down)

	return(result)
}

adv.get_aroon_oscillator <- function(date, time, stock){
	# use the aroon function to get the aroon oscillator

	aroon = adv.get_aroon(date, time, stock)

	result = aroon[1] - aroon[2]

	return(result)
}

adv.get_average_true_range <- function(date, time, stock, X){
	# get the average true range

	data_points = use.get_x_data_points(date, time, stock, X)

	method_1 = use.get_max(data_points) - use.get_min(data_points)
	method_2 = abs(use.get_max(data_points) - use.get_x_close(date, stock, 1))
	method_3 = abs(use.get_min(data_points) - use.get_x_close(date, stock, 1))

	result = use.get_max(c(method_1, method_2, method_3))

	return(result)
}

adv.get_bandwidth <- function(date, time, stock){
	# use the bollinger bands to get bandwidth

	bands = adv.get_bollinger_bands(date, time, stock)

	result = ((bands[2] - bands[3]) / bands[1]) * 100

	return(result)
}

adv.get__B <- function(date, time, stock){
	# use the bollinger bands to calculate %B

	bands = adv.get_bollinger_bands(date, time, stock)

	result = (use.get_x_data_points(date, time, stock, 1) - bands[3]) / (bands[2] - bands[3])

	return(result)
}

adv.get_commodity_channel_index <- function(date, time, stock){
	# calculate the CCI

	constant = 0.015
	current_tp = use.get_typical_price(date, stock)
	list_tp = c()
	for (i in c(1:20)) {
		new_date = use.get_x_date(date, i)
		list_tp = c(list_tp, use.get_typical_price(new_date, stock))
	}
	sma_tp = use.get_average(list_tp)
	MD = sum(abs(list_tp - sma_tp)) / 20

	result = (current_tp - sma_tp) / (constant * MD)

	return(result)
}

adv.get_coppock_curve <- function(date, time, stock){
	# calculate the coppock curve

	dummy_list = c()

	for (i in c(1:10)) {

		new_date = use.get_x_date(date, i)

		roc_14 = adv.get_rate_of_change(new_date, time, stock, 14)
		roc_11 = adv.get_rate_of_change(new_date, time, stock, 11)

		temp = roc_14 + roc_11

		dummy_list = c(dummy_list, temp)
	}

	result = use.get_average(dummy_list)

	return(result)
}

adv.get_price_momentum_oscillator <- function(date, time, stock, X){
	# calculate the PMO
	# DIFFICULT NEED TIME

	smoothing_multiplier = (2 / X)

	result = 0

	return(result)
}

adv.get_detrended_price_oscillator <- function(date, time, stock, X){
	# calculate DPO

	data_points = use.get_x_data_points(date, time, stock, X)
	average = use.get_average(data_points)
	index = X / 2 + 1

	price = tail(use.get_x_data_points(date, time, stock, index),1)

	result = price - average

	return(result)
}

adv.get_mass_index <- function(date, time, stock){
	# calculate mass index

	# THIS ONE IS HARD

	# Single EMA = 9-period exponential moving average (EMA) of the high-low differential
	# Double EMA = 9-period EMA of the 9-period EMA of the high-low differential
	# EMA Ratio = Single EMA divided by Double EMA 
	# Mass Index = 25-period sum of the EMA Ratio 

	dummy_list = c()
	single_ema_list = c()

	for (i in c(1:9)) {

		for (j in c(1:9)) {
			data_points = use.get_x_day_data_points(date, j)
			max = use.get_max(data_points)
			min = use.get_min(data_points)
			difference = max - min

			dummy_list = c(dummy_list, difference)
		}

		single_ema = use.get_average(dummy_list)
		single_ema_list = c(single_ema_list, single_ema)
	}

	double_ema = use.get_average(single_ema_list)



	result = 0

	return(result)
}

adv.get_macd <- function(date, time, stock){
	# calculate Moving Average Convergence/Divergence

	signal_line_list = c()

	for (i in c(1:9)) {
		new_date = use.get_x_date(date, (i-1))

		data_points_12 = use.get_x_data_points(new_date, time, stock, 12)
		data_points_26 = use.get_x_data_points(new_date, time, stock, 26)
		aver_12 = use.get_average(data_points_12)
		aver_26 = use.get_average(data_points_26)
		macd_line = aver_12 - aver_26

		signal_line_list = c(signal_line_list, macd_line)
	}

	signal_line = use.get_average(signal_line_list)

	result = c(signal_line_list[1], signal_line)

	return(result)
}

adv.get_macd_histogram <- function(date, time, stock){
	# calculate MACD-Histogram

	macd = adv.get_macd(date, time, stock)

	result = macd[1] - macd[2]

	return(result)
}

adv.get_ppo <- function(date, time, stock){
	# calculate Percentage Price Oscillator

	signal_line_list = c()

	for (i in 1:9) {
		new_date = use.get_x_date(date, (i-1))

		data_points_12 = use.get_x_data_points(new_date, time, stock, 12)
		data_points_26 = use.get_x_data_points(new_date, time, stock, 26)
		aver_12 = use.get_average(data_points_12)
		aver_26 = use.get_average(data_points_26)
		PPO = ((aver_12 - aver_26) / aver_26) * 100

		signal_line_list = c(signal_line_list, PPO)
	}

	signal_line = use.get_average(signal_line_list)

	histogram = signal_line_list[1] - signal_line

	result = c(signal_line_list[1], signal_line, histogram)

	return(result)
}

adv.get_kst <- function(date, time, stock){
	# calculate Know Sure Thing

	signal_line_list = c()

	for (j in c(1:9)) {
		start_date = use.get_x_date(date, 9)

		roc_10_list = c()
		roc_15_list = c()
		roc_20_list = c()
		roc_30_list = c()

		for (i in c(1:15)) {
			new_date = use.get_x_date(start_date, (i-1))

			roc_10 = adv.get_rate_of_change(new_date, time, stock, 10)
			roc_15 = adv.get_rate_of_change(new_date, time, stock, 15)
			roc_20 = adv.get_rate_of_change(new_date, time, stock, 20)
			roc_30 = adv.get_rate_of_change(new_date, time, stock, 30)

			roc_10_list = c(roc_10_list, roc_10)
			roc_15_list = c(roc_15_list, roc_15)
			roc_20_list = c(roc_20_list, roc_20)
			roc_30_list = c(roc_30_list, roc_30)
		}

		RCMA1 = use.get_average(head(roc_10_list, 10))
		RCMA2 = use.get_average(head(roc_15_list, 10))
		RCMA3 = use.get_average(head(roc_20_list, 10))
		RCMA4 = use.get_average(head(roc_30_list, 15))

		KST = (RCMA1 * 1) + (RCMA2 * 2) + (RCMA3 * 3) + (RCMA4 * 4)
		signal_line_list = c(signal_line_list, KST)
	}

	result = use.get_average(signal_line_list)

	return(result)
}

adv.get_special_k <- function(date, time, stock){
	# calculate Pring Special K
	# this requires a lot of data and isn't always useful
	# HAS NOT BEEN TESTED, ALWAYS RUNS OUT OF DATA

    roc_10_list = c()
    roc_15_list = c()
    roc_20_list = c()
    roc_30_list = c()
    roc_40_list = c()
    roc_65_list = c()
    roc_75_list = c()
    roc_100_list = c()
    roc_195_list = c()
    roc_265_list = c()
    roc_390_list = c()
    roc_530_list = c()

    for (i in 1:195) {
    	new_date = use.get_x_date(date, (i-1))

    	roc_10_list = c(roc_10_list, adv.get_rate_of_change(new_date, time, stock, 10))
    	roc_15_list = c(roc_15_list, adv.get_rate_of_change(new_date, time, stock, 15))
    	roc_20_list = c(roc_20_list, adv.get_rate_of_change(new_date, time, stock, 20))
    	roc_30_list = c(roc_30_list, adv.get_rate_of_change(new_date, time, stock, 30))
    	roc_40_list = c(roc_40_list, adv.get_rate_of_change(new_date, time, stock, 40))
    	roc_65_list = c(roc_65_list, adv.get_rate_of_change(new_date, time, stock, 65))
    	roc_75_list = c(roc_75_list, adv.get_rate_of_change(new_date, time, stock, 75))
    	roc_100_list = c(roc_100_list, adv.get_rate_of_change(new_date, time, stock, 100))
    	roc_195_list = c(roc_195_list, adv.get_rate_of_change(new_date, time, stock, 195))
    	roc_265_list = c(roc_265_list, adv.get_rate_of_change(new_date, time, stock, 265))
    	roc_390_list = c(roc_390_list, adv.get_rate_of_change(new_date, time, stock, 390))
    	roc_530_list = c(roc_530_list, adv.get_rate_of_change(new_date, time, stock, 530))
    }

	result = (use.get_average(head(roc_10_list,10)) * 1)
			+(use.get_average(head(roc_15_list,10)) * 2)
			+(use.get_average(head(roc_20_list, 10)) * 3)
			+(use.get_average(head(roc_30_list, 15)) * 4)
			+(use.get_average(head(roc_40_list, 50)) * 1)
			+(use.get_average(head(roc_65_list, 65)) * 2)
			+(use.get_average(head(roc_75_list, 75)) * 3)
			+(use.get_average(head(roc_100_list, 100)) * 4)
			+(use.get_average(head(roc_195_list, 130)) * 1)
			+(use.get_average(head(roc_265_list, 130)) * 2)
			+(use.get_average(head(roc_390_list, 130)) * 3)
			+(use.get_average(head(roc_530_list, 195)) * 4)

	return(result)
}

adv.get_rate_of_change <- function(date, time, stock, X){
	# ge the rate of change of a stock from X periods ago

	result = ((use.get_x_close(date, stock, 0) - use.get_x_close(date, stock, X)) / use.get_x_close(date, stock, X)) * 100

	return(result)
}

adv.get_rsi <- function(date, time, stock){
	# calculate relative strength index

    gain_loss = use.get_total_gain_loss(date, time, stock, 14)
    average_gain = gain_loss[1] / 14
    average_loss = gain_loss[2] / 14
    RS = average_gain / average_loss
    RSI = 100 - (100 / 1 + RS)

	result = RSI

	return(result)
}

adv.get_sctr <- function(date, time, stock){
	# calculate StockCharts Technical Rank

	price = use.get_x_data_points(date, time, stock, 1)
	ema_200 = use.get_average(use.get_x_data_points(date, time, stock, 200))
	long_1 = ((price / ema_200) * 100) - 100

	long_2 = adv.get_rate_of_change(date, time, stock, 125)

	ema_50 = use.get_average(use.get_x_data_points(date, time, stock, 50))
	medium_1 = ((price / ema_50) * 100) - 100

	medium_2 = adv.get_rate_of_change(date, time, stock, 20)

	short_2 = adv.get_rsi(date, time, stock)

	result = c(long_1, long_2, medium_1, medium_2, short_2)

	return(result)
}

adv.get_slope <- function(date, time, stock, X){
	# calculate the line of best fit over X datapoints

	x_axis = c(1:X)
	y_axis = use.get_x_data_points(date, time, stock, X)

	fit = lm(y_axis~x_axis)

	result = summary(fit)

	return(result)
}

adv.get_so <- function(date, time, stock){
	# calculate Stochastic Oscillator
	# NOT CONVINCED

	d_list = c()

	for (i in c(1:3)) {
		new_date = use.get_x_date(date, (i-1))

		close = use.get_x_close(new_date, stock, 1)
		data_points = use.get_x_data_points(new_date, time, stock, 14)
		max = use.get_max(data_points)
		min = use.get_min(data_points)

		k_ = (close - min) / (max - min)
		d_list = c(d_list, k_)
	}

	print(d_list)

	result = use.get_average(d_list)

	return(result)
}

adv.get_stoch_rsi <- function(date, time, stock){
	# calculate StochRSI

	stoch_rsi_list = c()

	for (i in c(1:14)) {
		new_date = use.get_x_date(date, (i-1))

		rsi = adv.get_rsi(new_date, time, stock)

		stoch_rsi_list = c(stoch_rsi_list, rsi)
	}

	low = use.get_min(stoch_rsi_list)
	high = use.get_max(stoch_rsi_list)
	rsi = stoch_rsi_list[1]

	result = (rsi - low) / (high - low)

	return(result)
}

adv.get_trix <- function(date, time, stock){
	# calculate trix

	triple_smoothed_list = c()

	for (i in c(1:15)) {
		new_date = use.get_x_date(date, (i-1))

		double_smoothed_list = c()

		for (j in c(1:15)) {
			newer_date = use.get_x_date(new_date, (j-1))

			single_smoothed_list = c()

			for (k in c(1:15)) {
				newest_date = use.get_x_date(newer_date, (k-1))
				close = use.get_x_close(newest_date, stock, 1)

				single_smoothed_list = c(single_smoothed_list, close)
			}

			double_smoothed_list = c(double_smoothed_list, use.get_average(single_smoothed_list))
		}

		triple_smoothed_list = c(triple_smoothed_list, use.get_average(double_smoothed_list))
	}

	first = triple_smoothed_list[1]
	second = triple_smoothed_list[2]

	result = ((second / first) * 100) - 100

	return(result)
}

adv.get_tsi <- function(date, time, stock){
	# calculate true strength index

	double_smoothed_list = c()
	double_smoothed_list_abs = c()

	for (i in c(1:13)) {
		new_date = use.get_x_date(date, (i-1))

		single_smoothed_list = c()
		single_smoothed_list_abs = c()

		for (j in c(1:25)) {
			newer_date = use.get_x_date(new_date, (j-1))

			data_points = use.get_x_data_points(newer_date, time, stock, 2)			
			PC = data_points[1] - data_points[2]
			PC_abs = abs(data_points[1] - data_points[2])

			single_smoothed_list = c(single_smoothed_list, PC)
			single_smoothed_list_abs = c(single_smoothed_list_abs, PC_abs)
		}

		double_smoothed_list = c(double_smoothed_list, use.get_average(single_smoothed_list))
		double_smoothed_list_abs = c(double_smoothed_list_abs, use.get_average(single_smoothed_list_abs))
	}

	double_smoothed = use.get_average(double_smoothed_list)
	double_smoothed_abs = use.get_average(double_smoothed_list_abs)

	result = 100 * (double_smoothed / double_smoothed_abs)

	return(result)
}

adv.get_ulcer_index <- function(date, time, stock){
	# calculate ulcer index

	average_squared_list = c()

	for (i in c(1:14)) {
		new_date = use.get_x_date(date, (i-1))

		close_list = c()

		for (j in c(1:14)) {
			newer_date = use.get_x_date(new_date, (j-1))

			close = use.get_x_close(newer_date, stock, 1)

			close_list = c(close_list, close)
		}

		max = use.get_max(close_list)
		close = close_list[1]
		PD = ((close - max) / max) * 100

		average_squared_list = c(average_squared_list, (PD * PD))
	}

	SA = use.get_average(average_squared_list)

	result = sqrt(SA)

	return(result)
}

adv.get_UO <- function(date, time, stock){
	# calculate ultimate oscilator

	BP_list = c()
	TR_list = c()

	for (i in c(28)) {
		new_date = use.get_x_date(date, (i-1))

		close = use.get_x_close(new_date, stock, 0)
		min = use.get_min(use.get_x_day_data_points(new_date, stock, 0))
		max = use.get_max(use.get_x_day_data_points(new_date, stock, 0))

		BP = close - use.get_min(c(close, min))
		TR = use.get_max(c(max, close)) - use.get_min(c(min, close))

		BP_list = c(BP_list, BP)
		TR_list = c(TR_list, TR)
	}

	average7 = sum(head(BP_list, 7)) / sum(head(TR_list, 7))
	average14 = sum(head(BP_list, 14)) / sum(head(TR_list, 14))
	average28 = sum(head(BP_list, 28)) / sum(head(TR_list, 28))

	result = 100 * ((4 * average7) + (2 * average14) + (average28)) / (4 + 2 + 1)

	return(result)
}

adv.get_vortex <- function(date, time, stock){
	# calculate vortex indicator

	vm_plus_list = c()
	vm_minus_list = c()
	tr_list = c()

	for (i in c(1:14)) {
		new_date = use.get_x_date(date, (i-1))

		current_high = use.get_max(use.get_x_day_data_points(new_date, stock, 0))
		current_low = use.get_min(use.get_x_day_data_points(new_date, stock, 0))
		prior_high = use.get_max(use.get_x_day_data_points(new_date, stock, 1))
		prior_low = use.get_min(use.get_x_day_data_points(new_date, stock, 1))

		vm_plus = current_high - prior_low
		vm_minus = current_low - prior_high
		tr = adv.get_average_true_range(new_date, time, stock, 1)

		vm_plus_list = c(vm_plus_list, vm_plus)
		vm_minus_list = c(vm_minus_list, vm_minus)
		tr_list = c(tr_list, tr)
	}

	vi_plus = sum(vm_plus_list) / sum(tr_list)
	vi_minus = sum(vm_minus_list) / sum (tr_list)

	result = c(vi_plus, vi_minus)

	return(result)
}

adv.get__R <- function(date, time, stock){
	# calculate williams %R
	# NOT SURE ABOUT THIS

	data_points = use.get_x_data_points(date, time, stock, 14)
	close = use.get_x_close(date, stock, 1)

	result = ((use.get_max(data_points) - close) / (use.get_max(data_points) - use.get_min(data_points))) * -100

	return(result)
}

####################################################################
# Machine Learning - All ML related stuff
####################################################################

# THIS HAS BEEN COMMENTED: e1071 will not be present on any testing system.

# Create the data frame for storing the peaks
# SVMBuyData = data.frame(
# 	Date = double(),
# 	Time = double(),
# 	Stock_Value = double(),
# 	Should_Buy = logical(),
#     stringsAsFactors=FALSE
# )

# SVMSellData = data.frame(
# 	Date = double(),
# 	Time = double(),
# 	Stock_Value = double(),
# 	Should_Sell = logical(),
#     stringsAsFactors=FALSE
# )

# ml.get_peaks <- function(time, date, stock){
# 	# a function that will, given time date and stock use all available data within this stock to make 
# 	# a decision about if a given point is a peak or not

# 	data_to_use = Data[Data$DATE < date & Data$TIME < time,]

# 	for (i in data_to_use) {
# 		# iterate through all available data

# 		decision = get_differential_peak(i)

# 		SVMBuyData[nrow(SVMBuyData), ] = c(date, time, data_to_use[data_to_use$DATE = date & data_to_use$TIME = time, stock], decision)
# 	}

# 	return(SVMBuyData)
# }

# ml.get_troughs <-function(time, date, stock){
# 	# a function that will given time date and stock use all available data within this stock to make
# 	# a decison about if a given point is a peak or not

# 	data_to_use = Data[Data$DATE < date & Data$TIME < time, stock]

# 	for (i in data_to_use) {
# 		# iterate through all available data

# 		decision = get_differential_trough(i)

# 		SVMBuyData[nrow(SVMBuyData), ] = c(date, time, data_to_use[data_to_use$DATE = date & data_to_use$TIME = time, stock], decision)
# 	}

# 	return(SVMBuyData)
# }

# ml.get_rolling_average_peak <- function(X){

# 	result = FALSE

# 	mean = use.get_average(data_to_use)

# 	for (i in data_to_use) {
# 		if(data_to_use[i] > 1.1 * mean){
# 			result = TRUE
# 		}
# 	}

# 	return(result)
# }

# ml.get_rolling_average_trough <- function(X){

# 	result = FALSE

# 	mean = use.get_average(data_to_use)

# 	for (i in data_to_use) {
# 		if(data_to_use[i] < 0.9 * mean){
# 			result = TRUE
# 		}
# 	}

# 	return(result)
# }

# ml.get_spikes_peak <- function(X){

# 	result = FALSE

# 	for (i in data_to_use) {
# 		if(data_to_use[i] > data_to_use[i-1] & data_to_use[i] > data_to_use[i+1]){
# 			result = TRUE
# 		}
# 	}

# 	return(result)
# }

# ml.get_spikes_trough <- function(X){

# 	result = FALSE

# 	for (i in data_to_use) {
# 		if(data_to_use[i] < data_to_use[i-1] & data_to_use[i] < data_to_use[i+1]){
# 			result = TRUE
# 		}
# 	}

# 	return(result)
# }

# ml.get_differential_peak <- function(X){

# 	result = FALSE

# 	for (i in data_to_use) {
# 		running_average = average(data_to_use[:i])
# 		diff_1 = lm(running_average)
# 		diff_2 = lm(diff_1)

# 		if(diff_1 < 0.05 & diff_2 > 0){
# 			result = TRUE
# 		}
# 	}

# 	return(result)
# }

# ml.get_differential_trough <- function(X){

# 	result = FALSE

# 	for (i in data_to_use) {
# 		running_average = average(data_to_use[:i])
# 		diff_1 = lm(running_average)
# 		diff_2 = lm(diff_1)

# 		if(diff_1 < 0.05 & diff_2 < 0){
# 			result = TRUE
# 		}
# 	}

# 	return(result)
# }

# ml.get_median_peak <- function(X){

# 	result = FALSE

# 	range = range(data_to_use)

# 	max = use.get_max(data_to_use)

# 	for (i in data_to_use) {
# 		if(data_to_use[i] > max - (range * 0.1)){
# 			result = TRUE
# 		}
# 	}

# 	return(result)
# }

# ml.get_median_trough <- function(X){

# 	result = FALSE

# 	range = range(data_to_use)

# 	min = use.get_min(data_to_use)

# 	for (i in data_to_use) {
# 		if(data_to_use[i] < max + (range * 0.1)){
# 			result = TRUE
# 		}
# 	}

# 	return(result)
# }

####################################################################