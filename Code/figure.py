import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import sys

dataframe =  pd.read_csv('../Data/ALLSTOCKS.csv')

pd.to_datetime(dataframe['DATE'])
pd.to_datetime(dataframe['TIME'])

dataframe['DATETIME'] = pd.to_datetime(dataframe['DATE'] + ' ' + dataframe['TIME'])

dataframe = dataframe.sort(['DATETIME'])

def Bolinger_Bands(index):

	start = index - 22

	data = dataframe.iloc[start:index,25:26]
	# the one is the stock index

	average = np.mean(data)

	sd = np.std(data)

	middle_band = float(average)
	upper_band = float(average + (sd * 2))
	lower_band = float(average - (sd * 2))

	return middle_band, upper_band, lower_band

result_middle = []
result_upper = []
result_lower = []

a = float('nan')

for x in range(1,31):
	result_middle.append(a)
	result_upper.append(a)
	result_lower.append(a)

for x in range(30,len(dataframe['DATETIME'])):

	stock_price = dataframe.iloc[x, 25]

	if np.isnan(stock_price):
		result_middle.append(a)
		result_upper.append(a)
		result_lower.append(a)
	else:

		result = Bolinger_Bands(x)

		result_middle.append(result[0])
		result_upper.append(result[1])
		result_lower.append(result[2])

dataframe['middle'] = result_middle
dataframe['upper'] = result_upper
dataframe['lower'] = result_lower

# dataframe = dataframe.tail(390)

# print(dataframe)

f = plt.figure()
plt.plot(dataframe['DATETIME'], dataframe['HD'])
plt.plot(dataframe['DATETIME'], dataframe['middle'])
plt.plot(dataframe['DATETIME'], dataframe['upper'])
plt.plot(dataframe['DATETIME'], dataframe['lower'])

plt.legend(['Stock Price', 'Middle Band', 'Upper Band', 'Lower Band'], loc='upper left')
f.suptitle('HD Stock Price')
plt.xlabel('Time')
plt.ylabel('Stock Price (£)')
plt.show()

f.savefig("figure.pdf", bbox_inches='tight')

#						4		5		6		7		8		9		10	11		12	13		14		15		16	17		18
# Stock_Names = c("AA", "AAPL", "ADBE", "AIG", "AMAT", "AMT", "AXP", "BA", "BAC", "C", "CA", "CAT", "CSCO", "CVX", "DD", "DIS", "EMC", "FSLR", "GE", "GLW", "GOOG", "GS", "HD", "HPQ", "IBM", "INTC", "IP", "JNJ", "JPM", "KO", "MCD", "MMM", "MRK", "MSFT", "PFE", "PG", "T", "TRV", "UTX", "VZ", "WFC", "WMT", "XOM", "YAHOO", "YNDX")