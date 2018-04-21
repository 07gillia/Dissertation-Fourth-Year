import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import sys

dataframe =  pd.read_csv('../Data/ALLSTOCKS.csv')

pd.to_datetime(dataframe['DATE'])
pd.to_datetime(dataframe['TIME'])

dataframe['DATETIME'] = pd.to_datetime(dataframe['DATE'] + ' ' + dataframe['TIME'])

dataframe = dataframe.sort(['DATETIME'])

# dataframe = dataframe.tail(390)

a = float('nan')

MACD_line = []
Signal_line = []

for x in range(1,31):
	MACD_line.append(a)

for x in range(1,31):
	Signal_line.append(a)

for x in range(30,len(dataframe['DATETIME'])):

	start_12 = x - 12
	start_26 = x - 26
	data_12 = dataframe.iloc[start_12:x,25]
	data_26 = dataframe.iloc[start_26:x,25]

	average_12 = float(np.mean(data_12))
	average_26 = float(np.mean(data_26))

	if (not np.isnan(average_12)) or (not np.isnan(average_26)):

		MACD_line.append((average_12 - average_26))

	else:

		MACD_line.append(a)

	if (len(MACD_line) > 10):

		data = MACD_line[-9:]
		
		signal = float(np.mean(data))

		Signal_line.append(signal)

# print(MACD_line)
# print(Signal_line)

# print(len(MACD_line))
# print(len(Signal_line))
# print(len(dataframe['DATETIME']))

dataframe['MACD'] = MACD_line
dataframe['Signal'] = Signal_line

dataframe = dataframe.tail(390)

# print(dataframe)
plt.subplot(2, 1, 1)
plt.plot(dataframe['DATETIME'], dataframe['HD'])
plt.ylabel('Stock Price (£)')
plt.subplot(2, 1, 2)
plt.plot(dataframe['DATETIME'], dataframe['MACD'])
plt.plot(dataframe['DATETIME'], dataframe['Signal'])

plt.legend(['MACD', 'Signal Line'], loc='upper right')

plt.suptitle('HD Stock Price')
plt.xlabel('Time')
plt.ylabel('MACD Value')
plt.savefig("figure.pdf", bbox_inches='tight')
plt.show()

#						4		5		6		7		8		9		10	11		12	13		14		15		16	17		18
# Stock_Names = c("AA", "AAPL", "ADBE", "AIG", "AMAT", "AMT", "AXP", "BA", "BAC", "C", "CA", "CAT", "CSCO", "CVX", "DD", "DIS", "EMC", "FSLR", "GE", "GLW", "GOOG", "GS", "HD", "HPQ", "IBM", "INTC", "IP", "JNJ", "JPM", "KO", "MCD", "MMM", "MRK", "MSFT", "PFE", "PG", "T", "TRV", "UTX", "VZ", "WFC", "WMT", "XOM", "YAHOO", "YNDX")