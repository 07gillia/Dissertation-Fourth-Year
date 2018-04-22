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

peaks = []
troughs = []

for x in range(1,55001):
	peaks.append(a)
	troughs.append(a)

for x in range(55000,(len(dataframe['DATETIME'])-1)):

	stock_price = float(dataframe.iloc[x, 17])
	previous_stock_price = float(dataframe.iloc[(x-1),17])
	next_stock_price = float(dataframe.iloc[(x+1),17])

	start = x - 2490
	data = dataframe.iloc[start:x,17]

	average_price = float(np.mean(data))

	# print(average_price)

	# print(stock_price)
	# print(previous_stock_price)
	# print(next_stock_price)

	if (stock_price > previous_stock_price) & (stock_price > next_stock_price) & (stock_price > average_price):
		peaks.append(stock_price)
		troughs.append(a)
	elif (stock_price < previous_stock_price) & (stock_price < next_stock_price) & (stock_price < average_price):
		peaks.append(a)
		troughs.append(stock_price)
	else:
		peaks.append(a)
		troughs.append(a)

	# print(peaks)
	# print(troughs)

peaks.append(a)
troughs.append(a)

print(len(peaks))
print(len(troughs))
print(len(dataframe['DATETIME']))

dataframe['peaks'] = peaks
dataframe['troughs'] = troughs

DATETIMES = dataframe['DATETIME'].tolist()
stockPrice = dataframe['DD'].tolist()

# print(dataframe)

plt.plot(DATETIMES, stockPrice, color='g')
plt.scatter(DATETIMES, peaks, s=2, color='b')
plt.scatter(DATETIMES, troughs, s=2, color='r')

plt.legend(['Sell Points', 'Sell Points', 'Buy Points'], loc='upper left')

plt.suptitle('DD SVM Results')
plt.xlabel('Time')
plt.ylabel('Stock Price (£)')
plt.savefig("figure.pdf", bbox_inches='tight')
plt.show()

#						4		5		6		7		8		9		10	11		12	13		14		15		16	17		18
# Stock_Names = c("AA", "AAPL", "ADBE", "AIG", "AMAT", "AMT", "AXP", "BA", "BAC", "C", "CA", "CAT", "CSCO", "CVX", "DD", "DIS", "EMC", "FSLR", "GE", "GLW", "GOOG", "GS", "HD", "HPQ", "IBM", "INTC", "IP", "JNJ", "JPM", "KO", "MCD", "MMM", "MRK", "MSFT", "PFE", "PG", "T", "TRV", "UTX", "VZ", "WFC", "WMT", "XOM", "YAHOO", "YNDX")