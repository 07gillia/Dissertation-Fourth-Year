import pandas as pd
import matplotlib.pyplot as plt

dataframe =  pd.read_csv('../Data/ALLSTOCKS.csv')

pd.to_datetime(dataframe['DATE'])
pd.to_datetime(dataframe['TIME'])

dataframe['DATETIME'] = pd.to_datetime(dataframe['DATE'] + ' ' + dataframe['TIME'])

dataframe = dataframe.sort(['DATETIME'])

# get the last 22 datapoints
for x in range(30, len(dataframe['DATETIME'])):

	start = x - 22

	data = dataframe.iloc[22, start:x]
	print(data)

print(dataframe)

f = plt.figure()
plt.plot(dataframe['DATETIME'], dataframe['HD'])
f.suptitle('HD Stock Price')
plt.xlabel('Time')
plt.ylabel('Stock Price (£)')
plt.show()

f.savefig("figure.pdf", bbox_inches='tight')

# Stock_Names = c("AA", "AAPL", "ADBE", "AIG", "AMAT", "AMT", "AXP", "BA", "BAC", "C", "CA", "CAT", "CSCO", "CVX", "DD", "DIS", "EMC", "FSLR", "GE", "GLW", "GOOG", "GS", "HD", "HPQ", "IBM", "INTC", "IP", "JNJ", "JPM", "KO", "MCD", "MMM", "MRK", "MSFT", "PFE", "PG", "T", "TRV", "UTX", "VZ", "WFC", "WMT", "XOM", "YAHOO", "YNDX")