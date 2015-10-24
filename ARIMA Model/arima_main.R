require(quantmod)
require(xts)
require(zoo)
require(TTR)

source("load_data.R")
source("model_selection.r")
source("arima_prediction.r")
source("utilities.r")

## Configuration area
p.max = 4
d.max = 0
q.max = 4
start = 1258
day = 50
company.rets = table.rets
ahead = 50

## Code
back_test_result = back_test.arima(	company.rets, 
									p.max=p.max,
									d.max=d.max,
									q.max=q.max,
									day = day,
									start=start)

hr = hit_rate(back_test_result$predictions, back_test_result$real.values)
nmse = nMSE(back_test_result$predictions, back_test_result$real.values)

print(sprintf("Hit rate = %f, nMSE = %f", hr, nmse))



