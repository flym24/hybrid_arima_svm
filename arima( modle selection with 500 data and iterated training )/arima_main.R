require(quantmod)

source("load_data.R")
source("model_selection.r")
source("arima_prediction.r")
source("utilities.r")

## Configuration area
p.max = 4
d.max = 0
q.max = 4
start = 501
day = 30
company.rets = MSFT.rets

## Code
back_test_result = back_test.arima(	company.rets, 
									p.max=p.max,
									d.max=d.max,
									q.max=q.max,
									day = day,
									start=start)
									
plot(back_test_result$real.values, type="l", col='red')
lines(back_test_result$predictions)

hr = hit_rate(back_test_result$predictions, back_test_result$real.values)
nmse = nMSE(back_test_result$predictions, back_test_result$real.values)

print(sprintf("Hit rate = %f, nMSE = %f", hr, nmse))