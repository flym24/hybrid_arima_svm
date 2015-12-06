source("load_data.R")
source("model_selection.r")
source("arima_prediction.r")
source("utilities.r")

## Configuration area
p.max = 4
q.max = 4
start = 501
company.rets = MSFT.rets

## Code
model_selection_result = arma.model.selection(company.rets, p.max, q.max)

back_test_result = back_test.arima(	company.rets, 
									p=model_selection_result$best.p, 
									q=model_selection_result$best.q,
									start=start)
									
plot(back_test_result$real.values, type="l", col='red')
lines(back_test_result$predictions)

hr = hit_rate(back_test_result$predictions, back_test_result$real.values)
nmse = nMSE(back_test_result$predictions, back_test_result$real.values)

print(sprintf("Hit rate = %f, nMSE = %f", hr, nmse))