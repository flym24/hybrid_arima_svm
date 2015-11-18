require(quantmod)
require(xts)
require(zoo)
require(TTR)
require(e1071)

source("load_data.R")
source("arima_model_selection.r")
source("arima_prediction.r")
source("svm_model_selection.r")
source("svm_prediction.r")
source("e1071.R")

## Configuration area
p.max = 4
d.max = 0
q.max = 4
start = 500
day = 30
company.rets = table.rets
data = svmFeatures(table)
rets = table.rets[index(data)]

## Code
back_test_result_arima = back_test.arima(	rets, 
									p.max=p.max,
									d.max=d.max,
									q.max=q.max,
									day = day,
									start=start)

back_test_result_svm = back_test.svm(	data, rets, 
									day = day,
									start=start)									



