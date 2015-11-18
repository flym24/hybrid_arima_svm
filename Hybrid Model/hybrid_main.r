require(quantmod)
require(e1071)
require(xts)
require(zoo)
require(TTR)

##Configuration area 1

start = 500
day = 30

source("load_data.R")
source("hybrid_svm_model_selection.r")
source("hybrid_svm_prediction.r")
source("hybrid_utilities.r")
source("ensemble_main.r")

#Configuration area 2
#ARIMA parameters
p.max = 4
d.max = 0
q.max = 4

data = svmFeatures(table)
rets = table.rets[paste(index(data)[1], "/")]

#Code
back_test_result_hybrid = back_test.hybrid( data, rets,
							day = day,
							start = start)
							
back_test_result_hybrid_predictions = back_test_result_hybrid$predictions
back_test_result_hybrid_real.values = back_test_result_hybrid$real.values
print(back_test_result_hybrid_predictions)
print(sprintf("hybrid predictions = %f", back_test_result_hybrid_predictions))

hr = hit_rate3(back_test_result_hybrid_predictions, back_test_result_arima$predictions, back_test_result_svm$predictions, back_test_result_hybrid_real.values)
nmse = nMSE3(back_test_result_hybrid_predictions, back_test_result_arima$predictions, back_test_result_svm$predictions, back_test_result_hybrid_real.values)

print(sprintf("Hit rate = %f, nMSE = %f", hr, nmse))


