require(e1071)
require(zoo)
require(quantmod)
require(parallel)
require(TTR)
require(xts)

source("svm_load_data.r")
source("svm_training_model_prediction.r")
source("svm_model selection_feature selection.r")

#Configuration area
tt = table
historylen = 1200
corenum = 1
modelprd = "days"
featureslct = "all"

rets = na.trim( ROC( Cl( tt ),type = "discrete") )

data = svmFeatures( tt )

rets = rets[index(data)]
data = data[index(rets)]

stopifnot( NROW( rets ) == NROW( data ) )

startDate = "2010-3-17"
endDate = "2015-3-17"

fore = svmComputeForecasts(
				data = data,
				history = historylen,
				response = rets,
				cores = corenum,
				trace = T,
				modelPeriod = modelprd,
				startDate = startDate,
				endDate = endDate,
				featureSelection = featureslct 
				)

real = tt[paste(startDate, "/", endDate, sep = "")]

real.y = na.trim(ROC(Cl(real), n = 1))
pred.y = fore$Forecasts

print(sprintf("Hit Rate = %f (%d / %d),
				nMSE = %f ",
				hit.rate, pred.correct, test.examples, nmse)) 











				
				
				
				
				
				
				
				
				