require(e1071)
require(quantmod)
require(parallel)

print("Hello")

source("e1071.R")

tt = get( getSymbols( "^GSPC", from="1900-01-01" ) )

rets = na.trim( ROC( Cl( tt ), type="discrete" ) )

# only the first two features so that we may see some results in reasonable time
data = svmFeatures( tt )

rets = rets[index(data)]
data = data[index(rets)]

stopifnot( NROW( rets ) == NROW( data ) )

startDate="1959-11-1"
endDate="1959-12-31"

fore = svmComputeForecasts(
               data=data,
               history=500,
               response=rets,
               cores=1,
               trace=T,
               modelPeriod="days",
               startDate=startDate,
               endDate=endDate,
               featureSelection="all" )


real = rets[paste(startDate, "/", endDate, sep="")]

real.y = Cl(real)
pred.y = fore$Forecasts

fig1 = dev.new()
plot(real.y)
lines(pred.y, col="red")

fig2 = dev.new()
pred.compare = (real.y > 0) - (pred.y > 0)
barplot( pred.compare )

pred.correct = sum( pred.compare == 0 )
test.examples = length(real.y)

print(sprintf("Prediction accuracy = %f (%d / %d)\n", pred.correct / test.examples,
	pred.correct, test.examples)) 