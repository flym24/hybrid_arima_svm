back_test.arima<-function(company.rets, p = 2, q = 2, start = 501, last = length(company.rets) ){
	predictions <- NULL
	real.values <- NULL
	
	wnd_length <- start - 1
	
	for(i in start:last) {
		company.history.data<-company.rets[(i-wnd_length) : (i-1)]
		try({
			company.arima<-arima(company.history.data, order=c(p,0,q))
			company.pred<-predict(company.arima)$pred[1]
		})
		
		predictions <- c(predictions, company.pred)
		real.values <- c(real.values, company.rets[i])
		
		print(sprintf("Day %d: prediction = %f, real value=%f", i, company.pred, company.rets[i]))
		
		if (i %% 10 == 0)
		{
			plot(back_test_result$real.values, type="l", col='red')
			lines(back_test_result$predictions)

			hr = hit_rate(predictions, real.values)
			nmse = nMSE(predictions, real.values)

			print(sprintf("Hit rate = %f, nMSE = %f", hr, nmse))
		}
			
		flush.console()
	}
	
	list(predictions=predictions, real.values=real.values)
}

#source("load_data.R")
#back_test.arima(MSFT.rets)
#back_test.arima(IBM.rets)
#back_test.arima(Citi.rets)
