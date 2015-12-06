back_test.arima<-function(company.rets, day, p.max, d.max, q.max, start = 501, last = length(company.rets) ){
	predictions <- NULL
	real.values <- NULL
	
	wnd_length <- start - 1
	
	for(i in start:last) {		
		company.history.data<-company.rets[(i-wnd_length) : (i-1)]
		
		if ((i %% day == 0) | (i == start))
		{
			model_selection_result = arma.model.selection(company.history.data, 
						p.max, d.max, q.max)
			#model_selection_result = list(best.p = 2, best.d = 0, best.q = 2)
		}
		
		try({
			company.arima<-arima(company.history.data, 
								order=c(model_selection_result$best.p,
										model_selection_result$best.d,
										model_selection_result$best.q))
			company.pred<-predict(company.arima)$pred[1]
		})
		
		predictions <- c(predictions, company.pred)
		real.values <- c(real.values, company.rets[i])
		
		print(sprintf("Day %d: p = %d, d = %d, q = %d, prediction = %f, real value=%f", 
						i,
						model_selection_result$best.p, 
						model_selection_result$best.d, 						
						model_selection_result$best.q,
						company.pred, company.rets[i]))
		
		if (i %% 10 == 0)
		{
			plot(real.values, type="l", col='red')
			lines(predictions)

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
