back_test.hybrid<-function(company.data, company.rets, start=501, last=dim(data)[1], day = 10) {
	predictions <- c()
	real.values <- c()
	arima.preds <- c()
	
	wnd_length <- start - 1
	
	for(i in start:last) {		
		company.history.data	<- company.data[(i-wnd_length) : (i-1)]
		company.history.rets	<- company.rets[(i-wnd_length) : (i-1)]
		company.today.data		<- company.data[i]
		company.today.ret		<- company.rets[i]
		
		# Do model selection
		if ((i %% day == 0)	| (i == start))
		{
			#model_selection_result = hybrid.model.selection(company.history.data , company.history.rets)
			model_selection_result = list(best.p = 2, best.d = 0, best.q = 2, best.parameters = list(cost = 1, gamma = 0.07))
		}
		
		try({
			# Train an arima model
			company.arima<-arima(company.history.rets, 
								order=c(model_selection_result$best.p,
										model_selection_result$best.d,
										model_selection_result$best.q))
			
			# Get it residual
			company.residual<-company.arima$residual
			
			# Train a SVM model
			company.svm<-svm(company.history.data, company.residual,
				gamma	= model_selection_result$best.parameters$gamma,
				cost	= model_selection_result$best.parameters$cost,
				kernel	= "radial")
									
			# Make prediction
			arima_prediction <- predict(company.arima)$pred[1]
			svm_prediction <- predict(company.svm, company.today.data)
			
			company.pred <- arima_prediction + svm_prediction
		})
		
		predictions <- c(predictions, as.numeric(company.pred))
		real.values <- c(real.values, as.numeric(company.today.ret[1]))
		arima.preds <- c(arima.preds, as.numeric(arima_prediction))
		
		 print(sprintf("Day %d: p = %d, d = %d, q = %d, gamma = %f, cost = %f, prediction = %f, real value=%f", 
						 i,
						 model_selection_result$best.p,
						 model_selection_result$best.d,
						 model_selection_result$best.q,
						 model_selection_result$best.parameters$gamma, 
						 model_selection_result$best.parameters$cost,
						 company.pred, company.today.ret[1]))
		
		# if (i %% 10 == 0)
		# {				
			# r = as.xts(zoo(cbind(return=real.values), index(company.data)[start:i]))
			# p = as.xts(zoo(cbind(return=predictions), index(company.data)[start:i]))
			# a = as.xts(zoo(cbind(return=arima.preds), index(company.data)[start:i]))
			# plot(r, type="l")
			# lines(p, col="red")
			# lines(a, col="blue")

			# hr = hit_rate(predictions, real.values)
			# nmse = nMSE(predictions, real.values)

			# print(sprintf("Hit rate = %f, nMSE = %f", hr, nmse))
		# }
			
		flush.console()
	}
	
	list(predictions=predictions, real.values=real.values)
}
