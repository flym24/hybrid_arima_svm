back_test.svm<-function(company.data, company.rets, start=1200, last=dim(data)[1], day = 10) {
	predictions <- NULL
	real.values <- NULL
	
	wnd_length <- start - 1
	
	for(i in start:last) {		
		company.history.data	<- company.data[(i-wnd_length) : (i-1)]
		company.history.rets	<- company.rets[(i-wnd_length) : (i-1)]
		company.today.data		<- company.data[i]
		company.today.ret		<- company.rets[i]
		
		train.x	<- as.matrix( coredata( company.history.data ) )
		train.y	<- as.matrix( coredata( company.history.rets ) )
		
		test.x	<- as.matrix( coredata( company.today.data ) )
		test.y	<- as.matrix( coredata( company.today.ret ) )
		
		if ((i %% day == 0) | (i == start))
		{
			model_selection_result = svm.model.selection(
				train.x, train.y)
		}
		
		try({
			company.svm<-svm(train.x, train.y,
				gamma	= model_selection_result$best.parameters$gamma,
				cost	= model_selection_result$best.parameters$cost,
				kernel	= "radial")
				
			company.pred <- predict(company.svm, test.x)
		})
		
		predictions <- c(predictions, company.pred[1])
		real.values <- c(real.values, test.y[1])
		
		print(sprintf("Day %d: gamma = %f, cost = %f, prediction = %f, real value=%f", 
						i,
						model_selection_result$best.parameters$gamma, 
						model_selection_result$best.parameters$cost,
						company.pred[1], test.y[1]))
		
		# if (i %% 10 == 0)
		# {
			# plot(real.values, type="l", col='red')
			# lines(predictions)

			# hr = hit_rate(predictions, real.values)
			# nmse = nMSE(predictions, real.values)

			# print(sprintf("Hit rate = %f, nMSE = %f", hr, nmse))
		# }
			
		flush.console()
	}
	
	list(predictions=predictions, real.values=real.values)
}
