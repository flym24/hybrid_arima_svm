arma.model.selection <- function (rets, p.max = 4, q.max = 4)
{
	best.aic <- 1e9
	best.model <- NA
	best.p <- NA
	best.q <- NA

	aics <- array(NA, dim = c(p.max+1, q.max+1) )
	for (p in 0:p.max) {	
		for (q in 0:q.max) {
			print(sprintf("Model selection... p = %d, q = %d", p, q))
			flush.console()
			
			r <- list(aic=1e9)

			# tryCatch ({
				# r <- arima( rets, order=c(p,0,q))
			# }, warning = function(warning) {
				# print(warning)
			# })
			try( r <- arima(rets, order=c(p,0,q)) )

			aics[p+1, q+1] <- r$aic

			if (r$aic < best.aic)
			{
				best.aic <- r$aic
				best.model <- r
				best.p <- p
				best.q <- q
			}
		}
	}
	
	list(aics = aics, best.aic = best.aic, best.model = best.model, best.p = p, best.q = q)
}