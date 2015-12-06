arma.model.selection <- function (rets, p.max = 4, d.max = 2, q.max = 4)
{
	best.aic <- 1e9
	best.model <- NA
	best.p <- NA
	best.d <- NA
	best.q <- NA

	aics <- array(NA, dim = c(p.max+1, d.max+1, q.max+1) )
	for (p in 0:p.max) {	
		for (d in 0:d.max) {
			for (q in 0:q.max) {
				print(sprintf("Model selection... p = %d, d = %d, q = %d", p, d, q))
				flush.console()
				
				r <- list(aic=1e9)

				# tryCatch ({
					# r <- arima( rets, order=c(p,0,q))
				# }, warning = function(warning) {
					# print(warning)
				# })
				try( r <- arima(rets, order=c(p,0,q)) )

				aics[p+1, d+1, q+1] <- r$aic

				if (r$aic < best.aic)
				{
					best.aic <- r$aic
					best.model <- r
					best.p <- p
					best.d <- d
					best.q <- q
					print(sprintf("aic=%f, p=%d, d=%d, q=%d", r$aic, p, d, q))
				}
			}
		}
	}
	
	list(aics = aics, best.aic = best.aic, 
		best.model = best.model, 
		best.p = best.p, best.d = best.d, best.q = best.q)
}