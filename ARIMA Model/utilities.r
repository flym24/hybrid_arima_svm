# Calculate hit rate
hit_rate <- function( 	predictions,
						real.values){
	correct = sum(
		sum((predictions > 0)&(real.values > 0)),
		sum((predictions == 0)&(real.values == 0)),
		sum((predictions < 0)&(real.values < 0))
				)
		correct/(length(predictions))
	}


# Calculate mean square error
nMSE <- function( predictions, real.values){
			distance_pre_real = predictions - real.values
			distance_square = distance_pre_real*distance_pre_real
			mean(distance_square)/var(real.values)
		}
		