hit_rate <- function( 	predictions,
						real.values){
	correct = sum(
		sum((predictions > 0)&(real.values > 0)),
		sum((predictions == 0)&(real.values == 0)),
		sum((predictions < 0)&(real.values < 0))
				)
				
	# for( i in 1:length(predictions)){
		# if((predictions[i] > 0) - 
			# (real.values[i] > 0) != 0)
			# correct = corrects

		# if(predictions[i] > 0)
			# back_test_predictions.direction = 1
		# if(predictions[i] = 0)
			# back_test_predictions.direction = 0
		# else
			# back_test_predictions.direction = -1
		# if(real.values[i] > 0)
			# back_test_real.values.direction = 1
		# if(real.values[i] = 0)
			# back_test_real.values.direction = 0
		# else
			# back_test_real.values.direction = -1
	
		# if( back_test_predictions.direction == back_test_real.values.direction)
			# correct = correct + 1
		# else
			# correct = correct
		# }
		
		correct/(length(predictions))
	}


nMSE <- function( predictions, real.values){
			distance_pre_real = predictions - real.values
			distance_square = distance_pre_real*distance_pre_real
			mean(distance_square)/var(real.values)
		}
		