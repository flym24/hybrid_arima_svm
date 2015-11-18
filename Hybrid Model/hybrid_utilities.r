hit_rate3 <- function( 	predictions1, predictions2, predictions3, 
						real.values){
	agree = sum(
		sum((predictions1 > 0)&(predictions2 > 0)&(predictions3>0)),
		sum((predictions1 == 0)&(predictions2 == 0)&&(predictions3==0)),
		sum((predictions1 < 0)&(predictions2 < 0)&&(predictions3<0))
				)
	print(agree)
				
	agree.and.correct = sum(
		sum((predictions1 > 0)&(predictions2 > 0)&(predictions3>0)&(real.values>0)),
		sum((predictions1 == 0)&(predictions2 == 0)&(predictions3>0)&(real.values==0)),
		sum((predictions1 < 0)&(predictions2 < 0)&(predictions3>0)&(real.values<0))
				)
	print(agree.and.correct)
				
	agree.and.correct / agree	
	}


nMSE3 <- function( predictions1, predictions2, predictions3, real.values){
			ifagree = (((predictions1 > 0) == (predictions2 > 0)) && (predictions1 > 0) == (predictions3>0))
			predictions1 = predictions1[ifagree]
			predictions2 = predictions2[ifagree]
			predictions3 = predictions3[ifagree]
			real.values = real.values[ifagree]
			
			distance_pre_real = predictions1 - real.values
			distance_square = distance_pre_real*distance_pre_real
			mean(distance_square)/var(real.values)
		}
		