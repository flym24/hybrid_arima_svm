function arma_back_test()

Input
All data
Index of start day
Index of last day

(window length = start - 1)

Output
Real value (start ~ last)
Predicted value (start ~ last)

1. Find optimal model parameter with training data [1, wnd.len] = [1, start - 1]
2. For day in start:last
	Train an ARMA model with training data [day - wnd.len, day - 1]
	Predict the return of [day]
	
	Real value = c(real value, data[day])
	Predicted value = c(predicted value, pred)
	
3. return (real value and predicted value)


Warnings:
1.Data omission in three sheets.
2.