svm.model.selection <- function(	x, y, 
									gamma=10^(-3:0), 
									cost=10^(-3:0), 
									sampling="cross", cross=5, 
									kernel="radial")
{
	newSvm = tune( svm,
				   train.x=x,
				   train.y=y,
				   ranges=list( gamma=gamma, cost=cost ),
				   tunecontrol=tune.control( sampling=sampling, cross=cross ),
				   kernel=kernel )
				   
	newSvm
}
