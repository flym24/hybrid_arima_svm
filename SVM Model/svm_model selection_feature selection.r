# svm prediction
svmComputeOneForecast = function(
      id,
      data,
      response,
      startPoints,
      endPoints,
      len,
      history=500,
      trace=FALSE,
      kernel="radial",
      gamma=10^(-5:-1),
      cost=10^(0:2),
      sampling="cross",
      seed=1234,
      featureSelection=c("add", "prune", "all"),
      cross=2)
{
   print(id)
   flush.console()

   # Determine the forecast length
   startIndex = startPoints[id]
   endIndex = endPoints[id]

   forecastLength = endIndex - startIndex + 1

   # A row in the data is responsible for the corresponding value in the
   # response. Thus, to forecast day X, we train the model on the previous
   # response. Thus, to forecast day X, we train the model on the previous
   # *history* days, and then use the features for day X to forecast.
   xtsData = data[index(data)[(startIndex-history):(startIndex-1)]]
   xtsResponse = response[index(response)[(startIndex-history):(startIndex-1)]]

   # Convert the input data and response to a matrix and a vector, respectively
   xx = as.matrix( coredata( xtsData ) )
   yy = as.vector( coredata( xtsResponse ) )

   # We need to set the seed to have reprodcible results
   set.seed( seed )

   if(featureSelection[1] == "add") {
      # We add the features one by one, until we cannot improve the error
      best = NULL
      bestPerf = 1e9

      # Maintained sorted, the content are the column indexes in the original matrix
      features = c()
      availableFeatures = seq(1,ncol(xx))

      # Use greedy approach to add features
      repeat {
         bestColIdToAdd = 0L
         # print( features )
         for(colId in 1:length(availableFeatures)) {
            # Get the matrix for the current tunning and tune
            zz = xx[,sort(c(features, availableFeatures[colId]))]
            # print(paste(sep="", "trying adding feature ", availableFeatures[colId]))
            newSvm = tune( svm,
                           train.x=zz,
                           train.y=yy,
                           ranges=list( gamma=gamma, cost=cost ),
                           tunecontrol=tune.control( sampling=sampling, cross=cross ),
                           kernel=kernel )
            # Check the performance improvement
            newPerf = round(newSvm$best.performance, 8)
            # print( paste( sep="", "new performance=", newPerf ) )
            if(newPerf < bestPerf) {
               # print( paste( sep="", "old performance=", bestPerf, ", new performance=", newPerf ) )
               best = newSvm
               bestPerf = newPerf
               bestColIdToAdd = colId
            }
         }

         if(bestColIdToAdd > 0) {
            # print( paste( sep="", "improvement, adding feature ", availableFeatures[bestColIdToAdd] ) )

            # Found an improvement, update the features
            features = sort(c(features, availableFeatures[bestColIdToAdd]))
            availableFeatures = availableFeatures[-bestColIdToAdd]

            # Exit if no features left
            if(length(availableFeatures) == 0) break
         } else {

            # No improvements, done
            break
         }
      }
   } else {
      # Train the SVM
      # ss = svm( x=xx, y=yy, kernel=kernel, gamma=gamma[1], cost=cost[1] )
      best = tune( svm,
                   train.x=xx,
                   train.y=yy,
                   ranges=list( gamma=gamma, cost=cost ),
#                   tunecontrol=tune.control( sampling=sampling, cross=cross ),
			 tunecontrol=tune.control( sampling=sampling ),
                   kernel=kernel )

      # print( "gotBest" )
      # print( paste( sep="", "performance=", round( best$best.performance, 6 ) ) )

      # An array to keep track of the original participating features (by index)
      features = seq(1,ncol(xx))

      # print( length( features ) )

      # Use greedy approach to prune features
      if(featureSelection[1] == "prune") {
         repeat {
            bestColIdToRemove = 0L
            # print( features )
            for(colId in 1:ncol(xx)) {
               # Remove column colId
               zz = xx[,-colId]

               # print( paste( sep="", "trying without feature ", colId ) )

               # Tune with the reduced number of columns
               newBest = tune( svm,
                               train.x=zz,
                               train.y=yy,
                               ranges=list( gamma=gamma, cost=cost ),
                               tunecontrol=tune.control( sampling=sampling, cross=cross ),
                               kernel=kernel )
               # print( paste( sep="", "new performance=", round( newBest$best.performance, 6 ) ) )
               if(round( newBest$best.performance, 6 ) < round( best$best.performance, 6)) {
                  best = newBest
                  bestColIdToRemove = colId
                  # print( paste( sep="", "old performance=", round( best$best.performance, 6 ),
                  #              ", new performance=", round( newBest$best.performance, 6 ) ) )
               }
            }

            if(bestColIdToRemove > 0) {
               # Found an improvement
               xx = xx[,-bestColIdToRemove]
               features = features[-bestColIdToRemove]

               # print( paste( sep="", "improvement, removed feature ", bestColIdToRemove ) )

               # Break if there is only a single feature left
               if(length(features) == 1) break
            } else {
               # No improvements, done
               break
            }
         }
      }
   }

   # print( paste( sep="", "final features: (", paste( sep=",", collapse=",", features ), ")" ) )

   # Predict using the SVM, use only the remaining features
   xtsNewData = data[index(data)[startIndex:endIndex]]
   newData = as.matrix( coredata( xtsNewData[,features] ) )
   fore = predict( best$best.model, newData )

   if( trace ) {
      str = paste( sep="",
                       "\n", index(response)[startIndex], "\n",
                       "=======================\n",
                       "   from: ", head(index(xtsResponse),1),
                       ", to: ", tail(index(xtsResponse),1),
                       ", length: ", length(index(xtsResponse)),
                       "\n   new data: from: ", head(index(xtsNewData), 1),
                       ", to: ", tail(index(xtsNewData), 1),
                       ", length: ", NROW(xtsNewData),
                       "\n   forecast length: ", forecastLength,
                       "\n   best model performance: ", round( best$best.performance, 6 ),
                       "\n   best model features: (", paste( collapse=",", features), ")",
                       "\n   best model gamma: ", best$best.model$gamma,
                       "\n   best model cost: ", best$best.model$cost,
                       "\n   forecasts: ",
                       paste( collapse=", ", round( fore, 6 ) ),
                       "\n" )
      cat( sep="", str )
   }

   return( list( index=startIndex,
                 forecasts=fore,
                 performance=best$best.performance,
                 features=features,
                 gamma=best$best.model$gamma,
                 cost=best$best.model$cost ) )
}


















