## Training with SVM
svmComputeForecasts = function(
      data,
      response,
      history=500,
      modelPeriod="days",
      modelPeriodMultiple=1,
      trace=TRUE,
      startDate,
      endDate,
      kernel="radial",
      gamma=10^(-5:-1),
      cost=10^(0:2),
      sampling="cross",
      cross=10,
      featureSelection=c("add", "prune", "all"),
      cores)
{
   require( e1071 )

   stopifnot( NROW( data ) == NROW( response ) )

   len = NROW( response )

   # Determine the starting index
   if( !missing( startDate ) )
   {
      startIndex = max( len - NROW( index( data[paste( sep="", startDate, "/" )] ) ) + 1,
                        history + 1 )
   }
   else
   {
      startIndex = history + 1
   }

   # Determine the ending index
   if( missing( endDate ) )
   {
      lastIndex = len
   }
   else
   {
      lastIndex = NROW( index( data[paste( sep="", "/", endDate )] ) )
   }

   if( startIndex > lastIndex )
   {
      return( NULL )
   }

   modelPeriod = tolower( modelPeriod[1] )

   forecasts = rep( NA, len )
   gammas = rep( NA, len )
   costs = rep( NA, len )
   performances = rep( NA, len )
   features = rep( "", len )

   # Get the interesting indexes
   periods = index(data)[startIndex:lastIndex]

   # Compute the end points for each period (day, week, month, etc)
   endPoints = endpoints( periods, modelPeriod, modelPeriodMultiple )

   # Compute the starting points of each period, relative to the *data* index
   startPoints = endPoints + startIndex

   # Remove the last start point - it's outside
   length(startPoints) = length(startPoints) - 1

   # Make the end points relative to the *data* index
   endPoints = endPoints + startIndex - 1

   # Remove the first end point - it's always zero
   endPoints = tail( endPoints, -1 )

   stopifnot( length( endPoints ) == length( startPoints ) )

   if( missing( cores ) ) {
      cores = 1
   }

   res = mclapply( seq(1,length(startPoints)),
                   svmComputeOneForecast,
                   data=data,
                   response=response,
                   startPoints=startPoints,
                   endPoints=endPoints,
                   len=len,
                   history=history,
                   trace=TRUE,
                   kernel=kernel,
                   gamma=gamma,
                   cost=cost,
                   featureSelection=featureSelection,
                   mc.cores=cores )
   for( ll in res )
   {
      # Prepare the indexes 
      ii = ll[["index"]]
      jj = ii + NROW( ll[["forecasts"]] ) - 1

      # Copy the output
      forecasts[ii:jj] = ll[["forecasts"]]
      gammas[ii:jj] = ll[["gamma"]]
      costs[ii:jj] = ll[["cost"]]
      performances[ii:jj] = ll[["performance"]]

      # Encode the participating features as a bit mask stored in a single
      # integer. This representation limits us to max 32 features.
      features[ii:jj] = sum( 2^( ll[["features"]] - 1 ) )
   }

   sigUp = ifelse( forecasts >= 0, 1, 0 )
   sigUp[is.na( sigUp )] = 0

   sigDown = ifelse( forecasts < 0, -1, 0 )
   sigDown[is.na( sigDown)] = 0

   # forecasts[is.na( forecasts )] = 0

   sig = sigUp + sigDown

   res = merge( reclass( sig, response ),
                reclass( sigUp, response ),
                reclass( sigDown, response ),
                na.trim( reclass( forecasts, response ) ),
                reclass( performances, response ),
                reclass( gammas, response ),
                reclass( costs, response ),
                reclass( features, response ),
                all=F )
   colnames( res ) = c( "Indicator", "Up", "Down", "Forecasts", "Performance", "Gamma", "Cost", "Features" )

   return( res )
}


























