library(MASS)
library(glmnet)



evaluation_model = function( target_variable, prediction, MODEL)
{
  # target_variable = y
  # prediction = y_hat
  # MODEL = model
  
  VARIANCE = sum(( target_variable - mean( target_variable ))^2 ) / n
  MSE = sum((target_variable - prediction)^2 ) / n
  RSE = MSE / VARIANCE
  R_SQUARED = 1 - ( MSE / VARIANCE )
  R_SQUARED
  results = c( VARIANCE, MSE, RSE, R_SQUARED)
  results = data.frame( round( t( as.matrix( results )), digits = 3 ) )
  
  load_and_update_metrics(MODEL = MODEL, METRICS = results)
  
}



load_and_update_metrics = function( MODEL = NA,
                                    METRICS = NA, 
                                    path = "data/metrics_data.Rdata" )
{
  
  # METRICS = results
  
  load( path )
  TIME_STAMP = Sys.time()
  df = cbind(TIME_STAMP, MODEL, METRICS )
  colnames(df) = c('TIME_STAMP', 'MODEL', 'VARIANCE', 'MSE', 'RSE', 'R_SQUARED')
  
  print( df )

  if( df$MODEL %in% metrics_data$MODEL )
  {
    cat( "\n METRICS OF ", MODEL, "HAS BEEN UPDATED \n")
    metrics_data = metrics_data[ !(metrics_data$MODEL == df$MODEL), ] 
    metrics_data = rbind( metrics_data, df )
  } else
  { 
    cat( "\n METRICS OF ", MODEL, "HAS BEEN ADDED\n")
    metrics_data = rbind( metrics_data, df ) 
  }
  
  save( metrics_data, file = path )
  cat( "METRICS DATA HAS BEEN SAVED \n")
  
}





