library(MASS)
library(glmnet)
library(ggplot2)
library(parallel)



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

remove_columns_by_names = function (df, colNames)
{
  colnames_to_keep = colnames(df)[ ! colnames(df) %in% colNames ]
  df = as.data.frame( df )[ , colnames_to_keep ]
  df
}

ggplotRegression <- function (fit, regressore) 
{
  require(ggplot2)
  png(paste("img/", regressore, "_regression_plot.png"))
  
  plot = ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       # "Intercept =",signif(fit$coef[[1]],5 ),
                       # " Slope =",signif(fit$coef[[2]], 5),
                       " p-value (F-Statistic) =", signif(summary(fit)$coef[2,4], 5)))
  print(plot)
  dev.off()
}

ROC_analysis = function(
  prediction, 
  y_true, 
  probability_thresholds =  seq( 0.2 , 0.8, by = 0.05 ))
{  
  matrice_risultati = lapply( probability_thresholds, function( tresh, prediction, y_true  ) {
    
    y_hat = as.numeric( prediction >= tresh )
    
    roc_row = list(
      y_1__y_hat_1 = sum( y_true == 1 & y_hat == 1  ),
      y_1__y_hat_0 = sum( y_true == 1 & y_hat == 0  ),
      y_0__y_hat_1 = sum( y_true == 0 & y_hat == 1  ),
      y_0__y_hat_0 = sum( y_true == 0 & y_hat == 0  )
    )
    
    roc_row
    
  }, prediction, y_true ) 
  
  df_roc = rbindlist( matrice_risultati )
  
  df_roc$accuracy = ( df_roc$y_1__y_hat_1 + df_roc$y_0__y_hat_0 ) / length(y_true)
  df_roc$specificity = df_roc$y_0__y_hat_0 / sum( y_true == 0 )
  df_roc$sensitivity__recall = df_roc$y_1__y_hat_1 / sum( y_true == 1 )
  df_roc$positive_predictive_value = df_roc$y_1__y_hat_1 / ( df_roc$y_1__y_hat_1 + df_roc$y_0__y_hat_1 )
  
  fpr = df_roc$y_0__y_hat_1 / ( df_roc$y_0__y_hat_1 + df_roc$y_0__y_hat_0 ) 
  df_roc$positive_likelihood_ratio = df_roc$sensitivity / fpr
  
  df_roc$F1_score = 2 * ( ( df_roc$sensitivity__recall * df_roc$positive_predictive_value )
                          / ( df_roc$sensitivity__recall + df_roc$positive_predictive_value ) )
  
  colnames( df_roc ) <- c(
    "(y=1,y_hat=1) TP",
    "(y=1,y_hat=0) FN",
    "(y=0,y_hat=1) FP",
    "(y=0,y_hat=0) TN",
    "Accuracy: true/total",
    "Specificity: TN/negative",
    "Sensitivity (AKA Recall): TP/positive",
    "Positive Predictive Value: TP/predicted_positive",
    "Positive Likelihood Ratio",
    "F1_SCORE"
  )
  
  df_roc = cbind( probability_thresholds, df_roc ) 
  #rownames( df_roc ) <- probability_thresholds
  
  df_roc
}


#get the machine operating system
get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}


if (!require(caTools)) {
  install.packages("caTools")
  library(caTools)}
  
  if (!require(data.table)) {
    install.packages("data.table")
    library(data.table)
}


if (!require(ROCR)) {
  install.packages("ROCR")
  library(ROCR)
}



if (!require(glmnet)){
  install.packages("glmnet")
  library(glmnet)
}


if (!require(class)) {
  install.packages("class")
  library(class)
}


MD<-function(row){
  point<-as.matrix(test_matrix[row,])
  mahalanobis(train_matrix , point,cov = tr_cov)
}

library(parallel)


if (!require(caret)) {
  install.packages("caret")
  library(caret)
}



if (!require(e1071)) {
  install.packages("e1071")
  library(e1071)
}
