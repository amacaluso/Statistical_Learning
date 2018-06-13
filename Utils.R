ensureLibrary = function(packageName) {
  
  isPresent = any(row.names(installed.packages()) == packageName)
  if(! isPresent) {
    install.packages(packageName)
  }
  
  library(packageName, character.only=TRUE)
}


evaluation_model = function( target_variable, prediction, MODEL)
{
  # target_variable = Y
  # prediction = lm_pred_train
  # MODEL = model
  n = length( target_variable)
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
                                    path = "results/MODELING/REGRESSION/metrics_data.Rdata" )
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




MD<-function(row){
  point<-as.matrix(test_matrix[row,])
  mahalanobis(train_matrix , point,cov = tr_cov)
}




multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) 
{
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}




CV<-function(x){
  sd(x)/abs(mean(x))
}




save_table = function( df, type = 'REGRESSION')
{
#  df = linear_reg_summary
  name_file = deparse(substitute(df))
  PATH = paste0( "results/MODELING/", type, "/", name_file,".Rdata")
  save( linear_reg_summary, file = PATH)

  }
#



save_plot = function( plot, type = 'REGRESSION')
{
  #  df = linear_reg_summary
  name_file = deparse(substitute(plot))
  PATH = paste0( "results/MODELING/", type, "/plots/", name_file,".Rdata")
  save( plot, file = PATH)
  
}



plot_subset_error<-function(model.summary){
  
  par(mfrow=c(1,3))
  plot(model.summary$rss, 
       xlab = "Number of Variables", ylab = "RSS",
       main = "Residual Sum of Squares",
       col = "red", type = "b", pch = 16)
  
  imin = which.min(model.summary$rss)
  imin
  
  # Add a marker for the suggested number of variables
  points(imin, model.summary$rss[imin], pch = 17, col = "blue", cex=2)
  
  # Add the marker also according to adjusted R^2
  imin = which.min(model.summary$adjr2)
  imin
  
  points(imin, model.summary$rss[imin], pch = 17, col = "darkgoldenrod2", cex=2)
  
  # Plot BIC vs. the number of variables
  
  plot(model.summary$bic, xlab = "Number of Variables", ylab = "BIC",
       main = "BIC",
       col = "red", type = "b", pch = 16)
  
  imin = which.min(model.summary$bic)
  imin
  
  # Add a marker for the suggested number of variables
  points(imin, model.summary$bic[imin], pch = 17, col = "blue", cex=2)
  
  # Plot Mallow's Cp vs. the number of variables, and find 
  # the best specification
  
  plot(model.summary$cp, xlab = "Number of Variables", ylab = "Cp",
       main = "Mallow's CP",
       col = "red", type = "b", pch = 16)
  
  # Find the minimum 
  
  imin = which.min(model.summary$cp)
  imin
  
  # The best subset regression contains 11 variables plus 
  # the intercept. Add a point to the plot to highlight the minimum
  
  points(imin, model.summary$cp[imin], pch = 17, col = "blue", cex=2)
  
  return (imin) #according to Mallow's CP
}





