### **** Importazione dati e Descrittive **** ###

source( 'Utils.R')


ensureLibrary('MASS')
ensureLibrary('glmnet')
ensureLibrary('ggplot2')
ensureLibrary('parallel')
ensureLibrary('caret')
ensureLibrary('e1071')
ensureLibrary('caTools')
ensureLibrary('data.table')
ensureLibrary('ROCR')
ensureLibrary('glmnet')
ensureLibrary('class')
ensureLibrary('ggplot2')
ensureLibrary('plotly')
ensureLibrary('gridExtra')
ensureLibrary('plotROC')




red <- read.table( 'data/winequality-red.csv', sep = ";" , header = T)
white <- read.table( 'data/winequality-white.csv' ,sep=";" , header = T )


#create joint dataset
red$type <- 0 #"red"
white$type <- 1 #"white"

wine <- rbind.data.frame( red, white)
wine$binary_quality <- ifelse( wine$quality <= 5, yes = 0, no = 1) #as.factor(ifelse(wine$quality<=5,0,1))

wine_binary = remove_columns_by_names( df = wine, colNames = 'quality')
wine = remove_columns_by_names( df = wine, colNames = 'binary_quality')



# EXPLORATORY ANALYSES ----------------------------------------------------

folder = "results/EXPLORATORY_ANALYSES"
dir.create( folder )

# str(red)
# summary(red)
# pairs(red,col="red4")

# str(white)
# summary(white)
# pairs(white, col="khaki1")

breaks = seq(1,10,by=.9)

bw<-function(b, x) { b/bw.nrd0(x) }

red_hist_density = ggplot( data = red ) + 
  geom_histogram( aes( x = red$quality,  y = ..density..), 
                  breaks = breaks, colour = "black", fill = c("red4")) + 
  geom_density( aes( x = red$quality ), adjust=bw(0.4, red$quality)) +
  scale_x_continuous( breaks = breaks) +
  xlab("wine quality (1-10)") + ggtitle("Red wine") +
  theme_bw() 

red_hist_density = ggplotly( red_hist_density)


# ********** Saving a file ******************* #
################################################
file_name = paste0( folder, "/red_hist_density.Rdata")
save( red_hist_density, file = file_name)
################################################



white_hist_density = ggplot( data = white ) + 
  geom_histogram( aes( x = white$quality,  y = ..density..), 
                  breaks = breaks, colour = "white", fill = c("#ba9d1b")) + 
  geom_density( aes( x = white$quality ), adjust=bw( 0.4, white$quality)) +
  scale_x_continuous( breaks = breaks) +
  xlab("wine quality (1-10)") + ggtitle("White wine") +
  theme_bw() 

white_hist_density = ggplotly( white_hist_density)


# ********** Saving a file ******************* #
################################################
file_name = paste0( folder, "/white_hist_density.Rdata")
save( white_hist_density, file = file_name)
################################################



rm( breaks, red_hist_density, white_hist_density )




### L: CAMBIARE BANDWIDTH DI DENSITY???
plot_density = function( var, unit)
{
  # var = 'fixed.acidity'
  # unit = 'g(tartaric_acid)/dm^3)'
  legend = paste( "bandwith =",round(density( wine[ , var ])$bw, 3))
  
  plot = ggplot() + geom_density( aes( x = wine[ , var]), col = "deepskyblue", lwd = 1) +
    geom_density( aes( x = red[ , var]), linetype = "dotted", col = "red4") +
    geom_density( aes( x = white[ , var]), linetype = "dotted", col = "#ba9d1b") +                    
    ggtitle( gsub( ".", " ", var, fixed = T )) + 
    xlab( unit) +
    theme_bw()
  #geom_text( legend ) 
  return( plot)
}


n_var = length(colnames(wine))
variables = colnames( wine )[ -n_var]

unit_measures = c( 'g(tartaric_acid)/dm^3)', 'g(acetic_acid)/dm^3',
                   'g/dm^3', 'g/dm^3', 'g(sodium_chloride)/dm^3',
                   'mg/dm^3', 'mg/dm^3', 'g/cm^3','pH', 
                   'g(potassium_sulphate)/dm^3', '% volume')


plots = lapply( X = variables, FUN = plot_density, unit = unit_measures)
density_variables = do.call("grid.arrange", c(plots, ncol=3))


# ********** Saving a file ******************* #
################################################
file_name = paste0( folder, "/variables_density.Rdata")
save( density_variables, file = file_name)
################################################



rm( n_var, plots, variables, density_variables)

# SEED = 12344321
set.seed( SEED )
cat( "SEED is", SEED)


train.label = sample.split(wine_binary, SplitRatio = 3/5)
cat("SplitRatio is 3/5")


# Check that the split is balanced

train.wine_binary = wine_binary[train.label, ]
train.wine = wine[train.label, ]

test.wine_binary = wine_binary[!train.label,]
test.wine = wine[!train.label,]


statistics = c( "Mean", "Standard Deviation", "Coefficient of variation" )

means_tr = apply( X = train.wine, MARGIN = 2, FUN = mean)
sd_tr = apply( X = train.wine, MARGIN = 2, FUN = sd)
cv_tr = apply( X = train.wine, MARGIN = 2, FUN = CV)

means_ts = apply( X = test.wine, MARGIN = 2, FUN = mean)
sd_ts = apply( X = test.wine, MARGIN = 2, FUN = sd)
cv_ts = apply( X = test.wine, MARGIN = 2, FUN = CV)

descriptive_tr = as.data.frame( round( rbind( means_tr, sd_tr, cv_tr ), 3 ) )
descriptive_tr = cbind( Set = "Training Set", Statistic = statistics, descriptive_tr )

descriptive_ts = as.data.frame( round( rbind( means_ts, sd_ts, cv_ts ), 3 ))
descriptive_ts = cbind( Set = "Test Set", Statistic = statistics, descriptive_ts )

descriptive = rbind( descriptive_tr, descriptive_ts )
colnames( descriptive ) = gsub( pattern = ".", replacement = " ", x = colnames( descriptive ), fixed = T )



# ********** Saving a file ******************* #
################################################
save( descriptive, file = paste0(folder, "/descriptive_table.Rdata"))
################################################



rm( means_tr, sd_tr, cv_tr, means_ts, sd_ts, cv_ts,
    descriptive_tr, descriptive_ts, descriptive)

