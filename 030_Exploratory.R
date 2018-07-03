### **** Importazione dati e Descrittive **** ###
source( 'Utils.R')
SEED = 12344321
source( '020_Pre_processing.R')

# EXPLORATORY ANALYSES ----------------------------------------------------
folder = "results/EXPLORATORY_ANALYSES"
dir.create( folder )

breaks = seq(1,10,by=.9)

bw<-function(b, x) { b/bw.nrd0(x) }

red_hist_density = ggplot( data = red ) + 
                   geom_histogram( aes( x = red$quality,  y = ..density..), 
                                   breaks = breaks, colour = "black", fill = c("red4")) + 
                   geom_density( aes( x = red$quality ), adjust=bw(0.4, red$quality)) +
                   scale_x_continuous( breaks = breaks) +
                   xlab("wine quality (1-10)") + ggtitle("Red wine") +
                   theme_bw() 

red_hist_density = ggplotly( red_hist_density )


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



# rm( breaks, red_hist_density, white_hist_density )




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


# ********** Saving a file ******************* #
################################################
file_name = paste0( folder, "/variables_density.Rdata")
save( plots, file = file_name)
################################################



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



# Within group covariance matrices

corr_matrix = cor(wine[, 1:12])
corr_matrix = round( corr_matrix, 3)
corrplot = ggplotly( ggcorrplot(corr_matrix, hc.order = TRUE,
                                outline.col = "white",
                                #ggtheme = ggplot2::theme_gray,
                                colors = c("#6D9EC1", "white", "#E46726")) +
                       ggtitle("Matrice di correlazione"))
# ********** Saving a file ******************* #
file_name = paste0( folder, "/corrplot.Rdata")
save( corrplot, file = file_name)
# ******************************************** #

# Spearman correlation
spearman_v = c()
pearson_v = c()
kendall_v = c()

for (col in colnames( wine[ , 1:11]) )
{
  #data = cbind( wine[, col], wine[, 'quality'])
  pearson = cor( wine[, col], wine[, 'quality'] )
  pearson_v = c(pearson_v, pearson)
  
  spearman = cor(wine[, col], wine[, 'quality'], method = 'spearman')
  spearman_v = c( spearman_v, spearman )
  
  kendall = cor(wine[, col], wine[, 'quality'], method = 'kendall')
  kendall_v = c(kendall_v, kendall )
  
  cat( col, " --> ", pearson, " e ", spearman, "\n")
  
  #anova()
}

data_corr = data.frame( Variables = colnames( wine[ , 1:11]),
                        Pearson = round( pearson_v, 3),
                        Spearman = round(spearman_v,3), 
                        Kendall = round(kendall_v,3))

corr_Y = ggplot(data_corr, aes( Variables, Spearman, fill = Variables, text = paste('Kendall:', Kendall, "\n", 
                                                                                    'Pearson:', Pearson, "\n" ))) +
         geom_bar(  stat = "identity", position='stack') + 
         ggtitle( "Correlazione della variabile Qualità" ) + guides( fill = FALSE ) +
         theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
         ylab("Spearman's correlation") 
#  theme(axis.text.x = element_text(angle = 45, vjust = 1,  size = 12, hjust = 1))

corr_Y = ggplotly( corr_Y ) %>% layout( showlegend = FALSE )

# ********** Saving a file ******************* #
file_name = paste0( folder, "/corr_Y.Rdata")
save( corr_Y, file = file_name)
# ******************************************** #
# rm( means_tr, sd_tr, cv_tr, means_ts, sd_ts, cv_ts,
#     descriptive_tr, descriptive_ts, descriptive)

df = data.frame( table( wine$quality))
colnames( df ) = c("Quality", 'Frequencies')
df$Relative_Freq = paste( round( df$Frequencies/sum(df$Frequencies)*100, 1), '%')

quality_distr = ggplot(data=df, aes(x=Quality, y=Frequencies, fill = Quality)) +
                geom_bar(stat="identity") + 
                geom_text(aes(label=Relative_Freq),  vjust=-5, color="black") + #
                ggtitle( 'Qualità - Distribuzione di frequenza')

quality_distr = ggplotly( quality_distr )

# ********** Saving a file ******************* #
file_name = paste0( folder, "/quality_distr.Rdata")
save( quality_distr, file = file_name)
# ******************************************** #


rm(list= ls())

cat('\n\n SCRIPT ESEGUITO CORRETTAMENTE!! \n\n')
