source( 'code/Utils.R')
SEED = 12344321
source( 'code/020_Pre_processing.R') # REQUIRE SEED

tresholds=seq( from = 0, to = 1, by = 0.01)

#GAM
#########################################################
d = seq(1,10)
grid2 = expand.grid(.df = d)
control = trainControl(method = "cv", number = 10, savePredictions = TRUE)

gam = train(factor(binary_quality) ~ ., method="gamSpline", metric='Accuracy', tuneGrid = grid2, 
            trControl = control, data = train.wine_binary, type = "prob")
#summary(gam)
gam.coef = coef(gam$finalModel) ; gam.coef
gam.probs = predict(gam, newdata = test.wine_binary, type = "prob")
predob = prediction(gam.probs[,2], test.wine_binary$binary_quality)
auc = as.numeric(performance(predob, "auc")@y.values)

gam_data = gam$results

# Validazione GAM
gam_validation = ggplot(data = gam_data, aes( x = df)) +
                 geom_line( aes( y = Accuracy, color = 'red' )) +
                 geom_point( aes( y = Accuracy, color = 'red' ), show.legend = F) + 
                 geom_ribbon(aes(ymin=Accuracy-AccuracySD, ymax=Accuracy+AccuracySD), linetype=2, alpha=0.1) +
                 ggtitle( "Validazione parametri GAM" ) +
                 theme(plot.title = element_text(size = 15, face = "bold")) + 
                 scale_x_continuous(breaks=1:10)

ply_val_gam = ggplotly( gam_validation ) %>%
              layout(title = "Validazione parametri GAM", showlegend = F)
            # legend = list(orientation = "v")) # , y = 0, x = 0))
save_plot( ply_val_gam, type = "CLASSIFICATION")

tresholds=seq( from = 0, to = 1, by = 0.01)

# MATRICI PER CURVA ROC
ROC_gam = cbind( Model = 'Generalised Additive Model', 
                 ROC_analysis( prediction = gam.probs[, 2], 
                               y_true = test.wine_binary$binary_quality,
                               probability_thresholds = tresholds),
                 AUC = auc)



ROC_matrix_gam = ROC_analysis( prediction = gam.probs[, 2], 
                               y_true = test.wine_binary$binary_quality, 
                               probability_thresholds = tresholds)

ROC_matrix_gam = data.frame( treshold = ROC_matrix_gam$probability_thresholds,
                             FPR = 1-ROC_matrix_gam$`Specificity: TN/negative`, 
                             TPR = ROC_matrix_gam$`Sensitivity (AKA Recall): TP/positive` )

#########################################################


# #SINGLE VARIABLE TUNING GAM ---------------------------------------------------------

ensureLibrary('mgcv')
response = colnames(train.wine_binary)[13]
regressors = colnames(train.wine_binary)[-c(12,13)]

best_formula = as.formula(paste(response," ~ ",paste("s(",regressors,")",collapse=" + "), "+ type"))
best_formula

rgam.all = gam(best_formula,data=train.wine_binary, family="binomial")

rgam.all_summary = as.data.frame( round( summary(rgam.all)$s.table, 3))
rgam.all_summary = cbind( variable = row.names(rgam.all_summary), rgam.all_summary )


# ********** Saving a file ******************* #
save_table( df = rgam.all_summary, type = "CLASSIFICATION")
# summary(rgam.all)

### fixed.acidity, chlorides e pH non significativi --> provo a levarli e testare la differenza

fixed.par = rgam.all$sp

best_formula = as.formula(paste(response," ~ ",paste("s(",regressors[-c(1,5,9)],")",collapse=" + "), "+ type"))
best_formula
  
rgam.sig = gam(best_formula,data=train.wine_binary, family="binomial", sp=fixed.par[-c(1,5,9)])
  
test_allVSsig = as.data.frame(round(anova(rgam.sig, rgam.all, test="Chisq"),3))
test_allVSsig = cbind( variable = c("Full Model", "Restricted Model"), test_allVSsig )

# ********** Saving a file ******************* #
save_table( df = test_allVSsig, type = "CLASSIFICATION")

###continuiamo col modello senza variabili non significative 
rgam.sig = gam(best_formula,data=train.wine_binary, family="binomial")
save( rgam.sig, file = 'results/MODELING/CLASSIFICATION/rgam.sig.Rdata' )

summary(rgam.sig)

# plot(rgam.sig,scheme=1,shade=T,pages=2,col="blue3")
# png(filename='results/MODELING/CLASSIFICATION/restricted_gam.png')
# plot(rgam.sig,scheme=1,shade=T,pages=2,col="blue3")
# dev.off();

###proviamo un effetto lineare per: density e sulphates

smooth.regressors = colnames(train.wine_binary)[c(2,3,4,6,7,11)]
lin.regressors = colnames(train.wine_binary)[c(8,10,12)]

best_formula = as.formula(paste(response," ~ ",paste("s(",smooth.regressors,")",collapse=" + "), " + ", paste(lin.regressors,collapse=" + ")))
best_formula

fixed.par = rgam.sig$sp[-c(6,7)]

rgam.lin = gam(best_formula,data=train.wine_binary, family="binomial", sp=fixed.par)


test_sigVSlin = as.data.frame(round(anova(rgam.lin, rgam.sig, test="Chisq"),3))
test_sigVSlin = cbind( variable = c("Restricted Model", "Linear Effects"), test_sigVSlin )

# ********** Saving a file ******************* #
save_table( df = test_sigVSlin, type = "CLASSIFICATION")

rgam.lin = gam(best_formula,data=train.wine_binary, family="binomial")

# summary(rgam.lin)
rgam.lin_summary = as.data.frame( round( summary(rgam.lin)$s.table, 3))
rgam.lin_summary = cbind( variable = row.names(rgam.lin_summary), rgam.lin_summary )


# ********** Saving a file ******************* #
save_table( df = rgam.lin_summary, type = "CLASSIFICATION")

rgam.probs = predict(rgam.lin, newdata = test.wine_binary, type = "response")
predob = prediction(as.vector(rgam.probs), test.wine_binary$binary_quality)
auc = as.numeric(performance(predob, "auc")@y.values)

tresholds=seq( from = 0, to = 1, by = 0.01)

# MATRICI PER CURVA ROC
ROC_rgam = cbind( Model = 'Tuned Generalised Additive Model', 
                 ROC_analysis( prediction = as.vector(rgam.probs), 
                               y_true = test.wine_binary$binary_quality,
                               probability_thresholds = tresholds),
                 AUC = auc)



ROC_matrix_rgam = ROC_analysis( prediction = as.vector(rgam.probs), 
                               y_true = test.wine_binary$binary_quality, 
                               probability_thresholds = tresholds)

ROC_matrix_rgam = data.frame( treshold = ROC_matrix_rgam$probability_thresholds,
                             FPR = 1-ROC_matrix_rgam$`Specificity: TN/negative`, 
                             TPR = ROC_matrix_rgam$`Sensitivity (AKA Recall): TP/positive` )

#########################################################


################### CART ##########################
ensureLibrary('rpart')
ensureLibrary('rpart.plot')

set.seed(SEED)

Tmax = rpart( factor( binary_quality ) ~ ., data = train.wine_binary, 
               control = rpart.control( minsplit = 2, cp = 0.00000001 )) 

validation_data = as.data.frame( printcp(Tmax) )

colnames( validation_data ) = c( "CP", "nsplit", 'training_error', 'cv_error', 'cv_std')

tree_val = ggplot( data = validation_data, aes( x = nsplit)) +  
           geom_line( aes( y = cv_error), col = 'blue')  +
           geom_point( aes(y = cv_error), col = 'blue') +
           geom_ribbon(aes(ymin = cv_error - cv_std,
                           ymax = cv_error + cv_std ),
                       linetype=2, alpha=0.1) +
           geom_point( aes( y = training_error), col = 'red' ) + 
           geom_line( aes( y = training_error), col = 'red')  +
           ggtitle('Cart - Validazione CP') + xlab( ' Number of splits ') + ylab( 'Error') 
  

tree_val = ggplotly( tree_val )
save_plot( tree_val, type = 'CLASSIFICATION')

opt_cp = validation_data[ which.min(validation_data[,4]), 'CP' ]
albero = prune(Tmax, cp = opt_cp)

plot(albero)
text(albero)
# text(albero, fancy=T, fwidth = 0.3, fheight = 0.3, splits = F)

# rpart.plot(albero)
# prp(albero, type=1, extra=111, under=TRUE)
# heat.tree(albero)


 
cp_1se_soglia = validation_data[ which.min(validation_data[,4]), 'cv_error' ]  + validation_data[ which.min(validation_data[,4]), 'cv_std' ] 

validation_data_subset = validation_data[ 1:which.min(validation_data[,4]), ]
min_opt_cp = validation_data_subset[(validation_data_subset[,4] < cp_1se_soglia),][1,1]



tree2plot = prune(Tmax, cp = min_opt_cp)
save( tree2plot, file = 'results/MODELING/CLASSIFICATION/tree2plot.Rdata' )

# png(filename='results/MODELING/CLASSIFICATION/tree_plot.png')
# rpart.plot(tree2plot, type=3, extra=10, branch.lty=10, box.palette="RdYlGn")
# dev.off();

png(filename='results/MODELING/CLASSIFICATION/tree_plot.png')
rpart.plot(tree2plot, type=4)
dev.off();

Y_pred_cart = predict(albero, test.wine_binary)

importance = data.frame(Importance = albero$variable.importance)
importance$Variable = row.names( importance )

plot_importance_tree = ggplot(data = importance, aes( x = Variable , y = Importance,
                                                      fill = Variable)) +
                       geom_bar( stat="identity") + 
                       ggtitle( 'Cart - Variable Importance') + xlab( 'Variable') +
                       theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_importance_tree = ggplotly( plot_importance_tree )
save_plot( plot_importance_tree, type = 'CLASSIFICATION')


predob = prediction(Y_pred_cart[,2], test.wine_binary$binary_quality)
auc = as.numeric(performance(predob, "auc")@y.values)


ROC_CART = cbind( Model = 'CART',
                  ROC_analysis( Y_pred_cart[,2], test.wine_binary$binary_quality,
                                probability_thresholds = tresholds),
                  AUC = auc)



ROC_matrix_CART = ROC_analysis( prediction = Y_pred_cart[, 2], 
                               y_true = test.wine_binary$binary_quality, 
                               probability_thresholds = tresholds)

ROC_matrix_CART = data.frame( treshold = ROC_matrix_CART$probability_thresholds,
                             FPR = 1-ROC_matrix_CART$`Specificity: TN/negative`, 
                             TPR = ROC_matrix_CART$`Sensitivity (AKA Recall): TP/positive` )

#################################################################################





######################################################################
###################### Random Forest #################################
ensureLibrary('randomForest')


n = nrow( train.wine_binary )
p = ncol( train.wine_binary ) -1


mtry = seq( from = 1, to = p )
ntree = 500

validation = data.frame( matrix( nrow = ntree ))

for( i in 1:length( mtry ))
{
  print( i )
  rf = randomForest( factor(binary_quality) ~ ., data = train.wine_binary,
                     importance = T, ntree = ntree, mtry = mtry[i] )
  
  validation[, paste(mtry[i]) ] = rf$err.rate[,1]
  }

validation = data.frame( ntree = rep( x = 1:ntree,  p), melt( validation[,-1]))
colnames( validation ) = c( 'ntree', 'mtry', 'error' )


best_ntree = validation[ which.min(validation$error), 'ntree']
best_error = validation[ which.min(validation$error), 'error'] 
best_mtry = as.numeric( validation[ which.min(validation$error), 'mtry' ] )




RF_valid_err = ggplot(data = validation, aes( x = ntree, y = error, color = mtry)) +
               geom_line() +
               ggtitle( "OOB Error - Random Forest" ) +
               theme(plot.title = element_text(size = 15, face = "bold")) + 
               geom_point( aes( x = best_ntree, y = best_error , text = best_mtry))

RF_valid_err = ggplotly( RF_valid_err )  
save_plot( RF_valid_err, type = 'CLASSIFICATION')


rf = randomForest( factor(binary_quality) ~ ., data = train.wine_binary,
                   importance = T, ntree = best_ntree, mtry = best_mtry )

Y_RF = predict(rf, test.wine_binary, type = 'prob')[,2]

importance = data.frame(rf$importance[, -4])
importance$Variable = row.names( importance )
colnames( importance ) = c( 'class_0', 'class_1', 'Mean_Decrease_Accuracy' )

plot_importance_rf = ggplot(data = importance, aes( x = row.names( importance ), y = Mean_Decrease_Accuracy,
                                                    fill = row.names( importance ), 
                                                    text = paste( 'Classe 0 = ', class_0,"\n",
                                                                  'Classe 1 = ', class_1))) +
                     theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
                     geom_bar( stat="identity") + 
                     ggtitle( 'Random Forest - Variable Importance') + xlab('Variable')

plot_importance_rf = ggplotly( plot_importance_rf )
save_plot( plot_importance_rf, type = 'CLASSIFICATION' )


ROC_RF = ROC_analysis( Y_RF, test.wine_binary$binary_quality)


predob = prediction(Y_RF, test.wine_binary$binary_quality)
auc = as.numeric(performance(predob, "auc")@y.values)


ROC_RF = cbind( Model = 'RANDOM FOREST',
                  ROC_analysis( Y_RF, test.wine_binary$binary_quality,
                                probability_thresholds = tresholds),
                  AUC = auc)



ROC_matrix_RF = ROC_analysis( prediction = Y_RF, 
                                y_true = test.wine_binary$binary_quality, 
                                probability_thresholds = tresholds)

ROC_matrix_RF = data.frame( treshold = ROC_matrix_RF$probability_thresholds,
                              FPR = 1-ROC_matrix_RF$`Specificity: TN/negative`, 
                              TPR = ROC_matrix_RF$`Sensitivity (AKA Recall): TP/positive` )
############################################################################################


############ Risultati complessivi ###############

ROC_all = rbind( ROC_gam, ROC_rgam, ROC_CART, ROC_RF)

ROC_best_DM = ROC_all %>%
  group_by(Model) %>%
  slice(which.max(`Accuracy: true/total`))

ROC_best_DM = remove_columns_by_names( ROC_best_DM, colNames = c( "(y=1,y_hat=1) TP" , "(y=1,y_hat=0) FN",
                                                            "(y=0,y_hat=1) FP", "(y=0,y_hat=0) TN",
                                                            "Positive Predictive Value: TP/predicted_positive",
                                                            "Positive Likelihood Ratio", "F1_SCORE" ))

save_table( ROC_best_DM, type = 'CLASSIFICATION')



ROC_all$FPR = 1 - ROC_all$`Specificity: TN/negative`
ROC_all$TPR = ROC_all$`Sensitivity (AKA Recall): TP/positive`


roc_curve_all = ggplot(data = ROC_all, aes( x = FPR, y = TPR, color = Model, 
                                            text = paste("AUC:", AUC, "\n"))) +
                geom_line() +
                ggtitle( "ROC CURVE ALL" ) +
                theme(plot.title = element_text(size = 15, face = "bold"))


roc_curve_all_DM = ggplotly( roc_curve_all )
save_plot( roc_curve_all_DM, type = 'CLASSIFICATION')
####################################################
