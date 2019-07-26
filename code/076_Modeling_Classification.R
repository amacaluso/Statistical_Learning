### ***** IMPORT ***** ###
##########################


source( 'code/Utils.R')

SEED = 12344321
source( 'code/072_Modeling_lpm.R') # REQUIRES SEED

SEED = 12344321
source( 'code/074_Modeling_lda.R') # REQUIRES SEED

SEED = 12344321
source( 'code/020_Pre_processing.R') # REQUIRE SEED



### ***** SAVING FOLDER ***** ###
folder = "results/MODELING/CLASSIFICATION"
dir.create( folder )
##################################



## Quadratic discriminant analysis
###################################
# Fit the model on the training sample 

qda.fit = qda(binary_quality ~ ., data = train.wine_binary)

# Summary of results
qda.fit

# Predict the qda fit on the test sample
qda.pred = predict(qda.fit, newdata = test.wine_binary)

test_accuracy <- mean(qda.pred$class==test.wine_binary$binary_quality)
test_accuracy

# Test set ROC curve and AUC.
pred_qda = prediction(qda.pred$posterior[ ,2], test.wine_binary$binary_quality)
perf = performance(pred_qda, "tpr", "fpr")

auc = as.numeric(performance(pred_qda, "auc")@y.values)


tresholds<-seq( from = 0, to = 1, by = 0.01)
ROC_qda = cbind( Model = 'Quadratic Discriminant Analysis',
                 ROC_analysis( prediction = qda.pred$posterior[,2], 
                               y_true = test.wine_binary$binary_quality,
                               probability_thresholds = tresholds),
                 AUC = auc)

ROC_all = rbind( ROC_all, ROC_qda )



ROC_matrix_qda = ROC_analysis( prediction = qda.pred$posterior[,2], 
                               y_true = test.wine_binary$binary_quality, 
                               probability_thresholds = tresholds)

ROC_matrix_qda = data.frame( treshold = ROC_matrix_qda$probability_thresholds,
                             FPR = 1-ROC_matrix_qda$`Specificity: TN/negative`, 
                             TPR = ROC_matrix_qda$`Sensitivity (AKA Recall): TP/positive` )

roc_curve_qda = ggplot(ROC_matrix_qda, aes(x = FPR, y = TPR, label = treshold)) +
                geom_line(color = "yellow") + theme_bw() + 
                style_roc() + #annotate("point", x = v, y = h, colour = "white")+
                ggtitle( "Quadratic discriminant analysis - test set")


roc_curve_qda = ggplotly( roc_curve_qda )
save_plot( roc_curve_qda, type = 'CLASSIFICATION')
  
#rm(list=setdiff(ls(), 'ROC_all'))
###################################################



# LOGISTIC REGRESSION ----------------------------------
###########################################################

train.wine_binary$binary_quality <- as.factor(train.wine_binary$binary_quality)
test.wine_binary$binary_quality <- as.factor(test.wine_binary$binary_quality)

# Logistic regression fit on the training sample.

glm.fit = glm( binary_quality ~ ., data = train.wine_binary, family = binomial)

# Predicted probabilities on the test sample 
glm.probs = predict(glm.fit, newdata = test.wine_binary, type = "response") 

# Predicted responses on the test set
glm.pred = ifelse(glm.probs > 0.5, 1, 0)


# Test set ROC curve and AUC
pred_logistic = prediction(glm.probs, test.wine_binary$binary_quality)
perf = performance(pred_logistic, "tpr", "fpr")


auc = as.numeric(performance(pred_logistic, "auc")@y.values)


tresholds<-seq( from = 0, to = 1, by = 0.01)

ROC_log = cbind( Model = 'Logistic Regression', 
                 ROC_analysis( prediction = glm.probs, 
                               y_true = test.wine_binary$binary_quality,
                               probability_thresholds = tresholds), 
                 AUC = auc)

ROC_all = rbind( ROC_all, ROC_log )



ROC_matrix_log = ROC_analysis( prediction = glm.probs, 
                               y_true = test.wine_binary$binary_quality, 
                               probability_thresholds = tresholds)

ROC_matrix_log = data.frame( treshold = ROC_matrix_log$probability_thresholds,
                             FPR = 1-ROC_matrix_log$`Specificity: TN/negative`, 
                             TPR = ROC_matrix_log$`Sensitivity (AKA Recall): TP/positive` )

roc_curve_log = ggplot(ROC_matrix_log, aes(x = FPR, y = TPR, label = treshold)) +
                geom_line(color = 15) + theme_bw() + 
                style_roc() + #annotate("point", x = v, y = h, colour = "white")+
                ggtitle( "Logistic Regression - test set")


roc_curve_log = ggplotly( roc_curve_log )
save_plot( roc_curve_log, type = 'CLASSIFICATION')
################################################

# rm(list=setdiff(ls(), c("test_error", 'ROC_all')))


# REGULARIZED LOGISTIC REGRESSION -------------------
################################################################
# Load the 'glmnet' package

# Create the matrix of predictors in the usual way

x <- model.matrix(binary_quality ~ . -1, data = wine_binary) 
y <- wine_binary$binary_quality

# Let's make again a training and test set

x.train = x[train.label, ]
y.train = y[train.label]
x.test = x[!train.label, ]
y.test = y[!train.label]

# To select the optimal penalty parameter, we run the 'cv.glmnet' 
# function to do 10-fold CV on 'lambda'

cv.ridge = cv.glmnet(x.train, y.train, alpha = 0, family = "binomial")
plot(cv.ridge)
cv.ridge$glmnet.fit


# ridge_val = as.data.frame( cv.ridge$glmnet.fit) ### DA PLOTTARE ###
# class( ridge_val )


# RIDGE coefficient estimates at the optimal lambda
coef(cv.ridge)

# Use the best model according to the 1-SE rule to predict on the test set and compute the AUC
ridge.probs = predict(cv.ridge, x.test, s = cv.ridge$lambda.1se, family = "binomial", type = "response")

pred_ridge = prediction(ridge.probs, y.test)

perf = performance(pred_ridge, "tpr", "fpr")
auc = as.numeric(performance(pred_ridge, "auc")@y.values)



tresholds<-seq( from = 0, to = 1, by = 0.01)

ROC_ridge = cbind( Model = 'Ridge',  
                 ROC_analysis( prediction = pred_ridge@predictions[[1]], 
                               y_true = test.wine_binary$binary_quality,
                               probability_thresholds = tresholds),
                 AUC = auc)

ROC_all = rbind( ROC_all, ROC_ridge )



ROC_matrix_ridge = ROC_analysis( prediction = pred_ridge@predictions[[1]], 
                                 y_true = test.wine_binary$binary_quality, 
                                 probability_thresholds = tresholds)

ROC_matrix_ridge = data.frame( treshold = ROC_matrix_ridge$probability_thresholds,
                             FPR = 1-ROC_matrix_ridge$`Specificity: TN/negative`, 
                             TPR = ROC_matrix_ridge$`Sensitivity (AKA Recall): TP/positive` )

roc_curve_ridge = ggplot(ROC_matrix_ridge, aes(x = FPR, y = TPR, label = treshold)) +
                  geom_line(color = 15) + theme_bw() + 
                  style_roc() + #annotate("point", x = v, y = h, colour = "white")+
                  ggtitle( "Logistic Regression - test set")


roc_curve_ridge = ggplotly( roc_curve_ridge )
save_plot( roc_curve_ridge, type = 'CLASSIFICATION')



### plot profile ###
ridge = glmnet(x.train, y.train, alpha = 0, family = "binomial")
lambda = ridge$lambda
coeffs = as.matrix( ridge$beta)
coeffs = t(coeffs)
n_col = ncol(coeffs)
df = melt(coeffs)
df = cbind( df, lambda = rep(lambda, n_col))
colnames(df) = c( "s", "Variable", "Coefficient", "lambda")
df = df[ df$lambda<10, ]
# df = df[df$Variable != 'density', ]

ridge_profile_plot = ggplot(data = df, aes( x = lambda, y = Coefficient, color = Variable)) +
  geom_line() +
  ggtitle( "Profile - Ridge" ) +
  theme(plot.title = element_text(size = 15, face = "bold"))
# ridge_profile_plot = ggplotly( ridge_profile_plot )
ridge_profile_plot = ggplotly( ridge_profile_plot ) %>% 
  layout(
    xaxis = list(range = c(-0.01,10)), 
    yaxis = list(range = c(-15, 5)))
save_plot( ridge_profile_plot, type = 'CLASSIFICATION')
################################################



# LASSO logistic regression; for this we use the default 'alpha = 1'
#######################################################################
cv.lasso = cv.glmnet(x.train, y.train, family = "binomial")
plot(cv.lasso)
coef(cv.lasso)
cv.lasso$glmnet.fit
### DA PLOTTARE ###


# Use the best model according to the 1-SE rule to predict on the test set and compute the AUC
lasso.probs = predict(cv.lasso, x.test, s = cv.lasso$lambda.1se, family = "binomial", type = "response")
pred_lasso = prediction(lasso.probs, y.test)
perf = performance(pred_lasso, "tpr", "fpr")
auc = as.numeric(performance(pred_lasso, "auc")@y.values)


tresholds<-seq( from = 0, to = 1, by = 0.01)

ROC_lasso = cbind( Model = 'Lasso',
                   ROC_analysis( prediction = pred_lasso@predictions[[1]], 
                                 y_true = test.wine_binary$binary_quality,
                                 probability_thresholds = tresholds),
                   AUC = auc)

ROC_all = rbind( ROC_all, ROC_lasso )



ROC_matrix_lasso = ROC_analysis( prediction = pred_lasso@predictions[[1]], 
                                 y_true = test.wine_binary$binary_quality, 
                                 probability_thresholds = tresholds)

ROC_matrix_lasso = data.frame( treshold = ROC_matrix_lasso$probability_thresholds,
                               FPR = 1-ROC_matrix_lasso$`Specificity: TN/negative`, 
                               TPR = ROC_matrix_lasso$`Sensitivity (AKA Recall): TP/positive` )

roc_curve_lasso = ggplot(ROC_matrix_lasso, aes(x = FPR, y = TPR, label = treshold)) +
                  geom_line(color = 15) + theme_bw() + 
                  style_roc() + #annotate("point", x = v, y = h, colour = "white")+
                  ggtitle( "Logistic Regression - test set")


roc_curve_lasso = ggplotly( roc_curve_lasso )
save_plot( roc_curve_lasso, type = 'CLASSIFICATION')


##### PLOT PROFILE #######
lasso = glmnet(x.train, y.train, alpha = 1, family = "binomial")
lambda = lasso$lambda
coeffs = as.matrix( lasso$beta)
coeffs = t(coeffs)
n_col = ncol(coeffs)
df = melt(coeffs)
df = cbind( df, lambda = rep(lambda, n_col))
colnames(df) = c( "s", "Variable", "Coefficient", "lambda")
df = df[ df$lambda<.2, ]
# df = df[df$Variable != 'density', ]

lasso_profile_plot = ggplot(data = df, aes( x = lambda, y = Coefficient, color = Variable)) +
                      geom_line() +
                      ggtitle( "Profile - Lasso" ) +
                      theme(plot.title = element_text(size = 15, face = "bold"))
lasso_profile_plot = ggplotly( lasso_profile_plot ) %>% 
  layout(
    xaxis = list(range = c(-0.01,.2)), 
    yaxis = list(range = c(-10, 5)))
save_plot( lasso_profile_plot, type = 'CLASSIFICATION')

#################################################
#rm(list=setdiff(ls(), c("test_error", 'ROC_all')))




#ELASTIC-NET
#################################################
alphas = seq(0.1, 1, by = 0.1)
lambdas = seq(0.1, 1.5, by = 0.05)
grid = expand.grid(.alpha = alphas, .lambda = lambdas)
control = trainControl(method = "cv", number = 10, savePredictions = TRUE)
elnet = train(binary_quality ~., method="glmnet", metric='Accuracy', tuneGrid = grid, 
              trControl = control, data = train.wine_binary)
plot(elnet)

elnet_data = elnet$results
elnet_data$alpha = as.factor( elnet_data$alpha )
head(elnet_data)


elnet_validation = ggplot(data = elnet_data, aes( x = lambda, group = alpha,
                                             text = paste("Kappa:", Kappa, "\n"))) +
                                          #              "Precisione nominale:", Precision) +
                          geom_line( aes( y = Accuracy, color = alpha )) +
                          geom_point( aes( y = Accuracy, color = alpha ), show.legend = F) +    
                          ggtitle( "Validazione parametri Elastic Net" ) +
                          theme(plot.title = element_text(size = 15, face = "bold"))

ply_val_elnet = ggplotly( elnet_validation ) %>%
                layout(title = "Validazione parametri Elastic Net",
                       legend = list(orientation = "v")) # , y = 0, x = 0))

save_plot( ply_val_elnet, type = "CLASSIFICATION")

elnet.probs = predict(elnet, newdata = test.wine_binary, type = "prob")

predob = prediction(elnet.probs[,2], test.wine_binary$binary_quality)
perf.el = performance(predob, "tpr", "fpr")
auc = as.numeric(performance(predob, "auc")@y.values)


ROC_elnet = cbind( Model = 'Elastic Net', 
                   ROC_analysis( prediction = elnet.probs[, 2], 
                                 y_true = test.wine_binary$binary_quality,
                                 probability_thresholds = tresholds),
                   AUC = auc)

ROC_all = rbind( ROC_all, ROC_elnet )


ROC_matrix_elnet = ROC_analysis( prediction = elnet.probs[, 2], 
                                 y_true = test.wine_binary$binary_quality, 
                                 probability_thresholds = tresholds)

ROC_matrix_elnet = data.frame( treshold = ROC_matrix_elnet$probability_thresholds,
                               FPR = 1-ROC_matrix_elnet$`Specificity: TN/negative`, 
                               TPR = ROC_matrix_elnet$`Sensitivity (AKA Recall): TP/positive` )

roc_curve_elnet = ggplot(ROC_matrix_elnet, aes(x = FPR, y = TPR, label = treshold)) +
                  geom_line(color = 15) + theme_bw() + 
                  style_roc() + #annotate("point", x = v, y = h, colour = "white")+
                  ggtitle( "Elastic Net")


roc_curve_elnet = ggplotly( roc_curve_elnet )
save_plot( roc_curve_elnet, type = 'CLASSIFICATION')
#########################################################




#GAM
#########################################################
d = seq(1,10)
grid2 = expand.grid(.df = d)
gam = train(binary_quality ~ ., method="gamSpline", metric='Accuracy', tuneGrid = grid2, 
            trControl = control, data = train.wine_binary, type = "prob")
summary(gam)
gam.coef = coef(gam$finalModel) ; gam.coef
gam.probs = predict(gam, newdata = test.wine_binary, type = "prob")
predob = prediction(gam.probs[,2], test.wine_binary$binary_quality)
auc = as.numeric(performance(predob, "auc")@y.values)

gam_data = gam$results


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

ROC_gam = cbind( Model = 'Generalised Additive Model', 
                   ROC_analysis( prediction = gam.probs[, 2], 
                                 y_true = test.wine_binary$binary_quality,
                                 probability_thresholds = tresholds),
                   AUC = auc)

ROC_all = rbind( ROC_all, ROC_gam )


ROC_matrix_gam = ROC_analysis( prediction = gam.probs[, 2], 
                                 y_true = test.wine_binary$binary_quality, 
                                 probability_thresholds = tresholds)

ROC_matrix_elnet = data.frame( treshold = ROC_matrix_gam$probability_thresholds,
                               FPR = 1-ROC_matrix_gam$`Specificity: TN/negative`, 
                               TPR = ROC_matrix_gam$`Sensitivity (AKA Recall): TP/positive` )

roc_curve_gam = ggplot(ROC_matrix_elnet, aes(x = FPR, y = TPR, label = treshold)) +
                geom_line(color = 15) + theme_bw() + 
                style_roc() + #annotate("point", x = v, y = h, colour = "white")+
                ggtitle( "Generalize Additive Model")


roc_curve_gam = ggplotly( roc_curve_gam )
save_plot( roc_curve_gam, type = 'CLASSIFICATION')
#########################################################



# K-NEAREST NEIGHBOR ------------------------
#########################################################
# In the knn approach there are no parameters; prediction is done completely at evaluation time.
# We need to specify the X in- and out-of-sample, along with the Y in-sample. The last argument is k


# KNN CARET
trctrl <- trainControl(method = "cv", number = 20)
set.seed(3333)
data<-as.data.frame(cbind(train.wine_binary[,-13],as.factor(train.wine_binary$binary_quality)))
colnames(data)<-colnames(train.wine_binary)
knn_fit <- train(binary_quality~., data = data, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 100)

plot(knn_fit,xlab="# Neighbours")

cv_df = knn_fit$results
cv_df$Accuracy_lower = cv_df$Accuracy - cv_df$AccuracySD
cv_df$Accuracy_upper = cv_df$Accuracy + cv_df$AccuracySD

cv_df$Kappa_lower = cv_df$Kappa - cv_df$KappaSD
cv_df$Kappa_upper = cv_df$Kappa + cv_df$KappaSD

knn_acc  = ggplot(data = cv_df, aes(x = k, y = Accuracy)) + 
           geom_point(col = 'royalblue2') + geom_line(col = 'cyan2') +
           geom_ribbon(aes(ymin=cv_df$Accuracy_lower, ymax=cv_df$Accuracy_upper), linetype=2, alpha=0.1)
knn_acc = ggplotly( knn_acc )

knn_kappa = ggplot(data = cv_df, aes(x = k)) + 
            geom_line( aes( y = Kappa), col = 'olivedrab3') + 
            geom_point( aes( y = Kappa ), col = 'limegreen') +
            geom_ribbon(aes(ymin=cv_df$Kappa_lower, ymax=cv_df$Kappa_upper), linetype=2, alpha=0.1)
knn_kappa = ggplotly( knn_kappa )

knn_cv_plot = subplot( knn_acc, knn_kappa ) %>% 
              layout(title = "Validazione parametri knn - Accuratezza vs Kappa", legend = list(orientation = "v"))


save_plot( knn_cv_plot, type = 'CLASSIFICATION')
#################################################



knn_probs = predict(knn_fit, test.wine_binary[,-13], type = "prob")
knn_probs = knn_probs[, 2]


pred_knn = prediction(knn_probs, test.wine_binary[,13])
perf = performance(pred_knn, "tpr", "fpr")
auc = as.numeric(performance(pred_lasso, "auc")@y.values)




tresholds<-seq( from = 0, to = 1, by = 0.01)

ROC_knn = cbind( Model = 'K nearest neighbour', 
                 ROC_analysis( prediction = knn_probs, 
                               y_true = test.wine_binary$binary_quality,
                               probability_thresholds = tresholds),
                 AUC = auc )

ROC_all = rbind( ROC_all, ROC_knn )



ROC_matrix_knn = ROC_analysis( prediction = knn_probs, 
                               y_true = test.wine_binary$binary_quality, 
                               probability_thresholds = tresholds)

ROC_matrix_knn = data.frame( treshold = ROC_matrix_knn$probability_thresholds,
                               FPR = 1-ROC_matrix_knn$`Specificity: TN/negative`, 
                               TPR = ROC_matrix_knn$`Sensitivity (AKA Recall): TP/positive` )

roc_curve_knn = ggplot( ROC_matrix_knn, aes(x = FPR, y = TPR, label = treshold)) +
                        geom_line(color = 15) + theme_bw() + 
                        style_roc() + #annotate("point", x = v, y = h, colour = "white")+
                        ggtitle( 'knn')


roc_curve_knn = ggplotly( roc_curve_knn )
save_plot( roc_curve_knn, type = 'CLASSIFICATION')
#################################################


# ********** Saving file ******************* #
save_plot( ROC_all, type = "CLASSIFICATION")
################################################


ROC_best = ROC_all %>%
                    group_by(Model) %>%
                    slice(which.max(`Accuracy: true/total`))

ROC_best = remove_columns_by_names( ROC_best, colNames = c( "(y=1,y_hat=1) TP" , "(y=1,y_hat=0) FN",
                                                            "(y=0,y_hat=1) FP", "(y=0,y_hat=0) TN",
                                                            "Positive Predictive Value: TP/predicted_positive",
                                                            "Positive Likelihood Ratio", "F1_SCORE" ))

# ********** Saving file ******************* #
save_table( ROC_best, type = 'CLASSIFICATION')
################################################


######################################
ROC_all$FPR = 1 - ROC_all$`Specificity: TN/negative`
ROC_all$TPR = ROC_all$`Sensitivity (AKA Recall): TP/positive`


roc_curve_all = ggplot(data = ROC_all, aes( x = FPR, y = TPR, color = Model, 
                                               text = paste("AUC:", AUC, "\n"))) +
                geom_line() +
                ggtitle( "ROC CURVE ALL" ) +
                theme(plot.title = element_text(size = 15, face = "bold"))


roc_curve_all = ggplotly( roc_curve_all )
save_plot( roc_curve_all, type = 'CLASSIFICATION')
#################################################


cat('\n\n SCRIPT ESEGUITO CORRETTAMENTE!! \n\n')
