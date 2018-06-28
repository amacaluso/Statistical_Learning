### ***** IMPORT ***** ###
##########################

SEED = 12344321
source( '072_Modeling_lpm.R') # REQUIRES SEED

SEED = 12344321
source( '074_Modeling_lda.R') # REQUIRES SEED



source( 'Utils.R')
SEED = 12344321
source( '020_Pre_processing.R') # REQUIRE SEED



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


# Test set ROC curve and AUC.
pred_qda = prediction(qda.pred$posterior[ ,2], test.wine_binary$binary_quality)
perf = performance(pred_qda, "tpr", "fpr")

auc = as.numeric(performance(pred_qda, "auc")@y.values)


tresholds<-seq( from = 0, to = 1, by = 0.01)
ROC_qda = cbind( Model = 'Quadratic_Discriminant_Analysis', 
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
roc_curve_qda

# ********** Saving file ******************* #
file_name = paste0( folder, "/qda_roc_curve.Rdata")
save( roc_curve_qda, file = file_name)
################################################

#rm(list=setdiff(ls(), 'ROC_all'))




# LOGISTIC REGRESSION -----------------------------------------------------
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

ROC_log = cbind( Model = 'Logistic_Regression', 
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
roc_curve_log

# ********** Saving file ******************* #
file_name = paste0( folder, "/log_roc_curve.Rdata")
save( roc_curve_log, file = file_name)
################################################

# rm(list=setdiff(ls(), c("test_error", 'ROC_all')))


# REGULARIZED LOGISTIC REGRESSION -------------------------------------------
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

# RIDGE coefficient estimates at the optimal lambda
coef(cv.ridge)

# Use the best model according to the 1-SE rule to predict on the test set and compute the AUC
ridge.probs = predict(cv.ridge, x.test, s = cv.ridge$lambda.1se, family = "binomial", type = "response")

pred_ridge = prediction(ridge.probs, y.test)

perf = performance(pred_ridge, "tpr", "fpr")
auc = as.numeric(performance(pred_ridge, "auc")@y.values)



tresholds<-seq( from = 0, to = 1, by = 0.01)

ROC_ridge = cbind( Model = 'Regularized_Logistic_Regression', 
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
roc_curve_ridge

# ********** Saving file ******************* #
file_name = paste0( folder, "/ridge_roc_curve.Rdata")
save( roc_curve_ridge, file = file_name)
################################################






# LASSO logistic regression; for this we use the default 'alpha = 1'
#######################################################################
cv.lasso = cv.glmnet(x.train, y.train, family = "binomial")
plot(cv.lasso)
coef(cv.lasso)

# Use the best model according to the 1-SE rule to predict on the test set and compute the AUC
lasso.probs = predict(cv.lasso, x.test, s = cv.lasso$lambda.1se, family = "binomial", type = "response")

pred_lasso = prediction(lasso.probs, y.test)

perf = performance(pred_lasso, "tpr", "fpr")

auc = as.numeric(performance(pred_lasso, "auc")@y.values)




tresholds<-seq( from = 0, to = 1, by = 0.01)

ROC_lasso = cbind( Model = 'Regularized_Logistic_Regression (LASSO)', 
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
roc_curve_lasso

# ********** Saving file ******************* #
file_name = paste0( folder, "/roc_curve_lasso.Rdata")
save( roc_curve_lasso, file = file_name)
################################################

#rm(list=setdiff(ls(), c("test_error", 'ROC_all')))



# K-NEAREST NEIGHBOR -----------------------------------------------------
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
                 tuneLength = 10)

plot(knn_fit)

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

knn_cv_plot = subplot( knn_acc, knn_kappa )

# ********** Saving file ******************* #
file_name = paste0( folder, "/knn_cv_plot.Rdata")
save( knn_cv_plot, file = file_name)
################################################



knn_probs = predict(knn_fit, test.wine_binary[,-13], type = "prob")
knn_probs = knn_probs[, 2]


pred_knn = prediction(knn_probs, test.wine_binary[,13])
perf = performance(pred_knn, "tpr", "fpr")
auc = as.numeric(performance(pred_lasso, "auc")@y.values)




tresholds<-seq( from = 0, to = 1, by = 0.01)

ROC_knn = cbind( Model = 'k nearest neighbor', 
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
roc_curve_knn

# ********** Saving file ******************* #
file_name = paste0( folder, "/knn_roc_curve.Rdata")
save( roc_curve_knn, file = file_name)
################################################


# ********** Saving file ******************* #
file_name = paste0( folder, "/ROC_all.Rdata")
save( ROC_all, file = file_name)
################################################


ROC_best = ROC_all %>%
                    group_by(Model) %>%
                    slice(which.max(`Accuracy: true/total`))

# ********** Saving file ******************* #
file_name = paste0( folder, "/ROC_best.Rdata")
save( ROC_best, file = file_name)
################################################


ROC_matrix_lasso = data.frame( treshold = ROC_matrix_lasso$probability_thresholds,
                               FPR = 1-ROC_matrix_lasso$`Specificity: TN/negative`, 
                               TPR = ROC_matrix_lasso$`Sensitivity (AKA Recall): TP/positive` )

ROC_all$FPR = 1 - ROC_all$`Specificity: TN/negative`
ROC_all$TPR = ROC_all$`Sensitivity (AKA Recall): TP/positive`

roc_curve_all = ggplot( ROC_all, aes(x = FPR, y = TPR, label = probability_thresholds, text = Model), alpha = 0.2) +
                        geom_line(data = subset(ROC_all, Model == 'Linear_Probability_Model'), col = 2) +
                        geom_line(data = subset(ROC_all, Model == 'Linear_Discriminant_Analysis'), col = 3) +
                        geom_line(data = subset(ROC_all, Model == 'Quadratic_Discriminant_Analysis'), col = 4) +
                        geom_line(data = subset(ROC_all, Model == 'Logistic_Regression'), col = 5) +
                        geom_line(data = subset(ROC_all, Model == 'Regularized_Logistic_Regression'), col = 6) +
                        geom_line(data = subset(ROC_all, Model == 'Regularized_Logistic_Regression (LASSO)'), col = 7) +
                        geom_line(data = subset(ROC_all, Model == 'k nearest neighbor'), col = 8) +
                        ggtitle( "ROC ANALYSIS ALL")  
roc_curve_all = ggplotly( roc_curve_all )
roc_curve_all



# ********** Saving file ******************* #
file_name = paste0( folder, "/roc_curve_all.Rdata")
save( roc_curve_all, file = file_name)
################################################
