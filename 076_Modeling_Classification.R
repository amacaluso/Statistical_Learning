### ***** IMPORT ***** ###
##########################
source( '072_Modeling_lpm.R') # REQUIRE SEED
source( '074_Modeling_lda.R') # REQUIRE SEED



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

#variable importance

# group_distances<-sort(abs(diff(qda.fit$means)))
# names(group_distances)<-colnames(diff(qda.fit$means))[as.vector(order((abs(diff(qda.fit$means)))))]
# group_distances
# 
# var_importance<-sort(abs(diff(qda.fit$means))/coeff_var)
# names(var_importance)<-colnames(diff(qda.fit$means))[as.vector(order((abs(diff(qda.fit$means))/coeff_var)))]
# var_importance


# Predict the qda fit on the test sample

qda.pred = predict(qda.fit, newdata = test.wine_binary)

# Confusion matrix

table(true = test.wine_binary$binary_quality, predict = qda.pred$class)

# Total success rate

mean(qda.pred$class == test.wine_binary$binary_quality)

test_error = c(test_error, 1-mean(qda.pred$class == test.wine_binary$binary_quality))
names(test_error)[3]="qda"
test_error

# Test set ROC curve and AUC.

predob = prediction(qda.pred$posterior[ ,2], test.wine_binary$binary_quality)
perf = performance(predob, "tpr", "fpr")
plot(perf, main = "Quadratic Discriminant Analysis - test set", colorize = TRUE,
     print.cutoffs.at = seq(0, 1, by = 0.1), text.adj = c(-0.2, 1.7))
auc = c(auc, as.numeric(performance(predob, "auc")@y.values))
names(auc)[3] = "qda"
auc




tresholds<-seq( from = 0, to = 1, by = 0.01)

ROC_qda = cbind( Model = 'Quadratic_Discriminant_Analysis', 
                 ROC_analysis( prediction = qda.pred$posterior[,2], 
                               y_true = test.wine_binary$binary_quality,
                               probability_thresholds = tresholds))

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

# rm(list=setdiff(ls(), c("test_error", 'ROC_all')))




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

# Test sample confusion matrix
table(true = test.wine_binary$binary_quality, predict = glm.pred)

# Test sample total success rate
mean(glm.pred == test.wine_binary$binary_quality)

test_error = c(test_error, 1-mean(glm.pred == test.wine_binary$binary_quality))
names(test_error)[4]="logreg"
test_error

# Test set ROC curve and AUC

predob = prediction(glm.probs, test.wine_binary$binary_quality)
perf = performance(predob, "tpr", "fpr")
par(mfrow = c(1, 1))
plot(perf, main = "Logistic Regression -  test set", colorize = TRUE,
     print.cutoffs.at = seq(0, 1, by = 0.1), text.adj = c(-0.2, 1.7))
auc = c(auc, as.numeric(performance(predob, "auc")@y.values))
names(auc)[4] = "logreg"
auc




tresholds<-seq( from = 0, to = 1, by = 0.01)

ROC_log = cbind( Model = 'Logistic_Regression', 
                 ROC_analysis( prediction = glm.probs, 
                               y_true = test.wine_binary$binary_quality,
                               probability_thresholds = tresholds))

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

predob = prediction(ridge.probs, y.test)

# Test sample total success rate
mean(round(predob@predictions[[1]]) == test.wine_binary$binary_quality)

test_error = c(test_error, 1-mean(round(predob@predictions[[1]]) == test.wine_binary$binary_quality))
names(test_error)[5]="ridge"
test_error

perf = performance(predob, "tpr", "fpr")
par(mfrow = c(1, 1))
plot(perf, main = "Ridge", colorize = TRUE,
     print.cutoffs.at = seq(0, 1, by = 0.1), text.adj = c(-0.2, 1.7))
auc = c(auc, as.numeric(performance(predob, "auc")@y.values))
names(auc)[5] = "ridge"
auc



tresholds<-seq( from = 0, to = 1, by = 0.01)

ROC_ridge = cbind( Model = 'Regularized_Logistic_Regression', 
                 ROC_analysis( prediction = predob@predictions[[1]], 
                               y_true = test.wine_binary$binary_quality,
                               probability_thresholds = tresholds))

ROC_all = rbind( ROC_all, ROC_ridge )



ROC_matrix_ridge = ROC_analysis( prediction = predob@predictions[[1]], 
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

predob = prediction(lasso.probs, y.test)

# Test sample total success rate
mean(round(predob@predictions[[1]]) == test.wine_binary$binary_quality)

test_error = c(test_error, 1-mean(round(predob@predictions[[1]]) == test.wine_binary$binary_quality))
names(test_error)[6]="lasso"
test_error

perf = performance(predob, "tpr", "fpr")
par(mfrow = c(1, 1))
plot(perf, main = "Lasso", colorize = TRUE,
     print.cutoffs.at = seq(0, 1, by = 0.1), text.adj = c(-0.2, 1.7))
auc = c(auc, as.numeric(performance(predob, "auc")@y.values))
names(auc)[6] = "lasso"
auc




tresholds<-seq( from = 0, to = 1, by = 0.01)

ROC_ = cbind( Model = 'Regularized_Logistic_Regression', 
                   ROC_analysis( prediction = predob@predictions[[1]], 
                                 y_true = test.wine_binary$binary_quality,
                                 probability_thresholds = tresholds))

ROC_all = rbind( ROC_all, ROC_ridge )



ROC_matrix_ridge = ROC_analysis( prediction = predob@predictions[[1]], 
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

#rm(list=setdiff(ls(), c("test_error", 'ROC_all')))



# K-NEAREST NEIGHBOR -----------------------------------------------------
#########################################################
# In the knn approach there are no parameters; prediction is done completely at evaluation time.
# We need to specify the X in- and out-of-sample, along with the Y in-sample. The last argument is k

X = as.matrix(wine_binary[, -13])
X.train = X[train.label,]
X.test = X[!train.label,]
Y = wine_binary$binary_quality
Y.train = Y[train.label]
Y.test = Y[!train.label]

# Fit knn on the training sample. We will consider a sequence of fits, with K ranging between 1 and 100, with unit steps

K.vec = seq(from = 1, to = 50)

# We write a function which, for each K: 
# 1) fits the model, 
# 2) computes the test data success rate

sr.vec = rep(0, times = length(K.vec))
knn.sr = function(K){
  knn.pred = knn(X.train, X.test, Y.train, k = K, prob = TRUE)
  return(mean(knn.pred == Y.test))
}

# Now, set up a loop to evaluate 'knn.sr' on all the elements of K.vec, and store the results in 'sr.vec'

for (h in 1:10){
  
  # set.seed(abs(rnorm(n = 1)))
  rand_prop=4/5#runif(min = 0, max = 1, n = 1)
  seed_sampling=abs(rnorm(n = 1,mean = rpois(lambda = 46,n = 1),sd = 231))
  set.seed(seed_sampling)
  train.label = sample.split(wine_binary, SplitRatio = rand_prop)
  
  # Check that the split is balanced
  
  train.wine_binary = wine_binary[train.label, ]
  train.wine = wine[train.label, ]
  
  test.wine_binary = wine_binary[!train.label,]
  test.wine = wine[!train.label,]
  
  X = as.matrix(wine_binary[, -13])
  X.train = X[train.label,]
  X.test = X[!train.label,]
  Y = wine_binary$binary_quality
  Y.train = Y[train.label]
  Y.test = Y[!train.label]
  
  seed_knn=runif(min = 1, max = 1000, n = 1)
  set.seed(seed_knn) #needed for ties in majority votes
  if(get_os()=="linux")
    sr.vec <- lapply(K.vec,knn.sr) else 
      sr.vec <- mclapply(K.vec,knn.sr,mc.cores = 1)
  
  # knn.sr1 = function(K){
  #   knn.pred = knn(X.train, X.test, Y.train, k = K)
  #   return(c(K, mean(knn.pred == Y.test)))
  # }
  # 
  # set.seed(12344321)
  # sr.vec1 <- lapply(K.vec,knn.sr1)
  # 
  # for (i in 1:length(sr.vec)){
  #   if (sr.vec1[[i]][2]!=sr.vec[[i]])
  #     print(i)
  # }
  
  # for(i in 1:length(K.vec)) {
  #   sr.vec[i] = knn.sr(K.vec[i])
  # }
  
  # Finally, plot the success rate as a function of K
  
  par(mfrow = c(1, 1))
  plot(K.vec, sr.vec,type="l",main=paste0("Iterantion ",h))
  # Notice the 'inverse U-shape'. K between 10 and 20 seems optimal
  
  k.min = min(which.max(sr.vec))
  print(paste0("Iteration n. ",h))
  print(paste0("random seed for sampling: ",seed_sampling))
  print(paste0("random proprotion: ",rand_prop))
  print(paste0("random seed for knn: ",seed_knn))
  print(paste0("Min k neighbour: ",k.min))
  print(paste0("Accuracy: ",sr.vec[k.min]))
}

# [1] "Iteration n. 14"
# [1] "random seed for sampling: 95.500383694519"
# [1] "random proprotion: 0.8"
# [1] "random seed for knn: 450.110531971557"
# [1] "Min k neighbour: 1"
# [1] "Accuracy: 0.762349799732977"

seed_knn=runif(min = 1, max = 1000, n = 1)
set.seed(seed_knn) #needed for ties in majority votes
#if(get_os()=="linux")
sr.vec <- lapply(K.vec,knn.sr)
#    sr.vec <- mclapply(K.vec,knn.sr,mc.cores = 4)

k.min = min(which.max(sr.vec))

# Finally, plot the success rate as a function of K

par(mfrow = c(1, 1))
plot(K.vec, sr.vec,type="l",main=paste0("Iteration ",h))


test_error = c(test_error, 1 - sr.vec[k.min][[1]])
names(test_error)[7]="knn"
test_error

# Consider the best model, with K = k.min 
# Does it fit the data adequately? The answer is NO. 
# Its test set error rate is roughly the same as the 
# unconditional model one (approximately 80%).
# To check this, we refit the model using the optional argument 
# 'prob = TRUE' in the knn function call to retrieve 
# fitted probabilities.

knn.pred = knn(X.train, X.test, Y.train, k = k.min, prob = TRUE)
knn.probs = attributes(knn.pred)$prob
table(knn.probs)

# Draw a histogram of the probabilities

hist(knn.probs)

table(true = Y.test, predict = knn.pred)

# Another way to see how awful the 'best' knn fit is is to 
# plot the test set ROC curve

predob = prediction(knn.probs, Y.test)

perf = performance(predob, "tpr", "fpr")
par(mfrow = c(1, 1))
plot(perf, main = "KNN with K = 1", colorize = TRUE,
     print.cutoffs.at = seq(0, 1, by = 0.1), text.adj = c(-0.2, 1.7))

# It looks a lot like random guessing. 
# Compute the AUC

auc = c(auc, as.numeric(performance(predob, "auc")@y.values))
names(auc)[7] = "knn"
auc



# Random guessing would get AUC = 0.5.
# Why does knn perform so badly? Easy: curse of dimensionality!
# 8 predictors are too many (especially since the number of
# training observations is just 600). 


###TRY MAHALANOBIS DISTANCE
# if (!require(knnGarden)) {
#   install.packages("knnGarden")
#   library(knnGarden)
# }
# 
# mhs.knn.sr = function(K){
#   mhs.knn.pred = knnMCN(train.wine_binary[,-13], train.wine_binary[,13], TstX = test.wine_binary[,13], K = 1, ShowObs = F)
#   return(mean(mhs.knn.pred == Y.test))
# }
# 
# seed_mhs.knn=runif(min = 1, max = 1000, n = 1)
# set.seed(seed_mhs.knn) #needed for ties in majority votes
# if(get_os()=="linux")
#   mhs.knn <- lapply(K.vec,knn.sr) else 
#     mhs.knn <- mclapply(K.vec,knn.sr,mc.cores = 4)
# knnMCN(train.wine_binary[,-13], train.wine_binary[,13], TstX = test.wine_binary[,13], K = 1, ShowObs = F)
# 
# 

train_matrix<-as.matrix(train.wine_binary[,-13])
test_matrix<-as.matrix(test.wine_binary[,-13])
tr_cov<-cov(train_matrix,method = "pearson")


MD_matrix<-mclapply(1:nrow(test_matrix),MD,mc.cores = 1)

length(MD_matrix)
dim(train_matrix)
dim(test_matrix)

length(MD_matrix[[1]])

MD_retrive_neighbors<-function(row,n_neigh){
  # n_neigh<-k
  ordered_indexes<-order(MD_matrix[[row]])
  k_neighbors<-ordered_indexes[1:n_neigh]
}

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

MD_predict<-function(row,k_neigh){
  votes<-train.wine_binary$binary_quality[c(k_neigh[[row]])]
  getmode(votes)
}

MD_knn<-function(k){
  #n_neigh<-k
  
  k_neigh<-mclapply(1:nrow(test_matrix),MD_retrive_neighbors,k)
  y.hat<-sapply(1:nrow(test_matrix),MD_predict,k_neigh,simplify = T)
  mean(y.hat==test.wine_binary$binary_quality)
  # accuracy=0
  # for (i in 1:nrow(test_matrix))
  #   accuracy =  accuracy + ifelse(y.hat[[i]]==test.wine_binary$binary_quality[i],1,0)
  # accuracy/nrow(test_matrix)
}

sr.vec<-sapply(1:100,MD_knn,simplify = T)

k.min<-which.min(sr.vec)

test_error = c(test_error, 1 - sr.vec[k.min])
names(test_error)[8]="mahaknn"
test_error

plot(sr.vec,type="l", xlab="k", ylab="accuracy", main ="KNN - Mahalanobis distance")
abline(v=which.max(sr.vec),col="red",lty=2)

# KNN CARET


trctrl <- trainControl(method = "cv", number = 20)
set.seed(3333)
data<-as.data.frame(cbind(train.wine_binary[,-13],as.factor(train.wine_binary$binary_quality)))
colnames(data)<-colnames(train.wine_binary)
knn_fit <- train(binary_quality~., data = data, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)

knn_fit
plot(knn_fit)
predob1 = predict(knn_fit, test.wine_binary[,-13])

# Test sample total success rate
mean(predob1 == test.wine_binary$binary_quality)

pred_knn = predict(knn_fit, test.wine_binary[,-13], type = "prob")
pred_knn = pred_knn[, 2]


tresholds<-seq( from = 0, to = 1, by = 0.01)

ROC_knn = cbind( Model = 'k nearest neighbor', 
              ROC_analysis( prediction = pred_knn, 
                            y_true = test.wine_binary$binary_quality,
                            probability_thresholds = tresholds))

ROC_all = rbind( ROC_all, ROC_knn )



ROC_matrix_knn = ROC_analysis( prediction = pred_knn, 
                               y_true = test.wine_binary$binary_quality, 
                               probability_thresholds = tresholds)

ROC_matrix_knn = data.frame( treshold = ROC_matrix_knn$probability_thresholds,
                               FPR = 1-ROC_matrix_knn$`Specificity: TN/negative`, 
                               TPR = ROC_matrix_knn$`Sensitivity (AKA Recall): TP/positive` )

roc_curve_knn = ggplot(ROC_matrix_knn, aes(x = FPR, y = TPR, label = treshold)) +
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



tapply( X = ROC_all$`Accuracy: true/total`,
        INDEX = ROC_all$Model, FUN = max )

ROC_all$FPR = ROC_all$`(y=0,y_hat=1) FP`/(ROC_all$`(y=0,y_hat=0) TN`+ROC_all$`(y=0,y_hat=1) FP`)
ROC_all$TPR = ROC_all$`Sensitivity (AKA Recall): TP/positive`


roc_curve_all = ggplot( ROC_all, aes(x = FPR, y = TPR, label = probability_thresholds), alpha = 0.2) +
  geom_line(data = subset(ROC_all, Model == 'Linear_Probability_Model'), col = 2) +
  geom_line(data = subset(ROC_all, Model == 'Linear_Discriminant_Analysis'), col = 3) +
  geom_line(data = subset(ROC_all, Model == 'Quadratic_Discriminant_Analysis'), col = 4) +
  geom_line(data = subset(ROC_all, Model == 'Logistic_Regression'), col = 5) +
  geom_line(data = subset(ROC_all, Model == 'Regularized_Logistic_Regression'), col = 6) +
  geom_line(data = subset(ROC_all, Model == 'Regularized_Logistic_Regression (Lasso)'), col = 7) +
  geom_line(data = subset(ROC_all, Model == 'k nearest neighbor'), col = 8) +
  ggtitle( "ROC ANALYSIS ALL")  +
  style_roc()


roc_curve_all = ggplotly( roc_curve_all )
roc_curve_all

# ********** Saving file ******************* #
file_name = paste0( folder, "/roc_curve_all.Rdata")
save( roc_curve_all, file = file_name)
################################################
