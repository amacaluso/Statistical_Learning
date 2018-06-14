source('Utils.R')
source('010_Data_preparation.R')
 



options( scipen = 999)

linear_regression = step( lm( alcohol ~ ., data = train_dataset ), direction = 'both' , trace = 5000)
summary( linear_regression )
target_variable = "alcohol"

for(variable in colnames(dataset))
  {
#    variable = colnames(dataset)[ 3 ]
    current_dataset = dataset[ , c(target_variable, variable) ] 
    fit <- lm(alcohol ~ . , data = current_dataset)
    
    ggplotRegression( fit, variable)
     }




# We will use the package 'glmnet', which does not use 
# the model formula language, so we will set up a 'x' and 'y'. 
# First, load the package


if (!require(glmnet)){
  install.packages("glmnet")
  library(glmnet)
}

# 'glmnet' won't work with the usual formula. Instead, 
# you need to supply a matrix of predictors, 'x', and 
# a vector of outcomes, 'y'. The next code snippet sets them up. 
# Notice that we drop the constant from 'x'

x = model.matrix(alcohol ~ . -1, data = dataset) 
head(x)
y = dataset$alcohol

# Let's make again a training and test set

# x.train = x[prostate[, "train"], ]
# y.train = y[prostate[, "train"]]
# x.test = x[!prostate[, "train"], ]
# y.test = y[!prostate[, "train"]]

# We now fit a ridge regression model. This is 
# achieved by calling 'glmnet' with 'alpha = 0' (see 
# the helpfile)

fit.ridge = glmnet(x, y, alpha = 0)
plot(fit.ridge, xvar = "lambda", label = TRUE)
fit.ridge

# 'fit.ridge' prints a three columns matrix: 
# 'Df', '%Dev' and 'Lambda'. Each row corresponds to a 
# different model, one for each Lambda value. There are 
# 100 of them

# There is also a 'cv.glmnet' function which will do 
# 10-fold CV on 'lambda'

cv.ridge = cv.glmnet(x, y, alpha = 0)
plot(cv.ridge)

# Ridge coefficient estimates at the optimal (i.e., 1-SE) 
# lambda

coef(cv.ridge)

# Use the best model according to the 1-SE rule to predict 
# on the test set and compute the MSE

pred.ridge = predict(cv.ridge, x, s = cv.ridge$lambda.1se)
MSPE.TE.ridge = mean((pred.ridge-y)^2)
MSPE = c(MSPE, MSPE.TE.ridge)
names(MSPE)[7] = "TE.ridge"
MSPE

### Lasso regression

# We now fit a LASSO model; for this we use the default 
# 'alpha = 1'

fit.lasso = glmnet(x, y)
plot(fit.lasso, xvar = "lambda", label = TRUE)
fit.lasso

# Again, CV can be done automatically using 'cv.glmnet'

cv.lasso = cv.glmnet(x, y)
plot(cv.lasso)
coef(cv.lasso)

# Which is the best model according to the 1-SE rule?

i.best = which(fit.lasso$lambda == cv.lasso$lambda.1se)
i.best

# How many nonzero parameters does it contain?

fit.lasso$df[i.best]

# Use the best model according to the 1-SE rule to predict 
# on the test set and compute the MSE

pred.lasso = predict(fit.lasso, x.test, s = cv.lasso$lambda.1se)
MSPE.TE.lasso = mean((pred.lasso-y.test)^2)
MSPE = c(MSPE, MSPE.TE.lasso)
names(MSPE)[8] = "TE.lasso"
MSPE










# fit1 <- lm(alcohol ~ free.sulfur.dioxide, data = current_dataset)
# ggplotRegression(fit1, "free.sulfur.dioxide")
# 
# fit = fit1
# 
#   
# summary(linear_regression)
# 
# linear_regression$coefficients
# 
# y_hat = predict.glm( object = linear_regression, newdata = test_dataset )
# model = "LINEAR_REGRESSION_SEMPLICE", VARIABLE
# 
# 
# evaluation_model( target_variable = test_dataset$alcohol, prediction = y_hat, MODEL = model)







x_train = model.matrix( quality ~ . , data = train_dataset) 
head(x_train)
y_train = train_dataset$quality 

fit_ridge = glmnet(x_train, y_train, alpha = 0)
plot(fit_ridge, xvar = "lambda", label = TRUE)
fit_ridge


cv_ridge = cv.glmnet(x_train, y_train, alpha = 0)
plot(cv_ridge)

# Ridge coefficient estimates at the optimal (i.e., 1-SE) 
# lambda

coef(cv.ridge)

# Use the best model according to the 1-SE rule to predict 
# on the test set and compute the MSE

pred.ridge = predict(cv.ridge, x.test, s = cv.ridge$lambda.1se)
MSPE.TE.ridge = mean((pred.ridge-y.test)^2)
MSPE = c(MSPE, MSPE.TE.ridge)
names(MSPE)[7] = "TE.ridge"
MSPE



######################################################################################
#**************************** Linear Discriminant Analysis **************************#
######################################################################################

if (!require(MASS)) {
  install.packages("MASS")
  library(MASS)
}

dataset$binary_quality = ifelse( dataset$quality>5, yes = 1,  no = 0 )
seed = 100
set.seed( seed ) 

sample_id <- sample.int(n = nrow( dataset ), size = floor(0.80 * nrow( dataset )), replace = F)
train_dataset <- dataset[ sample_id , ]
test_dataset  <- dataset[ -sample_id , ]

### BINARY QUALITY ###
lda_fit = lda(binary_quality ~ ., data = remove_columns_by_names(train_dataset, "quality"))

# Summary of results

lda_fit

# Histograms of discriminant function values by class

plot(lda_fit)

# Predict the lda fit on the test sample

lda_pred = predict(lda_fit, newdata = test_dataset)
prob_1 = lda_pred$posterior[, 2]
Y_true = test_dataset$binary_quality
ROC_matrix = ROC_analysis( prob_1,  Y_true)

library(ROCR)

predob = prediction(prob_1, Y_true)
perf = performance(predob, "tpr", "fpr")
par(mfrow = c(1, 1))
plot(perf, main = "LDA", colorize = TRUE)

###############################################################
###############################################################
###############################################################


if (!require(MASS)) {
  install.packages("MASS")
  library(MASS)
}


dataset$binary_quality = ifelse( dataset$quality>5, yes = 1,  no = 0 )
seed = 100
set.seed( seed ) 

sample_id <- sample.int(n = nrow( dataset ), size = floor(0.80 * nrow( dataset )), replace = F)
train_dataset <- dataset[ sample_id , c(11,2,14) ]
test_dataset  <- dataset[ -sample_id , c(11,2, 14) ]

### BINARY QUALITY ###
lda_fit = lda(binary_quality ~ ., data = train_dataset)


np <- 10
nd.x <- seq(from = min(test_dataset$alcohol), to = max(test_dataset$alcohol), length.out = np)
nd.y <- seq(from = min(test_dataset$volatile.acidity), to = max(test_dataset$volatile.acidity), length.out = np)
nd <- expand.grid(x = nd.x, y = nd.y)

prd <- as.numeric(predict(lda_fit, newdata = test_dataset)$class)

plot(test_dataset[, 1:2], col = factor(test_dataset$binary_quality))
points(lda_fit$means, pch = "+", cex = 3, col = c("black", "red"))
contour(x = nd.x, y = nd.y, z = matrix(prd, nrow = np, ncol = np), 
        levels = c(1, 2), add = TRUE, drawlabels = FALSE)



# Summary of results

coefficients = lda_fit$scaling
retta = as.matrix(test_dataset[, c(1,2)])%*%coefficients
# Histograms of discriminant function values by class

plot(lda_fit)
plot(test_dataset[, 1:2], col = factor(test_dataset$binary_quality))
lines(retta)
points(lda_fit$means, pch = "+", cex = 3, col = c("black", "red"))
contour(x = nd.x, y = nd.y, z = matrix(prd, nrow = np, ncol = np), 
        levels = c(1, 2), add = TRUE, drawlabels = FALSE)



p = predict(lda_fit, newdata = test_dataset)
nd.x <- seq(from = min(test_dataset$fixed.acidity), to = max(test_dataset$fixed.acidity), length.out = np)
nd.y <- seq(from = min(test_dataset$volatile.acidity), to = max(test_dataset$volatile.acidity), length.out = np)
nd <- expand.grid(x = nd.x, y = nd.y)
prd <- as.numeric(predict(lda_fit, newdata = test_dataset)$class)

contour(x = nd.x, y = nd.y, z = matrix(prd, nrow = np, ncol = np), 
        levels = c(1, 2), add = TRUE, drawlabels = FALSE)


# Predict the lda fit on the test sample

lda_pred = predict(lda_fit, newdata = test_dataset)
prob_1 = lda_pred$posterior[, 2]
Y_true = test_dataset$binary_quality
ROC_matrix = ROC_analysis( prob_1,  Y_true)

library(ROCR)

predob = prediction(prob_1, Y_true)
perf = performance(predob, "tpr", "fpr")
par(mfrow = c(1, 1))
plot(perf, main = "LDA", colorize = TRUE)









if (!require(MASS)) {
  install.packages("MASS")
  library(MASS)
}


train_dataset$binary_quality = as.factor(ifelse( train_dataset$quality > 5, yes = "good", no = "bad"  ))
test_dataset$binary_quality = as.factor( ifelse( test_dataset$quality > 5, yes = "good", no = "bad"  ))

# Even though LDA assumes normal predictors, we will still use 
# Fit the model on the training sample 
predictors = c("pH", "sulphates")
target_variable = "binary_quality"

current_tr_set = train_dataset[, c(predictors, target_variable) ]
current_ts_set = test_dataset[, c(predictors, target_variable) ]


lda_fit = lda( binary_quality ~ ., data = current_tr_set)

# Summary of results
lda_fit

# Histograms of discriminant function values by class
plot(lda_fit)

# Predict the lda fit on the test sample
lda_pred = predict(lda_fit, newdata = current_ts_set)

# Attributes in lda.pred
names(lda_pred)

# Display the first 25 predicted classes
head(lda_pred$class, 10)
table(lda_pred$class)

# Display first 10 posterior probabilities
head(lda_pred$posterior, 10)

par( mfrow = c(2, 1) )
plot( current_tr_set[, predictors[1]], current_tr_set[, predictors[2]],
      col = factor(current_tr_set[, target_variable]))

plot( current_ts_set[, predictors[1]], current_ts_set[, predictors[2]],
      col = factor(current_ts_set[, target_variable]))


# http://suanfazu.com/t/linear-discriminant-analysis-plot-projections-of-data-points-in-ggplot2/3016

# Intercept and slope of decision boundary
gmean <- lda_fit$prior %*% lda_fit$means
const <- as.numeric( gmean %*% lda_fit$scaling )
slope <- lda_fit$scaling[2]/lda_fit$scaling[1] 
intercept <- const / lda_fit$scaling[2]

par( mfrow = c(1,2) )
# Plot decision boundary
plot(current_tr_set[,predictors[2]] ~ current_tr_set[, predictors[1]], 
     xlab = predictors[1], ylab = predictors[2], xlim = c(-1,4.5), ylim = c(0,3),
     data = current_tr_set, 
     pch = unclass(current_tr_set[,target_variable]), 
     col = unclass(current_tr_set[,target_variable]))
abline(slope, intercept )

plot(current_ts_set[,predictors[2]] ~ current_ts_set[, predictors[1]], 
     xlab = predictors[1], ylab = predictors[2], xlim = c(-1,4.5), ylim = c(0,3),
     data = current_ts_set, 
     pch = unclass(current_ts_set[,target_variable]), 
     col = unclass(current_ts_set[,target_variable]) ) #♣, xlim = c(-2, 2))
abline(intercept, slope )

table(true = current_ts_set$binary_quality, predict = lda_pred$class)


pred <- lda_pred
my_points <- t(lda_fit$scaling %*% t(pred$x))

head(my.points)
head(current_ts_set)

dim(my.points)
dim(current_ts_set)


my.df <- data.frame(var1 = current_ts_set[,predictors[1]],
                    var2 = current_ts_set[,predictors[2]],
                    LD1 = my.points[,1], 
                    LD2 = my.points[,2])




ggplot(data = my.df, aes(predictors[2], predictors[1]))+
  geom_point(color = "red") 
#  geom_point(aes(x=LD1, y=LD2 * slope + 2)) + 
  geom_abline(intercept = intercept, slope = slope, size = 0.2)
#  coord_fixed(xlim = c(-4, 4), ylim = c(-1, 2))










# Test set confusion matrix

table(true = test.lasvegas$delinquent, predict = lda.pred$class)

# Total success rate

mean(lda.pred$class == test.lasvegas$delinquent)

# That's not bad, but notice the low sensitivity of this model.
# Test set ROC curve and AUC

predob = prediction(lda.pred$posterior[, 2], test.lasvegas$delinquent)
perf = performance(predob, "tpr", "fpr")
par(mfrow = c(1, 2))
plot(perf, main = "LDA", colorize = TRUE)
auc = c(auc, as.numeric(performance(predob, "auc")@y.values))
names(auc)[2] = "lda"
auc

# Exactly the same as the linear probability model. 

## Quadratic discriminant analysis

# Fit the model on the training sample 

qda.fit = qda(delinquent ~ ., data = train.lasvegas)

# Summary of results

qda.fit

# Predict the qda fit on the test sample

qda.pred = predict(qda.fit, newdata = test.lasvegas)

# Confusion matrix

table(true = test.lasvegas$delinquent, predict = qda.pred$class)

# Total success rate

mean(qda.pred$class == test.lasvegas$delinquent)

# That's slightly worse than lda. Again, sensitivity is very low.
# Test set ROC curve and AUC.

predob = prediction(qda.pred$posterior[ ,2], test.lasvegas$delinquent)
perf = performance(predob, "tpr", "fpr")
plot(perf, main = "QDA", colorize = TRUE)
auc = c(auc, as.numeric(performance(predob, "auc")@y.values))
names(auc)[3] = "qda"
auc



