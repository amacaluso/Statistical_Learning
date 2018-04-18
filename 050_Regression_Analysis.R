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




