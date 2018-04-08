
source('Utils.R')
source('010_Data_preparation.R')



linear_regression = step( lm( alcohol ~ ., data = train_dataset ), direction = 'both' )
summary(linear_regression)


y_hat = predict.glm( object = linear_regression, newdata = test_dataset )
model = "LINEAR_REGRESSION_STEPWISE"


evaluation_model( target_variable = test_dataset$quality, prediction = y_hat, MODEL = model)







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




