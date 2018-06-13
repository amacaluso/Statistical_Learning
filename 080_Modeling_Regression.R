### ***** IMPORT ***** ###
##########################
# source( '072_Modeling_lpm.R') # REQUIRE SEED
# source( '074_Modeling_lda.R') # REQUIRE SEED

source( 'Utils.R')
SEED = 12344321
source( '020_Pre_processing.R') # REQUIRE SEED


X = remove_columns_by_names(df = train.wine, colNames = 'alcohol')
Y = train.wine$alcohol

X_test = remove_columns_by_names(df = test.wine, colNames = 'alcohol')
Y_test = test.wine$alcohol



# MULTIPLE REGRESSION: RESPONSE = ALCOHOL ------------------------------------------
################################################

linear_reg = lm( Y  ~ ., data = X)

linear_reg_summary = as.data.frame( round( summary( linear_reg )$coefficients, 2))
save_table(df = linear_reg_summary )

lm_pred_train = predict(object = linear_reg, newdata = train.wine)
evaluation_model( target_variable = Y, prediction = lm_pred_train, MODEL = 'Multiple Regression (train)' )

lm_pred_test = predict(object = linear_reg, newdata = test.wine)
evaluation_model( target_variable = Y_test, prediction = lm_pred_test, MODEL = 'Multiple Regression (test)' )



# +++++++++++++++ Feature selection +++++++++++++ #

## Best subset selection
fit = regsubsets(Y ~ ., data = X, nvmax = ncol(X))
fit.summary = summary(fit)

# non-nested models: fixed.acidity in model with two regressors but not with three (back again in 4)
names(fit.summary)

# Plot the RSS vs. the number of variables (this is similar 
# to the red line in fig. 3.5)

#plot_subset_error<-function(model.summary){

model.summary = fit.summary



df_rss = data.frame( index = 1:length( model.summary$rss ), RSS = model.summary$rss )
df_bic = data.frame( index = 1:length( model.summary$bic ), BIC = model.summary$bic )
df_cp = data.frame( index = 1:length( model.summary$cp ), CP = model.summary$cp )

plt_rss = ggplot( data = df_rss, aes( x = index, y = RSS) ) + 
          geom_line( col = 'red') + 
          geom_point( col = 'red') +  xlab("Number of Variables") + ylab ( "RSS") + 
          geom_point( aes( x = which.min( df_rss$RSS), y = df_rss$RSS[ which.min( df_rss$RSS)]),
                           col = 'blue', size = 3)


plt_bic = ggplot( data = df_bic, aes( x = index, y = BIC) ) + 
          geom_line( col = 'red') + 
          geom_point( col = 'red') +  xlab("Number of Variables") + ylab ( "BIC") + 
          geom_point( aes( x = which.min( df_bic$BIC), y = df_bic$BIC[ which.min( df_bic$BIC)]),
                      col = 'blue', size = 3)



plt_cp = ggplot( data = df_cp, aes( x = index, y = CP) ) + 
         geom_line( col = 'red') + 
         geom_point( col = 'red') +  xlab("Number of Variables") + ylab ( "CP") + 
         geom_point( aes( x = which.min( df_cp$CP), y = df_cp$CP[ which.min( df_cp$CP)]),
                           col = 'blue', size = 3)

  
plt_rss = ggplotly( plt_rss )
plt_bic = ggplotly( plt_bic )
plt_cp = ggplotly( plt_cp )


subplot( list( plt_rss, plt_bic, plt_cp) , titleX = F ) %>% layout(title = "RSS vs BIC vs CP")

  

# Another plot, based on a plot method for the 'regsubsets' object, 
# provides a visual description of the recursive structure of 
# the best subset regressions for each number of variables
# 
# par(mfrow=c(1,1))
# plot(fit, scale = "Cp")

# Here are the coefficients of the best model


# The best subset regression drops 'total.sulfur.dioxide' only. Let's compute
# its test set MSPE
# lm(alcohol ~ . -citric.acid - chlorides - free.sulfur.dioxide - total.sulfur.dioxide, data = train.wine), newdata = test.wine)

imin = which.min(model.summary$cp)

response<-colnames(test.wine)[11]
regressors <- names( fit.summary$which[imin,fit.summary$which[imin,]==T])[-1]

best_formula<-as.formula(paste(response," ~ ",paste(regressors,collapse=" + ")))
best_model<-lm(best_formula, data = train.wine)

lm_sub_pred = predict(best_model, newdata = test.wine)

evaluation_model( target_variable = Y_test, prediction = lm_sub_pred, MODEL = 'Multiple Regression (BSS)' )



## Forward Stepwise Selection

fit = regsubsets(Y ~ ., data = X, method = "forward", nvmax = ncol(X))
fit.summary<-summary(fit)

imin<-plot_subset_error(fit.summary)

# CV using the 'caret' package
# Info at http://topepo.github.io/caret/index.html


# We consider LOOCV, repeated one

set.seed(1)
fitControl = trainControl(method = "loocv")
pGrid = expand.grid(nvmax = seq(from = 1, to = dim(train.wine)[2]-1, by = 1))
repetition=10
CV.fit = train(alcohol ~ ., data = train.wine, number = repetition,
               method = "leapForward", trControl = fitControl,
               tuneGrid = pGrid)

CV.fit$times

CV.fit

# Notice that the models are evaluated in terms of 
# RMSE and Rsquared. RMSE is the root mean squared error 
# averaged over CV iterations. Rsquared is the R^2 coefficient 
# averaged across the resampling results. We look for models 
# with low 'RMSE' and large 'Rsquared'. Note that the RMSE and
# Rsquared standard deviation is also calculated. 


CV.fit$results

# Plot the output
# Automatic plot
df = data.frame( n_var = CV.fit$results$nvmax, RMSE = CV.fit$results$RMSE)

ggplot( data = df, aes( x = n_var, y = RMSE)) + geom_line(col = "blue") + geom_point( col = 5)

par(mfrow=c(1,1))
k = CV.fit$results$nvmax
avg = CV.fit$results$RMSE
sdev = CV.fit$results$RMSESD/sqrt(repetition)
plot(k, avg,
     ylim = range(c(avg-sdev, avg+sdev)),
     pch = 19, col = "darkgoldenrod1", 
     xlab = "Subset Size", ylab = "CV Error",
     main = "Forward Subset Selection")
lines(k, avg, lty = 1, lwd = 2, col = "darkgoldenrod1")
arrows(k, avg-sdev, k, avg+sdev, 
       length = 0.05, angle = 90, code = 3, col = "skyblue1")

# To identify the final model, we look for the simplest one 
# within 1 standard error from the best one
# In the manual plot:

k.min = which.min(avg)
# abline(h = avg[k.min]+sdev[k.min], lty = 2, lwd = 1, col = "purple") 
k.oneSE = which.max(avg[1:k.min] <= avg[k.min]+sdev[k.min]) #min(k[avg[1:k.min] <= avg[k.min]+sdev[k.min]])
# abline(v = k.oneSE, lty = 2, lwd = 1, col = "purple")

points(k.oneSE, avg[k.oneSE], pch = 17, col = "red", cex=2)


# Automatic command:

oneSE.reg = oneSE(CV.fit$results, metric = "RMSE", 
                  num = nrow(train.wine), maximize = FALSE)
CV.fit$results[oneSE.reg, ]

# The minimum-RMSE model is with k = 11

coef(fit, imin)

# The 1-SE rule suggests k = 7

coef(fit, oneSE.reg)

# Let's compute its test set MSPE

regressors<-names(fit.summary$which[oneSE.reg,fit.summary$which[oneSE.reg,]==T])[-1]
best_formula<-as.formula(paste(response," ~ ",paste(regressors,collapse=" + ")))
best_model<-lm(best_formula, data = train.wine)

pred = predict(best_model, newdata = test.wine)

MSPE = c(MSPE, mean((test.wine$alcohol-pred)^2))
names(MSPE)[3] = "FSS"
MSPE

## Backward Stepwise Selection

fit = regsubsets(alcohol ~ ., data = train.wine, method = "backward", nvmax = ncol(train.wine))
fit.summary<-summary(fit)

fit.summary

imin<-plot_subset_error(fit.summary)

# It's the same as with FSS, but this needs not always be the case

# CV using the 'caret' package

set.seed(1)
CV.fit = train(alcohol ~ ., data = train.wine, number = repetition,
               method = "leapBackward", 
               trControl = fitControl, 
               tuneGrid = pGrid)

CV.fit$times

CV.fit

# Automatic plot

plot(CV.fit)

# Best model according to the 1-standard error rule

oneSE.reg = oneSE(CV.fit$results, metric = "RMSE", 
                  num = repetition, maximize = FALSE)

# > oneSE.reg
# [1] 5


CV.fit$results[oneSE.reg, ]

# Here the 1-SE rule suggests k = 6. Let's compute its 
# test set MSPE

coef(fit, oneSE.reg)

par(mfrow=c(1,1))
k = CV.fit$results$nvmax
avg = CV.fit$results$RMSE
sdev = CV.fit$results$RMSESD/sqrt(repetition)
plot(k, avg,
     ylim = range(c(avg-sdev, avg+sdev)),
     pch = 19, col = "darkgoldenrod1", 
     xlab = "Subset Size", ylab = "CV Error",
     main = "Backward Subset Selection")
lines(k, avg, lty = 1, lwd = 2, col = "darkgoldenrod1")
arrows(k, avg-sdev, k, avg+sdev, 
       length = 0.05, angle = 90, code = 3, col = "skyblue1")

# To identify the final model, we look for the simplest one 
# within 1 standard error from the best one
# In the manual plot:

k.min = which.min(avg)
# abline(h = avg[k.min]+sdev[k.min], lty = 2, lwd = 1, col = "purple") 
k.oneSE = which.max(avg[1:k.min] <= avg[k.min]+sdev[k.min]) #min(k[avg[1:k.min] <= avg[k.min]+sdev[k.min]])
# abline(v = k.oneSE, lty = 2, lwd = 1, col = "purple")

points(k.oneSE, avg[k.oneSE], pch = 17, col = "red", cex=2)

# Let's compute its test set MSPE

regressors<-names(fit.summary$which[oneSE.reg,fit.summary$which[oneSE.reg,]==T])[-1]
best_formula<-as.formula(paste(response," ~ ",paste(regressors,collapse=" + ")))
best_model<-lm(best_formula, data = train.wine)

pred = predict(best_model, newdata = test.wine)


MSPE = c(MSPE, mean((test.wine$alcohol-pred)^2))
names(MSPE)[4] = "BackSS"
MSPE

# SHRINKAGE METHODS -------------------------------------------------------

## Ridge regression

if (!require(glmnet)){
  install.packages("glmnet")
  library(glmnet)
}

x.train = model.matrix(alcohol ~ . -1, data = train.wine) 
y.train = train.wine$alcohol
x.test = model.matrix(alcohol ~ . -1, data = test.wine) 
y.test = test.wine$alcohol


# We now fit a ridge regression model. This is 
# achieved by calling 'glmnet' with 'alpha = 0' (see 
# the helpfile)

fit = glmnet(x.train, y.train, alpha = 0)

effective_df<-function(lambda, X){
  I_lambda<- matrix(lambda,ncol(X),ncol(X))
  sum( diag( X %*% solve(t(X) %*% X + I_lambda) %*% t(X) ) )
}

plot(fit, xvar = "lambda", label = T, ylim=c(-1,1.5))
fit

df<-sapply(fit$lambda, effective_df,simplify = T, x.train)

palette<-rainbow(n=ncol(x.train))
for (i in 1: ncol(x.train)){
  if (i==1){
    plot(x=df,y=fit$beta[i,],col=palette[i], type = "l", ylim = c(-1,1), xlim = c(11,12),
         main = "Ridge Coefficients Profiles", xlab="df(lambda)", ylab="coefficient")
    text(x=df[80],y=fit$beta[i,80],labels=labels(fit$beta)[[1]], adj=1, col=palette[i])
    
  } else{
    lines(x=df,y=fit$beta[i,],col=palette[i])
    text(x=df[100],y=fit$beta[i,100],labels=labels(fit$beta)[[1]][i], adj=1, col=palette[i])  
  }
}

# 'fit' prints a three columns matrix: 
# 'Df', '%Dev' and 'Lambda'. Each row corresponds to a 
# different model, one for each Lambda value. There are 
# 100 of them

# There is also a 'cv.glmnet' function which will do 
# 10-fold CV on 'lambda'

cv.ridge = cv.glmnet(x.train, y.train, alpha = 0)
plot(cv.ridge)

# Ridge coefficient estimates at the optimal (i.e., 1-SE) 
# lambda

coef(cv.ridge)

# Use the best model according to the 1-SE rule to predict 
# on the test set and compute the MSE

pred.ridge = predict(cv.ridge, x.test, s = cv.ridge$lambda.1se)
MSPE.test.ridge = mean((pred.ridge-y.test)^2)
MSPE = c(MSPE, MSPE.test.ridge)
names(MSPE)[7] = "test.ridge"
MSPE

### Lasso regression

# We now fit a LASSO model; for this we use the default 
# 'alpha = 1'

fit.lasso = glmnet(x.train, y.train)
plot(fit.lasso, xvar = "lambda", label = TRUE)
fit.lasso

# Again, CV can be done automatically using 'cv.glmnet'

cv.lasso = cv.glmnet(x.train, y.train)
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
MSPE.test.lasso = mean((pred.lasso-y.test)^2)
MSPE = c(MSPE, MSPE.test.lasso)
names(MSPE)[8] = "test.lasso"
MSPE

### Elastic net regression

# Finally, we fit an elastic net model; for this we use 
# 'alpha = 0.5'. In principle, the optimal value of 'alpha'
# could be estimated by CV, but it is rarely worth the effort

fit.elnet = glmnet(x.train, y.train, alpha = 0.5)
plot(fit.elnet, xvar = "lambda", label = TRUE)
cv.elnet = cv.glmnet(x.train, y.train, alpha = 0.5)
plot(cv.elnet)
coef(cv.lasso)

i.best = which(fit.elnet$lambda == cv.elnet$lambda.1se)
fit.elnet$df[i.best]

pred.elnet = predict(fit.elnet, x.test, s = cv.elnet$lambda.1se)
MSPE.test.elnet = mean((pred.elnet-y.test)^2)
MSPE = c(MSPE, MSPE.test.elnet)
names(MSPE)[9] = "test.elnet"
MSPE

### Principal Components Regression (PCR)

## First, let's look at the Principal Components of the features

summary(wine[, 1:13])
summary(prostate[, 1:9])
# It is clear that these variables are measured in different 
# scales. As the outcome of PCA is highly sensitive to the scale 
# of the data, let's rescale the variables.

std.wine = scale(wine[, 1:13], 
                 center = TRUE, scale = TRUE)
apply(std.wine, 2, mean)
apply(std.wine, 2, sd)

# Compute PCAs on the standardized variables.

pca.out = prcomp(std.wine)
names(pca.out)
summary(pca.out)
pca.out

# There is a simple command to draw the screeplot

screeplot(pca.out, npcs = length(pca.out$sdev))

# Cumulative screeplot 

plot(100*summary(pca.out)$importance[3, ], type = "h", 
     lwd = 30, lend = "square", col = "magenta2",
     xlab = "Number of PCs", ylab = "Cumulative variance (%)",
     main = "Cumulative screeplot")

# There seems to be a "corner" at the fourth PC.

# Biplot of the first two PCs:

biplot(pca.out)

# To visualize the relationship between the first two PCs
# and the response 'lpsa', let's use a color coded plot

rbPal = colorRampPalette(c('purple', 'blue', 'cyan', 'green', 
                           'yellow', 'orange', 'red'),
                         interpolate = "linear")
col = rbPal(70)[as.numeric(cut(wine$alcohol, breaks = 70))]
plot(pca.out$x[, 1], pca.out$x[, 2], pch = 20, col = col,
     xlab = "PC 1", ylab = "PC 2", main = "Purple: low, Red: high")

## To run PCR, install the 'pls' package

if (!require(pls)){
  install.packages("pls")
  library(pls)
}

# The 'pcr' command can automatically conduct CV. 
# Fix the seed and fit the model on the 
# training data. As usual, use '?pcr' to get help on 
# the command

set.seed(1)
pcr.fit = pcr(alcohol ~ ., data = train.wine, 
              scale = TRUE, validation = "CV")

# Summary of results

summary(pcr.fit)

# What does the 'pcr' object contain? Let's explore it:

names(pcr.fit)

# Plot of the cross-validation MSEs

validationplot(pcr.fit, val.type = "MSEP")

# The smallest MSE occurs for M = 8 - no 
# dimension reduction. However, the MSE is roughly constant 
# after M = 3. Let's pick this value:

pcr.fit = pcr(alcohol ~ ., data = train.wine, scale = TRUE, ncomp = 12)

# Coefficient estimates:

pcr.fit$coefficients

# Test set prediction and MSE

pcr.pred = predict(pcr.fit, newdata = test.wine)
MSPE.test.pcr = mean((test.wine$alcohol-pcr.pred)^2)
MSPE = c(MSPE, MSPE.test.pcr)
names(MSPE)[10] = "test12.pcr"
MSPE

# The test set error is large. Maybe we chose M too low.

### Partial Least Squares

# Fit the model with 10-fold CV:

set.seed(1)
pls.fit = plsr(alcohol ~ ., data = train.wine, 
               scale = TRUE, validation = "CV")

# Summary of the results:

summary(pls.fit)

# Validation plot:

validationplot(pls.fit, val.type = "MSEP")

# Here we may use M = 2. The dimension reduction 
# is remarkable, but the model is still dense. 
# Coefficient estimates:

pls.fit = plsr(alcohol ~ ., data = train.wine, 
               scale = TRUE, ncomp = 6)
pls.fit$coefficients

# Test set prediction and MSE

pls.pred = predict(pls.fit, newdata = test.wine)
MSPE.test.pls = mean((test.wine$alcohol-pls.pred)^2)
MSPE = c(MSPE, MSPE.test.pls)
names(MSPE)[11] = "test6.pls"
MSPE

# It does not work well on the test set

# GAM. For each component, we fit a smoothing spline with
# 4 degrees of freedom

if (!require(gam)) {
  install.packages("gam")
  library(gam)
}

names(train.wine)
gam.3.fit = gam(alcohol ~ s(fixed.acidity, df = 4) + 
                  s(volatile.acidity, df = 4) + 
                  s(citric.acid, df = 4) + 
                  s(residual.sugar, df = 4) + 
                  s(chlorides, df = 4) + 
                  s(free.sulfur.dioxide, df = 4) + 
                  s(total.sulfur.dioxide, df = 4) + 
                  s(density, df = 4) + 
                  s(pH, df = 4) + 
                  s(sulphates, df = 4) + 
                  s(quality, df = 4),
                
                data = train.wine, family = gaussian, 
                control = gam.control(bf.maxit = 100, trace = TRUE))

# Predicted probabilities on the test sample 

gam.4.pred = predict(gam.3.fit, newdata = test.wine, 
                     type = "response") 


# Test set prediction and MSE

MSPE.test.gam4 = mean((test.wine$alcohol-gam.4.pred)^2)
MSPE = c(MSPE, MSPE.test.gam4)
names(MSPE)[19] = "test.gam"
MSPE

# Global test of nonlinear vs. linear effects only

anova(linreg, gam.3.fit, test = "Chisq")

# Plot of all the terms

plot(gam.3.fit, rugplot = TRUE, se = TRUE, 
     ask = TRUE)

# Plot of the 'char_freq_excl' component 
# (i.e., the effect of the frequency of '!' signs)

index = 5

plot(gam.3.fit, rugplot = TRUE, se = TRUE, 
     terms = c(names(coef(gam.3.fit))[index+1]), lwd = 2)

# Let's add the estimate of the effect from the 
# linear logistic regression model

beta = coef(linreg)[index+1]
m = mean(train.wine$chlorides)
abline(a = -beta*m, b = beta, col = "red", lwd = 2)

# Plot of the 'char_freq_excl' component on the original scale
# (without log transform)

preds = predict(gam.3.fit, type = "terms", se = TRUE)
x = train.wine[, index]
x = exp(x)-0.1
yhat = preds$fit[, index]
se = preds$se.fit[, index]
l = yhat - 2*se
u = yhat + 2*se
d = data.frame(x, yhat, l, u)
names(d) = c("x", "yhat", "l", "u")
sd = d[order(d$x), ]
par(mar = c(5.1,4.5,4.1,2.1))
xlab = names(train.spam)[index]
ylab = names(gam.4.fit$coefficients)[index+1]
plot(5, 5, xlim = c(min(sd$x), max(sd$x)), 
     ylim = c(min(sd$l), max(sd$u)), type = "n",
     xlab = xlab, ylab = ylab)
polygon(c(sd$x, rev(sd$x)), c(sd$l, rev(sd$u)), 
        col = "gold", border = NA)
lines(sd$x, sd$yhat, type = "l", lwd = 2, col = "green")
rug(jitter(sd$x), ticksize = 0.03, side = 1, lwd = 0.5, col = "red")

# Is nonlinearity really necessary? Let's run a test.
# First, estimate a linear logistic regression model
# with a 4-degrees of freedom smoothing spline for 'char_freq_excl'

gam.4.fit.nl = gam(spam ~ . - char_freq_excl + s(char_freq_excl, df = 4),
                   data = train.spam, family = binomial)

# Now run an anova test comparing this model with the one with linear
# effects only

anova(glm.fit, gam.4.fit.nl, test = "Chisq")
