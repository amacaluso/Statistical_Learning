# DATA IMPORT -------------------------------------------------------------

# setwd("C:/Users/Luca/Desktop/Dottorato/Statistical Learning/Project") #WINDOWS
setwd("/home/luca/workspace/Statistical_Learning") #LINUX
red<-read.table('winequality-red.csv',sep=";",header=T)
white<-read.table('winequality-white.csv',sep=";",header=T)

# EXPLORATORY ANALYSES ----------------------------------------------------

str(red)
summary(red)
pairs(red,col="red4")

str(white)
summary(white)
pairs(white, col="khaki1")

par(mfrow=c(2,1))
hist(red$quality,breaks = seq(1,10,by=.9),col="red4",xlab="wine quality (1-10)", main="Red wine",freq=F,ylim=c(0,0.6),xaxt="n")
axis(1,at=1:10)
lines(density(red$quality,bw=0.4)$x,density(red$quality,bw=0.4)$y,col="black",lwd=2)
hist(white$quality,breaks = seq(1,10,by=.9),col="khaki1",xlab="wine quality (1-10)", main="White wine",freq=F,ylim=c(0,0.6),xaxt="n")
axis(1,at=1:10)
lines(density(white$quality,bw=0.4)$x,density(white$quality,bw=0.4)$y,col="black",lwd=2)
par(mfrow=c(1,1))

### L: CAMBIARE BANDWIDTH DI DENSITY???

f_red<-function(var){

  meas_unit<-c(expression(g(tartaric_acid)/dm^3),
               expression(g(acetic_acid)/dm^3),
               expression(g/dm^3),
               expression(g/dm^3),
               expression(g(sodium_chloride)/dm^3),
               expression(mg/dm^3),
               expression(mg/dm^3),
               expression(g/cm^3),
               "pH",
               expression(g(potassium_sulphate)/dm^3),
               "% volume"
  )
  plot(density( x= eval(parse(text = noquote(paste0("red$",var)) ) ) ),col="red4", lwd=2,
       main=gsub("."," ",var,fixed=T),xlab=meas_unit[which(names(red)==var)])
  #text(x=12,y=0.2,paste0("bandwith = ",round(density( x= eval(parse(text = noquote(paste0("red$",var)) ) ) )$bw)))
  legend("topright",paste0("bandwith = ",round(density( x= eval(parse(text = noquote(paste0("red$",var)) ) ) )$bw,4)), bty ="n", pch=NA) 
  }

f_white<-function(var){
  
  meas_unit<-c(expression(g(tartaric_acid)/dm^3),
               expression(g(acetic_acid)/dm^3),
               expression(g/dm^3),
               expression(g/dm^3),
               expression(g(sodium_chloride)/dm^3),
               expression(mg/dm^3),
               expression(mg/dm^3),
               expression(g/cm^3),
               "pH",
               expression(g(potassium_sulphate)/dm^3),
               "% volume"
  )
  plot(density( x= eval(parse(text = noquote(paste0("white$",var)) ) ) ),col="khaki1", lwd=2,
       main=gsub("."," ",var,fixed=T),xlab=meas_unit[which(names(red)==var)])
  #text(x=12,y=0.2,paste0("bandwith = ",round(density( x= eval(parse(text = noquote(paste0("white$",var)) ) ) )$bw)))
  legend("topright",paste0("bandwith = ",round(density( x= eval(parse(text = noquote(paste0("white$",var)) ) ) )$bw,4)), bty ="n", pch=NA) 
}

library(parallel)
par(mfrow=c(3,4))
mclapply(names(red),f_red)
mclapply(names(white),f_white)
par(mfrow=c(1,1))

install.packages("sqldf")
library(sqldf)
quality_red<-sqldf('select quality, count(*) as freq from red group by quality')
quality_white<-sqldf('select quality, count(*) as freq from white group by quality')
unconditional_pred_red<-quality_red$freq[which.max(quality_red$freq)]/sum(quality_red$freq)
unconditional_pred_white<-quality_white$freq[which.max(quality_white$freq)]/sum(quality_white$freq)

### L: PROBLEMA --> LE CLASSI ESTREME (i.e. <5 e >7) PRESENTANO POCHE OSSERVAZIONI IN ENTRAMBI I DATASET


# ANALYSES *RED* ----------------------------------------------------------------

## Linear probability model

lpm.fit = lm(quality ~ ., data = red)

# Summary of results

summary(lpm.fit)

# Predicted probabilities of delinquency

lpm.probs = predict(lpm.fit) 

par(mfrow = c(1, 1))
hist(x = lpm.probs, breaks = 50, col = "red",
     xlab = "Predicted probabilities", ylab = "Frequency",
     main = "Linear probability model")

# Predicted outcomes, using a 0.5 threshold

lpm.preds = round(lpm.probs)# > 0.5, 1, 0)

# Training set confusion matrix

table(true = red$quality, predict = lpm.preds)

# Total training sample success rate

mean(lpm.preds == red$quality)

# What is the success rate of the unconditional model that 
# does not use any predictor? For such a model, 
# the predicted probability would be the fraction
# of observations with delinquent = 1

# > unconditional_pred_red
# [1] 0.4258912



# PROVA -------------------------------------------------------------------

## Linear probability model

head(red)
red$binary_quality <- ifelse(red$quality<=5,0,1)

redinho<-red[,-12]

lpm.fit1 = lm(binary_quality ~ ., data = redinho)

# Summary of results

summary(lpm.fit1)

# Predicted probabilities of delinquency

lpm.probs1 = predict(lpm.fit1) 

par(mfrow = c(1, 1))
hist(x = lpm.probs1, breaks = 50, col = "red4",
     xlab = "Predicted probabilities", ylab = "Frequency",
     main = "Linear probability model")

# Predicted outcomes, using a 0.5 threshold

lpm.preds1 = ifelse(lpm.probs1 > 0.5, 1, 0)

# Training set confusion matrix

table(true = red$binary_quality, predict = lpm.preds1)

# Total training sample success rate

mean(lpm.preds1 == red$binary_quality)

# What is the success rate of the unconditional model that 
# does not use any predictor? For such a model, 
# the predicted probability would be the fraction
# of observations with delinquent = 1

# > unconditional_pred_red
# [1] 0.4258912


# ANALYSIS *ALL DATA* ---------------------------------------------------

#create joint dataset
red$type<-0#"red"
white$type<-1#"white"

wine<-rbind.data.frame(red,white)
wine$binary_quality <- ifelse(wine$quality<=5,0,1) #as.factor(ifelse(wine$quality<=5,0,1))

wine_binary<-wine[,-12]
wine<-wine[,-14]

# Let's now create a training and test set (60%/40%) split of 
# the 1,000 observations. Here we use stratified sampling, 
# using 'delinquent' as the stratification variable

if (!require(caTools)) {
  install.packages("caTools")
  library(caTools)
}
set.seed(1)
train.label = sample.split(wine_binary, SplitRatio = 3/5)

# Check that the split is balanced

train.wine_binary = wine_binary[train.label, ]
train.wine = wine[train.label, ]
mean(train.wine_binary$binary_quality)
mean(train.wine$quality)

test.wine_binary = wine_binary[!train.label,]
test.wine = wine[!train.label,]
mean(test.wine_binary$binary_quality)
mean(test.wine$quality)

# Linear Probability Model MULTI-CATEGORIES --------------------------------

lpm.fit.all1 <- lm(quality ~ ., data = train.wine)
summary(lpm.fit.all1)

# Predicted probabilities of quality

lpm.all.probs1 <- predict(lpm.fit.all1, newdata = test.wine) #test
lpm.all.probs_train1 <- predict(lpm.fit.all1, newdata = train.wine) #train

par(mfrow = c(1, 1))

hist(x = lpm.all.probs1, breaks = 50, col = "blue",
     xlab = "Predicted probabilities", ylab = "Frequency",
     main = "Linear probability model")

hist(x = lpm.all.probs_train1, breaks = 50, col = "orange",
     xlab = "Predicted probabilities", ylab = "Frequency",
     main = "Linear probability model")

# Predicted outcomes rounding response probabilities

lpm.all.class1 = round(lpm.all.probs1) # > 0.5, 1, 0) #test
lpm.all.class_train1 = round(lpm.all.probs_train1) # > 0.5, 1, 0) #train

# confusion matrix

table(true = test.wine$quality, predict = lpm.all.class1) #test
table(true = train.wine$quality, predict = lpm.all.class_train1) #train

# Total success rate

mean(lpm.all.class1 == test.wine$quality) #test
mean(lpm.all.class_train1 == train.wine$quality) #train


# Linear Probability Model BINARY QUALITY ---------------------------------

lpm.fit.all <- lm(binary_quality ~ ., data = train.wine_binary)
summary(lpm.fit.all)

# Predicted probabilities of quality

lpm.all.probs <- predict(lpm.fit.all, newdata = test.wine_binary) #test
lpm.all.probs_train <- predict(lpm.fit.all, newdata = train.wine_binary) #train

par(mfrow = c(1, 1))

hist(x = lpm.all.probs, breaks = 50, col = "blue",
     xlab = "Predicted probabilities", ylab = "Frequency",
     main = "Linear probability model")

hist(x = lpm.all.probs_train, breaks = 50, col = "orange",
     xlab = "Predicted probabilities", ylab = "Frequency",
     main = "Linear probability model")

# Predicted outcomes, threshold optimization

tresholds<-seq(0,1,by=0.01)
class_err<-matrix(0,length(tresholds),3)
for (i in tresholds){
lpm.all.class = ifelse(lpm.all.probs > i, 1, 0) #test
lpm.all.class_train = ifelse(lpm.all.probs_train > i, 1, 0) #train

# confusion matrix

table(true = test.wine_binary$binary_quality, predict = lpm.all.class) #test
table(true = train.wine_binary$binary_quality, predict = lpm.all.class_train) #train

# Total success rate

test_err<-mean(lpm.all.class == test.wine_binary$binary_quality) #test
train_err<-mean(lpm.all.class_train == train.wine_binary$binary_quality) #train

class_err[which(tresholds==i),]<-c(i,test_err,train_err)
writeLines(paste0("threshold: ", i))

writeLines(paste0("Test error: ",test_err))
writeLines(paste0("Train error: ",train_err,"\n"))

}

class_err[which.max(class_err[,2]),]
class_err[which.max(class_err[,3]),]

# > class_err[which.max(class_err[,2]),]
# [1] 0.4900000 0.7392464 0.7475700
# > class_err[which.max(class_err[,3]),]
# [1] 0.5200000 0.7385795 0.7524300
# BEST TRESHOLD 0.49 ---> 0.5 IS OK THEN, PERFORMANCE DO NOT VARY A LOT: ~0.74% ACCURACY

###restore best treshold model
lpm.all.class = ifelse(lpm.all.probs > 0.5, 1, 0) #test
lpm.all.class_train = ifelse(lpm.all.probs_train > 0.5, 1, 0) #train

# confusion matrix

table(true = test.wine_binary$binary_quality, predict = lpm.all.class) #test
table(true = train.wine_binary$binary_quality, predict = lpm.all.class_train) #train

if (!require(ROCR)) {
  install.packages("ROCR")
  library(ROCR)
}

# To draw the ROC plot, we need a vector containing 
# a numerical score for each observation and a vector containing 
# the class label for each observation. We already have both of them:
# 'lpm.all.probs' and 'binary_quality', respectively.
# Armed with these quantities, we need to execute a couple of steps:
# 1) Transform the input data into a standardized format. This 
#    is done using the 'prediction' function. place the result in
#    'predob', an object of class 'prediction'

predob = prediction(lpm.all.probs, test.wine_binary$binary_quality)

# 2) Evaluate the performance of the predictor using the
#    'performance' function, whose first argument is the output of the 
#    previous step

perf = performance(predob, "tpr", "fpr")

# 3) Plot 'perf'

par(mfrow = c(1, 1))
plot(perf, main = "Linear probability model, test set", colorize = TRUE)
abline(h=perf@y.values[[1]][   head(which(perf@alpha.values[[1]] <= 0.5))[1] ],lty=2,col="blueviolet" )
abline(v=perf@x.values[[1]][   head(which(perf@alpha.values[[1]] <= 0.5))[1] ],lty=2,col="blueviolet" )

auc = c(as.numeric(performance(predob, "auc")@y.values))
names(auc) = c("lpm")
auc

# DISCRIMINANT ANALYSIS --------------------------------------------

## Linear discriminant analysis

if (!require(MASS)) {
  install.packages("MASS")
  library(MASS)
}

# Even though LDA assumes normal predictors, we will still use our variables
# Fit the model on the training sample 


### BINARY QUALITY ###

lda.fit = lda(binary_quality ~ ., data = train.wine_binary)

# Summary of results

lda.fit

# Histograms of discriminant function values by class

plot(lda.fit)

# Predict the lda fit on the test sample

lda.pred = predict(lda.fit, newdata = test.wine_binary)

# Attributes in lda.pred

names(lda.pred)

# Test set confusion matrix

table(true = test.wine_binary$binary_quality, predict = lda.pred$class)

# Total success rate

mean(lda.pred$class == test.wine_binary$binary_quality)

# That's not bad, but notice the low sensitivity of this model.
# Test set ROC curve and AUC

predob = prediction(lda.pred$posterior[, 2], test.wine_binary$binary_quality)
perf = performance(predob, "tpr", "fpr")
par(mfrow = c(1, 2))
plot(perf, main = "LDA", colorize = TRUE)
auc = c(auc, as.numeric(performance(predob, "auc")@y.values))
names(auc)[2] = "lda"
auc

### MULTI-LEVEL QUALITY ###

lda.fit1 = lda(quality ~ ., data = train.wine)

# Summary of results

lda.fit1

# Histograms of discriminant function values by class

plot(lda.fit1)

# Predict the lda fit on the test sample

lda.pred1 = predict(lda.fit1, newdata = test.wine)

# Attributes in lda.pred

names(lda.pred1)

# Test set confusion matrix

table(true = test.wine$quality, predict = lda.pred1$class)

# Total success rate

mean(lda.pred1$class == test.wine$quality)

# That's not bad, but notice the low sensitivity of this model.
# Test set ROC curve and AUC

predob = prediction(lda.pred1$posterior[, 2], test.wine$quality)
perf = performance(predob, "tpr", "fpr")
par(mfrow = c(1, 2))
plot(perf, main = "LDA", colorize = TRUE)
auc = c(auc, as.numeric(performance(predob, "auc")@y.values))
names(auc)[2] = "lda"
auc

## Quadratic discriminant analysis

# Fit the model on the training sample 

qda.fit = qda(binary_quality ~ ., data = train.wine_binary)

# Summary of results

qda.fit

# Predict the qda fit on the test sample

qda.pred = predict(qda.fit, newdata = test.wine_binary)

# Confusion matrix

table(true = test.wine_binary$binary_quality, predict = qda.pred$class)

# Total success rate

mean(qda.pred$class == test.wine_binary$binary_quality)

# That's slightly worse than lda. Again, sensitivity is very low.
# Test set ROC curve and AUC.

predob = prediction(qda.pred$posterior[ ,2], test.wine_binary$binary_quality)
perf = performance(predob, "tpr", "fpr")
plot(perf, main = "QDA", colorize = TRUE)
auc = c(auc, as.numeric(performance(predob, "auc")@y.values))
names(auc)[3] = "qda"
auc


# Logistic regression -----------------------------------------------------

train.wine_binary$binary_quality <- as.factor(train.wine_binary$binary_quality)
test.wine_binary$binary_quality <- as.factor(test.wine_binary$binary_quality)

# Logistic regression fit on the training sample.
# Notice how close this command is to the 'lm' one

glm.fit = glm( binary_quality ~ ., data = train.wine_binary, family = binomial)

# Predicted probabilities on the test sample 
glm.probs = predict(glm.fit, newdata = test.wine_binary, type = "response") 

# Predicted responses on the test set
glm.pred = ifelse(glm.probs > 0.5, 1, 0)

# Test sample confusion matrix
table(true = test.wine_binary$binary_quality, predict = glm.pred)

# Test sample total success rate
mean(glm.pred == test.wine_binary$binary_quality)

# Test set ROC curve and AUC

predob = prediction(glm.probs, test.wine_binary$binary_quality)
perf = performance(predob, "tpr", "fpr")
par(mfrow = c(1, 1))
plot(perf, main = "Logistic Regression", colorize = TRUE)
auc = c(auc, as.numeric(performance(predob, "auc")@y.values))
names(auc)[4] = "logreg"
auc


# Penalised Logistic Regression -------------------------------------------

# Load the 'glmnet' package

if (!require(glmnet)){
  install.packages("glmnet")
  library(glmnet)
}

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

perf = performance(predob, "tpr", "fpr")
par(mfrow = c(1, 1))
plot(perf, main = "Ridge", colorize = TRUE)
auc = c(auc, as.numeric(performance(predob, "auc")@y.values))
names(auc)[5] = "ridge"
auc

# LASSO logistic regression; for this we use the default 'alpha = 1'
cv.lasso = cv.glmnet(x.train, y.train, family = "binomial")
plot(cv.lasso)
coef(cv.lasso)

# Use the best model according to the 1-SE rule to predict on the test set and compute the AUC
lasso.probs = predict(cv.lasso, x.test, s = cv.lasso$lambda.1se, family = "binomial", type = "response")

predob = prediction(lasso.probs, y.test)

# Test sample total success rate
mean(round(predob@predictions[[1]]) == test.wine_binary$binary_quality)

perf = performance(predob, "tpr", "fpr")
par(mfrow = c(1, 1))
plot(perf, main = "Lasso", colorize = TRUE)
auc = c(auc, as.numeric(performance(predob, "auc")@y.values))
names(auc)[6] = "lasso"
auc


# K-Nearest Neighbors -----------------------------------------------------


if (!require(class)) {
  install.packages("class")
  library(class)
}

# In the knn approach there are no parameters; prediction is done completely at evaluation time.
# We need to specify the X in- and out-of-sample, along with the Y in-sample. The last argument is k

wine_binary$type = ifelse(wine_binary$type=="white", 1, 0)
X = as.matrix(wine_binary[, -13])
X.train = X[train.label,]
X.test = X[!train.label,]
Y = wine_binary$binary_quality
Y.train = Y[train.label]
Y.test = Y[!train.label]

# Fit knn on the training sample. We will consider a sequence of fits, with K ranging between 1 and 100, with unit steps

K.vec = seq(from = 1, to = 100)

# We write a function which, for each K: 
# 1) fits the model, 
# 2) computes the test data success rate

sr.vec = rep(0, times = length(K.vec))
knn.sr = function(K){
  knn.pred = knn(X.train, X.test, Y.train, k = K)
  mean(knn.pred == Y.test)  
}

# Now, set up a loop to evaluate 'knn.sr' on all the elements of K.vec, and store the results in 'sr.vec'

for(i in 1:length(K.vec)) {
  sr.vec[i] = knn.sr(K.vec[i])
}

# Finally, plot the success rate as a function of K

par(mfrow = c(1, 1))
plot(K.vec, sr.vec,type="l")

# Notice the 'inverse U-shape'. K between 10 and 20 seems optimal

k.min = min(which.max(sr.vec))
k.min

# Consider the best model, with K = k.min 
# Does it fit the data adequately? The answer is NO. 
# Its test set error rate is roughly the same as the 
# unconditional model one (approximately 80%).
# To check this, we refit the model using the optional argument 
# 'prob = TRUE' in the knn function call to retrieve 
# fitted probabilities.

knn.pred = knn(X.train, X.test, Y.train, k = k.min, prob = TRUE)
knn.probs = attributes(knn.pred)$prob

# Note that the probabilities that are returned in this way
# are those for the 'winning' class, which can be either 0 or 1. 
# To convert these number to probabilities of getting '1' we need
# to be careful with observations for which class 0 wins!

knn.probs[knn.pred == 0] = 1 - knn.probs[knn.pred == 0]

# Draw a histogram of the probabilities

hist(knn.probs)

# They are all smaller than 0.5! 
# (Actually, there is just one larger than 0.5.)
# As a consequence, the predicted classes will be always 0 
# (except one observation)

table(true = Y.test, predict = knn.pred)

# Another way to see how awful the 'best' knn fit is is to 
# plot the test set ROC curve

predob = prediction(knn.probs, Y.test)

# Test sample total success rate
mean(round(predob@predictions[[1]]) == test.wine_binary$binary_quality)

perf = performance(predob, "tpr", "fpr")
par(mfrow = c(1, 1))
plot(perf, main = "KNN with K = 1", colorize = TRUE)

# It looks a lot like random guessing. 
# Compute the AUC

auc = c(auc, as.numeric(performance(predob, "auc")@y.values))
names(auc)[7] = "knn"
auc

# Random guessing would get AUC = 0.5.
# Why does knn perform so badly? Easy: curse of dimensionality!
# 8 predictors are too many (especially since the number of
# training observations is just 600). 



# EXTRAS: NEURAL NETWORKS -------------------------------------------------


if (!require(neuralnet)) {
  install.packages("neuralnet")
  library(neuralnet)
}

# n<-names(wine_binary)
# f <- as.formula(paste("binary_quality ~", paste(n[!n %in% "binary_quality"], collapse = " + ")))

##alternative:
response<-"binary_quality"
predictors<-setdiff(names(wine_binary),response)
f <- as.formula(paste0(response," ~", paste0(predictors, collapse = " + ")))

maxs <- apply(wine_binary[,-c(12,13)], 2, max) 
maxs<-c(maxs,0,0)
mins <- apply(wine_binary[,-c(12,13)], 2, min)
mins<-c(mins,-1,-1)
scaled_train.wine_binary<-as.data.frame(scale(wine_binary[,-c(12,13)]))#as.data.frame(scale(wine_binary, center = mins, scale = maxs - mins))
scaled_train.wine_binary$type <- wine_binary$type
scaled_train.wine_binary$binary_quality <- wine_binary$binary_quality
head(scaled_train.wine_binary)

nn<-neuralnet(f , data = scaled_train.wine_binary, hidden = c(7))#, stepmax = 10000, rep = 5, err.fct = "ce", act.fct = "logistic", linear.output = F )
nn<-neuralnet(f , data = train.wine_binary, hidden = c(7), stepmax = 1000000000, rep = 1, err.fct = "ce", act.fct = "logistic", linear.output = F )

library(h2o)
h2o.init()

nn.h2o<-h2o.deeplearning(y=13,x=1:12,training_frame = as.h2o(train.wine_binary), validation_frame = as.h2o(test.wine_binary),
                         activation = "Tanh",hidden = c(5,5,5),epochs = 1000, input_dropout_ratio = .30, loss="Automatic")
plot(nn.h2o)

predob = as.data.frame(predict(nn.h2o,newdata=as.h2o(test.wine_binary)))
predob.h2o = predict(nn.h2o,newdata=as.h2o(test.wine_binary))
mean(predob$predict==test.wine_binary$binary_quality)

perf = performance(predob.h2o, "tpr", "fpr")
par(mfrow = c(1, 1))
plot(perf, main = "Lasso", colorize = TRUE)
auc = c(auc, as.numeric(performance(predob, "auc")@y.values))
names(auc)[6] = "lasso"
auc


# REGRESSION: RESPONSE = ALCOHOL ------------------------------------------

# First, standardize the predictors to have zero mean and 
# unit variance

scaled_train.wine = as.data.frame(scale(x = train.wine, center = TRUE, scale = TRUE))
class(scaled_train.wine)
# Second, linear regression fit

reg.lin = lm(alcohol ~ ., data = train.wine)
#reg.lin = lm(alcohol ~ ., data = scaled_train.wine)
summary(reg.lin)

if (!require(car)) {
  install.packages("car")
  library(car)
}

linearHypothesis(model = reg.lin,
                 hypothesis.matrix = c(#"quality= 0", 
                                       "chlorides = 0",
                                       "total.sulfur.dioxide = 0"), 
                 test = "F")

# train set error

MSPE.train.lm = mean((reg.lin$residuals)^2)

# Second, compute predictions and test set error

alcohol.test.hat = predict(object = reg.lin, newdata = test.wine)
MSPE.test.lm = mean((test.wine$alcohol-alcohol.test.hat)^2)

# Finally, mean prediction error using the mean 
# training value of lpsa

MSPE.test.0 = mean((test.wine$alcohol-mean(train.wine$alcohol))^2)

# Display results

MSPE = c(MSPE.train.lm, MSPE.test.lm, MSPE.test.0)
names(MSPE) = c("train.lm", "test.lm", "test.0")
# > MSPE
# train.lm   test.lm    test.0 
# 0.2066607 0.3015443 1.4570149 

### Subset selection

# Load the 'leaps' package

if (!require(leaps)){
  install.packages("leaps", repos="http://cran.rstudio.com/")
  library(leaps)
}

## Best subset selection

# The 'regsubsets' command implements BSS up to 8 predictors.
# This can be modified using the 'nvmax' argument (it's not 
# necessary here)

BSS.train = regsubsets(alcohol ~ ., data = train.wine)
BSS.train.summary = summary(BSS.train)

# Summary of results

BSS.train.summary

# As the number of included variables increases, we might get 
# non-nested models (that's not the case here)

# 'BSS.TR' is a composite object. Let's explore its content:

names(BSS.train.summary)

# Plot the RSS vs. the number of variables (this is similar 
# to the red line in fig. 3.5)

plot(BSS.train.summary$rss, 
     xlab = "Number of Variables", ylab = "RSS",
     col = "red", type = "b", pch = 16)

# Plot Mallow's Cp vs. the number of variables, and find 
# the best specification

plot(BSS.train.summary$cp, xlab = "Number of Variables", ylab = "Cp",
     col = "red", type = "b", pch = 16)

# Find the minimum 

imin = which.min(BSS.train.summary$cp)
imin

# The best subset regression contains 7 variables plus 
# the intercept. Add a point to the plot to highlight the minimum

points(imin, BSS.train.summary$cp[imin], pch = 11, col = "blue")

# Another plot, based on a plot method for the 'regsubsets' object, 
# provides a visual description of the recursive structure of 
# the best subset regressions for each number of variables

plot(BSS.train, scale = "Cp")

# Here are the coefficients of the best model

coef(BSS.train, imin)

# The best subset regression drops 'gleason' only. Let's compute
# its test set MSPE

BSS.alcohol.test.hat = predict(
  lm(alcohol ~ . -citric.acid - chlorides - free.sulfur.dioxide - total.sulfur.dioxide, data = train.wine), newdata = test.wine)
MSPE.test.bss = mean((test.wine$alcohol-BSS.alcohol.test.hat)^2)
MSPE = c(MSPE, MSPE.test.bss)
names(MSPE)[4] = "test.bss"
MSPE

## Forward Stepwise Selection

# Here we use the 'regsubsets' function but specify 
# the 'method = "forward"' option

FSS.train = regsubsets(alcohol ~ ., data = train.wine, method = "forward")
summary(FSS.train)

# Notice that here the models in the sequence are always nested
# Plot Mallow's Cp vs. the number of variables, and find 
# the best specification

plot(FSS.train, scale = "Cp")

# CV using the 'caret' package
# Info at http://topepo.github.io/caret/index.html

if (!require(caret)){
  install.packages("caret")
  library(caret)
}

# We consider 10-fold CV, repeated once

set.seed(1)
fitControl = trainControl(method = "repeatedcv")
CV.FSS.train = train(alcohol ~ ., data = train.wine, number = 10,
                  method = "leapForward", trControl = fitControl)
CV.FSS.train

# Notice that the models are evaluated in terms of 
# RMSE and Rsquared. RMSE is the root mean squared error 
# averaged over CV iterations. Rsquared is the R^2 coefficient 
# averaged across the resampling results. We look for models 
# with low 'RMSE' and large 'Rsquared'. Note that the RMSE and
# Rsquared standard deviation is also calculated. 

names(CV.FSS.train)

# 'results' is the most important attribute

head(CV.FSS.train$results)

# By default, 'train' applies CV to three values of 
# the tuning parameter 'nvmax', but this can be changed

set.seed(1)
pGrid = expand.grid(nvmax = seq(from = 1, to = dim(train.wine)[2]-1, by = 1))
CV.FSS.train = train(alcohol ~ ., data = train.wine, number = 10,
                     method = "leapForward", trControl = fitControl,
                  tuneGrid = pGrid)
CV.FSS.train$results

# Plot the output
# Automatic plot

plot(CV.FSS.train)

# Manual plot (similar to fig. 3.7)

k = CV.FSS.train$results$nvmax
avg = CV.FSS.train$results$RMSE
sdev = CV.FSS.train$results$RMSESD/sqrt(10)
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

k.min = k[which(avg == min(avg))]
abline(h = avg[k.min]+sdev[k.min], lty = 2, lwd = 1, col = "purple") 
k.oneSE = min(k[avg[1:k.min] <= avg[k.min]+sdev[k.min]])
abline(v = k.oneSE, lty = 2, lwd = 1, col = "purple")

# Automatic command:

oneSE.reg = oneSE(CV.FSS.train$results, metric = "RMSE", 
                  num = 10, maximize = FALSE)
CV.FSS.train$results[oneSE.reg, ]

# The minimum-RMSE model is with k = 7

coef(FSS.train, imin)

# The 1-SE rule suggests k = 2

coef(FSS.train, oneSE.reg)

# Let's compute its test set MSPE

FSS.alcohol.test.hat = predict(
  lm(alcohol ~ . -quality -citric.acid - chlorides -free.sulfur.dioxide -total.sulfur.dioxide, data = train.wine),
  newdata = test.wine)
MSPE.test.fss = mean((test.wine$alcohol-FSS.alcohol.test.hat)^2)
MSPE = c(MSPE, MSPE.test.fss)
names(MSPE)[5] = "test.fss"
MSPE

## Backward Stepwise Selection

# As before, we use the 'regsubsets' function but specify 
# the 'method = "backward"' option

BackSS.train = regsubsets(alcohol ~ ., data = train.wine, method = "backward")
summary(BackSS.train)

# Plot Mallow's Cp vs. the number of variables, and find 
# the best specification

plot(BackSS.train, scale = "Cp")

# It's the same as with FSS, but this needs not always be the case
# CV using the 'caret' package

set.seed(1)
CV.BackSS.train = train(alcohol ~ ., data = train.wine, number = 10,
                     method = "leapBackward", 
                     trControl = fitControl, 
                     tuneGrid = pGrid)

# Automatic plot

plot(CV.BackSS.train)

# Best model according to the 1-standard error rule

oneSE.reg = oneSE(CV.BackSS.train$results, metric = "RMSE", 
                  num = 10, maximize = FALSE)
CV.BackSS.train$results[oneSE.reg, ]

# Here the 1-SE rule suggests k = 6. Let's compute its 
# test set MSPE

coef(BackSS.train, oneSE.reg)
BackSS.alcohol.test.hat = predict(
  lm(alcohol ~ . -quality -citric.acid - chlorides -free.sulfur.dioxide -total.sulfur.dioxide, data = train.wine),
  newdata = test.wine)
MSPE.test.backss = mean((test.wine$alcohol-BackSS.alcohol.test.hat)^2)
MSPE = c(MSPE, MSPE.test.backss)
names(MSPE)[6] = "test.backss"
MSPE

### Ridge regression

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

x = model.matrix(alcohol ~ . -1, data = train.wine) 
head(x)
y = train.wine$alcohol

# Let's make again a training and test set

x = model.matrix(alcohol ~ . -1, data = wine) 
y = wine$alcohol

x.train = model.matrix(alcohol ~ . -1, data = train.wine) 
y.train = train.wine$alcohol
x.test = model.matrix(alcohol ~ . -1, data = test.wine) 
y.test = test.wine$alcohol


# We now fit a ridge regression model. This is 
# achieved by calling 'glmnet' with 'alpha = 0' (see 
# the helpfile)

fit.ridge = glmnet(x.train, y.train, alpha = 0)
plot(fit.ridge, xvar = "lambda", label = TRUE)
fit.ridge

# 'fit.ridge' prints a three columns matrix: 
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

anova(reg.lin, gam.3.fit, test = "Chisq")

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

beta = coef(reg.lin)[index+1]
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
