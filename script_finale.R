# DATA IMPORT -------------------------------------------------------------

# setwd("C:/Users/Luca/Desktop/Dottorato/Statistical Learning/Project") #WINDOWS
#setwd("/home/luca/workspace/Statistical_Learning") #LINUX

source( 'Utils.R')

red<-read.table( 'data/winequality-red.csv',sep=";", header = T)
white<-read.table( 'data/winequality-white.csv',sep=";", header = T)


#create joint dataset
red$type<-0#"red"
white$type<-1#"white"

wine<-rbind.data.frame(red,white)
wine$binary_quality <- ifelse(wine$quality<=5,0,1) #as.factor(ifelse(wine$quality<=5,0,1))

wine_binary<-wine[,-12]
wine<-wine[,-14]



# EXPLORATORY ANALYSES ----------------------------------------------------


str(red)
summary(red)
#pairs(red,col="red4")

str(white)
summary(white)
#pairs(white, col="khaki1")

# par(bg = "#c07f28")#dbaa87 #c6a17a #c6a17a
par(mfrow=c(2,1), bg="lightcyan3")
hist(red$quality,breaks = seq(1,10,by=.9),col="red4",xlab="wine quality (1-10)", main="Red wine",freq=F,ylim=c(0,0.6),xaxt="n")
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "white")
hist(red$quality,breaks = seq(1,10,by=.9),col="red4",xlab="wine quality (1-10)", main="Red wine",freq=F,ylim=c(0,0.6),xaxt="n",add=T)
axis(1,at=1:10)
lines(density(red$quality,bw=0.4)$x,density(red$quality,bw=0.4)$y,col="black",lwd=2)

hist(white$quality,breaks = seq(1,10,by=.9),col="#ba9d1b",xlab="wine quality (1-10)", main="White wine",freq=F,ylim=c(0,0.6),xaxt="n")
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "white")
hist(white$quality,breaks = seq(1,10,by=.9),col="#ba9d1b",xlab="wine quality (1-10)", main="White wine",freq=F,ylim=c(0,0.6),xaxt="n",add=T)
axis(1,at=1:10)
lines(density(white$quality,bw=0.4)$x,density(white$quality,bw=0.4)$y,col="black",lwd=2)
par(mfrow=c(1,1))

### GGPLOT2
# "wine quality (1-10)",y="Density"
par(mfrow=c(1,1),bg="lightcyan3")
p1 <- ggplot(red, 
       aes(x=quality,fill="white")
       )
p1 + geom_density()

# hist(red$quality,breaks = seq(1,10,by=.9),col="red4",xlab="wine quality (1-10)", main="Red wine",freq=F,ylim=c(0,0.6),xaxt="n")
# axis(1,at=1:10)
# lines(density(red$quality,bw=0.4)$x,density(red$quality,bw=0.4)$y,col="black",lwd=2)

hist(white$quality,breaks = seq(1,10,by=.9),col="#ba9d1b",xlab="wine quality (1-10)", main="White wine",freq=F,ylim=c(0,0.6),xaxt="n")
axis(1,at=1:10)
lines(density(white$quality,bw=0.4)$x,density(white$quality,bw=0.4)$y,col="black",lwd=2)
par(mfrow=c(1,1))


### L: CAMBIARE BANDWIDTH DI DENSITY???

f<-function(var){
  
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
  
  max_den=max(density( x= eval(parse(text = noquote(paste0("wine$",var)) ) ) )$y,
              density( x= eval(parse(text = noquote(paste0("red$",var)) ) ) )$y,
              density( x= eval(parse(text = noquote(paste0("white$",var)) ) ) )$y)
  
  print(max_den)
  
  plot(density( x= eval(parse(text = noquote(paste0("wine$",var)) ) ) ),col="deepskyblue", lwd=2,
       main=gsub("."," ",var,fixed=T),xlab=meas_unit[which(names(wine)==var)], ylim=c(0,max_den))
  lines(density( x= eval(parse(text = noquote(paste0("red$",var)) ) ) ),col="red4", lwd=2, lty=2,
       main=gsub("."," ",var,fixed=T),xlab=meas_unit[which(names(wine)==var)])
  lines(density( x= eval(parse(text = noquote(paste0("white$",var)) ) ) ),col="#ba9d1b", lwd=2, lty=2,
       main=gsub("."," ",var,fixed=T),xlab=meas_unit[which(names(wine)==var)])
  #text(x=12,y=0.2,paste0("bandwith = ",round(density( x= eval(parse(text = noquote(paste0("red$",var)) ) ) )$bw)))
  legend("topright",paste0("bandwith = ",round(density( x= eval(parse(text = noquote(paste0("wine$",var)) ) ) )$bw,4)), bty ="n", pch=NA) 
}


par(mfrow=c(3,4),bg="white")
ifelse(get_os()=="linux",lapply(names(wine)[-dim(wine)[2]],f),
       mclapply(names(wine)[-dim(wine)[2]],f,mc.cores = 1))
# mclapply(names(wine)[-dim(wine)[2]],f) #works on windows
# lapply(names(wine)[-dim(wine)[2]],f) #works on linux
par(mfrow=c(1,1))

# Let's now create a training and test set (80%/20%) split of the 6497 observations. 
# Here we use stratified sampling, using 'binary_quality' as the stratification variable



# SAMPLE SPLIT ------------------------------------------------------------




set.seed(12344321)
train.label = sample.split(wine_binary, SplitRatio = 3/5)

# Check that the split is balanced

train.wine_binary = wine_binary[train.label, ]
train.wine = wine[train.label, ]

test.wine_binary = wine_binary[!train.label,]
test.wine = wine[!train.label,]

mean(train.wine_binary$binary_quality)
mean(test.wine_binary$binary_quality)

mean(train.wine$quality)
mean(test.wine$quality)



# LINEAR PROBABILITY MODEL: BINARY QUALITY ---------------------------------

lpm.fit.all <- lm(binary_quality ~ ., data = train.wine_binary)
summary(lpm.fit.all)

# Predicted probabilities of quality
lpm.all.probs <- predict(lpm.fit.all, newdata = test.wine_binary) #test
lpm.all.probs_train <- predict(lpm.fit.all, newdata = train.wine_binary) #train

par(mfrow = c(1, 1))

hist(x = lpm.all.probs, breaks = 50, col = "chartreuse3",
     xlab = "Predicted values", ylab = "Frequency",
     main = "Linear probability model - test set")

# hist(x = lpm.all.probs_train, breaks = 50, col = "orange",
#      xlab = "Predicted probabilities", ylab = "Frequency",
#      main = "Linear probability model")

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

class_err<-as.data.frame(class_err)
names(class_err)<-c("treshold","test accuracy","train accuracy")
class_err[which.max(class_err[,2]),]
class_err[which.max(class_err[,3]),]

# > class_err[which.max(class_err[,2]),]
# treshold test error train error
# 55     0.54  0.7526667   0.7396438
# > class_err[which.max(class_err[,3]),]
# treshold test error train error
# 52     0.51  0.7513333   0.7442465

best_treshold = class_err[which.max(class_err[,2]),][1] #best test set treshold

###restore best treshold model
lpm.all.class = ifelse(lpm.all.probs > best_treshold$treshold, 1, 0) #test
lpm.all.class_train = ifelse(lpm.all.probs_train > best_treshold$treshold, 1, 0) #train

# confusion matrix
View(ROC_analysis( prediction = lpm.all.probs, y_true = test.wine_binary$binary_quality))

table(true = test.wine_binary$binary_quality, predict = lpm.all.class) #test
table(true = train.wine_binary$binary_quality, predict = lpm.all.class_train) #train

test_error = c(1-mean(lpm.all.class == test.wine_binary$binary_quality))
names(test_error)=c("lpm")
test_error


predob = prediction(lpm.all.probs, test.wine_binary$binary_quality)

perf = performance(predob, "tpr", "fpr")

par(mfrow = c(1, 1))
plot(perf, main = "Linear probability model - test set", colorize = TRUE,
     print.cutoffs.at = seq(0, 1, by = 0.1), text.adj = c(-0.2, 1.7))

abline(h=perf@y.values[[1]][   head(which(perf@alpha.values[[1]] <= best_treshold$treshold))[1] ],lty=2,col="blueviolet" )
abline(v=perf@x.values[[1]][   head(which(perf@alpha.values[[1]] <= best_treshold$treshold))[1] ],lty=2,col="blueviolet" )

auc = c(as.numeric(performance(predob, "auc")@y.values))
names(auc) = c("lpm")
auc


# DISCRIMINANT ANALYSIS --------------------------------------------

## Linear discriminant analysis

if (!require(MASS)) {
  install.packages("MASS")
  library(MASS)
}

# Coefficiente di variazione

CV<-function(x){
  sd(x)/abs(mean(x))
}

coeff_var<-apply(train.wine_binary[,-13],2,CV)

lda.fit = lda(binary_quality ~ ., data = train.wine_binary)

# Summary of results

lda.fit

#variable importance

group_distances<-sort(abs(diff(lda.fit$means)))
names(group_distances)<-colnames(diff(lda.fit$means))[as.vector(order((abs(diff(lda.fit$means)))))]
group_distances

var_importance<-sort(abs(diff(lda.fit$means))/coeff_var)
names(var_importance)<-colnames(diff(lda.fit$means))[as.vector(order((abs(diff(lda.fit$means))/coeff_var)))]
var_importance

# Histograms of discriminant function values by class

plot(lda.fit)

# Predict the lda fit on the test sample

lda.pred = predict(lda.fit, newdata = test.wine_binary) #test
lda.pred1 = predict(lda.fit, newdata = train.wine_binary) #train

hist(lda.pred$x[test.wine_binary$binary_quality==0],xlim = c(-10,10), probability = T,col="red") #bad
hist(lda.pred$x[test.wine_binary$binary_quality==1],xlim = c(-10,10),add=T,probability = T,col="blue") #good

#test sample
plot(density(lda.pred$x[test.wine_binary$binary_quality==0]),col="red") #bad
lines(density(lda.pred$x[test.wine_binary$binary_quality==1]),col="blue") #good
abline(v=(mean(lda.pred1$x[train.wine_binary$binary_quality==0])+
            mean(lda.pred1$x[train.wine_binary$binary_quality==1]))/2,lty=2,lwd=2, col = 3)


#train sample
plot(density(lda.pred1$x[train.wine_binary$binary_quality==0]),col="red")
lines(density(lda.pred1$x[train.wine_binary$binary_quality==1]),col="blue")
abline(v=(mean(lda.pred1$x[train.wine_binary$binary_quality==0])+
            mean(lda.pred1$x[train.wine_binary$binary_quality==1]))/2,lty=2,lwd=2, col = 'green')

### plot separation function on training sample

n <- dim(train.wine_binary)[1]
p <- dim(train.wine_binary)[2]-1 # Subtract 1 because one of the columns specifies the job

# Separate the 2 groups
good <-train.wine_binary[train.wine_binary$binary_quality==1,-13]
bad <-train.wine_binary[train.wine_binary$binary_quality==0,-13]

# Need sample statistics
n_good <- dim(good)[1]
n_bad <- dim(bad)[1]

# Group mean
mean.good <- apply(good,2,mean)
mean.bad <- apply(bad,2,mean)

mean.tot<-(mean.good*n_good+mean.bad*n_bad)/(n_good+n_bad)

# Within group covariance matrices
S.good <- var(good)
S.good

S.bad <- var(bad)
S.bad

W <- ((n_good-1)*S.good + (n_bad-1)*S.bad )/(n_good+n_bad-2)
W

W.inv <- solve(W)

# Between group covariance
B<-1/(2-1)*( (n_good*(mean.good-mean.tot)%*% t(mean.good-mean.tot))+ 
               (n_bad*(mean.bad-mean.tot)%*% t(mean.bad-mean.tot)) )
B

A<- W.inv %*% B # Calculating the canonical matrix

eigen_res<- eigen(A)
ifelse(rep(10**(-6),length(eigen_res$values))>Re(eigen_res$values),0,eigen_res$values)
#just one eigenvalue "different" from zero
eigen_res$vectors


a.vect<-Re(eigen_res$vectors[,1]) #corresponding to the only non-zero eigenvalue

Y<-as.matrix(train.wine_binary[,-13])%*%(a.vect)
length(Y)
dim(train.wine_binary)

#PROJECTION ONTO Y
y.mean.good<-mean.good%*% a.vect
y.mean.bad<-mean.bad%*% a.vect
y.mean.good
y.mean.bad

#Euclidean centroid distance over Y
dist.groupY<-matrix(0,nrow=nrow(Y),3)
colnames(dist.groupY)<-c("dist.good","dist.bad","Group")
for (i in 1:nrow(Y)){
  dist.good<-sqrt(sum((Y[i,]-y.mean.good)^2)) #Euclidean distance
  dist.bad<-sqrt(sum((Y[i,]-y.mean.bad)^2)) #Euclidean distance
  
  dist.groupY[i,]<-c(dist.good,dist.bad,which.max(c(dist.good,dist.bad))-1)
}

dist.groupY

#Mahalanobis centroid distance over X
dist.groupX<-matrix(0,nrow=nrow(Y),3)
colnames(dist.groupX)<-c("dist.good","dist.bad","Group")
for (i in 1:nrow(Y)){
  dist.good<-(Y[i,]-y.mean.good)%*%(t(a.vect)%*%W.inv%*%a.vect)%*%t(Y[i,]-y.mean.good) #mahalanobis distance
  dist.bad<-(Y[i,]-y.mean.bad)%*%(t(a.vect)%*%W.inv%*%a.vect)%*%t(Y[i,]-y.mean.bad) #mahalanobis distance
  dist.groupX[i,]<-c(dist.good,dist.bad,which.max(c(dist.good,dist.bad))-1)
}

dist.groupX

#plot on canonical variable -- real class
plot(Y,type="n",xlab="Index",ylab="First Canonical variable",xlim=c(0,max(n_good,n_bad)))
points(Y[train.wine_binary[,13]==1,],pch=21,col="green")
points(Y[train.wine_binary[,13]==0,],pch=24,col="red")
abline(h=y.mean.good,col="blue",lty=2,lwd=2)
abline(h=y.mean.bad,col="yellow",lty=2,lwd=2)
abline(h=(y.mean.good+y.mean.bad)/2,col="black",lty=2,lwd=2) #linear separation


legend("bottomright",c("Good","Bad"),col=c("green","red"),pch=c(21,24))

#plot on canonical variable -- predicted class
plot(Y,type="n",xlab="Index",ylab="First Canonical variable",xlim=c(0,max(n_good,n_bad)))
points(Y[dist.groupY[,3]==1,],pch=21,col="green")
points(Y[dist.groupY[,3]==0,],pch=24,col="red")
abline(h=(y.mean.good+y.mean.bad)/2,col="black",lty=2,lwd=2) #linear separation


hist(Y[which(train.wine_binary[,13]==1),],pch=21,col="green",bg="green")
hist(Y[which(train.wine_binary[,13]==0),],pch=24,col="red",bg="red",add=T)
abline(v=(y.mean.good+y.mean.bad)/2,col="black",lty=2,lwd=2) #linear separation



density.good<-density(Y[train.wine_binary[,13]==1,])
density.bad<-density(Y[train.wine_binary[,13]==0,])

plot(density.good,col="green",bg="green",ylim=c(0,max(density.good$y,density.bad$y)))
lines(density(Y[train.wine_binary[,13]==0,]),col="red",bg="red")
abline(v=(y.mean.good+y.mean.bad)/2,col="black",lty=2,lwd=2) #linear separation

###comparing results with lda
psi<-t(a.vect)%*%W%*%a.vect
a.vect%*%(solve(psi)^(1/2))

#the other way around
coef(lda.fit)%*%solve(solve(psi)^(1/2))

# Test set confusion matrix
table(true = test.wine_binary$binary_quality, predict = lda.pred$class)

# Total success rate

mean(lda.pred$class == test.wine_binary$binary_quality)

test_error = c(test_error, 1-mean(lda.pred$class == test.wine_binary$binary_quality))
names(test_error)[2]="lda"
test_error

# That's not bad, but notice the low sensitivity of this model.
# Test set ROC curve and AUC

predob = prediction(lda.pred$posterior[, 2], test.wine_binary$binary_quality)
perf = performance(predob, "tpr", "fpr")
par(mfrow = c(1, 2))
plot(perf, main = "Linear Discriminant Analysis - test set", colorize = TRUE,
     print.cutoffs.at = seq(0, 1, by = 0.1), text.adj = c(-0.2, 1.7))
auc = c(auc, as.numeric(performance(predob, "auc")@y.values))
names(auc)[2] = "lda"
auc


## Quadratic discriminant analysis
# Fit the model on the training sample 

qda.fit = qda(binary_quality ~ ., data = train.wine_binary)

# Summary of results

qda.fit

#variable importance

group_distances<-sort(abs(diff(qda.fit$means)))
names(group_distances)<-colnames(diff(qda.fit$means))[as.vector(order((abs(diff(qda.fit$means)))))]
group_distances

var_importance<-sort(abs(diff(qda.fit$means))/coeff_var)
names(var_importance)<-colnames(diff(qda.fit$means))[as.vector(order((abs(diff(qda.fit$means))/coeff_var)))]
var_importance


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


# LOGISTIC REGRESSION -----------------------------------------------------

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


# REGULARIZED LOGISTIC REGRESSION -------------------------------------------

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

# LASSO logistic regression; for this we use the default 'alpha = 1'
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


# K-NEAREST NEIGHBOR -----------------------------------------------------

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
if(get_os()=="linux")
  sr.vec <- lapply(K.vec,knn.sr) else 
    sr.vec <- mclapply(K.vec,knn.sr,mc.cores = 4)

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

# KNN CARET ---------------------------------------------------------------


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


# MULTIPLE REGRESSION: RESPONSE = ALCOHOL ------------------------------------------

linreg = lm(alcohol ~ ., data = train.wine)

summary(linreg)

if (!require(car)) {
  install.packages("car")
  library(car)
}

# significance test: exclude total.sulfur.dioxide
linearHypothesis(model = linreg,
                 hypothesis.matrix = c("total.sulfur.dioxide = 0"), test = "F")

# train set error

MSPE.train.lm = mean((linreg$residuals)^2)

# Prediction error of the naive estimator (training set alcohol mean)

MSPE.test.naive = mean((test.wine$alcohol-mean(train.wine$alcohol))^2)

MSPE.train.lm
MSPE.test.naive

# Predictions and test set error

pred = predict(object = linreg, newdata = test.wine)
MSPE = c(mean((test.wine$alcohol-pred)^2))
names(MSPE) = c("lm")

# > MSPE
# train.lm   test.lm    test.0 
# 0.2119810 0.2948535 1.4100943 


# Feature selection -------------------------------------------------------

# Load the 'leaps' package
if (!require(leaps)){
  install.packages("leaps", repos="http://cran.rstudio.com/")
  library(leaps)
}

## Best subset selection

fit = regsubsets(alcohol ~ ., data = train.wine, nvmax = ncol(train.wine)-1)
fit.summary = summary(fit)

# Summary of results

fit.summary

# non-nested models: fixed.acidity in model with two regressors but not with three (back again in 4)

names(fit.summary)



# Plot the RSS vs. the number of variables (this is similar 
# to the red line in fig. 3.5)

plot_subset_error<-function(model.summary){

  par(mfrow=c(1,3))
  plot(model.summary$rss, 
       xlab = "Number of Variables", ylab = "RSS",
       main = "Residual Sum of Squares",
       col = "red", type = "b", pch = 16)
  
  imin = which.min(model.summary$rss)
  imin
  
  # Add a marker for the suggested number of variables
  points(imin, model.summary$rss[imin], pch = 17, col = "blue", cex=2)
  
  # Add the marker also according to adjusted R^2
  imin = which.min(model.summary$adjr2)
  imin
  
  points(imin, model.summary$rss[imin], pch = 17, col = "darkgoldenrod2", cex=2)
  
  # Plot BIC vs. the number of variables
  
  plot(model.summary$bic, xlab = "Number of Variables", ylab = "BIC",
       main = "BIC",
       col = "red", type = "b", pch = 16)
  
  imin = which.min(model.summary$bic)
  imin
  
  # Add a marker for the suggested number of variables
  points(imin, model.summary$bic[imin], pch = 17, col = "blue", cex=2)
  
  # Plot Mallow's Cp vs. the number of variables, and find 
  # the best specification
  
  plot(model.summary$cp, xlab = "Number of Variables", ylab = "Cp",
       main = "Mallow's CP",
       col = "red", type = "b", pch = 16)
  
  # Find the minimum 
  
  imin = which.min(model.summary$cp)
  imin
  
  # The best subset regression contains 11 variables plus 
  # the intercept. Add a point to the plot to highlight the minimum
  
  points(imin, model.summary$cp[imin], pch = 17, col = "blue", cex=2)

  return (imin) #according to Mallow's CP
}

# Another plot, based on a plot method for the 'regsubsets' object, 
# provides a visual description of the recursive structure of 
# the best subset regressions for each number of variables
# 
# par(mfrow=c(1,1))
# plot(fit, scale = "Cp")

# Here are the coefficients of the best model

coef(fit, imin)

# The best subset regression drops 'total.sulfur.dioxide' only. Let's compute
# its test set MSPE
# lm(alcohol ~ . -citric.acid - chlorides - free.sulfur.dioxide - total.sulfur.dioxide, data = train.wine), newdata = test.wine)

response<-colnames(test.wine)[11]
regressors<-names(fit.summary$which[imin,fit.summary$which[imin,]==T])[-1]
best_formula<-as.formula(paste(response," ~ ",paste(regressors,collapse=" + ")))
best_model<-lm(best_formula, data = train.wine)

pred = predict(best_model, newdata = test.wine)

MSPE = c(MSPE, mean((test.wine$alcohol-pred)^2))
names(MSPE)[2] = "BSS"
MSPE

## Forward Stepwise Selection

fit = regsubsets(alcohol ~ ., data = train.wine, method = "forward",nvmax = ncol(train.wine)-1)
fit.summary<-summary(fit)

imin<-plot_subset_error(fit.summary)

# CV using the 'caret' package
# Info at http://topepo.github.io/caret/index.html

if (!require(caret)){
  install.packages("caret")
  library(caret)
}

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

plot(CV.fit)

# Manual plot (similar to fig. 3.7)

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
