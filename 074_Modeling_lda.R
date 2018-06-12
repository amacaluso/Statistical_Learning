### ***** IMPORT ***** ###
##########################

source( 'Utils.R')
source( '072_Modeling_lpm.R') 

SEED = 12344321
source( '020_Pre_processing.R') # REQUIRE SEED



### ***** SAVING FOLDER ***** ###

folder = "results/MODELING/CLASSIFICATION"
dir.create( folder )

folder_plot = paste0( folder, "/plots")
dir.create( folder_plot )

##################################


# DISCRIMINANT ANALYSIS

## Linear discriminant analysis
################################################
# Coefficiente di variazione

coeff_var<-apply( train.wine_binary[, -13 ], 2, CV)
lda.fit = lda(binary_quality ~ ., data = train.wine_binary)

# Summary of results
group_means_lda = lda.fit$means
coeff_lda = round(lda.fit$scaling, 3)

# ---> DA SALVARE? MAGARI INTERPRETARE

#variable importance

group_distances<-sort(abs(diff(lda.fit$means)))
names(group_distances)<-colnames(diff(lda.fit$means))[as.vector(order((abs(diff(lda.fit$means)))))]
group_distances

var_importance<-sort(abs(diff(lda.fit$means))/coeff_var)
names(var_importance)<-colnames(diff(lda.fit$means))[as.vector(order((abs(diff(lda.fit$means))/coeff_var)))]
var_importance



var_importance = data.frame( variable = names(var_importance), 
                             Importance = round( var_importance,2) , 
                             row.names = 1:length(var_importance) )
#var_importance[ order(var_importance$Importance)]

lda_importance = ggplot(var_importance, aes( variable, Importance, color = variable)) +
  geom_bar(  stat = "identity", position='stack') + 
  ggtitle( "LDA - Variable importance" ) + theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1,  size = 12, hjust = 1))

lda_importance = ggplotly( lda_importance) 
lda_importance

# ********** Saving a file ******************* #
file_name = paste0( folder_plot, "/lda_importance.Rdata")
save( lda_importance, file = file_name)
################################################




# Histograms of discriminant function values by class
######################################################

plot(lda.fit)


# Predict the lda fit on the test sample

lda.pred = predict(lda.fit, newdata = test.wine_binary) #test
lda.pred1 = predict(lda.fit, newdata = train.wine_binary) #train

hist(lda.pred$x[test.wine_binary$binary_quality==0],xlim = c(-10,10), probability = T,col="red") #bad
hist(lda.pred$x[test.wine_binary$binary_quality==1],xlim = c(-10,10),add=T,probability = T,col="blue") #good

intersection = (mean(lda.pred1$x[train.wine_binary$binary_quality==0])+
                  mean(lda.pred1$x[train.wine_binary$binary_quality==1]))/2 



#test sample
plot(density(lda.pred$x[test.wine_binary$binary_quality==0]),col="red") #bad
lines(density(lda.pred$x[test.wine_binary$binary_quality==1]),col="blue") #good
abline(v = intersection,lty = 2, lwd = 2, col = 3)

#train sample
# plot(density(lda.pred1$x[train.wine_binary$binary_quality==0]),col="red")
# lines(density(lda.pred1$x[train.wine_binary$binary_quality==1]),col="blue")
# abline(v = intersection, lty = 2, lwd = 2, col = 'green')


# Predict the lda fit on the test sample

lda_pred_bad_ts = data.frame( label = 'bad', prob = lda.pred$x[test.wine_binary$binary_quality==0] )
lda_pred_good_ts = data.frame( label = 'good', prob = lda.pred$x[test.wine_binary$binary_quality==1] )
lda_pred_ts = rbind( lda_pred_bad_ts, lda_pred_good_ts )

lda_hist_1_vs_0 = ggplot(lda_pred_ts, aes( x = prob, y = ..density.. )) +
  geom_histogram(data = subset(lda_pred_ts, label == 'bad'), fill = "red", alpha = 0.2, binwidth = 0.5) +
  geom_histogram(data = subset(lda_pred_ts, label == 'good'), fill = "blue", alpha = 0.2, binwidth = 0.5) +
  ggtitle( "Bad vs Good (test set)") 

lda_hist_1_vs_0 = ggplotly( lda_hist_1_vs_0)


# ********** Saving a file ******************* #
file_name = paste0( folder_plot, "/lda_hist_1_vs_0.Rdata")
save( lda_hist_1_vs_0, file = file_name)
# ******************************************** #





lda_line_1_vs_0 = ggplot(lda_pred_ts, aes( x = prob, y = ..density.. )) +
  geom_density(data = subset(lda_pred_ts, label == 'bad'), fill = "red", alpha = 0.2) +
  geom_density(data = subset(lda_pred_ts, label == 'good'), fill = "blue", alpha = 0.2) +
  ggtitle( "Bad vs Good (test set)") + 
  geom_vline( xintercept = intersection )

lda_line_1_vs_0 = ggplotly( lda_line_1_vs_0 )
lda_line_1_vs_0


# ********** Saving a file ******************* #
file_name = paste0( folder_plot, "/lda_line_1_vs_0.Rdata")
save( lda_line_1_vs_0, file = file_name)
################################################




### plot separation function on training sample
###############################################

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

corr_matrix = cor(wine[, 1:12])
corr_plot = corrplot(corr_matrix, method="color")
corrplot = ggplotly( ggcorrplot(corr_matrix, hc.order = TRUE,
                                outline.col = "white",
                                #ggtheme = ggplot2::theme_gray,
                                colors = c("#6D9EC1", "white", "#E46726")))
# ********** Saving a file ******************* #
file_name = paste0( folder_plot, "/corrplot.Rdata")
save( corrplot, file = file_name)
# ******************************************** #

S.good <- var(good)
S.bad <- var(bad)

W <- ((n_good-1)*S.good + (n_bad-1)*S.bad )/(n_good+n_bad-2)
W.inv <- solve(W)

# Between group covariance
B<-1/(2-1)*( (n_good*(mean.good-mean.tot)%*% t(mean.good-mean.tot))+ 
               (n_bad*(mean.bad-mean.tot)%*% t(mean.bad-mean.tot)) )

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
#################################################################


#plot on canonical variable -- real class
################################################################

plot(Y,type="n",xlab="Index",ylab="First Canonical variable",xlim=c(0,max(n_good,n_bad)))
points(Y[train.wine_binary[,13]==1,],pch=21,col="green")
points(Y[train.wine_binary[,13]==0,],pch=24,col="red")
abline(h=y.mean.good,col="blue",lty=2,lwd=2)
abline(h=y.mean.bad,col="yellow",lty=2,lwd=2)
abline(h=(y.mean.good+y.mean.bad)/2,col="black",lty=2,lwd=2) #linear separation
legend("bottomright",c("Good","Bad"),col=c("green","red"),pch=c(21,24))


Y_bad = Y[train.wine_binary[,13]==1,]
Y_good = Y[train.wine_binary[,13]==0,]
canonic_var = rbind( data.frame( label ='bad', index = 1:length(Y_bad), can_var = Y_bad ),
                     data.frame( label ='good', index = 1:length(Y_good),can_var = Y_good ))

canonical_variable = ggplot(canonic_var, aes( x = index, y= can_var )) +
                     geom_point(data = subset(canonic_var, label == 'bad'), col = "red", alpha = 0.5) +
                     geom_point(data = subset(canonic_var, label == 'good'), col = "blue", alpha = 0.5) +
                     ggtitle( "Canonical variable") +
                     geom_hline( yintercept = y.mean.good, col="blue", lty = 2, lwd = 1 ) +
                     geom_hline( yintercept = y.mean.bad, col = "yellow", lty = 2, lwd = 1) + 
                     geom_hline( yintercept = (y.mean.good+y.mean.bad)/2, col = "black", lty = 2, lwd = 1)



canonical_variable = ggplotly( canonical_variable )
canonical_variable


# ********** Saving a file ******************* #
file_name = paste0( folder_plot, "/canonical_variable.Rdata")
save( canonical_variable, file = file_name)
# ******************************************** #


#plot on canonical variable -- predicted class
plot(Y,type="n",xlab="Index",ylab="First Canonical variable",xlim=c(0,max(n_good,n_bad)))
points(Y[dist.groupY[,3]==1,],pch=21,col="green")
points(Y[dist.groupY[,3]==0,],pch=24,col="red")
abline(h=(y.mean.good+y.mean.bad)/2,col="black",lty=2,lwd=2) #linear separation


Y_bad = Y[dist.groupY[,3]==1,]
Y_good = Y[dist.groupY[,3]==0,]
canonic_var = rbind( data.frame( label ='bad', index = 1:length(Y_bad), can_var = Y_bad ),
                     data.frame( label ='good', index = 1:length(Y_good),can_var = Y_good ))

canonical_variable2 = ggplot(canonic_var, aes( x = index, y= can_var )) +
                      geom_point(data = subset(canonic_var, label == 'bad'), col = "red", alpha = 0.5) +
                      geom_point(data = subset(canonic_var, label == 'good'), col = "blue", alpha = 0.5) +
                      ggtitle( "Canonical variable" ) +
                      geom_hline( yintercept = (y.mean.good+y.mean.bad)/2, col="black", lty = 2, lwd = 1 ) 

canonical_variable2 = ggplotly( canonical_variable2 )
canonical_variable2


# ********** Saving a file ******************* #
file_name = paste0( folder_plot, "/canonical_variable2.Rdata")
save( canonical_variable2, file = file_name)
# ******************************************** #



hist(Y[which(train.wine_binary[,13]==1),],pch=21,col="green",bg="green")
hist(Y[which(train.wine_binary[,13]==0),],pch=24,col="red",bg="red",add=T)
abline(v=(y.mean.good+y.mean.bad)/2,col="black",lty=2,lwd=2) #linear separation


pred_bad = data.frame( label = 'bad', prob = Y[which(train.wine_binary[,13]==1),] )
pred_good = data.frame( label = 'good', prob = Y[which(train.wine_binary[,13]==0),] )
pred = rbind( pred_bad, pred_good )

lda_hist = ggplot(pred, aes( x = prob, y = ..density.. )) +
           geom_histogram(data = subset(pred, label == 'bad'), fill = "green", alpha = 0.2, binwidth = 0.5) +
           geom_histogram(data = subset(pred, label == 'good'), fill = "red", alpha = 0.2, binwidth = 0.5) +
           ggtitle( "Bad vs Good") 

lda_hist = ggplotly( lda_hist )


# ********** Saving a file ******************* #
file_name = paste0( folder_plot, "/lda_hist.Rdata")
save( lda_hist, file = file_name)
# ******************************************** #

################################################################



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
par(mfrow = c(1, 1))
plot(perf, main = "Linear Discriminant Analysis - test set", colorize = TRUE,
     print.cutoffs.at = seq(0, 1, by = 0.1), text.adj = c(-0.2, 1.7))
auc = c(auc, as.numeric(performance(predob, "auc")@y.values))
names(auc)[2] = "lda"
auc
################################################

tresholds<-seq( from = 0, to = 1, by = 0.01)

ROC_lda = cbind( Model = 'Linear_Discriminant_Analysis', 
                 ROC_analysis( prediction = lda.pred$posterior[,2], 
                               y_true = test.wine_binary$binary_quality,
                               probability_thresholds = tresholds))

ROC_all = rbind( ROC_all, ROC_lda )



ROC_matrix_lda = ROC_analysis( prediction = lda.pred$posterior[,2], 
                               y_true = test.wine_binary$binary_quality, 
                               probability_thresholds = tresholds)
ROC_matrix_lda = data.frame( treshold = ROC_matrix_lda$probability_thresholds,
                             FPR = 1-ROC_matrix_lda$`Specificity: TN/negative`, 
                             TPR = ROC_matrix_lda$`Sensitivity (AKA Recall): TP/positive` )

roc_curve_lda = ggplot(ROC_matrix_lda, aes(x = FPR, y = TPR, label = treshold)) +
                geom_line(color = "green") + theme_bw() + 
                style_roc() + #annotate("point", x = v, y = h, colour = "white")+
                ggtitle( "Linear discriminant analysis - test set")


roc_curve_lda = ggplotly( roc_curve_lda )


# ********** Saving file ******************* #
file_name = paste0( folder_plot, "/lda_roc_curve.Rdata")
save( roc_curve_lda, file = file_name)
################################################






rm(list=setdiff(ls(), c("test_error", 'ROC_all')))



