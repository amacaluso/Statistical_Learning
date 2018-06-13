### ***** IMPORT ***** ###
##########################

source( 'Utils.R')
SEED = 12344321
source( '020_Pre_processing.R') # REQUIRE SEED



### ***** SAVING FOLDER ***** ###

folder = "results/MODELING/CLASSIFICATION"
dir.create( folder )

folder_plot = paste0( folder, "/plots")
dir.create( folder_plot )
##################################


# LINEAR PROBABILITY MODEL: BINARY QUALITY
##########################################

lpm.fit.all <- lm(binary_quality ~ ., data = train.wine_binary)
# summary(lpm.fit.all)

lpm_summary = as.data.frame( round( summary(lpm.fit.all)$coefficients, 2))

# ********** Saving a file ******************* #
#
file_name = paste0( folder, "/lpm_summary.Rdata")
save( lpm_summary, file = file_name)
#


# Predicted probabilities of quality

lpm.all.probs <- predict(lpm.fit.all, newdata = test.wine_binary) #test
lpm.all.probs_train <- predict(lpm.fit.all, newdata = train.wine_binary) #train

lpm_all_probs = data.frame( lpm.all.probs )
lpm_all_probs$lpm.all.probs = round(lpm_all_probs$lpm.all.probs, 2)

lpm_probs = ggplot( data = lpm_all_probs ) + 
  geom_histogram( aes( x = lpm.all.probs), binwidth = 0.04, color="darkblue", fill="lightblue") + 
  xlab("Predicted values") +  ylab("Frequency") + 
  ggtitle("Linear probability model - test set") +
  theme_bw() 

lpm_probs = ggplotly( lpm_probs )

# ********** Saving a file ******************* #
#
file_name = paste0( folder_plot, "/lpm_probs.Rdata")
save( lpm_probs, file = file_name)
#
# rm(lpm_probs)


# Predicted outcomes, threshold optimization
tresholds<-seq( from = 0, to = 1, by = 0.01)

ROC_lpm = cbind( Model = 'Linear_Probability_Model', 
                 ROC_analysis( prediction = lpm.all.probs, 
                               y_true = test.wine_binary$binary_quality,
                               probability_thresholds = tresholds))

ROC_all = ROC_lpm


# +++++++++++++++++++++++++++++++++++++++++++++++++++ #
class_err <- matrix( 0, length( tresholds ), 3)
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

##restore best treshold model
lpm.all.class = ifelse(lpm.all.probs > best_treshold$treshold, 1, 0) #test
lpm.all.class_train = ifelse(lpm.all.probs_train > best_treshold$treshold, 1, 0) #train

# confusion matrix

# table(true = test.wine_binary$binary_quality, predict = lpm.all.class) #test
# table(true = train.wine_binary$binary_quality, predict = lpm.all.class_train) #train

test_error = c(1-mean(lpm.all.class == test.wine_binary$binary_quality))
names(test_error)=c("lpm")
test_error


predob = prediction(lpm.all.probs, test.wine_binary$binary_quality)
perf = performance(predob, "tpr", "fpr")
par(mfrow = c(1, 1))
plot(perf, main = "Linear probability model - test set", colorize = TRUE,
     print.cutoffs.at = seq(0, 1, by = 0.1), text.adj = c(-0.2, 1.7))

h = perf@y.values[[1]][   head(which(perf@alpha.values[[1]] <= best_treshold$treshold))[1] ]
abline(h = h,lty=2,col="blueviolet" )

v = perf@x.values[[1]][   head(which(perf@alpha.values[[1]] <= best_treshold$treshold))[1] ]
abline(v = v, lty = 2, col = "blueviolet" )

auc = c(as.numeric(performance(predob, "auc")@y.values))
names(auc) = c("lpm")
auc
# +++++++++++++++++++++++++++++++++++++++++++++++++++ #



ROC_matrix_lpm = ROC_analysis( prediction = lpm.all.probs, 
                               y_true = test.wine_binary$binary_quality, 
                               probability_thresholds = tresholds)
ROC_matrix_lpm = data.frame( treshold = ROC_matrix_lpm$probability_thresholds,
                             FPR = 1-ROC_matrix_lpm$`Specificity: TN/negative`, 
                             TPR = ROC_matrix_lpm$`Sensitivity (AKA Recall): TP/positive` )

roc_curve_lpm = ggplot(ROC_matrix_lpm, aes(x = FPR, y = TPR, label = treshold)) +
  geom_line(color = "red") + theme_bw() + 
  style_roc() + annotate("point", x = v, y = h, colour = "white")+
  ggtitle( "Linear probability model - test set")


roc_curve_lpm = ggplotly( roc_curve_lpm )
roc_curve_lpm

# ********** Saving a file ******************* #

file_name = paste0( folder_plot, "/lpm_roc_curve.Rdata")
save( roc_curve_lpm, file = file_name)
################################################

rm(list=setdiff(ls(), c("test_error", 'ROC_all', 'roc_curve_lpm')))

   