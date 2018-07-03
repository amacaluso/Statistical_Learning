### ***** IMPORT ***** ###
##########################

# SEED = 12344321

source( 'Utils.R')
source( '020_Pre_processing.R') # REQUIRE SEED



### ***** SAVING FOLDER ***** ###
folder = "results/MODELING/CLASSIFICATION"
dir.create( folder )
##################################


# LINEAR PROBABILITY MODEL: BINARY QUALITY
##########################################

lpm.fit.all <- lm(binary_quality ~ ., data = train.wine_binary)
# summary(lpm.fit.all)

lpm_summary = as.data.frame( round( summary(lpm.fit.all)$coefficients, 2))
lpm_summary = cbind( variable = row.names(lpm_summary), lpm_summary )


# ********** Saving a file ******************* #
save_table( df = lpm_summary, type = "CLASSIFICATION")


# Predicted probabilities of quality

lpm.all.probs <- predict(lpm.fit.all, newdata = test.wine_binary) #test
lpm.all.probs_train <- predict(lpm.fit.all, newdata = train.wine_binary) #train

lpm_all_probs = data.frame( lpm.all.probs )
lpm_all_probs$lpm.all.probs = round(lpm_all_probs$lpm.all.probs, 2)

lpm_probs = ggplot( data = lpm_all_probs ) + 
            geom_histogram( aes( x = lpm.all.probs), binwidth = 0.02, color="darkblue", fill="lightblue") + 
            xlab("Predicted values") +  ylab("Frequency") + 
            ggtitle("Linear Probability Model") +
            theme_bw() 

lpm_probs = ggplotly( lpm_probs )

# ********** Saving a file ******************* #
save_plot( lpm_probs, type = "CLASSIFICATION")
#


# Predicted outcomes, threshold optimization
tresholds<-seq( from = 0, to = 1, by = 0.01)

ROC_lpm = cbind( Model = 'Linear_Probability_Model', 
                 ROC_analysis( prediction = lpm.all.probs, 
                               y_true = test.wine_binary$binary_quality,
                               probability_thresholds = tresholds))

best_threshold = ROC_lpm[which.max(ROC_lpm$`Accuracy: true/total`), 'probability_thresholds']

##restore best treshold model
lpm.all.class = ifelse(lpm.all.probs > best_threshold, 1, 0) #test

pred_lpm = prediction(lpm.all.probs, test.wine_binary$binary_quality)
perf = performance(pred_lpm, "tpr", "fpr")

auc = c(as.numeric(performance(pred_lpm, "auc")@y.values))
ROC_lpm$AUC = auc
ROC_all = ROC_lpm


ROC_matrix_lpm = ROC_analysis( prediction = lpm.all.probs, 
                               y_true = test.wine_binary$binary_quality, 
                               probability_thresholds = tresholds)
ROC_matrix_lpm = data.frame( treshold = ROC_matrix_lpm$probability_thresholds,
                             FPR = 1-ROC_matrix_lpm$`Specificity: TN/negative`, 
                             TPR = ROC_matrix_lpm$`Sensitivity (AKA Recall): TP/positive` )

roc_curve_lpm = ggplot(ROC_matrix_lpm, aes(x = FPR, y = TPR, label = treshold)) +
                geom_line(color = "red") + theme_bw() + 
                style_roc() + # annotate("point", x = v, y = h, colour = "white")+
                ggtitle( "Linear probability model - test set")


roc_curve_lpm = ggplotly( roc_curve_lpm )

save_plot( roc_curve_lpm, type = "CLASSIFICATION")
rm(list=setdiff(ls(), c('ROC_all', 'roc_curve_lpm')))

   