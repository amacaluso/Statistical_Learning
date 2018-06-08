
#######################################
#### DATA PREPARATION #################
#######################################

redwine_data = read.csv( "data/winequality-red.csv", header = T, sep = ";")
redwine_data$type = "red"
### 
whitewine_data = read.csv( "data/winequality-white.csv", header = T, sep = ";")
whitewine_data$type = "white"

wine_data = rbind( redwine_data, whitewine_data )

# table( wine_data$quality)
# pairs( wine_data[ , -13] )
# summary( wine_data )

means = apply( wine_data[ , -13 ], MARGIN = 2, FUN = mean )
std_dev = apply( wine_data[ , -13 ], MARGIN = 2, FUN = sd )

CV = std_dev / means

dataset = na.fail( wine_data )


n = nrow( dataset )    # Number of observations in training set
p = ncol( dataset ) - 2  # The last column is the response 

seed = 100
set.seed( seed ) 

sample_id <- sample.int(n = nrow( dataset ), size = floor(0.75 * nrow( dataset )), replace = F)
train_dataset <- dataset[ sample_id , ]
test_dataset  <- dataset[ -sample_id , ]