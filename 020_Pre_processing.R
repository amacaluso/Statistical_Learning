### **** Importazione dati e Descrittive **** ###

source( 'Utils.R')

ensureLibrary('MASS')
ensureLibrary('glmnet')
ensureLibrary('ggplot2')
ensureLibrary('parallel')
ensureLibrary('caret')
ensureLibrary('e1071')
ensureLibrary('caTools')
ensureLibrary('data.table')
ensureLibrary('ROCR')
ensureLibrary('glmnet')
ensureLibrary('class')
ensureLibrary('plotly')
# ensureLibrary('devtools')
ensureLibrary('gridExtra')
ensureLibrary('plotROC')
ensureLibrary('corrplot')
ensureLibrary("GGally")
ensureLibrary('ggcorrplot')
ensureLibrary('car')
ensureLibrary('leaps')
ensureLibrary('pls')
ensureLibrary('gam')


### ***** SAVING FOLDER ***** ###

folder = "results/EXPLORATORY_ANALYSES"
dir.create( folder )

##################################



red <- read.table( 'data/winequality-red.csv', sep = ";" , header = T)
white <- read.table( 'data/winequality-white.csv' ,sep=";" , header = T )


#create joint dataset
red$type <- 0 #"red"
white$type <- 1 #"white"

wine <- rbind.data.frame( red, white)
wine$binary_quality <- ifelse( wine$quality <= 5, yes = 0, no = 1) #as.factor(ifelse(wine$quality<=5,0,1))

wine_binary = remove_columns_by_names( df = wine, colNames = 'quality')
wine = remove_columns_by_names( df = wine, colNames = 'binary_quality')


# rm( n_var, plots, variables, density_variables)

# SEED = 12344321
set.seed( SEED )
cat( "SEED is", SEED, "\n")


train.label = sample.split(wine_binary, SplitRatio = 3/5)
cat("SplitRatio is 3/5")


# Check that the split is balanced

train.wine_binary = wine_binary[train.label, ]
train.wine = wine[train.label, ]

test.wine_binary = wine_binary[!train.label,]
test.wine = wine[!train.label,]



