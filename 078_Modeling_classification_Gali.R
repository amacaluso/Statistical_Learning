source( 'Utils.R')
SEED = 12344321
source( '020_Pre_processing.R') # REQUIRE SEED










##compute the best treshold model on the whole data
lpm.fit.CLIX <- lm(binary_quality ~ ., data = wine_binary)
lpm.all.probs_CLIX <- predict(lpm.fit.CLIX, newdata = wine_binary) #test
class(lpm.all.probs_CLIX  )
lpm.all.class_CLIX = ifelse(lpm.fit.CLIX$fitted.values > 0.54, 1, 0) #test

ifelse(c(1,2,3,4,5)>3,1,0)

dim(wine_binary)[1]
num <- wine_binary$binary_quality - lpm.all.class_CLIX
den <- 1 - (dim(wine_binary)[2])/dim(wine_binary)[1]
GCV_error <- 1/dim(wine_binary)[1] * sum( ( num / den )^2 )
















ensureLibrary('tree')
ensureLibrary('rpart')

n = nrow(train.wine_binary)

# La function che implementa la crescita dell’albero è rpart
?rpart		      # si veda la sintassi e rpart.control
Tmax <- rpart(factor(binary_quality) ~ ., data = train.wine_binary, control = rpart.control( minsplit=2, cp=0.00000001 )) 
plot(Tmax) 	   # disegna l’albero
text(Tmax)  		 # etichetta i nodi (con varie migliorie grafiche); posso commentare i primi split

Tmax$cptable
#Calcolo Cross-validation per ogni CP
cv_error=( xpred.rpart( Tmax, xval = 1000 ))
#Calcolo errore di cross-validation per ogni CP
y = train.wine_binary$binary_quality
xerr=(cv_error-y)^2
standardError=apply(xerr,2,mean)/var(y)
cp=as.numeric(names(standardError))
#plot(cbind(cp,standardError),xlim=c(0,0.009)
#Selezione CP con errore minimo
min_cp=which.min(standardError)
#Calcolo albero ottimo
treeFinal=rpart(factor(binary_quality) ~ ., data = train.wine_binary, control = rpart.control(cp = cp[min_cp]),method = "class")
plot( treeFinal ) 	   # disegna l’albero
text( treeFinal )  		 # etichetta i nodi (con varie migliorie grafiche); posso commentare i primi split


# risultati del  pruning selettivo e corrispondente rappresentazione grafica
ris <- printcp(Tmax)
# printcp riporta la sequenza finita di sottoalberi annidati di Tmax ottenuta minimizzando la funzione
# costo-complessità per valori decrescenti del parametro di penalizzazione, . Tale parametro è qui
# denotato con CP (Complexity Parameter).  Per ognuno dei diversi valori di CP indicati, considerato il
# corrispondente sottoalbero ottimale T , sono riportate alcune informazioni ad esso relative:
# nsplit = numero di foglie di T - 1 (n.b. npslit=0)
# relative error = Relative Squared Error(T) = SSE(T)/SSE(radice)  (0,1) calcolato sul training
# xerror = stima di cross-validation dell’ “errore quadratico relativo atteso” di T
# xstd = errore standard (misura di variabilità) della stima puntuale riportata in xerror 

# osservare l’andamento (al crescere del nr delle foglie di T) di relative error e di  xerror
par(mfrow=c(1,2))
rsq.rpart(Tmax)

# cerchiamo, nella sequenza di sottoalberi di Tmax, quello “migliore”: 
which.min(ris[,4]) 

#(n.b. per selezionare dalla sequenza un particolare sottoalbero si usa la funzione prune
# impostando l’argomento cp ad un valore di poco superiore a quello corrispondente al sottoalbero che
# si vuole selezionare):
albero <- prune(Tmax, cp=0.0037968054) # 23 foglie
# A&S si soffermano per ragioni di semplicità interpretativa sul sottoalbero con nsplit=6 
# albero <- prune(Tmax, cp=0.018) # 7 foglie (J=nsplit+1)
par(mfrow=c(1,1)) 
plot(albero)
text(albero)
# il primo split è fatto rispetto a out.dur.offpeak (uno dei 2 addendi di Y) osservata nel mese che
# precede quello di interesse. E lo stesso vale per molti altri split; in generale, si nota che quasi tutti
# gli split sono fatti rispetto a variabili relative al comportamento del cliente nel mese precedente.
# NB: operata drastica selezione dei regressori!

# Interpretazione del modello ad albero selezionato:
albero

# La procedura ha lavorato su 10179 unità (le unità del training set).
# Ogni riga di questo output descrive un nodo
# Con un asterisco sono evidenziate le foglie.
# Per ogni nodo, sono elencati 5 argomenti, ognuno dei quali riporta un’informazione relativa al nodo:

# l’argomento node) etichetta numericamente, con criterio genealogico, i nodi da sx a dx 
# e dall’alto al basso. Ogni riga è indentata secondo il livello dell’albero in cui figura il nodo
# (anche la numerazione dipende dal livello, e procede come descritto a lezione):
# N.B.: mancano i nodi 8 e 9.

# l’argomento split è la “caratterizzazione del nodo” (si noti che il nodo radice non ha
# alcuna caratterizzazione, ma è solo indicato come root (= radice)). Per esempio, lo split del
# nodo radice è fatto rispetto alla variabile q09.out.dur.offpeak.
# Si ricordi la convenzione che si adotta nella scrittura dello split:
# il figlio sx(dx) comprende le unità per le quali la regola logica è vera(falsa); 

# l’argomento n è il numero di unità contenute nel nodo: (infatti, per la radice, l’argomento
# vale 10179). Inoltre,
# root.sx <- data.tr[data.tr$q09.out.dur.offpeak < 3188.5,]
# nrow(root.sx)
# root.dx <- data.tr[data.tr$q09.out.dur.offpeak >= 3188.5,]
# nrow(root.dx)


# l’argomento deviance è DevTOT(Y) nel nodo;
# l’argomento yval è il valore previsto di Y sul nodo = , cioè la media
# aritmetica dei valori Y osservati sulle unità del training set che appartengono a quel nodo:

# by(data.tr$y, data.tr$q09.out.dur.offpeak<3188.5, mean)
# boxplot(data.tr$y~data.tr$q09.out.dur.offpeak<3188.5)


# Nel grafico ad albero, le foglie sono etichettate con il corrispondente valore di  . 
# Si noti che il valore di yval per le foglie (= altezza del gradino) va da un minimo di 1034 ad un
# massimo di 162214: si caratterizzano i clienti più "preziosi" e quelli "meno preziosi" per l’azienda.

# calcoliamo i valori previsti di Y dal modello albero per le unità del test set,
# per poter costruire poi, confrontando tali valori con quelli realmente osservati, opportune misure di 
# valutazione della performance previsiva del modello su nuove unità statistiche
Y.previsto.albero.ts <- predict(albero, test.wine_binary)

ROC_analysis( Y.previsto.albero.ts[,2], test.wine_binary$binary_quality)


# errore quadratico
MSE.albero.ts <- mean((Y.previsto.albero.ts - test.wine_binary$binary_quality)^2)
RMSE.albero.ts <- sqrt(MSE.albero.ts)		# 4828.32
SSE.albero.ts <- sum((Y.previsto.albero.ts-test.wine_binary$binary_quality)^2)
RelativeSquaredError.albero.ts <- SSE.albero.ts/(var(test.wine_binary$binary_quality)*(nrow(test.wine_binary)-1))

# Il complemento a 1 di RSE si interpreta in modo equivalente a R2:
R2.albero.ts<- 1 - RelativeSquaredError.albero.ts   # 0.61

# errore assoluto
MAE.albero.ts <- mean(abs(Y.previsto.albero.ts-test.wine_binary$binary_quality))  # 1769.565


#############################################################################
#############################################################################

ensureLibrary('randomForest')

rf = randomForest(factor(binary_quality) ~ ., data = train.wine_binary, importance =T)

plot(rf)
rf$oob.times


Y_RF <- predict(rf, test.wine_binary, type = 'prob')[,2]

ROC_analysis( Y_RF, test.wine_binary$binary_quality)

rf$importance

oob.err=double(13)
test.err=double(13)

#mtry is no of Variables randomly chosen at each split
for(mtry in 1:13) 
{
  rf=randomForest(factor(binary_quality) ~ . , data = train.wine_binary ,mtry=mtry,ntree=400) 
  oob.err[mtry] = rf$err.rate[400] #Error of all Trees fitted
  
  pred<-predict(rf, test.wine_binary) #Predictions on Test Set for each Tree
  test.err[mtry]= mean((as.numeric(test.wine_binary$binary_quality) - (as.numeric(pred)-1))^2) #Mean Squared Test Error
  
  cat(mtry," ") #printing the output to the console
  
}

matplot(1:mtry , cbind(oob.err,test.err), pch=19 , col=c("red","blue"),type="b",ylab="Accuracy",
        xlab="mtry", main = 'Random Forest')
legend("topright",legend=c("Out of Bag Error","Test Error"),pch=19, col=c("red","blue"))
