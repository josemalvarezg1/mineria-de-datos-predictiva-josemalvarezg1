setwd("C:/Users/Jos� Manuel/Documents/ICD/mineria-de-datos-predictiva-josemalvarezg1")

# ------- Pre-procesamiento ------- 

#Se lee el dataset AdultData.
adultsData = read.csv(file = "data/adult.data", header = F)
#Se lee el dataset AdultTest.
adultsTest = read.csv(file = "data/adult.test", header = F)
#Se representan en un dataframe Adults como un todo.
adults <- rbind(adultsData, adultsTest) 

#Se identifican las columnas
colnames(adults) <- c("Edad", "Tipo_empleo", "Peso_final", "Educaci�n", "N�mero_educaci�n", "Estado_civil", "Ocupaci�n", "Relaci�n", "Raza", "Sexo", "Ganancia_capital", "P�rdida_capital", "Horas_por_semana", "Pa�s_natal", "Clase")

#Algunas clases tienen un punto al final; se elimina ese punto

adults$Clase <- sub("50K.", "50K", adults$Clase)

#Se ignoran los registros que contienen valores desconocidos
adults <- na.omit(adults[1:15])

# ------- �rboles de Decisi�n ------- 

library("rpart")
library("rpart.plot")

#Se obtiene el n�mero de registros del dataset
n <- nrow(adults)
#Se barajea el dataset para no depender de un orden en espec�fico
shuffled <- adults[sample(n),]
#Se obtiene el �ndice de training
train_indices <- 1:round(0.7 * n)
#Se obtiene el �ndice de testing
test_indices <- (round(0.7 * n) + 1):n
#Se obtiene el conjunto de training
train <- shuffled[train_indices,]
#Se obtiene el conjunto de testing
test <- shuffled[test_indices,]
#Se obtiene el �rbol de decisi�n
tree <- rpart(Clase ~ ., train, method = "class")

#Se obtienen los valores probables
all_prob <- predict(tree,test,type="prob")
prob <- all_prob[,2]

#Se grafica el �rbol de decisi�n
rpart.plot(tree)

# ------- Curvas ROC ------- 

library("ROCR")

set.seed(1)
#Se obtiene el �rbol de decisi�n nuevamente
tree <- rpart(Clase ~ ., train, method = "class")
prob <- predict(tree, test, type = "prob")[,2]
prob_tree <- prob
pred <- prediction(prob,test$Clase)

#Se obtiene la tasa de verdaderos y falsos positivos
perf <- performance(pred,"tpr","fpr")

#Se grafica la tasa anterior
plot(perf)

#Se obtiene el �rea bajo la curva
set.seed(1)
tree <- rpart(Clase ~ ., train, method = "class")
prob <- predict(tree, test, type = "prob")[,2]
prob_curve <- prob
pred <- prediction(prob,test$Clase)

perf <- performance(pred,"auc")
#Se obtiene el porcentaje de precisi�n
perf@y.values[[1]] * 100

#Se comparan los m�todos
pred_tree <- prediction(prob_tree,test$Clase)
pred_curve <- prediction(prob_curve,test$Clase)

perf_tree <- performance(pred_tree,"tpr","fpr")
perf_curve <- performance(pred_curve,"tpr","fpr")

#Se grafica el desempe�o de ambos m�todos
plot(perf_tree)
plot(perf_curve)