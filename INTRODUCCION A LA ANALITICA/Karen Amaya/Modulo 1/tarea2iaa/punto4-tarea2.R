data <- read.csv("costumer_loan_details.csv")
#data[,'gender'] <- as.factor(data[,'gender'])
#data[,'race'] <- as.factor(data[,'race'])
#data[,'marital_status'] <- as.factor(data[,'marital_status'])
#data[,'loan_type'] <- as.factor(data[,'loan_type'])
#data[,'loan_decision_type'] <- as.factor(data[,'loan_decision_type'])

#sum(is.na(data))
#str(data)

#Cree un conjunto de datos de entrenamiento del 75% y el restante 25 % 
#tratelo como datos de test o de prueba

n = nrow(data)
train_ind = sample(1:n, size = round(0.75*n), replace=FALSE)
train = data[train_ind,]
test = data[-train_ind,]

##==================================================
x_train <-data.frame('debts' = as.data.frame(train[,'debts']))
y_train <-data.frame('income' = as.data.frame(train[,"income"]))
y_test <- test[,'income']
###########
data2<- data.frame('debts' = x_train, 'income'= y_train)
colnames(data2) <- c('debts','income' )
############

#implemente Knn (con al menos tres valores para K) usando income como 
#el supervisor y debts como predictor. Grafique 

library(FNN)



par(mfrow=c(2,2))
for (i in 1:4){
  fit.knn <- knn.reg(train = as.data.frame(data2$debts) , 
                   test = as.data.frame(data2$debts) , 
                   as.data.frame(data2$income),k = 4*i, 
                   algorithm=c("brute"))

  plot(data2$debts, data2$income, col="black", ylab = "income",
       xlab = "debts")
  points(data2$debts,fit.knn$pred,pch=1,col="red")
}

#plot
plot(data2$debts, data2$income, col="black", ylab = "income",
     xlab = "debts",main = "Knn con K=16")
points(data2$debts,fit.knn$pred,pch=1,col="red")
legend(x = 'topright', legend = c("Observado", "Predicho"),
       cex = 0.6, border="#000000",fill = c('black','red') )
#Con los datos de entrenamiento, implemente regresion lineal simple usando
#income como el supervisor y debts como predictor. Grafique e interprete.

mod <- lm(income ~ debts, data = data2)
ypred <- mod$fitted.values
plot(data2$debts,data2$income,col="black", ylab = "income",xlab = "debts")
points(data2$debts,ypred,pch=1,col="red")
legend(x = 'bottomright', legend = c("original", "predicción"),  fill = c('black','red') )

summary(mod)
#d)Use los respectivos ajustes de cada uno de los modelos anteriores y 
#con el conjuntode prueba, calcule el test-MSE. ¿Que observa?

#--------------------------------------------------
x_test <- as.data.frame(test[,'debts'])
colnames(x_test)<- 'debts'
test_pred <-predict(mod, newdata = x_test)
test_pred %>% head
length(test_pred)

#MSE MODELO LM
mean((y_test-test_pred)^2)
#--------------------------------------------------
x_test <- as.data.frame(test[,'debts'])

library(FNN)
fit.knn <- knn.reg(train = as.data.frame(data2$debts) , 
                   test = as.data.frame(data2$debts) , 
                   as.data.frame(data2$income),k =16, 
                   algorithm=c("brute"))
ypred_knn <- fit.knn$pred

fit2.knn <- knn.reg(train = as.data.frame(data2$debts) , 
                   test = as.data.frame(x_test) , 
                   as.data.frame(data2$income),k =16, 
                   algorithm=c("brute"))

#MSE MODELO KNN
mean((y_test - fit2.knn$pred)^2)
#data.frame("original"= y_test, 'predicho' = fit2.knn$pred  )





