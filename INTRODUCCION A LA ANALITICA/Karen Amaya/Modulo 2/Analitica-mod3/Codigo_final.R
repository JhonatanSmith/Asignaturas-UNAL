#Punto 1
library(kableExtra) #tablas
library(ISLR)
library(tree)
library(randomForest)
n <- length(Carseats$Sales)
set.seed(1)
train<- sample (n,(n*0.7))
arbol = tree(Sales~.,Carseats, subset = train)
summary(arbol)
plot(arbol)
text(arbol,pretty=0)
yhat = predict(arbol ,newdata = Carseats[-train ,])
arbol.test = Carseats[-train ,"Sales"]
mean((yhat-arbol.test)^2)
cv.arbol =cv.tree(arbol)
plot(cv.arbol$size ,cv.arbol$dev,type="b")
prune.carseats<-prune.tree(arbol,best=8)
plot(prune.carseats)
text(prune.carseats) 
yphat=predict(prune.carseats,Carseats[-train,])
arbolp.test = Carseats[-train ,"Sales"]
mean((yphat-arbolp.test)^2)
set.seed(1)
arbolbag<-randomForest(Sales~.,data=Carseats,subset=train,mtry=10,importance=T)
ybhat<-predict(arbolbag,Carseats[-train,])
mean((ybhat-Carseats[-train,"Sales"])^2)
importance(arbolbag) %>% kbl() %>% kable_styling()
set.seed(1)
bosque<-randomForest(Sales~.,data=Carseats,subset=train,mtry=5,importance=T)
yhat.rf<-predict(bosque,Carseats[-train,])
mean((yhat.rf-Carseats[-train,"Sales"])^2)
importance(bosque) %>% kbl() %>% kable_styling()
MSE=c()
set.seed(1)
for(i in 3:10){
  bosqueit=randomForest(Sales~.,data=Carseats,subset=train,mtry=i,importance=T)
  yhat.rfi=predict(bosqueit,Carseats[-train,])
  MSE<-rbind(MSE,mean((yhat.rfi-Carseats[-train,"Sales"])^2))
}
plot(3:10,MSE,type="b")

#Punto 2
#librerias
library(e1071)
library(ISLR)

data(Auto)
m <- median(Auto$mpg)
y <- ifelse(Auto$mpg>m, 1,0)
y <- as.factor(y)
dat<- data.frame(Auto,y)

#validacion cruzada para mirar el mejor smv con varios valores para cost
#kernel linear
set.seed(32668)
vec <- tune(svm ,y~.,data=dat ,kernel ="linear",
            ranges =list(cost=c( 0.01, 0.1,0.5, 1,10,100,1000) ))

summary(vec)

#kernel radial
set.seed(23522)
vec <- tune(svm , y~., data=dat, kernel = "radial",
            ranges=list(cost=c(0.1,1,10,100,1000),
                        gamma=c(0.5,1,2,3,4) ))

summary(vec)

#kernel polynomial
set.seed(8527)
tune.out=tune(svm , y~., data=dat, kernel = "polynomial",
              ranges=list(cost=c(0.01,0.1,1),degree=c(2,3,4),
                          gamma=c(0.5,1,2,3) ))

summary(tune.out)

#Graficos
#modelo ajustado kernel linear
svm1 =svm(y~., data=dat, kernel = "linear",cost=1)

plot(svm1, data = dat, mpg~horsepower,col=c("azure","deeppink1"))
plot(svm1, data = dat, mpg~cylinders,col=c("azure","deeppink1"))
plot(svm1, data = dat, mpg~displacement,col=c("azure","deeppink1"))
plot(svm1, data = dat, mpg~weight,col=c("azure","deeppink1"))
plot(svm1, data = dat, mpg~acceleration,col=c("azure","deeppink1"))
plot(svm1, data = dat, mpg~year,col=c("azure","deeppink1"))
plot(svm1, data = dat, mpg~origin,col=c("azure","deeppink1"))

#modelo ajustado  kernel radial
svm2 =svm(y~., data=dat, kernel ="radial",cost=10,gamma=0.5)

plot(svm2, data = dat, mpg~horsepower,col=c("azure","deeppink1"))
plot(svm2, data = dat, mpg~cylinders,col=c("azure","deeppink1"))
plot(svm2, data = dat, mpg~displacement,col=c("azure","deeppink1"))
plot(svm2, data = dat, mpg~weight,col=c("azure","deeppink1"))
plot(svm2, data = dat, mpg~acceleration,col=c("azure","deeppink1"))
plot(svm2, data = dat, mpg~year,col=c("azure","deeppink1"))
plot(svm2, data = dat, mpg~origin,col=c("azure","deeppink1"))


#modelo ajustado  kernel polinomial
svm3 =svm(y~., data=dat, kernel="polynomial",
          cost=0.1,gamma=2,degree=3)

plot(svm3, data = dat, mpg~horsepower,col=c("azure","deeppink1"))
plot(svm3, data = dat, mpg~cylinders,col=c("azure","deeppink1"))
plot(svm3, data = dat, mpg~displacement,col=c("azure","deeppink1"))
plot(svm3, data = dat, mpg~weight,col=c("azure","deeppink1"))
plot(svm3, data = dat, mpg~acceleration,col=c("azure","deeppink1"))
plot(svm3, data = dat, mpg~year,col=c("azure","deeppink1"))
plot(svm3, data = dat, mpg~origin,col=c("azure","deeppink1"))

#Punto 3
library(dplyr)
library(ISLR)
library(e1071)

data <- ISLR::OJ

# a. Datos
set.seed(1234)
subset <- sample((1:nrow(data)), 800)
train <- data[subset,] 
test <- data[-subset,]

# b. Clasificador con kernel lineal
svm_linear <- svm(Purchase~., data=train , kernel = "linear", cost = 0.1,
                  scale = FALSE )
summary(svm_linear)

# c. tasas de error
# Conjunto de entrenamiento
train_linear <- predict(svm_linear, train)
table(prediccion = train_linear, real = train$Purchase)
print("Porcentaje mal clasificado:")
100 * mean(train$Purchase != train_linear)

# Conjunto de prueba
test_linear <- predict(svm_linear, test)
table(prediccion = test_linear, real = test$Purchase)
print("Porcentaje mal clasificado:")
100 * mean(test$Purchase != test_linear)

# d. Uso de función tune()
tune_linear <- tune(svm, Purchase~., data=train ,kernel ="linear",
                    ranges = list(cost=c(0.01, 0.1, 1,5,10)))
summary(tune_linear)

# Mejor modelo
best_linear <- tune_linear$best.model
summary(best_linear)

# e. Tasas de error mejor modelo
# Conjunto de entrenamiento
pred_best_linear <- predict(best_linear, train)
table(prediccion = pred_best_linear, real = train$Purchase)
print("Porcentaje mal clasificado:")
100 * mean(train$Purchase != pred_best_linear)

# Conjunto de prueba
pred_best_linear <- predict(best_linear, test)
table(prediccion = pred_best_linear, real = test$Purchase)
print("Porcentaje mal clasificado:")
100 * mean(test$Purchase != pred_best_linear)

# f. Clasificador con kernel radial
svm_radial <- svm(Purchase~., data = train, kernel = "radial", cost =0.1)
summary(svm_radial)

train_radial <- predict(svm_radial, train)
table(prediccion = train_radial, real = train$Purchase)
print("Porcentaje mal clasificado:")
100 * mean(train$Purchase != train_linear)

test_radial <- predict(svm_radial, test)
table(prediccion = test_radial, real = test$Purchase)
print("Porcentaje mal clasificado:")
100 * mean(test$Purchase != test_linear)

tune_radial <- tune(svm, Purchase~., data=train ,kernel ="radial",
                    ranges = list(cost=c(0.01, 0.1, 1,5,10)))
summary(tune_radial)

best_radial <- tune_radial$best.model
summary(best_radial)

pred_best_radial <- predict(best_radial, train)
table(prediccion = pred_best_radial, real = train$Purchase)
print("Porcentaje mal clasificado:")
100 * mean(train$Purchase != pred_best_radial)

pred_best_radial <- predict(best_radial, test)
table(prediccion = pred_best_radial, real = test$Purchase)
print("Porcentaje mal clasificado:")
100 * mean(test$Purchase != pred_best_radial)

# g. Clasificador con kernel polinomial
svm_poli <- svm(Purchase~., data = train, kernel = "polynomial",
                degree = 2, cost = 0.1)
summary(svm_poli)

train_poli <- predict(svm_poli, train)
table(prediccion = train_poli, real = train$Purchase)
print("Porcentaje mal clasificado:")
100 * mean(train$Purchase != train_poli)

test_poli <- predict(svm_poli, test)
table(prediccion = test_poli, real = test$Purchase)
print("Porcentaje mal clasificado:")
100 * mean(test$Purchase != test_poli)

tune_poli <- tune(svm, Purchase~., data=train ,kernel ="polynomial",
                  degree = 2,ranges = list(cost=c(0.01, 0.1, 1,5,10)))
summary(tune_poli)

best_poli <- tune_poli$best.model
summary(best_poli)

pred_best_poli <- predict(best_poli, train)
table(prediccion = pred_best_poli, real = train$Purchase)
print("Porcentaje mal clasificado:")
100 * mean(train$Purchase != pred_best_poli)

pred_best_poli <- predict(best_poli, test)
table(prediccion = pred_best_poli, real = test$Purchase)
print("Porcentaje mal clasificado:")
100 * mean(test$Purchase != pred_best_poli)

#Punto 4
#librerias
library(FactoMineR)#PCA 
library(factoextra)
library(gridExtra)#par
library(kableExtra)#tablas
library(dplyr)

#A
set.seed(123)
df<- data.frame(matrix(nrow=60,ncol = 50))
for(i in 1:50){
  a=rnorm(n = 20,52,3)
  b=rnorm(n = 20,72,5)
  c=runif(n = 20,min = 30,max = 55)
  df[,i] <- c(a,b,c)
}
df$clase <- c(rep("1",20),rep("2",20),rep("3",20))

#B

#Ajuste
res.pca <- PCA(df[,-51],graph =F)
#Gráfico
fviz_pca_ind(res.pca,
             geom.ind = "point", #mostrar puntos
             col.ind = df$clase, #clases
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, #elipses
             legend.title = "Clases"
)+ theme_gray()



#C

set.seed(123)
km.out =kmeans (df[,-51],3, nstart =20)
grid.arrange(
  fviz_pca_ind(res.pca,
               geom.ind = c("point","text"), #mostrar puntos
               col.ind = df$clase, #clases
               palette = c("#00AFBB", "#E7B800", "#FC4E07"),
               addEllipses = TRUE, #elipses
               legend.title = "Clases")+ theme_gray(),
  fviz_cluster(km.out, data = df[,-51]))

km2 <- km.out$cluster
for(i in 1:length(km2)){
  if(km2[i]==1){
    km2[i]<-2
  }else if(km2[i]==2){
    km2[i]<- 1
  }else{
    next
  }}; km2

table(km2,df$clase)

#D

set.seed(123) #semilla
km.out =kmeans (df[,-51],2, nstart =20)
fviz_cluster(km.out, data = df[,-51])

#E

set.seed(123) #semilla
km.out =kmeans(df[,-51],4, nstart =20)
fviz_cluster(km.out, data = df[,-51])

#F

Z1 <- res.pca$ind$coord[,1]
Z2 <- res.pca$ind$coord[,2]
Z<- data.frame(Z1,Z2)
set.seed(123)
km.out =kmeans(Z,3, nstart =20)
fviz_cluster(km.out, data = Z)

#G

set.seed(123)
sd.data=scale(df[,-51])
km.out =kmeans (sd.data,3, nstart =20)
fviz_cluster(km.out, data = sd.data)

#Punto 5
#Librerias
library(ISLR)
library(factoextra)
hc.complete =hclust(dist(USArrests), method ="complete")
hc.complete
plot(hc.complete ,main =" Complete Linkage ", xlab="", sub ="",
     cex =0.7)
clust <- cutree(hc.complete, k = 3)
fviz_cluster(list(data = USArrests, cluster = clust))  
plot(hc.complete, hang = -1, cex = 0.6)
rect.hclust(hc.complete, k = 3, border = c("red","green","blue"))
USAsc=scale(USArrests)
hc.scale = hclust(dist(USAsc), method ="complete")
plot(hc.scale, main ="Hierarchical Clustering with Scaled Features", xlab="", sub ="",
     cex =0.7)
clust <- cutree(hc.scale, k = 3)
fviz_cluster(list(data = USAsc, cluster = clust)) 
plot(hc.scale, hang = -1, cex = 0.6)
rect.hclust(hc.scale, k = 3, border = c("skyblue","green","red"))