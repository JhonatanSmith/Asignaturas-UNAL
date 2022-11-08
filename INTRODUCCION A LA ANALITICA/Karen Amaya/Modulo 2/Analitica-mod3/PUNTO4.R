#PUNTO 4

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







