library(car)
library(rsm)
library(rgl)
library(perturb)
library(leaps)
library(scatterplot3d)
library(olsrr)
library(GGally)
ggplot2::theme_set(ggplot2::theme_bw()) 

#CREANDO FUNCIÓN PARA EXTRAER COEFICIENTES ESTIMADOS SUS IC DEL 95%, VIF'S Y COEFICIENTES ESTANDARIZADOS
miscoeficientes=function(modeloreg,datosreg){
  coefi=coef(modeloreg)
  datos2=as.data.frame(scale(datosreg))
  coef.std=c(0,coef(lm(update(formula(modeloreg),~.+0),datos2)))
  limites=confint(modeloreg,level=0.95)
  vifs=c(0,vif(modeloreg))
  resul=data.frame(Estimación=coefi,Limites=limites,Vif=vifs,Coef.Std=coef.std)
  cat("Coeficientes estimados, sus I.C, Vifs y Coeficientes estimados estandarizados","\n")
  resul
}


datos=read.table(file.choose(),header=T,sep=',')#Leer datos datosRLMCMLOPERA.csv
attach(datos)
names(datos)

#Matriz de dispersión con histogramas en la diagonal
gg2<-ggpairs(datos,upper=list(continuous = wrap("smooth",alpha = 0.3, size=1.2,method = "lm")),lower=list(continuous ="cor"))
for(i in 1:ncol(datos)){
  gg2[i,i]<-gg2[i,i]+
    geom_histogram(breaks=hist(datos[,i],breaks = "FD",plot=F)$breaks,
                   colour = "red",fill="lightgoldenrod1")
}
win.graph()
gg2

pwin.graph()
ggpairs(datos,diag=list(continuous=wrap("box_no_facet",color="red",fill="lightgoldenrod1",alpha=0.3)),upper=list(continuous = wrap("smooth",alpha = 0.3, size=1.2,method = "lm")),lower=list(continuous ="cor"))


modelo3=lm(Y~.,datos[-1]) #No se considera la primera columna de datos que es el ID
summary(modelo3)

#Anova del modelo
anova(rsm(Y~FO(X1,X2,X3,X4,X5,X6,X7,X8,X9)))


##GRÁFICOS DE RESIDUALES
residualPlots(modelo3,tests=FALSE,type="rstudent",quadratic=FALSE,col=2,cex=1.5)


test=shapiro.test(rstudent(modelo3)) #Test de normalidad sobre residuales estudentizados
qqnorm(rstudent(modelo3),cex=2)
qqline(rstudent(modelo3),col=2)
legend("topleft",legend=rbind(c("Statistic W","p.value"),round(c(test$statistic,test$p.value),digits=5)),cex=0.5)


miscoeficientes(modelo,datos[-1])

####MEDIDAS DE INFLUENCIA 
at=influence.measures(modelo3)
a1=influence.measures(modelo3)$is.inf

infIndexPlot(modelo3)
influencePlot(modelo3,xlim=c(0,1),ylim=c(-6.0,4.5))

dfbetaPlots(modelo3,intercept=T,cex=2,col=2)



#con datos centrados
Ind=colldiag(modelo3,center=TRUE)
X=model.matrix(modelo3)[,-1]
val.prop=prcomp(X,center=TRUE,scale=TRUE)$sdev^2
resul2=data.frame(Val.propio=val.prop,Ind.Cond=Ind$condindx,Pi=Ind$pi)
resul2
