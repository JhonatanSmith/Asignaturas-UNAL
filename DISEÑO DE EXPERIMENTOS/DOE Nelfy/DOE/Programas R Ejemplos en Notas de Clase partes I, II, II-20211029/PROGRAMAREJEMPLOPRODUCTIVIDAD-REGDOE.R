library(gmodels)
library(multcomp)
library(daewr)
library(car)

#CREANDO FUNCIÓN USUARIO mismediastratamientos()
mismediastratamientos=function(modeloanova,nivel=95){
MSE=anova(modeloanova)["Mean Sq"][2,]
df=anova(modeloanova)["Df"][2,]
ni=unlist(model.tables(modeloanova,type = "means")["n"])
alfa=1-nivel/100
alfa.med=(1-(nivel/100))/2
t=qt(alfa.med,df=df,lower.tail=F)
medias.tratam=unlist(model.tables(modeloanova,type = "means")["tables"])[-1]
interval=cbind(ni=ni,Medias=medias.tratam,LIC=medias.tratam-t*sqrt(MSE/ni),LSC=medias.tratam+t*sqrt(MSE/ni))
cat("Tabla de medias de tratamientos y sus I.C de",nivel,"%","\n")
cat("alfa","               ",alfa,"\n")
cat("grados de libertad    ",df,"\n")
cat("error cuadrático medio",MSE,"\n")
cat("valor crítico t       ",t,"\n","\n")
interval
}


#ENTRADA DE DATOS:
ni=c(6,9,12)
diseño=data.frame(ID=factor(rep(c("alto","bajo","medio"),times=ni)),mprod=scan())
8.5 9.7 10.1 7.8 9.6 9.5
7.6 8.2 6.8 5.8 6.9 6.6 6.3 7.7 6.0 
6.7 8.1 9.4 8.6 7.8 7.7 8.9 7.9 8.3 8.7 7.1 8.4

diseño

attach(diseño)

#CALCULANDO MEDIAS DE TRATAMIENTO PARA LUEGO USAR EN GRÁFICA DE BOXPLOTS
mediasy=sapply(split(mprod,ID),mean)
#BOXPLOTS COMPARATIVOS
boxplot(mprod~ID,boxwex = 0.5)
lines(1:3,mediasy,col=2,lty=2,type="b",pch=19)

#AJUSTE MODELO ANOVA UN FACTOR
modelo=aov(mprod~ID)

#OBTENIENDO TABLA ANOVA
anova(modelo) 
#o bien 
summary(modelo)

#OBTENIENDO MEDIAS ESTIMADAS POR NIVEL DEL FACTOR
model.tables(modelo,type = "means",se=TRUE)

#OBTENCIÓN DE MEDIAS DE TRATAMIENTO CON SUS I.C DEL 95%
mismediastratamientos(modelo)
library(lsmeans)
lsmeans(modelo,~ID) #esta función hace lo mismo que mismediastratamientos()
lsmeans(modelo,"ID")

#ESTIMACIÓN DE LOS EFECTOS PRINCIPALES:
model.tables(modelo,type = "effects",se=TRUE)


#CÁLCULO DE LOS EFECTOS DE TRATAMIENTOS IGNORANDO DESBALANCE, SUS TESTES T Y I.C DEL 95%:
#La función fit.contrast requiere la librería gmodels
efect.alto=fit.contrast(modelo,"ID",rbind(":efecto alto"=c(2/3,-1/3,-1/3)),conf=0.95)
efect.bajo=fit.contrast(modelo,"ID",rbind(":efecto bajo"=c(-1/3,2/3,-1/3)),conf=0.95)
efect.medio=fit.contrast(modelo,"ID",rbind(":efecto medio"=c(-1/3,-1/3,2/3)),conf=0.95)
rbind(efect.alto,efect.bajo,efect.medio)

#OTRA FORMA PARA CÁLCULO INDIVIDUAL DE EFECTOS CON SUS INTERVALOS DE CONFIANZA DEL 95%:
#LA FUNCIÓN glht REQUIERE LA LIBRERÍA multcomp
contr.alto= rbind("efecto id alto"= c(2/3,-1/3,-1/3))
contr.bajo= rbind("efecto id bajo"= c(-1/3,2/3,-1/3))
contr.medio= rbind("efecto id medio"= c(-1/3,-1/3,2/3))

rbind(confint(glht(modelo,linfct=mcp(ID=contr.alto)))$confint,
confint(glht(modelo,linfct=mcp(ID=contr.bajo)))$confint,
confint(glht(modelo,linfct=mcp(ID=contr.medio)))$confint) 

#CORRECCIÓN POR DESBALANCE, DE LA ESTIMACIÓN PUNTUAL Y POR I.C DEL 95% PARA EFECTOS DE TRATAMIENTOS
efect.alto=fit.contrast(modelo,"ID",rbind(":efecto alto"=c(21/27,-9/27,-12/27)),conf=0.95)
efect.bajo=fit.contrast(modelo,"ID",rbind(":efecto bajo"=c(-6/27,18/27,-12/27)),conf=0.95)
efect.medio=fit.contrast(modelo,"ID",rbind(":efecto medio"=c(-6/27,-9/27,15/27)),conf=0.95)
rbind(efect.alto,efect.bajo,efect.medio)

contr.alto= rbind("efecto id alto"= c(21/27,-9/27,-12/27))
contr.bajo= rbind("efecto id bajo"= c(-6/27,18/27,-12/27))
contr.medio= rbind("efecto id medio"= c(-6/27,-9/27,15/27))

rbind(confint(glht(modelo,linfct=mcp(ID=contr.alto)))$confint,
confint(glht(modelo,linfct=mcp(ID=contr.bajo)))$confint,
confint(glht(modelo,linfct=mcp(ID=contr.medio)))$confint) 

#GRÁFICOS PARA ANÁLISIS DE RESIDUOS INTERNAMENTE ESTUDENTIZADOS
layout(rbind(c(1,1,2,2),c(0,3,3,0)))
stripchart(rstandard(modelo)~ID,vertical=TRUE,ylim=c(-2.5,2.5),pch=1,cex=1)
abline(h=c(-2,0,2),lty=2)
plot(fitted(modelo),rstandard(modelo),ylim=c(-2.5,2.5))
abline(h=c(-2,0,2),lty=2)
qqnorm(rstandard(modelo))
qqline(rstandard(modelo),lty=2)

plot(modelo)

#TESTS DE HOMOGENEIDAD DE VARIANZAS
bartlett.test(mprod~ID,data=diseño)

library(car)
leveneTest(mprod~ID,data=diseño,center="median") #usando la mediana para centrar
leveneTest(mprod~ID,data=diseño,center="mean") #usando la media para centrar
leveneTest(mprod~ID,data=diseño,center="mean",trim=0.1) #usando media recortada al 10% para centrar

#O bien 
library(lawstat)
levene.test(diseño$mprod,diseño$ID,location="median") #usando la mediana para centrar
levene.test(diseño$mprod,diseño$ID,location="mean")#usando la media para centrar
levene.test(diseño$mprod,diseño$ID,location="trim.mean",trim.alpha=0.1) #usando media recortada al 10% para centrar

#INTERVALOS DE TUKEY
TukeyHSD(modelo, conf.level = 0.95) #Función es cargada por defecto por librería stats 
plot(TukeyHSD(modelo, conf.level = 0.95),cex.lab=0.8,las=1)

library(agricolae)
HSD.test(modelo,"ID", group=TRUE,console=TRUE) #Comparaciones de Tukey
duncan.test(modelo,"ID",alpha=0.05,group=TRUE,console=TRUE) #Rango múltiple de Duncan
LSD.test(modelo,"ID",group=TRUE,console=TRUE) #Método LSD

#Uso librería multcomp para Dunnett. Como Dunnett asume que primer nivel es el control
#Para cambiar el nivel de control usamos relevel
ID2=relevel(ID,ref="bajo")
modelo2=aov(mprod~ID2)
summary(glht(modelo2, linfct=mcp(ID2="Dunnett"),alternative="two.sided")) 
confint(glht(modelo2, linfct=mcp(ID2="Dunnett"),alternative="two.sided"))

#CONTRASTE ID: ALTO VS. DEMÁS, TEST T Y I.C DEL 95%:
fit.contrast(modelo,"ID",rbind(":ALTO VS. DEMAS"=c(1,-1/2,-1/2)),conf=0.95)

#OTRA FORMA PARA CÁLCULO DEL CONTRASTE ID. ALTO VS. DEMÁS, Y SU I.C DEL 95%
contrasteb=rbind("alto vs. demás "=c(1,-1/2,-1/2))
confint(glht(modelo,linfct=mcp(ID=contrasteb),alternative="two.sided"))
summary(glht(modelo,linfct=mcp(ID=contrasteb),alternative="two.sided"))

#TEST ID. ALTO MAYOR QUE DEMÁS, 
summary(glht(modelo,linfct=mcp(ID=contrasteb),alternative="greater"))

detach(diseño)




