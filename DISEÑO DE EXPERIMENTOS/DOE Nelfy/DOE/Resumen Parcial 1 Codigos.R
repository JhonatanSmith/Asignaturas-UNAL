# PARCIAL 1

#  ANOVA -----------------------------------------------------------------------
modelo <- aov(y~x)
summary(modelo)
anova(modelo)

# COMPARACIONES DE MEDIAS ------------------------------------------------------
library(lsmeans)
lsmeans(modelo2,~nitrato) #esta funcion hace lo mismo que mismediastratamientos()
lsmeans(modelo2,"nitrato")

#ESTIMACI?N DE LOS EFECTOS PRINCIPALES:
model.tables(modelo2,type = "effects",se=TRUE)

#C?LCULO INDIVIDUAL DE EFECTOS, SUS TESTES T Y I.C DEL 95%:
efect.0=fit.contrast(modelo2,"nitrato",
                     rbind(":efecto 0"=c(4/5,-1/5,-1/5,-1/5,-1/5)),conf=0.95)
efect.50=fit.contrast(modelo2,"nitrato",
                      rbind(":efecto 50"=c(-1/5,4/5,-1/5,-1/5,-1/5)),conf=0.95)
efect.100=fit.contrast(modelo2,"nitrato",
                       rbind(":efecto 100"=c(-1/5,-1/5,4/5,-1/5,-1/5)),conf=0.95)
efect.150=fit.contrast(modelo2,"nitrato",
                       rbind(":efecto 150"=c(-1/5,-1/5,-1/5,4/5,-1/5)),conf=0.95)
efect.200=fit.contrast(modelo2,"nitrato",
                       rbind(":efecto 200"=c(-1/5,-1/5,-1/5,-1/5,4/5)),conf=0.95)

rbind(efect.0,efect.50,efect.100,efect.150,efect.200)


#GRAFICOS DE INTERVALOS DE TUKEY
plot(TukeyHSD(diseno,"Metodo",conf.level = 0.95),cex.lab=0.8,las=1)
#---------------------------------------------------------------------------------

#TEST DE NORMALIDAD CON RESIDUOS INTERNAMENTE ESTUDENTIZADOS
shapiro.test(rstandard(modelo2)) 

#TESTES PARA HOMOGENEIDAD DE VARIANZA
bartlett.test(nlechug~nitrato)
leveneTest(nlechug~nitrato)
cochran.test(nlechug~nitrato,data=diseno2)

# AGRUPACIONES DE MEDIAS COMPARACIONES DE MEDIAS ----------------------------------

library(agricolae)
HSD.test(modelo2,"nitrato", group=TRUE,console=TRUE) #Comparaciones de Tukey
duncan.test(modelo2,"nitrato",alpha=0.05,group=TRUE,console=TRUE) #Rango m?ltiple de Duncan
LSD.test(modelo2,"nitrato",group=TRUE,console=TRUE) #M?todo LSD

# --------------------------------------------------------------------------------

# Test de incorrelación Ljung-box
Box.test(y, lag = 1, type = c("Ljung-Box"), fitdf = 0)

# hallar valor p
pf(q = 5.535160 ,df1 = 4 ,df2 = 20, lower.tail = FALSE)


# DCA un solo factor aleatorio -------------------------------------------------

library(lme4)
library(MASS)
#AJUSTANDO EFECTOS ALEATORIOS PARA OBTENER COMPONENTES DE VARIANZA
diseno=lmer(contl~1|fardos)

#OBTENIENDO LA ANOVA 
summary(aov(contl~Error(fardos)))
dis2=aov(contl~Error(fardos))
summary(dis2)

anova(aov(contl~fardos))

# intervalos de confianzas
confint(diseno)

#OBTENEMOS RESIDUALES INTERNAMENTE ESTUDENTIZADOS Y VALORES AJUSTADOS COMO SI EL MODELO FUESE DE EFECTOS FIJOS Y CON ELLOS HACEMOS GR?FICOS DE RESIDUALES
res.estudent=stdres(aov(contl~fardos))
Yhat=fitted(aov(contl~fardos))

# DBCA -------------------------------------------------------------------------------

#AJUSTANDO EL MODELO ANOVA Y OBTENCI?N DE LA TABLA ANOVA
diseno=aov(Tiempo~Metodo+Operador)
anova(diseno)

#OBTENCI?N DE MEDIAS DE TRATAMIENTO CON SUS I.C DEL 95%
lsmeans(diseno,"Metodo")

#OBTENIENDO EFECTOS DE TRATAMIENTOS, RESULTADOS PARA TEST DE SIGNIFICANCIA Y SUS I.C DEL 95%
efect.m?todoA=fit.contrast(dise?o,"M?todo",rbind(":efecto m?todo A"=c(3/4,-1/4,-1/4,-1/4)),conf=0.95)
efect.m?todoB=fit.contrast(dise?o,"M?todo",rbind(":efecto m?todo B"=c(-1/4,3/4,-1/4,-1/4)),conf=0.95)
efect.m?todoC=fit.contrast(dise?o,"M?todo",rbind(":efecto m?todo C"=c(-1/4,-1/4,3/4,-1/4)),conf=0.95)
efect.m?todoD=fit.contrast(dise?o,"M?todo",rbind(":efecto m?todo D"=c(-1/4,-1/4,-1/4,3/4)),conf=0.95)
rbind(efect.m?todoA,efect.m?todoB,efect.m?todoC,efect.m?todoD)

#INTERVALOS DE TUKEY PARA LAS DIFERENCIAS DE MEDIAS DEL FACTOR DE TRATAMIENTOS
TukeyHSD(diseno,"Metodo",conf.level=0.95)
HSD.test(diseno,"Metodo", group=TRUE,console=TRUE) #Comparaciones de Tukey

#GR?FICOS DE INTERVALOS DE TUKEY
plot(TukeyHSD(diseno,"Metodo",conf.level = 0.95),cex.lab=0.8,las=1)

#OBTENIENDO TEST DE NORMALIDAD SOBRE RESIDUALES ESTUDENTIZADOS INTERNAMENTE
shapiro.test(rstandard(dise?o))


