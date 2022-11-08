library(MASS)
Advertising<-read.csv(file="F:/INTRODUCCIÓN A LA ANALÍTICA/IA Virtual 02_2020/Advertising.csv",
                      header=T,sep=',',dec='.')
Sales=Advertising$sales
Tv=Advertising$TV
Radio=Advertising$radio
Newspaper=Advertising$newspaper
fit.lm<-lm(Sales~Tv)
list(summary=summary(fit.lm),IC=confint(fit.lm))

########################################################

library(MASS)
Advertising<-read.csv(file="F:/INTRODUCCIÓN A LA ANALÍTICA/IA Virtual 02_2020/Advertising.csv",
                      header=T,sep=',',dec='.')
#Advertising<-read.csv(file="Advertising.csv",header=T,sep=',',dec='.')
Sales=Advertising$sales
Tv=Advertising$TV
Radio=Advertising$radio
Newspaper=Advertising$newspaper
fit.lm<-lm(Sales~Tv)
summary(fit.lm)
list(Mean_of_Sales=mean(Sales))

##########################################################

library(MASS)
Advertising<-read.csv(file="F:/INTRODUCCIÓN A LA ANALÍTICA/IA Virtual 02_2020/Advertising.csv",
                      header=T,sep=',',dec='.')
#Advertising<-read.csv(file="Advertising.csv",header=T,sep=',',dec='.')
Sales=Advertising$sales
Tv=Advertising$TV
Radio=Advertising$radio
Newspaper=Advertising$newspaper
fit.lm<-lm(Sales~Tv)
#summary(fit.lm)
#names(summary(fit.lm))
summary(fit.lm)$adj.r.squared

###########################################################

library(MASS)
Advertising<-read.csv(file="F:/INTRODUCCIÓN A LA ANALÍTICA/IA Virtual 02_2020/Advertising.csv",
                      header=T, sep=',',dec='.')
Sales=Advertising$sales
Tv=Advertising$TV
Radio=Advertising$radio
Newspaper=Advertising$newspaper
cor(Tv, Sales,  method = "pearson", use = "complete.obs")

############################################################

library(MASS)
Advertising<-read.csv(file="F:/INTRODUCCIÓN A LA ANALÍTICA/IA Virtual 02_2020/Advertising.csv",
                      header=T,sep=',',dec='.')
Sales=Advertising$sales
Tv=Advertising$TV
Radio=Advertising$radio
Newspaper=Advertising$newspaper
Modelo1<-summary(lm(Sales~Tv))$coefficients
Modelo2<-summary(lm(Sales~Radio))$coefficients
Modelo3<-summary(lm(Sales~Newspaper))$coefficients
list(Modelo1=Modelo1,Modelo2=Modelo2,Modelo3=Modelo3)

############################################################

library(ISLR)
library(ggplot2)
library(MASS)
library("plot3D")
Advertising<-read.csv(file="F:/INTRODUCCIÓN A LA ANALÍTICA/IA Virtual 02_2020/Advertising.csv",
                      header=T,sep=',',dec='.')
#Advertising<-read.csv(file="Advertising.csv",header=T,sep=',',dec='.')
z=Advertising$sales
x=Advertising$TV
y=Advertising$radio
#Newspaper=Advertising$newspaper
# Compute the linear regression (z = ax + by + d)
fit <- lm(z ~ x + y)
# predict values on regular xy grid
grid.lines = 50
x.pred <- seq(min(x), max(x), length.out = grid.lines)
y.pred <- seq(min(y), max(y), length.out = grid.lines)
xy <- expand.grid( x = x.pred, y = y.pred)
z.pred <- matrix(predict(fit, newdata = xy), 
                 nrow = grid.lines, ncol = grid.lines)
# fitted points for droplines to surface
fitpoints <- predict(fit)
#scatter plot with regression plane with few colors
scatter3D(x, y, z, pch = 19, cex = 1,col='red',
          theta = 20, phi = 17, scale=TRUE, expand=0.3,
          xlab = "TV", ylab = "Radio", zlab = "Sales",
          surf = list(x = x.pred, y = y.pred, z = z.pred,col='#006633',
                      facets = NA, fit = fitpoints), main = "Advertising Dataset ISLR")

####################################################################

library(ISLR)
library(ggplot2)
library(MASS)
library("plot3D")
Advertising<-read.csv(file="F:/INTRODUCCIÓN A LA ANALÍTICA/IA Virtual 02_2020/Advertising.csv",
                      header=T,sep=',',dec='.')
#Advertising<-read.csv(file="Advertising.csv",header=T,sep=',',dec='.')
Sales=Advertising$sales
TV=Advertising$TV
Radio=Advertising$radio
Newspaper=Advertising$newspaper
fit <- lm(Sales ~ TV+Radio+Newspaper)
summary(fit)$coefficients

####################################################################

library(ISLR)
library(ggplot2)
library(MASS)
library(reshape2)
Advertising<-read.csv(file="F:/INTRODUCCIÓN A LA ANALÍTICA/IA Virtual 02_2020/Advertising.csv",
                      header=T,sep=',',dec='.')
#Advertising<-read.csv(file="Advertising.csv",header=T,sep=',',dec='.')
# cormat <- round(cor(mydata),2)
# head(cormat)
cormat<-round(cor(Advertising[,2:5]),2)
melted_cormat <- melt(cormat)
#head(melted_cormat)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()
# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
library(reshape2)
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Heatmap
library(ggplot2)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
