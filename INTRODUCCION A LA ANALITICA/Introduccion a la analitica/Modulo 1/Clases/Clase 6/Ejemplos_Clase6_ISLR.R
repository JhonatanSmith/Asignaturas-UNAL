#Ejemplos clase 6 ISLR

library(ISLR)
library(ggplot2)
library(MASS)
library("plot3D")

Income3D=read.csv(file="Income2.csv", header=T,sep=',',dec='.')
x=Income3D$Education
y=Income3D$Seniority
z=Income3D$Income
# Compute the linear regression (z = ax + by + d)
fit <- lm(z ~ x + y)
# predict values on regular xy grid
grid.lines = 45
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
          xlab = "Education", ylab = "Seniority", zlab = "Income",
          surf = list(x = x.pred, y = y.pred, z = z.pred,col='#009933',
                      facets = NA, fit = fitpoints), main = "Income Dataset ISLR")

## REGRESIÓN LINEAL MÚLTIPLE
#Obtaining figure 3.4
Advertising<-read.csv(file="Advertising.csv",header=T,sep=',',dec='.')
z=Advertising$sales
x=Advertising$TV
y=Advertising$radio
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

## REGRESIÓN LINEAL MÚLTIPLE
#Obtaining figure 3.4
Advertising<-read.csv(file="Advertising.csv",header=T,sep=',',dec='.')
z=Advertising$sales
x=Advertising$TV
y=Advertising$newspaper
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
          xlab = "TV", ylab = "Newspaper", zlab = "Sales",
          surf = list(x = x.pred, y = y.pred, z = z.pred,col='#006633',
                      facets = NA, fit = fitpoints), main = "Advertising Dataset ISLR")

## REGRESIÓN LINEAL MÚLTIPLE
#Obtaining figure 3.4

Advertising<-read.csv(file="Advertising.csv", header=T,sep=',',dec='.')
z=Advertising$sales
x=Advertising$radio
y=Advertising$newspaper
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
          xlab = "Radio", ylab = "Newspaper", zlab = "Sales",
          surf = list(x = x.pred, y = y.pred, z = z.pred,col='#006633',
                      facets = NA, fit = fitpoints), main = "Advertising Dataset ISLR")
#Calculating correlation matrix
Advertising<-read.csv(file="Advertising.csv", header=T,sep=',',dec='.')
cor(Advertising[,2:5])

# Matriz de correlacion

library(reshape2)
Advertising<-read.csv(file="Advertising.csv", header=T,sep=',',dec='.')
cormat<-round(cor(Advertising[,2:5]),2)
melted_cormat <- melt(cormat)
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

