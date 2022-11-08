library(ISLR)
library(ggplot2)
library(MASS)
library(latex2exp)

Advertising1<-read.csv(file="F:/INTRODUCCIÓN A LA ANALÍTICA/IA Virtual 02_2020/
                       Advertising.csv",header=T,sep=',',dec='.')
Sales=Advertising1$sales
Tv=Advertising1$TV-mean(Advertising1$TV)
Radio=Advertising1$radio
Newspaper=Advertising1$newspaper
mod <- lm(Sales ~ Tv, data = Advertising)
RSS2<-sum((Sales-14.02-0.0475*Tv)^2)
beta0 <- seq(8, 20, length = 50)  ## already in increasing order
beta1 <- seq(0,0.1, length=50)
RSS1=matrix(NA,nrow=length(beta0),ncol=length(beta1))

f<-function(x,y){
  for(i in 1:length(beta0)){for(j in 1:length(beta1)){
    RSS1[i,j]<-sum((Sales-x[i]-y[j]*Tv)^2)}};RSS1
}

RSS<-f(beta0,beta1)/1000

# Using plotly for interactive plot
require(plotly)
library(plotly)
aX <- list(title = "beta0")
aY <- list(title = "beta1")
aZ <- list(title = "RSS")
df2=data.frame(x=7.03,y=0.0475,z=2.102545)
dim(df2)
df2=t(df2)
z1<-RSS
z1=t(z1)
plot_ly() %>% add_surface(x=beta0,y=beta1,z=z1)%>%
  layout(scene = list(xaxis = aX, yaxis = aY, zaxis=aZ,dragmode="turntable"),
         autosize=0.9)

#Contour plus surface. A very elegant way to do it

library(plotly)
p <- plot_ly(x=beta0,y=beta1,z=z1) %>% add_surface(
  contours = list(
    z = list(
      show=TRUE,
      usecolormap=TRUE,
      highlightcolor="#ff0000",
      project=list(z=TRUE),
      showlabels = TRUE,
      autocontour=TRUE
    )
  )
) %>%
  layout(
    scene = list(xaxis =aX, yaxis = aY,  zaxis=aZ,
                 camera=list(
                   eye = list(x=0.5, y=1.5, z=0.84)
                 )
    )
  ) 
p
