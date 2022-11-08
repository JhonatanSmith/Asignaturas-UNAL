library(MASS)
Advertising<-read.csv(file="Advertising.csv",
                      header=T,sep=',',dec='.')
Sales=Advertising$sales
Tv=Advertising$TV
Radio=Advertising$radio
Newspaper=Advertising$newspaper
fit.lm<-lm(Sales~Tv)
list(summary=summary(fit.lm),IC=confint(fit.lm))


fit.lm1<-lm(sales~TV+radio+newspaper,data=Advertising)
summary(fit.lm1)$adj.r.squared

cor(Tv, Sales, method = "pearson", use = "complete.obs")
cor(Tv, Sales, method = "spearman", use = "complete.obs")

cor(Advertising[,2:5])
