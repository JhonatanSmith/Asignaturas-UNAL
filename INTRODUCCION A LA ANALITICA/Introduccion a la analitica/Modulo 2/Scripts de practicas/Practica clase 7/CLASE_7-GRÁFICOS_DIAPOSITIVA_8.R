require(ISLR)

mod1<-lm(wage ~ poly(age,degree = 4), data=Wage)
fit1<-mod1$fitted.values
Wage$wage2<-ifelse(Wage$wage>250,1,0)
mod2<-glm(wage2~ poly(age, degree=4), data=Wage, family=binomial(link = "logit"))
fit2<-mod2$fitted.values

par(mfrow=c(1,2))
plot(Wage$age, Wage$wage, pch=19, col="gray")
lines(sort(Wage$age), fit1[order(Wage$age)], col=4, lwd=2)

plot(sort(Wage$age), fit2[order(Wage$age)], col=4, lwd=2, type="l", ylim=c(0, 0.2), 
     xlab="Age", ylab="Pr(Wage>250)")


# Actividad de clase ajustar curva con diferentes modelos

mod1 <- glm(wage~poly(age, 1), data=Wage)
cv.err1 <- cv.glm(Wage, mod1, K=10)
cv.err1$delta[1]


mod2 <- glm(wage~poly(age, 2), data=Wage)
cv.err2 <- cv.glm(Wage, mod2, K=10)
cv.err2$delta[1]

mod3 <- glm(wage~poly(age, 3), data=Wage)
cv.err3 <- cv.glm(Wage, mod3, K=10)
cv.err3$delta[1]

mod4 <- glm(wage~poly(age, 4), data=Wage)
cv.err4 <- cv.glm(Wage, mod4, K=10)
cv.err4$delta[1]