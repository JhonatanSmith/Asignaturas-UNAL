##estimacion para la media ##
Nh<- c(155,62,93)
N=sum(Nh)
nh<- c(20,8,12)
A=c(35,43,36,39,28,28,29,25,38,27,26,32,29,40,35,41,37,31,45,34)
B=c(27,15,4,41,49,25,10,30)
C=c(8,14,12,15,30,32,21,20,37,7,11,24)
n=sum(nh)
yh<-c(mean(A),mean(B),mean(C))
th<-c(9600,7200,6000)
s2h<-c(var(A),(var(B)),var(C))
yest= (1/N)*sum(Nh*yh)
varyest=(1/N^2)*sum(Nh^2*varyh)
varyh=c(a,b,c)
B1= sqrt(varyest)
Li= yest-((qnorm(1-0.025))*B1)
Ls= yest+((qnorm(1-0.025))*B1)

##Estimaci?n para el total poblacional##
test=sum(th)
vartest=(N^2)*(varyest)
B3=sqrt(vartest)
Li= test-((qnorm(1-0.025))*B2)
Ls= test+((qnorm(1-0.025))*B2)

##funcion para calcular varianza estimada##
varyh= function(N,n,s2){
  result<- (s2/n)*((N-n)/N)
  return(result)
}
##para recordar, yh=mean, s2=var##

##Prorporciones y totales muestrales##
Nh=c(4000,6000,10000)
N=sum(Nh)
nh=c(36,40,44)
ph=c(0.9,0.6,0.2)
Ah=c(3600,3600,2000)
qh=c(0.1,0.4,0.8)
varph=c(0.002548,0.006113,0.003704)
pest=(1/N)*sum(Ah)
pest
varpest=(1/N^2)*sum((Nh^2)*varph)
B=sqrt(varpest)
Li= pest-((qnorm(1-0.025))*B)
Ls= pest+((qnorm(1-0.025))*B)
Aest=N*pest 
varAest=N^2*varpest
B1=sqrt(varAest)
Li= Aest-((qnorm(1-0.025))*B1)
Ls= Aest+((qnorm(1-0.025))*B1)

##tama?o de la muestra aproximado para miu##
Nh=c(155,62,93)
N=sum(Nh)
D=2^2/(2)^2
s2h=c(25,225,100)
B=2
wh=c(1/3,1/3,1/3)
n=(sum((Nh^2*s2h/wh)))/((N^2*D)+sum(Nh*s2h))
nt=ceiling(n)*wh
sum(ceiling(nt))
##Tama?o de la muestra para miu, Asignacion optima del tama?o de la muestra##
Nh=c(155,62,93)
N=sum(Nh)
s2h=c(25,225,100)
sh=sqrt(s2h)
C=c(9,9,16)
B=2
D=B^2/(2)^2
wh=((Nh*sh)/sqrt(C))/sum((Nh*sh)/sqrt(C))
n=ceiling((sum((Nh^2*s2h/wh)))/((N^2*D)+sum(Nh*s2h)))
n
##Asignacion para cada estrato##
nt=ceiling(n*wh)
sum(ceiling(nt))
##costo de muestreo##
costo=sum(C*nt)
##tama?o de la muestra para miu, asignacion de neyman##
Nh=c(155,62,93)
N=sum(Nh)
s2h=c(25,225,100)
sh=sqrt(s2h)
B=2
D=B^2/(2)^2
wh=(Nh*sh)/(sum(Nh*sh))
n=ceiling((sum((Nh^2*s2h/wh)))/((N^2*D)+sum(Nh*s2h)))
nt=ceiling(n*wh)
sum(ceiling(nt))
##tama?o de la muestra para miu, asignaci?n proporcional##
Nh=c(155,62,93)
N=sum(Nh)
s2h=c(25,225,100)
sh=sqrt(s2h)
B=2
C=9
D=B^2/(2)^2
wh=(Nh/sum(Nh))
n=ceiling((sum((Nh^2*s2h/wh)))/((N^2*D)+sum(Nh*s2h)))
##Asignacion para cada estrato##
nt=ceiling(n*wh)
sum(ceiling(nt))
##Tma?o de muestra para p asignacion optima para el tama?o de muestra##
nh=c(20,8,12)
ph=c(0.8,0.25,0.5)
C=c(9,9,13)
Nh=c(155,62,93)
N=sum(Nh)
B=0.1
s2h=ph*(1-ph)
sh=sqrt(s2h)
D=B^2/(2)^2
wh=((Nh*sh)/sqrt(C))/sum((Nh*sh)/sqrt(C))
n=ceiling((sum((Nh^2*s2h/wh)))/((N^2*D)+sum(Nh*s2h)))
nt=ceiling(n*wh)
##muestreo doble para estratificacion##
nh.=c(84,57)
n.=sum(nh.)
wh.=nh./n.
nh=c(11,12)
incriprivada=c(1618,1140,1000,1225,791,1600,746,1701,701,6918,1050)
inscripublica=c(7332,2356,21879,935,1293,1293,8500,6491,781,7255,2136,5380)
wh.
yh=c(mean(incriprivada),mean(inscripublica))
s2h=c(var(incriprivada),var(inscripublica))
ymdest=sum(wh.*yh)
varymdest=sum((wh.^2*s2h)/nh)+sum((wh.*(yh-ymdest)^2)/n.)
B=2*sqrt(varymdest)

#Muestro POSTESTRATIFICACION

y_bar_post= 1/N*sum(Nh*yh)
yh #promedios estratos
Nh #Total en estratos
Var_ybar_post = ((N-n)/(N^2*n))*sum(Nh*s2h)+(1/n^2)*((N-n)/(N-1)*sum((1-Nh/N)*s2h)) #Varianza Y barra

##muestreo sistematico poblacional##
n1=c(23,23,43,40,20,14,17,20,15,16)
n2=c(30,38,23,17,20,22,12,14,29,22)
n3=c(19,19,14,40,24,24,30,15,10,16)
n4=c(38,18,17,23,14,14,14,14,10,12)
yij=c(n1,n2,n3,n4)
ysis=c(mean(n1),mean(n2),mean(n3),mean(n4))
si2=c(var(n1),var(n2),var(n3),var(n4))
k=4
N=40
miu=(1/N)*sum(yij)
sigma2=(1/(N-1))*sum((yij-miu)^2)
var(yij)
miu
varysis=(1/k)*sum((ysis-miu)^2)
B=2*sqrt(varysis)
varpsis=(1/4)*sum(si2)
##muestreo sistematico normal##
N=2000
n=100
diasp=c(32,36,13,26,5,10,2,8,28,34,7,22,6,22,35,26,22,34,32,36,3,11,14,15,28,34
        ,4,7,4,6,10,30,30,6,11,35,30,5,20,28,18,34,7,36,30,19,29,28,31,23,4,10,
        14,10,20,15,18,35,31,32,36,11,6,2,19,15,31,31,21,27,6,1,18,30,25,27,5,17
        ,9,11,23,28,4,22,33,36,15,30,11,8,28,25,5,6,12,22,27,34,21,25)
ysis=mean(diasp)
s2=var(diasp)
sum((diasp-ysis)^2/99)
varysis= function(N,n,s2){
  result<- (s2/n)*((N-n)/N)
  return(result)}
Varysis=varysis(N,n,s2)
B=2*sqrt(Varysis)
Li= ysis-B
Ls=ysis+B

##estimacion del total poblacional##
tsis=Nysis
vartsis=(N^2)*Varysis
B=2*sqrt(vartsis)
Li=tsis-B
Ls=tsis+B
##estimacion de la proporcion##
psis=a/n
qsis=1-psis
varpsis=((N-n)/N)*((psis-qsis)/n-1)
B=2*sqrt(varpsis)
Li=psis-B
Ls=psis+B
##estimacion de A##
Asis=N*psis
varAsis=(N^2)*varpsis
B=2*sqrt(varAsis)
Li=Asis-B
Ls=Asis+B
##tama?o de la muestra para miu##
N=2500
s2h=100
B=2
D=(B^2)/4
n=ceiling((N*s2h)/(((N-1)*D)+s2h))
##tama?o de la muestra para p##
N=5000
B=0.03
p=0.5
q=1-p
D=B^2/4
n=ceiling((N*p*q)/(((N-1)*D)+p*q))
##muestreo sistematico repetido##
N=2000
e=0.05
z=2
mo=5
no=8
n=no*mo
k=N/n
k.=mo*k
n1=c(10,17,33,25,4,28,10,10)
n2=c(6,8,5,17,30,27,25,7)
n3=c(34,3,29,17,19,17,14,13)
n4=c(10,17,10,9,15,24,23,13)
n5=c(13,15,29,31,29,30,34,1)
nt=c(n1,n2,n3,n4,n5)
yh=c(mean(n1),mean(n2),mean(n3),mean(n4),mean(n5))
yt=(1/5)*sum(yh)
s2=(1/(mo-1))*sum((yh-yt)^2)
m=(((z^2*s2)/yt^2)*(N/no))/(e^2*(N/no)+(z^2*s2)/yt^2)
ceiling(m)

  