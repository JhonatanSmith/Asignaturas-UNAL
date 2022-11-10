##Repaso de muestreo
Nh=c(500,1000,4000)
nh=c(50, 80, 160)
yh=c(40, 60, 100)
s2h=c(400,300,100)
N=sum(Nh)
n=sum(nh)
yest= (1/N)*sum(Nh*yh)
th=c(Nh*yh)
##intervalo de confianza para la media**
varyh= function(N,n,s2){
  result<- (s2/n)*((N-n)/N)
  return(result)
}
Varyh=varyh(Nh,nh,s2h)
varyest=(1/N^2)*sum(Nh^2*Varyh)
B1= sqrt(varyest)
Li= yest-(2*B1)
Ls= yest+(2*B1)
##intervalo para el total poblacional
test=sum(th)
vartest=(N^2)*(varyest)
B3=sqrt(vartest)
Li= test-(2*B3)
Ls= test+(2*B3)
#PUNTO 2##
n=210
Nh=c(1000,2000,5000)
s2h=c(225,81,36)
##afijacion igual##
H=3
Wh=1/H
nh=n/H
Wh
nh
##afijacion proporcional##
N=sum(Nh)
wh=(Nh/sum(Nh))
ceiling(n*wh)
##afijaci?n optima##
sh=sqrt(s2h)
wh=(Nh*sh)/(sum(Nh*sh))
n*wh
#PUNTO3#
Nh=c(65,42,93,25)
N=sum(Nh)
nh=c(14,9,21,6)
n=sum(nh)
ah=c(4,2,8,1)
ph=ah/nh
qh=1-ph
varph= function(N,n,ph){
  result<- ((ph*qh)/(n-1))*((N-n)/N)
  return(result)
}
Varph=varph(Nh,nh,ph)
Ah=Nh*ph
pest=(1/N)*sum(Ah)
pest
varpest=(1/N^2)*sum((Nh^2)*Varph)
B=sqrt(varpest)
Li= pest-(2*B)
Ls= pest+(2*B)
Li
Ls
#PUNTO4#
Nh=c(132,92,27)
sh=c(36,25,9)
n=300
N=sum(Nh)
who=(Nh*sh)/(sum(Nh*sh))
nt=ceiling(n*who)
nt
sum(nt)
##PUNTO 5##
A=c(8,24,0,0,16,32,6,0,16,7,4,4,9,5,8,18,2,0)
B=c(4,0,8,3,1,5,24,12,2,8)
C=c(1,8)
nh=c(18,10,2)
yh=c(mean(A),mean(B),mean(C))
th=N*yh
s2h=sh^2
varyh= function(N,n,s2){
  result<- (s2/n)*((N-n)/N)
  return(result)
}
Varyh=varyh(Nh,nh,s2h)
varyest=(1/N^2)*sum(Nh^2*Varyh)
vartest=(N^2)*(varyest)
B3=sqrt(vartest)
Li= test-(2*B3)
Ls= test+(2*B3)
##PUNTO 5##
D=0.1
C=c(9,25,36)
s2h=c(2.25,3.24,3.24)
sh=sqrt(s2h)
Nh=c(112,68,39)
N=sum(Nh)
wh=((Nh*sh)/sqrt(C))/sum((Nh*sh)/sqrt(C))
n=ceiling((sum((Nh^2*s2h/wh)))/((N^2*D)+sum(Nh*s2h)))
n
##PUNTO 7##
Nh=c(55,80,64)
nh=c(14,20,16)
N=sum(Nh)
n=sum(nh)
A=c(80,92,68,85,72,87,85,91,90,81,62,79,61,83)
B=c(85,82,48,75,53,73,65,78,49,69,72,81,53,59,68,52,71,61,59,42)
C=c(42,32,36,31,65,29,43,19,53,14,61,31,42,30,39,32)
#puntuacion promedio sexto grado#
yh<-c(mean(A),mean(B),mean(C))
s2h<-c(var(A),(var(B)),var(C))
yest= (1/N)*sum(Nh*yh)
varyh= function(N,n,s2){
  result<- (s2/n)*((N-n)/N)
  return(result)
}
Varyh=varyh(Nh,nh,s2h)
varyest=(1/N^2)*sum(Nh^2*Varyh)
B1= sqrt(varyest)
Li= yest-((qnorm(1-0.025))*B1)
Ls= yest+((qnorm(1-0.025))*B1)
Li
Ls
yest
#diferencia promedio entre A y B#
mean(A)-mean(B)
mean(A)
mean(B)
nt1=50
# neyman#
sh=sqrt(s2h)
wh=(Nh*sh)/(sum(Nh*sh))
nt=nt1*wh
sum(ceiling(nt))
#tama?o de muestra miu-afijaccion proporcional#
B=4
D=B^2/(2)^2
wh=(Nh/sum(Nh))
n=ceiling((sum((Nh^2*s2h/wh)))/((N^2*D)+sum(Nh*s2h)))
n
#neyman again#
B=4
D=B^2/(2)^2
wh=(Nh*sh)/(sum(Nh*sh))
n=ceiling((sum((Nh^2*s2h/wh)))/((N^2*D)+sum(Nh*s2h)))
nt=ceiling(n*wh)
sum(ceiling(nt))
n
##PUNTO 8##
N=240
n=40
Nh=c(86,72,52,30)
nh=c(14,12,9,5)
A=c(97,42,25,105,27,45,53,67,125,92,86,43,59,21)
B=c(125,67,256,310,220,142,155,96,47,236,352,190)
C=c(142,310,495,320,196,256,440,510,396)
D=c(167,220,780,655,540)
yh<-c(mean(A),mean(B),mean(C),mean(D))
th<-yh*Nh
s2h<-c(var(A),(var(B)),var(C),var(D))
sh=sqrt(s2h)
yest= (1/N)*sum(Nh*yh)
varyh= function(N,n,s2){
  result<- (s2/n)*((N-n)/N)
  return(result)
}
Varyh=varyh(Nh,nh,s2h)
test=sum(th)
vartest=(N^2)*(varyest)
B3=sqrt(vartest)
Li= test-(2*B3)
Ls= test+(2*B3)
Li
Ls
test
B3
#neyman#
B=5000
D=B^2/(4*N^2)
wh=(Nh*sh)/(sum(Nh*sh))
n=ceiling((sum((Nh^2*s2h/wh)))/((N^2*D)+sum(Nh*s2h)))
nt=ceiling(n*wh)
sum(ceiling(nt))
n
n
##PUNTO 9##
N=96
Nh=c(43,53)
sh=c((20-5)/6,(14-3)/6)
s2h=sh^2
B=1
D=B^2/4
wh=(Nh*sh)/(sum(Nh*sh))
n=ceiling((sum((Nh^2*s2h/wh)))/((N^2*D)+sum(Nh*s2h)))
nt=ceiling(n*wh)
sum(nt)
##PUNTO 11##
n=15
N=56
Nh=c(34,22)
Q=20.27/2
nh=c(7,8)
A=c(110,142,212,227,167,130,194)
B=c(387,345,465,308,280,480,355,405)
yh<-c(mean(A),mean(B))
s2h<-c(var(A),(var(B)))
ypost= (1/N)*sum(Nh*yh)
varpost=(((N-n)/(N^2*n))*sum(Nh*s2h))+((1/n^2)*((N-n)/(N-1))*sum((1-(Nh/N))*s2h))
varpost
(b)
Q=300.000
nh=c(8,7)
Nh=c(38,18)
N=56
A=c(110,142,212,227,167,130,194,280)
B=c(387,345,465,308,480,355,405)
yh<-c(mean(A),mean(B))
s2h<-c(var(A),(var(B)))
ypost= (1/N)*sum(Nh*yh)
varpost=(((N-n)/(N^2*n))*sum(Nh*s2h))+((1/n^2)*((N-n)/(N-1))*sum((1-(Nh/N))*s2h))
varpost
##PUNTO 12##
Q=32.6239/4
Q
Q*2
Q*3
Q*4
Nh=c(2,39,33,23)
N=sum(Nh)
N
##PUNTO 13##
wh=c(0.60,0.40)
nh=c(38,62)
ph=c(6/38,10/62)
n=100
varpost=(((N-n)/(N^2*n))*sum(Nh*s2h))+((1/n^2)*((N-n)/(N-1))*sum((1-(Nh/N))*s2h))
##punto 17##
N=46
Nh=c(20,26)
sh=c(100/6,(200-10)/6)
s2h=sh^2
B=200
D=B^2/(4*N^2)
wh=(Nh*sh)/(sum(Nh*sh))
n=ceiling((sum((Nh^2*s2h/wh)))/((N^2*D)+sum(Nh*s2h)))
nt=ceiling(n*wh)
sum(ceiling(nt))
##PUNTO 18##
nh.=c(84,57)
n.=141
nh=c(11,12)   
profpriv=c(122,88,65,55,79,79,40,75,32,428,110)
profpub=c(452,131,996,50,106,106,506,371,108,298,128,280)
wh.=nh./n.
yh=c(mean(profpriv),mean(profpub))
s2h=c(var(profpriv),var(profpub))
ymdest=sum(wh.*yh)
varymdest=sum((wh.^2*s2h)/nh)+sum((wh.*(yh-ymdest)^2)/n.)
B=2*sqrt(varymdest)
ymdest
varymdest
##PUNTO 19
N=1000
Nh=c(800,200)
nh=c(80,20)
n=100
ph=c(60/80, 8/20)
varph= function(N,n,ph){
  result<- ((ph*qh)/(n-1))*((N-n)/N)
  return(result)
}
qh=1-ph
Ah=Nh*ph
Varph=varph(Nh,nh,ph)
Varph
pest=(1/N)*sum(Ah)
varpest=(1/N^2)*sum((Nh^2)*Varph)
pest
B=2*sqrt(varpest)
B
##PUNTO 21##
ph=c(68.7,82.4,78.2)
nh=c(1330,165,1100)
wh=c(0.5,0.1,0.4)
qh=1-ph
varph= function(n,ph){
  result<- ((ph*qh)/(n-1))*1
  return(result)
}
Varph=varph(nh,ph)
pest=sum(wh*ph)
pest
varpest=sum(wh^2*Varph)
B2=2*sqrt(varpest)
varpest
B2
##PUNTO 22##
yh=c(7.63,7.74,6.55)
sh=c(0.15,0.35,0.11)
s2h=sh^2
nh=c(1347,163,1095)
wh=c(0.5,0.1,0.4)
n=sum(nh)
yest=sum(wh*yh)
varyh= function(n,s2){
  result<- (s2/n)*1
  return(result)
}
Varyh=varyh(nh,s2h)
varyest=sum(Varyh*wh^2)
B2=2*sqrt(varyest)
yest
varyest
B2
##PUNTO 25##
nh=c(214,249,261)
ph=c(214/724,249/724,261/724)
wh=c(0.33,0.34,0.33)
qh=1-ph
varph= function(n,ph){
  result<- ((ph*qh)/(n-1))*1
  return(result)
}
Varph=varph(nh,ph)
pest=sum(wh*ph)
pest
varpest=sum(wh^2*Varph)
B2=2*sqrt(varpest)
varpest
B2
#segunda parte#
nh=c(211,225,255)
ph=c(211/691, 225/691,255/691)
wh=c(0.33,0.34,0.33)
varph= function(n,ph){
  result<- ((ph*qh)/(n-1))*1
  return(result)
}
Varph=varph(nh,ph)
pest=sum(wh*ph)
pest
varpest=sum(wh^2*Varph)
B2=2*sqrt(varpest)
varpest
B2
##PUNTO 26##  
Nh=c(5000,2000,1000,8000,4000)
N=sum(Nh)
B=0.5
ph=c(0.2,0.2,0.2,0.2,0.2)
qh=1-ph
s2h=ph*qh
D=B^2/(2)^2
wh=(Nh/sum(Nh))
n=ceiling((sum((Nh^2*s2h/wh)))/((N^2*D)+sum(Nh*s2h)))
n
nt=ceiling(n*wh)
nt
#segunda parte#
Nh=c(5000,2000,1000,8000,4000)
N=sum(Nh)
B=0.1
ph=c(0.2,0.2,0.2,0.2,0.2)
qh=1-ph
s2h=ph*qh
D=B^2/(2)^2
wh=(Nh/sum(Nh))
n=ceiling((sum((Nh^2*s2h/wh)))/((N^2*D)+sum(Nh*s2h)))
n
nt=ceiling(n*wh)
nt
#tercera parte#
Nh=c(5000,2000,1000,8000,4000)
N=sum(Nh)
wh=(Nh/sum(Nh))
nt=ceiling(n*who)
sum(nt)
nt
