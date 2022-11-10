function GraficaCercha(x,y,S)
n=length(x)-1;
plot(x,y,'.r')
hold on
for i=1:n
    xx=linspace(x(i),x(i+1),1000);
    yy=polyval(S(i,:),xx-x(i));
    if rem(i,2)==1
        plot(xx,yy,'m')
    else
        plot(xx,yy,'b')
    end
end   