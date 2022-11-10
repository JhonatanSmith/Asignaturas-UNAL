function D = getDerivate(f,n)

syms t
syms y

yp=f;
dz=[f];
for i=2:n
    dzdt=diff(f,t)*(1)+diff(f,y)*(yp);
    dz(i)=dzdt;
    f=dzdt;
end
D=dz';
end