function s = Cuadratura_M4(f,a,b,M)
if mod(M,4) ~=0
    fprintf('Error M debe ser multiplo de 4')
    return
end

h= (b-a)/M;
x=a:h:b;
xa = (a+4/5*h):4:M+1;
s1 = (-2/9)*sum(f(x(1:4:M)));
s3 = (-8/9)*sum(f(x(3:4:M))) ;
s4 = (256/99)*sum(f(x(4:4:M)));
s2 = (250/99)*sum(f(xa(1:4:M)));
s = h*(s1+s2+s3+s4);

end