function s = Cuadratura_M5(f,a,b,M)
if mod(M,5) ~=0
    fprintf('Error M debe ser multiplo de 5')
    return
end

h= (b-a)/M;
x=a:h:b;
s1 = (-5/144)*sum(f(x(1:5:M)));
s2 = (175/72)*sum(f(x(2:5:M)));
s3= (25/72)*sum(f(x(4:5:M)));
s4 = (325/144)*sum(f(x(5:5:M)));

s = h*(s1+s2+s3+s4)

end