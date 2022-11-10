function ML = Milne(f,a,b,ya,M)
h = (b - a) / M;
T = zeros(1, M+1);
Y = zeros(1, M+1);
T = a:h:b;
Y(1) = ya;
for i = 1:3
    Y(i+1) = Y(i) +(h/4)*((feval(f,T(i),Y(i)))+(3*feval(f, T(i)+(2/3)*h,Y(i)+(2/3)*h*feval(f,T(i),Y(i)))));
end

for  j = 4:M
   Y(j+1) = Y(j-3) + ((4/3)*h) *((2*feval(f, T(j), Y(j)))-(feval(f,T(j-1),Y(j-1)))+(2*feval(f,T(j-2),Y(j-2))));
end

ML = [T', Y'];