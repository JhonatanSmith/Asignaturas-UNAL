function  T = taylorN(f,n, a, b, ya, M)
% Entrada  - f: y'=f(t,y) (NO CREAR LA FUNCIÓN CON @(t,y))
%          - n: Grado de la derivada (Taylor orden n)
%          - a y b son los extremos izquierdo y derecho
%          - ya es la condicion inicial  y(a)
%          - M es el numero de pasos
% Salida   - T = [T', Y'] donde  T  es el vector de abscisas y
%            Y  es el vector de ordenadas

%  METODOS NUMERICOS 2021-1S Universidad Nacional de Colombia, Sede Med.
% (c) 2021 Denilson Andrés Molina Truyot

%Creación de df:
df=[f 0 0 0];
syms t
syms y
yp=f;
for i=2:n
    dfdt=diff(f,t)*(1)+diff(f,y)*(yp);
    df(i)=dfdt;
    f=dfdt;
end

%Metodo de Taylor
h = (b - a) / M;
T = zeros(1, M+1);
Y = zeros(1, M+1);
T = a:h:b;
Y(1) = ya;

for  j = 1:M
   acu=0;
   D = subs(df, [t y], [T(j) Y(j)]);
   for i=1:n
    acu=acu+((h^i)/factorial(i))*D(i);
   end
   Y(j+1) = Y(j) + acu;
end

T = [T', Y'];



end
