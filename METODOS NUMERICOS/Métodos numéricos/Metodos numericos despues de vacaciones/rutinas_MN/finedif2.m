function  U = finedif2 (f, g, q1, q2, a, b, c, d, n, m)

% Entrada - f = u (x, 0) funcion creada con @
%         - g = ut (x, 0) funcion creada con @
%         - q1 = u(a,t)   funcion creada con @
%         - q2 = u(b,t)   funcion creada con @
%         - a y b extremos de dominio espacial [a, b]
%         - d extremo derecho de intervalo temporal [0,d] 
%         - c la constante en la ecuacion de onda
%         - n y m numero de puntos (nodos) en la rejilla  [a,b]  y  [0, d]
% Salida  - U matriz solucion
% modificacion de rutina finedif.m
%  METODOS NUMERICOS: Programas en Matlab
% (c) 2004 por John H. Mathews y Kurtis D. Fink
%  Software complementario acompa�ando al texto:
%  METODOS NUMERICOS con Matlab, Cuarta Edicion
%  ISBN: 0-13-065248-2
%  Prentice-Hall Pub. Inc.
%  One Lake Street
%  Upper Saddle River, NJ 07458

% Inicializar parametros y U

h = (b-a) / (n-1);
k = d / (m-1);
r = c * k / h;
r2 = r^2;
r22 = r^2 / 2;
s1 = 1 - r^2;
s2 = 2 - 2*r^2;
U = zeros (n, m);
for i = 2:n-1
    U(i,1) = feval(f,a+(i-1)*h);
       U(i, 2) = s1 * feval(f, h*(i-1)) + k * feval(g, h*(i-1)) ...
            + r22 * (feval(f, h*i) + feval(f, h*(i-2)));

%    U(i,2) = U(i,1) + k*feval(g,a+(i-1)*h);
end
for j = 2:m
    U(1,j) = feval(q1,(j-1)*k);
    U(n,j) = feval(q2, (j-1)*k);
end
U(1,1) = feval(f,a);
U(n,1) = feval(f,b);

for  j = 3:m,
  for  i = 2:(n-1)
     U(i, j) = s2 * U(i, j-1) + r2 * (U(i-1, j-1) + U(i+1, j-1)) - U(i, j-2);
  end
end

U = U';
