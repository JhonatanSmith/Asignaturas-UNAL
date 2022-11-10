function  R = rk4 (f, a, b, ya, M)

% Entrada   - f funcion creada con @
%           - a y b son los extremos izquierdo y derecho
%           - ya es la condicion inicial  y(a)
%           - M es el numero de pasos
% Salida    - R = [T', Y'] donde  T  es el vector de abscisas
%             y  Y  es el vector de ordenadas

%  METODOS NUMERICOS: Programas en Matlab
% (c) 2004 por John H. Mathews y Kurtis D. Fink
%  Software complementario acompa�ando al texto:
%  METODOS NUMERICOS con Matlab, Cuarta Edicion
%  ISBN: 0-13-065248-2
%  Prentice-Hall Pub. Inc.
%  One Lake Street
%  Upper Saddle River, NJ 07458

h = (b - a) / M;
T = zeros(1, M+1);
Y = zeros(1, M+1);
T = a:h:b;
Y(1) = ya;
for  j = 1:M
   k1 = h * feval(f, T(j), Y(j));
   k2 = h * feval(f, T(j) + h/2, Y(j) + k1/2);
   k3 = h * feval(f, T(j) + h/2, Y(j) + k2/2);
   k4 = h * feval(f, T(j) + h,   Y(j) + k3);
   Y(j+1) = Y(j) + (k1 + 2 * k2 + 2 * k3 + k4) / 6;
end

R = [T', Y'];
