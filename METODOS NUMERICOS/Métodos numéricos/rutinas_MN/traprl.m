function  s = traprl (f, a, b, M)

% Entrada  - f funcion integrando creada con @
%          - a y b son los limites superior e inferior de integracion
%          - M es el numero de subintervalos
% Salida   - s es la suma de la regla trapezoidal (integral)

%  METODOS NUMERICOS: Programas en Matlab
% (c) 2004 por John H. Mathews y Kurtis D. Fink
%  Software complementario acompa�ando al texto:
%  METODOS NUMERICOS con Matlab, Cuarta Edicion
%  ISBN: 0-13-065248-2
%  Prentice-Hall Pub. Inc.
%  One Lake Street
%  Upper Saddle River, NJ 07458

h = (b - a) / M;
s = 0;

for  k = 1:(M-1)
   x = a + h * k;
   s = s + feval(f, x);
end

s = h *(feval(f, a) + feval(f, b)) / 2 + h * s;
