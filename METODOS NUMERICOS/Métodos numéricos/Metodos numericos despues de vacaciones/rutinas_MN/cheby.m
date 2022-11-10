function  [C, X, Y] = cheby (fun, n, a, b)

% Entrada   - fun es la funcion introducida con @
%           - N es el grado del polinomial interpolante de Chebyshev
%           - a es el extremo izquierdo
%           - b es el extremo derecho
% Salida    - C es la lista de coeficientes para el polinomio
%           - X contiene las abscisas
%           - Y contiene las ordenadas

%  METODOS NUMERICOS: Programas en Matlab
% (c) 2004 por John H. Mathews y Kurtis D. Fink
%  Software complementario acompa�ando al texto:
%  METODOS NUMERICOS con Matlab, Cuarta Edicion
%  ISBN: 0-13-065248-2
%  Prentice-Hall Pub. Inc.
%  One Lake Street
%  Upper Saddle River, NJ 07458

if  nargin == 2, a = -1; b = 1; end
d = pi / (2 * n + 2);
C = zeros(1, n+1);

for  k = 1:n+1
   X(k) = cos((2 * k - 1) * d);
end

X = (b - a) * X / 2 + (a + b) / 2;
x = X;
%%%%%%%%
% Se cambia eval por feval
%%%%%%%%%
Y = feval(fun,x);

for  k = 1:n+1
   z = (2 * k - 1) * d;
   for  j = 1:n+1
      C(j) = C(j) + Y(k) * cos((j - 1) * z);
   end
end

C = 2 * C / (n + 1);
C(1) = C(1) / 2;
