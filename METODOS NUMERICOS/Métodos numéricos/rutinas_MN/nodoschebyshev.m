function  [X, Y] = nodoschebyshev (fun, n, a, b)

% Entrada   - fun funcion creada con @
%           - n es el grado del polinomio interpolante de Lagrange-Chebyshev
%           - a es el extremo izquierdo
%           - b es el extremo derecho
% Salida   
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