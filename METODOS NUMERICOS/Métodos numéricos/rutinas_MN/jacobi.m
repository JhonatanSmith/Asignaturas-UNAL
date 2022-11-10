function  X = jacobi (A, B, P, delta, max1)

% Entrada  - A es una matriz no singular  N x N
%          - B es una matriz  N x 1
%          - P es una matriz  N x 1; los supuestos iniciales
%	       - delta es la tolerancia para  P
%	       - max1 es el numero maximo de iteraciones
% Salida   - X es una matriz  N x 1: la aproximacion de jacobi a
%	         la solucion de  AX = B

%  METODOS NUMERICOS: Programas en Matlab
% (c) 2004 por John H. Mathews y Kurtis D. Fink
%  Software complementario acompañando al texto:
%  METODOS NUMERICOS con Matlab, Cuarta Edicion
%  ISBN: 0-13-065248-2
%  Prentice-Hall Pub. Inc.
%  One Lake Street
%  Upper Saddle River, NJ 07458

N = length(B);

for  k = 1:max1
   for  j = 1:N
      X(j) = (B(j) - A(j, [1:j-1, j+1:N]) * P([1:j-1, j+1:N])) / A(j, j);
   end
   err = abs(norm(X' - P));
   relerr = err/(norm(X) + eps);
   P = X';
      if  (err < delta)  |  (relerr < delta)
     break
   end
end

X = X';
k
err
