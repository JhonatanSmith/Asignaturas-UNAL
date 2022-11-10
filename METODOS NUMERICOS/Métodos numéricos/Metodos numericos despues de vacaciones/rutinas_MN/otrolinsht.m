function  L = otrolinsht (F, a, b, alpha, beta, M)

% Entrada   - F funcion vectorial creada con @
%           - a y b son los extremos del intervalo
%           - alpha = x(a)  y  beta = x(b); las condiciones frontera
%           - M es el numero de pasos
% Salida    - L = [T', X]; donde  T' es el vector de abscisas (M+1) x 1
%             y  X  es el vector de ordenadas  (M+1) x 1

%  METODOS NUMERICOS: Programas en Matlab
% (c) 2004 por John H. Mathews y Kurtis D. Fink
%  Software complementario acompa�ando al texto:
%  METODOS NUMERICOS con Matlab, Cuarta Edicion
%  ISBN: 0-13-065248-2
%  Prentice-Hall Pub. Inc.
%  One Lake Street
%  Upper Saddle River, NJ 07458

% Resolver el sistema  F1

Za = [alpha, 0];
[T, Z] = rks4 (F, a, b, Za, M);
U = Z(:, 1);

% Resolver el sistema  F2

Za = [alpha, 1];
[T, Z] = rks4 (F, a, b, Za, M);
V = Z(:, 1);

% Calcular la solucion al problema de valor frontera
lambda = (beta - V(M+1))./(U(M+1) - V(M+1));
X = lambda*U + (1-lambda) * V;
L = [T', X];
