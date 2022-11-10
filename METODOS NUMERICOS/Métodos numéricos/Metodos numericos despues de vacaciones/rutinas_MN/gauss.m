function  quad = gauss (f, a, b, A, W)

% Entrada   - f funcion integrando creada con @
%           - a y b limites superior e inferior de integracion
%           - A es el vector 1 x N de abscisas (nodos) de la Tabla 7.9
%           - W es el vector 1 x N de pesos de la Tabla 7.9
% Salida    - quad es el valor de cuadratura

%  METODOS NUMERICOS: Programas en Matlab
% (c) 2004 por John H. Mathews y Kurtis D. Fink
%  Software complementario acompa�ando al texto:
%  METODOS NUMERICOS con Matlab, Cuarta Edicion
%  ISBN: 0-13-065248-2
%  Prentice-Hall Pub. Inc.
%  One Lake Street
%  Upper Saddle River, NJ 07458

N = length(A);
T = zeros(1, N);
T = ((a + b) / 2) + ((b - a) / 2) * A;
quad = ((b - a) / 2) * sum(W .* feval(f, T));
