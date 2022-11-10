function  U = forwdif (f, c1, c2, a, b, c, n, m)

% Entrada - f = u (x, 0) funcion creada con @
%         - c1 = u (0, t)  y  c2 = u (a, t)
%         - a y b extremos derechos de  [0, a]  y  [0, b]
%         - c la constante en la ecuacion de calor
%         - n y m  numero de puntos (nodos) de la cuadricula en  [0, a]  y  [0, b]
% Salida  - U matriz solucion; analoga a la Tabla 10.4

%  METODOS NUMERICOS: Programas en Matlab
% (c) 2004 por John H. Mathews y Kurtis D. Fink
%  Software complementario acompa�ando al texto:
%  METODOS NUMERICOS con Matlab, Cuarta Edicion
%  ISBN: 0-13-065248-2
%  Prentice-Hall Pub. Inc.
%  One Lake Street
%  Upper Saddle River, NJ 07458

% Inicializar parametros y de U

h = a / (n-1);
k = b / (m-1);
r = c^2 * k / h^2;
s = 1 - 2 * r;
U = zeros(n, m);

% Condiciones frontera

U(1, 1:m) = c1;
U(n, 1:m) = c2;

% Generar la primera fila
U(2:n-1, 1) = feval(f, h:h:(n-2)*h)';

% Generar las filas restantes de  U

for  j = 2:m
   for  i = 2:n-1
      U(i, j) = s * U(i, j-1) + r * (U(i-1, j-1) + U(i+1, j-1));
   end
end

U = U';
