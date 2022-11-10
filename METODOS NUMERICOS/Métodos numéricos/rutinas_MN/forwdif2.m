function  U = forwdif2 (f, g1, g2, a, b, c, n, m)

% Resuelve la ecuacion del calor con el metodo de diferencias finitas
% progresivas 
% Entrada - f = u (x, 0) funcion creada con @
%           g1 = u (0, t) funcion creada con @
%           g2 = u (a, t) funcion creada con @
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

V=linspace(0,b,m);


% Condiciones frontera
U(1, 1:m)=feval(g1,V);
U(n, 1:m)=feval(g2,V);

% Generar la primera fila
U(2:n-1, 1) = feval(f, h:h:(n-2)*h)';

% Generar las filas restantes de  U

for  j = 2:m
   for  i = 2:n-1
      U(i, j) = s * U(i, j-1) + r * (U(i-1, j-1) + U(i+1, j-1));
   end
end

U = U';
