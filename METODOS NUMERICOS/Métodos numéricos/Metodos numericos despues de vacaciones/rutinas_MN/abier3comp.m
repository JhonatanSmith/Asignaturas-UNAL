function  s = abier3comp (f, a, b, n)

% Entrada  - f funcion integrando creada con @
%          - a y b son los limites superior e inferior de integracion
%          - n es el numero de subintervalos
% Salida   - s es la suma de la regla abierta de M=3

%  METODOS NUMERICOS: Programas en Matlab
% (c) 2004 por John H. Mathews y Kurtis D. Fink
%  Software complementario acompa�ando al texto:
%  METODOS NUMERICOS con Matlab, Cuarta Edicion
%  ISBN: 0-13-065248-2
%  Prentice-Hall Pub. Inc.
%  One Lake Street
%  Upper Saddle River, NJ 07458


if mod(n,5) ~= 0
sprintf('Error!!  El numero de subintervalos debe ser multiplo de 5 ...')
return

end

h= (b-a)/n;

x= a:h:b; %recorra de a hacia b con saltos de a h

s = (5*h/24)*((11*sum(f(x(2:5:n+1)))+sum(f(x(3:5:n+1)))+sum(f(x(4:5:n+1)))+11*sum(f(x(5:5:n+1)))));


