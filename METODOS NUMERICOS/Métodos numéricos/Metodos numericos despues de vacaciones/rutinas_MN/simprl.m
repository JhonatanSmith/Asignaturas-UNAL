function  s = simprl (f, a, b, M)

% Entrada  - f funcion integrando creada con @
%          - a y b son los limites superior e inferior de integracion
%          - M es el numero de subintervalos
% Salida   - s es la suma de la regla de simpson

%  METODOS NUMERICOS: Programas en Matlab
% (c) 2004 por John H. Mathews y Kurtis D. Fink
%  Software complementario acompa�ando al texto:
%  METODOS NUMERICOS con Matlab, Cuarta Edicion
%  ISBN: 0-13-065248-2
%  Prentice-Hall Pub. Inc.
%  One Lake Street
%  Upper Saddle River, NJ 07458


if mod(M,2) == 1
sprintf('Error!!  El n�mero de subintervalos debe ser par ...')
return

end

h = (b - a) / (M);
s1 = 0;
s2 = 0;

N=M/2;

for  k = 1:N
   x = a + h * (2 * k - 1);
   s1 = s1 + feval(f, x);
end
for  k = 1:(N-1)
   x = a + h * 2 * k;
   s2 = s2 + feval(f, x);
end

s = h * (feval(f, a) + feval(f, b) + 4 * s1 + 2 * s2) / 3;
