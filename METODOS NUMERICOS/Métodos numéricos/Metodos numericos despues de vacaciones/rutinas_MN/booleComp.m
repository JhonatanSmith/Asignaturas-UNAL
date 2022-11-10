function  s = booleComp (f, a, b, M)

% Entrada  - f funcion integrando creada con @
%          - a y b son los limites superior e inferior de integracion
%          - M es el numero de subintervalos
% Salida   - s es la suma de la regla de Boole compuesta

%  METODOS NUMERICOS: Programas en Matlab
% (c) 2004 por John H. Mathews y Kurtis D. Fink
%  Software complementario acompa�ando al texto:
%  METODOS NUMERICOS con Matlab, Cuarta Edicion
%  ISBN: 0-13-065248-2
%  Prentice-Hall Pub. Inc.
%  One Lake Street
%  Upper Saddle River, NJ 07458


if mod(M,4) ~= 0
sprintf('Error!!  El numero de subintervalos debe ser multiplo de 4 ...')
return

end

h = (b - a) / M;
s1 = 0;
s2 = 0;
s3 = 0;

N=M/4;

for  k = 1:N
   x = a + h * (4 * k - 1); %Esto sale del analisis del ejercicio
   x2 = a + h * (4 * k - 3);
   s1 = s1 + feval(f, x) + feval(f, x2);
   x3 = a + h * (4 * k - 2);
   s2 = s2 + feval(f, x3);
end
for  k = 1:(N-1)
   x = a + h * 4 * k;
   s3 = s3 + feval(f, x);
end

s = (2*h)/(45) * (7*feval(f, a) + 7*feval(f, b) + 32 * s1 + 12 * s2 + 14 * s3) ;
