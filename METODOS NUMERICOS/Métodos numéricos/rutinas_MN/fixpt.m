function  [p, k, err, P] = fixpt (g, p0, tol, max1)

% Entrada - g funcion creada con @
%         - p0 es el supuesto inicial para el punto fijo
%         - tol es la tolerancia
%         - max1 es el numero maximo de iteraciones
% Salida  - p es la aproximacion del punto fijo
%	      - k es el numero de iteraciones realizadas
%	      - err es el error en la aproximacion
%	      - P' contiene la secuencia {pn}

%  METODOS NUMERICOS: Programas en Matlab
% (c) 2004 por John H. Mathews y Kurtis D. Fink
%  Software complementario acompa�ando al texto:
%  METODOS NUMERICOS con Matlab, Cuarta Edicion
%  ISBN: 0-13-065248-2
%  Prentice-Hall Pub. Inc.
%  One Lake Street
%  Upper Saddle River, NJ 07458

P(1) = p0;
for  k = 2:max1+1
	P(k) = feval(g, P(k-1));
	err = abs(P(k) - P(k-1));
	relerr = err / (abs(P(k)) + eps);
	p = P(k);
	if  (err < tol) | (relerr < tol), break; end
end

if  k == max1+1
	disp('maximum number of iterations exceeded')
end

P = P(2:end)';

	 