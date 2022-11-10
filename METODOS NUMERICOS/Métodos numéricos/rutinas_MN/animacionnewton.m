function p=animacionnewton(f, df, p0, delta, epsilon, max1)
% Animacion del comportamiento del metodo de Newton
% Entradas: 
% f = la funcion objetivo introducida como cadena de caracteres  'f'
% df = la derivada de  f  introducida como cadena de caracteres  'df'
% p0 = la aproximacion inicial a cero de  f
% delta = la tolerancia para  p0
% epsilon = la tolerancia para los valores de la funcion  y
% max1 = el numero maximo de iteraciones
% Salida:
% p = la aproximacion de Newton-Raphson hacia cero
%
% Desarrollado en la Universidad Nacional de Colombia Sede Medellin 2007
% Basado en:
%  METODOS NUMERICOS: Programas en Matlab
% (c) 2004 por John H. Mathews y Kurtis D. Fink
%  Software complementario acompañando al texto:
%  METODOS NUMERICOS con Matlab, Cuarta Edicion
%  ISBN: 0-13-065248-2
%  Prentice-Hall Pub. Inc.
%  One Lake Street
%  Upper Saddle River, NJ 07458

pn(1)=p0;
for  k = 1:max1	
	p1 = p0 - feval(f, p0) / feval(df, p0);
	err = abs(p1 - p0);
	relerr = 2 * err / (abs(p1) + delta);
	p0 = p1;
    pn(k+1)=p0;
	y = feval(f, p0);
	if  (err < delta)  |  (relerr < delta)  |  (abs(y) < epsilon), break, end
end
p=p0;
maxx=0.1*round(10*max(pn)+1);
minx=0.1*round(10*min(pn)-1);

close all
fplot(f,[minx maxx],'g');
grid on
hold on
fplot(@(x)0*ones(size(x)),[minx maxx],'b');
legend({'funcion f(x)','recta y=0'},'Location','Best')
hh=warndlg({'Oprima OK para empezar la animacion','No cierre la ventana hasta que finalice la animacion'},'Instruccion');
waitfor(hh)
for i=1:length(pn)-1
    plot(pn(i),0,'.r')
    plot(pn(i)*[1 1],[0 feval(f,pn(i))],'r')
    pause(0.33)
    plot([pn(i) pn(i+1)],[feval(f,pn(i)) 0],'r')
    xlabel(['aproximacion al cero de f(x), p(' mat2str(i) ') = ' mat2str(pn(i+1))])
    pause(0.33)
end