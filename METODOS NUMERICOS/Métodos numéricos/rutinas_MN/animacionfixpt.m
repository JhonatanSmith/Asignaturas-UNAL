function p = animacionfixpt(g, p0, tol, maxi)
% Animacion del comportamiento del algoritmo de punto fijo
% Entradas:
% g = funcion de iteracion de punto fijo
% p0 = punto inicial
% tol = tolerancia, se refiere al error relativo entre Pn-1 y Pn
% maxi = numero maximo de iteraciones permitido
% Salida:
% p = aprocimacion del punto fijo obtenida
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

close all

delta=0.2;
[p, k, err, P] = fixpt (g, p0, tol, maxi);
figure
fplot(g,[round(10*(min(P)-delta))/10 round(10*(max(P)+delta))/10],'Color','g')
grid on
hold on
fplot('x',[round(10*(min(P)-delta))/10 round(10*(max(P)+delta))/10])
legend({'funcion g(x)','recta y=x'},'Location','NorthOutside')
plot(P(1),0,'.r','Markersize',10)
hh=warndlg({'Oprima OK para empezar la animacion','No cierre la ventana hasta que finalice la animacion'},'Instruccion');
waitfor(hh)
plot([P(1) P(1)],[0 feval(g,P(1))],'r')
for k=2:length(P)
    plot([P(k-1) P(k)],[P(k) P(k)],'r')
    pause(0.5)
    plot([P(k) P(k)],[P(k) feval(g,P(k))],'r')
    plot([P(k)],[0],'.r','Markersize',10)
    pause(0.5)
    xlabel(['aproximacion al punto fijo p(' mat2str(k) ') = ' mat2str(P(k))])
end