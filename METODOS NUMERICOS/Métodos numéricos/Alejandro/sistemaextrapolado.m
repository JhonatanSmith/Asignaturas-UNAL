function [Ac_extrapolado, B] = sistemaextrapolado(X,Y)
    % Funcion por Alejandro Bedoya 
    % Dudas o correcciones: 3015300512
    % Métodos numéricos 2021
    % sistemaextrapolado: Arroja el sistema de ecuaciones
    %              en términos de C  hasta un máximo de 
    %              15 C's 
    % Ingresan: X y Y
    % Resultados: El sistema de ecuaciones en términos de las
    %             constantes. Caso Spline Cubico Extrapolado
    %             El vector B es el lado derecho de la igualdad
    %             También llamado en las notas u.
    n = length(X)-1;
    a = Y;
    h = diff(X);
    syms c0 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15
    c = [c0 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15];
    B = zeros(n-1,1);
    for i=1:n-1
        B(i) = (3./(h(i+1))).*(a(i+2)-a(i+1)) - (3./(h(i))).*(a(i+1)-a(i));
    end
    c0 = (((2*c(3)-2*c(2))/h(2))*h(1) - 2*c(2))/(-2);
    c(1) = c0;
    cn = (((2*c(n)-2*c(n-1))/(h(n-1)))*h(n)+2*c(n))/2;
    c(n+1) = cn;
    Ac_extrapolado=sym('Ac_extrapolado',[n-1 1]);
    for i=1:n-1
       Ac_extrapolado(i) = h(i)*c(i) + 2*c(i+1)*(h(i)+h(i+1)) + h(i+1)*c(i+2);
    end
 
end