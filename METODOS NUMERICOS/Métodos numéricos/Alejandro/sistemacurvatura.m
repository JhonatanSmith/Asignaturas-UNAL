function [Ac_curvatura, B] = sistemacurvatura(X,Y,ddsx0,ddsxn)
    % Funcion por Alejandro Bedoya 
    % Dudas o correcciones: 3015300512
    % Métodos numéricos 2021
    % sistemacurvatura: Arroja el sistema de ecuaciones
    %              en términos de hasta un máximo de 
    %              15 C's 
    % Ingresan: X y Y
    % Resultados: El sistema de ecuaciones en términos de las
    %             constantes. Caso Spline Cubico con curvatura dada.
    %             El vector B es el lado derecho de la igualdad
    %             También llamado en las notas u.
    % IMPORTANTE: El resultado corresponde a los extremos del sistema.
    %      Las ecuaciones intermedias (entre las dos que arroja el resultado)
    %      Corresponderán a las del Sistema de ecuaciones base.
    %      B es modificado, deberán efectuar la resta manualmente. 
    % Por ejemplo si el sistema es 3c1 + 3c2 + 4 = 12 
    %     pues pasan el 4 a restar al otro lado 
    
    % dsx0 = Valor de la segunda derivada en x0
    % dsxn = Valor de la segunda derivada en xn
    
    n = length(X)-1;
    a = Y;
    h = diff(X);
    syms c0 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15
    c = [c0 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15];
    B = zeros(n-1,1);
    for i=1:n-1
        B(i) = (3./(h(i+1))).*(a(i+2)-a(i+1)) - (3./(h(i))).*(a(i+1)-a(i));
    end
    % Sistema Spline Cubico AC = B (No tocar nada) Base
    Ac_base=sym('Ac_base',[n-1 1]);
    for i=1:n-1
        Ac_base(i) = h(i)*c(i) + 2*c(i+1)*(h(i)+h(i+1)) + h(i+1)*c(i+2);
    end
    c0_extra = (1/2).*(ddsx0);  
    cn_extra = (1/2).*(ddsxn); 
    ac1=subs(Ac_base(1),c0,c0_extra);
    acn=subs(Ac_base(n-1),c(n+1),cn_extra);
    Ac_curvatura=Ac_base;
    Ac_curvatura(1)=ac1;
    Ac_curvatura(n-1)=acn;
end