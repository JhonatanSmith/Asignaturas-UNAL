function  AproxSol = vectorialSystem(F, a, b, Za, M,TYPE)
% Entrada   - F funcion vectorial creada con @
%           - a y b los extremos del intervalo
%           - Za = [x1(a), ... , xn(a)] las condiciones iniciales
%           - M es el numero de pasos
%           - TYPE es el tipo de método:
%               * "Euler"
%               * "EulerMod"
%               * "RK2" o "Punto1/2"
%               * "Heun" o "RK3"
%               * "RK4"
%               * "AdamBF2" 
%               * "AdamBF3"
%               * "TaylorN" -> Para Taylor por ahora no hay método para
%               Sistemas de ecuaciones diferenciales, solo está para
%               ecuaciones de la forma y'=f(t,y)

% Salida    - T es el vector de pasos
%           - Z = [x1(t), ... , xn(t)] donde  xk(t)  es la aproximacion a la
%             k-esima variable dependiente


%  METODOS NUMERICOS 2021-1S. Universidad Nacional de Colombia, Sede Med.
% (c) 2021 por Denilson Andrés Molina Truyot

h = (b - a) / M;
T = zeros(1, M+1);
Z = zeros(M+1, length(Za));
T = a:h:b;
Z(1, :) = Za;

if(strcmp("Euler",TYPE))
    for  j = 1:M
        Z(j+1,:) = Z(j,:) + h * feval(F, T(j), Z(j,:));
    end
elseif(strcmp("EulerMod",TYPE))
    for  j = 1:M
        k1 = feval(F, T(j), Z(j,:));
        k2 = feval(F, T(j+1), Z(j,:)+h*k1);
        Z(j+1,:) = Z(j,:) + (h / 2) * (k1 + k2);
    end
elseif(or(strcmp("RK2",TYPE),strcmp("Punto1/2",TYPE)))
    for  j = 1:M
        k1 = feval(F, T(j), Z(j,:));
        Z(j+1,:) = Z(j,:) + h * feval(F, T(j) + h/2, Z(j,:) + (h/2)*k1 );
    end
elseif(or(strcmp("RK3",TYPE),strcmp("Heun",TYPE)))
    for  j = 1:M
        k1 = feval(F, T(j), Z(j,:));
        k2 = feval(F, T(j)+(h/3), Z(j,:)+(h/3)*k1);
        k3 = feval(F, T(j)+(2*h/3), Z(j,:)+(2*h/3)*k2);
        Z(j+1,:) = Z(j,:) + (h/4)*(k1 + 3*k3);
    end
elseif(strcmp("RK4",TYPE))
    for  j = 1:M
        k1 = h * feval(F, T(j), Z(j, :));
        k2 = h * feval(F, T(j) + h/2, Z(j, :) + k1/2);
        k3 = h * feval(F, T(j) + h/2, Z(j, :) + k2/2);
        k4 = h * feval(F, T(j) + h, Z(j, :) + k3);
        Z(j+1, :) = Z(j, :) + (k1 + 2 * k2 + 2 * k3 + k4) / 6;
    end
elseif(strcmp("AdamBF2",TYPE))
    Z(2, :) = Z(1, :) + h*feval(F,T(1),Z(1, :));
    for j=2:M
        Z(j+1, :)= Z(j, :)+(h/2)*(3*feval(F,T(j),Z(j, :))-feval(F,T(j-1),Z(j-1, :)));
    end
elseif(strcmp("AdamBF3",TYPE))
    Z(2, :) = Z(1, :) + h*feval(F,T(1),Z(1, :));
    Z(3, :) = Z(2, :) + h*feval(F,T(2),Z(2, :));
    for j=3:M
        Z(j+1, :)= Z(j, :)+(h/12)*(23*feval(F,T(j),Z(j, :))-16*feval(F,T(j-1),Z(j-1, :))+5*feval(F,T(j-2),Z(j-2, :)));
    end
elseif(strcmp(extractBetween(TYPE,1,6),"Taylor"))
    n=str2double(extractBetween(TYPE,7,strlength(TYPE)))
    %Creación de df:
    f=F;
    df=[f 0 0 0];
    syms t
    syms y
    yp=f;
    for i=2:n
        dfdt=diff(f,t)*(1)+diff(f,y)*(yp);
        df(i)=dfdt;
        f=dfdt;
    end
    
    %Metodo de Taylor
    for  j = 1:M
        acu=0;
        D = subs(df, [t y], [T(j) Z(j,:)]);
        for i=1:n
            acu=acu+((h^i)/factorial(i))*D(i);
        end
        Z(j+1,:) = Z(j,:) + acu;
    end
end
AproxSol=[T' Z];
end