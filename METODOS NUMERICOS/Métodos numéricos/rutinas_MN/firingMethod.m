function  solPVF = firingMethod(F1, F2, a, b, alpha, beta, M,TYPE)
% Entrada   - F1 y F2 son los sistemas de ecuaciones de primer orden
%             representando los Problemas de Valor Inicial (P.V.I.'s) (9) and (10),
%             respectivamente; funciones creadas con @
%           - a y b son los extremos del intervalo
%           - alpha = x(a)  y  beta = x(b); las condiciones frontera
%           - M es el numero de pasos
%           - TYPE es el tipo de método:
%               * "Euler"
%               * "EulerMod"
%               * "RK2" o "Punto1/2"
%               * "Heun" o "RK3"
%               * "RK4"
%               * "AdamBF2" 
%               * "AdamBF3"
% Salida    - solPVF = [T', X]; donde  T' es el vector de abscisas (M+1) x 1
%             y  X  es el vector de ordenadas  (M+1) x 1

% METODOS NUMERICOS 2021-1S Universidad Nacional de Colombia, sede Med.
% (c) 2021 por Denilson Andrés Molina Truyot
% Update de linsht.m

%Rutina: -Resolver F1,F2 usando el método TYPE para luego resolver el PVF.

Zf1a = [alpha, 0]; %VI de F1
Zf2a = [0, 1]; %VI de F2

solF1= vectorialSystem(F1, a, b, Zf1a, M,TYPE);
U = solF1(:, 2);
    
solF2= vectorialSystem(F2, a, b, Zf2a, M,TYPE);
V = solF2(:, 2);

h = (b - a) / M;
T = a:h:b;

% Calcular la solucion al problema de valor frontera
X = U + (beta - U(M+1)) * V / V(M+1); 
solPVF = [T', X];
end