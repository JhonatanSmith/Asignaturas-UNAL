

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Instrucciones para uso: Sistermas de ecuaciones %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% SISTEMA DE ECUACIONES BASE
[Ac_base, B] = sistemabase(X,Y)

% SISTEMA DE ECUACIONES: SPLINE CUBICO NATURAL 
[Ac_natural, B] = sistemanatural(X,Y)

% SISTEMA DE ECUACIONES FRONTERA SUJETA
[Ac_parabolica, B] = sistemaparabolico(X,Y)

% SISTEMA DE ECUACIONES: FRONTERA EXTRAPOLADA
[Ac_extrapolado, B] = sistemaextrapolado(X,Y)

% SISTEMA DE ECUACIONES: FRONTERA SUJETA

% IMPORTANTE: El resultado corresponde a los extremos del sistema.
%      Las ecuaciones intermedias (entre las dos que arroja el resultado)
%      Corresponderán a las del Sistema de ecuaciones base.
%      B es modificado, deberán efectuar la resta manualmente. 
% Por ejemplo si el sistema es 3c1 + 3c2 + 4 = 12 
%     pues pasan el 4 a restar al otro lado 
% dsx0 = Valor de la primera derivada en x0
% dsxn = Valor de la primera derivada en xn
[Ac_sujeto, B] = sistemasujeto(X,Y,dsx0,dsxn)

% SISTEMA DE ECUACIONES: CURVATURA CONOCIDA

% IMPORTANTE: El resultado corresponde a los extremos del sistema.
%      Las ecuaciones intermedias (entre las dos que arroja el resultado)
%      Corresponderán a las del Sistema de ecuaciones base.
%      B es modificado, deberán efectuar la resta manualmente. 
% Por ejemplo si el sistema es 3c1 + 3c2 + 4 = 12 
%     pues pasan el 4 a restar al otro lado 
% dsx0 = Valor de la segunda derivada en x0
% dsxn = Valor de la segunda derivada en xn

[Ac_curvatura, B] = sistemacurvatura(X,Y,ddsx0,ddsxn)


