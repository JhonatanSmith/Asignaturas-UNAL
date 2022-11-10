function S=csnatural(X,Y)

% Entrada   - X es el vector de abscisas  1 x n
%           - Y es el vector de ordenadas 1 x n
% Salida    - S: las filas de S son los coeficientes 
%                para los interpolantes (trazadores) cubicos

N=length(X)-1;
H=diff(X);
D=diff(Y)./H;
U=6*diff(D);

if N<2
    disp('No aplica'),S=[];return
elseif N==2
    m=U(1)/(2*(H(2)+H(1)));
else
    C=H(2:N-1);
    B=2*(H(1:N-1)+H(2:N));
    T=gallery('tridiag',C,B,C);
    %% T=diag(B)+diag(C,1)+diag(C,-1);
    m=T\U';
end

M=[0;m;0];

%%% Coeficientes
for k=0:N-1
   S(k+1,1)=(M(k+2)-M(k+1))/(6*H(k+1));
   S(k+1,2)=M(k+1)/2;
   S(k+1,3)=D(k+1)-H(k+1)*(2*M(k+1)+M(k+2))/6;
   S(k+1,4)=Y(k+1);
end