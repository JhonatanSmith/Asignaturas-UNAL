function  Y = sor (A, B, P, w, delta, max1)

% Entrada  - A es una matriz no singular  N x N
%          - B es una matriz  N x 1
%          - P es una matriz  N x 1; el supuesto inicial
%          - w parametro de sobrerelajacion (0<w<2)
%	       - delta es la tolerancia para  P
%	       - max1 es el numero maximo de iteraciones
% Salida   - Y es una matriz  N x 1: la aproximacion de SOR a
%	         la solucion de  AX = B

%  Basada en
%  METODOS NUMERICOS: Programas en Matlab
% (c) 2004 por John H. Mathews y Kurtis D. Fink
%  Software complementario acompañando al texto:
%  METODOS NUMERICOS con Matlab, Cuarta Edicion
%  ISBN: 0-13-065248-2
%  Prentice-Hall Pub. Inc.
%  One Lake Street
%  Upper Saddle River, NJ 07458

N = length(B);

for  k = 1:max1
   for  j = 1:N
      if  j == 1
        X(1) = (B(1) - A(1, 2:N) * P(2:N)) / A(1, 1);
        Y(1) = (1-w) * P(1) + w * X(1); 
      elseif  j == N
	     X(N) = (B(N) - A(N, 1:N-1) * (Y(1:N-1))') / A(N, N);
         Y(N) = (1-w) * P(N) + w * X(N);
      else
	     % X contiene la k-esima aproximacion  y  P la (k-1)-esima
        X(j) = (B(j) - A(j, 1:j-1) * Y(1:j-1)' - A(j, j+1:N) * P(j+1:N)) / A(j, j);
        Y(j) = (1-w) * P(j) + w * X(j);
      end
   end
   err = abs(norm(Y' - P));
   relerr = err / (norm(Y) + eps);
% %%%%%%%%%%PPPPPPPPPPPPPPPPPPPPP
%     out=[k,Y];
%     fprintf('%4.0f %13.3e %13.3e %13.3e \n',out);
% %%%%%%%%%%%%%%%%%%%%%%%%%PPPPPPPPPPPPPPPP
    P = Y';
      if  (err < delta)  |  (relerr < delta)
     break
   end
end

Y = Y';
k