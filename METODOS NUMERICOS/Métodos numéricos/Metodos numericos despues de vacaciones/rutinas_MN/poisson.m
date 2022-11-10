function U = poisson(g,f1,f2,f3,f4,a,b,c,d,m,n)
% poisson.m  -- Resuelve el problema de Poisson u_{xx} + u_{yy} = g(x,y)
% en [a,b] x [c,d], usando el metodo de diferencias finitas de cinco puntos
% g funcion creada con @
% f1 es la condicion de frontera abajo (en y=c) funcion creada con @
% f2 es la condicion de frontera derecha (en x=b) funcion creada con @
% f3 es la condicion de frontera arriba (en y=d) funcion creada con @
% f4 es la condicion de frontera izquierda (en x=a) funcion creada con @
% Las funciones f1, f2, f3 y f4 se ingresan como cadenas de carateres
% m es el numero de puntos sobre el eje x
% n es el numero de puntos sobre el eje y

mx=m-2;  %numero de puntos interiores del intervalo [a,b]
my=n-2;  %numero de puntos interiores del intervalo [c,d]
h=(b-a)/(m-1);  % tamanno de paso en x
k=(d-c)/(n-1);  % tamanno de paso en y

    x = linspace(a,b,mx+2);   % malla en x incluida la frontera
    y = linspace(c,d,my+2);   % malla en y incluida la frontera


    [X,Y] = meshgrid(x,y);    % malla en x y y
    X = X';                      
    Y = Y';                     

    Iint = 2:mx+1;            % indices para puntos interiores, vector  x
    Jint = 2:my+1;            % indices para puntos interiores, vector  y
    
    Xint = X(Iint,Jint);      % puntos interiores de la malla rectangular
    Yint = Y(Iint,Jint);

   
    rhs=feval(g,Xint,Yint);  % Evaluaci�n de g en los puntos interiores.
                             % Para los valores de frontera, el arreglo rhs
                             % se modificar� en una instrucci�n posterior
                             % (lineas 39 a 53 de este programa)
                               

   
   % Adecaci�n de las condiciones de frontera del arreglo soluci�n: usoln 
   
    v1=feval(f1,x(Iint));
    v3=feval(f3,x(Iint));
   
    v1=v1';
    v3=v3';
            
    v2=feval(f2,y(Jint));
    v4=feval(f4,y(Jint));
        
    rhs(:,1) = rhs(:,1)-v1/k^2;
    rhs(:,my) = rhs(:,my)-v3/k^2;
    rhs(1,:) = rhs(1,:)-v4/h^2;
    rhs(mx,:) = rhs(mx,:)-v2/h^2;


    % En la linea 57 se convierte el arreglo bidimensional en un arreglo 
    % vectorial, (en un vector columna):
    F = reshape(rhs,mx*my,1);

   
    % Construcci�n de la matriz asociada al esquema de diferencias finitas:
    I1 = speye(mx);
    I2 = speye(my);
    e1 = ones(mx,1);
    e2 = ones(my,2);
    
       
    T1= spdiags([e1 -2*e1 e1],[-1 0 1],mx,mx);
    
    T2=-2*I1;
    S2=spdiags([e2 e2],[-1 1],my,my);
        
    A1=kron(I2,T1)/h^2;
    %B1=full(A1);
    A2=(kron(I2,T2)+kron(S2,I1))/k^2;
    %B2=full(A2);
    
    A=A1+A2;
       
    % Soluci�n del sistema lineal:
    uvec = A\F;  

    % Se cambia el vector soluci�n, uvec por un arreglo bidimensional 
    % Este arreglo bidimensional, usoln, corresponde a la soluci�n
    % aproximada en los puntos de la malla inicial.
    usoln(Iint,Jint) = reshape(uvec,mx,my);
   

 usoln(2:mx+1,2:my+1);
 w2=feval(f2,y);
 w4=feval(f4,y);
 U=usoln(2:mx+1,2:my+1);
 U=[v1 U v3];
 U=[w4;U;w2];
 U=U';