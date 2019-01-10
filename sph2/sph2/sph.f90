!данный скрипт создает подготовленный файл с вхводными данными для метода
!полная инициализация начальный данных, работа с геометрией области
program sph
!d размерность задачи нигде пока не используется
 
    integer N!количество частиц
    integer i,j!счетчики
    integer a!временная для чтения номера
    real rho_0, v_0, T, l,CFL!плостность, начальная скорость, время расчета, высота квадрата, число куранта
    real S,m,h!площадь квадрата,масса чистицы, радиус сглаживания
    real nu,mu,cs_0,E,k !параметры материала
    real dh!для вычисление через конечные разности градиента ядра
    real, allocatable :: x(:,:)!координаты частиц
    real, allocatable :: x_old(:,:)!координаты частиц в отсчетной конфигурации
    real, allocatable :: v(:,:)!скорость частиц
    real, allocatable :: W(:,:)
    real, allocatable :: nabla_W(:,:,:)
    real, allocatable :: C(:,:,:)
    real, allocatable :: F(:,:,:)
    real, allocatable :: vol(:)
    
    real, allocatable :: acc(:,:)
    
    interface
      function Compute_W (xi,xj,h)
       real :: xi(2)
       real :: xj(2)
       real :: h 
       real :: compute_W
      end function compute_W
      
      function Compute_nabla_W (x,h,vol,N,W,nabla_W,dh)
       integer :: N
       real :: x(2,N)
       real :: h
       real :: vol(N)
       real :: W(N,N)
       real :: nabla_W(2,N,N)
       real :: dh
       real :: Compute_nabla_W 
      end function Compute_nabla_W 
      
      function Compute_W_cor(x,xper,h,N,vol,W)
       integer :: N
       real :: x(2,N)
       real :: xper(2,N)
       real :: h
       real :: vol(N)
       real :: W(N,N)
       real :: Compute_W_cor
        end function Compute_W_cor
    end interface
   
    open (unit=1, file="input.txt", status='old',    &
             access='sequential', form='formatted', action='read' )
    open (unit=2, file="output.txt")
     
    read (1, 1100) rho_0, T,nu, mu, l, dh,CFL,N 
    write (*, 1100)rho_0, T,nu, mu, l, dh,CFL,N
    
    S=l*l
    m=rho_0*S/N
    
    k=2.0*mu*(1.0+nu)/(3.0*(1.0-2.0*nu));
    E=9.0*k*mu/(3.0*k+mu);   

    cs_0=sqrt((E+4.0/3.0*mu)/rho_0);
    h=1.4*sqrt(m/rho_0)
          
    allocate(vol(N))
    allocate(x(2,N))
    allocate(v(2,N))
    allocate(W(N,N))
    allocate(nabla_W(2,N,N))
    allocate(F(2,2,N))
    allocate(C(2,2,N))
   
    vol=m/rho_0
        
    do i=1,N
        read (1, 1110) a,x(1,i),x(2,i)
    enddo
    
    do i=1,N
        read (1, 1110) a,v(1,i),v(2,i)
    enddo
    
    x_old=x
    a=compute_W_cor(x,x,h,N,vol,W)
    a=Compute_nabla_W(x,h,vol,N,W,nabla_W,dh)
    
    deallocate(x)
    deallocate(v)
        pause
        
1100 format (7f10.0,1i3)
1110 format (1i11,1f15.0,1f9.0)
    
    end program sph
    
    !!!!!!!!!!!!!!!!!!!!!!!!!
    function compute_W(xi,xj,h)
        real::xi(2)
        real::xj(2)
        real h
        
        real::r(2)
        real q
        real C
        real KER
        KER=0
        r=xi-xj
        q=sqrt(r(1)*r(1)+r(2)*r(2))/h
        C=1.0/(3.14159265358979323846*h*h)

        if((q>=0)*(q<=1)) then
               KER=C*(10.0 / 7.0)*(1.0-3.0/2.0*q*q*(1.0-q/2.0))
        end if
    
        if ((q > 1) * (q <=2)) then
            KER = C*(10.0 / 7.0)*(1.0/4.0)*(2.0 - q)*(2.0 - q)*(2.0 - q)
        end if
        
    
    compute_W=KER
    end function compute_W


    function Compute_nabla_W(x,h,vol,N,W,nabla_W,dh)
        integer N
        real::x(2,N)
        real::xper1(2,N)
        real::xper2(2,N)
        real h
        real retur
        real::vol(N)
        real::W(N,N)
        real::Wper1(N,N)
        real::Wper2(N,N)
        real::nabla_W(2,N,N)
        real dh
        xper1=x
        xper2=x
        xper1(1,1:N)=xper1(1,1:N)+dh
        xper2(2,1:N)=xper2(2,1:N)+dh
        
        retur=Compute_W_cor(x,x,h,N,vol,W)
        retur=Compute_W_cor(x,xper1,h,N,vol,Wper1)
        retur=Compute_W_cor(x,xper2,h,N,vol,Wper2)
        nabla_W(1,1:N,1:N)=(Wper1-W)/dh
        nabla_W(2,1:N,1:N)=(Wper2-W)/dh
        Compute_nabla_W=1
        
    end function Compute_nabla_W
    
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    function Compute_W_cor(x,xper,h,N,vol,W)
       integer :: N
       real :: x(2,N)
       real :: xper(2,N)
       real :: h
       real :: vol(N)
       real :: W(N,N)
        
        real ::xi(2)
        real ::xj(2)
        real ::r(2)
        real ::sumW(2)
        real ::betaij(2)
        real ::cormat(2,2)
        real ::cormat_tmp(2,2)
        real alpha,detcormat_tmp
        
        do i=1,N
            sumW=0
            alpha=0
            betaij=0
            cormat=0
            cormat_tmp=0
            xi=xper(1:2,i)
            
            do j=1,N
                xj=x(1:2,j)
                r=xj-xi
                sumW(1)=sumW(1)+vol(j)*Compute_W(xi,xj,h)*r(1)
                sumW(2)=sumW(2)+vol(j)*Compute_W(xi,xj,h)*r(2)
            enddo
            
            do j=1,N
                xj=x(1:2,j)
                r=xi-xj
                cormat_tmp(1,1)=cormat_tmp(1,1)+r(1)*r(1)*vol(j)*Compute_W(xi,xj,h)
                cormat_tmp(1,2)=cormat_tmp(1,2)+r(1)*r(2)*vol(j)*Compute_W(xi,xj,h)
                cormat_tmp(2,1)=cormat_tmp(2,1)+r(2)*r(1)*vol(j)*Compute_W(xi,xj,h)
                cormat_tmp(2,2)=cormat_tmp(2,2)+r(2)*r(2)*vol(j)*Compute_W(xi,xj,h)
            enddo
            detcormat_tmp=cormat_tmp(1,1)*cormat_tmp(2,2)-cormat_tmp(1,2)*cormat_tmp(2,1)
            cormat(1,1)=cormat_tmp(2,2)/detcormat_tmp
            cormat(1,2)=-cormat_tmp(1,2)/detcormat_tmp
            cormat(2,1)=-cormat_tmp(2,1)/detcormat_tmp
            cormat(2,2)=cormat_tmp(1,1)/detcormat_tmp
            
            betaij(1)=cormat(1,1)*sumW(1)+cormat(1,2)*sumW(2)
            betaij(2)=cormat(1,2)*sumW(1)+cormat(2,2)*sumW(2)
            
            do j=1,N
                xj=x(1:2,j)
                r=xi-xj
                alpha=alpha+vol(j)*(1.0+betaij(1)*r(1)+betaij(2)*r(2))*Compute_W(xi,xj,h)
            enddo
            alpha=1.0/alpha
            
            do j=1,N
                xj=x(1:2,j)
                r=xi-xj
                W(i,j)=Compute_W(xi,xj,h)*alpha*(1.0+betaij(1)*r(1)+betaij(2)*r(2))
            enddo
            
        enddo
        
        Compute_W_cor=0.0
      end function Compute_W_cor