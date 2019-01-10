!данный скрипт создает подготовленный файл с вхводными данными для метода
!полная инициализация начальный данных, работа с геометрией области
program sph
!d размерность задачи нигде пока не используется
 
    integer N!количество частиц
    integer step!счетчик
    integer a!временная для чтения номера
    real rho_0, v_0, T, l,CFL!плостность, начальная скорость, время расчета, высота квадрата, число куранта
    real S,m,h!площадь квадрата,масса чистицы, радиус сглаживания
    real nu,mu,cs_0,E,k !параметры материала
    real dh!для вычисление через конечные разности градиента ядра
    real dt,time
    real, allocatable :: x(:,:)!координаты частиц
    real, allocatable :: x_old(:,:)!координаты частиц в отсчетной конфигурации
    real, allocatable :: v(:,:)!скорость частиц
   
    real, allocatable :: W(:,:)
    real, allocatable :: nabla_W(:,:,:)
    real, allocatable :: nabla_W_0(:,:,:)
    
    real, allocatable :: C(:,:,:)
    real, allocatable :: F(:,:,:)
    real, allocatable :: vol(:)
    
    real, allocatable :: acc(:,:)
    real, allocatable :: x_0(:,:),x_n_1(:,:),x_n_2(:,:),x_n_1_2(:,:),x_n_3_2(:,:)
    real, allocatable :: v_0_0(:,:),v_n_1(:,:),v_n_2(:,:),v_n_1_2(:,:),v_n_3_2(:,:)
    
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
      
    function Compute_F(vol,x,x_old,nabla_W_0,N,F)
              integer :: N
              real :: VOL(N)
              real :: x(2,N)
              real :: x_old(2,N)
              real :: nabla_W_0(2,N,N)
              real :: F(2,2,N)
              real :: Compute_F
    end function Compute_F
    
    function Compute_Stress(F,C,mu,k,N)
              integer:: N
              real :: F(2,2,N)
              real :: C(2,2,N)
              real :: mu
              real :: k
              real :: Compute_stress
    end function Compute_stress
    
    function Compute_Acceleration(N,h,dh,rho_0,mu,k,vol,F,C,x,x_old,nabla_W_0,nabla_W,W,acc)
              integer :: N
              real :: h
              real :: dh
              real :: rho_0
              real :: mu
              real :: k
              real :: vol(N)
              real :: F(2,2,N)
              real :: C(2,2,N)
              real :: x(2,N)
              real :: x_old(2,N)
              real :: nabla_W_0(2,N,N)
              real :: nabla_W(2,N,N)
              real :: W(N,N)
              real :: acc(2,N)
              real :: Compute_Acceleration
            end function Compute_Acceleration
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
    dt=CFL*h/cs_0
    
    allocate(vol(N))
    allocate(x(2,N))
    allocate(x_old(2,N))
    allocate(v(2,N))
    
    allocate(acc(2,N))
    allocate(x_0(2,N),x_n_1(2,N),x_n_2(2,N),x_n_1_2(2,N),x_n_3_2(2,N))
    allocate(v_0_0(2,N),v_n_1(2,N),v_n_2(2,N),v_n_1_2(2,N),v_n_3_2(2,N))
    allocate(W(N,N))
    allocate(nabla_W(2,N,N))
    allocate(nabla_W_0(2,N,N))
    
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
    a=Compute_nabla_W(x,h,vol,N,W,nabla_W_0,dh)
    a=Compute_F(vol,x,x_old,nabla_W_0,N,F)
    a=Compute_Stress(F,C,mu,k,N)
    a=Compute_Acceleration(N,h,dh,rho_0,mu,k,vol,F,C,x,x_old,nabla_W_0,nabla_W,W,acc)
    
    step=int(T/dt)
    do step=1,int(T/dt)
        x_0=x
        v_0_0=v
        a=Compute_Acceleration(N,h,dh,rho_0,mu,k,vol,F,C,x_0,x_old,nabla_W_0,nabla_W,W,acc)
        x_n_1=x_old+dt*v_0_0
        v_n_1=v_0_0+dt*acc
        a=Compute_Acceleration(N,h,dh,rho_0,mu,k,vol,F,C,x_n_1,x_old,nabla_W_0,nabla_W,W,acc)
        x_n_2=x_n_1+dt*v_n_1
        v_n_2=v_n_1+dt*acc
        x_n_1_2=3.0/4.0*x_old+1.0/4.0*x_n_2
        v_n_1_2=3.0/4.0*v_0_0+1.0/4.0*v_n_2
        a=Compute_Acceleration(N,h,dh,rho_0,mu,k,vol,F,C,x_n_1_2,x_old,nabla_W_0,nabla_W,W,acc)
        x_n_3_2=x_n_1_2+dt*v_n_1_2
        v_n_3_2=v_n_1_2+dt*acc
        x=1.0/3.0*x_0+2.0/3.0*x_n_3_2;
        v=1.0/3.0*v_0_0+2.0/3.0*v_n_3_2;
        lifetime=(step*dt)
        write(*,1111) v(1,N),v(2,N),lifetime
    enddo
    
    
    
    
    deallocate(nabla_W_0)
    deallocate(x)
    deallocate(v)
        pause
        
1100 format (7f10.0,1i3)
1110 format (1i11,1f15.0,1f9.0)
1111 format (3f10.6)
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

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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
    
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    function Compute_F(vol,x,x_old,nabla_W_0,N,F)
              integer :: N
              real :: vol(N)
              real :: x(2,N)
              real :: x_old(2,N)
              real :: nabla_W_0(2,N,N)
              real :: F(2,2,N)
              F=0
              do i=1,N
                  do j=1,N
                      do beta=1,2
                          do alpha=1,2
                              F(alpha,beta,i)=F(alpha,beta,i)+vol(j)*((x(alpha,j)-x_old(alpha,j))-(x(alpha,i)-x_old(alpha,i)))*nabla_W_0(beta,i,j)
                          enddo
                      enddo
                  enddo
                  F(1,1,i)= F(1,1,i)+1
                  F(2,2,i)= F(2,2,i)+1
              enddo   
              
              Compute_F=1
    end function Compute_F
    
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    function Compute_Stress(F,C,mu,k,N)
              integer:: N
              real :: F(2,2,N)
              real :: C(2,2,N)
              real :: mu
              real :: k
              
              real::detFp
              real ::Fp(3,3)
              real :: trans_Fp(3,3)
              real ::B(3,3)
              real ::B_iso(3,3)
              real ::dev_B_iso(3,3)
              C=0
              B=0
              do i=1,N
                  
                  do alpha=1,2
                      Fp(alpha,3)=0
                      Fp(3,alpha)=0
                      
                      do beta=1,2
                          Fp(alpha,beta)=F(alpha,beta,i)
                      enddo
                  enddo
                  
                  Fp(3,3)=1
                  detFp=Fp(1,1)*Fp(2,2)-Fp(1,2)*Fp(2,1)
                  
                  do alpha=1,3
                      do beta=1,3
                          trans_Fp(alpha,beta)=Fp(beta,alpha)
                      enddo
                  enddo
                  
                  do alpha=1,3
                      do beta=1,3
                          do gamma=1,3
                            B(alpha,beta)=B(alpha,beta)+Fp(alpha,gamma)*trans_Fp(gamma,beta)
                          enddo
                      enddo
                  enddo
                  
                  B_iso=detFp**(-2.0/3.0)*B
                  dev_B_iso(1,1)=B_iso(1,1)-1.0/3.0*(B_iso(1,1)+B_iso(2,2)+B_iso(3,3))
                  dev_B_iso(2,2)=B_iso(2,2)-1.0/3.0*(B_iso(1,1)+B_iso(2,2)+B_iso(3,3))
                  dev_B_iso(3,3)=B_iso(3,3)-1.0/3.0*(B_iso(1,1)+B_iso(2,2)+B_iso(3,3))
                  C(1:2,1:2,i)=mu*dev_B_iso(1:2,1:2)
                  
                  C(1,1,i)=C(1,1,i)+k/10.0*(detFp**5-detFp**(-5))/detFp!ВОПРОС
                  C(2,2,i)=C(2,2,i)+k/10.0*(detFp**5-detFp**(-5))/detFp!ВОПРОС
                  
               enddo   
                  
              
              Compute_stress=1
    end function Compute_stress
    
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
        function Compute_Acceleration(N,h,dh,rho_0,mu,k,vol,F,C,x,x_old,nabla_W_0,nabla_W,W,acc)
              integer :: N
              real :: h
              real :: dh
              real :: rho_0
              real :: mu
              real :: k
              real :: vol(N)
              real :: F(2,2,N)
              real :: C(2,2,N)
              real :: x(2,N)
              real :: x_old(2,N)
              real :: nabla_W_0(2,N,N)
              real :: nabla_W(2,N,N)
              real :: W(N,N)
              real :: acc(2,N)
              
              a=compute_W_cor(x,x,h,N,vol,W)
              a=Compute_nabla_W(x,h,vol,N,W,nabla_W,dh)
              a=Compute_F(vol,x,x_old,nabla_W_0,N,F)
              a=Compute_Stress(F,C,mu,k,N)
              
              acc=0
              
              do i=1,N
                  do j=1,N
                    do beta=1,2
                        do alpha=1,2
                            acc(alpha,i)=acc(alpha,i)-vol(j)*C(alpha,beta,j)*nabla_W(beta,j,i)
                        enddo
                    enddo
                  enddo
                  
                  do alpha=1,2
                      acc(alpha,i)=acc(alpha,i)/rho_0
                  enddo
              enddo
              
              
              
              
              Compute_Acceleration=1
            end function Compute_Acceleration