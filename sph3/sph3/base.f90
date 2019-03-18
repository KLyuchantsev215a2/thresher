program base
!spatial integration method  SPH
   ! use ogpf

    
    integer N,i!the number of particles that sample the environment
    integer step!counter for time steps
    integer sqn
    real*8 :: rho_0, T, l,CFL!density, total calculation time ,the size of the side of the square, Courant number
    real*8 :: S,m,h!body area, mass of a single particle , smoothing radius
    real*8 :: nu,mu,cs_0,E,k !material constants
    real*8 :: dh!indent for calculating the derived kernel through finite differences
    real*8 :: dt,time_calculated!time step, time during calculation
    
    real*8, allocatable :: x(:,:)
    real*8, allocatable :: xplot(:,:,:)
    real*8, allocatable :: x_init(:,:)
    real*8, allocatable :: v(:,:)
   
    real*8, allocatable :: W(:,:)
    real*8, allocatable :: Wper1(:,:)
    real*8, allocatable :: Wper2(:,:)
    real*8, allocatable :: Wper3(:,:)!tmp
    real*8, allocatable :: Wper4(:,:)!tmp
    real*8, allocatable :: nabla_W(:,:,:)
    real*8, allocatable :: nabla_W_0(:,:,:)
    
    real*8, allocatable :: C(:,:,:)
   ! real*8, allocatable :: Cochi(:,:,:)
    real*8, allocatable :: F(:,:,:)
    real*8, allocatable :: vol(:)
    
    real*8, allocatable :: acc(:,:)
    real*8, allocatable :: x_0(:,:),x_n_1(:,:),x_n_2(:,:),x_n_1_2(:,:),x_n_3_2(:,:)
    real*8, allocatable :: v_0_0(:,:),v_n_1(:,:),v_n_2(:,:),v_n_1_2(:,:),v_n_3_2(:,:)
    
    real*8 :: test_M(3,3)
    
     interface
        function Compute_W (xi,xj,h)
            real*8 :: xi(2)
            real*8 :: xj(2)
            real*8 :: h 
            real*8 :: Compute_W
        end function Compute_W
      
        function det (M)
            real*8 :: M(3,3)
            real*8 :: det
        end function det
               
        function trace (M)
            real*8 :: M(3,3)
            real*8 :: trace
        end function trace
        
        function trans (M)
            real*8 :: M(3,3)
            real*8 :: trans(3,3)
        end function trans
        
        function dev (M)
            real*8 :: M(3,3)
            real*8 :: dev(3,3)
        end function dev
      
     end interface
    
    open (unit=1, file="input21.txt", status='old',    &
             access='sequential', form='formatted', action='read' )
    open (unit=2, file="output_x.txt", action='write')
    open (unit=3, file="output_C.txt", action='write')
    
       
    
    read (1, 1100) rho_0, T,nu, mu, l, dh,CFL,N 
    write (*, 1113) rho_0, T,nu, mu, l, dh,CFL,N
    
    sqn=21
    S=l*l
    m=rho_0*S/N
    
    k=2.0*mu*(1.0+nu)/(3.0*(1.0-2.0*nu))
    E=9.0*k*mu/(3.0*k+mu)

    cs_0=sqrt((E+4.0/3.0*mu)/rho_0)
    h=1*sqrt(m/rho_0)
    dt=CFL*h/(cs_0)
    
    allocate(vol(N))
    allocate(x(2,N))
    allocate(xplot(2,N,int(T/dt)))
    allocate(x_init(2,N))
    allocate(v(2,N))
    
    allocate(acc(2,N))
    allocate(x_0(2,N),x_n_1(2,N),x_n_2(2,N),x_n_1_2(2,N),x_n_3_2(2,N))
    allocate(v_0_0(2,N),v_n_1(2,N),v_n_2(2,N),v_n_1_2(2,N),v_n_3_2(2,N))
    allocate(W(N,N))
    
    allocate(Wper1(N,N))
    allocate(Wper2(N,N))
    allocate(Wper3(N,N))!tmp
    allocate(Wper4(N,N))!tmp
    
    allocate(nabla_W(2,N,N))
    allocate(nabla_W_0(2,N,N))
    
    allocate(F(2,2,N))
    allocate(C(2,2,N))
    !allocate(Cochi(2,2,N))
   
    vol=m/rho_0
        
    do i=1,N
        read (1, 1110) a,x(1,i),x(2,i)
    enddo
     
    do i=1,N
        read (1, 1110) a,v(1,i),v(2,i)
    enddo
   
   ! do i=1,N!new condition
  !      v(1,i)=0
   !     v(2,i)=0
   ! enddo
    
    x_init=x
    
 
   ! call Compute_W_cor(x,x,h,N,vol,W)
    call Compute_nabla_W(x,h,vol,N,W,Wper1,Wper2,Wper3,Wper4,nabla_W_0,dh)!tmp
   ! call Compute_F(vol,x,x_init,nabla_W_0,N,F)
    !call Compute_Stress(F,C,mu,k,N)
   ! call Compute_Acceleration(N,h,dh,rho_0,mu,k,vol,F,C,x,x_init,nabla_W_0,nabla_W,W,Wper1,Wper2,acc)
    
     
    
    do step=1,int(T/dt)
        x_0=x
        v_0_0=v
        call Compute_Acceleration(N,h,dh,rho_0,mu,k,vol,F,C,x_0,x_init,nabla_W_0,nabla_W,W,Wper1,Wper2,Wper3,Wper4,acc)
        x_n_1=x_0+dt*v_0_0
        v_n_1=v_0_0+dt*acc
        call Compute_Acceleration(N,h,dh,rho_0,mu,k,vol,F,C,x_n_1,x_init,nabla_W_0,nabla_W,W,Wper1,Wper2,Wper3,Wper4,acc)
        x_n_2=x_n_1+dt*v_n_1
        v_n_2=v_n_1+dt*acc
        x_n_1_2=3.0/4.0*x_0+1.0/4.0*x_n_2
        v_n_1_2=3.0/4.0*v_0_0+1.0/4.0*v_n_2
        call Compute_Acceleration(N,h,dh,rho_0,mu,k,vol,F,C,x_n_1_2,x_init,nabla_W_0,nabla_W,W,Wper1,Wper2,Wper3,Wper4,acc)
        x_n_3_2=x_n_1_2+dt*v_n_1_2
        v_n_3_2=v_n_1_2+dt*acc
        x=1.0/3.0*x_0+2.0/3.0*x_n_3_2
        v=1.0/3.0*v_0_0+2.0/3.0*v_n_3_2
        
      !  i=1
     !!    do while(i<=sqn*sqn-sqn+1)  !new condition
       !     x(1,i)=x_init(1,i)
     !!       i=i+sqn
      !   end do
        
      !   i=sqn
      !  do while(i<=N)         !new condition
      !      x(1,i)=x_init(1,i)+x_init(1,i)*(real(step)*dt)*(real(step)*dt)
       !     i=i+sqn
       ! end do
        
        time_calculated=(real(step)*dt)
        
        xplot(1:2,1:N,step)=x
     !  call Convert_PK1_to_Cochi(F,Cochi,C,N)
    
        write (2,1111) x(1,441)-x_init(1,441),x(2,441)-x_init(2,441),time_calculated
        write (3,1112) C(1,2,221),C(1,1,221),C(2,2,221),time_calculated
       ! call  plot(x,N)
    enddo
    
    pause
    call  plot(xplot,N,int(T/dt))
    
    
    deallocate(vol)
    deallocate(x)
    deallocate(x_init)
    deallocate(v)
    
    deallocate(acc)
    deallocate(x_0,x_n_1,x_n_2,x_n_1_2,x_n_3_2)
    deallocate(v_0_0,v_n_1,v_n_2,v_n_1_2,v_n_3_2)
    deallocate(W)
    deallocate(nabla_W)
    deallocate(nabla_W_0)
    
    1100 format (7f10.6,1i4)
    1113 format ("Density "1f10.6,/,"Time "1f10.6,/,"Poisson's ratio " 1f10.6,/,"Shear modulus " 1f10.6,/,"Side of a square " 1f10.6,/,"For finite difference " 1f10.6,/,"CFL " 1f10.6,/,"Particle count " 1i4)
    1110 format (1i12,1f25.0,1f20.0)
    1111 format (3f10.6)
    1112 format (4f10.6)
    
    end program base
    
    
    function Compute_W(xi,xj,h)
        real*8::xi(2)
        real*8::xj(2)
        real*8::h
        
        real*8::r(2)
        real*8::q
        real*8::C
        real*8::KER
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
        
    
    Compute_W=KER
    end function Compute_W
    
    function det (M)
         real*8 :: M(3,3)
        
         det=M(1,1)*M(2,2)-M(1,2)*M(2,1)
         
    end function det
        
    function trace (M)
            real*8 :: M(3,3)
            trace=M(1,1)+M(2,2)+M(3,3)
    end function trace
    
    function trans (M)
    
        real*8 :: M(3,3)
        real*8 :: trans(3,3)
        
        do alpha=1,3
            do beta=1,3
                trans(alpha,beta)=M(beta,alpha)
            enddo
        enddo
            
    end function trans
    
    function dev (M)
            real*8:: M(3,3)
            real*8:: dev(3,3)
            
            dev=M
            
            do alpha=1,3
                dev(alpha,alpha)=dev(alpha,alpha)-(1.0/3.0)*trace(M)
            enddo
   
    end function dev
    
