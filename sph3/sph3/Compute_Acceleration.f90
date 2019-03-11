subroutine Compute_Acceleration(N,h,dh,rho_0,mu,k,vol,F,C,x,x_old,nabla_W_0,nabla_W,W,Wper1,Wper2,Wper3,Wper4,acc)
    integer :: N
    real*8 :: h
    real*8 :: dh
    real*8 :: rho_0
    real*8 :: mu
    real*8 :: k
    real*8 :: vol(N)
    real*8 :: F(2,2,N)
    real*8 :: C(2,2,N)
    real*8 :: x(2,N)
    real*8 :: x_old(2,N)
    real*8 :: nabla_W_0(2,N,N)
    real*8 :: nabla_W(2,N,N)
    real*8 :: W(N,N)
    real*8 ::Wper1(N,N)
    real*8 ::Wper2(N,N)
    real*8 ::Wper3(N,N)
    real*8 ::Wper4(N,N)
    real*8 :: acc(2,N)

   ! call compute_W_cor(x,x,h,N,vol,W)
    !call Compute_nabla_W(x,h,vol,N,W,Wper1,Wper2,Wper3,Wper4,nabla_W,dh)
    call Compute_F(vol,x,x_old,nabla_W_0,N,F)
    call Compute_Stress_PK1(F,C,mu,k,N)

    acc=0

    do i=1,N
        do j=1,N
            do beta=1,2
                do alpha=1,2
                    acc(alpha,i)=acc(alpha,i)-(vol(j))*C(alpha,beta,j)*nabla_W_0(beta,j,i)
                enddo
            enddo
        enddo

        do alpha=1,2
            acc(alpha,i)=acc(alpha,i)/rho_0
        enddo
    enddo
    return
    end