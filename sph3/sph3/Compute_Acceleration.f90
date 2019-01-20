subroutine Compute_Acceleration(N,h,dh,rho_0,mu,k,vol,F,C,x,x_old,nabla_W_0,nabla_W,W,Wper1,Wper2,acc)
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
    real ::Wper1(N,N)
    real ::Wper2(N,N)
    real :: acc(2,N)

    call compute_W_cor(x,x,h,N,vol,W)
    call Compute_nabla_W(x,h,vol,N,W,Wper1,Wper2,nabla_W,dh)
    call Compute_F(vol,x,x_old,nabla_W_0,N,F)
    call Compute_Stress(F,C,mu,k,N)

    acc=0

    do i=1,N
        do j=1,N
            do beta=1,2
                do alpha=1,2
                    acc(alpha,i)=acc(alpha,i)-(vol(j))*C(alpha,beta,j)*nabla_W(beta,j,i)
                enddo
            enddo
        enddo

        do alpha=1,2
            acc(alpha,i)=acc(alpha,i)/rho_0
        enddo
    enddo
    return
    end