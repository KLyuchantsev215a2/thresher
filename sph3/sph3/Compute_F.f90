subroutine Compute_F(vol,x,x_old,nabla_W_0,N,F)
    
    integer :: N
    
    real*8 :: vol(N)
    real*8 :: x(2,N)
    real*8 :: x_old(2,N)
    
    real*8 :: nabla_W_0(2,N,N)
    real*8 :: F(2,2,N)
    real*8 ui,uj
    
    F=0
        do i=1,N
            do j=1,N
                do beta=1,2
                    do alpha=1,2
                        uj=x(alpha,j)-x_old(alpha,j)
                        ui=x(alpha,i)-x_old(alpha,i)
                        F(alpha,beta,i)=F(alpha,beta,i)+vol(j)*(uj-ui)*nabla_W_0(beta,i,j)
                    enddo
                enddo
            enddo
            F(1,1,i)= F(1,1,i)+1.0
            F(2,2,i)= F(2,2,i)+1.0
        enddo
        
    return
end