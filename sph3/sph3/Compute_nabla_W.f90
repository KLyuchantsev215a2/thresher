subroutine Compute_nabla_W(x,h,vol,N,W,Wper1,Wper2,nabla_W,dh)
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
        
        call Compute_W_cor(x,x,h,N,vol,W)
        call Compute_W_cor(x,xper1,h,N,vol,Wper1)
        call Compute_W_cor(x,xper2,h,N,vol,Wper2)
        
        nabla_W(1,1:N,1:N)=(Wper1(1:N,1:N)-W(1:N,1:N))/dh
        nabla_W(2,1:N,1:N)=(Wper2(1:N,1:N)-W(1:N,1:N))/dh
        
        return
    end 