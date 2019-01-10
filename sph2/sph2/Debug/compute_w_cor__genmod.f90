        !COMPILER-GENERATED INTERFACE MODULE: Tue Jan 10 20:37:23 2017
        MODULE COMPUTE_W_COR__genmod
          INTERFACE 
            FUNCTION COMPUTE_W_COR(X,XPER,H,N,VOL,W)
              INTEGER(KIND=4) :: N
              REAL(KIND=4) :: X(2,N)
              REAL(KIND=4) :: XPER(2,N)
              REAL(KIND=4) :: H
              REAL(KIND=4) :: VOL(N)
              REAL(KIND=4) :: W(N,N)
              REAL(KIND=4) :: COMPUTE_W_COR
            END FUNCTION COMPUTE_W_COR
          END INTERFACE 
        END MODULE COMPUTE_W_COR__genmod
