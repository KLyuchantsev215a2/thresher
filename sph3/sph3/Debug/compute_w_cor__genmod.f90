        !COMPILER-GENERATED INTERFACE MODULE: Sun Jan 20 12:56:47 2019
        MODULE COMPUTE_W_COR__genmod
          INTERFACE 
            SUBROUTINE COMPUTE_W_COR(X,XPER,H,N,VOL,W)
              INTEGER(KIND=4) :: N
              REAL(KIND=4) :: X(2,N)
              REAL(KIND=4) :: XPER(2,N)
              REAL(KIND=4) :: H
              REAL(KIND=4) :: VOL(N)
              REAL(KIND=4) :: W(N,N)
            END SUBROUTINE COMPUTE_W_COR
          END INTERFACE 
        END MODULE COMPUTE_W_COR__genmod
