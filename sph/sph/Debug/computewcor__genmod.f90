        !COMPILER-GENERATED INTERFACE MODULE: Thu Jan 03 22:40:30 2019
        MODULE COMPUTEWCOR__genmod
          INTERFACE 
            FUNCTION COMPUTEWCOR(X,XPER,H,VOL,N,W)
              INTEGER(KIND=4) :: N
              REAL(KIND=4) :: X(2,N)
              REAL(KIND=4) :: XPER(2,N)
              REAL(KIND=4) :: H
              REAL(KIND=4) :: VOL(N)
              REAL(KIND=4) :: W(N,N)
              REAL(KIND=4) :: COMPUTEWCOR
            END FUNCTION COMPUTEWCOR
          END INTERFACE 
        END MODULE COMPUTEWCOR__genmod
