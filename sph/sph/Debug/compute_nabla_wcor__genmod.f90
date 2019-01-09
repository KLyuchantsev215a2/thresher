        !COMPILER-GENERATED INTERFACE MODULE: Wed Jan 09 19:33:44 2019
        MODULE COMPUTE_NABLA_WCOR__genmod
          INTERFACE 
            FUNCTION COMPUTE_NABLA_WCOR(X,H,VOL,N,W,NABLA_W,DH)
              INTEGER(KIND=4) :: N
              REAL(KIND=4) :: X(2,N)
              REAL(KIND=4) :: H
              REAL(KIND=4) :: VOL(N)
              REAL(KIND=4) :: W(N,N)
              REAL(KIND=4) :: NABLA_W(2,N,N)
              REAL(KIND=4) :: DH
              REAL(KIND=4) :: COMPUTE_NABLA_WCOR
            END FUNCTION COMPUTE_NABLA_WCOR
          END INTERFACE 
        END MODULE COMPUTE_NABLA_WCOR__genmod
