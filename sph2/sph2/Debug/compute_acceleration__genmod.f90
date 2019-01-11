        !COMPILER-GENERATED INTERFACE MODULE: Fri Jan 11 19:36:30 2019
        MODULE COMPUTE_ACCELERATION__genmod
          INTERFACE 
            FUNCTION COMPUTE_ACCELERATION(N,H,DH,RHO_0,MU,K,VOL,F,C,X,  &
     &X_OLD,NABLA_W_0,NABLA_W,W,ACC)
              INTEGER(KIND=4) :: N
              REAL(KIND=4) :: H
              REAL(KIND=4) :: DH
              REAL(KIND=4) :: RHO_0
              REAL(KIND=4) :: MU
              REAL(KIND=4) :: K
              REAL(KIND=4) :: VOL(N)
              REAL(KIND=4) :: F(2,2,N)
              REAL(KIND=4) :: C(2,2,N)
              REAL(KIND=4) :: X(2,N)
              REAL(KIND=4) :: X_OLD(2,N)
              REAL(KIND=4) :: NABLA_W_0(2,N,N)
              REAL(KIND=4) :: NABLA_W(2,N,N)
              REAL(KIND=4) :: W(N,N)
              REAL(KIND=4) :: ACC(2,N)
              REAL(KIND=4) :: COMPUTE_ACCELERATION
            END FUNCTION COMPUTE_ACCELERATION
          END INTERFACE 
        END MODULE COMPUTE_ACCELERATION__genmod
