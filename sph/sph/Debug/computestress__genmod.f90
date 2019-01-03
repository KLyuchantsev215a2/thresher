        !COMPILER-GENERATED INTERFACE MODULE: Thu Jan 03 19:36:39 2019
        MODULE COMPUTESTRESS__genmod
          INTERFACE 
            FUNCTION COMPUTESTRESS(F,C,MU,K,N)
              INTEGER(KIND=4) :: N
              REAL(KIND=4) :: F(2,2,N)
              REAL(KIND=4) :: C(2,2,N)
              REAL(KIND=4) :: MU
              REAL(KIND=4) :: K
              REAL(KIND=4) :: COMPUTESTRESS
            END FUNCTION COMPUTESTRESS
          END INTERFACE 
        END MODULE COMPUTESTRESS__genmod
