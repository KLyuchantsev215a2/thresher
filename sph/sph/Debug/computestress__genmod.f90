        !COMPILER-GENERATED INTERFACE MODULE: Wed Jan 09 19:33:44 2019
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
