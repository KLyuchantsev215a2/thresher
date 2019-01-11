        !COMPILER-GENERATED INTERFACE MODULE: Fri Jan 11 19:36:30 2019
        MODULE COMPUTE_STRESS__genmod
          INTERFACE 
            FUNCTION COMPUTE_STRESS(F,C,MU,K,N)
              INTEGER(KIND=4) :: N
              REAL(KIND=4) :: F(2,2,N)
              REAL(KIND=4) :: C(2,2,N)
              REAL(KIND=4) :: MU
              REAL(KIND=4) :: K
              REAL(KIND=4) :: COMPUTE_STRESS
            END FUNCTION COMPUTE_STRESS
          END INTERFACE 
        END MODULE COMPUTE_STRESS__genmod
