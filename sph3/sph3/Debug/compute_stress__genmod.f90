        !COMPILER-GENERATED INTERFACE MODULE: Sun Jan 20 12:55:39 2019
        MODULE COMPUTE_STRESS__genmod
          INTERFACE 
            SUBROUTINE COMPUTE_STRESS(F,C,MU,K,N)
              INTEGER(KIND=4) :: N
              REAL(KIND=4) :: F(2,2,N)
              REAL(KIND=4) :: C(2,2,N)
              REAL(KIND=4) :: MU
              REAL(KIND=4) :: K
            END SUBROUTINE COMPUTE_STRESS
          END INTERFACE 
        END MODULE COMPUTE_STRESS__genmod
