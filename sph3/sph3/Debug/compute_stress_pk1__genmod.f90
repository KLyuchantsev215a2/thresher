        !COMPILER-GENERATED INTERFACE MODULE: Wed Mar 13 18:03:30 2019
        MODULE COMPUTE_STRESS_PK1__genmod
          INTERFACE 
            SUBROUTINE COMPUTE_STRESS_PK1(F,KIRC,MU,K,N)
              INTEGER(KIND=4) :: N
              REAL(KIND=8) :: F(2,2,N)
              REAL(KIND=8) :: KIRC(2,2,N)
              REAL(KIND=8) :: MU
              REAL(KIND=8) :: K
            END SUBROUTINE COMPUTE_STRESS_PK1
          END INTERFACE 
        END MODULE COMPUTE_STRESS_PK1__genmod
