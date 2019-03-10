        !COMPILER-GENERATED INTERFACE MODULE: Sun Mar 10 16:20:44 2019
        MODULE ONESTEPMAXWELL__genmod
          INTERFACE 
            SUBROUTINE ONESTEPMAXWELL(FP,MU,K,ETA,DT,CIP,N,C,CI_NEW)
              INTEGER(KIND=4) :: N
              REAL(KIND=8) :: FP(3,3)
              REAL(KIND=8) :: MU
              REAL(KIND=8) :: K
              REAL(KIND=4) :: ETA
              REAL(KIND=4) :: DT
              REAL(KIND=4) :: CIP
              REAL(KIND=8) :: C(2,2,N)
              REAL(KIND=4) :: CI_NEW
            END SUBROUTINE ONESTEPMAXWELL
          END INTERFACE 
        END MODULE ONESTEPMAXWELL__genmod
