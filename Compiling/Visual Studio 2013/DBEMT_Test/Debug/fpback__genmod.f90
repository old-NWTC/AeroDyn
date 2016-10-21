        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 20 09:18:46 2016
        MODULE FPBACK__genmod
          INTERFACE 
            SUBROUTINE FPBACK(A,Z,N,K,C,NEST)
              INTEGER(KIND=4) :: NEST
              INTEGER(KIND=4) :: K
              INTEGER(KIND=4) :: N
              REAL(KIND=4) :: A(NEST,K)
              REAL(KIND=4) :: Z(N)
              REAL(KIND=4) :: C(N)
            END SUBROUTINE FPBACK
          END INTERFACE 
        END MODULE FPBACK__genmod
