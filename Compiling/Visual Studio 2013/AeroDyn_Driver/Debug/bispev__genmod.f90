        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 20 09:18:46 2016
        MODULE BISPEV__genmod
          INTERFACE 
            SUBROUTINE BISPEV(TX,NX,TY,NY,C,KX,KY,X,MX,Y,MY,Z,WRK,LWRK, &
     &IWRK,KWRK,IER)
              INTEGER(KIND=4) :: KWRK
              INTEGER(KIND=4) :: LWRK
              INTEGER(KIND=4) :: MY
              INTEGER(KIND=4) :: MX
              INTEGER(KIND=4) :: KY
              INTEGER(KIND=4) :: KX
              INTEGER(KIND=4) :: NY
              INTEGER(KIND=4) :: NX
              REAL(KIND=4) :: TX(NX)
              REAL(KIND=4) :: TY(NY)
              REAL(KIND=4) :: C((NX-KX-1)*(NY-KY-1))
              REAL(KIND=4) :: X(MX)
              REAL(KIND=4) :: Y(MY)
              REAL(KIND=4) :: Z(MX*MY)
              REAL(KIND=4) :: WRK(LWRK)
              INTEGER(KIND=4) :: IWRK(KWRK)
              INTEGER(KIND=4) :: IER
            END SUBROUTINE BISPEV
          END INTERFACE 
        END MODULE BISPEV__genmod
