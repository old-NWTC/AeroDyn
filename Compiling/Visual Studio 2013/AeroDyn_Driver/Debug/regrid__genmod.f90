        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 20 09:18:46 2016
        MODULE REGRID__genmod
          INTERFACE 
            SUBROUTINE REGRID(IOPT,MX,X,MY,Y,Z,XB,XE,YB,YE,KX,KY,S,NXEST&
     &,NYEST,NX,TX,NY,TY,C,FP,WRK,LWRK,IWRK,KWRK,IER)
              INTEGER(KIND=4) :: KWRK
              INTEGER(KIND=4) :: LWRK
              INTEGER(KIND=4) :: NYEST
              INTEGER(KIND=4) :: NXEST
              INTEGER(KIND=4) :: KY
              INTEGER(KIND=4) :: KX
              INTEGER(KIND=4) :: MY
              INTEGER(KIND=4) :: MX
              INTEGER(KIND=4) :: IOPT
              REAL(KIND=4) :: X(MX)
              REAL(KIND=4) :: Y(MY)
              REAL(KIND=4) :: Z(MX*MY)
              REAL(KIND=4) :: XB
              REAL(KIND=4) :: XE
              REAL(KIND=4) :: YB
              REAL(KIND=4) :: YE
              REAL(KIND=4) :: S
              INTEGER(KIND=4) :: NX
              REAL(KIND=4) :: TX(NXEST)
              INTEGER(KIND=4) :: NY
              REAL(KIND=4) :: TY(NYEST)
              REAL(KIND=4) :: C((NXEST-KX-1)*(NYEST-KY-1))
              REAL(KIND=4) :: FP
              REAL(KIND=4) :: WRK(LWRK)
              INTEGER(KIND=4) :: IWRK(KWRK)
              INTEGER(KIND=4) :: IER
            END SUBROUTINE REGRID
          END INTERFACE 
        END MODULE REGRID__genmod
