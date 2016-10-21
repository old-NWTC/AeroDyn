        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 20 09:18:46 2016
        MODULE FPREGR__genmod
          INTERFACE 
            SUBROUTINE FPREGR(IOPT,X,MX,Y,MY,Z,MZ,XB,XE,YB,YE,KX,KY,S,  &
     &NXEST,NYEST,TOL,MAXIT,NC,NX,TX,NY,TY,C,FP,FP0,FPOLD,REDUCX,REDUCY,&
     &FPINTX,FPINTY,LASTDI,NPLUSX,NPLUSY,NRX,NRY,NRDATX,NRDATY,WRK,LWRK,&
     &IER)
              INTEGER(KIND=4) :: LWRK
              INTEGER(KIND=4) :: NC
              INTEGER(KIND=4) :: NYEST
              INTEGER(KIND=4) :: NXEST
              INTEGER(KIND=4) :: MZ
              INTEGER(KIND=4) :: MY
              INTEGER(KIND=4) :: MX
              INTEGER(KIND=4) :: IOPT
              REAL(KIND=4) :: X(MX)
              REAL(KIND=4) :: Y(MY)
              REAL(KIND=4) :: Z(MZ)
              REAL(KIND=4) :: XB
              REAL(KIND=4) :: XE
              REAL(KIND=4) :: YB
              REAL(KIND=4) :: YE
              INTEGER(KIND=4) :: KX
              INTEGER(KIND=4) :: KY
              REAL(KIND=4) :: S
              REAL(KIND=4) :: TOL
              INTEGER(KIND=4) :: MAXIT
              INTEGER(KIND=4) :: NX
              REAL(KIND=4) :: TX(NXEST)
              INTEGER(KIND=4) :: NY
              REAL(KIND=4) :: TY(NYEST)
              REAL(KIND=4) :: C(NC)
              REAL(KIND=4) :: FP
              REAL(KIND=4) :: FP0
              REAL(KIND=4) :: FPOLD
              REAL(KIND=4) :: REDUCX
              REAL(KIND=4) :: REDUCY
              REAL(KIND=4) :: FPINTX(NXEST)
              REAL(KIND=4) :: FPINTY(NYEST)
              INTEGER(KIND=4) :: LASTDI
              INTEGER(KIND=4) :: NPLUSX
              INTEGER(KIND=4) :: NPLUSY
              INTEGER(KIND=4) :: NRX(MX)
              INTEGER(KIND=4) :: NRY(MY)
              INTEGER(KIND=4) :: NRDATX(NXEST)
              INTEGER(KIND=4) :: NRDATY(NYEST)
              REAL(KIND=4) :: WRK(LWRK)
              INTEGER(KIND=4) :: IER
            END SUBROUTINE FPREGR
          END INTERFACE 
        END MODULE FPREGR__genmod
