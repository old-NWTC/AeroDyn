        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 20 09:18:46 2016
        MODULE FPGRRE__genmod
          INTERFACE 
            SUBROUTINE FPGRRE(IFSX,IFSY,IFBX,IFBY,X,MX,Y,MY,Z,MZ,KX,KY, &
     &TX,NX,TY,NY,P,C,NC,FP,FPX,FPY,MM,MYNX,KX1,KX2,KY1,KY2,SPX,SPY,    &
     &RIGHT,Q,AX,AY,BX,BY,NRX,NRY)
              INTEGER(KIND=4) :: KY2
              INTEGER(KIND=4) :: KY1
              INTEGER(KIND=4) :: KX2
              INTEGER(KIND=4) :: KX1
              INTEGER(KIND=4) :: MYNX
              INTEGER(KIND=4) :: MM
              INTEGER(KIND=4) :: NC
              INTEGER(KIND=4) :: NY
              INTEGER(KIND=4) :: NX
              INTEGER(KIND=4) :: MZ
              INTEGER(KIND=4) :: MY
              INTEGER(KIND=4) :: MX
              INTEGER(KIND=4) :: IFSX
              INTEGER(KIND=4) :: IFSY
              INTEGER(KIND=4) :: IFBX
              INTEGER(KIND=4) :: IFBY
              REAL(KIND=4) :: X(MX)
              REAL(KIND=4) :: Y(MY)
              REAL(KIND=4) :: Z(MZ)
              INTEGER(KIND=4) :: KX
              INTEGER(KIND=4) :: KY
              REAL(KIND=4) :: TX(NX)
              REAL(KIND=4) :: TY(NY)
              REAL(KIND=4) :: P
              REAL(KIND=4) :: C(NC)
              REAL(KIND=4) :: FP
              REAL(KIND=4) :: FPX(NX)
              REAL(KIND=4) :: FPY(NY)
              REAL(KIND=4) :: SPX(MX,KX1)
              REAL(KIND=4) :: SPY(MY,KY1)
              REAL(KIND=4) :: RIGHT(MM)
              REAL(KIND=4) :: Q(MYNX)
              REAL(KIND=4) :: AX(NX,KX2)
              REAL(KIND=4) :: AY(NY,KY2)
              REAL(KIND=4) :: BX(NX,KX2)
              REAL(KIND=4) :: BY(NY,KY2)
              INTEGER(KIND=4) :: NRX(MX)
              INTEGER(KIND=4) :: NRY(MY)
            END SUBROUTINE FPGRRE
          END INTERFACE 
        END MODULE FPGRRE__genmod
