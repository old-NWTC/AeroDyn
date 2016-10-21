        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 20 09:18:46 2016
        MODULE FPBISP__genmod
          INTERFACE 
            SUBROUTINE FPBISP(TX,NX,TY,NY,C,KX,KY,X,MX,Y,MY,Z,WX,WY,LX, &
     &LY)
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
              REAL(KIND=4) :: WX(MX,KX+1)
              REAL(KIND=4) :: WY(MY,KY+1)
              INTEGER(KIND=4) :: LX(MX)
              INTEGER(KIND=4) :: LY(MY)
            END SUBROUTINE FPBISP
          END INTERFACE 
        END MODULE FPBISP__genmod
