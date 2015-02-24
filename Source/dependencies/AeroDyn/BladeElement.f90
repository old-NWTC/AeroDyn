module BladeElement
   
   use AirfoilInfo_Types
   
   public :: BE_CalcCxCyCoefs
   public :: BE_CalcOutputs
   
   contains
   
   subroutine BE_CalcCxCyCoefs(phi, useAIDrag, useTIDrag, Cl, Cd, Cx, Cy)
      real(ReKi),             intent(in   ) :: phi
      logical,                intent(in   ) :: useAIDrag
      logical,                intent(in   ) :: useTIDrag
      real(ReKi),             intent(in   ) :: Cl
      real(ReKi),             intent(in   ) :: Cd
      real(ReKi),             intent(  out) :: Cx
      real(ReKi),             intent(  out) :: Cy
   
         !Locals
      real(ReKi)      cphi, sphi
   
      cphi = cos(phi)
      sphi = sin(phi)
   
            ! resolve into normal (x) and tangential (y) forces
       if (  useAIDrag ) then
           Cx = Cl*cphi + Cd*sphi
       else      
           Cx = Cl*cphi
       end if
    
       if (  useTIDrag ) then     
           Cy = Cl*sphi - Cd*cphi
       else     
           Cy = Cl*sphi
       end if
   end subroutine BE_CalcCxCyCoefs
   
   subroutine BE_CalcOutputs(AFInfo, AOA, Re, Cl, Cd, ErrStat, ErrMsg)
   
      type(AFInfoType), intent(in   ) :: AFInfo
      real(ReKi),       intent(in   ) :: AOA
      real(ReKi),       intent(in   ) :: Re
      real(ReKi),       intent(  out) :: Cl
      real(ReKi),       intent(  out) :: Cd
      integer(IntKi),   intent(  out) :: ErrStat     ! Error status of the operation
      character(*),     intent(  out) :: ErrMsg      ! Error message if ErrStat /= ErrID_None
   
      real                            :: IntAFCoefs(4)                ! The interpolated airfoil coefficients.
      integer                         :: s1
      ErrStat = ErrID_None
      ErrMsg  = ''
   
      ! TODO: Extend this to use the UnsteadyAero module to determine the Cl, Cd, Cm info, as needed.  We may need to be tracking whether this call is
      ! part of an UpdateStates action or a CalcOutput action, because the calculation chain differs for the two.
      
      
      ! NOTE: we use Table(1) because the right now we can only interpolate with AOA and not Re or other variables.  If we had multiple tables stored
      ! for changes in other variables (Re, Mach #, etc) then then we would need to interpolate across tables.
      !
      s1 = size(AFInfo%Table(1)%Coefs,2)
   
      IntAFCoefs(1:s1) = CubicSplineInterpM( 1.0*real( AOA ) &
                                              , AFInfo%Table(1)%Alpha &
                                              , AFInfo%Table(1)%Coefs &
                                              , AFInfo%Table(1)%SplineCoefs &
                                              , ErrStat, ErrMsg )
   
      Cl = IntAFCoefs(1)
      Cd = IntAFCoefs(2)
   
   end subroutine BE_CalcOutputs
   
end module BladeElement