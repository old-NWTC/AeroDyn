module fminfcn
   
   use NWTC_Library
   use AirFoilInfo_Types
   use BEMTUnCoupled
   
   type, public :: fmin_fcnArgs 
      real(ReKi)        :: psi
      real(ReKi)        :: chi0
      real(ReKi)        :: airDens
      real(ReKi)        :: mu
      integer           :: numBlades
      real(ReKi)        :: rlocal   
      real(ReKi)        :: rtip   
      real(ReKi)        :: chord 
      real(ReKi)        :: theta         
      real(ReKi)        :: rHub
      real(ReKi)        :: lambda
      type(AFInfoType)  :: AFInfo
      real(ReKi)        :: Vx
      real(ReKi)        :: Vy
      logical           :: useTanInd 
      logical           :: useAIDrag
      logical           :: useTIDrag
      logical           :: useHubLoss
      logical           :: useTipLoss 
      integer(IntKi)    :: SkewWakeMod
      integer(IntKi)    :: errStat       ! Error status of the operation
      character(4096)   :: errMsg        ! Error message if ErrStat /= ErrID_None
   end type fmin_fcnArgs
   
   public :: fmin_fcn
   
   contains
   
!real(ReKi) function fmin_fcn(x, iflag, fcnArgs)
!   real(ReKi),           intent(in   )   :: x
!   integer   ,           intent(  out)   :: iflag   
!   type(fmin_fcnArgs),  intent(inout)    :: fcnArgs
real(ReKi) function fmin_fcn(x, fcnArgs)
   real(ReKi),           intent(in   )   :: x
   type(fmin_fcnArgs),  intent(inout)    :: fcnArgs
   
   integer(IntKi)      :: errStat       ! Error status of the operation
   character(4096)     :: errMsg        ! Error message if ErrStat /= ErrID_None
   !iflag = 0
   
   ! Call the UncoupledErrFn subroutine to compute the residual
   fmin_fcn = UncoupledErrFn( x,  fcnArgs%psi, fcnArgs%chi0, 1, fcnArgs%airDens, fcnArgs%mu, fcnArgs%numBlades, fcnArgs%rlocal, fcnArgs%rtip, fcnArgs%chord, fcnArgs%theta, fcnArgs%rHub, fcnArgs%lambda, fcnArgs%AFInfo, &
                              fcnArgs%Vx, fcnArgs%Vy, fcnArgs%useTanInd, fcnArgs%useAIDrag, fcnArgs%useTIDrag, fcnArgs%useHubLoss, fcnArgs%useTipLoss,  fcnArgs%SkewWakeMod, &
                              errStat, errMsg)  
   
   fcnArgs%errStat = errStat
   fcnArgs%errMsg  = errMsg

end function fmin_fcn

end module fminfcn
   