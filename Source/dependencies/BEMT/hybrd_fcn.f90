module hybrd_fcn
   
   use NWTC_Library
   use AirFoilInfo_Types
   use BEMTCoupled
   
   type, public :: hybrd_fcnArgs
 
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
      integer           :: AFindx
      type(AFInfoType)  :: AFInfo
      real(ReKi)        :: Vx
      real(ReKi)        :: Vy
      real(ReKi)        :: Vinf
      logical           :: useTanInd 
      logical           :: useAIDrag
      logical           :: useTIDrag
      logical           :: useHubLoss
      logical           :: useTipLoss 
      integer(IntKi)    :: SkewWakeMod
      integer(IntKi)    :: errStat       ! Error status of the operation
      character(4096)      :: errMsg        ! Error message if ErrStat /= ErrID_None
   end type hybrd_fcnArgs
   
   public :: fcn
   
   contains
   
subroutine fcn(n, x, fvec, iflag, fcnArgs)
   integer   ,           intent(in   )   :: n
   integer   ,           intent(  out)   :: iflag
   real(ReKi),           intent(in   )   ::  x(n)
   real(ReKi),           intent(  out)   :: fvec(n)
   type(hybrd_fcnArgs),  intent(inout)   :: fcnArgs
   
   
   integer(IntKi)      :: errStat       ! Error status of the operation
   character(4096)     :: errMsg        ! Error message if ErrStat /= ErrID_None
   iflag = 0
   
   ! Call the BEMTC_ElementalErrFn subroutine to compute the residual
   fvec = BEMTC_ElementalErrFn( x(1), x(2), fcnArgs%psi, fcnArgs%chi0, fcnArgs%airDens, fcnArgs%mu, fcnArgs%numBlades, fcnArgs%rlocal, fcnArgs%rtip, fcnArgs%chord, fcnArgs%theta, fcnArgs%rHub, fcnArgs%lambda, fcnArgs%AFInfo, &
                              fcnArgs%Vx, fcnArgs%Vy, fcnArgs%Vinf, fcnArgs%useTanInd, fcnArgs%useAIDrag, fcnArgs%useTIDrag, fcnArgs%useHubLoss, fcnArgs%useTipLoss,  fcnArgs%SkewWakeMod, &
                              errStat, errMsg)
   
   
   
   fcnArgs%errStat = errStat
   fcnArgs%errMsg  = errMsg
      
   if (ErrStat /= ErrID_None) then
      iflag = 1
      
   end if
   
end subroutine fcn

end module hybrd_fcn
   