!**********************************************************************************************************************************
! LICENSING
! Copyright (C) 2015  National Renewable Energy Laboratory
!
!    This file is part of AeroDyn.
!
! Licensed under the Apache License, Version 2.0 (the "License");
! you may not use this file except in compliance with the License.
! You may obtain a copy of the License at
!
!     http://www.apache.org/licenses/LICENSE-2.0
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.
!
!**********************************************************************************************************************************
! File last committed: $Date$
! (File) Revision #: $Rev$
! URL: $HeadURL$
!**********************************************************************************************************************************
module fminfcn
   
   use NWTC_Library
   use AirFoilInfo_Types
   !use UnsteadyAero_Types
   use BEMTUnCoupled, only: UncoupledErrFn
   
   type, public :: fmin_fcnArgs 
      real(ReKi)        :: airDens
      real(ReKi)        :: mu
      integer           :: numBlades
      real(ReKi)        :: rlocal      
      real(ReKi)        :: chord 
      real(ReKi)        :: theta         
      real(ReKi)        :: Vx
      real(ReKi)        :: Vy
      real(ReKi)        :: Re
      logical           :: useTanInd 
      logical           :: useAIDrag
      logical           :: useTIDrag
      logical           :: useHubLoss
      logical           :: useTipLoss 
      real(ReKi)        :: hubLossConst 
      real(ReKi)        :: tipLossConst
      logical           :: IsValidSolution
      integer(IntKi)    :: errStat       ! Error status of the operation
      character(ErrMsgLen)   :: errMsg        ! Error message if ErrStat /= ErrID_None
   end type fmin_fcnArgs
   
   public :: fmin_fcn
   
   contains
   
!real(ReKi) function fmin_fcn(x, iflag, fcnArgs)
!   real(ReKi),           intent(in   )   :: x
!   integer   ,           intent(  out)   :: iflag   
!   type(fmin_fcnArgs),  intent(inout)    :: fcnArgs
   
real(ReKi) function fmin_fcn(x, fcnArgs, AFInfo)
   real(ReKi),          intent(in   )    :: x
   type(fmin_fcnArgs),  intent(inout)    :: fcnArgs
   type(AFInfoType),    intent(in)       :: AFInfo
   
   integer(IntKi)       :: errStat       ! Error status of the operation
   character(ErrMsgLen) :: errMsg        ! Error message if ErrStat /= ErrID_None
   
   
      ! Call the UncoupledErrFn subroutine to compute the residual
   fmin_fcn = UncoupledErrFn( x,  fcnArgs%theta, fcnArgs%Re, fcnArgs%numBlades,  fcnArgs%rlocal, fcnArgs%chord, AFInfo, &
                              fcnArgs%Vx, fcnArgs%Vy, fcnArgs%useTanInd, fcnArgs%useAIDrag, fcnArgs%useTIDrag, fcnArgs%useHubLoss, fcnArgs%useTipLoss,  fcnArgs%hubLossConst, fcnArgs%tipLossConst,  &
                              fcnArgs%IsValidSolution, errStat, errMsg)  
   
  ! fmin_fcn = UncoupledErrFn( x,  AOA, fcnArgs%psi, fcnArgs%Re, 1, fcnArgs%airDens, fcnArgs%mu, fcnArgs%numBlades, fcnArgs%rlocal, fcnArgs%chord, fcnArgs%theta, AFInfo, &
  !                            fcnArgs%Vx, fcnArgs%Vy, fcnArgs%useTanInd, fcnArgs%useAIDrag, fcnArgs%useTIDrag, fcnArgs%useHubLoss, fcnArgs%useTipLoss,  fcnArgs%hubLossConst, fcnArgs%tipLossConst,  &
  !                            errStat, errMsg)  
   
   
   fcnArgs%errStat = errStat
   fcnArgs%errMsg  = errMsg

end function fmin_fcn

end module fminfcn
   