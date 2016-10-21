!**********************************************************************************************************************************
! LICENSING
! Copyright (C) 2015-2016  National Renewable Energy Laboratory
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
module DBEMT
   
   use NWTC_Library   
   use DBEMT_Types
   
   implicit none 

private


   public :: DBEMT_Init
   public :: DBEMT_UpdateStates
   public :: DBEMT_CalcOutput
   public :: DBEMT_TEST01
   public :: DBEMT_TEST02
   public :: DBEMT_TEST03
   public :: DBEMT_TEST04
   public :: DBEMT_TEST05
   public :: DBEMT_TEST06
   public :: DBEMT_TEST07
   public :: DBEMT_TEST08
   public :: DBEMT_TEST09
   public :: DBEMT_TEST10
   public :: DBEMT_TEST11
  ! public :: DBEMT_CalcOutput
  ! public :: DBEMT_End

   contains
   
   
subroutine DBEMT_ValidateInitInp(interval, InitInp, errStat, errMsg)
   real(DbKi),                      intent(inout) :: interval      !< Coupling interval in seconds: the rate that
   type(DBEMT_InitInputType),       intent(in   ) :: InitInp       !< Input data for initialization routine
   integer(IntKi),                  intent(  out) :: errStat       !< Error status of the operation
   character(*),                    intent(  out) :: errMsg        !< Error message if ErrStat /= ErrID_None
   
   integer(IntKi)                              :: errStat2      ! temporary error status of the operation
   character(ErrMsgLen)                        :: errMsg2       ! temporary error message 
   character(*), parameter                     :: RoutineName = 'DBEMT_ValidateInitInp'
   real(ReKi)                                  :: rlocalMax
   integer(IntKi)                              :: i,j
     ! Initialize variables for this routine

   errStat2 = ErrID_None
   errMsg2  = ""
   
   if ( InitInp%DBEMT_Mod  == 0 ) return  ! DBEMT is turned off
   
   if ( interval < sqrt(epsilon(1.0_ReKi)) ) call SetErrStat( ErrID_Fatal, " The timestep size for DBEMT (interval) must be larger than sqrt(epsilon).  ", ErrStat, ErrMsg, RoutineName)
   if ( (InitInp%DBEMT_Mod .ne. 1) .and. (InitInp%DBEMT_Mod .ne. 2) ) call SetErrStat( ErrID_Fatal, " DBEMT_Mod must be set to 0, 1, or 2.  ", ErrStat, ErrMsg, RoutineName)
   
   if (InitInp%numBlades < 1) call SetErrStat( ErrID_Fatal, " InitInp%numBlades must set to 1 or more.  ", ErrStat, ErrMsg, RoutineName)
   if (InitInp%numNodes < 2) call SetErrStat( ErrID_Fatal, " InitInp%numNodes must set to 2 or more.  ", ErrStat, ErrMsg, RoutineName)
  
   if ( (InitInp%DBEMT_Mod == 1) )then
   
      if (InitInp%tau1_const < 0.0_ReKi)  call SetErrStat( ErrID_Fatal, " InitInp%tau1_const must be greater than zero.  ", ErrStat, ErrMsg, RoutineName)
        ! Default = 0.33_ReKi
   
      if (.not. allocated(InitInp%rlocal) ) then
         call SetErrStat( ErrID_Fatal, " InitInput%rlocal must be allocated to size (InitInp%numNodes,InitInp%numBlades).  ", ErrStat, ErrMsg, RoutineName)
         return
      end if
   
      do j = 1,InitInp%numBlades
         rlocalMax = 0.0_ReKi
         do i= 1,InitInp%numNodes
            if (InitInp%rlocal(i,j) < 0.0_ReKi ) then
               call SetErrStat( ErrID_Fatal, " Blades nodes must be located at a positive radial distance (rlocal) greater than zero.  ", ErrStat, ErrMsg, RoutineName)
               return
            end if
            
            rlocalMax = max(rlocalMax,InitInp%rlocal(i,j))
         end do
         if ( EqualRealNos(rlocalMax, 0.0_ReKi) ) call SetErrStat( ErrID_Fatal, " Blades must have nodes located at a radial distance (rlocal) greater than zero.  ", ErrStat, ErrMsg, RoutineName)
      end do
      
   end if

end subroutine DBEMT_ValidateInitInp


!----------------------------------------------------------------------------------------------------------------------------------   
!> This routine is called at the start of the simulation to perform initialization steps.
!! The parameters are set here and not changed during the simulation.
!! The initial states and initial guess for the input are defined.
subroutine DBEMT_Init( InitInp, u, p, x, m, Interval, InitOut, ErrStat, ErrMsg )
!..................................................................................................................................

   type(DBEMT_InitInputType),       intent(in   ) :: InitInp       !< Input data for initialization routine
   type(DBEMT_InputType),           intent(  out) :: u             !< An initial guess for the input; input mesh must be defined
   type(DBEMT_ParameterType),       intent(  out) :: p             !< Parameters
   type(DBEMT_ContinuousStateType), intent(  out) :: x             !< Initial continuous states
   type(DBEMT_MiscVarType),         intent(  out) :: m             !< Initial misc/optimization variables
   real(DbKi),                      intent(inout) :: interval      !< Coupling interval in seconds: the rate that
                                                                   !!   (1) DBEMT_UpdateStates() is called in loose coupling &
                                                                   !!   (2) DBEMT_UpdateDiscState() is called in tight coupling.
                                                                   !!   Input is the suggested time from the glue code;
                                                                   !!   Output is the actual coupling interval that will be used
                                                                   !!   by the glue code.
   type(DBEMT_InitOutputType),      intent(  out) :: InitOut       !< Output for initialization routine
   integer(IntKi),                  intent(  out) :: errStat       !< Error status of the operation
   character(*),                    intent(  out) :: errMsg        !< Error message if ErrStat /= ErrID_None
   
   
     ! Local variables
   integer(IntKi)                              :: i,j             ! loop counter
   real(ReKi)                                  :: rTip          ! Maximum tip radius for all blades 
   integer(IntKi)                              :: errStat2      ! temporary error status of the operation
   character(ErrMsgLen)                        :: errMsg2       ! temporary error message 
   character(*), parameter                     :: RoutineName = 'DBEMT_Init'
   
   
   
      ! Initialize variables for this routine

   errStat2 = ErrID_None
   errMsg2  = ""
   
   
   
      ! Validate the Initialization inputs
   call DBEMT_ValidateInitInp(interval, InitInp, errStat2, errMsg2)
      call SetErrStat( errStat2, errMsg2, ErrStat, ErrMsg, RoutineName)
      if (errStat >= AbortErrLev) return
      
      ! Set parameter data using the initialization inputs
   
   p%DBEMT_Mod  = InitInp%DBEMT_Mod
   
   if (p%DBEMT_Mod == 0) return  ! DBEMT is turned off.
   
   p%dt = interval
   p%numBlades  = InitInp%numBlades
   p%numNodes   = InitInp%numNodes
   p%k_0ye      = 0.6_ReKi
   p%c5         = 1.1_ReKi
   p%c6         = 1.0_ReKi
   p%c7         = 1.3_ReKi
   p%c8         = 0.39_ReKi
   p%c9         = 0.26_ReKi
   p%tau1_const = InitInp%tau1_const  ! Default = 0.33_ReKi
   
   if (p%DBEMT_Mod == 1) then
      ! DBEMT_Mod = 1 uses constant tau1 and tau2
      allocate( p%spanRatio(p%numNodes, p%numBlades), STAT=ErrStat2)
      if (ErrStat2 /= 0 ) then
         call SetErrStat( ErrID_Fatal, " Error allocating p%spanRatio.  ", ErrStat, ErrMsg, RoutineName)
         return
      end if
         ! compute the largest tip radius of all blades
      rTip = 0.0_ReKi
      do j = 1,p%numBlades
         do i= 1,p%numNodes
            rTip = max(rTip, InitInp%rlocal(i,j))
         end do
      end do
      
      do j = 1,p%numBlades
         do i= 1,p%numNodes
            p%spanRatio(i,j) = InitInp%rlocal(i,j)/rTip  ! normalized radial distance from center of rotation to node
         end do
      end do
   end if
   
      ! Initialize the continuous states
   allocate( x%vind(2,p%numNodes, p%numBlades), STAT=ErrStat2)
      if (ErrStat2 /= 0 ) then
         call SetErrStat( ErrID_Fatal, " Error allocating x%vind.  ", ErrStat, ErrMsg, RoutineName)
         return
      end if
   x%vind = 0.0 ! This is the axial and tangential induced velocity at node i on blade j

   allocate( x%vind_1(2,p%numNodes, p%numBlades), STAT=ErrStat2)
      if (ErrStat2 /= 0 ) then
         call SetErrStat( ErrID_Fatal, " Error allocating x%vind_1.  ", ErrStat, ErrMsg, RoutineName)
         return
      end if
   x%vind_1 = 0.0 ! This is the axial and tangential induced velocity at node i on blade j
   
   allocate( m%areStatesInitialized(p%numNodes, p%numBlades), STAT=ErrStat2)
      if (ErrStat2 /= 0 ) then
         call SetErrStat( ErrID_Fatal, " Error allocating m%areStatesInitialized.  ", ErrStat, ErrMsg, RoutineName)
         return
      end if
   m%areStatesInitialized = .false.
      
end subroutine DBEMT_Init


!!----------------------------------------------------------------------------------------------------------------------------------
!> Loose coupling routine for solving for constraint states, integrating continuous states, and updating discrete and other states.
!! Continuous, constraint, discrete, and other states are updated for t + Interval
subroutine DBEMT_UpdateStates( i, j, t, u,  p, x, m, errStat, errMsg )
!..................................................................................................................................
   integer(IntKi),                  intent(in   ) :: i          !< blade node counter
   integer(IntKi),                  intent(in   ) :: j          !< blade counter
   real(DbKi),                      intent(in   ) :: t          !< Current simulation time in seconds
   type(DBEMT_InputType),           intent(inout) :: u(2)       !< Inputs at utimes (out only for mesh record-keeping in ExtrapInterp routine)
   type(DBEMT_ParameterType),       intent(in   ) :: p          !< Parameters
   type(DBEMT_ContinuousStateType), intent(inout) :: x          !< Input: Continuous states at t;
                                                                !!   Output: Continuous states at t + Interval
   type(DBEMT_MiscVarType),         intent(inout) :: m          !< Initial misc/optimization variables
   integer(IntKi),                  intent(  out) :: errStat    !< Error status of the operation
   character(*),                    intent(  out) :: errMsg     !< Error message if ErrStat /= ErrID_None
  
   ! local variables
   type(DBEMT_InputType)                           :: uInterp     ! Interpolated/Extrapolated input
   real(ReKi)                                   :: spanRatio       ! local version of r / R
   real(ReKi)                                   :: temp, tau1, tau2 , A, B, C0, k_tau, C0_2 ! tau1_plus1, C_tau1, C, K1
   integer(IntKi)                               :: indx
   integer(IntKi)                               :: ErrStat2          ! temporary Error status
   character(ErrMsgLen)                         :: ErrMsg2           ! temporary Error message
   character(*), parameter                      :: RoutineName = 'DBEMT_UpdateStates'
      
   ErrStat = ErrID_None
   ErrMsg  = ""
   
   if (p%DBEMT_Mod == 0) return  ! DBEMT is turned off.
   
   if ( .not. m%areStatesInitialized(i,j) ) then
      x%vind_1(1,i,j) = u(1)%vind_s(1)
      x%vind_1(2,i,j) = u(1)%vind_s(2)
      x%vind(1,i,j) = u(1)%vind_s(1)
      x%vind(2,i,j) = u(1)%vind_s(2)
      m%areStatesInitialized(i,j) = .true.
      return
   end if
   
   if ( p%DBEMT_Mod == 1 ) then
      tau1       = p%tau1_const
    !  tau1_plus1 = tau1
      spanRatio   = p%spanRatio(i,j)     
   else
      
   ! We need to extrapolate the radius and disk velocity to the i+1 timestep
   ! We will grab the i+1 version of vind,s and the disk averaged induction by using the
   ! the already updated states of the BEMT module.
   !
   
   
      temp   = (1-1.3*u(1)%AxInd_disk)*u(1)%Un_disk
         ! Check if temp is zero, because that will cause the tau1 equation to blow up.
      tau1   = 1.1*u(1)%R_disk/temp
      
      spanRatio = u(1)%spanRatio
   end if
   
   
   do indx=1,2
      
      
      ! TODO: Deal with initialization so that we avoid spikes???
      
      B =  ( u(2)%vind_s(indx) - u(1)%vind_s(indx) ) / p%dt
      A = u(1)%vind_s(indx) + B*p%k_0ye*tau1
      
      C0 = x%vind_1(indx,i,j) - A + B*tau1
      
      x%vind_1(indx,i,j) = C0*exp(-p%dt/tau1) + A + B*(p%dt-tau1)
      
      k_tau = 0.39 - 0.26*spanRatio**2
      tau2 = k_tau*tau1
      C0_2 = x%vind(indx,i,j) - C0/(1-k_tau) - A + B*(tau1 + tau2)
      x%vind(indx,i,j) = C0_2*exp(-p%dt/tau2) + A + B*(p%dt-tau1-tau2) + (C0/(1-k_tau))*exp(-p%dt/tau1)
      
      
      !C      = (u(2)%vind_s(indx) - u(1)%vind_s(indx))/p%dt ! v_ind_s_future could come from BEMT update states, but this seems to violate the framework
      !A      = u(1)%vind_s(indx) + C*p%k_0ye*tau1
      !B      = C*(p%k_0ye*C_tau1+1)
      !K1     = (A + A*C_tau1 - B*tau1) / (1+C_tau1)
      !C0     = ( x%vind_1(indx,i,j) - K1 )*tau1**(1.0/C_tau1)
      !k_tau  = 0.39 - 0.26*(spanRatio)**2
      !C0_2   = ( x%vind(indx,i,j) - K1 - C0 / ((1-k_tau)**(1.0/C_tau1)) + B*k_tau*tau1/((1+C_tau1)*(1+k_tau*C_tau1)) ) * tau1**(1.0/(k_tau*C_tau1))
      !x%vind(indx,i,j) = K1 + B*(p%dt-k_tau*tau1)/((1+C_tau1)*(1+k_tau*C_tau1)) + C0 / ((1-k_tau)*(tau1+C_tau1*p%dt)**(1.0/C_tau1)) + C0_2 / ((tau1 + C_tau1*p%dt)**(1.0/(k_tau*C_tau1)))
   end do
end subroutine DBEMT_UpdateStates

!----------------------------------------------------------------------------------------------------------------------------------
!> Routine for computing outputs, used in both loose and tight coupling.
!! This subroutine is used to compute the output channels (motions and loads) and place them in the WriteOutput() array.
!! The descriptions of the output channels are not given here. Please see the included OutListParameters.xlsx sheet for
!! for a complete description of each output parameter.
subroutine DBEMT_CalcOutput( i, j, t, u, y_vind, p, x, m, errStat, errMsg )
! NOTE: no matter how many channels are selected for output, all of the outputs are calcalated
! All of the calculated output channels are placed into the m%AllOuts(:), while the channels selected for outputs are
! placed in the y%WriteOutput(:) array.
!..................................................................................................................................
   integer(IntKi),                  intent(in   ) :: i          !< blade node counter
   integer(IntKi),                  intent(in   ) :: j          !< blade counter
   real(DbKi),                      intent(in   ) :: t          !< Current simulation time in seconds
   type(DBEMT_InputType),          intent(in   ) :: u          !< Inputs at t 
   real(ReKi),                      intent(  out) :: y_vind(2)
   !type(DBEMT_OutputType),          intent(inout) :: y          !< Inputs at utimes (out only for mesh record-keeping in ExtrapInterp routine)
   type(DBEMT_ParameterType),       intent(in   ) :: p          !< Parameters
   type(DBEMT_ContinuousStateType), intent(in   ) :: x          !< Input: Continuous states at t;
                                                                !!   Output: Continuous states at t + Interval
   type(DBEMT_MiscVarType),         intent(inout) :: m          !< Initial misc/optimization variables

   integer(IntKi),                  intent(  out) :: errStat    !< Error status of the operation
   character(*),                    intent(  out) :: errMsg     !< Error message if ErrStat /= ErrID_None
  
   ! local variables
   
   real(ReKi)                                   :: spanRatio       ! local version of r / R
   real(ReKi)                                   :: temp, tau1, tau1_plus1, C_tau1, C, A, B, K1, C0, k_tau, C0_2
   integer(IntKi)                               :: indx
   integer(IntKi)                               :: ErrStat2          ! temporary Error status
   character(ErrMsgLen)                         :: ErrMsg2           ! temporary Error message
   character(*), parameter                      :: RoutineName = 'DBEMT_CalcOutput'
      
   ErrStat = ErrID_None
   ErrMsg  = ""
   
   if ( .not. m%areStatesInitialized(i,j) ) then
      y_vind = u%vind_s
   else     
      y_vind(:) = x%vind(:,i,j)
   end if
   
      
end subroutine DBEMT_CalcOutput

!----------------------------------------------------------------------------------------------------------------------------------
!> This routine is called at the end of the simulation.
subroutine DBEMT_End( u, p, x, m, ErrStat, ErrMsg )
!..................................................................................................................................

      TYPE(DBEMT_InputType),           INTENT(INOUT)  :: u(2)           !< System inputs
      TYPE(DBEMT_ParameterType),       INTENT(INOUT)  :: p           !< Parameters
      TYPE(DBEMT_ContinuousStateType), INTENT(INOUT)  :: x           !< Continuous states
      type(DBEMT_MiscVarType),         intent(inout)  :: m          !< Initial misc/optimization variables
      INTEGER(IntKi),               INTENT(  OUT)  :: ErrStat     !< Error status of the operation
      CHARACTER(*),                 INTENT(  OUT)  :: ErrMsg      !< Error message if ErrStat /= ErrID_None



         ! Initialize ErrStat

      ErrStat = ErrID_None
      ErrMsg  = ""


         ! Place any last minute operations or calculations here:


         ! Close files here:



         ! Destroy the input data:

      CALL DBEMT_DestroyInput( u(1), ErrStat, ErrMsg )
      CALL DBEMT_DestroyInput( u(2), ErrStat, ErrMsg )


         ! Destroy the parameter data:

      CALL DBEMT_DestroyParam( p, ErrStat, ErrMsg )


         ! Destroy the state data:

      CALL DBEMT_DestroyContState(   x,           ErrStat, ErrMsg )


      CALL DBEMT_DestroyMisc(   m,           ErrStat, ErrMsg )


END SUBROUTINE DBEMT_End


logical function DBEMT_TEST01(errStat, errMsg)

   
   integer(IntKi), INTENT(out)                  :: errStat       !< Error status of the operation
   character(ErrMsgLen), INTENT(out)                    :: errMsg        !< Error message if ErrStat /= ErrID_None
   type(DBEMT_InitInputType)       :: InitInp       !< Input data for initialization routine
   type(DBEMT_InputType)           :: u             !< An initial guess for the input; input mesh must be defined
   type(DBEMT_ParameterType)       :: p             !< Parameters
   type(DBEMT_ContinuousStateType) :: x             !< Initial continuous states
   type(DBEMT_MiscVarType)         :: m             !< Initial misc/optimization variables
   real(DbKi)                      :: interval      !< Coupling interval in seconds: the rate that
                                                     !!   (1) DBEMT_UpdateStates() is called in loose coupling &
                                                     !!   (2) DBEMT_UpdateDiscState() is called in tight coupling.
                                                     !!   Input is the suggested time from the glue code;
                                                     !!   Output is the actual coupling interval that will be used
                                                     !!   by the glue code.
   type(DBEMT_InitOutputType)      :: InitOut       !< Output for initialization routine
   
   
   
   DBEMT_TEST01 = .false.
   
   ! This test will evaluate the initialization of the module 
   
   errStat = ErrID_None
   errMsg  = ""

   InitInp%DBEMT_Mod = -1
   
   call DBEMT_Init(InitInp, u, p, x, m, Interval, InitOut, errStat, errMsg)

   if (errStat == AbortErrLev) DBEMT_TEST01 = .true.

end function DBEMT_TEST01

logical function DBEMT_TEST02(errStat, errMsg)

   
   integer(IntKi), INTENT(out)                  :: errStat       !< Error status of the operation
   character(ErrMsgLen), INTENT(out)                    :: errMsg        !< Error message if ErrStat /= ErrID_None
   type(DBEMT_InitInputType)       :: InitInp       !< Input data for initialization routine
   type(DBEMT_InputType)           :: u(2)             !< An initial guess for the input; input mesh must be defined
   type(DBEMT_ParameterType)       :: p             !< Parameters
   type(DBEMT_ContinuousStateType) :: x             !< Initial continuous states
   type(DBEMT_MiscVarType)         :: m             !< Initial misc/optimization variables
   real(DbKi)                      :: interval      !< Coupling interval in seconds: the rate that
                                                     !!   (1) DBEMT_UpdateStates() is called in loose coupling &
                                                     !!   (2) DBEMT_UpdateDiscState() is called in tight coupling.
                                                     !!   Input is the suggested time from the glue code;
                                                     !!   Output is the actual coupling interval that will be used
                                                     !!   by the glue code.
   type(DBEMT_InitOutputType)      :: InitOut       !< Output for initialization routine
   
   integer(IntKi)                              :: errStat2      ! temporary error status of the operation
   character(ErrMsgLen)                        :: errMsg2       ! temporary error message 
   character(*), parameter                     :: RoutineName = 'DBEMT_TEST02'
   
     ! Initialize variables for this routine

   errStat2 = ErrID_None
   errMsg2  = ""

   
   DBEMT_TEST02 = .false.
   
   ! This test will evaluate the initialization of the module 
   
   errStat = ErrID_None
   errMsg  = ""

   InitInp%DBEMT_Mod = 0
   call DBEMT_Init(InitInp, u(1), p, x, m, Interval, InitOut, errStat2, errMsg2)
      call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName)
      
   call DBEMT_UpdateStates(1, 1, interval, u,  p, x, m, errStat2, errMsg2 )  
      call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName)
      
      ! Since DBEMT is turned off, their should be no errors, even though other initialization data is missing
   if (errStat == ErrID_None) DBEMT_TEST02 = .true.

end function DBEMT_TEST02

logical function DBEMT_TEST03(errStat, errMsg)

   
   integer(IntKi), INTENT(out)                  :: errStat       !< Error status of the operation
   character(ErrMsgLen), INTENT(out)                    :: errMsg        !< Error message if ErrStat /= ErrID_None
   type(DBEMT_InitInputType)       :: InitInp       !< Input data for initialization routine
   type(DBEMT_InputType)           :: u             !< An initial guess for the input; input mesh must be defined
   type(DBEMT_ParameterType)       :: p             !< Parameters
   type(DBEMT_ContinuousStateType) :: x             !< Initial continuous states
   type(DBEMT_MiscVarType)         :: m             !< Initial misc/optimization variables
   real(DbKi)                      :: interval      !< Coupling interval in seconds: the rate that
                                                     !!   (1) DBEMT_UpdateStates() is called in loose coupling &
                                                     !!   (2) DBEMT_UpdateDiscState() is called in tight coupling.
                                                     !!   Input is the suggested time from the glue code;
                                                     !!   Output is the actual coupling interval that will be used
                                                     !!   by the glue code.
   type(DBEMT_InitOutputType)      :: InitOut       !< Output for initialization routine
   
   
   
   DBEMT_TEST03 = .false.
   
   ! This test will evaluate the initialization of the module 
   
   errStat = ErrID_None
   errMsg  = ""
   
   InitInp%DBEMT_Mod = 1
   
      ! The InitInp%numBlades has not been set.  This should produce a fatal error.
      ! The InitInp%numNodes has not been set.  This should produce a fatal error.
      ! The InitInp%rlocal array has not been allocated.  This should produce a fatal error.

   call DBEMT_Init(InitInp, u, p, x, m, Interval, InitOut, errStat, errMsg)

   if (errStat == AbortErrLev) DBEMT_TEST03 = .true.

end function DBEMT_TEST03

logical function DBEMT_TEST04(errStat, errMsg)

   
   integer(IntKi), INTENT(out)                  :: errStat       !< Error status of the operation
   character(ErrMsgLen), INTENT(out)                    :: errMsg        !< Error message if ErrStat /= ErrID_None
   type(DBEMT_InitInputType)       :: InitInp       !< Input data for initialization routine
   type(DBEMT_InputType)           :: u             !< An initial guess for the input; input mesh must be defined
   type(DBEMT_ParameterType)       :: p             !< Parameters
   type(DBEMT_ContinuousStateType) :: x             !< Initial continuous states
   type(DBEMT_MiscVarType)         :: m             !< Initial misc/optimization variables
   real(DbKi)                      :: interval      !< Coupling interval in seconds: the rate that
                                                     !!   (1) DBEMT_UpdateStates() is called in loose coupling &
                                                     !!   (2) DBEMT_UpdateDiscState() is called in tight coupling.
                                                     !!   Input is the suggested time from the glue code;
                                                     !!   Output is the actual coupling interval that will be used
                                                     !!   by the glue code.
   type(DBEMT_InitOutputType)      :: InitOut       !< Output for initialization routine
   
   
   
   DBEMT_TEST04 = .false.
   
   ! This test will evaluate the initialization of the module 
   
   errStat = ErrID_None
   errMsg  = ""

   InitInp%DBEMT_Mod = 1
   InitInp%numBlades = 1
   call DBEMT_Init(InitInp, u, p, x, m, Interval, InitOut, errStat, errMsg)

      ! The InitInp%numNodes has not been set.  This should produce a fatal error.
      ! The InitInp%rlocal array has not been allocated.  This should produce a fatal error.
   if (errStat == AbortErrLev) DBEMT_TEST04 = .true.

end function DBEMT_TEST04

logical function DBEMT_TEST05(errStat, errMsg)

   
   integer(IntKi), INTENT(out)                  :: errStat       !< Error status of the operation
   character(ErrMsgLen), INTENT(out)                    :: errMsg        !< Error message if ErrStat /= ErrID_None
   type(DBEMT_InitInputType)       :: InitInp       !< Input data for initialization routine
   type(DBEMT_InputType)           :: u             !< An initial guess for the input; input mesh must be defined
   type(DBEMT_ParameterType)       :: p             !< Parameters
   type(DBEMT_ContinuousStateType) :: x             !< Initial continuous states
   type(DBEMT_MiscVarType)         :: m             !< Initial misc/optimization variables
   real(DbKi)                      :: interval      !< Coupling interval in seconds: the rate that
                                                     !!   (1) DBEMT_UpdateStates() is called in loose coupling &
                                                     !!   (2) DBEMT_UpdateDiscState() is called in tight coupling.
                                                     !!   Input is the suggested time from the glue code;
                                                     !!   Output is the actual coupling interval that will be used
                                                     !!   by the glue code.
   type(DBEMT_InitOutputType)      :: InitOut       !< Output for initialization routine
   
   
   
   DBEMT_TEST05 = .false.
   
   ! This test will evaluate the initialization of the module 
   
   errStat = ErrID_None
   errMsg  = ""

   InitInp%DBEMT_Mod = 1
   InitInp%numBlades = 1
   InitInp%numNodes  = 3
   call DBEMT_Init(InitInp, u, p, x, m, Interval, InitOut, errStat, errMsg)

      ! The InitInp%rlocal array has not been allocated.  This should produce a fatal error.
   if (errStat == AbortErrLev) DBEMT_TEST05 = .true.

end function DBEMT_TEST05

logical function DBEMT_TEST06(errStat, errMsg)

   
   integer(IntKi), INTENT(out)                  :: errStat       !< Error status of the operation
   character(ErrMsgLen), INTENT(out)                    :: errMsg        !< Error message if ErrStat /= ErrID_None
   type(DBEMT_InitInputType)       :: InitInp       !< Input data for initialization routine
   type(DBEMT_InputType)           :: u             !< An initial guess for the input; input mesh must be defined
   type(DBEMT_ParameterType)       :: p             !< Parameters
   type(DBEMT_ContinuousStateType) :: x             !< Initial continuous states
   type(DBEMT_MiscVarType)         :: m             !< Initial misc/optimization variables
   real(DbKi)                      :: interval      !< Coupling interval in seconds: the rate that
                                                     !!   (1) DBEMT_UpdateStates() is called in loose coupling &
                                                     !!   (2) DBEMT_UpdateDiscState() is called in tight coupling.
                                                     !!   Input is the suggested time from the glue code;
                                                     !!   Output is the actual coupling interval that will be used
                                                     !!   by the glue code.
   type(DBEMT_InitOutputType)      :: InitOut       !< Output for initialization routine
   
   
   
   DBEMT_TEST06 = .false.
   
   ! This test will evaluate the initialization of the module 
   
   errStat = ErrID_None
   errMsg  = ""

   InitInp%DBEMT_Mod = 1
   InitInp%numBlades = 1
   InitInp%numNodes  = 2
   allocate(InitInp%rlocal(InitInp%numNodes,InitInp%numBlades))
   
   ! The rlocal values have been allocated, but not initialized.  This should produce a fatal error, but the values 
   ! that are set are indeterminate, so we will never know if an error is triggered or not.
   
   call DBEMT_Init(InitInp, u, p, x, m, Interval, InitOut, errStat, errMsg)

   if (errStat == AbortErrLev) DBEMT_TEST06 = .true.

end function DBEMT_TEST06

logical function DBEMT_TEST07(errStat, errMsg)

   
   integer(IntKi), INTENT(out)                  :: errStat       !< Error status of the operation
   character(ErrMsgLen), INTENT(out)                    :: errMsg        !< Error message if ErrStat /= ErrID_None
   type(DBEMT_InitInputType)       :: InitInp       !< Input data for initialization routine
   type(DBEMT_InputType)           :: u             !< An initial guess for the input; input mesh must be defined
   type(DBEMT_ParameterType)       :: p             !< Parameters
   type(DBEMT_ContinuousStateType) :: x             !< Initial continuous states
   type(DBEMT_MiscVarType)         :: m             !< Initial misc/optimization variables
   real(DbKi)                      :: interval      !< Coupling interval in seconds: the rate that
                                                     !!   (1) DBEMT_UpdateStates() is called in loose coupling &
                                                     !!   (2) DBEMT_UpdateDiscState() is called in tight coupling.
                                                     !!   Input is the suggested time from the glue code;
                                                     !!   Output is the actual coupling interval that will be used
                                                     !!   by the glue code.
   type(DBEMT_InitOutputType)      :: InitOut       !< Output for initialization routine
   
   integer(IntKi)                  :: i,j
   
   DBEMT_TEST07 = .false.
   
   ! This test will evaluate the initialization of the module 
   
   errStat = ErrID_None
   errMsg  = ""

   InitInp%DBEMT_Mod = 1
   InitInp%numBlades = 1
   InitInp%numNodes  = 2
   allocate(InitInp%rlocal(InitInp%numNodes,InitInp%numBlades))
   do j=1,InitInp%numBlades
      do i=1,InitInp%numNodes
         InitInp%rlocal(i,j) = real(i,ReKi)
      end do
   end do
   
   call DBEMT_Init(InitInp, u, p, x, m, Interval, InitOut, errStat, errMsg)

   if (errStat == AbortErrLev) DBEMT_TEST07 = .true.

end function DBEMT_TEST07

logical function DBEMT_TEST08(errStat, errMsg)

   
   integer(IntKi), INTENT(out)                  :: errStat       !< Error status of the operation
   character(ErrMsgLen), INTENT(out)                    :: errMsg        !< Error message if ErrStat /= ErrID_None
   type(DBEMT_InitInputType)       :: InitInp       !< Input data for initialization routine
   type(DBEMT_InputType)           :: u(2)             !< An initial guess for the input; input mesh must be defined
   type(DBEMT_ParameterType)       :: p             !< Parameters
   type(DBEMT_ContinuousStateType) :: x             !< Initial continuous states
   type(DBEMT_MiscVarType)         :: m             !< Initial misc/optimization variables
   real(DbKi)                      :: interval      !< Coupling interval in seconds: the rate that
                                                     !!   (1) DBEMT_UpdateStates() is called in loose coupling &
                                                     !!   (2) DBEMT_UpdateDiscState() is called in tight coupling.
                                                     !!   Input is the suggested time from the glue code;
                                                     !!   Output is the actual coupling interval that will be used
                                                     !!   by the glue code.
   type(DBEMT_InitOutputType)      :: InitOut       !< Output for initialization routine
   
   integer(IntKi)                  :: i,j
   integer(IntKi)                              :: errStat2      ! temporary error status of the operation
   character(ErrMsgLen)                        :: errMsg2       ! temporary error message 
   character(*), parameter                     :: RoutineName = 'DBEMT_TEST08'
   
     ! Initialize variables for this routine

   errStat2 = ErrID_None
   errMsg2  = ""
   DBEMT_TEST08 = .false.
   
   ! This test will evaluate the initialization of the module 
   
   errStat = ErrID_None
   errMsg  = ""

   InitInp%DBEMT_Mod = 1
   InitInp%numBlades = 1
   InitInp%numNodes  = 2
   allocate(InitInp%rlocal(InitInp%numNodes,InitInp%numBlades))
   do j=1,InitInp%numBlades
      do i=1,InitInp%numNodes
         InitInp%rlocal(i,j) = real(i,ReKi)
      end do
   end do
   Interval = 0.1_ReKi
   InitInp%tau1_const = 0.0_ReKi
   
   call DBEMT_Init(InitInp, u(1), p, x, m, Interval, InitOut, errStat2, errMsg2)
      call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName)
     
   u(1)%vind_s(1) = 10.0_ReKi
   u(1)%vind_s(2) =  0.0_ReKi
   
   u(2)%vind_s(1) = 12.0_ReKi
   u(2)%vind_s(2) =  0.0_ReKi
   
   call DBEMT_UpdateStates(1, 1, 0.0_DbKi, u,  p, x, m, errStat2, errMsg2 )  
      call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName)
   
   if ( (errStat == ErrID_None) .and. (EqualRealNos(u(1)%vind_s(1),x%vind(1,1,1)) ) &
         .and. (EqualRealNos(u(1)%vind_s(1),x%vind_1(1,1,1)) )                       &
         .and. (EqualRealNos(u(1)%vind_s(2),x%vind(2,1,1)) )                         &
         .and. (EqualRealNos(u(1)%vind_s(2),x%vind_1(2,1,1)) )) then
      DBEMT_TEST08 = .true.
   else
      DBEMT_TEST08 = .false.
   end if
   
   call DBEMT_End(u, p, x, m, ErrStat, ErrMsg )
   
   InitInp%tau1_const = 1.0_ReKi
   call DBEMT_Init(InitInp, u(1), p, x, m, Interval, InitOut, errStat2, errMsg2)
      call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName)
      
   u(1)%vind_s(1) = 10.0_ReKi
   u(1)%vind_s(2) =  0.0_ReKi
   
   u(2)%vind_s(1) = 12.0_ReKi
   u(2)%vind_s(2) =  0.0_ReKi
   
   call DBEMT_UpdateStates(1, 1, 0.0_DbKi, u,  p, x, m, errStat2, errMsg2 )  
      call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName)
   
   if ( (errStat == ErrID_None) .and. (EqualRealNos(u(1)%vind_s(1),x%vind(1,1,1)) ) &
         .and. (EqualRealNos(u(1)%vind_s(1),x%vind_1(1,1,1)) )                       &
         .and. (EqualRealNos(u(1)%vind_s(2),x%vind(2,1,1)) )                         &
         .and. (EqualRealNos(u(1)%vind_s(2),x%vind_1(2,1,1)) )) then
      DBEMT_TEST08 = (.true. .and. DBEMT_TEST08)
   else
      DBEMT_TEST08 = .false.
      
   end if
   
   call DBEMT_End(u, p, x, m, ErrStat, ErrMsg )
   
end function DBEMT_TEST08

logical function DBEMT_TEST09(errStat, errMsg)

   
   integer(IntKi), INTENT(out)                  :: errStat       !< Error status of the operation
   character(ErrMsgLen), INTENT(out)                    :: errMsg        !< Error message if ErrStat /= ErrID_None
   type(DBEMT_InitInputType)       :: InitInp       !< Input data for initialization routine
   type(DBEMT_InputType)           :: u(2)             !< An initial guess for the input; input mesh must be defined
   type(DBEMT_ParameterType)       :: p             !< Parameters
   type(DBEMT_ContinuousStateType) :: x             !< Initial continuous states
   type(DBEMT_MiscVarType)         :: m             !< Initial misc/optimization variables
   real(DbKi)                      :: interval      !< Coupling interval in seconds: the rate that
                                                     !!   (1) DBEMT_UpdateStates() is called in loose coupling &
                                                     !!   (2) DBEMT_UpdateDiscState() is called in tight coupling.
                                                     !!   Input is the suggested time from the glue code;
                                                     !!   Output is the actual coupling interval that will be used
                                                     !!   by the glue code.
   type(DBEMT_InitOutputType)      :: InitOut       !< Output for initialization routine
   
   integer(IntKi)                  :: i,j,n
   real(DbKi)                      :: t
   integer(IntKi)                              :: errStat2      ! temporary error status of the operation
   character(ErrMsgLen)                        :: errMsg2       ! temporary error message 
   character(*), parameter                     :: RoutineName = 'DBEMT_TEST09'
   
     ! Initialize variables for this routine

   errStat2 = ErrID_None
   errMsg2  = ""
   DBEMT_TEST09 = .true.
   
   ! This test will evaluate the initialization of the module 
   
   errStat = ErrID_None
   errMsg  = ""

   InitInp%DBEMT_Mod = 1
   InitInp%numBlades = 1
   InitInp%numNodes  = 2
   allocate(InitInp%rlocal(InitInp%numNodes,InitInp%numBlades))
   do j=1,InitInp%numBlades
      do i=1,InitInp%numNodes
         InitInp%rlocal(i,j) = real(i,ReKi)
      end do
   end do
   Interval = 0.1_ReKi
   InitInp%tau1_const = 0.0_ReKi
   
   call DBEMT_Init(InitInp, u(1), p, x, m, Interval, InitOut, errStat2, errMsg2)
      call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName)
     
   u(1)%vind_s(1) = 10.0_ReKi
   u(1)%vind_s(2) =  0.0_ReKi
   
   u(2)%vind_s(1) = 10.0_ReKi
   u(2)%vind_s(2) =  0.0_ReKi
   do n=1,10
      t = n*Interval
      call DBEMT_UpdateStates(1, 1, t, u,  p, x, m, errStat2, errMsg2 )  
         call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName)
   
      if ( (errStat == ErrID_None) .and. (EqualRealNos(u(1)%vind_s(1),x%vind(1,1,1)) ) &
            .and. (EqualRealNos(u(1)%vind_s(1),x%vind_1(1,1,1)) )                       &
            .and. (EqualRealNos(u(1)%vind_s(2),x%vind(2,1,1)) )                         &
            .and. (EqualRealNos(u(1)%vind_s(2),x%vind_1(2,1,1)) )) then
         DBEMT_TEST09 = .true. .and. DBEMT_TEST09
      else
         DBEMT_TEST09 = .false.
      end if
   end do
   call DBEMT_End(u, p, x, m, ErrStat, ErrMsg )
   
   
   
end function DBEMT_TEST09


logical function DBEMT_TEST10(errStat, errMsg)

   
   integer(IntKi), INTENT(out)                  :: errStat       !< Error status of the operation
   character(ErrMsgLen), INTENT(out)                    :: errMsg        !< Error message if ErrStat /= ErrID_None
   type(DBEMT_InitInputType)       :: InitInp       !< Input data for initialization routine
   type(DBEMT_InputType)           :: u(2)             !< An initial guess for the input; input mesh must be defined
   type(DBEMT_ParameterType)       :: p             !< Parameters
   type(DBEMT_ContinuousStateType) :: x             !< Initial continuous states
   type(DBEMT_MiscVarType)         :: m             !< Initial misc/optimization variables
   real(DbKi)                      :: interval      !< Coupling interval in seconds: the rate that
                                                     !!   (1) DBEMT_UpdateStates() is called in loose coupling &
                                                     !!   (2) DBEMT_UpdateDiscState() is called in tight coupling.
                                                     !!   Input is the suggested time from the glue code;
                                                     !!   Output is the actual coupling interval that will be used
                                                     !!   by the glue code.
   type(DBEMT_InitOutputType)      :: InitOut       !< Output for initialization routine
   
   integer(IntKi)                  :: i,j,n
   real(DbKi)                      :: t
   integer(IntKi)                              :: errStat2      ! temporary error status of the operation
   character(ErrMsgLen)                        :: errMsg2       ! temporary error message 
   character(*), parameter                     :: RoutineName = 'DBEMT_TEST10'
   
     ! Initialize variables for this routine

   errStat2 = ErrID_None
   errMsg2  = ""
   DBEMT_TEST10 = .true.
   
   ! This test will evaluate the initialization of the module 
   
   errStat = ErrID_None
   errMsg  = ""

   InitInp%DBEMT_Mod = 1
   InitInp%numBlades = 1
   InitInp%numNodes  = 2
   allocate(InitInp%rlocal(InitInp%numNodes,InitInp%numBlades))
   do j=1,InitInp%numBlades
      do i=1,InitInp%numNodes
         InitInp%rlocal(i,j) = real(i,ReKi)
      end do
   end do
   Interval = 0.1_ReKi
   InitInp%tau1_const = 0.0_ReKi
   
   call DBEMT_Init(InitInp, u(1), p, x, m, Interval, InitOut, errStat2, errMsg2)
      call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName)
     
   u(1)%vind_s(1) = 10.0_ReKi
   u(1)%vind_s(2) = 0.0_ReKi
   
   u(2)%vind_s(1) = 12.0_ReKi
   u(2)%vind_s(2) =  1.0_ReKi
   do n=0,10
      t = n*Interval
      call DBEMT_UpdateStates(1, 1, t, u,  p, x, m, errStat2, errMsg2 )  
         call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName)
   
      if ( (errStat == ErrID_None) .and. (EqualRealNos(u(1)%vind_s(1),x%vind(1,1,1)) ) &
            .and. (EqualRealNos(u(1)%vind_s(1),x%vind_1(1,1,1)) )                       &
            .and. (EqualRealNos(u(1)%vind_s(2),x%vind(2,1,1)) )                         &
            .and. (EqualRealNos(u(1)%vind_s(2),x%vind_1(2,1,1)) )) then
         DBEMT_TEST10 = .true. .and. DBEMT_TEST10
      else
         DBEMT_TEST10 = .false.
      end if
      
      u(1)%vind_s(1) = 12.0_ReKi
      u(1)%vind_s(2) = 1.0_ReKi
   
   end do
   call DBEMT_End(u, p, x, m, ErrStat, ErrMsg )
   
   
   
end function DBEMT_TEST10

logical function DBEMT_TEST11(errStat, errMsg)

   
   integer(IntKi), INTENT(out)                  :: errStat       !< Error status of the operation
   character(ErrMsgLen), INTENT(out)                    :: errMsg        !< Error message if ErrStat /= ErrID_None
   type(DBEMT_InitInputType)       :: InitInp       !< Input data for initialization routine
   type(DBEMT_InputType)           :: u(2)             !< An initial guess for the input; input mesh must be defined
   type(DBEMT_ParameterType)       :: p             !< Parameters
   type(DBEMT_ContinuousStateType) :: x             !< Initial continuous states
   type(DBEMT_MiscVarType)         :: m             !< Initial misc/optimization variables
   real(DbKi)                      :: interval      !< Coupling interval in seconds: the rate that
                                                     !!   (1) DBEMT_UpdateStates() is called in loose coupling &
                                                     !!   (2) DBEMT_UpdateDiscState() is called in tight coupling.
                                                     !!   Input is the suggested time from the glue code;
                                                     !!   Output is the actual coupling interval that will be used
                                                     !!   by the glue code.
   type(DBEMT_InitOutputType)      :: InitOut       !< Output for initialization routine
   
   integer(IntKi)                  :: i,j,n
   real(DbKi)                      :: t
   integer(IntKi)                              :: errStat2      ! temporary error status of the operation
   character(ErrMsgLen)                        :: errMsg2       ! temporary error message 
   character(*), parameter                     :: RoutineName = 'DBEMT_TEST11'
   real(ReKi)          :: maxVind1
     ! Initialize variables for this routine

   errStat2 = ErrID_None
   errMsg2  = ""
   DBEMT_TEST11 = .true.
   maxVind1 = 0.0
   ! This test will evaluate the initialization of the module 
   
   errStat = ErrID_None
   errMsg  = ""

   InitInp%DBEMT_Mod = 1
   InitInp%numBlades = 1
   InitInp%numNodes  = 2
   allocate(InitInp%rlocal(InitInp%numNodes,InitInp%numBlades))
   do j=1,InitInp%numBlades
      do i=1,InitInp%numNodes
         InitInp%rlocal(i,j) = real(i,ReKi)
      end do
   end do
   Interval = 0.1_ReKi
   InitInp%tau1_const = .01000_ReKi
   
   call DBEMT_Init(InitInp, u(1), p, x, m, Interval, InitOut, errStat2, errMsg2)
      call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName)
     
   u(1)%vind_s(1) = 10.0_ReKi
   u(1)%vind_s(2) = 0.0_ReKi
   
   u(2)%vind_s(1) = 12.0_ReKi
   u(2)%vind_s(2) =  1.0_ReKi
   do n=0,10000
      t = n*Interval
      call DBEMT_UpdateStates(1, 1, t, u,  p, x, m, errStat2, errMsg2 )  
         call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName)
   
      if ( x%vind(1,1,1) > maxVind1 ) maxVind1 = x%vind(1,1,1)
      
      u(1)%vind_s(1) = 12.0_ReKi
      u(1)%vind_s(2) = 1.0_ReKi
   
   end do
   if ( (errStat == ErrID_None) .and. (EqualRealNos(u(1)%vind_s(1),x%vind(1,1,1)) ) &
            .and. (EqualRealNos(u(1)%vind_s(1),x%vind_1(1,1,1)) )                       &
            .and. (EqualRealNos(u(1)%vind_s(2),x%vind(2,1,1)) )                         &
            .and. (EqualRealNos(u(1)%vind_s(2),x%vind_1(2,1,1)) )) then
         DBEMT_TEST11 = (.true. .and. DBEMT_TEST11)
      else
         DBEMT_TEST11 = .false.
      end if
   call DBEMT_End(u, p, x, m, ErrStat, ErrMsg )
   
   
   
end function DBEMT_TEST11

end module DBEMT