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
module BEMT
    
   use NWTC_Library
   
   use BEMT_Types
   use BEMTUncoupled

   
   use UnsteadyAero
   !USE AeroDyn_Types
   use AirfoilInfo
   

   implicit none
         
   
   private
   
   type(ProgDesc), parameter  :: BEMT_Ver = ProgDesc( 'BEM', 'v1.03.00', '29-Oct-2016' )
   character(*),   parameter  :: BEMT_Nickname = 'BEM'
      
   
   ! ..... Public Subroutines ...................................................................................................

   public :: BEMT_Init                           ! Initialization routine
   public :: BEMT_End                            ! Ending routine (includes clean up)

   public :: BEMT_UpdateStates                   ! Loose coupling routine for solving for constraint states, integrating
                                               !   continuous states, and updating discrete states
   public :: BEMT_CalcOutput                     ! Routine for computing outputs

   public :: BEMT_CalcConstrStateResidual        ! Tight coupling routine for returning the constraint state residual
   public :: BEMT_CalcContStateDeriv             ! Tight coupling routine for computing derivatives of continuous states
   public :: BEMT_UpdateDiscState                ! Tight coupling routine for updating discrete states
   
   ! routines for linearization
   public :: Get_phi_perturbations   
   public :: ComputeFrozenWake
   public :: CheckLinearizationInput
   
   contains


    
!----------------------------------------------------------------------------------------------------------------------------------   
real(ReKi) function ComputePhiWithInduction( Vx, Vy, a, aprime )
! This routine is used to compute the inflow angle, phi, from the local velocities and the induction factors.
!..................................................................................................................................
   real(ReKi),                    intent(in   )  :: Vx          ! Local velocity component along the thrust direction
   real(ReKi),                    intent(in   )  :: Vy          ! Local velocity component along the rotor plane-of-rotation direction
   real(ReKi),                    intent(in   )  :: a           ! Axial induction factor
   real(ReKi),                    intent(in   )  :: aprime      ! Tangential induction factor
   
   real(ReKi)                                    :: x
   real(ReKi)                                    :: y
      
   x = Vx*(1-a)
   y = Vy*(1+aprime)
   
   if ( EqualRealNos(y, 0.0_ReKi) .AND. EqualRealNos(x, 0.0_ReKi) ) then
      ComputePhiWithInduction = 0.0_ReKi
   else
      ComputePhiWithInduction  = atan2( x , y )
   end if
   
   
end function ComputePhiWithInduction
 
!----------------------------------------------------------------------------------------------------------------------------------   
subroutine BEMT_Set_UA_InitData( InitInp, interval, Init_UA_Data, errStat, errMsg )
! This routine is called from BEMT_Init.
! The parameters are set here and not changed during the simulation.
!..................................................................................................................................
   type(BEMT_InitInputType),       intent(inout)  :: InitInp     ! Input data for initialization routine, out is needed because of copy below
   real(DbKi),                     intent(in   )  :: interval    ! time interval  
   type(UA_InitInputType),         intent(  out)  :: Init_UA_Data           ! Parameters
   integer(IntKi),                 intent(  out)  :: errStat     ! Error status of the operation
   character(*),                   intent(  out)  :: errMsg      ! Error message if ErrStat /= ErrID_None

   integer                                        :: i,j
   integer(intKi)                                 :: ErrStat2           ! temporary Error status
   
      ! Set up initialization data
   
   Allocate(Init_UA_Data%c(InitInp%numBladeNodes,InitInp%numBlades), STAT = errStat2)
   if (ErrStat2 /= 0) then
      ErrStat = ErrID_Fatal
      ErrMsg = "BEMT_Set_UA_InitData:Error allocating Init_UA_Data%c."
      return
   else
      ErrStat = ErrID_None
      ErrMsg = ""
   end if
   
   do j = 1,InitInp%numBlades
      do i = 1,InitInp%numBladeNodes
         Init_UA_Data%c(i,j)      = InitInp%chord(i,j)
      end do
   end do
   
      ! TODO:: Fully implement these initialization inputs
   
   Init_UA_Data%dt              = interval          
   Init_UA_Data%OutRootName     = ''
               
   Init_UA_Data%numBlades       = InitInp%numBlades 
   Init_UA_Data%nNodesPerBlade  = InitInp%numBladeNodes
                                  
   Init_UA_Data%NumOuts         = 0
   Init_UA_Data%UAMod           = InitInp%UAMod  
   Init_UA_Data%Flookup         = InitInp%Flookup
   Init_UA_Data%a_s             = InitInp%a_s ! m/s  
   
end subroutine BEMT_Set_UA_InitData

   
!----------------------------------------------------------------------------------------------------------------------------------   
subroutine BEMT_SetParameters( InitInp, p, errStat, errMsg )
! This routine is called from BEMT_Init.
! The parameters are set here and not changed during the simulation.
!..................................................................................................................................
   type(BEMT_InitInputType),       intent(inout)  :: InitInp     ! Input data for initialization routine, out is needed because of copy below
   type(BEMT_ParameterType),       intent(  out)  :: p           ! Parameters
   integer(IntKi),                 intent(  out)  :: errStat     ! Error status of the operation
   character(*),                   intent(  out)  :: errMsg      ! Error message if ErrStat /= ErrID_None


      ! Local variables
   integer(IntKi)                                :: errStat2                ! temporary Error status of the operation
   character(*), parameter                       :: RoutineName = 'BEMT_SetParameters'
   integer(IntKi)                                :: i, j

   
      ! Initialize variables for this routine

   errStat = ErrID_None
   errMsg  = ""

   p%numBladeNodes  = InitInp%numBladeNodes 
   p%numBlades      = InitInp%numBlades    
   p%UA_Flag        = InitInp%UA_Flag
   
   allocate ( p%chord(p%numBladeNodes, p%numBlades), STAT = errStat2 )
   if ( errStat2 /= 0 ) then
      call SetErrStat( ErrID_Fatal, 'Error allocating memory for p%chord.', errStat, errMsg, RoutineName )
      return
   end if 
   
   allocate ( p%zHub(p%numBlades), STAT = errStat2 )
   if ( errStat2 /= 0 ) then
      call SetErrStat( ErrID_Fatal, 'Error allocating memory for p%zHub.', errStat, errMsg, RoutineName )
      return
   end if 
   
   allocate ( p%AFindx(p%numBladeNodes,p%numBlades), STAT = errStat2 )
   if ( errStat2 /= 0 ) then
      call SetErrStat( ErrID_Fatal, 'Error allocating memory for p%AFindx.', errStat, errMsg, RoutineName )
      return
   end if 
   
   allocate ( p%tipLossConst(p%numBladeNodes, p%numBlades), STAT = errStat2 )
   if ( errStat2 /= 0 ) then
      call SetErrStat( ErrID_Fatal, 'Error allocating memory for p%tipLossConst.', errStat, errMsg, RoutineName )
      return
   end if 
   
   allocate ( p%hubLossConst(p%numBladeNodes, p%numBlades), STAT = errStat2 )
   if ( errStat2 /= 0 ) then
      call SetErrStat( ErrID_Fatal, 'Error allocating memory for p%hubLossConst.', errStat, errMsg, RoutineName )
      return
   end if 
   
   p%AFindx = InitInp%AFindx 
   
      ! Compute the tip and hub loss constants using the distances along the blade (provided as input for now) 
   do j=1,p%numBlades
      p%zHub(j) = InitInp%zHub(j)
      do i=1,p%numBladeNodes
         p%chord(i,j)        = InitInp%chord(i,j)
         p%tipLossConst(i,j) = p%numBlades*(InitInp%zTip    (j) - InitInp%zLocal(i,j)) / (2.0*InitInp%zLocal(i,j))
         p%hubLossConst(i,j) = p%numBlades*(InitInp%zLocal(i,j) - InitInp%zHub    (j)) / (2.0*InitInp%zHub    (j))
      end do
   end do
   
   
  !p%DT               = InitInp%DT                             
   p%airDens          = InitInp%airDens          
   p%kinVisc          = InitInp%kinVisc          
   p%skewWakeMod      = InitInp%skewWakeMod     
   p%useTipLoss       = InitInp%useTipLoss       
   p%useHubLoss       = InitInp%useHubLoss 
   p%useInduction     = InitInp%useInduction
   p%useTanInd        = InitInp%useTanInd        
   p%useAIDrag        = InitInp%useAIDrag           
   p%useTIDrag        = InitInp%useTIDrag           
   p%numReIterations  = InitInp%numReIterations  
   p%maxIndIterations = InitInp%maxIndIterations 
   p%aTol             = InitInp%aTol
   
end subroutine BEMT_SetParameters

!----------------------------------------------------------------------------------------------------------------------------------
subroutine BEMT_InitContraintStates( z, p, errStat, errMsg )
! This routine is called from BEMT_Init.
! The constraint state data is allocated and set to zero.
!..................................................................................................................................

   type(BEMT_ConstraintStateType), intent(  out)  :: z           ! Input data for initialization routine
   type(BEMT_ParameterType),       intent(in   )  :: p           ! Parameters
   integer(IntKi),                 intent(  out)  :: errStat     ! Error status of the operation
   character(*),                   intent(  out)  :: errMsg      ! Error message if ErrStat /= ErrID_None


      ! Local variables
   character(*), parameter                       :: RoutineName = 'BEMT_InitContraintStates'
   integer(IntKi)                                :: errStat2                ! temporary Error status of the operation
   
      ! Initialize variables for this routine

   errStat = ErrID_None
   errMsg  = ""
      
   allocate ( z%phi( p%numBladeNodes, p%numBlades ), STAT = errStat2 )
   if ( errStat2 /= 0 ) then
      call SetErrStat( ErrID_Fatal, 'Error allocating memory for z%phi.', errStat, errMsg, RoutineName )
      return
   end if 
   z%phi = 0.0_ReKi
   
  
end subroutine BEMT_InitContraintStates


!----------------------------------------------------------------------------------------------------------------------------------
subroutine BEMT_InitOtherStates( OtherState, p, errStat, errMsg )
! This routine is called from BEMT_Init.
! The OtherState data is allocated and set to zero.
!..................................................................................................................................

   type(BEMT_OtherStateType),      intent(inout)  :: OtherState  ! OtherState data
   type(BEMT_ParameterType),       intent(in   )  :: p           ! Parameters
   integer(IntKi),                intent(  out)  :: errStat     ! Error status of the operation
   character(*),                  intent(  out)  :: errMsg      ! Error message if ErrStat /= ErrID_None


      ! Local variables
   character(ErrMsgLen)                          :: errMsg2                 ! temporary Error message if ErrStat /= ErrID_None
   integer(IntKi)                                :: errStat2                ! temporary Error status of the operation
   character(*), parameter                       :: RoutineName = 'BEMT_InitOtherStates'
   
      ! Initialize variables for this routine

   errStat = ErrID_None
   errMsg  = ""
   
   if (p%UseInduction) then
      
      allocate ( OtherState%ValidPhi( p%numBladeNodes, p%numBlades ), STAT = errStat2 )
      if ( errStat2 /= 0 ) then
         call SetErrStat( ErrID_Fatal, 'Error allocating memory for OtherState%ValidPhi.', errStat, errMsg, RoutineName )
         return
      end if 
      OtherState%ValidPhi = .true.

   end if
   
   OtherState%nodesInitialized = .false. ! z%phi hasn't been initialized properly, so make sure we compute a value for phi until we've updated them in the first call to BEMT_UpdateStates()
   
!
!      
!   allocate ( OtherState%axInduction( p%numBladeNodes, p%numBlades ), STAT = errStat2 )
!   if ( errStat2 /= 0 ) then
!      errStat2 = ErrID_Fatal
!      errMsg2  = 'Error allocating memory for OtherState%axInduction.'
!      call SetErrStat( errStat2, errMsg2, errStat, errMsg, 'BEMT_InitOtherStates' )
!      return
!   end if 
!   OtherState%axInduction = 0.0_ReKi
!   
!   allocate ( OtherState%tanInduction( p%numBladeNodes, p%numBlades ), STAT = errStat2 )
!   if ( errStat2 /= 0 ) then
!      errStat2 = ErrID_Fatal
!      errMsg2  = 'Error allocating memory for OtherState%tanInduction.'
!      call SetErrStat( errStat2, errMsg2, errStat, errMsg, 'BEMT_InitOtherStates' )
!      return
!   end if 
!   OtherState%tanInduction = 0.0_ReKi
!
!   allocate ( OtherState%Re( p%numBladeNodes, p%numBlades ), STAT = errStat2 )
!   if ( errStat2 /= 0 ) then
!      errStat2 = ErrID_Fatal
!      errMsg2  = 'Error allocating memory for OtherState%Re.'
!      call SetErrStat( errStat2, errMsg2, errStat, errMsg, 'BEMT_InitOtherStates' )
!      return
!   end if 
!   OtherState%Re = 0.0_ReKi
!   
!   allocate ( OtherState%AOA( p%numBladeNodes, p%numBlades ), STAT = errStat2 )
!   if ( errStat2 /= 0 ) then
!      errStat2 = ErrID_Fatal
!      errMsg2  = 'Error allocating memory for OtherState%AOA.'
!      call SetErrStat( errStat2, errMsg2, errStat, errMsg, 'BEMT_InitOtherStates' )
!      return
!   end if 
!   OtherState%AOA = 0.0_ReKi
!   
!   allocate ( OtherState%Cx( p%numBladeNodes, p%numBlades ), STAT = errStat2 )
!   if ( errStat2 /= 0 ) then
!      errStat2 = ErrID_Fatal
!      errMsg2  = 'Error allocating memory for OtherState%Cx.'
!      call SetErrStat( errStat2, errMsg2, errStat, errMsg, 'BEMT_InitOtherStates' )
!      return
!   end if 
!   OtherState%Cx = 0.0_ReKi
!   
!   allocate ( OtherState%Cy( p%numBladeNodes, p%numBlades ), STAT = errStat2 )
!   if ( errStat2 /= 0 ) then
!      errStat2 = ErrID_Fatal
!      errMsg2  = 'Error allocating memory for OtherState%Cy.'
!      call SetErrStat( errStat2, errMsg2, errStat, errMsg, 'BEMT_InitOtherStates' )
!      return
!   end if 
!   OtherState%Cy = 0.0_ReKi
!   
!   allocate ( OtherState%Cl( p%numBladeNodes, p%numBlades ), STAT = errStat2 )
!   if ( errStat2 /= 0 ) then
!      errStat2 = ErrID_Fatal
!      errMsg2  = 'Error allocating memory for OtherState%Cl.'
!      call SetErrStat( errStat2, errMsg2, errStat, errMsg, 'BEMT_InitOtherStates' )
!      return
!   end if 
!   OtherState%Cl = 0.0_ReKi
!   
!   allocate ( OtherState%Cd( p%numBladeNodes, p%numBlades ), STAT = errStat2 )
!   if ( errStat2 /= 0 ) then
!      errStat2 = ErrID_Fatal
!      errMsg2  = 'Error allocating memory for OtherState%Cd.'
!      call SetErrStat( errStat2, errMsg2, errStat, errMsg, 'BEMT_InitOtherStates' )
!      return
!   end if 
!   OtherState%Cd = 0.0_ReKi
   
end subroutine BEMT_InitOtherStates

!----------------------------------------------------------------------------------------------------------------------------------
subroutine BEMT_AllocInput( u, p, errStat, errMsg )
! This routine is called from BEMT_Init.
!  
!  
!..................................................................................................................................

   type(BEMT_InputType),           intent(  out)  :: u           ! Input data
   type(BEMT_ParameterType),       intent(in   )  :: p           ! Parameters
   integer(IntKi),                 intent(  out)  :: errStat     ! Error status of the operation
   character(*),                   intent(  out)  :: errMsg      ! Error message if ErrStat /= ErrID_None


      ! Local variables
   integer(IntKi)                                :: errStat2                ! temporary Error status of the operation
   character(*), parameter                       :: RoutineName = 'BEMT_AllocInput'

      ! Initialize variables for this routine

   errStat = ErrID_None
   errMsg  = ""

   allocate ( u%theta( p%numBladeNodes, p%numBlades ), STAT = errStat2 )
   if ( errStat2 /= 0 ) then
      call SetErrStat( ErrID_Fatal, 'Error allocating memory for u%theta.', errStat, errMsg, RoutineName )
      return
   end if 
   u%theta = 0.0_ReKi
   
   allocate ( u%psi( p%numBlades ), STAT = errStat2 )
   if ( errStat2 /= 0 ) then
      call SetErrStat( ErrID_Fatal, 'Error allocating memory for u%psi.', errStat, errMsg, RoutineName )
      return
   end if 
   u%psi = 0.0_ReKi
   
   allocate ( u%Vx( p%numBladeNodes, p%numBlades ), STAT = errStat2 )
   if ( errStat2 /= 0 ) then
      call SetErrStat( ErrID_Fatal, 'Error allocating memory for u%Vx.', errStat, errMsg, RoutineName )
      return
   end if 
   u%Vx = 0.0_ReKi
   
   allocate ( u%Vy( p%numBladeNodes, p%numBlades ), STAT = errStat2 )
   if ( errStat2 /= 0 ) then
      call SetErrStat( ErrID_Fatal, 'Error allocating memory for u%Vy.', errStat, errMsg, RoutineName )
      return
   end if 
   u%Vy = 0.0_ReKi
 
   
   allocate ( u%rLocal( p%numBladeNodes, p%numBlades ), STAT = errStat2 )
   if ( errStat2 /= 0 ) then
      call SetErrStat( ErrID_Fatal, 'Error allocating memory for u%rLocal.', errStat, errMsg, RoutineName )
      return
   end if 
   u%rLocal = 0.0_ReKi
   
   
   
   u%omega  = 0.0_ReKi
   
end subroutine BEMT_AllocInput


!----------------------------------------------------------------------------------------------------------------------------------
subroutine BEMT_AllocOutput( y, p, errStat, errMsg )
! This routine is called from BEMT_Init.
!  
!  
!..................................................................................................................................

   type(BEMT_OutputType),         intent(  out)  :: y           ! output data
   type(BEMT_ParameterType),      intent(in   )  :: p           ! Parameters
   integer(IntKi),                intent(  out)  :: errStat     ! Error status of the operation
   character(*),                  intent(  out)  :: errMsg      ! Error message if ErrStat /= ErrID_None


      ! Local variables
   character(ErrMsgLen  )                        :: errMsg2                 ! temporary Error message if ErrStat /= ErrID_None
   integer(IntKi)                                :: errStat2                ! temporary Error status of the operation
   character(*), parameter                       :: RoutineName = 'BEMT_AllocOutput'
   
      ! Initialize variables for this routine

   errStat = ErrID_None
   errMsg  = ""

   call allocAry( y%Vrel, p%numBladeNodes, p%numBlades, 'y%Vrel', errStat2, errMsg2); call setErrStat(errStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)
   call allocAry( y%phi, p%numBladeNodes, p%numBlades, 'y%phi', errStat2, errMsg2); call setErrStat(errStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)
   call allocAry( y%chi, p%numBladeNodes, p%numBlades, 'y%chi', errStat2, errMsg2); call setErrStat(errStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)
   call allocAry( y%Re, p%numBladeNodes, p%numBlades, 'y%Re', errStat2, errMsg2); call setErrStat(errStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)
   call allocAry( y%axInduction, p%numBladeNodes, p%numBlades, 'y%axInduction', errStat2, errMsg2); call setErrStat(errStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)
   call allocAry( y%tanInduction, p%numBladeNodes, p%numBlades, 'y%tanInduction', errStat2, errMsg2); call setErrStat(errStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)
   call allocAry( y%AOA, p%numBladeNodes, p%numBlades, 'y%AOA', errStat2, errMsg2); call setErrStat(errStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)
   call allocAry( y%Cx, p%numBladeNodes, p%numBlades, 'y%Cx', errStat2, errMsg2); call setErrStat(errStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)
   call allocAry( y%Cy, p%numBladeNodes, p%numBlades, 'y%Cy', errStat2, errMsg2); call setErrStat(errStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)
   call allocAry( y%Cm, p%numBladeNodes, p%numBlades, 'y%Cm', errStat2, errMsg2); call setErrStat(errStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)
   call allocAry( y%Cl, p%numBladeNodes, p%numBlades, 'y%Cl', errStat2, errMsg2); call setErrStat(errStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)
   call allocAry( y%Cd, p%numBladeNodes, p%numBlades, 'y%Cd', errStat2, errMsg2); call setErrStat(errStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)
   
   if (ErrStat >= AbortErrLev) RETURN
   
      ! outputs documented in AeroDyn
   y%Vrel = 0.0_ReKi
   y%phi = 0.0_ReKi   
   y%Cx = 0.0_ReKi
   y%Cy = 0.0_ReKi
   y%Cm = 0.0_ReKi

      ! others:
   y%chi = 0.0_ReKi
   y%Re = 0.0_ReKi   
   y%axInduction = 0.0_ReKi   
   y%tanInduction = 0.0_ReKi
   y%AOA = 0.0_ReKi   
   y%Cl = 0.0_ReKi
   y%Cd = 0.0_ReKi
   
end subroutine BEMT_AllocOutput



subroutine BEMT_MapOutputs(p, OtherState, y, errStat, errMsg)

   type(BEMT_ParameterType),       intent(in   )  :: p           ! Parameters
   type(BEMT_OtherStateType),      intent(in   )  :: OtherState  ! other states
   type(BEMT_OutputType),          intent(inout)  :: y           ! system outputs 
   integer(IntKi),                 intent(  out)  :: errStat     ! Error status of the operation
   character(*),                   intent(  out)  :: errMsg      ! Error message if ErrStat /= ErrID_None
   
   ErrStat = ErrID_None
   ErrMsg = ""
   
end subroutine BEMT_MapOutputs


!----------------------------------------------------------------------------------------------------------------------------------
subroutine BEMT_Init( InitInp, u, p, x, xd, z, OtherState, AFInfo, y, misc, Interval, InitOut, ErrStat, ErrMsg )
! This routine is called at the start of the simulation to perform initialization steps.
! The parameters are set here and not changed during the simulation.
! The initial states and initial guess for the input are defined.
!..................................................................................................................................

   type(BEMT_InitInputType),       intent(inout)  :: InitInp     ! Input data for initialization routine, needs to be inout because there is a copy of some data in InitInp in BEMT_SetParameters()
   type(BEMT_InputType),           intent(  out)  :: u           ! An initial guess for the input; input mesh must be defined
   type(BEMT_ParameterType),       intent(  out)  :: p           ! Parameters
   type(BEMT_ContinuousStateType), intent(  out)  :: x           ! Initial continuous states
   type(BEMT_DiscreteStateType),   intent(  out)  :: xd          ! Initial discrete states
   type(BEMT_ConstraintStateType), intent(  out)  :: z           ! Initial guess of the constraint states
   type(BEMT_OtherStateType),      intent(  out)  :: OtherState  ! Initial other states
   type(BEMT_MiscVarType),         intent(  out)  :: misc        ! Initial misc/optimization variables
   type(AFInfoType),               intent(in   )  :: AFInfo(:)   ! The airfoil parameter data
   type(BEMT_OutputType),          intent(  out)  :: y           ! Initial system outputs (outputs are not calculated;
                                                                 !   only the output mesh is initialized)
   real(DbKi),                     intent(inout)  :: interval    ! Coupling interval in seconds: the rate that
                                                                 !   (1) BEMT_UpdateStates() is called in loose coupling &
                                                                 !   (2) BEMT_UpdateDiscState() is called in tight coupling.
                                                                 !   Input is the suggested time from the glue code;
                                                                 !   Output is the actual coupling interval that will be used
                                                                 !   by the glue code.
   type(BEMT_InitOutputType),      intent(  out)  :: InitOut     ! Output for initialization routine
   integer(IntKi),                 intent(  out)  :: errStat     ! Error status of the operation
   character(*),                   intent(  out)  :: errMsg      ! Error message if ErrStat /= ErrID_None


      ! Local variables
   character(ErrMsgLen)                           :: errMsg2     ! temporary Error message if ErrStat /= ErrID_None
   integer(IntKi)                                 :: errStat2    ! temporary Error status of the operation
   character(*), parameter                        :: RoutineName = 'BEMT_Init'
   type(UA_InputType)                             :: u_UA
   type(UA_InitInputType)                         :: Init_UA_Data
   type(UA_InitOutputType)                        :: InitOutData_UA
   integer(IntKi)                                 :: i,j
   real(ReKi)                                             :: Cn1             ! critical value of Cn_prime at LE separation for alpha >= alpha0    
   real(ReKi)                                             :: Cn2             ! critical value of Cn_prime at LE separation for alpha < alpha0
   real(ReKi)                                             :: Cd0
   real(ReKi)                                             :: Cm0
   real(ReKi)                                             :: k0
   real(ReKi)                                             :: k1
   real(ReKi)                                             :: k2
   real(ReKi)                                             :: k3
   real(ReKi)                                             :: T_VL
   real(ReKi)                                             :: x_cp_bar 
   real(ReKi)                                             :: alpha0              ! zero lift angle of attack (radians)
   real(ReKi)                                             :: eta_e
   real(ReKi)                                             :: St_sh
   real(ReKi)                                             :: Re
   real(ReKi)                                             :: alpha

   real(ReKi)                                     :: M, alpha1, alpha2, C_nalpha, C_nalpha_circ, T_f0, T_V0, T_p, b1,b2,b5,A1,A2,A5,S1,S2,S3,S4,k1_hat
   real(ReKi)                :: filtCutOff                    ! airfoil parameter for the low-pass cut-off frequency for pitching rate and accelerations (Hz)
   character(64)                                  :: chanPrefix
      ! Initialize variables for this routine
   errStat = ErrID_None
   errMsg  = ""


      ! Initialize the NWTC Subroutine Library
   call NWTC_Init( EchoLibVer=.FALSE. )

      ! Display the module information
   call DispNVD( BEMT_Ver )

    
   

      !............................................................................................
      ! Define parameters here
      !............................................................................................
       
   call BEMT_SetParameters( InitInp, p, errStat, errMsg )
   if (errStat >= AbortErrLev) return
   p%DT = interval
      !............................................................................................
      ! Define states here
      !............................................................................................
   
      ! We need to set an other state version so that we can change this during execution if the AOA is too large!
   allocate ( OtherState%UA_Flag( p%numBladeNodes, p%numBlades ), STAT = errStat2 )
   if ( errStat2 /= 0 ) then
      errStat2 = ErrID_Fatal
      errMsg2  = 'Error allocating memory for OtherState%UA_Flag.'
      call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
      return
   end if 
   OtherState%UA_Flag = p%UA_Flag
   
      ! initialize the constraint states
   call BEMT_InitContraintStates( z, p, errStat2, errMsg2 )     ! initialize the continuous states
      call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
      if (errStat >= AbortErrLev) return
   
   x%DummyContState = 0.0_SiKi
   
      ! Initialize other states
   call BEMT_InitOtherStates( OtherState, p,  errStat, errMsg )    ! initialize the other states
   if (errStat >= AbortErrLev) return
      
   if ( p%UA_Flag ) then
      call BEMT_Set_UA_InitData( InitInp, interval, Init_UA_Data, errStat2, errMsg2 )
         call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
         if (errStat >= AbortErrLev) then
            call cleanup()
            return
         end if
      
      
      call UA_Init( Init_UA_Data, u_UA, p%UA, xd%UA, OtherState%UA, misc%y_UA, misc%UA, interval, InitOutData_UA, errStat2, errMsg2 )       
         call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
         if (errStat >= AbortErrLev) then
            call cleanup()
            return
         end if
      
#ifdef UA_OUTS   
   !CALL GetNewUnit( UnUAOuts, ErrStat, ErrMsg )
   !IF ( ErrStat /= ErrID_None ) RETURN

   CALL OpenFOutFile ( 69, 'Debug.UA.out', errStat, errMsg )
         IF (ErrStat >= AbortErrLev) RETURN

   
      ! Heading:
   WRITE (69,'(/,A)')  'This output information was generated by '//TRIM( GetNVD(BEMT_Ver) )// &
                         ' on '//CurDate()//' at '//CurTime()//'.'
   WRITE (69,'(:,A11)', ADVANCE='no' ) 'Time   '
   do j = 1, p%numBlades
      do i = 1, p%numBladeNodes  
         chanPrefix = "B"//trim(num2lstr(j))//"N"//trim(num2lstr(i))  
         WRITE (69,'(:,A11)', ADVANCE='no' )  'ALPHA'//chanPrefix      
         WRITE (69,'(:,A11)', ADVANCE='no' )  'VREL'//chanPrefix  
         WRITE (69,'(:,A11)', ADVANCE='no' )  'CN'//chanPrefix      
         WRITE (69,'(:,A11)', ADVANCE='no' )  'CC'//chanPrefix
         WRITE (69,'(:,A11)', ADVANCE='no' )  'CL'//chanPrefix
         WRITE (69,'(:,A11)', ADVANCE='no' )  'CD'//chanPrefix
         WRITE (69,'(:,A11)', ADVANCE='no' )  'CM'//chanPrefix     
         WRITE (69,'(:,A11)', ADVANCE='no' )  'CNCP'//chanPrefix    
         WRITE (69,'(:,A11)', ADVANCE='no' )  'CNIQ'//chanPrefix
         WRITE (69,'(:,A11)', ADVANCE='no' )  'CNPOT'//chanPrefix
         WRITE (69,'(:,A11)', ADVANCE='no' )  'DPP'//chanPrefix       
         WRITE (69,'(:,A11)', ADVANCE='no' )  'CNP'//chanPrefix        
         WRITE (69,'(:,A11)', ADVANCE='no' )  'FSP'//chanPrefix       
         WRITE (69,'(:,A11)', ADVANCE='no' )  'DF'//chanPrefix
         WRITE (69,'(:,A11)', ADVANCE='no' )  'CNV'//chanPrefix         
         WRITE (69,'(:,A11)', ADVANCE='no' )  'TAUV'//chanPrefix
         WRITE (69,'(:,A11)', ADVANCE='no' )  'LESF'//chanPrefix
         WRITE (69,'(:,A11)', ADVANCE='no' )  'TESF'//chanPrefix 
         WRITE (69,'(:,A11)', ADVANCE='no' )  'VRTX'//chanPrefix
         WRITE (69,'(:,A11)', ADVANCE='no' )  'CVN'//chanPrefix
         WRITE (69,'(:,A11)', ADVANCE='no' )  'CMI'//chanPrefix
         WRITE (69,'(:,A11)', ADVANCE='no' )  'CMQ'//chanPrefix
         WRITE (69,'(:,A11)', ADVANCE='no' )  'CMV'//chanPrefix
         WRITE (69,'(:,A11)', ADVANCE='no' )  'AFEP'//chanPrefix
         WRITE (69,'(:,A11)', ADVANCE='no' )  'DFAF'//chanPrefix
         WRITE (69,'(:,A11)', ADVANCE='no' )  'PMC'//chanPrefix
         WRITE (69,'(:,A11)', ADVANCE='no' )  'T_f'//chanPrefix
         WRITE (69,'(:,A11)', ADVANCE='no' )  'T_V'//chanPrefix
         WRITE (69,'(:,A11)', ADVANCE='no' )  'dS'//chanPrefix
      end do
   end do  
   write (69,'(A)')    ' '
   
   WRITE (69,'(:,A9)', ADVANCE='no' ) '      (s)'
   do j = 1, p%numBlades
      do i = 1, p%numBladeNodes  
         WRITE (69,'(:,A11)', ADVANCE='no' ) '(deg)'
         WRITE (69,'(:,A11)', ADVANCE='no' ) '(m/s)'
         WRITE (69,'(:,A11)', ADVANCE='no' ) '(-)'
         WRITE (69,'(:,A11)', ADVANCE='no' ) '(-)'
         WRITE (69,'(:,A11)', ADVANCE='no' ) '(-)'
         WRITE (69,'(:,A11)', ADVANCE='no' ) '(-)'
         WRITE (69,'(:,A11)', ADVANCE='no' ) '(-)'
         WRITE (69,'(:,A11)', ADVANCE='no' ) '(-)'
         WRITE (69,'(:,A11)', ADVANCE='no' ) '(-)'
         WRITE (69,'(:,A11)', ADVANCE='no' ) '(-)'
         WRITE (69,'(:,A11)', ADVANCE='no' ) '(-)'
         WRITE (69,'(:,A11)', ADVANCE='no' ) '(-)'
         WRITE (69,'(:,A11)', ADVANCE='no' ) '(-)'
         WRITE (69,'(:,A11)', ADVANCE='no' ) '(-)'
         WRITE (69,'(:,A11)', ADVANCE='no' ) '(-)'
         WRITE (69,'(:,A11)', ADVANCE='no' ) '(-)'
         WRITE (69,'(:,A11)', ADVANCE='no' ) '(-)'
         WRITE (69,'(:,A11)', ADVANCE='no' ) '(-)'
         WRITE (69,'(:,A11)', ADVANCE='no' ) '(-)'
         WRITE (69,'(:,A11)', ADVANCE='no' ) '(-)'
         WRITE (69,'(:,A11)', ADVANCE='no' ) '(-)'
         WRITE (69,'(:,A11)', ADVANCE='no' ) '(-)'
         WRITE (69,'(:,A11)', ADVANCE='no' ) '(-)'
         WRITE (69,'(:,A11)', ADVANCE='no' ) '(-)'
         WRITE (69,'(:,A11)', ADVANCE='no' ) '(-)'
         WRITE (69,'(:,A11)', ADVANCE='no' ) '(-)'
         WRITE (69,'(:,A11)', ADVANCE='no' ) '(-)'
         WRITE (69,'(:,A11)', ADVANCE='no' ) '(-)'
         WRITE (69,'(:,A11)', ADVANCE='no' ) '(-)'
      end do
   end do
   write (69,'(A)')    ' '
   
#endif

      do j = 1,p%numBlades
         do i = 1,p%numBladeNodes ! Loop over blades and nodes
      
            ! u_UA values were not set in UA_Init, so we will use arbitrary values for M,Re, and alpha,  and we need them only because we're trying to determine if C_nalpha is 0 so we can shut off UA.

            M = 0.1_ReKi
            Re = 10.0_ReKi
            alpha = 2.0*D2R
            !M           = u_UA%U / p%UA%a_s
            !call UA_CheckMachNumber(M, misc%UA%FirstWarn_M, ErrStat2, ErrMsg2 )
            
            call AFI_GetAirfoilParams( AFInfo(p%AFindx(i,j)), M, Re, alpha0, alpha1, alpha2, eta_e, C_nalpha, C_nalpha_circ, &
                                    T_f0, T_V0, T_p, T_VL, St_sh, b1, b2, b5, A1, A2, A5, S1, S2, S3, S4, Cn1, Cn2, Cd0, Cm0, k0, k1, k2, k3, k1_hat, x_cp_bar, filtCutOff, errMsg2, errStat2 )           
            ! AFI_GetAirfoilParams does not set errors
            
            if ( EqualRealNos(C_nalpha, 0.0_ReKi) .and. OtherState%UA_Flag(i,j) ) then
               OtherState%UA_Flag(i,j) = .false.
               call WrScr( 'Warning: Turning off Unsteady Aerodynamics because C_nalpha is 0.  BladeNode = '//trim(num2lstr(i))//', Blade = '//trim(num2lstr(j)) )
            end if
            
         end do
      end do
      
   
   end if
      !............................................................................................
      ! Define initial guess for the system inputs here:
      !............................................................................................

         ! allocate all the arrays that store data in the input type:
   call BEMT_AllocInput( u, p, errStat2, errMsg2 )      
      call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
      if (errStat >= AbortErrLev) then
         call cleanup()
         return                  
      end if
      
   
  

      !............................................................................................
      ! Define system output initializations (set up meshes) here:
      !............................................................................................
      !............................................................................................
      ! Define initialization-routine output here:
      !............................................................................................
   
   !call BEMT_InitOut(p, InitOut,  errStat2, errMsg2)
   !call CheckError( errStat2, errMsg2 )
   
   call BEMT_AllocOutput(y, p, errStat2, errMsg2) !u is sent so we can create sibling meshes
      call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
      if (errStat >= AbortErrLev) then
         call cleanup()
         return                  
      end if
   
   
   misc%useFrozenWake = .FALSE.

   InitOut%Version         = BEMT_Ver
   
 

      !............................................................................................
      ! If you want to choose your own rate instead of using what the glue code suggests, tell the glue code the rate at which
      !   this module must be called here:
      !............................................................................................

   Interval = p%DT


       ! Print the summary file if requested:
   !IF (InputFileData%SumPrint) THEN
   !   CALL BEMT_PrintSum( p, OtherState, GetAdamsVals, ErrStat2, ErrMsg2 )
   !   call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
   !END IF
       
       ! Destroy the InputFileData structure (deallocate arrays)

   !CALL BEMT_DestroyInputFile(InputFileData, ErrStat2, ErrMsg2 )
   !   call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )

CONTAINS
   !...............................................................................................................................
   SUBROUTINE Cleanup()
   ! This subroutine cleans up local variables that may have allocatable arrays
   !...............................................................................................................................

   call UA_DestroyInput( u_UA, ErrStat2, ErrMsg2 )
   call UA_DestroyInitInput( Init_UA_Data, ErrStat2, ErrMsg2 )
   call UA_DestroyInitOutput( InitOutData_UA, ErrStat2, ErrMsg2 )

   END SUBROUTINE Cleanup

END SUBROUTINE BEMT_Init
!----------------------------------------------------------------------------------------------------------------------------------
subroutine BEMT_End( u, p, x, xd, z, OtherState, y, ErrStat, ErrMsg )
! This routine is called at the end of the simulation.
!..................................................................................................................................

      TYPE(BEMT_InputType),           INTENT(INOUT)  :: u           ! System inputs
      TYPE(BEMT_ParameterType),       INTENT(INOUT)  :: p           ! Parameters
      TYPE(BEMT_ContinuousStateType), INTENT(INOUT)  :: x           ! Continuous states
      TYPE(BEMT_DiscreteStateType),   INTENT(INOUT)  :: xd          ! Discrete states
      TYPE(BEMT_ConstraintStateType), INTENT(INOUT)  :: z           ! Constraint states
      TYPE(BEMT_OtherStateType),      INTENT(INOUT)  :: OtherState  ! Other states
      TYPE(BEMT_OutputType),          INTENT(INOUT)  :: y           ! System outputs
      INTEGER(IntKi),               INTENT(  OUT)  :: ErrStat     ! Error status of the operation
      CHARACTER(*),                 INTENT(  OUT)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None



         ! Initialize ErrStat

      ErrStat = ErrID_None
      ErrMsg  = ""


         ! Place any last minute operations or calculations here:


         ! Close files here:



         ! Destroy the input data:

      CALL BEMT_DestroyInput( u, ErrStat, ErrMsg )
      

         ! Destroy the parameter data:

      CALL BEMT_DestroyParam( p, ErrStat, ErrMsg )


         ! Destroy the state data:

      CALL BEMT_DestroyContState(   x,           ErrStat, ErrMsg )
      CALL BEMT_DestroyDiscState(   xd,          ErrStat, ErrMsg )
      CALL BEMT_DestroyConstrState( z,           ErrStat, ErrMsg )
      CALL BEMT_DestroyOtherState(  OtherState,  ErrStat, ErrMsg )


         ! Destroy the output data:

      CALL BEMT_DestroyOutput( y, ErrStat, ErrMsg )

#ifdef UA_OUTS
   CLOSE(69)
#endif


END SUBROUTINE BEMT_End


!----------------------------------------------------------------------------------------------------------------------------------
subroutine BEMT_UpdateStates( t, n, u1, u2,  p, x, xd, z, OtherState, AFInfo, m, errStat, errMsg )
! Loose coupling routine for solving for constraint states, integrating continuous states, and updating discrete states
! Continuous, constraint, discrete, and other states are updated for t + Interval
!
! NOTE:  This is a non-standard framework interface!!!!!  GJH
!..................................................................................................................................

   real(DbKi),                          intent(in   ) :: t          ! Current simulation time in seconds
   integer(IntKi),                      intent(in   ) :: n          ! Current simulation time step n = 0,1,...
   type(BEMT_InputType),                intent(in   ) :: u1,u2      ! Input at t and t+ dt 
   !real(DbKi),                         intent(in   ) :: utime      ! Times associated with u(:), in seconds
   type(BEMT_ParameterType),            intent(in   ) :: p          ! Parameters   
   type(BEMT_ContinuousStateType),      intent(inout) :: x          ! Input: Continuous states at t;
                                                                    !   Output: Continuous states at t + Interval
   type(BEMT_DiscreteStateType),        intent(inout) :: xd         ! Input: Discrete states at t;
                                                                    !   Output: Discrete states at t  + Interval
   type(BEMT_ConstraintStateType),      intent(inout) :: z          ! Input: Constraint states at t;
                                                                    !   Output: Constraint states at t + Interval
   type(BEMT_OtherStateType),           intent(inout) :: OtherState ! Input: Other states at t;
                                                                    !   Output: Other states at t + Interval
   type(BEMT_MiscVarType),              intent(inout) :: m          ! Misc/optimization variables
   type(AFInfoType),                    intent(in   ) :: AFInfo(:)  ! The airfoil parameter data
   integer(IntKi),                      intent(  out) :: errStat    ! Error status of the operation
   character(*),                        intent(  out) :: errMsg     ! Error message if ErrStat /= ErrID_None

      
   integer(IntKi)                                    :: i,j
   type(UA_InputType)                                :: u_UA
   real(ReKi)                                        :: chi, axInduction, tanInduction ! BEMT outputs
   real(ReKi)                                        :: Rtip, Re, Vrel, fzero, phitemp
   logical                                           :: IsValidSolution !< this is set to false if k<=1 in propeller brake region or k<-1 in momentum region, indicating an invalid solution
      
   character(ErrMsgLen)                           :: errMsg2     ! temporary Error message if ErrStat /= ErrID_None
   integer(IntKi)                                 :: errStat2    ! temporary Error status of the operation
   character(*), parameter                        :: RoutineName = 'BEMT_UpdateStates'
   
   character(20)                                  :: NodeTxt
      
   ErrStat = ErrID_None
   ErrMsg = ""
         
   !...............................................................................................................................
   ! if we haven't initialized z%phi, we want to get a better guess as to what the actual values of phi at t are:
   !...............................................................................................................................

   if (.not. OtherState%nodesInitialized) then
      if (p%useInduction) then
         
         do j = 1,p%numBlades ! Loop through all blades
            do i = 1,p%numBladeNodes ! Loop through the blade nodes / elements
               NodeTxt = '(node '//trim(num2lstr(i))//', blade '//trim(num2lstr(j))//')'
               
               call BEMT_UnCoupledSolve(z%phi(i,j), p%numBlades, p%airDens, p%kinVisc, AFInfo(p%AFIndx(i,j)), u1%rlocal(i,j), p%chord(i,j), u1%theta(i,j),  &
                           u1%Vx(i,j), u1%Vy(i,j), p%useTanInd, p%useAIDrag, p%useTIDrag, p%useHubLoss, p%useTipLoss, p%hubLossConst(i,j), p%tipLossConst(i,j), &
                           p%maxIndIterations, p%aTol, OtherState%ValidPhi(i,j), errStat2, errMsg2)                 
                     call SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName//trim(NodeTxt))
                     if (errStat >= AbortErrLev) return 
            end do
         end do
         
      else
                     ! We'll simply compute a geometrical phi based on both induction factors being 0.0
         do j = 1,p%numBlades ! Loop through all blades
            do i = 1,p%numBladeNodes ! Loop through the blade nodes / elements
               z%phi(i,j) = ComputePhiWithInduction(u1%Vx(i,j), u1%Vy(i,j),  0.0_ReKi, 0.0_ReKi)            
            end do
         end do
         
      end if
      OtherState%nodesInitialized = .true.         ! state at t+1
   end if
   
   !...............................................................................................................................
   !  compute states at t+1
   !...............................................................................................................................
   
   
   do j = 1,p%numBlades
            
         ! Locate the maximum rlocal value for this time step and this blade.  This is passed to the solve as Rtip
      if ( p%useInduction ) then
         Rtip = 0.0_ReKi
         do i = 1,p%numBladeNodes
            Rtip = max( Rtip, u1%rlocal(i,j) ) 
         end do
      end if
      
      do i = 1,p%numBladeNodes 

         NodeTxt = '(node '//trim(num2lstr(i))//', blade '//trim(num2lstr(j))//')'
         
            ! We only update the UnsteadyAero states if we have unsteady aero turned on for this node      
         if (OtherState%UA_Flag(i,j) .and. n > 0) then
            
               ! Set the active blade element for UnsteadyAero
            m%UA%iBladeNode = i
            m%UA%iBlade     = j
            
               ! Initialize these variables
            u_UA%alpha   = z%phi(i,j) - u1%theta(i,j)
            axInduction  = 0.0_ReKi
            tanInduction = 0.0_ReKi
            
            
            if ( p%useInduction ) then
               if (OtherState%ValidPhi(i,j)) then
               
                     ! Compute Re based on zero inductions (axInduction, tanInduction), this would needed for the airfoil lookups 
                     !    which occur within BEMTU_InductionWithResidual if we had implemented airfoil tables which are dependent on Reynold's number: Currently unused, 4-Nov-2015
                  call BEMTU_Wind( 0.0_ReKi, 0.0_ReKi, u1%Vx(i,j), u1%Vy(i,j), p%chord(i,j), p%airDens, p%kinVisc, Vrel, Re )
               
                     ! Need to get the induction factors for these conditions without skewed wake correction and without UA
                     ! COMPUTE: axInduction, tanInduction   
                  fzero = BEMTU_InductionWithResidual(z%phi(i,j), u_UA%alpha, Re, p%numBlades, u1%rlocal(i,j), p%chord(i,j), AFInfo(p%AFIndx(i,j)), &
                              u1%Vx(i,j), u1%Vy(i,j), p%useTanInd, p%useAIDrag, p%useTIDrag, p%useHubLoss, p%useTipLoss, p%hubLossConst(i,j), p%tipLossConst(i,j), &
                              axInduction, tanInduction, IsValidSolution, ErrStat2, ErrMsg2)
                  call SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName//trim(NodeTxt))
                  if (errStat >= AbortErrLev) return 
                  
                     ! Apply the skewed wake correction to the axial induction
                  if ( p%skewWakeMod == SkewMod_PittPeters ) then 
                     if ( .not.( p%useTiploss .and. EqualRealNos(p%tipLossConst(i,j),0.0_ReKi) ) .and. .not. ( p%useHubloss .and. EqualRealNos(p%hubLossConst(i,j),0.0_ReKi) ) ) then
                              ! Correct for skewed wake, by recomputing axInduction
                        call ApplySkewedWakeCorrection( u1%Vx(i,j), u1%Vy(i,j), u1%psi(j), u1%chi0, u1%rlocal(i,j)/Rtip, axInduction, tanInduction, chi, ErrStat2, ErrMsg2 ) !replaced phiOut with phitemp  RRD
                           ! ApplySkewedWakeCorrection doesn't produce errors
                     end if   
                  end if
                  
                  ! recompute phi and alpha
                  
                     ! we're recomputing phi because we may have modified axInduction or tanInduction in BEMTU_InductionWithResidual and/or ApplySkewedWakeCorrection
                  phitemp = ComputePhiWithInduction( u1%Vx(i,j), u1%Vy(i,j),  axInduction, tanInduction )  
                        ! angle of attack
                  u_UA%alpha = phitemp - u1%theta(i,j)
                  
               else
                  axInduction  = 0.0_ReKi
                  tanInduction = 0.0_ReKi
                  phitemp = ComputePhiWithInduction( u1%Vx(i,j), u1%Vy(i,j),  axInduction, tanInduction )  
                     ! angle of attack
                  u_UA%alpha = phitemp - u1%theta(i,j)
                  
               end if
            end if
                  
            
               ! Need to compute local velocity including both axial and tangential induction
               ! COMPUTE: u_UA%U, u_UA%Re
            call BEMTU_Wind( axInduction, tanInduction, u1%Vx(i,j), u1%Vy(i,j), p%chord(i,j), p%airDens, p%kinVisc, u_UA%U, u_UA%Re) !replaced phiOut with phitemp  RRD

            if ( abs(u_UA%alpha) >= AFInfo(p%AFIndx(i,j))%Table(1)%UA_BL%UACutout ) then  ! Is the angle of attack larger than the UA cut-out for this airfoil?
               OtherState%UA_Flag(i,j) = .FALSE.
               call WrScr( 'Warning: Turning off Unsteady Aerodynamics due to high angle-of-attack. BladeNode = '//trim(num2lstr(i))//', Blade = '//trim(num2lstr(j)) )
            elseif (EqualRealNos(u_UA%U, 0.0_ReKi) ) then
               OtherState%UA_Flag(i,j) = .FALSE.
               call WrScr( 'Warning: Turning off Unsteady Aerodynamics due to zero relative velocity. BladeNode = '//trim(num2lstr(i))//', Blade = '//trim(num2lstr(j)) )
                  
            else    
                  ! COMPUTE: xd%UA, OtherState%UA
               call UA_UpdateStates( i, j, u_UA, p%UA, xd%UA, OtherState%UA, AFInfo(p%AFIndx(i,j)), m%UA, errStat2, errMsg2 )
                  call SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName//trim(NodeTxt))
                  if (errStat >= AbortErrLev) return 
            end if
            
         end if      ! if (OtherState%UA_Flag(i,j)) then
               
            ! Now we need to update the inflow angle state, z%phi, independent of UnsteadyAero
         if ( p%useInduction ) then
               ! Solve this without any skewed wake correction and without UA
            ! call BEMT_UnCoupledSolve2(i, j, u, p, z, Rtip, AFInfo, phiOut, axIndOut, tanIndOut, errStat, errMsg)
               ! COMPUTE:  z%phi(i,j)     
            call BEMT_UnCoupledSolve(z%phi(i,j), p%numBlades, p%airDens, p%kinVisc, AFInfo(p%AFIndx(i,j)), u2%rlocal(i,j), p%chord(i,j), u2%theta(i,j),  &
                        u2%Vx(i,j), u2%Vy(i,j), p%useTanInd, p%useAIDrag, p%useTIDrag, p%useHubLoss, p%useTipLoss, p%hubLossConst(i,j), p%tipLossConst(i,j), &
                        p%maxIndIterations, p%aTol, OtherState%ValidPhi(i,j), errStat2, errMsg2)  
                  call SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName//trim(NodeTxt))
                  if (errStat >= AbortErrLev) return 
                  
         else
               ! We'll simply compute a geometrical phi based on both induction factors being 0.0
            z%phi(i,j) = ComputePhiWithInduction(u2%Vx(i,j), u2%Vy(i,j),  0.0_ReKi, 0.0_ReKi)            
         end if
    
      end do  
   end do
   
end subroutine BEMT_UpdateStates


!----------------------------------------------------------------------------------------------------------------------------------
subroutine BEMT_CalcOutput( t, u, p, x, xd, z, OtherState, AFInfo, y, m, errStat, errMsg )
! Routine for computing outputs, used in both loose and tight coupling.
! This SUBROUTINE is used to compute the output channels (motions and loads) and place them in the WriteOutput() array.
! NOTE: the descriptions of the output channels are not given here. Please see the included OutListParameters.xlsx sheet for
! for a complete description of each output parameter.
! NOTE: no matter how many channels are selected for output, all of the outputs are calcalated
! All of the calculated output channels are placed into the OtherState%AllOuts(:), while the channels selected for outputs are
! placed in the y%WriteOutput(:) array.
!..................................................................................................................................

   
   real(DbKi),                     intent(in   )  :: t           ! Current simulation time in seconds
   type(BEMT_InputType),           intent(in   )  :: u           ! Inputs at Time t
   type(BEMT_ParameterType),       intent(in   )  :: p           ! Parameters
   type(BEMT_ContinuousStateType), intent(in   )  :: x           ! Continuous states at t
   type(BEMT_DiscreteStateType),   intent(in   )  :: xd          ! Discrete states at t
   type(BEMT_ConstraintStateType), intent(in   )  :: z           ! Constraint states at t
   type(BEMT_OtherStateType),      intent(in   )  :: OtherState  ! Other states at t
   type(BEMT_MiscVarType),         intent(inout)  :: m           ! Misc/optimization variables
   type(AFInfoType),               intent(in   )  :: AFInfo(:)   ! The airfoil parameter data
   type(BEMT_OutputType),          intent(inout)  :: y           ! Outputs computed at t (Input only so that mesh con-
                                                                 !   nectivity information does not have to be recalculated)
   integer(IntKi),                 intent(  out)  :: errStat     ! Error status of the operation
   character(*),                   intent(  out)  :: errMsg      ! Error message if ErrStat /= ErrID_None


      ! Local variables:

  
   real(ReKi)                     :: Re, fzero
   real(ReKi)                     :: Rtip ! maximum rlocal value for node j over all blades

   integer(IntKi)                 :: i                                               ! Generic index
   integer(IntKi)                 :: j                                               ! Loops through nodes / elements
   
   character(ErrMsgLen)                           :: errMsg2     ! temporary Error message if ErrStat /= ErrID_None
   integer(IntKi)                                 :: errStat2    ! temporary Error status of the operation
   character(*), parameter                        :: RoutineName = 'BEMT_CalcOutput'
   
   character(20)                                  :: NodeTxt
   
   
#ifdef UA_OUTS
   integer(IntKi)                 :: k
#endif

   logical, parameter             :: UpdateValues  = .TRUE.                          ! determines if the OtherState values need to be updated
   type(BEMT_ContinuousStateType) :: dxdt                                            ! Continuous state derivs at t
   real(ReKi)                     :: Vrel
   logical                        :: IsValidSolution !< this is set to false if k<=1 in propeller brake region or k<-1 in momentum region, indicating an invalid solution
         ! Initialize some output values
   errStat = ErrID_None
   errMsg  = ""
           
   
   !...............................................................................................................................
   ! if we haven't initialized z%phi, we want to get a better guess as to what the actual values of phi are:
   !...............................................................................................................................

   if (OtherState%nodesInitialized) then
      y%phi = z%phi
   else
      if (p%useInduction) then
         
         do j = 1,p%numBlades ! Loop through all blades
            do i = 1,p%numBladeNodes ! Loop through the blade nodes / elements
               NodeTxt = '(node '//trim(num2lstr(i))//', blade '//trim(num2lstr(j))//')'
               
               y%phi(i,j) = z%phi(i,j)
               call BEMT_UnCoupledSolve(y%phi(i,j), p%numBlades, p%airDens, p%kinVisc, AFInfo(p%AFIndx(i,j)), u%rlocal(i,j), p%chord(i,j), u%theta(i,j),  &
                           u%Vx(i,j), u%Vy(i,j), p%useTanInd, p%useAIDrag, p%useTIDrag, p%useHubLoss, p%useTipLoss, p%hubLossConst(i,j), p%tipLossConst(i,j), &
                           p%maxIndIterations, p%aTol, IsValidSolution, errStat2, errMsg2)                 
                     call SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName//trim(NodeTxt))
                     if (errStat >= AbortErrLev) return 
            end do
         end do
         
      else
                     ! We'll simply compute a geometrical phi based on both induction factors being 0.0
         do j = 1,p%numBlades ! Loop through all blades
            do i = 1,p%numBladeNodes ! Loop through the blade nodes / elements
               y%phi(i,j) = ComputePhiWithInduction(u%Vx(i,j), u%Vy(i,j),  0.0_ReKi, 0.0_ReKi)            
            end do
         end do
         
      end if
   end if
   
   ! Array OtherState%AllOuts() is initialized to 0.0 in initialization, so we are not going to reinitialize it here.
   
      
   !...............................................................................................................................
   ! Calculate all of the total forces and moments using all of the partial forces and moments calculated in RtHS().  Also,
   !   calculate all of the total angular and linear accelerations using all of the partial accelerations calculated in RtHS().
   !   To do this, first initialize the variables using the portions not associated with the accelerations.  Then add the portions
   !   associated with the accelerations one by one:
   !...............................................................................................................................  
   
   do j = 1,p%numBlades ! Loop through all blades
      
         ! Locate the maximum rlocal value for this time step and this blade.  This is passed to the solve as Rtip
      Rtip = 0.0_ReKi
      do i = 1,p%numBladeNodes
         Rtip = max( Rtip, u%rlocal(i,j) ) 
      end do
            
      do i = 1,p%numBladeNodes ! Loop through the blade nodes / elements
         
         NodeTxt = '(node '//trim(num2lstr(i))//', blade '//trim(num2lstr(j))//')'
         
            ! Set the active blade element for UnsteadyAero
         m%UA%iBladeNode = i
         m%UA%iBlade     = j
         
            ! Copy the current state to the outputs structure
         !y%phi(i,j) = z%phi(i,j)     ! this was done above (to take possible initializion into account)
            ! angle of attack
         y%AOA(i,j) = y%phi(i,j) - u%theta(i,j)
         
            ! initialize other outputs assuming no induction or skewed wake corrections
         y%axInduction(i,j)  = 0.0_ReKi
         y%tanInduction(i,j) = 0.0_ReKi             
         y%chi(i,j) = u%chi0   ! with no induction, chi = chi0
            
         if ( p%useInduction ) then
            
            if (m%UseFrozenWake) then
               y%axInduction(i,j)  = -m%AxInd_op(i,j) / u%Vx(i,j)               
               y%tanInduction(i,j) =  m%TnInd_op(i,j) / u%Vy(i,j)
               y%phi(i,j) = ComputePhiWithInduction( u%Vx(i,j), u%Vy(i,j),  y%axInduction(i,j), y%tanInduction(i,j) )  
               y%AOA(i,j) = y%phi(i,j) - u%theta(i,j)               
            else
               if (OtherState%ValidPhi(i,j)) then               
                     ! Compute Re based on zero inductions (axInduction, tanInduction), this would needed for the airfoil lookups 
                     !    which occur within BEMTU_InductionWithResidual if we had implemented airfoil tables which are dependent on Reynold's number: Currently unused, 4-Nov-2015
                  call BEMTU_Wind( 0.0_ReKi, 0.0_ReKi, u%Vx(i,j), u%Vy(i,j), p%chord(i,j), p%airDens, p%kinVisc, Vrel, Re )
            
                     ! Compute inductions using steady aero.  NOTE: When we use Re, we are using uninduced Re for Steady Airfoil Coef looks here
                  fzero = BEMTU_InductionWithResidual(y%phi(i,j), y%AOA(i,j), Re, p%numBlades, u%rlocal(i,j), p%chord(i,j), AFInfo(p%AFindx(i,j)), &
                                 u%Vx(i,j), u%Vy(i,j), p%useTanInd, p%useAIDrag, p%useTIDrag, p%useHubLoss, p%useTipLoss, p%hubLossConst(i,j), p%tipLossConst(i,j), &
                                 y%axInduction(i,j), y%tanInduction(i,j), IsValidSolution, errStat2, errMsg2)
                     call SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName//trim(NodeTxt))
                     if (errStat >= AbortErrLev) return 
                               
                  ! Apply the skewed wake correction to the axial induction (y%axInduction)
                  if ( p%skewWakeMod == SkewMod_PittPeters ) then
                     if ( .not.( p%useTiploss .and. EqualRealNos(p%tipLossConst(i,j),0.0_ReKi) ) .and. .not. ( p%useHubloss .and. EqualRealNos(p%hubLossConst(i,j),0.0_ReKi) ) ) then
                        call ApplySkewedWakeCorrection( u%Vx(i,j), u%Vy(i,j), u%psi(j), u%chi0, u%rlocal(i,j)/Rtip, y%axInduction(i,j), y%tanInduction(i,j), y%chi(i,j), ErrStat2, ErrMsg2 )           
                           ! ApplySkewedWakeCorrection doesn't set errors
                     end if
                  end if

                     ! recompute y%phi, y%AOA:
                     ! we're recomputing phi because we may have modified axInduction or tanInduction in BEMTU_InductionWithResidual and/or ApplySkewedWakeCorrection
                  y%phi(i,j) = ComputePhiWithInduction( u%Vx(i,j), u%Vy(i,j),  y%axInduction(i,j), y%tanInduction(i,j) )
                        ! angle of attack
                  y%AOA(i,j) = y%phi(i,j) - u%theta(i,j)
               end if ! ValidPhi
                  
            end if ! UseFrozenWake
            
         end if        
            
            ! Compute Re, Vrel based on current values of axInduction, tanInduction
         call BEMTU_Wind( y%axInduction(i,j), y%tanInduction(i,j), u%Vx(i,j), u%Vy(i,j), p%chord(i,j), p%airDens, p%kinVisc, y%Vrel(I,J), y%Re(i,j) )
  
            ! Now depending on the option for UA get the airfoil coefs, Cl, Cd, Cm for unsteady or steady implementation
         if (OtherState%UA_Flag(i,j) .and. t > 0.0_DbKi) then            
            call Compute_UA_AirfoilCoefs( y%AOA(i,j), y%Vrel(I,J), y%Re(i,j),  AFInfo(p%AFindx(i,j)), p%UA, xd%UA, OtherState%UA, m%y_UA, m%UA, &
                                         y%Cl(i,j), y%Cd(i,j), y%Cm(i,j), errStat2, errMsg2 ) 
               call SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName//trim(NodeTxt))
               if (errStat >= AbortErrLev) return 
               
            ! NOTE: In this version, we are disabling the UA Cm values only we have performed further validation.  GJH 3/9/2016
               ! Obtain the steady state Cm value from the static tables
            !call ComputeSteadyAirfoilCoefs( y%AOA(i,j), y%Re(i,j),  AFInfo(p%AFindx(i,j)), &
            !                             localCl, localCd, y%Cm(i,j), errStat2, errMsg2 ) 
            !   call SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName//trim(NodeTxt))
            !   if (errStat >= AbortErrLev) return 

         else 
               ! TODO: When we start using Re, should we use the uninduced Re since we used uninduced Re to solve for the inductions!? Probably this won't change, instead create a Re loop up above.
            call ComputeSteadyAirfoilCoefs( y%AOA(i,j), y%Re(i,j),  AFInfo(p%AFindx(i,j)), &
                                         y%Cl(i,j), y%Cd(i,j), y%Cm(i,j), errStat2, errMsg2 ) 
               call SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName//trim(NodeTxt))
               if (errStat >= AbortErrLev) return 
         end if

         
            ! Compute Cx, Cy given Cl, Cd and phi
            ! NOTE: For these calculations we force the useAIDrag and useTIDrag flags to .TRUE.
         call Transform_ClCd_to_CxCy( y%phi(i,j), .TRUE., .TRUE., y%Cl(i,j), y%Cd(i,j),y%Cx(i,j), y%Cy(i,j) )
            
      enddo             ! I - Blade nodes / elements

   enddo          ! J - All blades

#ifdef UA_OUTS
  ! if ( mod(REAL(t,ReKi),.1) < p%dt) then
            WRITE (69, '(F9.4,'//trim(num2lstr(p%numBladeNodes*p%numBlades*p%UA%NumOuts))//'(:,A,ES12.3E3))') t, (' ', m%y_UA%WriteOutput(k), k=1,p%UA%NumOuts*p%numBladeNodes*p%numBlades)
  ! end if
     !WRITE (69, '((F8.3,'//TRIM(num2lstr(13*p%numBladeNodes*p%numBlades))//'(:,A,ES10.3E2))')
            !Frmt = '(F8.3,'//TRIM(Int2LStr(p%WAMIT%NumOuts+p%Morison%NumOuts))//'(:,A,'//TRIM( p%OutFmt )//'))'
   !Frmt = '('ES10.3E2,'//TRIM(13*p%numBladeNodes*p%numBlades)//'(:,A,'ES10.3E2'))'
   
           
#endif 
   
   !...............................................................................................................................
   ! Place the selected output channels into the WriteOutput(:) array with the proper sign:
   !...............................................................................................................................

   call BEMT_MapOutputs(p, OtherState, y, errStat2, errMsg2)
      call SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)
      if (errStat >= AbortErrLev) return 
   
   !DO I = 1,p%NumOuts  ! Loop through all selected output channels
   !
   !   y%WriteOutput(I) = p%OutParam(I)%SignM * OtherState%AllOuts( p%OutParam(I)%Indx )
   !
   !ENDDO             ! I - All selected output channels

   
   !...............................................................................................................................
   ! Outputs required for AeroDyn
   !...............................................................................................................................
  
   !...........
   ! Blade elements:
   !...........
   
               
   return
   

end subroutine BEMT_CalcOutput


!----------------------------------------------------------------------------------------------------------------------------------
subroutine BEMT_CalcContStateDeriv( t, u, p, x, xd, z, OtherState, m, dxdt, ErrStat, ErrMsg )
! Tight coupling routine for computing derivatives of continuous states
!..................................................................................................................................

   REAL(DbKi),                     INTENT(IN   )  :: t           ! Current simulation time in seconds
   TYPE(BEMT_InputType),           INTENT(IN   )  :: u           ! Inputs at t
   TYPE(BEMT_ParameterType),       INTENT(IN   )  :: p           ! Parameters
   TYPE(BEMT_ContinuousStateType), INTENT(IN   )  :: x           ! Continuous states at t
   TYPE(BEMT_DiscreteStateType),   INTENT(IN   )  :: xd          ! Discrete states at t
   TYPE(BEMT_ConstraintStateType), INTENT(IN   )  :: z           ! Constraint states at t
   TYPE(BEMT_OtherStateType),      INTENT(IN   )  :: OtherState  ! Other states at t
   type(BEMT_MiscVarType),         intent(inout)  :: m           ! Misc/optimization variables
   TYPE(BEMT_ContinuousStateType), INTENT(  OUT)  :: dxdt        ! Continuous state derivatives at t
   INTEGER(IntKi),                 INTENT(  OUT)  :: ErrStat     ! Error status of the operation
   CHARACTER(*),                   INTENT(  OUT)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None

  

      ! Initialize ErrStat

   ErrStat = ErrID_None
   ErrMsg  = ""

   dxdt%DummyContState = 0.0_ReKi
      
   
END SUBROUTINE BEMT_CalcContStateDeriv
!----------------------------------------------------------------------------------------------------------------------------------
subroutine BEMT_UpdateDiscState( t, n, u, p, x, xd, z, OtherState, m, ErrStat, ErrMsg )
! Tight coupling routine for updating discrete states
!..................................................................................................................................

   REAL(DbKi),                     INTENT(IN   )  :: t           ! Current simulation time in seconds
   INTEGER(IntKi),                 INTENT(IN   )  :: n           ! Current step of the simulation: t = n*Interval
   TYPE(BEMT_InputType),           INTENT(IN   )  :: u           ! Inputs at t
   TYPE(BEMT_ParameterType),       INTENT(IN   )  :: p           ! Parameters
   TYPE(BEMT_ContinuousStateType), INTENT(IN   )  :: x           ! Continuous states at t
   TYPE(BEMT_DiscreteStateType),   INTENT(INOUT)  :: xd          ! Input: Discrete states at t;
                                                                 !   Output: Discrete states at t + Interval
   TYPE(BEMT_ConstraintStateType), INTENT(IN   )  :: z           ! Constraint states at t
   TYPE(BEMT_OtherStateType),      INTENT(IN   )  :: OtherState  ! Other states at t
   type(BEMT_MiscVarType),         intent(inout)  :: m           ! Misc/optimization variables
   INTEGER(IntKi),                 INTENT(  OUT)  :: ErrStat     ! Error status of the operation
   CHARACTER(*),                   INTENT(  OUT)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None


         ! Initialize ErrStat

      ErrStat = ErrID_None
      ErrMsg  = ""


         ! Update discrete states here:

      ! StateData%DiscState =

END SUBROUTINE BEMT_UpdateDiscState
!----------------------------------------------------------------------------------------------------------------------------------
subroutine BEMT_CalcConstrStateResidual( Time, u, p, x, xd, z, OtherState, m, z_residual, AFInfo, ErrStat, ErrMsg )
! Tight coupling routine for solving for the residual of the constraint state equations
!..................................................................................................................................

   real(DbKi),                     intent(in   )  :: Time        ! Current simulation time in seconds
   type(BEMT_InputType),           intent(in   )  :: u           ! Inputs at Time
   type(BEMT_ParameterType),       intent(in   )  :: p           ! Parameters
   type(BEMT_ContinuousStateType), intent(in   )  :: x           ! Continuous states at Time
   type(BEMT_DiscreteStateType),   intent(in   )  :: xd          ! Discrete states at Time
   type(BEMT_ConstraintStateType), intent(in   )  :: z           ! Constraint states at Time (possibly a guess)
   type(BEMT_OtherStateType),      intent(in   )  :: OtherState  ! Other states at Time
   type(BEMT_MiscVarType),         intent(inout)  :: m           ! Misc/optimization variables
   type(BEMT_ConstraintStateType), intent(inout)  :: z_residual  ! Residual of the constraint state equations using
                                                                 !     the input values described above
   type(AFInfoType),               intent(in   )  :: AFInfo(:)   ! The airfoil parameter data
   integer(IntKi),                 intent(  out)  :: ErrStat     ! Error status of the operation
   character(*),                   intent(  out)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None


      !# set epsilon
   !REAL(ReKi), PARAMETER     ::epsilon = 1e-6
   
      ! Local variables
   INTEGER    :: i,j
   real(ReKi)  axInduction, tanInduction, Vrel, Re
   character(ErrMsgLen)                           :: errMsg2     ! temporary Error message if ErrStat /= ErrID_None
   integer(IntKi)                                 :: errStat2    ! temporary Error status of the operation
   character(*), parameter                        :: RoutineName = 'BEMT_CalcConstrStateResidual'
   logical                                        :: IsValidSolution ! placeholder for flag to determine if the residual solution is invalid
  
   
   ErrStat = ErrID_None
   ErrMsg  = ""
   
   
   if (p%useInduction) then 
      
      if ( m%UseFrozenWake ) then ! we are linearizing with frozen wake assumption; i.e., p%FrozenWake is true and this was called from the linearization routine
         do j = 1,p%numBlades            
            do i = 1,p%numBladeNodes
               Z_residual%phi(i,j) = sin(z%phi(i,j)) * (u%Vy(i,j) + m%TnInd_op(i,j)) - cos(z%phi(i,j)) * (u%Vx(i,j) + m%AxInd_op(i,j))
            end do
         end do
         
      else
            
         do j = 1,p%numBlades            
            do i = 1,p%numBladeNodes
         
                  ! Need to initialize the inductions to zero for this calculation (NOTE: They are actually computed within UnCpldReFn(), but still need to be intialized to zero!)
               axInduction  = 0.0_ReKi
               tanInduction = 0.0_ReKi
            
                  ! Need to call BEMTU_Wind to obtain Re, even though we aren't using it in this version.
               call BEMTU_Wind( axInduction, tanInduction, u%Vx(i,j), u%Vy(i,j), p%chord(i,j), p%airDens, p%kinVisc, Vrel, Re )
            
                  ! Solve for the constraint states here:
               Z_residual%phi(i,j) = UncoupledErrFn(z%phi(i,j), u%theta(i,j), Re, p%numBlades, u%rlocal(i,j), p%chord(i,j),  AFInfo(p%AFindx(i,j)), &
                                    u%Vx(i,j), u%Vy(i,j), p%useTanInd, p%useAIDrag, p%useTIDrag, p%useHubLoss, p%useTipLoss, p%hubLossConst(i,j), p%tipLossConst(i,j), &
                                    IsValidSolution, ErrStat2, ErrMsg2)
               call SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)
               if (ErrStat >= AbortErrLev) return                  
           
            end do
         end do
         
      end if ! not frozen wake
      
   else
      
      do j = 1,p%numBlades            
         do i = 1,p%numBladeNodes     
            Z_residual%Phi(i,j) = sin(z%phi(i,j)) * u%Vy(i,j) - cos(z%phi(i,j)) * u%Vx(i,j)
         end do
      end do
      

   end if
   
   
END SUBROUTINE BEMT_CalcConstrStateResidual

!----------------------------------------------------------------------------------------------------------------------------------
!> This subroutine computes the BEMT inductions that are frozen when linearizing with the FrozenWake flag.
!> It first calls BEMT_CalcOutput to compute y%tanInduction and y%axInduction at this operating point.
!SUBROUTINE computeFrozenWake( t, u, p, x, xd, z, OtherState, AFInfo, y, m, errStat, errMsg )
SUBROUTINE computeFrozenWake( u, p, y, m )
!..................................................................................................................................

   
   !real(DbKi),                     intent(in   )  :: t           ! Current simulation time in seconds
   type(BEMT_InputType),           intent(in   )  :: u           ! Inputs at Time t
   type(BEMT_ParameterType),       intent(in   )  :: p           ! Parameters
   !type(BEMT_ContinuousStateType), intent(in   )  :: x           ! Continuous states at t
   !type(BEMT_DiscreteStateType),   intent(in   )  :: xd          ! Discrete states at t
   !type(BEMT_ConstraintStateType), intent(in   )  :: z           ! Constraint states at t
   !type(BEMT_OtherStateType),      intent(in   )  :: OtherState  ! Other states at t
   type(BEMT_MiscVarType),         intent(inout)  :: m           ! Misc/optimization variables
   !type(AFInfoType),               intent(in   )  :: AFInfo(:)   ! The airfoil parameter data
   type(BEMT_OutputType),          intent(inout)  :: y           ! Outputs computed at t (Input only so that mesh con-
                                                                 !   nectivity information does not have to be recalculated)
   !integer(IntKi),                 intent(  out)  :: errStat     ! Error status of the operation
   !character(*),                   intent(  out)  :: errMsg      ! Error message if ErrStat /= ErrID_None


      ! local variables
   INTEGER(IntKi)                                 :: j,k  ! loop counters
   character(*), parameter                        :: RoutineName = 'computeFrozenWake'
   
      ! get a and aprime            
   !call BEMT_CalcOutput(t, u, p, x, xd, z, OtherState, AFInfo, y, m, errStat, errMsg)
      
   do k = 1,p%numBlades            
      do j = 1,p%numBladeNodes
            
         m%AxInd_op(j,k) = - u%Vx(j,k) * y%axInduction( j,k)
         m%TnInd_op(j,k) =   u%Vy(j,k) * y%tanInduction(j,k)
            
      end do
   end do
      
      
     
END SUBROUTINE computeFrozenWake
!----------------------------------------------------------------------------------------------------------------------------------
!> This subroutine checks for BEMT inputs that are invalid when linearizing constraint state equations.
SUBROUTINE CheckLinearizationInput(p, u, z, m, OtherState, ErrStat, ErrMsg)

   type(BEMT_ParameterType),       intent(in   )  :: p           ! Parameters
   type(BEMT_InputType),           intent(in   )  :: u           ! Inputs at the operating point
   type(BEMT_ConstraintStateType), intent(in   )  :: z           ! Constraint states at the operating point
   type(BEMT_MiscVarType),         intent(in   )  :: m           ! Misc/optimization variables
   type(BEMT_OtherStateType),      intent(in   )  :: OtherState  ! Other state at the operating point
   integer(IntKi),                 intent(  out)  :: ErrStat     ! Error status of the operation
   character(*),                   intent(  out)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None

      ! local variables
   INTEGER(IntKi)                                 :: j,k  ! loop counters
   character(*), parameter                        :: RoutineName = 'CheckLinearizationInput'
   
   
   ErrStat = ErrID_None
   ErrMsg  = ''
   
   if (p%UseInduction) then
      
      do k = 1,p%numBlades            
         do j = 1,p%numBladeNodes
            if (.not. OtherState%ValidPhi(j,k)) then
               call SetErrSTat(ErrID_Fatal,"Blade"//trim(num2lstr(k))//', node '//trim(num2lstr(k))//&
                      ": Current operating point does not contain a valid value of phi.",ErrStat,ErrMsg,RoutineName)
               return
            end if
            
         end do
      end do
         
      

      if ( m%UseFrozenWake )  then ! we are linearizing with frozen wake assumption (i.e., p%FrozenWake is true and this is called from linearization routine)
      
         do k = 1,p%numBlades            
            do j = 1,p%numBladeNodes
            
               if ( VelocityIsZero( u%Vy(j,k)+m%TnInd_op(j,k)) .and. VelocityIsZero( u%Vx(j,k)+m%AxInd_op(j,k) ) ) then
                  call SetErrStat(ErrID_Fatal,"Blade"//trim(num2lstr(k))//', node '//trim(num2lstr(k))//&
                      ": residual is undefined because u%Vy + TnInd_op = u%Vx + AxInd_op = 0.",ErrStat,ErrMsg,RoutineName)
                  return
               end if
            
            end do
         end do
      
      else
      
         do k = 1,p%numBlades            
            do j = 1,p%numBladeNodes
            
               if ( EqualRealNos(z%phi(j,k), 0.0_ReKi) ) then
                  call SetErrStat(ErrID_Fatal,"Blade"//trim(num2lstr(k))//', node '//trim(num2lstr(k))//&
                       ": residual is discontinuous or undefined because z%phi = 0.",ErrStat,ErrMsg,RoutineName)
                  return
               else if ( VelocityIsZero(u%Vy(j,k)) ) then
                  call SetErrStat(ErrID_Fatal,"Blade"//trim(num2lstr(k))//', node '//trim(num2lstr(k))//&
                       ": residual is discontinuous or undefined because u%Vy = 0.",ErrStat,ErrMsg,RoutineName)
                  return
               else if ( VelocityIsZero(u%Vx(j,k)) ) then
                  call SetErrStat(ErrID_Fatal,"Blade"//trim(num2lstr(k))//', node '//trim(num2lstr(k))//&
                       ": residual is discontinuous or undefined because u%Vx = 0.",ErrStat,ErrMsg,RoutineName)
                  return
               end if
                           
            end do
         end do
               
      end if      
      
      
            
   else ! .not. p%UseInduction:
      
      do k = 1,p%numBlades            
         do j = 1,p%numBladeNodes
            
            if ( EqualRealNos( u%Vy(j,k), 0.0_ReKi ) .and. EqualRealNos( u%Vx(j,k), 0.0_ReKi ) ) then
               call SetErrStat(ErrID_Fatal,"Blade"//trim(num2lstr(k))//', node '//trim(num2lstr(k))//&
                    ": residual is undefined because u%Vy = u%Vx = 0.",ErrStat,ErrMsg,RoutineName)
               return
            end if
            
         end do
      end do      
      

                  
   end if
                  
END SUBROUTINE CheckLinearizationInput
!----------------------------------------------------------------------------------------------------------------------------------
!> This gets the constraint-state perturbations for linearization about a given operating point. This returns two deltas (one plus 
!! and one minus) such that z_op+dz_p and z_op-dz_m are in the same solution region (or that they are in adjacent regions that have 
!! continuous solution regions [i.e, the pi/2 boundary is okay because it is continuous across the momentum/empirical regions]).
SUBROUTINE Get_phi_perturbations(p, m, z_op, dz_p, dz_m)

   type(BEMT_ParameterType),       intent(in   )  :: p           ! Parameters
   type(BEMT_MiscVarType),         intent(in   )  :: m           ! Misc/optimization variables

   REAL(ReKi), intent(in    ) :: z_op     !< value of z%phi(i,j) at the operating point
   REAL(ReKi), intent(   out) :: dz_p     !< change in z_op in the plus direction 
   REAL(ReKi), intent(   out) :: dz_m     !< change in z_op in the minus direction

      ! local variables
   real(ReKi)                 :: dz         ! size of perturbation
   real(ReKi)                 :: zp         ! z_op+dz
   real(ReKi)                 :: zm         ! z_op-dz

   
   dz = 2*D2R
   
      ! we'll assume a central difference unless we are on the boundaries below  [default]
   dz_p = dz
   dz_m = dz
   
   
   if (p%UseInduction .and. .not. m%UseFrozenWake) then
   
      zp = z_op+dz
      zm = z_op-dz
   
         ! check if it goes past the pi-eps upper boundary
      if ( zp > pi - BEMT_epsilon2 ) then
         ! 1-sided difference
         dz_p = 0
         dz_m = dz
      
         ! next we care about the -pi/4-eps boundary:
      else if ( zm < -pi/4 - BEMT_epsilon2) then
         ! 1-sided difference
         dz_p = dz
         dz_m = 0
      
         ! next let's check the +eps boundaries:
      else if ( z_op > 0.0_ReKi .and. zm < BEMT_epsilon2 ) then
         ! 1-sided difference
         dz_p = dz
         dz_m = 0
      
         ! next let's check the -eps boundaries:
      else if ( z_op < 0.0_ReKi .and. zp > -BEMT_epsilon2 ) then
         ! 1-sided difference
         dz_p = 0
         dz_m = dz
      
      ! else ! we don't care about the pi/2 boundary, so let's do a central difference for everything else
      end if
   
   end if
      
   
END SUBROUTINE Get_phi_perturbations
!----------------------------------------------------------------------------------------------------------------------------------
subroutine GetSolveRegionOrdering(Vx, phiIn, test_lower, test_upper)
   real(ReKi),             intent(in   ) :: Vx
   real(ReKi),             intent(in   ) :: phiIn
   real(ReKi),             intent(  out) :: test_lower(3)
   real(ReKi),             intent(  out) :: test_upper(3)


   if (Vx > 0) then
   
      test_lower(1) = BEMT_epsilon2
      test_upper(1) = PiBy2 - BEMT_epsilon2
   
      if (phiIn < pi/4.0_ReKi  .and. phiIn > -pi/4.0_ReKi) then !bjj: added the negative for those cases where the previously calculated non-BEMT phi is in the [-pi,-pi/4] range
         test_lower(2) = -pi/4.0_ReKi
         test_upper(2) = -BEMT_epsilon2

         test_lower(3) = PiBy2 + BEMT_epsilon2
         test_upper(3) = pi - BEMT_epsilon2
      else
         test_lower(3) = -pi/4.0_ReKi
         test_upper(3) = -BEMT_epsilon2

         test_lower(2) = PiBy2 + BEMT_epsilon2
         test_upper(2) = pi - BEMT_epsilon2
      end if
      
   else
      
      test_lower(1) = -BEMT_epsilon2
      test_upper(1) = -PiBy2 + BEMT_epsilon2
   
      if (phiIn > -pi/4.0_ReKi  .and. phiIn < pi/4.0_ReKi) then !bjj: added the negative for those cases where the previously calculated non-BEMT phi is in the [-pi,-pi/4] range
         test_lower(2) = pi/4.0_ReKi
         test_upper(2) = BEMT_epsilon2

         test_lower(3) = -PiBy2 - BEMT_epsilon2
         test_upper(3) = -pi + BEMT_epsilon2
      else
         test_lower(3) = pi/4.0_ReKi
         test_upper(3) = BEMT_epsilon2

         test_lower(2) = -PiBy2 - BEMT_epsilon2
         test_upper(2) = -pi + BEMT_epsilon2
      end if

   end if
   
   
end subroutine GetSolveRegionOrdering
   
integer function TestRegion(phiLower, phiUpper, numBlades, rlocal, chord, theta, AFInfo, &
                        Vx, Vy, Re, useTanInd, useAIDrag, useTIDrag, useHubLoss, useTipLoss,  hubLossConst, tipLossConst,  atol, &
                        f1, f2, errStat, errMsg)

   real(ReKi),             intent(in   ) :: phiLower
   real(ReKi),             intent(in   ) :: phiUpper
   integer,                intent(in   ) :: numBlades
   !integer,                intent(in   ) :: numBladeNodes
   type(AFInfoType),       intent(in   ) :: AFInfo
   real(ReKi),             intent(in   ) :: rlocal                    
   real(ReKi),             intent(in   ) :: chord          
   real(ReKi),             intent(in   ) :: theta           
   real(ReKi),             intent(in   ) :: Vx             
   real(ReKi),             intent(in   ) :: Vy             
   real(ReKi),             intent(in   ) :: Re            
   logical,                intent(in   ) :: useTanInd 
   logical,                intent(in   ) :: useAIDrag
   logical,                intent(in   ) :: useTIDrag
   logical,                intent(in   ) :: useHubLoss
   logical,                intent(in   ) :: useTipLoss
   real(ReKi),             intent(in   ) :: hubLossConst
   real(ReKi),             intent(in   ) :: tipLossConst
   real(ReKi),             intent(in   ) :: atol
   real(ReKi),             intent(  out) :: f1 !< value of residual at phiLower
   real(ReKi),             intent(  out) :: f2 !< value of residual at phiUpper
   integer(IntKi),         intent(  out) :: ErrStat       ! Error status of the operation
   character(*),           intent(  out) :: ErrMsg        ! Error message if ErrStat /= ErrID_None
   
      ! Local variables  
   character(errMsgLen)                  :: errMsg2                 ! temporary Error message if ErrStat /= ErrID_None
   integer(IntKi)                        :: errStat2                ! temporary Error status of the operation
   character(*), parameter               :: RoutineName='TestRegion'
   logical                               :: IsValidSolution, IsValidSolution2 ! placeholder for flag to determine if the residual solution is invalid (we'll handle that after the brent solve) 
   
   ErrStat = ErrID_None
   ErrMsg  = ""
   
   f1 = UncoupledErrFn(phiLower, theta, Re, numBlades, rlocal, chord, AFInfo, &
                        Vx, Vy, useTanInd, useAIDrag, useTIDrag, useHubLoss, useTipLoss,  hubLossConst, tipLossConst, &
                        IsValidSolution, errStat2, errMsg2)
   
   call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName ) 
   if (errStat >= AbortErrLev) return
   
   f2 = UncoupledErrFn(phiUpper, theta, Re, numBlades, rlocal, chord, AFInfo, &
                     Vx, Vy, useTanInd, useAIDrag, useTIDrag, useHubLoss, useTipLoss,  hubLossConst, tipLossConst, &
                     IsValidSolution2, errStat2, errMsg2)
   call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName ) 
   if (errStat >= AbortErrLev) return
   
   
      ! Look for zero-crossing
   if ( EqualRealNos(f1, 0.0_ReKi) .and. EqualRealNos(f2, 0.0_ReKi) .and. IsValidSolution .and. IsValidSolution2) then
      TestRegion = 0  ! all solutions yield zero -- special case
      return
   else
      if ( abs(f1) < aTol ) then
         if ( abs(f2) < abs(f1) .and. IsValidSolution2 ) then
            TestRegion = 4 ! special case: upper end point is a zero (and it's smaller than the solution at the lower end point)
            return
         elseif ( IsValidSolution ) then
            TestRegion = 3 ! special case: lower end point is a zero
            return
         end if
      elseif ( abs(f2) < aTol .and. IsValidSolution2 ) then
            TestRegion = 4 ! special case: upper end point is a zero
            return
      end if
      
   end if
   
   if ( sign(1.0_ReKi,f1) /= sign(1.0_ReKi,f2) ) then
      TestRegion = 1
   else
      TestRegion = 2  ! No zero
   end if
   
end function TestRegion
      
subroutine BEMT_UnCoupledSolve( phi, numBlades, airDens, mu, AFInfo, rlocal, chord, theta,  &
                           Vx, Vy, useTanInd, useAIDrag, useTIDrag, useHubLoss, useTipLoss, hubLossConst, tipLossConst, &
                           maxIndIterations, aTol, ValidPhi, ErrStat, ErrMsg)

   use fminfcn
   use mod_root1dim
   !use fminMod
   real(ReKi),             intent(inout) :: phi
   integer,                intent(in   ) :: numBlades
   real(ReKi),             intent(in   ) :: airDens
   real(ReKi),             intent(in   ) :: mu
   TYPE(AFInfoType),       INTENT(IN   ) :: AFInfo
   real(ReKi),             intent(in   ) :: rlocal                    
   real(ReKi),             intent(in   ) :: chord          
   real(ReKi),             intent(in   ) :: theta           
   real(ReKi),             intent(in   ) :: Vx             
   real(ReKi),             intent(in   ) :: Vy                        
   logical,                intent(in   ) :: useTanInd 
   logical,                intent(in   ) :: useAIDrag
   logical,                intent(in   ) :: useTIDrag
   logical,                intent(in   ) :: useHubLoss
   logical,                intent(in   ) :: useTipLoss
   real(ReKi),             intent(in   ) :: hubLossConst
   real(ReKi),             intent(in   ) :: tipLossConst
   integer,                intent(in   ) :: maxIndIterations
   real(ReKi),             intent(in   ) :: aTol
   logical,                intent(inout) :: ValidPhi
   integer(IntKi),         intent(  out) :: errStat       ! Error status of the operation
   character(*),           intent(  out) :: errMsg        ! Error message if ErrStat /= ErrID_None
   
   
      ! Local variables
   type(fmin_fcnArgs)                    :: fcnArgs
   
   character(ErrMsgLen)                  :: errMsg2       ! temporary Error message if ErrStat /= ErrID_None
   integer(IntKi)                        :: errStat2      ! temporary Error status of the operation
   character(*), parameter               :: RoutineName = 'BEMT_UnCoupledSolve'
   real(ReKi), parameter                 :: MsgLimit = 0.07_ReKi ! don't print a message if we're within about 4 degrees of 0 or +/- pi/2 [arbitrary number picked by bjj]
   
   real(ReKi) :: f1, f_lower, f_upper
   real(ReKi) :: phi_lower(3), phi_upper(3)         ! upper and lower bounds for region of phi in which we are trying to find a solution to the BEM equations
   integer    :: i, TestRegionResult
   logical    :: IsValidSolution
   real(ReKi) :: Re, Vrel
   
   ErrStat = ErrID_None
   ErrMsg  = ""
  
   
   if ( VelocityIsZero(Vx) ) then
      phi =  0.0_ReKi
      ValidPhi = .true.
      return
   else if ( VelocityIsZero(Vy) ) then
      if (Vx>0.0_ReKi) then
         phi =  PiBy2
      else
         phi = -PiBy2
      end if
      ValidPhi = .true.
      return
   end if
   
      ! Need to call BEMTU_Wind to obtain Re, even though we aren't using it in this version.
      ! inductions are set to zero!
   call BEMTU_Wind( 0.0_ReKi, 0.0_ReKi, Vx, Vy, chord, airDens, mu, Vrel, Re )
   
   
   !# ------ BEM solution method see (Ning, doi:10.1002/we.1636) ------
   
      ! See if the previous value of phi still satisfies the residual equation.
      ! (If the previous phi wasn't a valid solution to BEMT equations, skip this check and just perform the solve)
   if (ValidPhi .and. .NOT. EqualRealNos(phi, 0.0_ReKi) .and. .not. EqualRealNos(abs(phi),PiBy2) ) then  
      f1 = UncoupledErrFn(phi, theta, Re, numBlades, rlocal, chord, AFInfo, &
                           Vx, Vy, useTanInd, useAIDrag, useTIDrag, useHubLoss, useTipLoss,  hubLossConst, tipLossConst, &               
                           IsValidSolution, errStat2, errMsg2)
      call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName ) 
      if (errStat >= AbortErrLev) return

      if ( abs(f1) < aTol .and. IsValidSolution ) then
         !phiStar =  phiIn
         return
      end if
   end if
   
   
      
   ! 
   ValidPhi = .false. ! initialize to false while we try to find a new valid solution
   
   !............
   ! 
   
      ! Set up the fcn argument settings for Brent's method

   fcnArgs%airDens         = airDens
   fcnArgs%mu              = mu
   fcnArgs%numBlades       = numBlades
   fcnArgs%rlocal          = rlocal
   fcnArgs%chord           = chord
   fcnArgs%theta           = theta   
   fcnArgs%Vx              = Vx
   fcnArgs%Vy              = Vy
   fcnArgs%Re              = Re
   fcnArgs%useTanInd       = useTanInd
   fcnArgs%useAIDrag       = useAIDrag
   fcnArgs%useTIDrag       = useTIDrag
   fcnArgs%useHubLoss      = useHubLoss
   fcnArgs%useTipLoss      = useTipLoss
   fcnArgs%hubLossConst    = hubLossConst
   fcnArgs%tipLossConst    = tipLossConst   
   
   
   call GetSolveRegionOrdering(Vx, phi, phi_lower, phi_upper)
   
   do i = 1,size(phi_upper)   ! Need to potentially test 3 regions
      TestRegionResult = TestRegion(phi_lower(i), phi_upper(i), numBlades, rlocal, chord, theta, AFInfo, &
                        Vx, Vy, Re, useTanInd, useAIDrag, useTIDrag, useHubLoss, useTipLoss,  hubLossConst, tipLossConst, atol, &
                        f_lower, f_upper, errStat2, errMsg2)
      call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName ) 
      
      if ( TestRegionResult == 1 ) then
         !............
         ! There is a zero in the solution region [phi_lower(i), phi_upper(i)] because the endpoints have residuals with different signs (SolutionRegion=1)
         ! We use Brent's Method to find the zero-residual solution in this region
                  
         call sub_brent(phi,fmin_fcn,phi_lower(i),phi_upper(i), aTol, maxIndIterations, fcnArgs, AFInfo, f_lower, f_upper)
            call SetErrStat(fcnArgs%ErrStat, fcnArgs%ErrMsg, ErrStat, ErrMsg, RoutineName)
   
         if (fcnArgs%IsValidSolution) then ! we have a valid BEMT solution
            ValidPhi = .true.
            exit
         end if    
      elseif (TestRegionResult == 3) then
         phi = phi_lower(i) !this boundary is a solution
         ValidPhi = .true.
         exit         
      elseif (TestRegionResult == 4) then
         phi = phi_upper(i) !this boundary is a solution
         ValidPhi = .true.
         exit         
      elseif (TestRegionResult == 0) then ! Special case where both end points return 0 residual; return value closest to 0 as the solution
         if (phi_lower(i) > 0.0_ReKi) then
            phi = phi_lower(i) !this boundary is a solution
            ValidPhi = .true.
            exit
         else
            phi = phi_upper(i) !this boundary is a solution
            ValidPhi = .true.
            exit
         end if         
      end if 
      
   end do
   

   if (.not. ValidPhi) then
      phi = ComputePhiWithInduction(Vx, Vy,  0.0_ReKi, 0.0_ReKi)
      
      if (abs(phi)>MsgLimit .and. abs(abs(phi)-PiBy2) > MsgLimit ) then
         call SetErrStat( ErrID_Info, 'There is no valid value of phi for these operating conditions: Vx = '//TRIM(Num2Lstr(Vx))//&
            ', Vy = '//TRIM(Num2Lstr(Vy))//', rlocal = '//TRIM(Num2Lstr(rLocal))//', theta = '//TRIM(Num2Lstr(theta))//', geometric phi = '//TRIM(Num2Lstr(phi)), errStat, errMsg, RoutineName )
      end if
      
         
   end if
   
         
end subroutine BEMT_UnCoupledSolve



end module BEMT
    