!**********************************************************************************************************************************
! LICENSING
! Copyright (C) 2015-2016  National Renewable Energy Laboratory
!
!    This file is part of AeroAcoustics.
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
! File last committed: $Date: 2016-04-07 10:43:27 -0600 (Thu, 07 Apr 2016) $
! (File) Revision #: $Rev: 211 $
!**********************************************************************************************************************************
module AeroAcoustics
    
   use NWTC_Library
   use AeroAcoustics_Types
   use AeroAcoustics_IO
   use NWTC_LAPACK
   
   
   implicit none

   private
         

   ! ..... Public Subroutines ...................................................................................................

   public :: AA_Init                           ! Initialization routine
   public :: AA_End                            ! Ending routine (includes clean up)

   public :: AA_UpdateStates                   ! Loose coupling routine for solving for constraint states, integrating
                                               !   continuous states, and updating discrete states
   public :: AA_CalcOutput                     ! Routine for computing outputs

!!   public :: AA_CalcConstrStateResidual        ! Tight coupling routine for returning the constraint state residual
   

   contains    
!----------------------------------------------------------------------------------------------------------------------------------   
!----------------------------------------------------------------------------------------------------------------------------------   
!> This routine is called at the start of the simulation to perform initialization steps.
!! The parameters are set here and not changed during the simulation.
!! The initial states and initial guess for the input are defined.
subroutine AA_Init( InitInp, u, p, x, xd, z, OtherState, y, m, Interval, InitOut, ErrStat, ErrMsg )
!..................................................................................................................................

   type(AA_InitInputType),       intent(in   ) :: InitInp       !< Input data for initialization routine
   type(AA_InputType),           intent(  out) :: u             !< An initial guess for the input; input mesh must be defined
   type(AA_ParameterType),       intent(  out) :: p             !< Parameters
   type(AA_ContinuousStateType), intent(  out) :: x             !< Initial continuous states
   type(AA_DiscreteStateType),   intent(  out) :: xd            !< Initial discrete states
   type(AA_ConstraintStateType), intent(  out) :: z             !< Initial guess of the constraint states

   type(AA_OtherStateType),      intent(  out) :: OtherState    !< Initial other states
   type(AA_OutputType),          intent(  out) :: y             !< Initial system outputs (outputs are not calculated;
                                                                !!   only the output mesh is initialized)
   type(AA_MiscVarType),         intent(  out) :: m             !< Initial misc/optimization variables
   real(DbKi),                   intent(inout) :: interval      !< Coupling interval in seconds: the rate that
                                                                !!   (1) AA_UpdateStates() is called in loose coupling &
                                                                !!   (2) AA_UpdateDiscState() is called in tight coupling.
                                                                !!   Input is the suggested time from the glue code;
                                                                !!   Output is the actual coupling interval that will be used
                                                                !!   by the glue code.
   type(AA_InitOutputType),      intent(  out) :: InitOut       !< Output for initialization routine
   integer(IntKi),               intent(  out) :: errStat       !< Error status of the operation
   character(*),                 intent(  out) :: errMsg        !< Error message if ErrStat /= ErrID_None
   

      ! Local variables
   integer(IntKi)                              :: i             ! loop counter
   
   integer(IntKi)                              :: errStat2      ! temporary error status of the operation
   character(ErrMsgLen)                        :: errMsg2       ! temporary error message 
      
   type(AA_InputFile)                          :: InputFileData ! Data stored in the module's input file
   integer(IntKi)                              :: UnEcho        ! Unit number for the echo file
   
   character(*), parameter                     :: RoutineName = 'AA_Init'
   
   
      ! Initialize variables for this routine

   errStat = ErrID_None
   errMsg  = ""
   UnEcho  = -1

      ! Initialize the NWTC Subroutine Library

   call NWTC_Init( EchoLibVer=.FALSE. )

      ! Display the module information

   call DispNVD( AA_Ver )
   
   
   p%NumBlades = InitInp%NumBlades ! need this before reading the AD input file so that we know how many blade files to read
   !bjj: note that we haven't validated p%NumBlades before using it below!
   p%RootName  = TRIM(InitInp%RootName)//'.NN'
   
      ! Read the primary AeroAcoustics input file
   call ReadInputFiles( InitInp%InputFile, InputFileData, interval, p%RootName, p%NumBlades, UnEcho, ErrStat2, ErrMsg2 )   
      call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName ) 
      if (ErrStat >= AbortErrLev) then
         call Cleanup()
         return
      end if
         
      
      ! Validate the inputs
   call ValidateInputData( InputFileData, p%NumBlades, ErrStat2, ErrMsg2 )

      call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName ) 
      if (ErrStat >= AbortErrLev) then
         call Cleanup()
         return
      end if
    
      ! Validate Initialization Input data ( not found in the AeroAcoustics input file )

   if (InitInp%AirDens <= 0.0)  call SetErrStat ( ErrID_Fatal, 'The air density (AirDens) must be greater than zero.', ErrStat, ErrMsg, RoutineName )
   if (InitInp%KinVisc <= 0.0)  call SetErrStat ( ErrID_Fatal, 'The kinesmatic viscosity (KinVisc) must be greater than zero.', ErrStat, ErrMsg, RoutineName )
   if (InitInp%SpdSound <= 0.0) call SetErrStat ( ErrID_Fatal, 'The speed of sound (SpdSound) must be greater than zero.', ErrStat, ErrMsg, RoutineName )  
      if (ErrStat >= AbortErrLev) then
         call Cleanup()
         return
      end if
      
      !............................................................................................
      ! Define parameters
      !............................................................................................
     ! set the rest of the parameters
   call SetParameters( InitInp, InputFileData, p, ErrStat2, ErrMsg2 )
      call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName ) 
      if (ErrStat >= AbortErrLev) then
         call Cleanup()
         return
      end if
   
      !............................................................................................
      ! Define and initialize inputs here 
      !............................................................................................
  call Init_u( u, p, InputFileData, InitInp, errStat2, errMsg2 ) 
      call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName ) 
      if (ErrStat >= AbortErrLev) then
         call Cleanup()
         return
      end if
     
      !............................................................................................
      ! Define outputs here
      !............................................................................................
   call Init_y(y, u, p, errStat2, errMsg2) ! do this after input meshes have been initialized
      call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName ) 
   
   
      !............................................................................................
      ! Initialize states and misc vars
      !............................................................................................
   call Init_MiscVars(m, p, u, y, errStat2, errMsg2)
      call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName ) 
      
   call Init_States(xd, p,  errStat2, errMsg2)
      call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName ) 


      !............................................................................................
      ! Define initialization output here
      !............................................................................................
   call AA_SetInitOut(p, InputFileData, InitOut, errStat2, errMsg2)
      call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName ) 



            
   call Cleanup() 
      
contains
   subroutine Cleanup()

      CALL AA_DestroyInputFile( InputFileData, ErrStat2, ErrMsg2 )
      IF ( UnEcho > 0 ) CLOSE( UnEcho )
      
   end subroutine Cleanup

end subroutine AA_Init
!----------------------------------------------------------------------------------------------------------------------------------   
!----------------------------------------------------------------------------------------------------------------------------------   
!> This routine validates the inputs from the AeroDyn input files.
SUBROUTINE ValidateInputData( InputFileData, NumBl, ErrStat, ErrMsg )
!..................................................................................................................................
      
      ! Passed variables:

   type(AA_InputFile),       intent(in)     :: InputFileData                       !< All the data in the AeroDyn input file
   integer(IntKi),           intent(in)     :: NumBl                               !< Number of blades
   integer(IntKi),           intent(out)    :: ErrStat                             !< Error status
   character(*),             intent(out)    :: ErrMsg                              !< Error message


   
      ! local variables
   integer(IntKi)                           :: k                                   ! Blade number
   integer(IntKi)                           :: j                                   ! node number
   character(*), parameter                  :: RoutineName = 'ValidateInputData'
   
   ErrStat = ErrID_None
   ErrMsg  = ""
   
   
   if (NumBl > MaxBl .or. NumBl < 1) call SetErrStat( ErrID_Fatal, 'Number of blades must be between 1 and '//trim(num2lstr(MaxBl))//'.', ErrSTat, ErrMsg, RoutineName )
   if (InputFileData%DTAero <= 0.0)  call SetErrStat ( ErrID_Fatal, 'DTAero must be greater than zero.', ErrStat, ErrMsg, RoutineName )

   if (InputFileData%IBLUNT /= IBLUNT_None .and. InputFileData%IBLUNT /= IBLUNT_BPM) call SetErrStat ( ErrID_Fatal, &
      'IBLUNT must '//trim(num2lstr(IBLUNT_None))//' (none) or '//trim(num2lstr(IBLUNT_BPM))//' (IBLUNT BPM Calculation).', ErrStat, ErrMsg, RoutineName ) 

   if (InputFileData%ILAM /= ILAM_None .and. InputFileData%ilam /= ILAM_BPM) then
      call SetErrStat ( ErrID_Fatal, 'ILAM must be '//trim(num2lstr(ILAM_None))//' (steady) or '//&
                        trim(num2lstr(ILAM_BPM))//' (ILAM BPM Calculation).', ErrStat, ErrMsg, RoutineName ) 
   end if
 
   if (InputFileData%ITIP /= ITIP_None .and. InputFileData%ITIP /= ITIP_ON) then
	print*, 'Your value InputFileData%ITIP ', InputFileData%ITIP
      call SetErrStat ( ErrID_Fatal, 'ITIP must be '//trim(num2lstr(ITIP_None))//' (Off) or '//&
                        trim(num2lstr(ITIP_On))//' (ITIP On).', ErrStat, ErrMsg, RoutineName ) 
   end if   

   if (InputFileData%ITRIP /= ITRIP_None .and. InputFileData%ITRIP /= ITRIP_Heavy .and. InputFileData%ITIP /= ITRIP_Light) then
	print*, 'Your value InputFileData%ITRIP ', InputFileData%ITRIP
      call SetErrStat ( ErrID_Fatal,'ITRIP must be '//trim(num2lstr(ITRIP_None))//' (none) or '//trim(num2lstr(ITRIP_Heavy))//&
	' (heavily tripped BL Calculation) or '//trim(num2lstr(ITRIP_Light))//' (lightly tripped BL)' ,ErrStat, ErrMsg, RoutineName ) 
   end if 

   if (InputFileData%ITURB /= ITURB_None .and. InputFileData%ITURB /= ITURB_BPM .and. InputFileData%ITURB /= ITURB_TNO) then
	print*, 'Your value InputFileData%ITURB ', InputFileData%ITURB
      call SetErrStat ( ErrID_Fatal, 'ITURB must be 0 (off) or 1 (BPM) or 2 (TNO) .', ErrStat, ErrMsg, RoutineName ) 
   end if    
 
   if (InputFileData%IInflow /= IInflow_None .and. InputFileData%IInflow /= IInflow_BPM ) then
	print*, 'Your value InputFileData%IInflow ', InputFileData%IInflow
      call SetErrStat ( ErrID_Fatal, 'IInflow must be 0 (off) or 1 (BPM)  .', ErrStat, ErrMsg, RoutineName ) 
   end if    
 
   if (InputFileData%OctBand /= 1 .and. InputFileData%OctBand /= 3 .and. InputFileData%OctBand /= 6) then
	print*, 'Your value InputFileData%OctBand ', InputFileData%OctBand
      call SetErrStat ( ErrID_Fatal, 'OctBand must be 1 (off) or 3 or 6 .', ErrStat, ErrMsg, RoutineName ) 
   end if    
   
      
   if (InputFileData%NrObsLoc <= 0.0) call SetErrStat ( ErrID_Fatal, 'Number of Observer Locations should be greater than zero', ErrStat, ErrMsg, RoutineName )
   
         
END SUBROUTINE ValidateInputData
!----------------------------------------------------------------------------------------------------------------------------------
!----------------------------------------------------------------------------------------------------------------------------------
!> This routine sets AeroAcoustics parameters for use during the simulation; these variables are not changed after AA_Init.
subroutine SetParameters( InitInp, InputFileData, p, ErrStat, ErrMsg )
   TYPE(AA_InitInputType),       intent(in   )  :: InitInp          !< Input data for initialization routine, out is needed because of copy below
   TYPE(AA_InputFile),           INTENT(IN)  :: InputFileData    !< Data stored in the module's input file -- intent(out) only for move_alloc statements
   TYPE(AA_ParameterType),       INTENT(INOUT)  :: p                !< Parameters
   INTEGER(IntKi),               INTENT(  OUT)  :: ErrStat          !< Error status of the operation
   CHARACTER(*),                 INTENT(  OUT)  :: ErrMsg           ! Error message if ErrStat /= ErrID_None


      ! Local variables
   CHARACTER(ErrMsgLen)                          :: ErrMsg2         ! temporary Error message if ErrStat /= ErrID_None
   INTEGER(IntKi)                                :: ErrStat2        ! temporary Error status of the operation
   INTEGER(IntKi)                                :: simcou          ! simple loop  counter 
   INTEGER(IntKi)                                :: i
   character(*), parameter                       :: RoutineName = 'SetParameters'
   
      ! Initialize variables for this routine

   ErrStat  = ErrID_None
   ErrMsg   = ""
   
   p%DT               = InputFileData%DTAero      
   p%IBLUNT           = InputFileData%IBLUNT
   p%ILAM             = InputFileData%ILAM
   p%ITIP             = InputFileData%ITIP
   p%ITRIP            = InputFileData%ITRIP
   p%ITURB            = InputFileData%ITURB
   p%IInflow          = InputFileData%IInflow
   p%ROUND            = InputFileData%ROUND
   p%OctBand          = InputFileData%OctBand
   

! Frequencioes to be calculated user inputs the octave band 1 or 1/3 or 1/6
! FreqList is a parameter of the AeroAcoustics module
   IF (p%OctBand .eq. 1) THEN
		CALL AllocAry( p%FreqList, 15, 'FreqList', ErrStat2, ErrMsg2)
		p%FreqList(1)=10
		DO simcou=2,size(p%FreqList)
		p%FreqList(simcou) = (2**(1))*p%FreqList(simcou-1)
		ENDDO	
   ELSEIF (p%OctBand .eq. 3) THEN
		CALL AllocAry( p%FreqList, 31, 'FreqList', ErrStat2, ErrMsg2)
		p%FreqList(1)=10
		DO simcou=2,size(p%Freqlist)
		p%FreqList(simcou) = (2**(1/3))*p%FreqList(simcou-1)
		ENDDO		
   ELSEIF (p%OctBand .eq. 6) THEN
		CALL AllocAry( p%FreqList, 61, 'FreqList', ErrStat2, ErrMsg2)
		p%FreqList(1)=10
		DO simcou=2,size(p%Freqlist)
		p%FreqList(simcou) = (2**(1/6))*p%FreqList(simcou-1)
		ENDDO	   		
   ENDIF

   p%NumBlNds         = InitInp%NumBlNds
   p%AirDens          = InitInp%AirDens          
   p%KinVisc          = InitInp%KinVisc
   p%SpdSound         = InitInp%SpdSound
   p%NrObsLoc 	       = InputFileData%NrObsLoc
	
   call AllocAry( p%ObsX,  p%NrObsLoc, 'p%ObsX', ErrStat2, ErrMsg2 )
     call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName ) 
   call AllocAry( p%ObsY,  p%NrObsLoc, 'p%ObsY', ErrStat2, ErrMsg2 )
     call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName ) 
   call AllocAry( p%ObsZ,  p%NrObsLoc, 'p%ObsZ', ErrStat2, ErrMsg2 )
     call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName ) 

   p%ObsX    	      = InputFileData%ObsX
   p%ObsY    	      = InputFileData%ObsY
   p%ObsZ    	      = InputFileData%ObsZ
   
  call AllocAry( p%TEThick,  p%NumBlNds, p%NumBlades, 'p%TEThick', ErrStat2, ErrMsg2 )
      call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName ) 
   do i=1,p%NumBlades
      p%TEThick(:,i) = InputFileData%BladeProps(i)%TEThick(:) !
   end do
   
   call AllocAry( p%TEAngle,  p%NumBlNds, p%NumBlades, 'p%TEAngle', ErrStat2, ErrMsg2 )
      call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
   do i=1,p%NumBlades
      p%TEAngle(:,i) = InputFileData%BladeProps(i)%TEAngle(:) !
   end do
   
   call AllocAry( p%AerCent,  2, p%NumBlNds, p%NumBlades,  'p%AerCent', ErrStat2, ErrMsg2 )
      call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
      
  ! TODO: Extract the aerodynamic center from  InitInp%AFInfo(f)%X_Coord(1) and InitInp%AFInfo(f)%Y_Coord(1) 
  !p%AerCent = InputFileData%BladeProps(1)%AerCent

   call AllocAry( p%BlSpn,  p%NumBlNds, p%NumBlades,  'p%BlSpn', ErrStat2, ErrMsg2 )
      call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
     p%BlSpn = InitInp%BlSpn
 
  call AllocAry( p%BlChord,  p%NumBlNds, p%NumBlades,  'p%BlChord', ErrStat2, ErrMsg2 )
      call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
     p%BlChord = InitInp%BlChord


   call AllocAry( p%AFTeCo, 3, p%NumBlNds,p%numBlades, 'p%AFTeCo', errStat2, errMsg2 )
      call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
   if (ErrStat >= AbortErrLev) RETURN 
   ! TODO
   p%AFTeCo=0.0_Reki

   call AllocAry( p%AFLeCo, 3, p%NumBlNds,p%numBlades, 'u%AFLeCo', errStat2, errMsg2 )
      call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
   if (ErrStat >= AbortErrLev) RETURN 
   !TODO
   p%AFLeCo=0.0_Reki
   
   ! TODO Allocate AFInfo and AFindx variables

end subroutine SetParameters
!----------------------------------------------------------------------------------------------------------------------------------
!----------------------------------------------------------------------------------------------------------------------------------   
!> This routine initializes AeroAcoustics module input array variables for use during the simulation.
subroutine Init_u( u, p, InputFileData, InitInp, errStat, errMsg )

   type(AA_InputType),           intent(  out)  :: u                 !< Input data
   type(AA_ParameterType),       intent(in   )  :: p                 !< Parameters

   type(AA_InputFile),           intent(in   )  :: InputFileData     !< Data stored in the module's input file
   type(AA_InitInputType),       intent(in   )  :: InitInp           !< Input data for AD initialization routine
   integer(IntKi),               intent(  out)  :: errStat           !< Error status of the operation
   character(*),                 intent(  out)  :: errMsg            !< Error message if ErrStat /= ErrID_None
!local variables
   integer(intKi)                               :: ErrStat2          ! temporary Error status
   character(ErrMsgLen)                         :: ErrMsg2           ! temporary Error message
   character(*), parameter                      :: RoutineName = 'Init_u'

   call AllocAry( u%AoANoise, p%NumBlNds,p%numBlades, 'u%AoANoise', errStat2, errMsg2 )
      call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
   if (ErrStat >= AbortErrLev) RETURN
   u%AoANoise=0.0_Reki

   call AllocAry( u%Vrel, p%NumBlNds,p%numBlades, 'u%Vrel', errStat2, errMsg2 )
      call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
   if (ErrStat >= AbortErrLev) RETURN
   u%Vrel=0.0_Reki

   call AllocAry( u%RotLtoG, 3, 3, p%NumBlNds,p%numBlades, 'u%RotLtoG', errStat2, errMsg2 )
      call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
   if (ErrStat >= AbortErrLev) RETURN 
   u%RotLtoG=0.0_Reki

   call AllocAry( u%AeroCent_G, 3, p%NumBlNds,p%numBlades, 'u%AeroCent_G', errStat2, errMsg2 )
      call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
   if (ErrStat >= AbortErrLev) RETURN 
   u%AeroCent_G=0.0_Reki
   
   

	
end subroutine Init_u
!----------------------------------------------------------------------------------------------------------------------------------
!----------------------------------------------------------------------------------------------------------------------------------   
!> This routine initializes AeroAcoustics  output array variables for use during the simulation.
subroutine Init_y(y, u, p, errStat, errMsg)
   type(AA_OutputType),           intent(  out)  :: y               !< Module outputs
   type(AA_InputType),            intent(inout)  :: u               !< Module inputs -- intent(out) because of mesh sibling copy
   type(AA_ParameterType),        intent(inout)  :: p               !< Parameters
   integer(IntKi),                intent(  out)  :: errStat         !< Error status of the operation
   character(*),                  intent(  out)  :: errMsg          !< Error message if ErrStat /= ErrID_None


      ! Local variables
   integer(intKi)                               :: k                 ! loop counter for blades
   integer(intKi)                               :: ErrStat2          ! temporary Error status
   character(ErrMsgLen)                         :: ErrMsg2           ! temporary Error message
   character(*), parameter                      :: RoutineName = 'Init_y'

      ! Initialize variables for this routine

   errStat = ErrID_None
   errMsg  = ""
   p%numOuts= p%NrObsLoc*size(p%Freqlist)
   print*, 'p%numOuts',p%numOuts
   call AllocAry( y%WriteOutput, p%numOuts, 'WriteOutput', errStat2, errMsg2 )
      call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
   if (ErrStat >= AbortErrLev) RETURN      
   
   call AllocAry( y%SumSpecNoise, p%NrObsLoc,size(p%FreqList), 'SumSpecNoise', errStat2, errMsg2 )
      call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
   if (ErrStat >= AbortErrLev) RETURN      
   
end subroutine Init_y

!----------------------------------------------------------------------------------------------------------------------------------   
!> This routine initializes (allocates) the misc variables for use during the simulation.
subroutine Init_MiscVars(m, p, u, y, errStat, errMsg)
   type(AA_MiscVarType),          intent(inout)  :: m                !< misc/optimization data (not defined in submodules)
   type(AA_ParameterType),        intent(in   )  :: p                !< Parameters
   type(AA_InputType),            intent(inout)  :: u                !< input for HubMotion mesh (create sibling mesh here)
   type(AA_OutputType),           intent(in   )  :: y                !< output (create mapping between output and otherstate mesh here)
   integer(IntKi),                intent(  out)  :: errStat          !< Error status of the operation
   character(*),                  intent(  out)  :: errMsg           !< Error message if ErrStat /= ErrID_None


      ! Local variables
   integer(intKi)                               :: k
   integer(intKi)                               :: ErrStat2          ! temporary Error status
   character(ErrMsgLen)                         :: ErrMsg2           ! temporary Error message
   character(*), parameter                      :: RoutineName = 'Init_MiscVars'

      ! Initialize variables for this routine

   errStat = ErrID_None
   errMsg  = ""
   
    call AllocAry( m%ChordAngleLE, p%NrObsLoc, p%NumBlNds, p%numBlades, 'ChordAngleLE', ErrStat2, ErrMsg2 )
     call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
    call AllocAry( m%SpanAngleLE, p%NrObsLoc, p%NumBlNds, p%numBlades, 'SpanAngleLE', ErrStat2, ErrMsg2 )
     call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )   
    call AllocAry( m%ChordAngleTE, p%NrObsLoc, p%NumBlNds, p%numBlades, 'ChordAngleTE', ErrStat2, ErrMsg2 )
     call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
    call AllocAry( m%SpanAngleTE, p%NrObsLoc, p%NumBlNds, p%numBlades, 'SpanAngleTE', ErrStat2, ErrMsg2 )
     call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
    call AllocAry( m%rTEtoObserve, p%NrObsLoc, p%NumBlNds, p%numBlades, 'rTEtoObserve', ErrStat2, ErrMsg2 )
     call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
    call AllocAry( m%rLEtoObserve, p%NrObsLoc, p%NumBlNds, p%numBlades, 'rLEtoObserve', ErrStat2, ErrMsg2 )
     call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName ) 
    call AllocAry( m%SPLLBL, size(p%FreqList), 'SPLLBL', errStat2, errMsg2 )
      call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
    call AllocAry( m%SPLP, size(p%FreqList), 'SPLP', errStat2, errMsg2 )
      call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName ) 
    call AllocAry( m%SPLS, size(p%FreqList), 'SPLS', errStat2, errMsg2 )
      call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
    call AllocAry( m%SPLALPH, size(p%FreqList),'SPLALPH', errStat2, errMsg2 )
      call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
    call AllocAry( m%SPLTBL, size(p%FreqList), 'SPLTBL', errStat2, errMsg2 )
      call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
    call AllocAry( m%SPLBLUNT, size(p%FreqList), 'SPLBLUNT', errStat2, errMsg2 )
      call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
    call AllocAry( m%SPLTIP, size(p%FreqList), 'SPLTIP', errStat2, errMsg2 )
      call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
    call AllocAry( m%SPLTI, size(p%FreqList), 'SPLTI', errStat2, errMsg2 )
      call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )

   
end subroutine Init_MiscVars
!----------------------------------------------------------------------------------------------------------------------------------
!----------------------------------------------------------------------------------------------------------------------------------   
!> This routine initializes (allocates) the misc variables for use during the simulation.
subroutine Init_states(xd, p, errStat, errMsg)
   type(AA_DiscreteStateType),    intent(inout)  :: xd               !
   type(AA_ParameterType),        intent(in   )  :: p                !< Parameters
   integer(IntKi),                intent(  out)  :: errStat          !< Error status of the operation
   character(*),                  intent(  out)  :: errMsg           !< Error message if ErrStat /= ErrID_None


      ! Local variables
   integer(intKi)                               :: k
   integer(intKi)                               :: ErrStat2          ! temporary Error status
   character(ErrMsgLen)                         :: ErrMsg2           ! temporary Error message
   character(*), parameter                      :: RoutineName = 'Init_DiscrStates'

      ! Initialize variables for this routine

   errStat = ErrID_None
   errMsg  = ""

   call AllocAry( xd%MeanVrel, p%NumBlNds, p%numBlades, 'xd%MeanVrel', ErrStat2, ErrMsg2 )
     call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
   call AllocAry( xd%VrelSq,   p%NumBlNds, p%numBlades,  'xd%VrelSq', ErrStat2, ErrMsg2 )
     call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
   call AllocAry( xd%TIVrel,   p%NumBlNds, p%numBlades,  'xd%TIVrel', ErrStat2, ErrMsg2 )
     call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )

	xd%VrelSq   = 0.0_ReKi  ! Relative Velocity Squared for TI calculation (on the fly)
	xd%MeanVrel = 0.0_ReKi  ! Relative Velocity Mean  calculation (on the fly)
	xd%TIVrel   = 0.0_ReKi  ! Turbulence Intensity (for on the fly calculation)

end subroutine Init_states
!----------------------------------------------------------------------------------------------------------------------------------
!..................................................................................................................................
subroutine AA_UpdateStates( t, n, u, p,  xd,  errStat, errMsg )
!..................................................................................................................................

   real(DbKi),                     intent(in   ) :: t          !< Current simulation time in seconds
   integer(IntKi),                 intent(in   ) :: n          !< Current simulation time step n = 0,1,...
   type(AA_InputType),             intent(in   ) :: u          !< Inputs at utimes (out only for mesh record-keeping in ExtrapInterp routine)
   TYPE(AA_ParameterType),         INTENT(IN   )  :: p           !< Parameters
   type(AA_DiscreteStateType),     intent(inout) :: xd         !< Input: Discrete states at t;
                                                               !!   Output: Discrete states at t  + Interval
   integer(IntKi),                 intent(  out) :: errStat    !< Error status of the operation
   character(*),                   intent(  out) :: errMsg     !< Error message if ErrStat /= ErrID_None

   ! local variables
   integer(intKi)                               :: ErrStat2          ! temporary Error status
   character(ErrMsgLen)                         :: ErrMsg2           ! temporary Error message
   character(*), parameter                      :: RoutineName = 'AA_UpdateStates'
   REAL(ReKi),DIMENSION(p%NumBlNds,p%numBlades)                    :: TEMPSTD  ! temporary standard deviation variable
	integer(intKi)                               :: i,j

   ErrStat = ErrID_None
   ErrMsg  = ""
! cumulative mean and standard deviation, states are updated as Vrel changes at each time step
   xd%MeanVrel = (u%Vrel + xd%MeanVrel*n) / (n+1)
   xd%VrelSq   = u%Vrel**2 + xd%VrelSq

       TEMPSTD     = sqrt(  (xd%VrelSq/(n+1)) - (xd%MeanVrel**2)   )
       xd%TIVrel   = (TEMPSTD / xd%MeanVrel ) * 100 ! check inflow noise input for multiplication with 100 or not


end subroutine AA_UpdateStates

!> This subroutine sets the initialization output data structure, which contains data to be returned to the calling program (e.g.,
!! FAST or AeroAcoustics_Driver)   
subroutine AA_SetInitOut(p, InputFileData, InitOut, errStat, errMsg)

   type(AA_InitOutputType),       intent(  out)  :: InitOut          ! output data
   type(AA_InputFile),            intent(in   )  :: InputFileData    ! input file data (for setting airfoil shape outputs)
   type(AA_ParameterType),        intent(in   )  :: p                ! Parameters
   integer(IntKi),                intent(  out)  :: errStat          ! Error status of the operation
   character(*),                  intent(  out)  :: errMsg           ! Error message if ErrStat /= ErrID_None


      ! Local variables
   integer(intKi)                               :: ErrStat2          ! temporary Error status
   character(ErrMsgLen)                         :: ErrMsg2           ! temporary Error message
   character(*), parameter                      :: RoutineName = 'AA_SetInitOut'
   
   
   
   integer(IntKi)                               :: i, j, k
   integer(IntKi)                               :: NumCoords

   integer(IntKi)                               :: m
   character(500)                                 ::chanPrefix

      ! Initialize variables for this routine

   errStat = ErrID_None
   errMsg  = ""
   
   InitOut%AirDens = p%AirDens
   
   call AllocAry( InitOut%WriteOutputHdr, p%numOuts, 'WriteOutputHdr', errStat2, errMsg2 )
      call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )

   
   call AllocAry( InitOut%WriteOutputUnt, p%numOuts, 'WriteOutputUnt', errStat2, errMsg2 )
      call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )

   if (ErrStat >= AbortErrLev) return
      
   
!#ifdef DBG_OUTS
!   ! Loop over blades and nodes to populate the output channel names and units
   
!   do k=1,p%numBlades
!      do j=1, p%NumBlNds
!	  m = (k-1)*p%NumBlNds*23 + (j-1)*23 
!         chanPrefix = "B"//trim(num2lstr(k))//"N"//trim(num2lstr(j))
!         InitOut%WriteOutputHdr( m + 1 ) = trim(chanPrefix)//"Vrel"
!         InitOut%WriteOutputUnt( m + 1 ) = '  (m/s)  '
!         InitOut%WriteOutputHdr( m + 2 ) = ' '//trim(chanPrefix)//"AOA"
!         InitOut%WriteOutputUnt( m + 2 ) = '  (deg)  '
!      end do
!   end do
!#else
	i=0
   do k=1,size(p%FreqList)
	do j=1,p%NrObsLoc
		i=i+1
	      InitOut%WriteOutputHdr(i) = "F"//trim(num2lstr(p%FreqList(k)))//" R"//" "//trim(num2lstr(j))
	      InitOut%WriteOutputUnt(i) = "SPL"
        end do
   enddo
!#endif
                      
   
   InitOut%Ver = AA_Ver

  
   
   
end subroutine AA_SetInitOut
!----------------------------------------------------------------------------------------------------------------------------------
     
!----------------------------------------------------------------------------------------------------------------------------------
!> This routine is called at the end of the simulation.
subroutine AA_End( u, p, x, xd, z, OtherState, y, m, ErrStat, ErrMsg )
!..................................................................................................................................

      TYPE(AA_InputType),           INTENT(INOUT)  :: u           !< System inputs
      TYPE(AA_ParameterType),       INTENT(INOUT)  :: p           !< Parameters
      TYPE(AA_ContinuousStateType), INTENT(INOUT)  :: x           !< Continuous states
      TYPE(AA_DiscreteStateType),   INTENT(INOUT)  :: xd          !< Discrete states
      TYPE(AA_ConstraintStateType), INTENT(INOUT)  :: z           !< Constraint states
      TYPE(AA_OtherStateType),      INTENT(INOUT)  :: OtherState  !< Other states
      TYPE(AA_OutputType),          INTENT(INOUT)  :: y           !< System outputs
      TYPE(AA_MiscVarType),         INTENT(INOUT)  :: m           !< Misc/optimization variables
      INTEGER(IntKi),               INTENT(  OUT)  :: ErrStat     !< Error status of the operation
      CHARACTER(*),                 INTENT(  OUT)  :: ErrMsg      !< Error message if ErrStat /= ErrID_None



         ! Initialize ErrStat

      ErrStat = ErrID_None
      ErrMsg  = ""


         ! Place any last minute operations or calculations here:


         ! Close files here:



         ! Destroy the input data:

      CALL AA_DestroyInput( u, ErrStat, ErrMsg )


         ! Destroy the parameter data:

      CALL AA_DestroyParam( p, ErrStat, ErrMsg )


         ! Destroy the state data:

      CALL AA_DestroyContState(   x,           ErrStat, ErrMsg )
      CALL AA_DestroyDiscState(   xd,          ErrStat, ErrMsg )
      CALL AA_DestroyConstrState( z,           ErrStat, ErrMsg )
      CALL AA_DestroyOtherState(  OtherState,  ErrStat, ErrMsg )
      CALL AA_DestroyMisc(        m,           ErrStat, ErrMsg ) 

         ! Destroy the output data:

      CALL AA_DestroyOutput( y, ErrStat, ErrMsg )




END SUBROUTINE AA_End


!> Routine for computing outputs, used in both loose and tight coupling.
!! This subroutine is used to compute the output channels (motions and loads) and place them in the WriteOutput() array.
!! The descriptions of the output channels are not given here. Please see the included OutListParameters.xlsx sheet for
!! for a complete description of each output parameter.
subroutine AA_CalcOutput( t, u, p, x, xd, z, OtherState, y, m, ErrStat, ErrMsg)
! NOTE: no matter how many channels are selected for output, all of the outputs are calcalated
! All of the calculated output channels are placed into the m%AllOuts(:), while the channels selected for outputs are
! placed in the y%WriteOutput(:) array.
!..................................................................................................................................

   REAL(DbKi),                   INTENT(IN   )  :: t           !< Current simulation time in seconds
   TYPE(AA_InputType),           INTENT(IN   )  :: u           !< Inputs at Time t
   TYPE(AA_ParameterType),       INTENT(IN   )  :: p           !< Parameters
   TYPE(AA_ContinuousStateType), INTENT(IN   )  :: x           !< Continuous states at t
   TYPE(AA_DiscreteStateType),   INTENT(IN   )  :: xd          !< Discrete states at t
   TYPE(AA_ConstraintStateType), INTENT(IN   )  :: z           !< Constraint states at t
   TYPE(AA_OtherStateType),      INTENT(IN   )  :: OtherState  !< Other states at t
   TYPE(AA_OutputType),          INTENT(INOUT)  :: y           !< Outputs computed at t (Input only so that mesh con-
   type(AA_MiscVarType),         INTENT(INOUT)  :: m           !< Misc/optimization variables
   INTEGER(IntKi),               INTENT(  OUT)  :: ErrStat     !< Error status of the operation
   CHARACTER(*),                 INTENT(  OUT)  :: ErrMsg      !< Error message if ErrStat /= ErrID_None
   

   integer, parameter                           :: indx = 1  ! m%BEMT_u(1) is at t; m%BEMT_u(2) is t+dt
   integer(intKi)                               :: i
   INTEGER(IntKi)                               :: nt     !< timestep increment
   integer(intKi)                               :: ErrStat2
   character(ErrMsgLen)                         :: ErrMsg2
   character(*), parameter                      :: RoutineName = 'AA_CalcOutput'
   
   
   ErrStat = ErrID_None
   ErrMsg  = ""
      ! assume integer divide is possible
   nt = t/ p%DT

   call CalcObserve(p,m,u,errStat2, errMsg2)
      call SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)

   call CalcAeroAcousticsOutput(u,p,m,xd,y,errStat2,errMsg2,nt)
          call SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)

   
   !-------------------------------------------------------   
   !     get values to output to file:  
   !-------------------------------------------------------   
!   if (p%NumOuts > 0) then
!#ifdef DBG_OUTS
!      call Calc_WriteDbgOutput( p, u, m, y, ErrStat2, ErrMsg2 ) 
!#else
      call Calc_WriteOutput( p, u, m, y,  ErrStat2, ErrMsg2 )   
!#endif   
      call SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)      
   
      !...............................................................................................................................   
      ! Place the selected output channels into the WriteOutput(:) array with the proper sign:
      !...............................................................................................................................   

!      do i = 1,p%NumOuts  ! Loop through all selected output channels
!#ifdef DBG_OUTS
!         y%WriteOutput(i) = m%AllOuts( i )
!#else
!         y%WriteOutput(i) = p%OutParam(i)%SignM * m%AllOuts( p%OutParam(i)%Indx )
!#endif

!      end do             ! i - All selected output channels
      
 !  end if
   
   
   
end subroutine AA_CalcOutput
!----------------------------------------------------------------------------------------------------------------------------------
!----------------------------------------------------------------------------------------------------------------------------------!
SUBROUTINE CalcObserve(p,m,u,errStat,errMsg)

   IMPLICIT NONE

   TYPE(AA_ParameterType),                   intent(in   )  :: p                ! Parameters
   TYPE(AA_InputType),                       intent(in   )  :: u                !< NN Inputs at Time
   TYPE(AA_MiscVarType),                     intent(inout)  :: m                !< misc/optimization data (not defined in submodules)
   INTEGER(IntKi),                           intent(  out)  :: errStat          ! Error status of the operation
   CHARACTER(*),                             intent(  out)  :: errMsg           ! Error message if ErrStat /= ErrID_None

   ! Local variables.
   REAL(ReKi)                   :: RLEObserve (3)                                  ! position vector from leading edge to observer in trailing edge coordinate system
   REAL(ReKi)                   :: RTEObserve (3)                                  ! position vector from trailing edge to observer in trailing edge coordinate system

   REAL(ReKi)                   :: RObserveInt(3)                                  ! RObserve in the internal coordinate system
   REAL(ReKi)                   :: rSLE       (3)                                  ! Distance from tower base to leading edge in trailing edge coordinate system
   REAL(ReKi)                   :: rSTE       (3)                                  ! Distance from tower base to trailing edge in trailing edge coordinate system
   REAL(ReKi)                   :: timeLE                                          ! Time of sound propagation from leading edge to observer
   REAL(ReKi)                   :: timeTE                                          ! Time of sound propagation from trailing edge to observer
   REAL(ReKi)                   :: tmpR       (3)                                  ! temporary distance vector
   REAL(ReKi)                   :: UConvect   (3)                                  ! convection velocity of noise source in trailing edge coordinate system

   INTEGER(intKi)               :: I                                               ! I A generic index for DO loops.  
   INTEGER(intKi)               :: J                                               ! J A generic index for DO loops.
   INTEGER(intKi)               :: K                                               ! K A generic index for DO loops.  

   INTEGER(intKi)               :: ErrStat2
   CHARACTER(ErrMsgLen)         :: ErrMsg2
   CHARACTER(*), parameter      :: RoutineName = 'CalcObserveDist'
   
   
   ErrStat = ErrID_None
   ErrMsg  = ""
! Transform RObserve to the internal a coordinate system
!RObserveInt (1) = RObserve (1)
!RObserveInt (2) = RObserve (3) + PtfmRef
!RObserveInt (3) =-RObserve (2)
     
   DO I = 1, p%numBlades
      DO J = 1, p%NumBlNds
	    DO K = 1,p%NrObsLoc
!!          ! Calculate position vector of trailing edge from tower base in trailing edge coordinate system
!!          rSTE (1) = DOT_PRODUCT(te1(J,I,:),rS(J,I,:))
!!          rSTE (2) = DOT_PRODUCT(te2(J,I,:),rS(J,I,:)) + 0.75*Chord(I)
!!          rSTE (3) = DOT_PRODUCT(te3(J,I,:),rS(J,I,:))
!!
!!          ! Calculate position vector of leading edge from tower base in trailing edge coordinate system
!!          rSLE (1) = rSTE (1)
!!          rSLE (2) = rSTE (2) - Chord(I)
!!          rSLE (3) = rSTE (3)
!!
!!          ! Calculate position vector of observer from tower base in trailing edge coordinate system
!!          tmpR (1) = DOT_PRODUCT(te1(J,I,:),RObserveInt)
!!          tmpR (2) = DOT_PRODUCT(te2(J,I,:),RObserveInt)
!!          tmpR (3) = DOT_PRODUCT(te3(J,I,:),RObserveInt)
!!
!!          ! Calculate position vector from leading and trailing edge to observer in trailing edge coordinate system
!!          RTEObserve = tmpR-rSTE
!!          RLEObserve = tmpR-rSLE
          
!================================================================
! TODO: Restructure based on new inputs: RotLtoG(3,3,numNodes, numBlades) and AeroCent_G(3,numNodes, numBlades)
!       
   	 !   RTEObserve(1)=p%Obsx(K)-u%BlTECo(J,I,1)
	    !RTEObserve(2)=p%Obsy(K)-u%BlTECo(J,I,2)
     !       RTEObserve(3)=p%Obsz(K)-u%BlTECo(J,I,3)
     !
	    !RLEObserve(1)=p%Obsx(K)-u%BlLECo(J,I,1)
	    !RLEObserve(2)=p%Obsy(K)-u%BlLECo(J,I,2)
	    !RLEObserve(3)=p%Obsz(K)-u%BlLECo(J,I,3)
! TODO : End rework
!================================================================
!!
!!          ! Calculate convection velocity of noise source
!!          ! Assumes noise source convects at some constant times the mean wind speed, approximately accounts for
!!          ! induction velocity and change in convection velocity as noise propagates to observer (likely on the ground)
!!          UConvect (1) = te1(J,I,1)*0.8*MeanVNoise
!!          UConvect (2) = te2(J,I,1)*0.8*MeanVNoise
!!          UConvect (3) = te3(J,I,1)*0.8*MeanVNoise
!          UConvect (1) = te1(J,I,1)*0.8*u%Vrel
!          UConvect (2) = te2(J,I,1)*0.8*u%Vrel
!          UConvect (3) = te3(J,I,1)*0.8*u%Vrel
!!
!!          ! Calculate time of noise propagation to observer
          timeTE = SQRT (RTEObserve(1)**2+RTEObserve(2)**2+RTEObserve(3)**2)/p%SpdSound
          timeLE = SQRT (RLEObserve(1)**2+RLEObserve(2)**2+RLEObserve(3)**2)/p%SpdSound
!!
!!          ! Calculate position vector from leading and trailing edge to observer in retarded trailing edge coordinate system
!!          RTEObserve = RTEObserve-UConvect*timeTE
!!          RLEObserve = RTEObserve-UConvect*timeLE  ! coerect the bug not RTE RLE
!!
!!          ! Calculate inputs into noise subroutines
          m%rTEtoObserve(K,J,I) = SQRT (RTEObserve(1)**2+RTEObserve(2)**2+RTEObserve(3)**2)
          m%rLEtoObserve(K,J,I) = SQRT (RLEObserve(1)**2+RLEObserve(2)**2+RLEObserve(3)**2)

          m%ChordAngleTE(K,J,I) = ACOS (RTEObserve(2)/SQRT(RTEObserve(1)**2+RTEObserve(2)**2+RTEObserve(3)**2))*R2D
          m%SpanAngleTE(K,J,I) = ACOS (RTEObserve(3)/SQRT(RTEObserve(1)**2+RTEObserve(3)**2))*R2D
          IF (m%SpanAngleTE(K,J,I)< 0) m%SpanAngleTE(K,J,I)= 180+m%SpanAngleTE(K,J,I)
          IF (m%ChordAngleTE(K,J,I)< 0) m%ChordAngleTE(K,J,I)= 180+m%ChordAngleTE(K,J,I)

          m%ChordAngleLE(K,J,I) = ACOS (RLEObserve(2)/SQRT(RLEObserve(1)**2+RLEObserve(2)**2+RLEObserve(3)**2))*R2D
          m%SpanAngleLE(K,J,I) = ACOS (RLEObserve(3)/SQRT(RLEObserve(1)**2+RLEObserve(3)**2))*R2D
          IF (m%SpanAngleLE(K,J,I)< 0) m%SpanAngleLE(K,J,I)= 180+m%SpanAngleLE(K,J,I)
          IF (m%ChordAngleLE(K,J,I)< 0) m%ChordAngleLE(K,J,I)= 180+m%ChordAngleLE(K,J,I)

      	ENDDO !K
     ENDDO  !J
   ENDDO  !I 

RETURN
END SUBROUTINE CalcObserve
!----------------------------------------------------------------------------------------------------------------------------------!
SUBROUTINE CalcAeroAcousticsOutput(u,p,m,xd,y,errStat,errMsg,nt)

   IMPLICIT NONE

  TYPE(AA_InputType),           	    INTENT(IN   )  :: u                !< Inputs at Time t
  TYPE(AA_OutputType),           	    INTENT(INOUT)  :: y                !
  TYPE(AA_ParameterType),                   INTENT(IN   )  :: p                ! Parameters
  TYPE(AA_MiscVarType),		       	    INTENT(INOUT)  :: m                !< misc/optimization data (not defined in submodules)
  TYPE(AA_DiscreteStateType),       	    INTENT(IN   )  :: xd               ! discrete state type
  integer(IntKi),                           INTENT(  OUT)  :: errStat          ! Error status of the operation
  character(*),                             INTENT(  OUT)  :: errMsg           ! Error message if ErrStat /= ErrID_None
  INTEGER(IntKi),                           INTENT(IN   )  :: nt     !DELETE LATER

   ! Local variables.

 integer(intKi)                :: III                                             !III A generic index for DO loops.
 integer(intKi)                :: I                                               !I   A generic index for DO loops.
 integer(intKi)                :: J                                               !J   A generic index for DO loops.
 integer(intKi)                :: K                                               !K   A generic index for DO loops.
 REAL(ReKi)                    :: AlphaNoise                                 ! 
 REAL(ReKi)                    :: UNoise                                     ! 


 real(ReKi)                                                 ::  Ptotal	
 real(ReKi)                                                 ::	PtotalLBL    
 real(ReKi)                                                 ::	PtotalTBLP   
 real(ReKi)                                                 ::	PtotalTBLS   
 real(ReKi)                                                 ::	PtotalSep    
 real(ReKi)                                                 ::	PtotalTBLAll 
 real(ReKi)                                                 ::	PtotalBlunt  
 real(ReKi)                                                 ::	PtotalTip    
 real(ReKi)                                                 ::	PtotalInflow 	
 real(ReKi)                                                 ::	PLBL
 real(ReKi)                                                 ::	PTBLP
 real(ReKi)                                                 ::	PTBLS
 real(ReKi)                                                 ::	PTBLALH
 real(ReKi)                                                 ::	PTip
 real(ReKi)                                                 ::	PTI
 real(ReKi)                                                 ::	PBLNT

 integer(intKi)                               :: ErrStat2
 character(ErrMsgLen)                         :: ErrMsg2
 character(*), parameter                      :: RoutineName = 'CalcAeroAcousticsOutput'
   	logical :: exist
  
   ErrStat = ErrID_None
   ErrMsg  = ""
   
   
	y%SumSpecNoise=0.0_Reki
   
   DO I = 1, p%numBlades
      DO J = 1, p%NumBlNds
	    DO K = 1,p%NrObsLoc
     		AlphaNoise= u%AoANoise(J,I) * R2D_D 

		Unoise =  u%Vrel(J,I) 

!  inquire(file="test.txt", exist=exist)
!  if (exist) then
!    open(1254, file="test.txt", status="old", position="append", action="write")
!  else
!    open(1254, file="test.txt", status="new", action="write")
!  end if
!  write(1254, *) AlphaNoise
 ! close(1254)

	 	IF ( (p%ILAM .EQ. 1) .AND. (p%ITRIP .EQ. 0) )    THEN
  		 CALL LBLVS(AlphaNoise,p%BlChord(J,I),UNoise,m%ChordAngleTE(K,J,I),m%SpanAngleTE(K,J,I), &
		   p%BlSpn(J,I),m%rTEtoObserve(K,J,I), &
  	           p,m%SPLLBL,errStat2,errMsg2)
		 CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName ) 
		ENDIF

		IF ( p%ITURB .EQ. 1 )   THEN                                          
 	          CALL TBLTE(AlphaNoise,p%BlChord(J,I),UNoise,m%ChordAngleTE(K,J,I),m%SpanAngleTE(K,J,I), &
		   p%BlSpn(J,I),m%rTEtoObserve(K,J,I), p, &
		   m%SPLP,m%SPLS,m%SPLALPH,m%SPLTBL,errStat2,errMsg2 )
		  CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName ) 
		ENDIF

		IF ( p%IBLUNT .EQ. 1 )   THEN                                          
	          CALL BLUNT(AlphaNoise,p%BlChord(J,I),UNoise,m%ChordAngleTE(K,J,I),m%SpanAngleTE(K,J,I), &
		  p%BlSpn(J,I),m%rTEtoObserve(K,J,I),p%TEThick(J,I),p%TEAngle(J,I),p,m%SPLBLUNT,errStat2,errMsg2 )
		  CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName ) 
		ENDIF

		IF (  (p%ITIP .EQ. 1) .AND. (J .EQ. p%NumBlNds)  ) THEN                                          
	          CALL TIPNOIS(AlphaNoise,p%ALpRAT,p%BlChord(J,I),UNoise,m%ChordAngleTE(K,J,I),m%SpanAngleTE(K,J,I), &
		   m%rTEtoObserve(K,J,I), p, m%SPLTIP,errStat2,errMsg2)
		  CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName ) 
		ENDIF	
! important checks to be done inflow tubulence inputs	
		IF ( p%IInflow .EQ. 1 )   THEN                                          
       	  	  CALL InflowAeroAcoustics(UNoise,p%BlChord(J,I),p%BlSpn(J,I),m%rLEtoObserve(K,J,I), &
                            m%ChordAngleTE(K,J,I),m%SpanAngleTE(K,J,I),u%Vrel(J,I),xd%TIVrel(J,I), &
			    p,m%SPLti,errStat2,errMsg2 )
		ENDIF
!----------------------------------------------------------------------------------------------------------------------------------!
!      ADD IN THIS SEGMENT'S CONTRIBUTION ON A MEAN-SQUARE
!      PRESSURE BASIS
!----------------------------------------------------------------------------------------------------------------------------------!
        Ptotal = 0.0
        PtotalLBL= 0.0
        PtotalTBLP= 0.0
        PtotalTBLS= 0.0
        PtotalSep= 0.0
        PtotalTBLAll = 0.0
        PtotalBlunt= 0.0
        PtotalTip= 0.0
        PtotalInflow= 0.0

    DO III=1,size(p%FreqList)

       IF ( (p%ILAM .EQ. 1) .AND. (p%ITRIP .EQ. 0) )  THEN
            PLBL = 10.**(m%SPLLBL(III)/10.)
            PtotalLBL = PtotalLBL + PLBL
            Ptotal = Ptotal + PLBL
     !       AvePressure (5,III) = AvePressure (5,III) + PLBL
     !       AvePressure (4,III) = AvePressure (4,III) + PLBL
          ENDIF

          IF ( p%ITURB .GT. 0 )  THEN
            PTBLP = 10.**(m%SPLP(III)/10.)
            PTBLS = 10.**(m%SPLS(III)/10.)
            PTBLALH = 10.**(m%SPLALPH(III)/10.)
            PtotalTBLP = PtotalTBLP + PTBLP
            PtotalTBLS = PtotalTBLS + PTBLS
            PtotalSep  = PtotalSep  + PTBLALH
            Ptotal = Ptotal + PTBLP + PTBLS + PTBLALH
            PtotalTBLAll = PtotalTBLAll + 10.**(m%SPLTBL(III)/10.)
    !        AvePressure (1,III) = AvePressure (1,III) + PTBLP
    !        AvePressure (2,III) = AvePressure (2,III) + PTBLS
    !        AvePressure (3,III) = AvePressure (3,III) + PTBLALH
    !        AvePressure (4,III) = AvePressure (4,III) + PTBLP + PTBLS + PTBLALH
          ENDIF

          IF ( p%IBLUNT .GT. 0 )  THEN
            PBLNT = 10.**(m%SPLBLUNT(III)/10.)
            PtotalBlunt = PtotalBlunt + PBLNT
            Ptotal = Ptotal + PBLNT
   !         AvePressure (6,III) = AvePressure (6,III) + PBLNT
   !         AvePressure (4,III) = AvePressure (4,III) + PBLNT
          ENDIF

          IF ( (p%ITIP .GT. 1) .AND. (I .EQ. p%NumBlNds) )  THEN
            PTip = 10.**(m%SPLTIP(III)/10.)
            PtotalTip = PtotalTip + PTip
            Ptotal = Ptotal + PTip
  !          AvePressure (7,III) = AvePressure (7,III) + PTip
  !          AvePressure (4,III) = AvePressure (4,III) + PTip
          ENDIF

          IF ( (p%IInflow .GT. 0)  )  THEN
            PTI = 10.**(m%SPLti(III)/10.)
            PtotalInflow = PtotalInflow + PTI
            Ptotal = Ptotal + PTI
  !          AvePressure (8,III) = AvePressure (8,III) + PTI
  !          AvePressure (4,III) = AvePressure (4,III) + PTI
          ENDIF

		y%SumSpecNoise(K,III) = Ptotal + y%SumSpecNoise(K,III)
   ENDDO ! III = 1, size(p%FreqList)

  ! IF (PtotalLBL    .NE. 0.) OASPLLBL   (K,J,I) = 10.*LOG10(PtotalLBL)
  ! IF (PtotalTBLP   .NE. 0.) OASPLTBLP  (K,J,I) = 10.*LOG10(PtotalTBLP)
  ! IF (PtotalTBLS   .NE. 0.) OASPLTBLS  (K,J,I) = 10.*LOG10(PtotalTBLS)
  ! IF (PtotalSep    .NE. 0.) OASPLSep   (K,J,I) = 10.*LOG10(PtotalSep)
  ! IF (PtotalTBLAll .NE. 0.) OASPLTBLAll(K,J,I) = 10.*LOG10(PtotalTBLAll)
  ! IF (PtotalBlunt  .NE. 0.) OASPLBlunt (K,J,I) = 10.*LOG10(PtotalBlunt)
  ! IF (PtotalTip    .NE. 0.) OASPLTip   (K,J,I) = 10.*LOG10(PtotalTip)
  ! IF (PtotalInflow .NE. 0.) OASPLInflow(K,J,I) = 10.*LOG10(PtotalInflow)
  ! OASPL(K,J,I) = 10.*LOG10(Ptotal)



	    ENDDO
	ENDDO
   ENDDO
   y%SumSpecNoise = 10.*LOG10(y%SumSpecNoise)
   


RETURN
END SUBROUTINE CalcAeroAcousticsOutput
!==================================================================================================================================!
!==================================================================================================================================!
  SUBROUTINE LBLVS(ALPSTAR,C,U,THETA,PHI,L,R,p,SPLLAM,errStat,errMsg)

  REAL(ReKi),           	                  INTENT(IN   )  :: ALPSTAR        ! AOA
  REAL(ReKi),           	                  INTENT(IN   )  :: C              ! Chord Length
  REAL(ReKi),           	                  INTENT(IN   )  :: U              ! Unoise FREESTREAM VELOCITY                METERS/SEC
  REAL(ReKi),           	                  INTENT(IN   )  :: THETA          ! DIRECTIVITY ANGLE                  DEGREES  
  REAL(ReKi),           	                  INTENT(IN   )  :: PHI            ! DIRECTIVITY ANGLE                  DEGREES  
  REAL(ReKi),           	                  INTENT(IN   )  :: L              ! SPAN                               METERS
  REAL(ReKi),           	                  INTENT(IN   )  :: R              !  OBSERVER DISTANCE FROM SEGMENT     METERS
  TYPE(AA_ParameterType),                         INTENT(IN   )  :: p          ! Noise module Parameters
  REAL(ReKi),DIMENSION(size(p%FreqList)),	  INTENT(  OUT)  :: SPLLAM         !
  INTEGER(IntKi),        	          	  INTENT(  OUT)  :: errStat        ! Error status of the operation
  character(*),                                   INTENT(  OUT)  :: errMsg         ! Error message if ErrStat /= ErrID_None

  integer(intKi)                                             :: ErrStat2           ! temporary Error status
  character(ErrMsgLen)                                       :: ErrMsg2            ! temporary Error message
  character(*), parameter                                    :: RoutineName = 'LBLVS'

   ! Local variables
   !!!real(ReKi)                                    :: STPRIM(size(p%FreqList))  !EB_DTU does not need to be a vector 
   real(ReKi)                                    :: STPRIM   !  STROUHAL NUMBER BASED ON PRESSURE SIDE BOUNDARY LAYER THICKNESS    ---
   real(ReKi)                                    :: M        ! MACH NUMBER
   real(ReKi)                                    :: RC       ! REYNOLDS NUMBER BASED ON  CHORD
   real(ReKi)                                    :: DELTAP   ! PRESSURE SIDE BOUNDARY LAYER THICKNESS METERS
   real(ReKi)                                    :: DSTRS    ! SUCTION SIDE BOUNDARY LAYER DISPLACEMENT THICKNESS           METERS
   real(ReKi)                                    :: DSTRP    ! PRESSURE SIDE BOUNDARY LAYER DISPLACEMENT THICKNESS           METERS
   real(ReKi)                                    :: DBARH    ! HIGH FREQUENCY DIRECTIVITY             ---
   real(ReKi)                                    :: ST1PRIM  ! REFERENCE STROUHAL NUMBER          ---
   real(ReKi)                                    :: STPKPRM  ! PEAK STROUHAL NUMBER               ---
   real(ReKi)                                    :: RC0      ! REFERENCE REYNOLDS NUMBER          ---
   real(ReKi)                                    :: D        ! REYNOLDS NUMBER RATIO              ---
   real(ReKi)                                    :: G1       ! SOUND PRESSURE LEVEL FUNCTION      DB
   real(ReKi)                                    :: G2       ! OVERALL SOUND PRESSURE LEVEL FUNCTION    DB
   real(ReKi)                                    :: G3       ! OVERALL SOUND PRESSURE LEVEL FUNCTION    DB
   real(ReKi)                                    :: E        ! STROUHAL NUMBER RATIO              ---
   real(ReKi)                                    :: SCALE    ! GEOMETRIC SCALING TERM
   integer(intKi)		                 :: I        ! I A generic index for DO loops.

   ErrStat = ErrID_None
   ErrMsg  = ""

!!!      COMPUTE REYNOLDS NUMBER AND MACH NUMBER
!!!      ---------------------------------------
        M          = U  / p%SpdSound        ! MACH NUMBER
        RC         = U  * C/p%KinVisc       ! REYNOLDS NUMBER BASED ON  CHORD
!!!      COMPUTE BOUNDARY LAYER THICKNESSES
!!!      ----------------------------------
       CALL THICK(C,M,RC,ALPSTAR,p,DELTAP,DSTRS,DSTRP,errStat2,errMsg2)
     CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName ) 
!!!      COMPUTE DIRECTIVITY FUNCTION
!!!      ----------------------------
       CALL DIRECTH(M,THETA,PHI,DBARH,errStat2,errMsg2)
     CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName ) 

      IF (DBARH <= 0) THEN
          SPLLAM = 0.
          RETURN
      ENDIF
!!!      COMPUTE REFERENCE STROUHAL NUMBER
!!!      ---------------------------------
      IF (RC .LE. 1.3E+05) ST1PRIM = .18
      IF((RC .GT. 1.3E+05).AND.(RC.LE.4.0E+05))ST1PRIM=.001756*RC**.3931
      IF (RC .GT. 4.0E+05) ST1PRIM = .28

      STPKPRM  = 10.**(-.04*ALPSTAR) * ST1PRIM
!!!      COMPUTE REFERENCE REYNOLDS NUMBER
!!!      ---------------------------------
      IF (ALPSTAR .LE. 3.0) RC0=10.**(.215*ALPSTAR+4.978)
      IF (ALPSTAR .GT. 3.0) RC0=10.**(.120*ALPSTAR+5.263)
!!!      COMPUTE PEAK SCALED SPECTRUM LEVEL
!!!      ----------------------------------
      D   = RC / RC0

      IF (D .LE. .3237) G2=77.852*LOG10(D)+15.328
      IF ((D .GT. .3237).AND.(D .LE. .5689)) &
        G2 = 65.188*LOG10(D) + 9.125
      IF ((D .GT. .5689).AND.(D .LE. 1.7579)) &
        G2 = -114.052 * LOG10(D)**2.
      IF ((D .GT. 1.7579).AND.(D .LE. 3.0889)) &
        G2 = -65.188*LOG10(D)+9.125
      IF (D .GT. 3.0889) G2 =-77.852*LOG10(D)+15.328

      G3      = 171.04 - 3.03 * ALPSTAR

      SCALE   = 10. * LOG10(DELTAP*M**5*DBARH*L/R**2)
!!!      COMPUTE SCALED SOUND PRESSURE LEVELS FOR EACH STROUHAL NUMBER
!!!      -------------------------------------------------------------
      DO I=1,SIZE(p%FreqList)

         STPRIM  = p%FreqList(I) * DELTAP / U

         E          = STPRIM / STPKPRM

         IF (E .LT. .5974) G1=39.8*LOG10(E)-11.12
         IF ((E .GE. .5974).AND.(E .LE. .8545)) &
           G1 = 98.409 * LOG10(E) + 2.0
         IF ((E .GE. .8545).AND.(E .LT. 1.17)) &
           G1 = -5.076+SQRT(2.484-506.25*(LOG10(E))**2.)
         IF ((E .GE. 1.17).AND.(E .LT. 1.674)) &
           G1 = -98.409 * LOG10(E) + 2.0
         IF (E .GE. 1.674) G1=-39.80*LOG10(E)-11.12

         SPLLAM(I) = G1 + G2 + G3 + SCALE
	ENDDO
!  100 CONTINUE

      RETURN

   END SUBROUTINE LBLVS
!==================================================================================================================================!
!==================================================================================================================================!
      SUBROUTINE TBLTE(ALPSTAR,C,U,THETA,PHI,L,R,p,SPLP,SPLS,SPLALPH,SPLTBL,errStat,errMsg)
	  REAL(ReKi),           	                  INTENT(IN   )  :: ALPSTAR        ! AOA					(deg)
  REAL(ReKi),           	                  INTENT(IN   )  :: C              ! Chord Length           (m)
  REAL(ReKi),           	                  INTENT(IN   )  :: U              ! Unoise					(m/s)
  REAL(ReKi),           	                  INTENT(IN   )  :: THETA          ! DIRECTIVITY ANGLE      (deg)
  REAL(ReKi),           	                  INTENT(IN   )  :: PHI            ! DIRECTIVITY ANGLE      (deg) 
  REAL(ReKi),           	                  INTENT(IN   )  :: L              ! SPAN					(m)
  REAL(ReKi),           	                  INTENT(IN   )  :: R              ! SOURCE TO OBSERVER DISTANCE (m)
  TYPE(AA_ParameterType),                         INTENT(IN   )  :: p              ! Noise Module Parameters
  REAL(ReKi),DIMENSION(size(p%FreqList)),	  INTENT(  OUT)  :: SPLP           ! SOUND PRESSURE LEVEL DUE TO PRESSURE SIDE OF AIRFOIL (db)
  REAL(ReKi),DIMENSION(size(p%FreqList)),	  INTENT(  OUT)  :: SPLS           ! SOUND PRESSURE LEVEL DUE TO SUCTION SIDE OF AIRFOIL  (db)
  REAL(ReKi),DIMENSION(size(p%FreqList)),	  INTENT(  OUT)  :: SPLTBL         ! TOTAL SOUND PRESSURE LEVEL DUE TO TBLTE MECHANISM    (db)
  REAL(ReKi),DIMENSION(size(p%FreqList)),	  INTENT(  OUT)  :: SPLALPH        ! SOUND PRESSURE LEVEL DUE TO ANGLE OF ATTACK CONTRIBUTION (db)
  INTEGER(IntKi),                   		  INTENT(  OUT)  :: errStat        ! Error status of the operation
  character(*),                               INTENT(  OUT)  :: errMsg         ! Error message if ErrStat /= ErrID_None

  integer(intKi)                                             :: ErrStat2       ! temporary Error status
  character(ErrMsgLen)                                       :: ErrMsg2        ! temporary Error message
  character(*), parameter                                    :: RoutineName = 'TBLTE'

  ! Local variables
!      REAL(ReKi)  :: STP      (NFrequency) !EB_DTU does not need to be a vector 
!      REAL(ReKi)  :: STS      (NFrequency) !EB_DTU does not need to be a vector 
	real(ReKi)                                    :: STP        ! PRESSURE SIDE STROUHAL NUMBER          --- 
	real(ReKi)                                    :: STS        ! SUCTION SIDE STROUHAL NUMBER           ---
	real(ReKi)                                    :: DSTRS      ! SUCTION SIDE DISPLACEMENT THICKNESS   METERS
	real(ReKi)                                    :: DSTRP      ! PRESSURE SIDE DISPLACEMENT THICKNESS  METERS
	real(ReKi)                                    :: RDSTRS     ! REYNOLDS NUMBER BASED ON SUCTION  SIDE DISPLACEMENT THICKNESS 
	real(ReKi)                                    :: RDSTRP     ! REYNOLDS NUMBER BASED ON PRESSURE SIDE DISPLACEMENT THICKNESS
	real(ReKi)                                    :: ST1        ! PEAK STROUHAL NUMBER                   ---
	real(ReKi)                                    :: ST2        ! PEAK STROUHAL NUMBER                   ---
	real(ReKi)                                    :: ST1PRIM    ! PEAK STROUHAL NUMBER                   ---
	real(ReKi)                                    :: A0         ! FUNCTION USED IN 'A' CALCULATION
	real(ReKi)                                    :: A02        ! FUNCTION USED IN 'A' CALCULATION
	real(ReKi)                                    :: ARA0       ! INTERPOLATION FACTOR
	real(ReKi)                                    :: ARA02      ! INTERPOLATION FACTOR
	real(ReKi)                                    :: B0         ! FUNCTION USED IN 'B' CALCULATION
	real(ReKi)                                    :: BMINB0     ! MINIMUM 'B' EVALUATED AT B0            DB
	real(ReKi)                                    :: BMINB      ! MINIMUM 'B' EVALUATED AT B             DB
	real(ReKi)                                    :: BMAXB0     ! MAXIMUM 'B' EVALUATED AT B0            DB
	real(ReKi)                                    :: BMAXB      ! MAXIMUM 'B' EVALUATED AT B             DB
	real(ReKi)                                    :: BRB0       ! INTERPOLATION FACTOR                   DB
	real(ReKi)                                    :: STPEAK     ! PEAK STROUHAL NUMBER                   ---
	real(ReKi)                                    :: AMINA      ! MINIMUM 'A' CURVE EVALUATED AT STROUHAL NUMBER RATIO                DB
	real(ReKi)                                    :: AMINB      ! MINIMUM 'A' CURVE EVALUATED AT B       DB
	real(ReKi)                                    :: AMAXA      ! MAXIMUM 'A' CURVE EVALUATED AT STROUHAL NUMBER RATIO    (DB)
	real(ReKi)                                    :: AMAXB      ! MAXIMUM 'A' CURVE EVALUATED AT B       DB
	real(ReKi)                                    :: AMINA0     ! MAXIMUM 'B' EVALUATED AT B0            DB
	real(ReKi)                                    :: AMINA02    ! MINIMUM 'A' CURVE EVALUATED AT A02     DB
	real(ReKi)                                    :: AMAXA0     ! MAXIMUM 'A' CURVE EVALUATED AT A0      DB
	real(ReKi)                                    :: AMAXA02    ! MAXIMUM 'A' CURVE EVALUATED AT A02      DB
	real(ReKi)                                    :: A          ! STROUHAL NUMBER RATIO                 ---
	real(ReKi)                                    :: B          ! STROUHAL NUMBER RATIO                 ---
	real(ReKi)                                    :: AA         ! 'A' SPECTRUM SHAPE EVALUATED AT STROUHAL NUMBER RATIO         DB
	real(ReKi)                                    :: BB         ! 'B' SPECTRUM SHAPE EVALUATED AT STROUHAL NUMBER RATIO                DB
	real(ReKi)                                    :: DELK1      ! CORRECTION TO AMPLITUDE FUNCTION       DB
	real(ReKi)                                    :: GAMMA      ! USED IN 'B' COMPUTATION                ---
	real(ReKi)                                    :: BETA       ! USED IN 'B' COMPUTATION               ---
	real(ReKi)                                    :: GAMMA0     ! USED IN 'B' COMPUTATION                ---
	real(ReKi)                                    :: BETA0      ! USED IN 'B' COMPUTATION               ---
	real(ReKi)                                    :: K1         ! AMPLITUDE FUNCTION						 (DB)
	real(ReKi)                                    :: K2         ! AMPLITUDE FUNCTION						 (DB)
	real(ReKi)                                    :: P1         ! PRESSURE SIDE PRESSURE 					 (NT/M2)
	real(ReKi)                                    :: P2         ! SUCTION SIDE PRESSURE                      (NT/M2)
	real(ReKi)                                    :: P4         ! PRESSURE FROM ANGLE OF ATTACK CONTRIBUTION (NT/M2)
	real(ReKi)                                    :: M          ! MACH NUMBER
	real(ReKi)                                    :: RC         ! REYNOLDS NUMBER BASED ON  CHORD
	real(ReKi)                                    :: DELTAP     ! PRESSURE SIDE BOUNDARY LAYER THICKNESS METERS
	real(ReKi)                                    :: XCHECK     ! USED TO CHECK FOR ANGLE OF ATTACK CONTRIBUTION     
	real(ReKi)                                    :: DBARH      ! HIGH FREQUENCY DIRECTIVITY             ---
	real(ReKi)                                    :: DBARL      ! LOW FREQUENCY DIRECTIVITY              ---
	
	integer(intKi)				                  :: I          ! I A generic index for DO loops.

    LOGICAL     :: SWITCH  !!LOGICAL FOR COMPUTATION OF ANGLE OF ATTACK CONTRIBUTION   

        ErrStat = ErrID_None
        ErrMsg  = ""

!!!      COMPUTE REYNOLDS NUMBER AND MACH NUMBER
!!!      ---------------------------------------
        M          = U  / p%SpdSound
        RC         = U  * C/p%KinVisc
!!!      COMPUTE BOUNDARY LAYER THICKNESSES
!!!      ----------------------------------
        CALL THICK(C,M,RC,ALPSTAR,p,DELTAP,DSTRS,DSTRP,errStat2,errMsg2)
		     CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName ) 
!!!     COMPUTE DIRECTIVITY FUNCTION
!!!     ----------------------------
       CALL DIRECTL(M,THETA,PHI,DBARL,errStat2,errMsg2)
		     CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName ) 	  
       CALL DIRECTH(M,THETA,PHI,DBARH,errStat2,errMsg2)
		     CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName ) 
      IF (DBARH <= 0) THEN
          SPLP = 0.
          SPLS = 0.
          SPLALPH = 0.
          RETURN
      ENDIF
!!!     CALCULATE THE REYNOLDS NUMBERS BASED ON PRESSURE AND
!!!     SUCTION DISPLACEMENT THICKNESS
!!!     ---------------------------------------------------
      RDSTRS = DSTRS * U  / p%KinVisc
      RDSTRP = DSTRP * U  / p%KinVisc
!      DETERMINE PEAK STROUHAL NUMBERS TO BE USED FOR
!      'A' AND 'B' CURVE CALCULATIONS
!      ----------------------------------------------

      ST1    = .02 * M ** (-.6)

      IF (ALPSTAR .LE. 1.333) ST2 = ST1
      IF ((ALPSTAR .GT. 1.333).AND.(ALPSTAR .LE. 12.5)) &
        ST2 = ST1*10.**(.0054*(ALPSTAR-1.333)**2.)
      IF (ALPSTAR .GT. 12.5) ST2 = 4.72 * ST1


      ST1PRIM = (ST1+ST2)/2.


      CALL A0COMP(RC,A0)
      CALL A0COMP(3.*RC,A02)

!      EVALUATE MINIMUM AND MAXIMUM 'A' CURVES AT A0
!      ----------------------------------------------

      CALL AMIN(A0,AMINA0)
      CALL AMAX(A0,AMAXA0)

      CALL AMIN(A02,AMINA02)
      CALL AMAX(A02,AMAXA02)

!      COMPUTE 'A' MAX/MIN RATIO
!      -------------------------

      ARA0  = (20. + AMINA0) / (AMINA0 - AMAXA0)
      ARA02 = (20. + AMINA02)/ (AMINA02- AMAXA02)

!      COMPUTE B0 TO BE USED IN 'B' CURVE CALCULATIONS
!      -----------------------------------------------

      IF (RC .LT. 9.52E+04) B0 = .30
      IF ((RC .GE. 9.52E+04).AND.(RC .LT. 8.57E+05)) &
         B0 = (-4.48E-13)*(RC-8.57E+05)**2. + .56
      IF (RC .GE. 8.57E+05) B0 = .56

!      EVALUATE MINIMUM AND MAXIMUM 'B' CURVES AT B0
!      ----------------------------------------------

      CALL BMIN(B0,BMINB0)
      CALL BMAX(B0,BMAXB0)

!      COMPUTE 'B' MAX/MIN RATIO
!      -------------------------

      BRB0  = (20. + BMINB0) / (BMINB0 - BMAXB0)

!      FOR EACH CENTER FREQUENCY, COMPUTE AN
!      'A' PREDICTION FOR THE PRESSURE SIDE
!      -------------------------------------

      STPEAK = ST1

      DO  I=1,size(p%FreqList)
        STP= p%FreqList(I) * DSTRP / U
        A      = LOG10( STP / STPEAK )
        CALL AMIN(A,AMINA)
        CALL AMAX(A,AMAXA)
        AA     = AMINA + ARA0 * (AMAXA - AMINA)

        IF (RC .LT. 2.47E+05) K1 = -4.31 * LOG10(RC) + 156.3
        IF((RC .GE. 2.47E+05).AND.(RC .LT. 8.0E+05)) &
          K1 = -9.0 * LOG10(RC) + 181.6
        IF (RC .GT. 8.0E+05) K1 = 128.5

        IF (RDSTRP .LE. 5000.) DELK1 = -ALPSTAR*(5.29-1.43* &
          LOG10(RDSTRP))
        IF (RDSTRP .GT. 5000.) DELK1 = 0.0

        SPLP(I)=AA+K1-3.+10.*LOG10(DSTRP*M**5.*DBARH*L/R**2.)+DELK1




      GAMMA   = 27.094 * M +  3.31
      BETA    = 72.650 * M + 10.74
      GAMMA0  = 23.430 * M +  4.651
      BETA0   =-34.190 * M - 13.820

      IF (ALPSTAR .LE. (GAMMA0-GAMMA)) K2 = -1000.0
      IF ((ALPSTAR.GT.(GAMMA0-GAMMA)).AND.(ALPSTAR.LE.(GAMMA0+GAMMA))) &
       K2=SQRT(BETA**2.-(BETA/GAMMA)**2.*(ALPSTAR-GAMMA0)**2.)+BETA0
      IF (ALPSTAR .GT. (GAMMA0+GAMMA)) K2 = -12.0

      K2 = K2 + K1



      STS = p%FreqList(I) * DSTRS / U

!      CHECK FOR 'A' COMPUTATION FOR SUCTION SIDE
!      ------------------------------------------

      XCHECK = GAMMA0
      SWITCH = .FALSE.
      IF ((ALPSTAR .GE. XCHECK).OR.(ALPSTAR .GT. 12.5))SWITCH=.TRUE.
      IF (.NOT. SWITCH) THEN
        A      = LOG10( STS / ST1PRIM )
        CALL AMIN(A,AMINA)
        CALL AMAX(A,AMAXA)
        AA = AMINA + ARA0 * (AMAXA - AMINA)

        SPLS(I) = AA+K1-3.+10.*LOG10(DSTRS*M**5.*DBARH* &
                 L/R**2.)

!      'B' CURVE COMPUTATION
!       --------------------

        B = ABS(LOG10(STS / ST2))
        CALL BMIN(B,BMINB)
        CALL BMAX(B,BMAXB)
        BB = BMINB + BRB0 * (BMAXB-BMINB)
        SPLALPH(I)=BB+K2+10.*LOG10(DSTRS*M**5.*DBARH*L/R**2.)

      ELSE

!       THE 'A' COMPUTATION IS DROPPED IF 'SWITCH' IS TRUE
!       --------------------------------------------------


        SPLS(I) = 0.0 + 10.*LOG10(DSTRS*M**5.*DBARL*L/R**2.)
        SPLP(I) = 0.0 + 10.*LOG10(DSTRS*M**5.*DBARL*L/R**2.)
        B = ABS(LOG10(STS / ST2))
        CALL AMIN(B,AMINB)
        CALL AMAX(B,AMAXB)
        BB = AMINB + ARA02 * (AMAXB-AMINB)
        SPLALPH(I)=BB+K2+10.*LOG10(DSTRS*M**5.*DBARL*L/R**2.)
      ENDIF


!      SUM ALL CONTRIBUTIONS FROM 'A' AND 'B' ON BOTH
!      PRESSURE AND SUCTION SIDE ON A MEAN-SQUARE PRESSURE
!      BASIS
!      ---------------------------------------------------

      IF (SPLP(I)    .LT. -100.) SPLP(I)    = -100.
      IF (SPLS(I)    .LT. -100.) SPLS(I)    = -100.
      IF (SPLALPH(I) .LT. -100.) SPLALPH(I) = -100.

      P1  = 10.**(SPLP(I) / 10.)
      P2  = 10.**(SPLS(I) / 10.)
      P4  = 10.**(SPLALPH(I) / 10.)

      SPLTBL(I) = 10. * LOG10(P1 + P2 + P4)

    ENDDO

      RETURN
      END SUBROUTINE TBLTE
!==================================================================================================================================!
!==================================================================================================================================!
	SUBROUTINE TIPNOIS(ALPHTIP,ALPRAT2,C,U ,THETA,PHI, R,p,SPLTIP, errStat, errMsg)

  REAL(ReKi),           	                  INTENT(IN   )  :: ALPHTIP        ! AOA
  REAL(ReKi),           	                  INTENT(IN   )  :: ALPRAT2        ! TIP LIFT CURVE SLOPE                 ---
  REAL(ReKi),           	                  INTENT(IN   )  :: C              ! Chord Length
  REAL(ReKi),           	                  INTENT(IN   )  :: U              ! FREESTREAM VELOCITY               METERS/SEC
  REAL(ReKi),           	                  INTENT(IN   )  :: THETA          ! DIRECTIVITY ANGLE                  DEGREES 
  REAL(ReKi),           	                  INTENT(IN   )  :: PHI            ! DIRECTIVITY ANGLE                  DEGREES
  REAL(ReKi),           	                  INTENT(IN   )  :: R              ! SOURCE TO OBSERVER DISTANCE        METERS
  TYPE(AA_ParameterType),                         INTENT(IN   )  :: p              ! Parameters

  REAL(ReKi),DIMENSION(size(p%FreqList)),	  INTENT(  OUT)  :: SPLTIP         !
  INTEGER(IntKi),                   		  INTENT(  OUT)  :: errStat        ! Error status of the operation
  character(*),                                   INTENT(  OUT)  :: errMsg         ! Error message if ErrStat /= ErrID_None
  integer(intKi)                                             :: ErrStat2       ! temporary Error status
  character(ErrMsgLen)                                       :: ErrMsg2        ! temporary Error message
  character(*), parameter                                    :: RoutineName = 'tipnoise'

! local variables
   REAL(ReKi)        :: M        ! MACH NUMBER                         ---
   REAL(ReKi)        :: MM       ! MAXIMUM MACH NUMBER                 ---
   REAL(ReKi)        :: ALPTIPP  ! CORRECTED TIP ANGLE OF ATTACK      DEGREES
   REAL(ReKi)        :: DBARH    ! DIRECTIVITY                         ---
   REAL(ReKi)        :: SCALE    ! SCALING TERM                        ---
   REAL(ReKi)        :: STPP     ! STROUHAL NUMBER                     ---
   REAL(ReKi)        :: UM       ! MAXIMUM VELOCITY                  METERS/SEC
   REAL(ReKi)        :: L        ! CHARACTERISTIC LENGTH FOR TIP      METERS
   REAL(ReKi)        :: TERM     ! SCALING TERM                        ---
   integer(intKi)    :: I        !I A generic index for DO loops.	

	
   ErrStat = ErrID_None
   ErrMsg  = ""

   
      IF (alphtip.eq.0.) THEN
         SPLTIP= 0
         RETURN
      ELSEIF (alphtip.lt.0.) THEN
!         alphtip = ABS (alphtip) !  (EB_DTU) NOT possible to change inten(in) variable, INSTEAD 
				  !  ALPTIPP is equal to abs(alphtip) - see next equation 
      ENDIF
!! used to be  ALPTIPP = ALPHTIP * ALPRAT2
               ALPTIPP = ABS(ALPHTIP) * ALPRAT2

	  
!!!      COMPUTE ´ MACH NUMBER
!!!      ---------------------------------------
        M          = U  / p%SpdSound
!!!      COMPUTE DIRECTIVITY FUNCTION
!!!      ----------------------------
	  CALL DIRECTH(M,THETA,PHI,DBARH,errStat2,errMsg2)
		 CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName ) 

      IF (p%ROUND) THEN
        L = .008 * ALPTIPP * C
      ELSE
        IF (ABS(ALPTIPP) .LE. 2.) THEN
          L = (.023 + .0169*ALPTIPP) * C
        ELSE
          L = (.0378 + .0095*ALPTIPP) * C
        ENDIF
      ENDIF


      MM     = (1. + .036*ALPTIPP) * M

      UM     = MM * p%SpdSound

      TERM  = M*M*MM**3.*L**2.*DBARH/R**2.
      IF (TERM .NE. 0.0) THEN
        SCALE = 10.*LOG10(TERM)
      ELSE
        SCALE = 0.0
      ENDIF

      DO I=1,size(p%FreqList)
        STPP      = p%FreqList(I) * L / UM
        SPLTIP(I) = 126.-30.5*(LOG10(STPP)+.3)**2. + SCALE
	  ENDDO

      RETURN
      END SUBROUTINE TipNois
!==================================================================================================================================!
!==================================================================================================================================!
SUBROUTINE InflowAeroAcoustics(U,Chord,d,RObs,THETA,PHI,MeanVNoise,TINoise,p,SPLti,errStat,errMsg)

	IMPLICIT NONE

  REAL(ReKi),           	                  INTENT(IN   )  :: Chord          ! Chord Length
  REAL(ReKi),           	                  INTENT(IN   )  :: U              !
  REAL(ReKi),           	                  INTENT(IN   )  :: d              ! element span
  REAL(ReKi),           	                  INTENT(IN   )  :: RObs           ! distance to observer
  REAL(ReKi),           	                  INTENT(IN   )  :: THETA          ! 
  REAL(ReKi),           	                  INTENT(IN   )  :: PHI            ! Spanwise directivity angle 
  REAL(ReKi),           	                  INTENT(IN   )  :: MeanVNoise     ! 
  REAL(ReKi),           	                  INTENT(IN   )  :: TINoise        ! 
  TYPE(AA_ParameterType),                         INTENT(IN   )  :: p              ! Parameters

  REAL(ReKi),DIMENSION(size(p%FreqList)),	  INTENT(  OUT)  :: SPLti         !
  INTEGER(IntKi),                   		  INTENT(  OUT)  :: errStat        ! Error status of the operation
  character(*),                                   INTENT(  OUT)  :: errMsg         ! Error message if ErrStat /= ErrID_None
  integer(intKi)                                                 :: ErrStat2       ! temporary Error status
  character(ErrMsgLen)                                           :: ErrMsg2        ! temporary Error message
  character(*), parameter                                        :: RoutineName = 'inflowAeroAcoustics'

! local variables
  REAL(ReKi)                   :: Beta2                                           ! Prandtl-Glauert correction factor
  REAL(ReKi)                   :: DBARH                                           ! High-frequency directivity correction factor
  REAL(ReKi)                   :: DBARL                                           ! Low-frequency directivity correction factor
  REAL(ReKi)                   :: Directivity                                     ! Directivity correction factor
  REAL(ReKi)                   :: Frequency_cutoff                                ! Cutoff frequency between
  REAL(ReKi)                   :: LFC                                             ! low-frequency correction factor
  REAL(ReKi)                   :: LTurb                                           ! turbulence length scale (isotropic integral scale parameter from IEC standard (Von Karman))
  REAL(ReKi)                   :: Mach                                            ! local mach number
  REAL(ReKi)                   :: Sears                                           ! Sears function
  REAL(ReKi)                   :: SPLhigh                                         ! predicted high frequency sound pressure level
  REAL(ReKi)                   :: Ums                                             ! mean square turbulence level
  REAL(ReKi)                   :: WaveNumber                                      ! wave number - non-dimensional frequency

  INTEGER(intKi)    	       :: I        !I A generic index for DO loops.	

	
   ErrStat = ErrID_None
   ErrMsg  = ""

   Mach = U/p%SpdSound
	
   IF (TINoise > 0) THEN
      Ums = (TINoise*MeanVNoise/100.)**2
   ELSE
      SPLti = 0.
     RETURN
   ENDIF

!  temporarily commented until FASTHH is udnerstood (EB_DTU)
!  IF (FASTHH < 30.0) THEN
!      LTurb = 3.5*0.7*FASTHH ! Prediction sensitive to this parameter!
!  ELSE
      LTurb = 3.5*21.
! ENDIF

!LTurb = LTurb/100

! Calculate directivity...?
!!!     ----------------------------
      CALL DIRECTL(Mach,THETA,PHI,DBARL,errStat2,errMsg2) !yes, assume that noise is low-freq in nature because turbulence length scale is large
	    CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName ) 	  
      CALL DIRECTH(Mach,THETA,PHI,DBARH,errStat2,errMsg2)
	    CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName ) 
      IF (DBARH <= 0) THEN
	    SPLti = 0.
          RETURN
      ENDIF

 Frequency_cutoff = 10*U/PI/Chord

    IF (DBARL <= 0.) THEN
        SPLti = 0.
        RETURN
    ENDIF

DO I=1,size(p%FreqList)
   IF (p%FreqList(I) <= Frequency_cutoff) THEN
       Directivity = DBARL
   ELSE
       Directivity = DBARH
   ENDIF
   WaveNumber = PI*p%FreqList(I)*Chord/U
   Beta2 = 1-Mach*Mach
   SPLhigh = 10.*LOG10(p%AirDens*p%AirDens*p%SpdSound*p%SpdSound*LTurb*(d/2.)/(RObs*RObs)*(Mach**3)*Ums* &
             (WaveNumber**3)*(1+WaveNumber**2)**(-7./3.)*Directivity) + 58.4
   Sears = 1/(2*PI*WaveNumber/Beta2+1/(1+2.4*WaveNumber/Beta2))
   LFC = 10*Sears*Mach*WaveNumber*WaveNumber/Beta2
   SPLti(I) = SPLhigh + 10.*LOG10(LFC/(1+LFC))
ENDDO

RETURN
END SUBROUTINE InflowAeroAcoustics
!====================================================================================================
  SUBROUTINE BLUNT(ALPSTAR,C,U ,THETA,PHI,L,R,H,PSI,p,SPLBLUNT,errStat,errMsg)
  REAL(ReKi),           	                  INTENT(IN   )  :: ALPSTAR        ! AOA
  REAL(ReKi),           	                  INTENT(IN   )  :: C              ! Chord Length
  REAL(ReKi),           	                  INTENT(IN   )  :: U              ! Unoise
  REAL(ReKi),           	                  INTENT(IN   )  :: THETA          ! DIRECTIVITY ANGLE                     ---
  REAL(ReKi),           	                  INTENT(IN   )  :: PHI            ! DIRECTIVITY ANGLE                     ---
  REAL(ReKi),           	                  INTENT(IN   )  :: L              ! SPAN                                  METERS
  REAL(ReKi),           	                  INTENT(IN   )  :: R              ! SOURCE TO OBSERVER DISTANCE           METERS 
  REAL(ReKi),           	                  INTENT(IN   )  :: H              ! TRAILING EDGE BLUNTNESS              METERS
  REAL(ReKi),           	                  INTENT(IN   )  :: PSI            ! TRAILING EDGE ANGLE                  DEGREES 
  TYPE(AA_ParameterType),                     INTENT(IN   )  :: p              ! Parameters
  
  REAL(ReKi),DIMENSION(size(p%FreqList)),	  INTENT(  OUT)  :: SPLBLUNT       !
  INTEGER(IntKi),                   		  INTENT(  OUT)  :: errStat        ! Error status of the operation
  character(*),                               INTENT(  OUT)  :: errMsg         ! Error message if ErrStat /= ErrID_None

  integer(intKi)                                             :: ErrStat2           ! temporary Error status
  character(ErrMsgLen)                                       :: ErrMsg2            ! temporary Error message
  character(*), parameter                                    :: RoutineName = 'BLUNT'

   ! Local variables
   !!real(ReKi)                                    :: STPPP(size(p%FreqList))  !EB_DTU does not need to be a vector 
   real(ReKi)                                    :: STPPP    ! STROUHAL NUMBER                       ---
   real(ReKi)                                    :: M        ! MACH NUMBER                           ---
   real(ReKi)                                    :: RC       ! REYNOLDS NUMBER BASED ON CHORD        ---
   integer(intKi)		                 :: I        ! I A generic index for DO loops.
   real(ReKi)                                    :: DELTAP   ! PRESSURE SIDE BOUNDARY LAYER THICKNESS METERS
   real(ReKi)                                    :: DSTRS    ! SUCTION SIDE DISPLACEMENT THICKNESS  METERS
   real(ReKi)                                    :: DSTRP    ! PRESSURE SIDE DISPLACEMENT THICKNESS METERS
   real(ReKi)                                    :: DBARH    ! HIGH FREQUENCY DIRECTIVITY           ---
   real(ReKi)                                    :: DSTRAVG  ! AVERAGE DISPLACEMENT THICKNESS       METERS
   real(ReKi)                                    :: HDSTAR   ! BLUNTNESS OVER AVERAGE DISPLACEMENT THICKNESS   ---
   real(ReKi)                                    :: DSTARH   ! AVERAGE DISPLACEMENT THICKNESS OVER TRAILING EDGE BLUNTNESS       ---
   real(ReKi)                                    :: ATERM    ! USED TO COMPUTE PEAK STROUHAL NO.    ---
   real(ReKi)                                    :: STPEAK   ! PEAK STROUHAL NUMBER                  ---
   real(ReKi)                                    :: ETA      ! RATIO OF STROUHAL NUMBERS             ---
   real(ReKi)                                    :: HDSTARL  ! MINIMUM ALLOWED VALUE OF HDSTAR       ---
   real(ReKi)                                    :: G514     ! G5 EVALUATED AT PSI=14.0              DB
   real(ReKi)                                    :: HDSTARP  ! MODIFIED VALUE OF HDSTAR              ---
   real(ReKi)                                    :: G50      ! G5 EVALUATED AT PSI=0.0               DB
   real(ReKi)                                    :: G4       ! SCALED SPECTRUM LEVEL                 DB
   real(ReKi)                                    :: G5       ! SPECTRUM SHAPE FUNCTION               DB
   real(ReKi)                                    :: F4TEMP   ! G5 EVALUATED AT MINIMUM HDSTARP       DB
   real(ReKi)                                    :: SCALE    ! SCALING FACTOR                        ---

   ErrStat = ErrID_None
   ErrMsg  = ""



!!!      COMPUTE REYNOLDS NUMBER AND MACH NUMBER
!!!      ---------------------------------------
        M          = U  / p%SpdSound
        RC         = U  * C/p%KinVisc
!!!      COMPUTE BOUNDARY LAYER THICKNESSES
!!!      ----------------------------------
       CALL THICK(C,M,RC,ALPSTAR,p,DELTAP,DSTRS,DSTRP,errStat2,errMsg2)
		     CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )  


!!!      COMPUTE AVERAGE DISPLACEMENT THICKNESS
!!!      --------------------------------------

      DSTRAVG = (DSTRS + DSTRP) / 2.
      HDSTAR  = H / DSTRAVG

      DSTARH = 1. /HDSTAR

!!!      COMPUTE DIRECTIVITY FUNCTION
!!!      ----------------------------
       CALL DIRECTH(M,THETA,PHI,DBARH,errStat2,errMsg2)
		     CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName ) 
      IF (DBARH <= 0) THEN
          SPLBLUNT = 0.
          RETURN
      ENDIF

!!!      COMPUTE PEAK STROUHAL NUMBER
!!!      ----------------------------
      ATERM  = .212 - .0045 * PSI

      IF (HDSTAR .GE. .2) &
        STPEAK    = ATERM / (1.+.235*DSTARH-.0132*DSTARH**2.)
      IF (HDSTAR .LT. .2) &
        STPEAK    = .1 * HDSTAR + .095 - .00243 * PSI

!!!      COMPUTE SCALED SPECTRUM LEVEL
!!!      -----------------------------

      IF (HDSTAR .LE. 5.) G4=17.5*LOG10(HDSTAR)+157.5-1.114*PSI
      IF (HDSTAR .GT. 5.) G4=169.7 - 1.114 * PSI


!!!      FOR EACH FREQUENCY, COMPUTE SPECTRUM SHAPE REFERENCED TO 0 DB
!!!      -------------------------------------------------------------

      DO I=1,SIZE(p%FreqList)
	  
        STPPP    = p%FreqList(I) * H / U
        ETA      = LOG10(STPPP/STPEAK)

        HDSTARL = HDSTAR

        CALL G5COMP(HDSTARL,ETA,G514,errStat2,errMsg2 )
		  CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName ) 

        HDSTARP = 6.724 * HDSTAR **2.-4.019*HDSTAR+1.107

        CALL G5COMP(HDSTARP,ETA,G50,errStat2,errMsg2 )
		  CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName ) 


        G5 = G50 + .0714 * PSI * (G514-G50)
        IF (G5 .GT. 0.) G5 = 0.
        CALL G5COMP(.25,ETA,F4TEMP,errStat2,errMsg2 )
		  CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName ) 
        IF (G5 .GT. F4TEMP) G5 = F4TEMP


        SCALE = 10. * LOG10(M**5.5*H*DBARH*L/R**2.)

        SPLBLUNT(I) = G4 + G5 + SCALE

	END DO

      RETURN

      END SUBROUTINE Blunt
	  
	  
!====================================================================================================
      SUBROUTINE G5COMP(HDSTAR,ETA,G5,errStat,errMsg)
	  
	  
  REAL(ReKi),           	                  INTENT(IN   )  :: HDSTAR         !
  REAL(ReKi),           	                  INTENT(IN   )  :: ETA            ! 

  REAL(ReKi),           	                  INTENT(  OUT)  :: G5            ! 

  INTEGER(IntKi),                   		  INTENT(  OUT)  :: errStat        ! Error status of the operation
  CHARACTER(*),                                   INTENT(  OUT)  :: errMsg         ! Error message if ErrStat /= ErrID_None

  INTEGER(intKi)                                                 :: ErrStat2           ! temporary Error status
  CHARACTER(ErrMsgLen)                                           :: ErrMsg2            ! temporary Error message
  CHARACTER(*), parameter                                        :: RoutineName = 'BLUNT'

   ! Local variables
   real(ReKi)                                    :: K 
   real(ReKi)                                    :: M
   real(ReKi)                                    :: MU
   real(ReKi)                                    :: ETALIMIT
   real(ReKi)                                    :: ETA0

	  
   ErrStat = ErrID_None
   ErrMsg  = ""


      IF (HDSTAR .LT. .25) MU = .1211
      IF ((HDSTAR .GT. .25).AND.(HDSTAR .LE. .62)) &
          MU=-.2175*HDSTAR + .1755
      IF ((HDSTAR .GT. .62).AND.(HDSTAR .LT. 1.15)) &
       MU = -.0308 * HDSTAR + .0596
      IF (HDSTAR .GE. 1.15)MU = .0242

      IF (HDSTAR .LE. .02) M = 0.0
      IF ((HDSTAR .GE. .02).AND.(HDSTAR .LT. .5)) &
          M=68.724*HDSTAR - 1.35
      IF ((HDSTAR .GT. .5).AND.(HDSTAR .LE. .62)) &
        M = 308.475 * HDSTAR - 121.23
      IF ((HDSTAR .GT. .62).AND.(HDSTAR .LE. 1.15)) &
        M = 224.811 * HDSTAR - 69.354
      IF ((HDSTAR .GT. 1.15) .AND. (HDSTAR .LT. 1.2)) &
        M = 1583.28 * HDSTAR - 1631.592
      IF (HDSTAR .GT. 1.2) M = 268.344
      IF (M .LT. 0.0) M = 0.0

      ETA0 = -SQRT((M*M*MU**4)/(6.25+M*M*MU*MU))

      K    = 2.5*SQRT(1.-(ETA0/MU)**2.)-2.5-M*ETA0

      ETALIMIT = 0.03615995

      IF (ETA .LE. ETA0) G5 = M * ETA + K
      IF ((ETA .GT. ETA0).AND.(ETA .LE. 0.))G5=2.5*SQRT(1.-(ETA/MU)**2.)-2.5
      IF((ETA.GT.0.).AND.(ETA.LE.ETALIMIT))G5=SQRT(1.5625-1194.99*ETA**2.)-1.25
      IF (ETA .GT. ETALIMIT) G5=-155.543 * ETA + 4.375

      RETURN

      END SUBROUTINE G5Comp
!====================================================================================================

      SUBROUTINE AMIN(A,AMINA)
	REAL(ReKi),           	                  INTENT(IN   )  :: A              ! 
	REAL(ReKi),           	                  INTENT(OUT  )  :: AMINA              ! 
!     THIS SUBROUTINE DEFINES THE CURVE FIT CORRESPONDING
!     TO THE A-CURVE FOR THE MINIMUM ALLOWED REYNOLDS NUMBER.
	REAL(ReKi) :: X1

      X1 = ABS(A)

      IF (X1 .LE. .204) AMINA=SQRT(67.552-886.788*X1**2.)-8.219
      IF((X1 .GT. .204).AND.(X1 .LE. .244))AMINA=-32.665*X1+3.981
      IF (X1 .GT. .244)AMINA=-142.795*X1**3.+103.656*X1**2.-57.757*X1+6.006

      RETURN

      END SUBROUTINE AMIN
!====================================================================================================
      SUBROUTINE AMAX(A,AMAXA)
	REAL(ReKi),           	                  INTENT(IN   )  :: A              ! 
	REAL(ReKi),           	                  INTENT(OUT  )  :: AMAXA              !
!     THIS SUBROUTINE DEFINES THE CURVE FIT CORRESPONDING
!     TO THE A-CURVE FOR THE MAXIMUM ALLOWED REYNOLDS NUMBER.
	REAL(ReKi) :: X1

      X1 = ABS(A)

      IF (X1 .LE. .13)AMAXA=SQRT(67.552-886.788*X1**2.)-8.219
      IF((X1 .GT. .13).AND.(X1 .LE. .321))AMAXA=-15.901*X1+1.098
      IF (X1 .GT. .321)AMAXA=-4.669*X1**3.+3.491*X1**2.-16.699*X1+1.149

      RETURN

      END SUBROUTINE AMAX
!====================================================================================================
      SUBROUTINE BMIN(B,BMINB)
	REAL(ReKi),           	                  INTENT(IN   )  :: B              ! 
	REAL(ReKi),           	                  INTENT(OUT  )  :: BMINB              !
!     THIS SUBROUTINE DEFINES THE CURVE FIT CORRESPONDING
!     TO THE B-CURVE FOR THE MINIMUM ALLOWED REYNOLDS NUMBER.
	REAL(ReKi) :: X1

      X1 = ABS(B)

      IF (X1 .LE. .13)BMINB=SQRT(16.888-886.788*X1**2.)-4.109
      IF((X1 .GT. .13).AND.(X1 .LE. .145))BMINB=-83.607*X1+8.138
      IF (X1.GT..145)BMINB=-817.81*X1**3.+355.21*X1**2.-135.024*X1+10.619

      RETURN

      END SUBROUTINE BMin
!====================================================================================================
      SUBROUTINE BMAX(B,BMAXB)
	REAL(ReKi),           	                  INTENT(IN   )  :: B              ! 
	REAL(ReKi),           	                  INTENT(OUT  )  :: BMAXB              !
!     THIS SUBROUTINE DEFINES THE CURVE FIT CORRESPONDING
!     TO THE B-CURVE FOR THE MAXIMUM ALLOWED REYNOLDS NUMBER.
	REAL(ReKi) :: X1
      X1 = ABS(B)

      IF (X1 .LE. .1) BMAXB=SQRT(16.888-886.788*X1**2.)-4.109
      IF((X1 .GT. .1).AND.(X1 .LE. .187))BMAXB=-31.313*X1+1.854
      IF (X1.GT..187)BMAXB=-80.541*X1**3.+44.174*X1**2.-39.381*X1+2.344

      RETURN

      END SUBROUTINE BMax
!====================================================================================================
      SUBROUTINE A0COMP(RC,A0)
	REAL(ReKi),           	                  INTENT(IN   )  :: RC              ! 
	REAL(ReKi),           	                  INTENT(OUT  )  :: A0              !
!     THIS SUBROUTINE DETERMINES WHERE THE A-CURVE
!     TAKES ON A VALUE OF -20 dB.

      IF (RC .LT. 9.52E+04) A0 = .57
      IF ((RC .GE. 9.52E+04).AND.(RC .LT. 8.57E+05)) &
         A0 = (-9.57E-13)*(RC-8.57E+05)**2. + 1.13
      IF (RC .GE. 8.57E+05) A0 = 1.13
      RETURN

      END SUBROUTINE A0COMP
!====================================================================================================

  SUBROUTINE THICK(C,M,RC,ALPSTAR,p,DELTAP,DSTRS,DSTRP,errStat,errMsg)
!!                  --------------------------------
!!                  ***** VARIABLE DEFINITIONS *****
!!                  --------------------------------
!!
!!       VARIABLE NAME               DEFINITION                  UNITS
!!       -------------               ----------                  -----
!!
!!       ALPSTAR            ANGLE OF ATTACK                    DEGREES
!!       C                  CHORD LENGTH                        METERS
!!       C0                 SPEED OF SOUND                    METERS/SEC
!!       DELTA0             BOUNDARY LAYER THICKNESS AT
!!                            ZERO ANGLE OF ATTACK              METERS
!!       DELTAP             PRESSURE SIDE BOUNDARY LAYER
!!                            THICKNESS                         METERS
!!       DSTR0              DISPLACEMENT THICKNESS AT ZERO
!!                            ANGLE OF ATTACK                   METERS
!!       DSTRP              PRESSURE SIDE DISPLACEMENT
!!                            THICKNESS                         METERS
!!       DSTRS              SUCTION SIDE DISPLACEMENT
!!                            THICKNESS                         METERS
!!       ITRIP              TRIGGER FOR BOUNDARY LAYER TRIPPING  ---
!!       M                  MACH NUMBER                          ---
!!       RC                 REYNOLDS NUMBER BASED ON CHORD       ---
!!       U                  FREESTREAM VELOCITY                METERS/SEC
!!       KinViscosity       KINEMATIC VISCOSITY                M2/SEC
!!
!!
!!      COMPUTE ZERO ANGLE OF ATTACK BOUNDARY LAYER
!!      THICKNESS (METERS) AND REYNOLDS NUMBER
!!      -------------------------------------------
  REAL(ReKi),           	                  INTENT(IN   )  :: ALPSTAR        ! AOA
  REAL(ReKi),           	                  INTENT(IN   )  :: C              ! Chord Length
  REAL(ReKi),           	                  INTENT(IN   )  :: RC             !
  REAL(ReKi),           	                  INTENT(IN   )  :: M              !
  TYPE(AA_ParameterType),                         INTENT(IN   )  :: p                ! Parameters

  REAL(ReKi),                            	  INTENT(  OUT)  :: DELTAP         !
  REAL(ReKi),                            	  INTENT(  OUT)  :: DSTRS          !
  REAL(ReKi),                            	  INTENT(  OUT)  :: DSTRP          !
  INTEGER(IntKi),                   		  INTENT(  OUT)  :: errStat        ! Error status of the operation
  character(*),                                   INTENT(  OUT)  :: errMsg         ! Error message if ErrStat /= ErrID_None
  integer(intKi)                                             :: ErrStat2           ! temporary Error status
  character(ErrMsgLen)                                       :: ErrMsg2            ! temporary Error message
  character(*), parameter                                    :: RoutineName = 'Thick'

      ! Initialize variables for this routine
   !real(ReKi)                                    :: STPRIM(size(p%FreqList))      
   real(ReKi)                                    :: DELTA0              ! BOUNDARY LAYER THICKNESS AT ZERO ANGLE OF ATTACK METERS
   real(ReKi)                                    :: DSTR0 		! DISPLACEMENT THICKNESS AT ZERO   ANGLE OF ATTACK METERS

   ErrStat = ErrID_None
   ErrMsg  = ""

!      M        = U  / C0
!
!      RC       = U  * C/KinViscosity
!
      DELTA0   = 10.**(1.6569-.9045*LOG10(RC)+ &
                .0596*LOG10(RC)**2.)*C
      IF (p%ITRIP .EQ. 2) DELTA0 = .6 * DELTA0
!!      COMPUTE PRESSURE SIDE BOUNDARY LAYER THICKNESS
!!      ----------------------------------------------
      DELTAP   = 10.**(-.04175*ALPSTAR+.00106*ALPSTAR**2.)*DELTA0

!!      COMPUTE ZERO ANGLE OF ATTACK DISPLACEMENT THICKNESS
!!      ---------------------------------------------------
      IF ((p%ITRIP .EQ. 1) .OR. (p%ITRIP .EQ. 2)) THEN
        IF (RC .LE. .3E+06) DSTR0 = .0601 * RC **(-.114)*C
        IF (RC .GT. .3E+06) &
         DSTR0=10.**(3.411-1.5397*LOG10(RC)+.1059*LOG10(RC)**2.)*C
        IF (p%ITRIP .EQ. 2) DSTR0 = DSTR0 * .6
      ELSE
        DSTR0=10.**(3.0187-1.5397*LOG10(RC)+.1059*LOG10(RC)**2.)*C
      ENDIF
!!      PRESSURE SIDE DISPLACEMENT THICKNESS
!!      ------------------------------------
       DSTRP   = 10.**(-.0432*ALPSTAR+.00113*ALPSTAR**2.)*DSTR0
      IF (p%ITRIP .EQ. 3) DSTRP = DSTRP * 1.48

!!      SUCTION SIDE DISPLACEMENT THICKNESS
!!      -----------------------------------
      IF (p%ITRIP .EQ. 1) THEN
        IF (ALPSTAR .LE. 5.) DSTRS=10.**(.0679*ALPSTAR)*DSTR0
        IF((ALPSTAR .GT. 5.).AND.(ALPSTAR .LE. 12.5)) &
         DSTRS = .381*10.**(.1516*ALPSTAR)*DSTR0
        IF (ALPSTAR .GT. 12.5)DSTRS=14.296*10.**(.0258*ALPSTAR)*DSTR0
      ELSE
        IF (ALPSTAR .LE. 7.5)DSTRS =10.**(.0679*ALPSTAR)*DSTR0
        IF((ALPSTAR .GT. 7.5).AND.(ALPSTAR .LE. 12.5)) &
         DSTRS = .0162*10.**(.3066*ALPSTAR)*DSTR0
        IF (ALPSTAR .GT. 12.5) DSTRS = 52.42*10.**(.0258*ALPSTAR)*DSTR0
      ENDIF

      RETURN

      END SUBROUTINE Thick

!====================================================================================================
      SUBROUTINE DIRECTH(M,THETA,PHI,DBAR, errStat, errMsg)

!     THIS SUBROUTINE COMPUTES THE HIGH FREQUENCY
!     DIRECTIVITY FUNCTION FOR THE INPUT OBSERVER LOCATION
  REAL(ReKi),           	                  INTENT(IN   )  :: THETA       ! 
  REAL(ReKi),           	                  INTENT(IN   )  :: PHI        ! 
  REAL(ReKi),           	                  INTENT(IN   )  :: M              !

  REAL(ReKi),                            	  INTENT(  OUT)  :: DBAR         !
  INTEGER(IntKi),                   		  INTENT(  OUT)  :: errStat        ! Error status of the operation
  character(*),                               INTENT(  OUT)  :: errMsg         ! Error message if ErrStat /= ErrID_None
  integer(intKi)                                             :: ErrStat2           ! temporary Error status
  character(ErrMsgLen)                                       :: ErrMsg2            ! temporary Error message
  character(*), parameter                                    :: RoutineName = 'Directh'

 ! Initialize variables for this routine
   real(ReKi)                                    :: MC
   real(ReKi)                                    :: DEGRAD
   real(ReKi)                                    :: PHIR
   real(ReKi)                                    :: THETAR

   ErrStat = ErrID_None
   ErrMsg  = ""


      DEGRAD  = .017453

      MC     = .8 * M
      THETAR = THETA * DEGRAD
      PHIR   = PHI * DEGRAD

      DBAR=2.*SIN(THETAR/2.)**2.*SIN(PHIR)**2./((1.+M*COS(THETAR))* &
           (1.+(M-MC)*COS(THETAR))**2.)
      RETURN

      END SUBROUTINE DirectH
!====================================================================================================
      SUBROUTINE DIRECTL(M,THETA,PHI,DBAR, errStat, errMsg)
!     THIS SUBROUTINE COMPUTES THE HIGH FREQUENCY
!     DIRECTIVITY FUNCTION FOR THE INPUT OBSERVER LOCATION
  REAL(ReKi),           	                  INTENT(IN   )  :: THETA       ! 
  REAL(ReKi),           	                  INTENT(IN   )  :: PHI        ! 
  REAL(ReKi),           	                  INTENT(IN   )  :: M              !

  REAL(ReKi),                            	  INTENT(  OUT)  :: DBAR         !
  INTEGER(IntKi),                   		  INTENT(  OUT)  :: errStat        ! Error status of the operation
  character(*),                               INTENT(  OUT)  :: errMsg         ! Error message if ErrStat /= ErrID_None
  integer(intKi)                                             :: ErrStat2           ! temporary Error status
  character(ErrMsgLen)                                       :: ErrMsg2            ! temporary Error message
  character(*), parameter                                    :: RoutineName = 'DirectL'

 ! Initialize variables for this routine
   real(ReKi)                                    :: MC
   real(ReKi)                                    :: DEGRAD
   real(ReKi)                                    :: PHIR
   real(ReKi)                                    :: THETAR

   ErrStat = ErrID_None
   ErrMsg  = "" 
	  
!     THIS SUBROUTINE COMPUTES THE LOW FREQUENCY
!     DIRECTIVITY FUNCTION FOR THE INPUT OBSERVER LOCATION

      DEGRAD  = .017453

      MC     = .8 * M
      THETAR = THETA * DEGRAD
      PHIR   = PHI * DEGRAD

      DBAR = (SIN(THETAR)*SIN(PHIR))**2/(1.+M*COS(THETAR))**4

      RETURN

      END SUBROUTINE DirectL
!====================================================================================================

	  END MODULE AeroAcoustics

