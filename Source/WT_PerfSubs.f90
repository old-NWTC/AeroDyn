module WTP_DvrSubs
   
   use NWTC_Library
   use WTP_Types
   use AeroDyn_Types
   use NWTC_Library
   
    
   implicit none   
   
   contains

   
   
   subroutine Set_AD_InitInp(WTP_Data, AD_Data, errStat, errMsg)

      type(WTP_InputFileData), intent(in   )   :: WTP_Data             ! Input data for initialization
      type(AD_InitInputType) , intent(  out)   :: AD_Data              ! Input data for initialization
      integer(IntKi)         , intent(inout)   :: errStat              ! Status of error message
      character(*)           , intent(inout)   :: errMsg               ! Error message if ErrStat /= ErrID_None

         ! locals
      integer(IntKi)                           :: i, j
      integer(IntKi)                           :: errStat2             ! local status of error message
      character(len(errMsg))                   :: errMsg2              ! local error message if ErrStat /= ErrID_None
      real(ReKi)                               :: rHub, zTip, deltar
      errStat2 = ErrID_None
      errMsg2  = ''
   
      AD_Data%numBladeNodes  = WTP_Data%numSeg
      AD_Data%numBlades      = WTP_Data%numBlade
      AD_Data%NumAF          = WTP_Data%NumAF
      
      
      AD_Data%BEMT%numBladeNodes  = WTP_Data%numSeg
      AD_Data%BEMT%numBlades      = WTP_Data%numBlade
      
   
      allocate ( AD_Data%chord(AD_Data%numBladeNodes, AD_Data%numBlades), STAT = errStat2 )
      if ( errStat2 /= 0 ) then
         errStat2 = ErrID_Fatal
         errMsg2  = 'Error allocating memory for AD_Data%chord.'
         call SetErrStat( errStat2, errMsg2, errStat, errMsg, 'Set_AD_InitInp' )
         return
      end if 
   
      allocate ( AD_Data%AFindx(AD_Data%numBladeNodes), STAT = errStat2 )
      if ( errStat2 /= 0 ) then
         errStat2 = ErrID_Fatal
         errMsg2  = 'Error allocating memory for AD_Data%chord array.'
         call SetErrStat( errStat2, errMsg2, errStat, errMsg, 'Set_AD_InitInp' )
         return
      end if 
      
      allocate ( AD_Data%zLocal(AD_Data%numBladeNodes, AD_Data%numBlades), STAT = errStat2 )
      if ( errStat2 /= 0 ) then
         errStat2 = ErrID_Fatal
         errMsg2  = 'Error allocating memory for AD_Data%zLocal array.'
         call SetErrStat( errStat2, errMsg2, errStat, errMsg, 'Set_AD_InitInp' )
         return
      end if 
   
      allocate ( AD_Data%zTip(AD_Data%numBlades), STAT = errStat2 )
      if ( errStat2 /= 0 ) then
         errStat2 = ErrID_Fatal
         errMsg2  = 'Error allocating memory for AD_Data%zTip array.'
         call SetErrStat( errStat2, errMsg2, errStat, errMsg, 'Set_AD_InitInp' )
         return
      end if 
      
      allocate ( AD_Data%zHub(AD_Data%numBlades), STAT = errStat2 )
      if ( errStat2 /= 0 ) then
         errStat2 = ErrID_Fatal
         errMsg2  = 'Error allocating memory for AD_Data%rHub array.'
         call SetErrStat( errStat2, errMsg2, errStat, errMsg, 'Set_AD_InitInp' )
         return
      end if 
   
      
      allocate ( AD_Data%BEMT%chord(AD_Data%numBladeNodes, AD_Data%numBlades), STAT = errStat2 )
      if ( errStat2 /= 0 ) then
         errStat2 = ErrID_Fatal
         errMsg2  = 'Error allocating memory for AD_Data%chord.'
         call SetErrStat( errStat2, errMsg2, errStat, errMsg, 'Set_AD_InitInp' )
         return
      end if 
   
      allocate ( AD_Data%BEMT%AFindx(AD_Data%numBladeNodes), STAT = errStat2 )
      if ( errStat2 /= 0 ) then
         errStat2 = ErrID_Fatal
         errMsg2  = 'Error allocating memory for AD_Data%chord array.'
         call SetErrStat( errStat2, errMsg2, errStat, errMsg, 'Set_AD_InitInp' )
         return
      end if 
      
      allocate ( AD_Data%BEMT%zLocal(AD_Data%numBladeNodes, AD_Data%numBlades), STAT = errStat2 )
      if ( errStat2 /= 0 ) then
         errStat2 = ErrID_Fatal
         errMsg2  = 'Error allocating memory for AD_Data%zLocal array.'
         call SetErrStat( errStat2, errMsg2, errStat, errMsg, 'Set_AD_InitInp' )
         return
      end if 
   
      allocate ( AD_Data%BEMT%zTip(AD_Data%numBlades), STAT = errStat2 )
      if ( errStat2 /= 0 ) then
         errStat2 = ErrID_Fatal
         errMsg2  = 'Error allocating memory for AD_Data%zTip array.'
         call SetErrStat( errStat2, errMsg2, errStat, errMsg, 'Set_AD_InitInp' )
         return
      end if 
      
      allocate ( AD_Data%BEMT%zHub(AD_Data%numBlades), STAT = errStat2 )
      if ( errStat2 /= 0 ) then
         errStat2 = ErrID_Fatal
         errMsg2  = 'Error allocating memory for AD_Data%rHub array.'
         call SetErrStat( errStat2, errMsg2, errStat, errMsg, 'Set_AD_InitInp' )
         return
      end if 
      
      allocate ( AD_Data%AF_File(AD_Data%NumAF), STAT = errStat2 )
      if ( errStat2 /= 0 ) then
         errStat2 = ErrID_Fatal
         errMsg2  = 'Error allocating memory for AD_Data%AF_File array.'
         call SetErrStat( errStat2, errMsg2, errStat, errMsg, 'Set_AD_InitInp' )
         return
      end if 
      
      allocate ( AD_Data%BEMT%AF_File(AD_Data%NumAF), STAT = errStat2 )
      if ( errStat2 /= 0 ) then
         errStat2 = ErrID_Fatal
         errMsg2  = 'Error allocating memory for AD_Data%BEMT%AF_File array.'
         call SetErrStat( errStat2, errMsg2, errStat, errMsg, 'Set_AD_InitInp' )
         return
      end if 
      
      do i=1, AD_Data%NumAF
         AD_Data%AF_File(i) =  WTP_Data%AF_File(i)
         AD_Data%BEMT%AF_File(i) =  WTP_Data%AF_File(i)
      end do
      
      do i=1,AD_Data%numBladeNodes
         
         AD_Data%AFindx(i) = WTP_Data%BladeData(i)%AFfile  
         AD_Data%BEMT%AFindx(i) = WTP_Data%BladeData(i)%AFfile  
      end do
   
      !zTip = 40.0
      !rHub =  2.0
      !deltar     = ( zTip-rHub ) / real(AD_Data%numBladeNodes + 1) 
      do j=1,AD_Data%numBlades
         AD_Data%zTip(j)  = WTP_Data%rotorRad  ! zTip
         AD_Data%zHub(j)  = WTP_Data%hubRad    ! rHub
         AD_Data%BEMT%zTip(j)  = WTP_Data%rotorRad  ! zTip
         AD_Data%BEMT%zHub(j)  = WTP_Data%hubRad    ! rHub
         
         do i=1,AD_Data%numBladeNodes
            AD_Data%chord (i,j)  = WTP_Data%BladeData(i)%Chord   ! 0.5 !real(i) 
            AD_Data%zLocal(i,j)  = WTP_Data%BladeData(i)%RLoc    ! AD_Data%zHub(j) + real(i)*(deltar)
            AD_Data%BEMT%chord (i,j)  = WTP_Data%BladeData(i)%Chord   ! 0.5 !real(i) 
            AD_Data%BEMT%zLocal(i,j)  = WTP_Data%BladeData(i)%RLoc    ! AD_Data%zHub(j) + real(i)*(deltar)
         end do
      end do
      
      AD_Data%DT               = 0.25 ! seconds   - not currently used                    
      AD_Data%airDens          = WTP_Data%AirDens  !  1.246 kg/m^3    
      AD_Data%kinVisc          = WTP_Data%KinVisc  !  1.4639e-5 ! m^2/s
      AD_Data%skewWakeMod      = WTP_Data%skewWakeMod 
      AD_Data%useTipLoss       = WTP_Data%useTipLoss
      AD_Data%useHubLoss       = WTP_Data%useHubLoss
      AD_Data%useTanInd        = WTP_Data%useTanInd
      AD_Data%useAIDrag        = WTP_Data%useAIDrag
      AD_Data%useTIDrag        = WTP_Data%useTIDrag    
      AD_Data%numReIterations  = 1                   ! This is currently not available in the input file and is only for testing
      AD_Data%maxIndIterations = WTP_Data%maxIter
      AD_Data%BEMT_SkewWakeMod = WTP_Data%skewWakeMod    ! This is needed because AD needs to branch according to the solution technique used within BEMT
      
      
      AD_Data%BEMT%DT               = 0.25 ! seconds   - not currently used                    
      AD_Data%BEMT%airDens          = WTP_Data%AirDens  !  1.246 kg/m^3    
      AD_Data%BEMT%kinVisc          = WTP_Data%KinVisc  !  1.4639e-5 ! m^2/s
      AD_Data%BEMT%skewWakeMod      = WTP_Data%skewWakeMod 
      AD_Data%BEMT%useTipLoss       = WTP_Data%useTipLoss
      AD_Data%BEMT%useHubLoss       = WTP_Data%useHubLoss
      AD_Data%BEMT%useInduction     = WTP_Data%useInduction
      AD_Data%BEMT%useTanInd        = WTP_Data%useTanInd
      AD_Data%BEMT%useAIDrag        = WTP_Data%useAIDrag
      AD_Data%BEMT%useTIDrag        = WTP_Data%useTIDrag    
      AD_Data%BEMT%numReIterations  = 1                   ! This is currently not available in the input file and is only for testing
      AD_Data%BEMT%maxIndIterations = WTP_Data%maxIter
      AD_Data%BEMT%aTol             = WTP_Data%ATol
     
   end subroutine Set_AD_InitInp

   !subroutine WTP_Dvr_SetInputs(n, u, WTP_Data, InitInData, errStat, errMsg )
   !
   !   integer(IntKi)         , intent(in   )   :: n                    ! time step counter
   !   type(AD_InputType),     intent(inout)   :: u                    ! Input data
   !   type(WTP_InputFileData), intent(in   )   :: WTP_Data
   !   type(AD_InitInputType), intent(in   )   :: InitInData           ! Input data for initialization
   !   integer(IntKi)         , intent(inout)   :: errStat              ! Status of error message
   !   character(*)           , intent(inout)   :: errMsg               ! Error message if ErrStat /= ErrID_None
   !
   !      ! locals
   !   integer(IntKi)                           :: i, j
   !   integer(IntKi)                           :: errStat2             ! local status of error message
   !   character(len(errMsg))                   :: errMsg2              ! local error message if ErrStat /= ErrID_None
   !   real(ReKi)                               :: velocityHub, psiRotor, deltar
   !   
   !   
   !   errStat2 = ErrID_None
   !   errMsg2  = ''
   !   
   !      ! Velocity of inflow wind at the hub height
   !   velocityHub = 10.0 ! m/s
   !      ! Zero yaw for now
   !   u%gamma = 0.0_ReKi
   !      ! Rotor angular velocity
   !   u%omega = 2.0*pi  ! 60 rev per minute in units of rad/s
   !      ! Average tip-speed ratio
   !   u%lambda = u%omega*InitInData%zTip(1) / velocityHub
   !      ! Angular position of the rotor, starts at 0.0 when n = 0.0 
   !   psiRotor = (n-1)*u%omega*n*InitInData%DT  
   !   do j=1,InitInData%numBlades
   !      u%psi(j)   = psiRotor * (j-1)*2.0*pi/(InitInData%numBlades*3.0) ! find psi for each blade based on the rotor psi value
   !      u%rTip(j)  = InitInData%zTip(j) ! set the tip radius to the initialization distance along the blade (straight blade assumption)
   !      deltar     = ( InitInData%zTip(j)-InitInData%zHub(j) ) / real(InitInData%numBladeNodes + 1)  
   !      do i=1,InitInData%numBladeNodes
   !         u%theta (i,j)  = 0.0_ReKi
   !         u%Vx    (i,j)  = velocityHub   ! for now just set the velocity perp to rotation plane = hub velocity of inflow wind
   !         u%rLocal(i,j)  = InitInData%zHub(j) + real(i)*(deltar)
   !         u%Vy    (i,j)  = u%omega*u%rLocal(i,j)                     
   !      end do
   !   end do
   !   
   !end subroutine WTP_Dvr_SetInputs
   
   
   !subroutine Alloc_AD_u( u, numInp, errStat, errMsg)
   !
   !
   !   type(AD_InputType)     , intent(  out)   :: u(:)            ! Input data (guess)
   !   integer(IntKi)         , intent(in   )   :: numInp               ! number of inputs
   !   integer(IntKi)         , intent(inout)   :: errStat              ! Status of error message
   !   character(*)           , intent(inout)   :: errMsg               ! Error message if ErrStat /= ErrID_None
   !
   !      ! locals
   !   integer(IntKi)                           :: errStat2             ! local status of error message
   !   character(len(errMsg))                   :: errMsg2              ! local error message if ErrStat /= ErrID_None
   !
   !   errStat2 = ErrID_None
   !   errMsg2  = ''
   !
   !   allocate ( u(numInp), STAT = errStat2 )
   !   if ( errStat2 /= 0 ) then
   !      errStat2 = ErrID_Fatal
   !      errMsg2  = 'Error allocating memory for u.'
   !      call SetErrStat( errStat2, errMsg2, errStat, errMsg, 'Alloc_AD_u' )
   !      return
   !   end if 
   !end subroutine Alloc_AD_u

!subroutine WTP_CalcOutput( t, u, p, x, xd, z, OtherState, y, ErrStat, ErrMsg )
!! Routine for computing outputs, used in both loose and tight coupling.
!! This SUBROUTINE is used to compute the output channels (motions and loads) and place them in the WriteOutput() array.
!! NOTE: the descriptions of the output channels are not given here. Please see the included OutListParameters.xlsx sheet for
!! for a complete description of each output parameter.
!! NOTE: no matter how many channels are selected for output, all of the outputs are calcalated
!! All of the calculated output channels are placed into the OtherState%AllOuts(:), while the channels selected for outputs are
!! placed in the y%WriteOutput(:) array.
!!..................................................................................................................................
!
!   REAL(DbKi),                   INTENT(IN   )  :: t           ! Current simulation time in seconds
!   TYPE(WTP_InputType),           INTENT(IN   )  :: u           ! Inputs at Time t
!   TYPE(WTP_ParameterType),       INTENT(IN   )  :: p           ! Parameters
!   TYPE(WTP_ContinuousStateType), INTENT(IN   )  :: x           ! Continuous states at t
!   TYPE(WTP_DiscreteStateType),   INTENT(IN   )  :: xd          ! Discrete states at t
!   TYPE(WTP_ConstraintStateType), INTENT(IN   )  :: z           ! Constraint states at t
!   TYPE(WTP_OtherStateType),      INTENT(INOUT)  :: OtherState  ! Other/optimization states
!   TYPE(WTP_OutputType),          INTENT(INOUT)  :: y           ! Outputs computed at t (Input only so that mesh con-
!                                                               !   nectivity information does not have to be recalculated)
!   INTEGER(IntKi),               INTENT(  OUT)  :: ErrStat     ! Error status of the operation
!   CHARACTER(*),                 INTENT(  OUT)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None
!
!
!      ! Local variables:
!
!  
!
!   REAL(ReKi)                   :: gamma, axInduction, tanInduction, theta, Vx, Vy, Cx, Cy, Cm, lambdar, chi, psi
!
!   INTEGER(IntKi)               :: i                                               ! Generic index
!   INTEGER(IntKi)               :: j                                               ! Loops through nodes / elements
!   integer(IntKi)               :: count
!   
!   INTEGER(IntKi)               :: ErrStat2                                        ! Temporary Error code
!   CHARACTER(LEN(ErrMsg))       :: ErrMsg2                                         ! Temporary error message
!   
!
!   LOGICAL, PARAMETER           :: UpdateValues  = .TRUE.                          ! determines if the OtherState values need to be updated
!   TYPE(WTP_ContinuousStateType) :: dxdt                                            ! Continuous state derivs at t
!
!         ! Initialize some output values
!      ErrStat = ErrID_None
!      ErrMsg  = ""
!
!      
!      ! SEE IF THESE NEED TO BE CALLED (i.e., if UpdateStates was called, these values are already calculated)
!   !IF ( UpdateValues ) THEN    
!   !      ! Update the OtherState data by calculating the derivative...
!   !   CALL WTP_CalcContStateDeriv( t, u, p, x, xd, z, OtherState, dxdt, ErrStat, ErrMsg )
!   !   CALL WTP_DestroyContState( dxdt, ErrStat2, ErrMsg2 )
!   !   IF (ErrStat >= AbortErrLev) RETURN
!   !END IF      
!
!
!   
!   
!   ! Array OtherState%AllOuts() is initialized to 0.0 in initialization, so we are not going to reinitialize it here.
!
!   !...............................................................................................................................
!   ! Calculate all of the total forces and moments using all of the partial forces and moments calculated in RtHS().  Also,
!   !   calculate all of the total angular and linear accelerations using all of the partial accelerations calculated in RtHS().
!   !   To do this, first initialize the variables using the portions not associated with the accelerations.  Then add the portions
!   !   associated with the accelerations one by one:
!   !...............................................................................................................................
!
!   gamma = u%gamma
!   count = 1
!     
!   y%WriteOutput(1  )  = u%omega*30.0/pi                          ! rotor speed in rpm
!   y%WriteOutput(2  )  = u%gamma                            ! Yaw angle
!   count = 3
!   do j = 1,p%numBlades ! Loop through all blades
!      
!      psi   = u%psi(J)
!      
!      do i = 1,p%numBladeNodes ! Loop through the blade nodes / elements
!         
!         !  local velocities and twist angle
!         Vx    = u%Vx(I,J)
!         Vy    = u%Vy(I,J)
!         theta = u%theta(I,J)
!         
!         
!         ! obtain the pre-computed induction factors and coefficients
!         axInduction  = OtherState%axInduction(I,J)
!         tanInduction = OtherState%tanInduction(I,J)
!         
!         y%Cx(I,J)    = OtherState%Cx(I,J)
!         y%Cy(I,J)    = OtherState%Cy(I,J)
!         y%AOA(I,J)   = OtherState%AOA(I,J)
!         
!           ! Estimated wake skew angle from Burton
!         chi = (0.6*axInduction + 1) * gamma
!        
!         if ( p%SkewWakeMod == 2 ) then
!            y%inducedVel(I,J) = sqrt( ( Vx*( cos(gamma) - axInduction + lambdar*tanInduction*sin(chi)*cos(psi)*( 1 + sin(chi)*sin(psi) ) ) )**2 + &
!                      ( Vy*( 1 + tanInduction*cos(chi)*( 1 + sin(chi)*sin(psi) ) ) + Vx*cos(psi)*( axInduction*tan(chi/2.0) - sin(gamma) ) )**2 )
!         else
!            y%inducedVel(I,J) = sqrt( ( Vx*( 1-axInduction) )**2 + ( Vy*( 1-tanInduction ) )**2 )
!         end if
!        
!!-------------------------------------------------------------------
!! FOR DEBUGGING UNTIL OUTPUT HANDLING IS COMPLETE: TODO - REMOVE GJH
!         ! Set the outputs for writing to file
!         y%WriteOutput(count  )  = u%theta(I,J)*R2D                            ! total twist angle
!         y%WriteOutput(count+1)  = u%psi(J)*R2D                            ! Aximuth angle
!         y%WriteOutput(count+2)  = u%Vx(I,J)
!         y%WriteOutput(count+3)  = u%Vy(I,J)
!         y%WriteOutput(count+4)  = OtherState%axInduction(I,J)
!         y%WriteOutput(count+5)  = OtherState%tanInduction(I,J)
!         y%WriteOutput(count+6)  = y%inducedVel(I,J)
!         y%WriteOutput(count+7)  = z%phi(i,j)*R2D       
!         y%WriteOutput(count+8)  = OtherState%AOA(I,J)*R2D
!         y%WriteOutput(count+9)  = OtherState%Cl(I,J)  ! new type
!         y%WriteOutput(count+10)  = OtherState%Cd(I,J)  ! new type
!         y%WriteOutput(count+11)  = OtherState%Cx(I,J)
!         y%WriteOutput(count+12) = OtherState%Cy(I,J)
!         count = count + 13
!!-------------------------------------------------------------------           
!         
!      enddo             ! I - Blade nodes / elements
!
!   enddo          ! J - All blades
!
!end subroutine WTP_CalcOutput
   
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE RunTimes( StrtTime, UsrTime1, SimStrtTime, UsrTime2, ZTime, UsrTime_out )
! This routine displays a message that gives that status of the simulation and the predicted end time of day.
!..................................................................................................................................

   IMPLICIT                        NONE

      ! Passed variables

   INTEGER   , INTENT(IN)       :: StrtTime (8)                                    ! Start time of simulation (including initialization)
   INTEGER   , INTENT(IN)       :: SimStrtTime (8)                                 ! Start time of simulation (after initialization)
   REAL      , INTENT(IN)       :: UsrTime1                                        ! User CPU time for simulation initialization.
   REAL,       INTENT(IN)       :: UsrTime2                                        ! User CPU time for simulation (without intialization)
   REAL(DbKi), INTENT(IN)       :: ZTime                                           ! The final simulation time (not necessarially TMax)
   REAL,OPTIONAL, INTENT(OUT)   :: UsrTime_out                                     ! User CPU time for entire run - optional value returned to calling routine

      ! Local variables

   REAL                         :: ClckTime                                        ! Elapsed clock time for the entire run.
   REAL                         :: ClckTimeSim                                     ! Elapsed clock time for the simulation phase of the run.
   REAL                         :: Factor                                          ! Ratio of seconds to a specified time period.
   REAL                         :: TRatio                                          ! Ratio of simulation time to elapsed clock time.
   REAL(ReKi), PARAMETER        :: SecPerDay = 24*60*60.0_ReKi                     ! Number of seconds per day

   REAL                         :: UsrTime                                         ! User CPU time for entire run.
   REAL                         :: UsrTimeSim                                      ! User CPU time for simulation (not including initialization).
   INTEGER                      :: EndTimes (8)                                    ! An array holding the ending clock time of the simulation.

   CHARACTER( 8)                :: TimePer
   CHARACTER(MaxWrScrLen)       :: BlankLine

      ! Get the end times to compare with start times.

   CALL DATE_AND_TIME ( VALUES=EndTimes )
   CALL CPU_TIME ( UsrTime )


   ! Calculate the elapsed wall-clock time in seconds.

   ClckTime     = GetClockTime(StrtTime,      EndTimes)
  !ClckTimeInit = GetClockTime(StrtTime,   SimStrtTime)
   ClckTimeSim  = GetClockTime(SimStrtTime,   EndTimes)

      ! Calculate CPU times.

   UsrTime    = UsrTime - UsrTime1
   UsrTimeSim = UsrTime - UsrTime2


   IF ( .NOT. EqualRealNos( UsrTimeSim, 0.0 ) )  THEN

      TRatio = REAL(ZTime) / UsrTimeSim

      IF     ( UsrTime > SecPerDay )  THEN
         Factor = 1.0/SecPerDay
         TimePer = ' days'
      ELSEIF ( UsrTime >  3600.0 )  THEN
         Factor = 1.0/3600.0
         TimePer = ' hours'
      ELSEIF ( UsrTime >    60.0 )  THEN
         Factor = 1.0/60.0
         TimePer = ' minutes'
      ELSE
         Factor = 1.0
         TimePer = ' seconds'
      ENDIF

      BlankLine = ""
      CALL WrOver( BlankLine )  ! BlankLine contains MaxWrScrLen spaces
      CALL WrScr1( ' Total Real Time:       '//TRIM( Num2LStr( Factor*ClckTime      ) )//TRIM( TimePer ) )
      CALL WrScr ( ' Total CPU Time:        '//TRIM( Num2LStr( Factor*UsrTime       ) )//TRIM( TimePer ) )
!     CALL WrScr ( ' ')
!     CALL WrScr ( ' Simulation Real Time:  '//TRIM( Num2LStr( Factor*ClckTimeSim   ) )//TRIM( TimePer ) )
      CALL WrScr ( ' Simulation CPU Time:   '//TRIM( Num2LStr( Factor*UsrTimeSim    ) )//TRIM( TimePer ) )      
      CALL WrScr ( ' Simulated Time:        '//TRIM( Num2LStr( Factor*REAL( ZTime ) ) )//TRIM( TimePer ) )
      CALL WrScr ( ' Time Ratio (Sim/CPU):  '//TRIM( Num2LStr( TRatio ) ) )

   ENDIF

   UsrTime_out = UsrTime
   RETURN
CONTAINS

   FUNCTION GetClockTime(StartClockTime, EndClockTime)
   ! return the number of seconds between StartClockTime and EndClockTime
   
      REAL                         :: GetClockTime          ! Elapsed clock time for the simulation phase of the run.
      INTEGER   , INTENT(IN)       :: StartClockTime (8)                                 ! Start time of simulation (after initialization)
      INTEGER   , INTENT(IN)       :: EndClockTime (8)                                 ! Start time of simulation (after initialization)
   
   !bjj: This calculation will be wrong at certain times (e.g. if it's near midnight on the last day of the month), but to my knowledge, no one has complained...
      GetClockTime =       0.001*( EndClockTime(8) - StartClockTime(8) ) &  ! Is the milliseconds of the second (range 0 to 999) - local time
                     +           ( EndClockTime(7) - StartClockTime(7) ) &  ! Is the seconds of the minute (range 0 to 59) - local time
                     +      60.0*( EndClockTime(6) - StartClockTime(6) ) &  ! Is the minutes of the hour (range 0 to 59) - local time
                     +    3600.0*( EndClockTime(5) - StartClockTime(5) ) &  ! Is the hour of the day (range 0 to 23) - local time
                     + SecPerDay*( EndClockTime(3) - StartClockTime(3) )    ! Is the day of the month
   
   
   END FUNCTION
   
END SUBROUTINE RunTimes

!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE SimStatus_FirstTime( PrevSimTime, PrevClockTime, SimStrtTime, UsrTimeSim, ZTime, TMax )
! This routine displays a message that gives that status of the simulation.
!..................................................................................................................................

   IMPLICIT                        NONE

      ! Passed variables
   REAL(DbKi), INTENT(IN   )    :: ZTime                                           ! Current simulation time (s)
   REAL(DbKi), INTENT(IN   )    :: TMax                                            ! Expected simulation time (s)
   REAL(DbKi), INTENT(  OUT)    :: PrevSimTime                                     ! Previous time message was written to screen (s > 0)
   REAL(ReKi), INTENT(  OUT)    :: PrevClockTime                                   ! Previous clock time in seconds past midnight
   INTEGER,    INTENT(  OUT)    :: SimStrtTime (8)                                 ! An array containing the elements of the start time.
   REAL,       INTENT(  OUT)    :: UsrTimeSim                                      ! User CPU time for simulation (without intialization)

      ! Local variables.

   REAL(ReKi)                   :: CurrClockTime                                   ! Current time in seconds past midnight.


      ! How many seconds past midnight?

   CALL DATE_AND_TIME ( Values=SimStrtTime )
   CALL CPU_TIME ( UsrTimeSim )                                                    ! Initial CPU time   
   CurrClockTime = TimeValues2Seconds( SimStrtTime )


   CALL WrScr ( ' Timestep: '//TRIM( Num2LStr( NINT( ZTime ) ) )//' of '//TRIM( Num2LStr( TMax ) )//' seconds.')


   ! Let's save this time as the previous time for the next call to the routine
   PrevClockTime = CurrClockTime
   PrevSimTime   = ZTime

   RETURN
END SUBROUTINE SimStatus_FirstTime

!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE SimStatus( PrevSimTime, PrevClockTime, ZTime, TMax )
! This routine displays a message that gives that status of the simulation and the predicted end time of day.
!..................................................................................................................................

   IMPLICIT                        NONE

      ! Passed variables
   REAL(DbKi), INTENT(IN)       :: ZTime                                           ! Current simulation time (s)
   REAL(DbKi), INTENT(IN)       :: TMax                                            ! Expected simulation time (s)
   REAL(DbKi), INTENT(INOUT)    :: PrevSimTime                                     ! Previous time message was written to screen (s > 0)
   REAL(ReKi), INTENT(INOUT)    :: PrevClockTime                                   ! Previous clock time in seconds past midnight


      ! Local variables.

   REAL(ReKi)                   :: CurrClockTime                                   ! Current time in seconds past midnight.
   REAL(ReKi)                   :: DeltTime                                        ! The amount of time elapsed since the last call.
   REAL(ReKi)                   :: EndTime                                         ! Approximate time of day when simulation will complete.
   REAL(ReKi), PARAMETER        :: InSecHr  = 1.0_ReKi/3600.0_ReKi                 ! Inverse of the number of seconds in an hour
   REAL(ReKi), PARAMETER        :: InSecMn  = 1.0_ReKi/  60.0_ReKi                 ! Inverse of the number of seconds in a minute
   REAL(ReKi)                   :: SimTimeLeft                                     ! Approximate clock time remaining before simulation completes

   REAL(ReKi), PARAMETER        :: SecPerDay = 24*60*60.0_ReKi                     ! Number of seconds per day

   INTEGER(4)                   :: EndHour                                         ! The hour when the simulations is expected to complete.
   INTEGER(4)                   :: EndMin                                          ! The minute when the simulations is expected to complete.
   INTEGER(4)                   :: EndSec                                          ! The second when the simulations is expected to complete.
   INTEGER(4)                   :: TimeAry  (8)                                    ! An array containing the elements of the start time.

   CHARACTER( 8)                :: ETimeStr                                        ! String containing the end time.


   IF ( ZTime <= PrevSimTime ) RETURN


      ! How many seconds past midnight?

   CALL DATE_AND_TIME ( Values=TimeAry )
   CurrClockTime = TimeValues2Seconds( TimeAry )

      ! Calculate elapsed clock time

   DeltTime = CurrClockTime - PrevClockTime


      ! We may have passed midnight since the last revoultion.  We will assume that (ZTime - PrevSimTime) of simulation time doesn't take more than a day.

   IF ( CurrClockTime < PrevClockTime )  THEN
      DeltTime = DeltTime + SecPerDay
   ENDIF


      ! Estimate the end time in hours, minutes, and seconds

   SimTimeLeft = REAL( ( TMax - ZTime )*DeltTime/( ZTime - PrevSimTime ), ReKi )          ! DeltTime/( ZTime - PrevSimTime ) is the delta_ClockTime divided by the delta_SimulationTime
   EndTime  =  MOD( CurrClockTime+SimTimeLeft, SecPerDay )
   EndHour  =  INT(   EndTime*InSecHr )
   EndMin   =  INT( ( EndTime - REAL( 3600*EndHour ) )*InSecMn )
   EndSec   = NINT(   EndTime - REAL( 3600*EndHour + 60*EndMin ) ) !bjj: this NINT can make the seconds say "60"

   WRITE (ETimeStr,"(I2.2,2(':',I2.2))")  EndHour, EndMin, EndSec

   CALL WrOver ( ' Timestep: '//TRIM( Num2LStr( NINT( ZTime ) ) )//' of '//TRIM( Num2LStr( TMax ) )// &
                 ' seconds.  Estimated final completion at '//ETimeStr//'.'                             )


      ! Let's save this time as the previous time for the next call to the routine
   PrevClockTime = CurrClockTime
   PrevSimTime   = ZTime

   RETURN
END SUBROUTINE SimStatus
!----------------------------------------------------------------------------------------------------------------------------------
FUNCTION TimeValues2Seconds( TimeAry )
! This routine takes an array of time values such as that returned from
!     CALL DATE_AND_TIME ( Values=TimeAry )
! and converts TimeAry to the number of seconds past midnight.
!..................................................................................................................................

      ! Passed variables:
   INTEGER, INTENT(IN)          :: TimeAry  (8)                                    ! An array containing the elements of the time
   REAL(ReKi)                   :: TimeValues2Seconds                              ! Current time in seconds past midnight


   TimeValues2Seconds = 3600*TimeAry(5) + 60*TimeAry(6) + TimeAry(7) + 0.001_ReKi*TimeAry(8)

END FUNCTION TimeValues2Seconds
!----------------------------------------------------------------------------------------------------------------------------------

end module WTP_DvrSubs