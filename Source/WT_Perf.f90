!  WT_Perf.f90 
!
!  FUNCTIONS:
!  WT_Perf - Entry point of console application.
!


!****************************************************************************
!!
!>  WT_Perf
!!
!!  This is the stand-alone driver program for the BEM module and is using for testing purposes only.
!!
!****************************************************************************

program WT_Perf

   use WTP_Types
   use WT_Perf_IO
   use AirfoilInfo_Types
   use AirfoilInfo
   use WTP_DvrSubs
   use NWTC_Library
   !use BEMT_Types
   use AeroDyn
   use NWTC_Library
   use NWTC_FitPack
   
    
   implicit none   

   integer(IntKi), parameter                      :: numInp = 1           !< Number of inputs sent to AD_UpdateStates
   
      ! Program variables

   real(DbKi)                                     :: time                 !< Variable for storing time, in seconds 
   real(DbKi)                                     :: inputTime(numInp)    !< Variable for storing time associated with inputs, in seconds
   real(DbKi)                                     :: dT                   !< BEM module requested time interval
  
   type(AD_InitInputType)                        :: AD_InitInData           ! Input data for initialization
   type(AD_InitOutputType)                       :: InitOutData          ! Output data from initialization
   type(AD_ContinuousStateType)                  :: x                    ! Continuous states
   type(AD_ContinuousStateType)                  :: x_new                ! Continuous states at updated time
   type(AD_DiscreteStateType)                    :: xd                   ! Discrete states
   type(AD_DiscreteStateType)                    :: xd_new               ! Discrete states at updated time
   type(AD_ConstraintStateType)                  :: z                    ! Constraint states
   type(AD_ConstraintStateType)                  :: z_residual           ! Residual of the constraint state equations (Z)
   type(AD_OtherStateType)                       :: OtherState           ! Other/optimization states
   type(AD_ParameterType)                        :: p                    ! Parameters
   type(AD_InputType)                            :: u(numInp)                 ! System inputs
   type(AD_OutputType)                           :: y                    ! System outputs
   type(AD_ContinuousStateType)                  :: dxdt                 ! First time derivatives of the continuous states
   
   type(WTP_InputFileData)                        :: WTP_FileData         ! The data obtained by parsing the WT_Perf style input file

   integer(IntKi)                                 :: i                    ! generic loop counter
   integer(IntKi)                                 :: j                    ! generic loop counter
   integer(IntKi)                                 :: n                    ! loop counter (for time step)
   integer(IntKi)                                 :: numSteps             ! number of time steps in the simulation
   integer(IntKi)                                 :: errStat              ! Status of error message
   character(4096)                                :: errMsg               ! Error message if ErrStat /= ErrID_None
   real(ReKi)                                     :: dcm (3,3)            ! The resulting transformation matrix from X to x, (-).
   character(1024)                                :: drvrFilename         ! Filename and path for the driver input file.  This is passed in as a command line argument when running the Driver exe.
   character(1024)                                :: outFileRoot
   integer(IntKi)                                 :: unOutFile
   character(1)                                   :: delim
   character(20)                                  :: outFmt, outFmtS
   integer                                        :: StrtTime (8)                            ! Start time of simulation (including intialization)
   integer                                        :: SimStrtTime (8)                         ! Start time of simulation (after initialization)
   real(ReKi)                                     :: PrevClockTime                           ! Clock time at start of simulation in seconds
   real                                           :: UsrTime1                                ! User CPU time for simulation initialization
   real                                           :: UsrTime2                                ! User CPU time for simulation (without intialization)
   real                                           :: UsrTimeDiff                             ! Difference in CPU time from start to finish of program execution
   real(DbKi)                                     :: TiLstPrn                                ! The simulation time of the last print
   real(DbKi)                                     :: t_global                                ! Current simulation time (for global/FAST simulation)
   real(DbKi)                                     :: TMax                                    ! Max simulation time (s)
   real(DbKi)                                     :: SttsTime                                ! Amount of time between screen status messages (sec)
   integer                                        :: n_SttsTime                              ! Number of time steps between screen status messages (-)
   !type(AFI_InitInputType)                        :: AFI_InitInputs                          ! The derived type for holding the airfoil file information.
   !type(AFI_ParameterType)                        :: AFI_Params                              ! The derived type for holding the airfoil parameter information.
   real(ReKi)                                     :: psiRotor, velocityHub, deltar
   integer                                        :: iSect, iCase
   INTEGER                                :: UnEc                             ! Unit number for the echo file.
   CHARACTER(1000)                        :: inputFile                        ! String to hold the file name.
   CHARACTER(1000)                        :: RootName                         ! String to hold the root of the input file name.
   CHARACTER(1000)                        :: EchoFile                         ! String to hold the Echo file name.
   TYPE(ProgDesc), PARAMETER              :: version    = &                  ! The version number of this program.
                                                ProgDesc( 'WT_Perf', 'v0.01.00', '28-Aug-2014' ) 
   INTEGER(IntKi)                 :: i1
   INTEGER(IntKi)                 :: i1_l  ! lower bounds for an array dimension
   INTEGER(IntKi)                 :: i1_u  ! upper bounds for an array dimension
   
                            

   errStat     = ErrID_None
   errMsg      = ''
   time        = 0.0 ! seconds
   outFileRoot = "ccBlade_UAE"
   unOutFile   = -1
   delim       = ' '
   outFmt      = "ES15.4e2"
   outFmtS     = "A15"
   SttsTime    = 1.0 ! seconds
   TMax        = 60.0 ! seconds
   dT          = 0.25  ! seconds
   numSteps    = ceiling(TMax / dT)
   
   
   ! If we have an array of inputs, u, of size greater than 1, then we need to allocate the remaining inputs, because the
   ! AD module only allocated the first!
   
   
   
      ! Get the current time
   call date_and_time ( Values=StrtTime )                               ! Let's time the whole simulation
   call cpu_time ( UsrTime1 )                                           ! Initial time (this zeros the start time when used as a MATLAB function)
   
     ! figure out how many time steps we should go before writing screen output:      
    n_SttsTime = MAX( 1, NINT( SttsTime / dT ) )
    
      ! Initialize the library which handle file echos and WrScr, for example
   call NWTC_Init()
   
   call WrScr('Beginning WT_Perf execution...')
   
   if (numInp > 1) then
      errStat = ErrID_Fatal
      errMsg = 'WT_PERF:  The number of inputs, numInp, must currently be set to 1'
      call WTP_DvrCleanup()
   end if
   
   if (command_argument_count() == 0 .OR. command_argument_count() > 2) then
      errStat = ErrID_Fatal
      errMsg = 'WT_PERF usage:  WT_Perf.exe settingsfile rootname'
      call WTP_DvrCleanup()
   end if
   
   call get_command_argument(1, inputFile)
   
   if (command_argument_count() == 2) then
      call get_command_argument(2, outFileRoot)
   else
      outFileRoot = 'WT_Perf'
   end if
   
   
      ! Read the WT_Perf style input file
   call WTP_ReadInputFile(inputFile, WTP_FileData, errStat, errMsg )
      if (ErrStat >= AbortErrLev) call WTP_DvrCleanup
      
      ! Allocate the AeroDyn input data structure
   !call Alloc_AD_u( u, numInp, errStat, errMsg)
   
      ! Set the Initialization input data for AeroDyn based on the WT_Perf input file data
   call Set_AD_InitInp(WTP_FileData, AD_InitInData, errStat, errMsg)
      if (ErrStat >= AbortErrLev) call WTP_DvrCleanup
   
      ! Initialize AeroDyn
   call AD_Init(AD_InitInData, u(1), p, x, xd, z, OtherState, y, dt, InitOutData, ErrStat, ErrMsg )
      if (ErrStat >= AbortErrLev) call WTP_DvrCleanup
   
   
   
   call WTP_InitializeOutputFile( version, delim, outFmtS, WTP_FileData%OutFileRoot, InitOutData%WriteOutputHdr, InitOutData%WriteOutputUnt, unOutFile, errStat, errMsg)
      if (ErrStat >= AbortErrLev) call WTP_DvrCleanup
   
   call SimStatus_FirstTime( TiLstPrn, PrevClockTime, SimStrtTime, UsrTime2, time, TMax )
      if (ErrStat >= AbortErrLev) call WTP_DvrCleanup
   
   do iCase = 1, WTP_FileData%NumCases
      dT = 60.0/WTP_FileData%Cases(iCase)%RotSpeed / WTP_FileData%NumSect ! sec
      do iSect = 1, WTP_FileData%NumSect
         
   
      time = (iSect-1) * dT
      inputTime(1) = time
      ! Angular position of the rotor, starts at 0.0 when iSect = 1
      psiRotor = time*pi*WTP_FileData%Cases(iCase)%RotSpeed/30.0 ! in s * rad/s 
      
         ! Velocity of inflow wind at the hub height
      velocityHub = WTP_FileData%Cases(iCase)%WndSpeed ! m/s
         ! Zero yaw for now
      u(1)%gamma = WTP_FileData%Cases(iCase)%Yaw*D2R  ! convert from deg to rad 
         ! Rotor angular velocity
      u(1)%omega = WTP_FileData%Cases(iCase)%RotSpeed*pi/30.0  !  rev per minute in units of rad/s
         ! Average tip-speed ratio
      u(1)%lambda = WTP_FileData%Cases(iCase)%TSR
        
      u(1)%Vinf   = velocityHub
      
      do j=1,AD_InitInData%numBlades
         u(1)%psi(j)   = psiRotor + (j-1)*2.0*pi/(AD_InitInData%numBlades) ! find psi for each blade based on the rotor psi value
         u(1)%rTip(j)  = WTP_FileData%RotorRad ! set the tip radius to the initialization distance along the blade (straight blade assumption)

         do i=1,AD_InitInData%numBladeNodes
            u(1)%theta (i,j)  = WTP_FileData%BladeData(i)%Twist + (WTP_FileData%Cases(iCase)%Pitch)*D2R ! convert from deg to rad
           ! u(1)%Vx    (i,j)  = velocityHub   ! for now just set the velocity perp to rotation plane = hub velocity of inflow wind
             u(1)%rLocal(i,j)  = WTP_FileData%BladeData(i)%RLoc
            !u(1)%Vy    (i,j)  = u(1)%omega*u(1)%rLocal(i,j) 
           ! temp1             = velocityHub*((cos(u(1)%gamma)*sin(WTP_FileData%tilt)*sin(u(1)%psi(j)) - sin(u(1)%gamma)*cos(u(1)%psi(j)) )
           ! temp2             = u(1)%omega*u(1)%rLocal(i,j)*WTP_FileData%CosCone)
            if (p%BEMT_SkewWakeMod < 2 ) then
                  ! This is incomplete and doesn't include all possible geometries or blade elastic motions
               u(1)%Vx    (i,j)  = velocityHub*((cos(u(1)%gamma)*sin(WTP_FileData%tilt)*cos(u(1)%psi(j))+sin(u(1)%gamma)*sin(u(1)%psi(j)))*WTP_FileData%SinCone + cos(u(1)%gamma)*cos(WTP_FileData%tilt)*WTP_FileData%CosCone)
               u(1)%Vy    (i,j)  = velocityHub*( cos(u(1)%gamma)*sin(WTP_FileData%tilt)*sin(u(1)%psi(j)) - sin(u(1)%gamma)*cos(u(1)%psi(j)) ) + u(1)%omega*u(1)%rLocal(i,j)*WTP_FileData%CosCone
            else
               u(1)%Vx    (i,j)  =  velocityHub*WTP_FileData%CosCone
               u(1)%Vy    (i,j)  =  u(1)%omega*u(1)%rLocal(i,j)*WTP_FileData%CosCone 
            end if 
            
         
         
         
         end do
      end do
      
         ! Set the inputs for this time step
     ! call WTP_Dvr_SetInputs(n, u(1), WTP_FileData, AD_InitInData, errStat, errMsg )
     
         ! Get state variables at next step: INPUT at step n, OUTPUT at step n + 1

      call AD_UpdateStates( time, n, u, InputTime, p, x, xd, z, OtherState, errStat, errMsg )
         if (errStat >= AbortErrLev) call WTP_DvrCleanup()
      
      
         ! Calculate outputs at n

      call AD_CalcOutput( time, u(1), p, x, xd, z, OtherState, y, errStat, errMsg )
         if (errStat >= AbortErrLev) call WTP_DvrCleanup()
   
      call WTP_WriteOutputLine(unOutFile, time, delim, outFmt, y%WriteOutput, errStat, errMsg)
         if (errStat >= AbortErrLev) call WTP_DvrCleanup()
      
      !----------------------------------------------------------------------------------------
      ! Display simulation status every SttsTime-seconds (i.e., n_SttsTime steps):
      !----------------------------------------------------------------------------------------   
      
      IF ( MOD( n + 1, n_SttsTime ) == 0 ) THEN

         CALL SimStatus( TiLstPrn, PrevClockTime, time, TMax )

      ENDIF 
      
      end do
      
   end do
   
   
   call WTP_DvrCleanup()
   
   contains
   
   
  
   subroutine WTP_DvrCleanup()
   
         ! Local variables
      character(len(errMsg))                        :: errMsg2                 ! temporary Error message if ErrStat /= ErrID_None
      integer(IntKi)                                :: errStat2                ! temporary Error status of the operation

   
      errStat2 = ErrID_None
      errMsg2  = ""
      
         ! Close the output file
      close(unOutFile)
      
      call AD_DestroyInitInput( AD_InitInData, errStat2, errMsg2 )
      call SetErrStat( errStat2, errMsg2, errStat, errMsg, 'WTP_DvrCleanup' )
      call AD_DestroyConstrState( z, errStat2, errMsg2 )
      call SetErrStat( errStat2, errMsg2, errStat, errMsg, 'WTP_DvrCleanup' )
      call AD_End( u(1), p, x, xd, z, OtherState, y, errStat2, errMsg2 )
      call SetErrStat( errStat2, errMsg2, errStat, errMsg, 'WTP_DvrCleanup' )
      call WrScr(ErrMsg)
      if (errStat /= ErrID_None) then !This assumes PRESENT(ErrID) is also .TRUE. :
         if ( time < 0.0 ) then
            ErrMsg = 'at initialization'
         else if ( time > TMax ) then
            ErrMsg = 'after computing the solution'
         else            
            ErrMsg = 'at simulation time '//trim(Num2LStr(time))//' of '//trim(Num2LStr(TMax))//' seconds'
         end if
                    
         call WrScr(ErrMsg)
         call ProgAbort( 'WT_Perf encountered an error '//trim(errMsg)//'.'//NewLine//' Simulation error level: '&
                         //trim(GetErrStr(errStat)), TrapErrors=.FALSE., TimeWait=20._ReKi )  ! wait 3 seconds (in case they double-clicked and got an error)
      end if
      
      
      call RunTimes( StrtTime, UsrTime1, SimStrtTime, UsrTime2, time, UsrTimeDiff )
      call NormStop()
      
   end subroutine WTP_DvrCleanup
   
   
   
end program WT_Perf
   