!  UnsteadyAeroTest.f90 
!
!  FUNCTIONS:
!  UnsteadyAeroTest - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: UnsteadyAeroTest
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************


subroutine Init_AFI(NumAFfiles, afNames, AFI_Params, errStat, errMsg)
use NWTC_Library
use AirfoilInfo_Types
use AirfoilInfo
   integer,             intent(in   )  :: NumAFfiles
   CHARACTER(1024),     intent(in   )  :: afNames(NumAFfiles)
   type(AFI_ParameterType), intent(  out)  :: AFI_Params
   integer(IntKi),      intent(  out)  :: errStat                       ! Error status.
   character(*),        intent(  out)  :: errMsg                        ! Error message.

   
  type(AFI_InitInputType)  :: AFI_InitInputs
  integer                  :: UnEc
  ! Initialize the Airfoil Info module
      ! Setup Airfoil info
   AFI_InitInputs%NumAFfiles = NumAFfiles
   UnEc = 0
   
   
   allocate ( AFI_InitInputs%FileNames( AFI_InitInputs%NumAFfiles ), STAT=ErrStat )
   if ( errStat /= 0 )  then
      errStat = ErrID_Fatal 
      errMsg = 'Init_AFI: Error allocating AFI_InitInputs%FileNames.'
      call AFI_DestroyInitInput(AFI_InitInputs, errStat, errMsg)
      return
   endif
   
   do i=1,AFI_InitInputs%NumAFfiles
      AFI_InitInputs%FileNames(i) = afNames(i) !InitInp%AF_File(i)
   end do
   
   AFI_InitInputs%UA_Model    = 0
   AFI_InitInputs%NumCoefs    = 3
   AFI_InitInputs%InCol_Alfa  = 1
   AFI_InitInputs%InCol_Cl    = 2
   AFI_InitInputs%InCol_Cd    = 3
   AFI_InitInputs%InCol_Cm    = 4
   AFI_InitInputs%InCol_Cpmin = 0

   
   
      ! Call AFI_Init to read in and process the airfoil files.
      ! This includes creating the spline coefficients to be used for interpolation.

   call AFI_Init ( AFI_InitInputs, AFI_Params, errStat, errMsg, UnEc )
   if ( errStat /= 0 )  then
      call ProgAbort ( NewLine//trim( adjustl( errMsg ) ), .FALSE., 10.0, errStat )
   endif ! ( ErrStatLcl /= 0 )
 
   !Clean up initialization inputs
   call AFI_DestroyInitInput(AFI_InitInputs, errStat, errMsg)
   
   
end subroutine Init_AFI
   
   
   
program UnsteadyAeroTest
   use NWTC_Library
   use AirfoilInfo_Types
   use UnsteadyAero_Types
   use UnsteadyAero
   
   implicit none

    ! Variables
   integer(IntKi), parameter                     :: NumInp = 1           ! Number of inputs sent to HydroDyn_UpdateStates
   
   real(DbKi)  :: dt, t
   integer     :: i, j, k, n 
   real(DbKi)                                    :: InputTime(NumInp)    ! Variable for storing time associated with inputs, in seconds
   type(UA_InitInputType)                        :: InitInData           ! Input data for initialization
   type(UA_InitOutputType)                       :: InitOutData          ! Output data from initialization
   type(UA_DiscreteStateType)                    :: xd                   ! Discrete states
   type(UA_OtherStateType)                       :: OtherState           ! Other/optimization states
   type(UA_ParameterType)                        :: p                    ! Parameters
   type(UA_InputType)                            :: u(NumInp)            ! System inputs
   type(UA_OutputType)                           :: y                    ! System outputs
   integer(IntKi)                                :: errStat              ! Status of error message
   character(4096)                               :: errMsg               ! Error message if ErrStat /= ErrID_None
   
   integer, parameter                            :: NumAFfiles = 1
   character(1024)                               :: afNames(NumAFfiles)
   !type(AFI_ParamType) :: AFI_Params
   
   character(1024)                               :: outFileName
   integer                                       :: unOutFile
   character(200)                                :: TimeFrmt, Frmt 
   
   
      ! Initialize the NWTC library
   call NWTC_Init()
   
    
   print *, 'Running UnsteadyAero Test'
   
   
   dt = .05_DbKi
   n  = 1
   InitInData%nNodesPerBlade  = 1 
   InitInData%numBlades       = 1
   
      ! All nodes/blades are using the same 2D airfoil
   afNames(1) = 'C:\Dev\NREL_SVN\WT_Perf\branches\v4.x\CertTest\af_files2\S809_CLN_298.DAT'
   
      ! Initialize the Airfoil Info Params
   call Init_AFI(NumAFfiles, afNames, InitInData%AFI_Params, errStat, errMsg)
 
      ! Set up initialization data
   Allocate(InitInData%AFIndx(InitInData%nNodesPerBlade,InitInData%numBlades), STAT = errStat)
   if ( errStat /= 0 ) then
      call SetErrStat( ErrID_Fatal, 'Error trying to allocate InitInData%AFIndx.', errStat, errMsg, 'UnsteadyAeroTest')
   end if
   Allocate(InitInData%c(InitInData%nNodesPerBlade,InitInData%numBlades), STAT = errStat)
   
   do j = 1,InitInData%numBlades
      do i = 1,InitInData%nNodesPerBlade
         InitInData%AFIndx(i,j) = 1
         InitInData%c(i,j)      = 1.0_ReKi - (i-1)*0.25_ReKi
      end do
   end do
   
      
   InitInData%DSMod     = 1  
   InitInData%a_s       = 340.29 ! m/s  
   
    ! Initialize UnsteadyAero
   call UA_Init( InitInData, u(1), p, xd, OtherState, y, dt, InitOutData, errStat, errMsg ) 
   
   
   ! Open the file for output
   outFileName = 'C:\dev\UnsteadyAeroTests\Constant_Alpha.UA.out'
   call GetNewUnit( unOutFile )
   
   call OpenFOutFile ( unOutFile, outFileName, errStat ) 
   if ( errStat /= 0 ) stop
      
      
      ! Write the output file header
      
   write (unOutFile,'(/,A/)', IOSTAT=errStat)  'These predictions were generated by UnSteadyAero on '//CurDate()//' at '//CurTime()//'.'
      ! Write the names of the output parameters:
   Frmt = '(A8)'
   write(unOutFile, Frmt, ADVANCE='no')  trim( 'Time' )
   Frmt = '('//trim(Int2LStr(p%NumOuts))//'(:,A,'//trim( p%OutSFmt )//'))'
   write(unOutFile, Frmt, ADVANCE='no')   ( p%Delim, trim( InitOutData%WriteOutputHdr(I)   ), i=1,p%NumOuts )   
   write (unOutFile,'()', IOSTAT=ErrStat)          ! write the line return
      ! Write the units of the output parameters:
  
   Frmt = '(A8)'
   write(unOutFile, Frmt, ADVANCE='no')  trim( '(sec)' ) 
   Frmt = '('//trim(Int2LStr(p%NumOuts))//'(:,A,'//trim( p%OutSFmt )//'))'
   write(unOutFile, Frmt, ADVANCE='no')   ( p%Delim, trim( InitOutData%WriteOutputUnt(I)   ), i=1,p%NumOuts )
   write (unOutFile,'()', IOSTAT=ErrStat)          ! write the line return
   
   TimeFrmt = '(F10.4)'
   Frmt     = '('//trim(Int2LStr(p%NumOuts))//'(:,A,'//trim( p%OutFmt )//'))'
   
      ! time marching loop
   do n = 1, 150
      
      t            = (n-1)*dt
      InputTime(1) = t
      
      do j = 1,InitInData%numBlades
         do i = 1,InitInData%nNodesPerBlade
            
               ! set the inputs for the node
            u(1)%U  = 10.0_ReKi  ! m/s
            u(1)%Re = 1e7  ! not used at the moment
            !u(1)%alpha =  2.0_ReKi * pi/180.0  
            u(1)%alpha =   (5.5_ReKi * sin((n-13)*2*pi/125) + 8.0)*pi/180.0  
            
               ! Use existing states to compute the outputs
            call UA_CalcOutput(i, j, u(1),  p, xd, OtherState, y, errStat, errMsg )
            
               ! Generate file outputs
            write (unOutFile,TimeFrmt,ADVANCE='no')  t  
            write (unOutFile,Frmt,ADVANCE='no')   ( p%Delim,  y%WriteOutput(k)  , k=1,p%NumOuts )   
 
               ! Prepare states for next time step
            call UA_UpdateStates(i, j, t, n, u, InputTime, p, xd, OtherState, errStat, errMsg )
         
         end do
      end do
      
      write (unOutFile,'()', IOSTAT=ErrStat)          ! write the line return
              
   end do
   
   write (unOutFile,'(/,A/)', IOSTAT=errStat)  'This output file was closed on '//CurDate()//' at '//CurTime()//'.'
   
   !-------------------------------------------------------------------------------------------------
   ! Close our output file
   !-------------------------------------------------------------------------------------------------
   close( unOutFile, IOSTAT = errStat )
      
end program UnsteadyAeroTest

