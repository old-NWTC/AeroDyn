module WT_Perf_IO
   USE                             NWTC_Library
   USE                             WTP_Types
   
   IMPLICIT                        NONE

   public :: WTP_ReadInputFile
   
   
   contains
   
subroutine WTP_ReadInputFile(inputFile, inputData, errStat, errMsg )
   ! This routine opens the gets the data from the input files.

   character(*),                  intent( in    )   :: inputFile
   type(WTP_InputFileData),       intent(   out )   :: inputData
   integer,                       intent(   out )   :: errStat              ! returns a non-zero value when an error occurs  
   character(*),                  intent(   out )   :: errMsg               ! Error message if errStat /= ErrID_None
   

      ! Local variables
   character(1024)              :: fileName
   character(1024)              :: rootName
   character(1024)              :: AF_File                                  ! String containing the name of an aifoil file.
   character(1024)              :: inpVersion                               ! String containing the input-version information.
   character(1024)              :: line                                     ! String containing a line of input.
   character(1024)              :: subTitle                                 ! String containing the RunTitle of a subsection.
   integer                      :: unIn, unEc, NumElmPr
   integer(IntKi)               :: ErrStat2                                 ! Temporary Error status
   character(len(errMsg))       :: ErrMsg2                                  ! Temporary Error message
   integer                      :: ISeg, IAF, ICase
   integer                      :: IOS, Sttus
   character( 11)               :: DateNow                                  ! Date shortly after the start of execution.
   character(  8)               :: TimeNow                                  ! Time of day shortly after the start of execution.
   real(ReKi)                   :: InpCase(4)                               ! Temporary array to hold combined-case input parameters.
   
   ! Open the input file
   fileName = trim(inputFile)
   call GetRoot ( fileName, rootName )
   call GetNewUnit( unIn )   
   call OpenFInpFile( unIn, fileName, errStat )
   if ( errStat /= ErrID_None ) then
      errMsg  = ' Failed to open WT_Perf input file: '//fileName
      errStat = ErrID_Fatal
      close( unIn )
      return
   end if

   
   call WrScr( 'Opening WT_Perf input file:  '//fileName )

      ! Skip a line, read the run title and the version information.

   read (unIn,'(/,A,/,A)',IOSTAT=IOS)  inputData%runTitle, inpVersion

   if ( IOS < 0 )  call PremEOF ( fileName , 'runTitle' )

   call WrScr1 ( ' '//inputData%runTitle )

   
      ! Read in the title line for the input-configuration subsection.

   read (unIn,'(A)',IOSTAT=IOS)  subTitle

   if ( IOS < 0 )  call PremEOF ( fileName , 'the input-configuration subtitle' )

      ! See if we should echo the output.     
   call ReadVar ( unIn, fileName, inputData%echo, 'Echo', 'Echo Input', errStat2, errMsg2 )
      if ( OnCheckErr() ) return

   if ( inputData%echo )  then
         ! Get date and time.
      dateNow = CurDate()
      timeNow = CurTime()
      call GetNewUnit( unEc ) 
      call  OpenFOutFile ( unEc, trim( rootName )//'.ech', errStat2, errMsg2 )
         if ( OnCheckErr() ) return
      write (unEc,'(A)')                        'Echo of WT_Perf Input File:'
      write (unEc,'(A)')                        ' "'//fileName//'"'
      write (unEc,'(A)')                        'Generated on: '//trim( dateNow )//' at '//trim( timeNow )//'.'
      write (unEc,'(A,/,A)')                    inputData%runTitle, inpVersion
      write (unEc,'(A)')                        subTitle
      write (unEc,"(2X,L11,2X,A,T27,' - ',A)")  inputData%echo, 'Echo', 'Echo input parameters to "rootname.ech"?'
   end if


      ! Read the rest of input-configuration section.

   call ReadVar ( unIn, fileName, inputData%dimenInp, 'DimenInp', 'Turbine parameters are dimensional?', errStat2, errMsg2 )
      if ( OnCheckErr() ) return
      
   call ReadVar ( unIn, fileName, inputData%metric,   'Metric',   'Turbine parameters are Metric (MKS vs FPS)?', errStat2, errMsg2 )
      if ( OnCheckErr() ) return


      ! Read the model-configuration section.

   call ReadCom ( unIn, fileName,                                 'the model-configuration subtitle'                )
   call ReadVar ( unIn, fileName, inputData%numSect,  'numSect',  'Number of circumferential sectors.'              )
   call ReadVar ( unIn, fileName, inputData%maxIter,  'maxIter',  'Max number of iterations for induction factor.'  )
   call ReadVar ( unIn, fileName, inputData%ATol,     'ATol',     'Error tolerance for induction iteration.'        )
   !call ReadVar ( unIn, fileName, inputData%SWTol,    'SWTol',    'Error tolerance for skewed-wake iteration.'      )


   inputData%ATol2 = inputData%ATol**2


      ! Check for valid choices.

   if ( inputData%NumSect < 1 )  then
      call Abort ( ' Variable "numSect" must be greater than 0.  Instead, it is "'//trim( Int2LStr( inputData%numSect ) )//'".' )
   endif
   if ( inputData%maxIter < 1 )  then
      call Abort ( ' Variable "maxIter" must be greater than 0.  Instead, it is "'//trim( Int2LStr( inputData%maxIter ) )//'".' )
   endif

   if ( inputData%ATol <= 0.0 )  then
      call Abort ( ' Variable "ATol" must be greater than 0.  Instead, it is "'//trim( Num2LStr( inputData%ATol ) )//'".' )
   endif

   !if ( inputData%SWTol <= 0.0 )  then
   !   call Abort ( ' Variable "SWTol" must be greater than 0.  Instead, it is "'//trim( Num2LStr( inputData%SWTol ) )//'".' )
   !endif
   
      ! Read the algorithm-configuration section.

   call ReadCom ( unIn, fileName,                       'the algorithm-configuration subtitle'                           )
   call ReadVar ( unIn, fileName, inputData%useTipLoss,  'useTipLoss',  'Use the Prandtl tip-loss model?'                                )
   call ReadVar ( unIn, fileName, inputData%useHubLoss,  'useHubLoss',  'Use the Prandtl hub-loss model?'                                )
   call ReadVar ( unIn, fileName, inputData%useTanInd,    'useTanInd',    'Include Swirl effects?'                                         )
   call ReadVar ( unIn, fileName, inputData%skewWakeMod, 'skewWakeMod', 'Skewed-wake correction model'                                  )
!Start of proposed change.  v3.02.00b-mlb 22-August-2006  M. Buhl
!v3.02.00b-mlb   CALL ReadVar ( unIn, fileName, AdvBrake, 'AdvBrake', 'Use the advanced brake-state model?'                            )
!End of proposed change.  v3.02.00b-mlb 22-August-2006  M. Buhl
!Start of proposed change.  v3.02.00a-mlb 03-August-2006  M. Buhl
!v3.02.00a-mlb   CALL ReadVar ( unIn, fileName, IndProp,  'IndProp',  'Use PROP-PC instead of PROPX induction algorithm?'              )
!Start of proposed change.  v3.02.00b-mlb 22-August-2006  M. Buhl
!v3.02.00b-mlb   CALL ReadVar ( unIn, fileName, IndType,  'IndType',  'Use: 0-None, 1-PROPPC, 2-PROPX induction algorithm.'            )
  ! CALL ReadVar ( unIn, fileName, IndType,  'IndType',  'Use: 0-None, 1-BEM induction algorithm.'                        )
!3.03.01a00-dcm
   call ReadVar ( unIn, fileName, inputData%useInduction,  'useInduction',  'Use BEM induction algorithm?'                        )
!End of proposed change.  v3.02.00b-mlb 22-August-2006  M. Buhl
!End of proposed change.  v3.02.00a-mlb 03-August-2006  M. Buhl
   call ReadVar ( unIn, fileName, inputData%useAIDrag,   'useAIDrag',   'Include the drag term in the axial-induction calculation?'      )
   call ReadVar ( unIn, fileName, inputData%useTIDrag,   'useTIDrag',   'Include the drag term in the tangential-induction calculation?' )


   if ( inputData%useAIDrag )  then
      inputData%AIDragM = 1.0
   else
      inputData%AIDragM = 0.0
   endif

   if ( inputData%useTIDrag )  then
      inputData%TIDragM = 1.0
   else
      inputData%TIDragM = 0.0
   endif

   !call ReadVar ( unIn, fileName, inputData%TISingularity,   'TISingularity',   'Use the singularity avoidance method in the tangential-induction calculation?' )

      ! Check for valid choices.


!Start Proposed Changes 3.03.01a00-dcm, 27-Jul-2009
!Start of proposed change.  v3.02.00b-mlb 22-August-2006  M. Buhl
!v3.02.00b-mlb   IF ( ( IndType < 0 ) .OR. ( IndType > 2 ) )  THEN
!v3.02.00b-mlb      CALL Abort ( ' Variable "IndType" must be 0, 1, or 2.  Instead, it is "'//Trim( Int2LStr( IndType ) )//'".' )
 !  IF ( ( IndType < 0 ) .OR. ( IndType > 1 ) )  THEN
  !    CALL Abort ( ' Variable "IndType" must be 0 or 1.  Instead, it is '//Trim( Int2LStr( IndType ) )//'.' )
!!End of proposed change.  v3.02.00b-mlb 22-August-2006  M. Buhl
 !  ELSEIF ( IndType == 0 )  THEN    ! Don't do skewed wakes if induction calculations are disabled.
  !    SkewWake = .FALSE.
  ! ENDIF

   if ( inputData%useInduction == .FALSE. )  then    ! Don't do skewed wakes if induction calculations are disabled.
      inputData%skewWakeMod = 0
   endif

!End Proposed Changes 3.03.01a00-dcm, 27-Jul-2009

      ! Read the turbine-data section.

   call ReadCom ( unIn, fileName, 'the turbine-data subtitle' )
   call ReadVar ( unIn, fileName, inputData%NumBlade, 'NumBlade', 'Number of blades.'     )
   call ReadVar ( unIn, fileName, inputData%RotorRad, 'RotorRad', 'Unconed rotor radius.' )
   call ReadVar ( unIn, fileName, inputData%HubRad,   'HubRad',   'Hub radius.' )
   call ReadVar ( unIn, fileName, inputData%PreCone,  'PreCone',  'Cone angle.' )
   call ReadVar ( unIn, fileName, inputData%Tilt,     'Tilt',     'Shaft tilt.' )
   call ReadVar ( unIn, fileName, inputData%Yaw,      'Yaw',      'Yaw error.' )
   call ReadVar ( unIn, fileName, inputData%HubHt,    'HubHt',    'Hub height.' )

  
   if ( inputData%dimenInp )  then
      inputData%HubRadND = inputData%HubRad/inputData%RotorRad
      inputData%HubHtND  = inputData%HubHt /inputData%RotorRad
   else
!Start of proposed change.  v3.04.00c-mlb, 12-Jan-2011,  M. Buhl
      inputData%HubRadND = inputData%HubRad
      inputData%HubHtND  = inputData%HubHt
!End of proposed change.  v3.04.00c-mlb, 12-Jan-2011,  M. Buhl
      inputData%HubRad = inputData%HubRadND*inputData%RotorRad
      inputData%HubHt  = inputData%HubHtND *inputData%RotorRad
   endif

   inputData%BldLen  = inputData%RotorRad - inputData%HubRad
   inputData%PreCone = inputData%PreCone*D2R
   inputData%SinCone = sin( inputData%PreCone )
   inputData%CosCone = cos( inputData%PreCone )

   inputData%Tilt    = inputData%Tilt*D2R
   inputData%CosTilt = COS( inputData%Tilt )
   inputData%SinTilt = SIN( inputData%Tilt )

   inputData%Yaw    = inputData%Yaw*D2R
   inputData%CosYaw = cos( inputData%Yaw )
   inputData%SinYaw = sin( inputData%Yaw )

   !if ( .NOT. inputData%useSkewWake .OR. ( ( inputData%Yaw == 0.0 ) .AND. ( inputData%Tilt == 0.0 ) ) )  then
   !   inputData%DoSkew   = .FALSE.
   !   inputData%useSkewWake = .FALSE.
   !   inputData%SWconst  = 0.0
   !   inputData%SWconv   = .TRUE.
   !   inputData%SWcorr   = 1.0
   !endif
   
  
      ! Read in the number of segments and allocate the property arrays.

   call ReadVar ( unIn, fileName, inputData%numSeg,   'numSeg',   'Number of blade segments (entire rotor radius).' )

   if ( inputData%numSeg < 1 )  call Abort ( ' Variable "numSeg" must be greater than 0.  Instead, it is "'//Int2LStr( inputData%numSeg )//'".' )

   ! TODO Implement this!
   !CALL AllocProp
   allocate ( inputData%BladeData(inputData%numSeg) , STAT=Sttus )
   if ( Sttus /= 0 )  then
      call Abort(' Error allocating memory for the BladeData array.')
   endif

   
      ! Read in the distributed blade properties.

   call ReadCom  ( unIn, fileName, 'the header for the blade-element data' )

   NumElmPr = 0

   do ISeg=1,inputData%numSeg

      read (unIn,'(A)',IOSTAT=IOS)  Line

      call CheckIOS ( IOS, fileName, 'line #'//trim( Int2LStr( ISeg ) )//' of the blade-element data table.' , StrType )

      read (Line,*,IOSTAT=IOS)  inputData%BladeData(ISeg)%RLoc, inputData%BladeData(ISeg)%Twist, inputData%BladeData(ISeg)%Chord, inputData%BladeData(ISeg)%AFfile, inputData%BladeData(ISeg)%PrntElem

      call CheckIOS ( IOS, fileName, 'line #'//trim( Int2LStr( ISeg ) )//' of the blade-element data table.' , NumType )

      if ( inputData%BladeData(ISeg)%Chord <= 0.0 )  then
         call Abort ( ' The chord for segment #'//trim( Int2LStr( inputData%NumSect ) )//' must be > 0.  Instead, it is "' &
                    //trim( Num2LStr( inputData%BladeData(ISeg)%Chord ) )//'".' )
      endif


         ! Convert to or from dimensional data.

      if (inputData%DimenInp )  then
         inputData%BladeData(ISeg)%RLocND = inputData%BladeData(ISeg)%RLoc /inputData%RotorRad
        ! inputData%BladeData(ISeg)%ChordND  = inputData%BladeData(ISeg)%Chord/inputData%RotorRad
      else
         inputData%BladeData(ISeg)%RLocND = inputData%BladeData(ISeg)%RLoc
         inputData%BladeData(ISeg)%RLoc   = inputData%BladeData(ISeg)%RLocND*inputData%RotorRad
      endif

      inputData%BladeData(ISeg)%Twist = inputData%BladeData(ISeg)%Twist*D2R

      if ( inputData%BladeData(ISeg)%PrntElem )  NumElmPr = NumElmPr +1

   enddo ! ISeg


      ! Compute the segment lengths and check their validity.
!TODO
   !CALL CompDR ( inputData%numSeg, inputData%BladeData, inputData%HubRad, inputData%RotorRad, inputData%DimenInp, DelRLoc )


       ! The Tim Olsen memorial hub-radius check.

   if ( ( inputData%HubRadND < 0.0 ) .OR. ( 1.0 <= inputData%HubRadND ) )  then
      errMsg = ' The hub radius must be positive and less than the rotor radius.  Instead it is ' &
                 //trim( Num2LStr( inputData%HubRad ) )//'meters.' 
      errStat = ErrID_Fatal
      return
   endif
!TODO

      ! Make sure hub is high enough so the blade doesn't hit the ground.  We wouldn't want to get it dirty.  :-)

   if ( inputData%HubHt*inputData%CosCone*inputData%CosTilt .LT. 1.0 )  call Abort ( ' The hub is so low, the blade will hit the ground.' )


      ! Read the aerodynamic-data section.

   call ReadCom ( unIn, fileName, 'the aerodynamic-data subtitle'   )
   call ReadVar ( unIn, fileName, inputData%AirDens,  'AirDens',  'Air density.' )
   call ReadVar ( unIn, fileName, inputData%KinVisc,  'KinVisc',  'Kinesmatic viscosity.' )
   call ReadVar ( unIn, fileName, inputData%ShearExp, 'ShearExp', 'Shear exponent.' )
   call ReadVar ( unIn, fileName, inputData%UseCm,    'UseCm',    'Cm data included in airfoil tables?' )
!Start of proposed change.  v3.03.02a-mlb, 10-Apr-2010,  M. Buhl
   call ReadVar ( unIn, fileName, inputData%UseCpmin, 'UseCpmin', 'Cp,min data included in airfoil tables?' )
!End of proposed change.  v3.03.02a-mlb, 10-Apr-2010,  M. Buhl
   call ReadVar ( unIn, fileName, inputData%NumAF,    'NumAF',    'Number of unique airfoil tables.'     )

   if ( inputData%AirDens <= 0.0 )  call Abort ( ' The air density must be greater than zero.' )
   if ( inputData%KinVisc <= 0.0 )  call Abort ( ' The kinesmatic viscosity must be greater than zero.' )


      ! Check the list of airfoil tables to make sure they are all within limits.

   if ( inputData%NumAF < 1 )  call Abort ( ' The number of unique airfoil tables (NumAF) must be greater than zero.' )

   do ISeg=1,inputData%numSeg
      if ( ( inputData%BladeData(ISeg)%AFfile < 1 ) .OR. ( inputData%BladeData(ISeg)%AFfile > inputData%NumAF ) )  then
         errMsg =  ' Segment #'//trim( Int2LStr( ISeg ) )//' requested airfoil input table #'//trim( Int2LStr( inputData%BladeData(ISeg)%AFfile ) ) &
                    //'.  However, it must be between 1 and NumAF (='//trim( Int2LStr( inputData%NumAF ) )//'), inclusive.' 
         errStat = ErrID_Fatal
         return
      endif
   enddo ! ISeg


      ! Allocate the airfoil data super-supertables for both unique data and complete data.
!TODO
   !ALLOCATE ( AF_Table(NumAF) , STAT=Sttus )
   !IF ( Sttus /= 0 )  THEN
   !   CALL Abort ( ' Error allocating memory for the AF_Uniq super-supertable in T_GetAF.' )
   !ENDIF


      ! Read in NumAF unique airfoil data files.
   allocate ( inputData%AF_File(inputData%NumAF) , STAT=Sttus )
   if ( Sttus /= 0 )  then
      errMsg= ' Error allocating memory for the AF_File array.'
      errStat = ErrID_Fatal
      return
   endif
   
   do IAF=1,inputData%NumAF

      call ReadVar ( unIn, fileName, inputData%AF_File(IAF), 'AF_File', 'Airfoil file #'//trim( Int2LStr( IAF ) )//'.' )
      

   enddo


      ! Make sure we have a minimum of four sectors if we have shear, shaft tilt, or yaw.

   !if (  ( inputData%Tilt /= 0.0 ) .OR. ( inputData%Yaw /= 0.0 ) .OR. ( inputData%ShearExp /= 0.0 ) )  THEN
   !   inputData%NumSect = max( inputData%NumSect, 4 )
   !else
   !   inputData%NumSect = 1
   !endif


      ! Read the I/O-configuration section.

   call ReadCom ( unIn, fileName, 'the I/O-configuration subtitle'                                                                          )
   call ReadVar ( unIn, fileName, inputData%OutFileRoot, 'OutFileRoot', 'Root name for any output files'                                    )
!Start of proposed change.  v3.03.02a-mlb, 04-Dec-2009  M. Buhl
   call ReadVar ( unIn, fileName, inputData%UnfPower, 'UnfPower', 'Write Power to an unformatted file?'                                     )
!End of proposed change.  v3.03.02a-mlb, 04-Dec-2009  M. Buhl
   call ReadVar ( unIn, fileName, inputData%TabDel,   'TabDel',   'Make output tab-delimited (fixed-width otherwise)?'                      )
!Start of proposed change.  v3.02.00c-mlb 24-August-2006  M. Buhl
   call ReadVar ( unIn, fileName, inputData%OutNines, 'OutNines', 'Output nines for cases that fail to satisfy the convergence criterion?'  )
!Start of proposed change.  v3.03.02a-mlb, 01-Dec-2009  M. Buhl
   call ReadVar ( unIn, fileName, inputData%Beep,     'Beep',     'Beep on exit?'                                                           )
!End of proposed change.  v3.03.02a-mlb, 01-Dec-2009  M. Buhl
!#IFDEF debug2
!#print *, "fileName:   Solution, Converge, OutNines = ", Solution, Converge, OutNines
!#ENDIF
!End of proposed change.  v3.02.00c-mlb 24-August-2006  M. Buhl
   call ReadVar ( unIn, fileName, inputData%KFact,    'KFact',    'Output dimensional parameters in K?'                                     )
   call ReadVar ( unIn, fileName, inputData%WriteBED, 'WriteBED', 'Write out blade element data to "bladelem.dat"?'                         )
   call ReadVar ( unIn, fileName, inputData%InputTSR, 'InputTSR', 'Input speeds as TSRs?'                                                   )
!Start of proposed change.  v3.03.02a-mlb, 10-Dec-2009  M. Buhl
   call ReadVar ( unIn, fileName, inputData%OutMaxCp, 'OutMaxCp', 'Output conditions leading to maximum Cp?'                                )
!End of proposed change.  v3.03.02a-mlb, 10-Dec-2009  M. Buhl
   call ReadVar ( unIn, fileName, inputData%SpdUnits, 'SpdUnits', 'Wind-speed units (mps, fps, mph).'                                       )


      ! Set units conversion and compute TSR parameters.

  ! CALL SetConv


         ! No sense creating a BED file if we're not putting anything in it.

      IF ( NumElmPr == 0 )  inputData%WriteBED = .FALSE.


      ! Read the combined-case section.

   call ReadCom  ( unIn, fileName,                       'the combined-case subtitle'     )
   call ReadVar  ( unIn, fileName, inputData%NumCases, 'NumCases', 'Number of cases to run.'        )
   call ReadCom  ( unIn, fileName,                       'the combined-case-block header' )

   IF ( inputData%NumCases < 0 )  THEN

      CALL Abort ( ' Variable "NumCases" must be >= 0.  Instead, it is "'//TRIM( Int2LStr( inputData%NumCases ) )//'".' )

   ELSEIF ( inputData%NumCases > 0 )  THEN

      ALLOCATE ( inputData%Cases(inputData%NumCases) , STAT=Sttus )
      IF ( Sttus /= 0 )  THEN
         CALL Abort(' Error allocating memory for the Cases array.')
      ENDIF

      DO ICase=1,inputData%NumCases

         CALL ReadAry ( unIn, fileName, InpCase,  4, 'InpCase',  'Wind Speed or TSR, Rotor Speed, and Pitch for Case #' &
                       //TRIM( Int2LStr( ICase ) )//'.', ErrStat2, ErrMsg2 )

         IF ( inputData%InputTSR )  THEN
            inputData%Cases(ICase)%TSR      = InpCase(1)
            inputData%Cases(ICase)%WndSpeed = inputData%RotorRad*InpCase(2)*Pi/( 30.0*InpCase(1) )
         ELSE
            inputData%Cases(ICase)%TSR      = inputData%RotorRad*InpCase(2)*Pi/( 30.0*InpCase(1) )
            inputData%Cases(ICase)%WndSpeed = InpCase(1)
         ENDIF

         inputData%Cases(ICase)%RotSpeed = InpCase(2)
         inputData%Cases(ICase)%Pitch    = InpCase(3)
         inputData%Cases(ICase)%Yaw      = InpCase(4)

      ENDDO ! ICase
   END IF
   
!   ELSE ! ( NumCases ==0 )
!
!
!         ! Read the parametric-analysis-configuration section.
!
!      CALL ReadCom ( unIn, fileName,                   'the parametric-analysis-configuration subtitle'  )
!      CALL ReadVar ( unIn, fileName, ParRow, 'ParRow', 'Row parameter    (1-rpm, 2-pitch, 3-TSR/speed).' )
!      CALL ReadVar ( unIn, fileName, ParCol, 'ParCol', 'Column parameter (1-rpm, 2-pitch, 3-TSR/speed).' )
!      CALL ReadVar ( unIn, fileName, ParTab, 'ParTab', 'Sheet parameter  (1-rpm, 2-pitch, 3-TSR/speed).' )
!      CALL ReadVar ( unIn, fileName, OutPwr, 'OutPwr', 'Request output of rotor power?'                  )
!      CALL ReadVar ( unIn, fileName, OutCp,  'OutCp',  'Request output of Cp?'                           )
!      CALL ReadVar ( unIn, fileName, OutTrq, 'OutTrq', 'Request output of shaft torque?'                 )
!      CALL ReadVar ( unIn, fileName, OutFlp, 'OutFlp', 'Request output of flap bending moment?'          )
!      CALL ReadVar ( unIn, fileName, OutThr, 'OutThr', 'Request output of rotor thrust?'                 )
!
!
!         ! Check for valid choices.
!
!      IF ( ( ParRow < 1 ) .OR. ( ParRow > 3 ) )  THEN
!         CALL Abort ( ' Variable "ParRow" must be between 1 and 3 (inclusive).  Instead, it is "'//TRIM( Int2LStr( ParRow ) ) &
!                      //'".' )
!      ENDIF
!
!      IF ( ( ParCol < 1 ) .OR. ( ParCol > 3 ) )  THEN
!         CALL Abort ( ' Variable "ParCol" must be between 1 and 3 (inclusive).  Instead, it is "'//TRIM( Int2LStr( ParCol ) ) &
!                      //'".' )
!      ENDIF
!
!      IF ( ( ParTab < 1 ) .OR. ( ParTab > 3 ) )  THEN
!         CALL Abort ( ' Variable "ParTab" must be between 1 and 3 (inclusive).  Instead, it is "'//TRIM( Int2LStr( ParTab ) ) &
!                      //'".' )
!      ENDIF
!
!      IF ( ParCol == ParRow )  THEN
!         CALL Abort ( ' Variable "ParCol" must differ from "ParRow".  Both are "'//TRIM( Int2LStr( ParRow ) )//'".' )
!      ENDIF
!
!      IF ( ParTab == ParRow )  THEN
!         CALL Abort ( ' Variable "ParTab" must differ from "ParRow".  Both are "'//TRIM( Int2LStr( ParRow ) )//'".' )
!      ELSEIF ( ParTab == ParCol )  THEN
!         CALL Abort ( ' Variable "ParTab" must differ from "ParCol".  Both are "'//TRIM( Int2LStr( ParCol ) )//'".' )
!      ENDIF
!
!
!         ! Make sure at least on request is made for output.
!
!      IF ( ( .NOT. OutCp  ) .AND. &
!           ( .NOT. OutFlp ) .AND. &
!           ( .NOT. OutPwr ) .AND. &
!           ( .NOT. OutThr ) .AND. &
!           ( .NOT. OutTrq ) )  THEN
!
!            CALL Abort ( ' No output requested.  At least one of OutCp, OutFlp, OutPwr, OutThr, OutTrq must be TRUE.' )
!
!      ENDIF
!
!      CALL ReadRAry ( unIn, fileName, PitSets,  3, 'PitSets',  'First, last, delta blade pitch (deg).' )
!      CALL ReadRAry ( unIn, fileName, OmgSets,  3, 'OmgSets',  'First, last, delta rotor speed (rpm).' )
!      CALL ReadRAry ( unIn, fileName, SpdSets,  3, 'SpdSets',  'First, last, delta speeds.'            )
!
!
!         ! Check for valid choices.
!
!      IF ( ( PitSets(3) /= 0.0 ) .AND. ( PitSets(2) - PitSets(1) )/PitSets(3) < 0.0 ) &
!                                       CALL Abort ( ' Your pitch settings (PitSt, PitEnd, PitDel) are not consistent.' )
!      IF ( ( OmgSets(3) /= 0.0 ) .AND. ( OmgSets(2) - OmgSets(1) )/OmgSets(3) < 0.0 ) &
!                                       CALL Abort ( ' Your rotor-speed settings (OmgSt, OmgEnd, OmgDel) are not consistent.' )
!      IF ( ( SpdSets(3) /= 0.0 ) .AND. ( SpdSets(2) - SpdSets(1) )/SpdSets(3) < 0.0 ) &
!                                       CALL Abort ( ' Your speed settings (SpdSt, SpdEnd, SpdDel) are not consistent.' )
!
!      PitSt  = PitSets(1)
!      PitEnd = PitSets(2)
!      PitDel = PitSets(3)
!
!      OmgSt  = OmgSets(1)
!      OmgEnd = OmgSets(2)
!      OmgDel = OmgSets(3)
!
!      SpdSt  = SpdSets(1)
!      SpdEnd = SpdSets(2)
!      SpdDel = SpdSets(3)
!
!      IF ( .NOT. InputTSR )  ParamStr(3) = 'WndSp'
!
!
!         ! Allocate the parameter arrays.
!
!      CALL AllocPar
!
!
!         ! Load the parameter arrays.
!
!      DO IPit=1,NumPit
!         PitAry(IPit) = PitSt + PitDel*( IPit - 1 )
!      ENDDO ! IPit
!
!      DO IOmg=1,NumOmg
!         OmgAry(IOmg) = OmgSt + OmgDel*( IOmg - 1 )
!      ENDDO ! IOmg
!
!      DO ISpd=1,NumSpd
!         SpdAry(ISpd) = SpdSt + SpdDel*( ISpd - 1 )
!      ENDDO ! ISpd
!
!
!         ! Point the row, column, and table parameter arrays to the appropiate pitch, omega, and speed parameter arrays.
!
!      SELECT CASE ( ParTab )
!      CASE ( 1 )
!         IF ( ParCol == 2 )  THEN
!            RowAry => SpdAry
!            ColAry => PitAry
!            TabAry => OmgAry
!         ELSE
!            RowAry => PitAry
!            ColAry => SpdAry
!            TabAry => OmgAry
!         ENDIF
!      CASE ( 2 )
!         IF ( ParCol == 1 )  THEN
!            RowAry => SpdAry
!            ColAry => OmgAry
!            TabAry => PitAry
!         ELSE
!            RowAry => OmgAry
!            ColAry => SpdAry
!           TabAry => PitAry
!         ENDIF
!      CASE ( 3 )
!         IF ( ParCol == 1 )  THEN
!            RowAry => PitAry
!            ColAry => OmgAry
!            TabAry => SpdAry
!         ELSE
!            RowAry => OmgAry
!            ColAry => PitAry
!            TabAry => SpdAry
!         ENDIF
!      END SELECT
!
!   ENDIF ! ( NumCases > 0 )
!
!      ! Close the input and echo files.

   CLOSE ( unIn )

  ! IF ( Echo )  CLOSE ( UnEc )


   RETURN
   
contains

logical function OnCheckErr()

   if ( errStat2 >= AbortErrLev ) then
      OnCheckErr = .TRUE.
   else
      OnCheckErr = .FALSE.
   end if

end function OnCheckErr

END SUBROUTINE WTP_ReadInputFile


subroutine WTP_WriteOutputLine(unOutFile, t, delim, outFmt, output, errStat, errMsg)

   integer(IntKi)         ,  intent(in   )   :: unOutFile            ! File unit for the output file
   real(DbKi)             ,  intent(in   )   :: t                    ! simulation time (s)
   character(1)           ,  intent(in   )   :: delim
   character(*)           ,  intent(in   )   :: outFmt
   real(ReKi)             ,  intent(in   )   :: output(:)            ! Rootname for the output file
   integer(IntKi)         ,  intent(inout)   :: errStat              ! Status of error message
   character(*)           ,  intent(inout)   :: errMsg               ! Error message if ErrStat /= ErrID_None
      
   ! Local variables.

   integer(IntKi)                   :: i                                         ! loop counter
   integer(IntKi)                   :: indxLast                                  ! The index of the last row value to be written to AllOutData for this time step (column).
   integer(IntKi)                   :: indxNext                                  ! The index of the next row value to be written to AllOutData for this time step (column).

   character(200)                   :: frmt                                      ! A string to hold a format specifier
   character(15)                    :: tmpStr                                    ! temporary string to print the time output as text
integer :: numOuts
   
   errStat = ErrID_None
   errMsg  = ''
   numOuts = size(output,1)
   frmt = '"'//delim//'"'//trim(outFmt)      ! format for array elements from individual modules
   
      ! time
   write( tmpStr, '(F15.4)' ) t
   call WrFileNR( unOutFile, tmpStr )
   call WrReAryFileNR ( unOutFile, output,  frmt, errStat, errMsg )
   if ( errStat >= AbortErrLev ) return
   
     ! write a new line (advance to the next line)
   write (unOutFile,'()')
      
end subroutine WTP_WriteOutputLine


subroutine WTP_InitializeOutputFile( version, delim, outFmtS, outFileRoot, WriteOutputHdr, WriteOutputUnt, unOutFile, errStat, errMsg)
      type(ProgDesc)         ,  intent(in   )   :: version
      character(1)           ,  intent(in   )   :: delim
      character(*)           ,  intent(in   )   :: outFmtS
      character(*)           ,  intent(in   )   :: outFileRoot          ! Rootname for the output file
      character(*)          ,  intent(in   )   :: WriteOutputHdr(:) 
      character(*)          ,  intent(in   )   :: WriteOutputUnt(:) 
      integer(IntKi)         ,  intent(  out)   :: unOutFile            ! File unit for the output file
      integer(IntKi)         ,  intent(inout)   :: errStat              ! Status of error message
      character(*)           ,  intent(inout)   :: errMsg               ! Error message if ErrStat /= ErrID_None

         ! locals
      integer(IntKi)                            :: i, j
      
      integer(IntKi)                            :: numOuts
      
      integer(IntKi)                            :: errStat2             ! local status of error message
      character(len(errMsg))                    :: errMsg2              ! local error message if ErrStat /= ErrID_None
      character(200)                            :: frmt ,frmt2                                     ! A string to hold a format specifier
      errStat2 = ErrID_None
      errMsg2  = ''
      
      !   ! Establish the number of output channels
      !allocate (WriteOutputHdr(numOuts), Stat = errStat2)
      !if ( errStat2 /= 0 ) then
      !   errStat2 = ErrID_Fatal
      !   errMsg2  = 'Error allocating memory for WriteOutputHdr.'
      !   call SetErrStat( errStat2, errMsg2, errStat, errMsg, 'WTP_DvrInitializeOutputFile' )
      !   return
      !end if 
      !
      !allocate (WriteOutputUnt(numOuts), Stat = errStat2)
      !if ( errStat2 /= 0 ) then
      !   errStat2 = ErrID_Fatal
      !   errMsg2  = 'Error allocating memory for WriteOutputUnt.'
      !   call SetErrStat( errStat2, errMsg2, errStat, errMsg, 'WTP_DvrInitializeOutputFile' )
      !   return
      !end if 
      
      numOuts = size(WriteOutputHdr)
      
      call GetNewUnit( unOutFile, ErrStat, ErrMsg )
         if ( ErrStat >= AbortErrLev ) return

      call OpenFOutFile ( unOutFile, trim(outFileRoot)//'.WTP.out', ErrStat, ErrMsg )
         if ( ErrStat >= AbortErrLev ) return
         
      write (unOutFile,'(/,A)')  'Predictions were generated on '//CurDate()//' at '//CurTime()//' using '//trim(GetNVD(version))
      
      write (unOutFile,'()' )    !print a blank line


         !......................................................
         ! Write the names of the output parameters on one line:
         !......................................................

      frmt = '"'//delim//'"'//trim(outFmtS)      ! format for array elements 
   
      
      
      !write (unOutFile,trim(outFmtS) ,ADVANCE='NO',IOSTAT=errStat)  '   Time        '
      call WrFileNR ( unOutFile, '     Time           ' )

      write(frmt2,*) SIZE(WriteOutputHdr)
      frmt2 = '('//trim(frmt2)//'('//trim(frmt)//'))'

      write (unOutFile,frmt2,ADVANCE='NO',IOSTAT=errStat)  WriteOutputHdr
      if ( errStat /= 0 ) then
         errMsg = 'Error '//trim(Num2LStr(errStat))//' occurred while writing to file in WTP_DvrInitializeOutputFile() using this format: '&
                  //trim(frmt2)
         errStat = ErrID_Fatal
      end if
      if ( errStat >= AbortErrLev ) return
      
      !do i=1,numOuts
      !   call WrFileNR ( unOutFile, delim//BEMT_InitOutData%WriteOutputHdr(i) )
      !end do ! I

      write (unOutFile,'()')

         !......................................................
         ! Write the units of the output parameters on one line:
         !......................................................

      call WrFileNR ( unOutFile, '      (s)           ' )

      write (unOutFile,frmt2,ADVANCE='NO',IOSTAT=errStat)  WriteOutputUnt
      if ( errStat /= 0 ) then
         errMsg = 'Error '//trim(Num2LStr(errStat))//' occurred while writing to file in WTP_DvrInitializeOutputFile() using this format: '&
                  //trim(frmt2)
         errStat = ErrID_Fatal
      end if
      if ( errStat >= AbortErrLev ) return
      write (unOutFile,'()' )    
      
end subroutine WTP_InitializeOutputFile


end module WT_Perf_IO
   