module WT_Perf_IO
   USE                             NWTC_Library
   USE                             WTP_Types
   
   IMPLICIT                        NONE

   public :: WTP_ReadInputFile
   
   
   contains
   
subroutine WTP_ReadInputFile(fileName, inputData, errStat, errMsg )
   ! This routine opens the gets the data from the input files.

   character(*),                  intent( in    )   :: fileName
   type(WTP_InputFileData),       intent(   out )   :: inputData
   integer,                       intent(   out )   :: errStat              ! returns a non-zero value when an error occurs  
   character(*),                  intent(   out )   :: errMsg               ! Error message if errStat /= ErrID_None
   

      ! Local variables
   character(1024)              :: PriPath
   character(1024)              :: inpVersion                               ! String containing the input-version information.
   character(1024)              :: line                                     ! String containing a line of input.
   character(1024)              :: subTitle                                 ! String containing the RunTitle of a subsection.
   integer                      :: unIn, unEc, NumElmPr
   integer                      :: ISeg, ICase
   integer                      :: IOS, Sttus
   character( 11)               :: DateNow                                  ! Date shortly after the start of execution.
   character(  8)               :: TimeNow                                  ! Time of day shortly after the start of execution.
   real(ReKi)                   :: InpCase(4)                               ! Temporary array to hold combined-case input parameters.
   
   INTEGER(IntKi)               :: ErrStat2                                        ! Temporary Error status
   CHARACTER(ErrMsgLen)         :: ErrMsg2                                         ! Temporary Err msg
   CHARACTER(*), PARAMETER      :: RoutineName = 'WTP_ReadInputFile'
   
   
   ErrStat = ErrID_None
   ErrMsg  = ''
   UnIn = -1
   UnEc = -1
   
   ! Open the input file
   CALL GetPath( fileName, PriPath )     ! Input files will be relative to the path where the primary input file is located.

   call GetNewUnit( unIn )   
   call OpenFInpFile( unIn, fileName, errStat2, ErrMsg2 )
   call setErrStat( errStat2, ErrMsg2 , errStat, ErrMsg , RoutineName )
   if ( errStat >= AbortErrLev ) then
      call cleanup()
      return
   end if

   
   call WrScr( 'Opening WT_Perf input file:  '//fileName )

      ! Skip a line, read the run title and the version information.

   CALL ReadStr( UnIn, fileName, inpVersion, 'inpVersion', 'File Header: (line 1)', ErrStat2, ErrMsg2 )
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
   
   CALL ReadStr( UnIn, fileName, inputData%runTitle, 'runTitle', 'File Header: File Description (line 2)', ErrStat2, ErrMsg2 )
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
   
   call WrScr1 ( ' '//inputData%runTitle )

   CALL ReadStr( UnIn, fileName, subTitle, 'subTitle', 'File Header: (line 3)', ErrStat2, ErrMsg2 )
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
   
      ! Read in the title line for the input-configuration subsection.
   CALL ReadStr( UnIn, fileName, line, 'line', 'File Header: (line 4)', ErrStat2, ErrMsg2 )
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )


      ! See if we should echo the output.     
   call ReadVar ( unIn, fileName, inputData%echo, 'Echo', 'Echo Input', errStat2, errMsg2 )
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )

   if ( inputData%echo )  then
         ! Get date and time.
      dateNow = CurDate()
      timeNow = CurTime()
      call GetNewUnit( unEc ) 
      call getroot(fileName,inputData%OutFileRoot)      
      call  OpenFOutFile ( unEc, trim( inputData%OutFileRoot )//'.ech', errStat2, errMsg2 )
         call setErrStat( errStat2, ErrMsg2 , errStat, ErrMsg , RoutineName )
         if ( errStat >= AbortErrLev ) then
            call cleanup()
            return
         end if
      
      write (unEc,'(A)')      'Echo of WT_Perf Input File:'
      write (unEc,'(A)')      ' "'//fileName//'"'
      write (unEc,'(A)')      'Generated on: '//trim( dateNow )//' at '//trim( timeNow )//'.'
      write (unEc,'(A)')      inpVersion
      write (unEc,'(A)')      inputData%runTitle
      write (unEc,'(A)')      subTitle
      write (unEc,'(A)')      line
      write (unEc,Ec_LgFrmt)  inputData%echo, 'Echo', 'Echo input parameters to "rootname.ech"?'
   end if


      ! Read the rest of input-configuration section.
      
   call ReadVar ( unIn, fileName, inputData%AD_InputFile,   'AD_InputFile',   'Name of the AeroDyn input file', errStat2, errMsg2, UnEc )
      call setErrStat( errStat2, ErrMsg2 , errStat, ErrMsg , RoutineName )
      if ( errStat >= AbortErrLev ) then
         call cleanup()
         return
      end if
   IF ( PathIsRelative( inputData%AD_InputFile ) ) inputData%AD_InputFile = TRIM(PriPath)//TRIM(inputData%AD_InputFile)



      ! Read the model-configuration section.

   call ReadCom ( unIn, fileName,                                 'the model-configuration subtitle', errStat2, errMsg2, UnEc )
      call setErrStat( errStat2, ErrMsg2 , errStat, ErrMsg , RoutineName )
   call ReadVar ( unIn, fileName, inputData%numSect,  'numSect',  'Number of circumferential sectors.', errStat2, errMsg2, UnEc )
      call setErrStat( errStat2, ErrMsg2 , errStat, ErrMsg , RoutineName )
   !call ReadVar ( unIn, fileName, inputData%SWTol,    'SWTol',    'Error tolerance for skewed-wake iteration.'      )


      ! Check for valid choices.

   if ( inputData%NumSect < 1 )  then
      call SetErrStat ( ErrID_Fatal,'Variable "numSect" must be greater than 0.  Instead, it is "'//trim( Int2LStr( inputData%numSect ) )//'".',ErrStat,ErrMsg,RoutineName )
      call cleanup()
      return
   endif


   
      ! Read the algorithm-configuration section.

   !if ( inputData%useAIDrag )  then
   !   inputData%AIDragM = 1.0
   !else
   !   inputData%AIDragM = 0.0
   !endif
   !
   !if ( inputData%useTIDrag )  then
   !   inputData%TIDragM = 1.0
   !else
   !   inputData%TIDragM = 0.0
   !endif

   !call ReadVar ( unIn, fileName, inputData%TISingularity,   'TISingularity',   'Use the singularity avoidance method in the tangential-induction calculation?' )

      ! Check for valid choices.


 !  IF ( ( IndType < 0 ) .OR. ( IndType > 1 ) )  THEN
  !    CALL Abort ( ' Variable "IndType" must be 0 or 1.  Instead, it is '//Trim( Int2LStr( IndType ) )//'.' )
 !  ELSEIF ( IndType == 0 )  THEN    ! Don't do skewed wakes if induction calculations are disabled.
  !    SkewWake = .FALSE.
  ! ENDIF

   !if ( .NOT. inputData%useInduction )  then    ! Don't do skewed wakes if induction calculations are disabled.
   !   inputData%skewWakeMod = 0
   !endif


      ! Read the turbine-data section.

   call ReadCom ( unIn, fileName, 'the turbine-data subtitle', errStat2, errMsg2, UnEc )
      call setErrStat( errStat2, ErrMsg2 , errStat, ErrMsg , RoutineName )
   call ReadVar ( unIn, fileName, inputData%NumBlade, 'NumBlade', 'Number of blades.', errStat2, errMsg2, UnEc )
      call setErrStat( errStat2, ErrMsg2 , errStat, ErrMsg , RoutineName )
   call ReadVar ( unIn, fileName, inputData%RotorRad, 'RotorRad', 'Unconed rotor radius.', errStat2, errMsg2, UnEc )
      call setErrStat( errStat2, ErrMsg2 , errStat, ErrMsg , RoutineName )
   call ReadVar ( unIn, fileName, inputData%HubRad,   'HubRad',   'Hub radius.', errStat2, errMsg2, UnEc )
      call setErrStat( errStat2, ErrMsg2 , errStat, ErrMsg , RoutineName )
   call ReadVar ( unIn, fileName, inputData%PreCone,  'PreCone',  'Cone angle.', errStat2, errMsg2, UnEc )
      call setErrStat( errStat2, ErrMsg2 , errStat, ErrMsg , RoutineName )
   call ReadVar ( unIn, fileName, inputData%Tilt,     'Tilt',     'Shaft tilt.', errStat2, errMsg2, UnEc )
      call setErrStat( errStat2, ErrMsg2 , errStat, ErrMsg , RoutineName )
   call ReadVar ( unIn, fileName, inputData%Yaw,      'Yaw',      'Yaw error.', errStat2, errMsg2, UnEc )
      call setErrStat( errStat2, ErrMsg2 , errStat, ErrMsg , RoutineName )
   call ReadVar ( unIn, fileName, inputData%HubHt,    'HubHt',    'Hub height.', errStat2, errMsg2, UnEc )
      call setErrStat( errStat2, ErrMsg2 , errStat, ErrMsg , RoutineName )

      if ( errStat >= AbortErrLev ) then
         call cleanup()
         return
      end if
      
  
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
   
  
   NumElmPr = 0


      ! Compute the segment lengths and check their validity.
!TODO


       ! The Tim Olsen memorial hub-radius check.

   !if ( ( inputData%HubRadND < 0.0 ) .OR. ( 1.0 <= inputData%HubRadND ) )  then
   !   errMsg = ' The hub radius must be positive and less than the rotor radius.  Instead it is ' &
   !              //trim( Num2LStr( inputData%HubRad ) )//'meters.' 
   !   errStat = ErrID_Fatal
   !   return
   !endif
!TODO

      ! Make sure hub is high enough so the blade doesn't hit the ground.  We wouldn't want to get it dirty.  :-)

   if ( inputData%HubHt*inputData%CosCone*inputData%CosTilt .LT. 1.0 )  call Abort ( ' The hub is so low, the blade will hit the ground.' )


      ! Read the aerodynamic-data section.

   call ReadCom ( unIn, fileName, 'the aerodynamic-data subtitle', errStat2, errMsg2, UnEc )
      call setErrStat( errStat2, ErrMsg2 , errStat, ErrMsg , RoutineName )
   call ReadVar ( unIn, fileName, inputData%ShearExp, 'ShearExp', 'Shear exponent.', errStat2, errMsg2, UnEc )
      call setErrStat( errStat2, ErrMsg2 , errStat, ErrMsg , RoutineName )



      ! Allocate the airfoil data super-supertables for both unique data and complete data.
!TODO
   !ALLOCATE ( AF_Table(NumAF) , STAT=Sttus )
   !IF ( Sttus /= 0 )  THEN
   !   CALL Abort ( ' Error allocating memory for the AF_Uniq super-supertable in T_GetAF.' )
   !ENDIF


      ! Make sure we have a minimum of four sectors if we have shear, shaft tilt, or yaw.

   !if (  ( inputData%Tilt /= 0.0 ) .OR. ( inputData%Yaw /= 0.0 ) .OR. ( inputData%ShearExp /= 0.0 ) )  THEN
   !   inputData%NumSect = max( inputData%NumSect, 4 )
   !else
   !   inputData%NumSect = 1
   !endif


      ! Read the I/O-configuration section.

   call ReadCom ( unIn, fileName, 'the I/O-configuration subtitle', errStat2, errMsg2, UnEc )
      call setErrStat( errStat2, ErrMsg2 , errStat, ErrMsg , RoutineName )
   call ReadVar ( unIn, fileName, inputData%OutFileRoot, 'OutFileRoot', 'Root name for any output files', errStat2, errMsg2, UnEc )
      call setErrStat( errStat2, ErrMsg2 , errStat, ErrMsg , RoutineName )
   if (len_trim(inputData%OutFileRoot) == 0) then
      call getroot(fileName,inputData%OutFileRoot)
   end if
   
   call ReadVar ( unIn, fileName, inputData%UnfPower, 'UnfPower', 'Write Power to an unformatted file?', errStat2, errMsg2, UnEc )
      call setErrStat( errStat2, ErrMsg2 , errStat, ErrMsg , RoutineName )
   call ReadVar ( unIn, fileName, inputData%TabDel,   'TabDel',   'Make output tab-delimited (fixed-width otherwise)?', errStat2, errMsg2, UnEc )
      call setErrStat( errStat2, ErrMsg2 , errStat, ErrMsg , RoutineName )
   call ReadVar ( unIn, fileName, inputData%OutNines, 'OutNines', 'Output nines for cases that fail to satisfy the convergence criterion?', errStat2, errMsg2, UnEc )
      call setErrStat( errStat2, ErrMsg2 , errStat, ErrMsg , RoutineName )
   call ReadVar ( unIn, fileName, Beep,               'Beep',     'Beep on exit?', errStat2, errMsg2, UnEc )
      call setErrStat( errStat2, ErrMsg2 , errStat, ErrMsg , RoutineName ) !bjj: this is a global variable in NWTC_Library
   call ReadVar ( unIn, fileName, inputData%KFact,    'KFact',    'Output dimensional parameters in K?', errStat2, errMsg2, UnEc )
      call setErrStat( errStat2, ErrMsg2 , errStat, ErrMsg , RoutineName )
   call ReadVar ( unIn, fileName, inputData%WriteBED, 'WriteBED', 'Write out blade element data to "bladelem.dat"?', errStat2, errMsg2, UnEc )
      call setErrStat( errStat2, ErrMsg2 , errStat, ErrMsg , RoutineName )
   call ReadVar ( unIn, fileName, inputData%InputTSR, 'InputTSR', 'Input speeds as TSRs?', errStat2, errMsg2, UnEc )
      call setErrStat( errStat2, ErrMsg2 , errStat, ErrMsg , RoutineName )
   call ReadVar ( unIn, fileName, inputData%OutMaxCp, 'OutMaxCp', 'Output conditions leading to maximum Cp?', errStat2, errMsg2, UnEc )
      call setErrStat( errStat2, ErrMsg2 , errStat, ErrMsg , RoutineName )
      if ( errStat >= AbortErrLev ) then
         call cleanup()
         return
      end if


      ! Set units conversion and compute TSR parameters.

  ! CALL SetConv


         ! No sense creating a BED file if we're not putting anything in it.

      IF ( NumElmPr == 0 )  inputData%WriteBED = .FALSE.


      ! Read the combined-case section.

   call ReadCom  ( unIn, fileName, 'the combined-case subtitle', errStat2, errMsg2, UnEc )
      call setErrStat( errStat2, ErrMsg2 , errStat, ErrMsg , RoutineName )
   call ReadVar  ( unIn, fileName, inputData%NumCases, 'NumCases', 'Number of cases to run.', errStat2, errMsg2, UnEc )
      call setErrStat( errStat2, ErrMsg2 , errStat, ErrMsg , RoutineName )
   call ReadCom  ( unIn, fileName, 'the combined-case-block header', errStat2, errMsg2, UnEc )
      call setErrStat( errStat2, ErrMsg2 , errStat, ErrMsg , RoutineName )

      if ( errStat >= AbortErrLev ) then
         call cleanup()
         return
      end if
      
   IF ( inputData%NumCases < 0 )  THEN

      call setErrStat( ErrID_Fatal,'Variable "NumCases" must be >= 0.  Instead, it is "'//TRIM( Int2LStr( inputData%NumCases ) )//'".' ,errstat,errmsg,routinename)
      call cleanup()
      return

   ELSEIF ( inputData%NumCases > 0 )  THEN

      ALLOCATE ( inputData%Cases(inputData%NumCases) , STAT=Sttus )
      IF ( Sttus /= 0 )  THEN
         call setErrStat( ErrID_Fatal,'Error allocating memory for the Cases array.',errstat,errmsg,routinename)
         call cleanup()
         return
      ENDIF

      DO ICase=1,inputData%NumCases

         CALL ReadAry ( unIn, fileName, InpCase,  4, 'InpCase',  'Wind Speed or TSR, Rotor Speed, and Pitch for Case #' &
                       //TRIM( Int2LStr( ICase ) )//'.', errStat2, errMsg2, UnEc )
            call setErrStat( errStat2, ErrMsg2 , errStat, ErrMsg , RoutineName )
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

   call cleanup ( )


   RETURN
contains
   subroutine cleanup()
      if (UnIn>0) close(UnIn)
      if (UnEc>0) close(UnEc)
   end subroutine cleanup
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
      integer(IntKi)                            ::  j
      
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
   