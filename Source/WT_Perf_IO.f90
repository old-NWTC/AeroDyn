module WT_Perf_IO
   USE                             NWTC_Library
   USE                             WTP_Types
   
   IMPLICIT                        NONE  
   
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
   real(ReKi)                   :: InpCase(10)                              ! Temporary array to hold combined-case input parameters.
   logical                      :: TabDel      

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
   
   CALL ReadStr( UnIn, fileName, inputData%OutFileData%runTitle, 'runTitle', 'File Header: File Description (line 2)', ErrStat2, ErrMsg2 )
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
   
   call WrScr1 ( ' '//inputData%OutFileData%runTitle )

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
      call getroot(fileName,inputData%OutFileData%Root)      
      call  OpenFOutFile ( unEc, trim( inputData%OutFileData%Root )//'.ech', errStat2, errMsg2 )
         call setErrStat( errStat2, ErrMsg2 , errStat, ErrMsg , RoutineName )
         if ( errStat >= AbortErrLev ) then
            call cleanup()
            return
         end if
      
      write (unEc,'(A)')      'Echo of Input File:'
      write (unEc,'(A)')      ' "'//fileName//'"'
      write (unEc,'(A)')      'Generated on: '//trim( dateNow )//' at '//trim( timeNow )//'.'
      write (unEc,'(A)')      inpVersion
      write (unEc,'(A)')      inputData%OutFileData%runTitle
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

   
      ! Read the turbine-data section.

   call ReadCom ( unIn, fileName, 'the turbine-data subtitle', errStat2, errMsg2, UnEc )
      call setErrStat( errStat2, ErrMsg2 , errStat, ErrMsg , RoutineName )
   call ReadVar ( unIn, fileName, inputData%NumBlade, 'NumBlade', 'Number of blades.', errStat2, errMsg2, UnEc )
      call setErrStat( errStat2, ErrMsg2 , errStat, ErrMsg , RoutineName )
   call ReadVar ( unIn, fileName, inputData%HubRad,   'HubRad',   'Hub radius.', errStat2, errMsg2, UnEc )
      call setErrStat( errStat2, ErrMsg2 , errStat, ErrMsg , RoutineName )
   call ReadVar ( unIn, fileName, inputData%HubHt,    'HubHt',    'Hub height.', errStat2, errMsg2, UnEc )
      call setErrStat( errStat2, ErrMsg2 , errStat, ErrMsg , RoutineName )

      if ( errStat >= AbortErrLev ) then
         call cleanup()
         return
      end if           


      ! Read the I/O-configuration section.

   call ReadCom ( unIn, fileName, 'the I/O-configuration subtitle', errStat2, errMsg2, UnEc )
      call setErrStat( errStat2, ErrMsg2 , errStat, ErrMsg , RoutineName )
   call ReadVar ( unIn, fileName, inputData%OutFileData%Root, 'OutFileRoot', 'Root name for any output files', errStat2, errMsg2, UnEc )
      call setErrStat( errStat2, ErrMsg2 , errStat, ErrMsg , RoutineName )
   if (len_trim(inputData%OutFileData%Root) == 0) then
      call getroot(fileName,inputData%OutFileData%Root)
   end if
   
   call ReadVar ( unIn, fileName, TabDel,   'TabDel',   'Make output tab-delimited (fixed-width otherwise)?', errStat2, errMsg2, UnEc )
      call setErrStat( errStat2, ErrMsg2 , errStat, ErrMsg , RoutineName )
      if (TabDel) then
         inputData%OutputFileData%delim = TAB
      else
         inputData%OutputFileData%delim = " "
      end if
               
   call ReadVar ( unIn, fileName, Beep,               'Beep',     'Beep on exit?', errStat2, errMsg2, UnEc )
      call setErrStat( errStat2, ErrMsg2 , errStat, ErrMsg , RoutineName ) !bjj: this is a global variable in NWTC_Library
   call ReadVar ( unIn, fileName, inputData%InputTSR, 'InputTSR', 'Input speeds as TSRs?', errStat2, errMsg2, UnEc )
      call setErrStat( errStat2, ErrMsg2 , errStat, ErrMsg , RoutineName )
      if ( errStat >= AbortErrLev ) then
         call cleanup()
         return
      end if


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

         CALL ReadAry ( unIn, fileName, InpCase,  10, 'InpCase',  'Wind Speed or TSR, Rotor Speed, and Pitch for Case #' &
                       //TRIM( Int2LStr( ICase ) )//'.', errStat2, errMsg2, UnEc )
            call setErrStat( errStat2, ErrMsg2 , errStat, ErrMsg , RoutineName )
            
         inputData%Cases(ICase)%TSR_or_WndSpeed = InpCase( 1) ! we'll calculate WS and TSR later, after we get RotorRad from AD                                      
         inputData%Cases(ICase)%ShearExp        = InpCase( 2)
         inputData%Cases(ICase)%RotSpeed        = InpCase( 3)
         inputData%Cases(ICase)%Pitch           = InpCase( 4)*D2R
         inputData%Cases(ICase)%Yaw             = InpCase( 5)*D2R
         inputData%Cases(ICase)%Tilt            = InpCase( 6)*D2R
         inputData%Cases(iCase)%PreCone         = InpCase( 7)*D2R
         inputData%Cases(iCase)%AzAng0          = InpCase( 8)*D2R
         inputData%Cases(iCase)%dT              = InpCase( 9)*D2R
         inputData%Cases(iCase)%Tmax            = InpCase(10)*D2R
               
      ENDDO ! ICase
   END IF
   
   call cleanup ( )


   RETURN
contains
   subroutine cleanup()
      if (UnIn>0) close(UnIn)
      if (UnEc>0) close(UnEc)
   end subroutine cleanup
END SUBROUTINE WTP_ReadInputFile


subroutine WTP_WriteOutputLine(unOutFile, OutFileData, t, output, errStat, errMsg)

   integer(IntKi)         ,  intent(in   )   :: unOutFile            ! File unit for the output file
   real(DbKi)             ,  intent(in   )   :: t                    ! simulation time (s)
   type(WTP_OutputFile)   ,  intent(in   )   :: OutFileData
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
   frmt = '"'//OutFileData%delim//'"'//trim(OutFileData%outFmt)      ! format for array elements from individual modules
   
      ! time
   write( tmpStr, '(F15.4)' ) t
   call WrFileNR( unOutFile, tmpStr )
   call WrReAryFileNR ( unOutFile, output,  frmt, errStat, errMsg )
   if ( errStat >= AbortErrLev ) return
   
     ! write a new line (advance to the next line)
   write (unOutFile,'()')
      
end subroutine WTP_WriteOutputLine


subroutine WTP_InitializeOutputFile( version, iCase, CaseData, OutFileData, WriteOutputHdr, WriteOutputUnt, unOutFile, errStat, errMsg)
      type(ProgDesc)         ,  intent(in   )   :: version
      type(WTP_OutputFile),     intent(in   )   :: OutFileData
      
      integer(IntKi)         ,  intent(in   )   :: iCase                ! case number (to write in file description line and use for file name)
      type(WTP_Case),           intent(in   )   :: CaseData
      
      integer(IntKi)         ,  intent(  out)   :: unOutFile            ! File unit for the output file
      integer(IntKi)         ,  intent(inout)   :: errStat              ! Status of error message
      character(*)           ,  intent(inout)   :: errMsg               ! Error message if ErrStat /= ErrID_None

         ! locals
      integer(IntKi)                            ::  j
      
      integer(IntKi)                            :: numOuts
      
      character(200)                            :: frmt ,frmt2         ! A string to hold a format specifier

      
      numOuts = size(OutFileData%WriteOutputHdr)
      
      call GetNewUnit( unOutFile, ErrStat, ErrMsg )
         if ( ErrStat >= AbortErrLev ) return

      call OpenFOutFile ( unOutFile, trim(outFileData%Root)//'.WTP.'//trim(num2lstr(iCase))//'.out', ErrStat, ErrMsg )
         if ( ErrStat >= AbortErrLev ) return
         
      WRITE (unOutFile,'(/,A)')  'Predictions were generated on '//CurDate()//' at '//CurTime()//' using '//trim(GetNVD(version))
      WRITE (unOutFile,'(1X,A)') trim(GetNVD(AD_version))
      WRITE (unOutFile,'()' )    !print a blank line
      WRITE (unOutFile,'(A,11(1x,A,"=",ES11.4e2,1x,A))'   ) 'Case '//trim(num2lstr(iCase))//':' &
         ,'WndSpeed', CaseData%WndSpeed, 'm/s' &
         ,'ShearExp', CaseData%ShearExp, '' &
         ,'TSR',      CaseData%TSR, '' &
         ,'RotSpeed', CaseData%RotSpeed,'m/s' &
         ,'Pitch',    CaseData%Pitch*R2D, 'deg' &
         ,'Yaw',      CaseData%Yaw*R2D, 'deg' &
         ,'Tilt',     CaseData%Tilt*R2D, 'deg' &
         ,'Precone',  CaseData%Precone*R2D, 'deg' &
         ,'AzAng0',   CaseData%AzAng0*R2D, 'deg' &
         ,'dT',       CaseData%dT, 's' &
         ,'Tmax',     CaseData%Tmax,'s'
      
      WRITE (unOutFile,'()' )    !print a blank line
         
      

         !......................................................
         ! Write the names of the output parameters on one line:
         !......................................................
      frmt = '"'//OutFileData%delim//'"A15'      ! format for array elements 
      frmt2 = '(A,'//trim(num2lstr(numOuts))//'('//trim(frmt)//'))'
               
      write (unOutFile,frmt2,IOSTAT=errStat)  '     Time           ', OutFileData%WriteOutputHdr
      if ( errStat /= 0 ) then
         errMsg = 'Error '//trim(Num2LStr(errStat))//' occurred while writing to file in WTP_DvrInitializeOutputFile() using this format: '&
                  //trim(frmt2)
         errStat = ErrID_Fatal
         return
      end if      

         !......................................................
         ! Write the units of the output parameters on one line:
         !......................................................

      write (unOutFile,frmt2,IOSTAT=errStat)  '      (s)           ', OutFileData%WriteOutputUnt
      if ( errStat /= 0 ) then
         errMsg = 'Error '//trim(Num2LStr(errStat))//' occurred while writing to file in WTP_DvrInitializeOutputFile() using this format: '&
                  //trim(frmt2)
         errStat = ErrID_Fatal
         return
      end if
      
end subroutine WTP_InitializeOutputFile


end module WT_Perf_IO
   