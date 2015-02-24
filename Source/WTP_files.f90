!=======================================================================
MODULE Precision


   ! This module stores constants to specify the KIND of variables.


IMPLICIT                           NONE

INTEGER, PARAMETER              :: B1Ki     = SELECTED_INT_KIND(  2 )           ! Default kind for one-byte whole numbers.
INTEGER, PARAMETER              :: B2Ki     = SELECTED_INT_KIND(  4 )           ! Default kind for two-byte whole numbers.
INTEGER, PARAMETER              :: B4Ki     = SELECTED_INT_KIND(  9 )           ! Default kind for four-byte whole numbers.
INTEGER, PARAMETER              :: B8Ki     = SELECTED_INT_KIND( 18 )           ! Default kind for eight-byte whole numbers.
INTEGER, PARAMETER              :: DbKi     = SELECTED_REAL_KIND( 14, 300 )     ! Default kind for double-precision, floating-point numbers.
INTEGER, PARAMETER              :: QuKi     = SELECTED_REAL_KIND( 20, 500 )     ! Kind for 16-byte, floating-point numbers.
INTEGER, PARAMETER              :: ReKi     = SELECTED_REAL_KIND(  6,  30 )     ! Default kind for floating-point numbers.
INTEGER, PARAMETER              :: SiKi     = SELECTED_REAL_KIND(  6,  30 )     ! Kind for four-byte, floating-point numbers.


END MODULE Precision
MODULE SysSubs


   ! This module contains routines with system-specific logic and references.
   ! It also contains standard (but not system-specific) routines it uses.

   ! SysGnu.f90 is specifically for the Gnu Fortran for Linux compiler.

   ! 20110512, jm updated to use ACCESS='STREAM' for binary OPEN
   !           modified definition of char string UnfForm, below
   !           removed 3rd arg. from calls to GETARG, as
   !           recommended by H. Currin.  


   ! It contains the following routines:

   !     FUNCTION    COMMAND_ARGUMENT_COUNT()
   !     SUBROUTINE  FileSize ( FileName, Size )
   !     SUBROUTINE  FindLine ( Str , MaxLen , StrEnd )
   !     SUBROUTINE  FlushOut ( Unit )
   !     SUBROUTINE  Get_Arg ( Arg_Num , Arg , Error )                                      ! Please use GET_COMMAND_ARGUMENT() instead.
   !     SUBROUTINE  Get_Arg_Num ( Arg_Num )                                                ! Please use COMMAND_ARGUMENT_COUNT() instead.
   !     SUBROUTINE  GET_COMMAND ( Command, Length, Status )
   !     SUBROUTINE  GET_COMMAND_ARGUMENT ( Number, Value, Length, Status )
   !     SUBROUTINE  GET_CWD( DirName, Status )
   !     FUNCTION    Get_Env( EnvVar )                                                      ! Please use GET_ENVIRONMENT_VARIABLE() instead.
   !     FUNCTION    GET_ENVIRONMENT_VARIABLE( Name, Value, Length, Status, Trim_Name )
   !     FUNCTION    Is_NaN( DblNum )
   !     SUBROUTINE  OpenBinFile ( Un, OutFile, RecLen, Error )
   !     SUBROUTINE  OpenBinInpFile( Un, InFile, Error )
   !     SUBROUTINE  OpenUnfInpBEFile ( Un, InFile, RecLen, Error )
   !     SUBROUTINE  OpenCon
   !     SUBROUTINE  ProgExit ( StatCode )
   !     SUBROUTINE  UsrAlarm
   !     FUNCTION    UserTime()                                                             ! Removed: Replace by F95 intrinsic, CPU_TIME().
   !     SUBROUTINE  WrNR ( Str )
   !     SUBROUTINE  WrOver ( Str )
   !     SUBROUTINE  WrScr ( Str )




   USE                             Precision

   IMPLICIT                        NONE


!=======================================================================


   INTEGER                      :: ConRecL  = 120                               ! The record length for console output.
   INTEGER                      :: CU       = 7                                 ! The I/O unit for the console.
   INTEGER                      :: NL_Len   = 2                                 ! The number of characters used for a new line.

   CHARACTER(10)                :: Endian   = 'BIG_ENDIAN'                      ! The internal format of numbers.
   CHARACTER( 1)                :: PathSep  = '\'                               ! The path separater.
   CHARACTER( 1)                :: SwChar   = '/'                               ! The switch character for command-line options.
!20110512 jm changed from 'BINARY' to 'UNFORMATTED' because 'BINARY' is not
!standard and caused problems in OPEN statements in NWTC_iO.f90 that use
!this definition
   CHARACTER(11)                :: UnfForm  = 'UNFORMATTED'                     ! The string to specify unformatted I/O files.


CONTAINS

!=======================================================================
   FUNCTION COMMAND_ARGUMENT_COUNT()


      ! This routine returns the number of argumenta entered on the command line..

      ! Note: This routine will be available intrinsically in Fortran 2000.


      ! Function declaration.

   INTEGER                      :: COMMAND_ARGUMENT_COUNT                       ! This function.  The command line.



      ! Determine the mumber of arguments.  Load the program name into the result.

   COMMAND_ARGUMENT_COUNT = IArgC()


   RETURN
   END FUNCTION COMMAND_ARGUMENT_COUNT ! ()
!=======================================================================
   SUBROUTINE FileSize ( FileName, Size )


      ! This routine calls the routine Stat to obtain the file size
      ! corresponding to a file name or returns -1 on error.

      ! mlb: WARNING!!!
      ! The standard version of the routine uses the file unit instead of file name.
      ! We need fix the routines that call this one.


      ! Argument declarations:

   INTEGER, INTENT(OUT)         :: Size

   CHARACTER(*), INTENT(IN)     :: FileName


      ! Intrinsic declarations:

   INTEGER(KIND=1)              :: Stat


      ! Local declarations:

   INTEGER                      :: StatArray(12)
   INTEGER                      :: Status



   Status = Stat( FileName, StatArray )

   IF ( Status /= 0 ) THEN
     Size = -1
   ELSE
     Size = StatArray(8)
   END IF


   RETURN
   END SUBROUTINE FileSize ! ( FileName, Size )
!=======================================================================
   SUBROUTINE FindLine ( Str , MaxLen , StrEnd )


      ! This routine finds one line of text with a maximum length of MaxLen from the Str.
      ! It tries to break the line at a blank.

      ! This routine isn't system specific, but it is called by WrScr(), which is, so it must be here.


   IMPLICIT                        NONE


      ! Argument declarations:

   INTEGER, INTENT(IN)          :: MaxLen                                       ! The maximum length of the string.
   INTEGER, INTENT(OUT)         :: StrEnd                                       ! The location of the end of the string.

   CHARACTER(*), INTENT(IN)     :: Str                                          ! The string to search.


      ! Local declarations:

   INTEGER         IC



   StrEnd = MaxLen

   IF ( LEN_TRIM( Str ) > MaxLen )  THEN

      IC = INDEX( Str(1:MaxLen), ' ', BACK = .TRUE. ) ! Find the last space in the line

      IF ( IC > 1 ) THEN ! We don't want to return just one character that's a space, or do we?

         StrEnd = IC-1    ! StrEnd > 0
         DO WHILE ( Str(StrEnd:StrEnd) == ' ' )
            StrEnd = StrEnd - 1
            IF ( StrEnd <= 0 ) THEN  ! This occurs if everything before IC is a space
               StrEnd = IC
               EXIT
            ENDIF
         ENDDO

      ENDIF ! IC > 1

   ENDIF ! LEN_TRIM( Str ) > MaxLen


   RETURN
   END SUBROUTINE FindLine ! ( Str , MaxLen , StrEnd )
!=======================================================================
   SUBROUTINE FlushOut ( Unit )


      ! This subroutine flushes the buffer on the specified Unit.
      ! It is especially useful when printing "running..." type messages.


      ! Argument declarations:

   INTEGER, INTENT(IN)          :: Unit                                         ! The maximum length of the string.



  ! CALL FLUSH ( Unit )


   RETURN
   END SUBROUTINE FlushOut ! ( Unit )
!=======================================================================
   SUBROUTINE Get_Arg ( Arg_Num , Arg , Error )


      ! This routine gets Arg_Num'th argument from the command line.

   ! Note: The functionality in this routine was replaced by GET_COMMAND_ARGUMENT(), which will be available intrinsically in Fortran 2000.


      ! Argument declarations.

   INTEGER, INTENT(IN)          :: Arg_Num                                      ! The argument number to get.

   LOGICAL, INTENT(OUT)         :: Error                                        ! The Error flag returned to the calling program.

   CHARACTER(*), INTENT(OUT)    :: Arg                                          ! The argument string returned to the calling program.


      ! Local declarations.

   INTEGER                      :: Status                                       ! The status of the attempt to get an argument.



   CALL GETARG ( Arg_Num, Arg )  !20110512 jm remove 3rd arg for Linux port

   IF ( LEN_TRIM( Arg ) > 0 )  THEN
      Error = .FALSE.
   ELSE
      Error = .TRUE.
   END IF


   RETURN
   END SUBROUTINE Get_Arg ! ( Arg_Num , Arg , Error )
!=======================================================================
   SUBROUTINE Get_Arg_Num ( Arg_Num )


      ! This routine gets the number of command line arguments.

   ! Note: The functionality in this routine was replaced by COMMAND_ARGUMENT_COUNT(), which will be available intrinsically in Fortran 2000.


      ! Argument declarations.

   INTEGER, INTENT(OUT)         :: Arg_Num                                      ! The argument to get from the command line.



   Arg_Num = IARGC()


   RETURN
   END SUBROUTINE Get_Arg_Num ! ( Arg_Num )
!=======================================================================
   SUBROUTINE GET_COMMAND ( Command, Length, Status )


      ! This routine returns the string associated with the full command line.
      ! It tries as best it can to mimic the Fortran 2000 intrinsic subroutine by the same name.


      ! Argument declarations.

   INTEGER, OPTIONAL, INTENT(OUT)      :: Length                                ! The length of the value of the environment variable.
   INTEGER, OPTIONAL, INTENT(OUT)      :: Status                                ! The status indication what happened.

   CHARACTER(*), OPTIONAL, INTENT(OUT) :: Command                               ! The command line.


      ! Local parameter declarations.

   INTEGER, PARAMETER                  :: MaxLen = 500                          ! The maximum length permitted for an environment variable value.


      ! Local declarations.

   INTEGER                             :: CallStat                              ! Status of the call.
   INTEGER                             :: IArg                                  ! Argument index.

   CHARACTER(MaxLen)                   :: Arg                                   ! The current argument.
   CHARACTER(MaxLen)                   :: ReturnVal                             ! The value that will be returned.



      ! Initialize the result with the program name.

   CALL GETARG ( 0, ReturnVal )  !20110512 jm remove 3rd arg for Linux port


      ! Loop through all the arguments and build the total command line.

   DO IArg=1,IArgC()
      CALL GETARG ( IArg, Arg )  !20110512 jm remove 3rd arg for Linux port
      ReturnVal = TRIM( ReturnVal )//' '//TRIM( Arg )
   END DO ! IArg


      ! When asking the OS about the variable, trim the name unless Trim_Name is false.

   IF ( PRESENT( Command ) )  Command = ReturnVal
   IF ( PRESENT( Length  ) )  Length  = LEN_TRIM( ReturnVal )
   IF ( PRESENT( Status  ) )  Status  = 0


   RETURN
   END SUBROUTINE GET_COMMAND ! ( Command, Length, Status )
!=======================================================================
   SUBROUTINE GET_COMMAND_ARGUMENT ( Number, Value, Length, Status )


      ! This routine returns the string associated with the Numberth command-line argument.
      ! It tries as best it can to mimic the Fortran 2000 intrinsic function by the same name.


      ! Argument declarations.

   INTEGER, OPTIONAL, INTENT(OUT)      :: Length                                ! The length of the value of the environment variable.
   INTEGER, INTENT(IN)                 :: Number                                ! The number of the argument desired.
   INTEGER, OPTIONAL, INTENT(OUT)      :: Status                                ! The status indication what happened.

   CHARACTER(*), OPTIONAL, INTENT(OUT) :: Value                                 ! The command line argument.


      ! Local parameter declarations.

   INTEGER, PARAMETER                  :: MaxLen = 500                          ! The maximum length permitted for an environment variable value.


      ! Local declarations.

   INTEGER                             :: CallStat                              ! The status of the intrinsic call.

   CHARACTER(MaxLen)                   :: ReturnVal                             ! The value that will be returned.



      ! Get the argument.

   CALL GETARG ( Number, ReturnVal )  !20110512 jm remove 3rd arg for Linux port


      ! Load up the return values.

   IF ( PRESENT( Value  ) )  Value  = ReturnVal
   IF ( PRESENT( Length ) )  Length = LEN_TRIM( ReturnVal )
   IF ( PRESENT( Status ) )  Status = CallStat


   RETURN
   END SUBROUTINE GET_COMMAND_ARGUMENT ! ( Number, Value, Length, Status )
!=======================================================================
!bjj note: this subroutine is not tested for this compiler
   SUBROUTINE Get_CWD ( DirName, Status )


      ! This routine retrieves the path of the current working directory.


   IMPLICIT                        NONE


      ! Passed variables.

   CHARACTER(*), INTENT(OUT)    :: DirName                                         ! A CHARACTER string containing the path of the current working directory.
   INTEGER,      INTENT(OUT)    :: Status                                          ! Status returned by the call to a portability routine.


   Status = GETCWD ( DirName )

   RETURN
   END SUBROUTINE Get_CWD
!=======================================================================
   FUNCTION Get_Env( EnvVar )


      ! This routine returns the string associated with the EnvVar environment variable in the OS.
      ! It returns the null string of the variable is not found.

   ! Note: The functionality in this routine was replaced by GET_ENVIRONMENT_VARIABLE(), which will be available intrinsically in Fortran 2000.


      ! Function declaration.

   CHARACTER(500)               :: Get_Env                                      ! This function.  The value of the environment variable.


      ! Argument declarations.

   CHARACTER(*), INTENT(IN)     :: EnvVar                                       ! The environment variable to look up.



   CALL GetEnv ( EnvVar, Get_Env )


   RETURN
   END FUNCTION Get_Env ! ( EnvVar )
!=======================================================================
   FUNCTION GET_ENVIRONMENT_VARIABLE( Name, Value, Length, Status, Trim_Name )


      ! This routine returns the string associated with the Name environment variable in the OS.
      ! It tries as best it can to mimic the Fortran 2000 intrinsic function by the same name.


      ! Argument declarations.

   INTEGER, OPTIONAL, INTENT(OUT)      :: Length                                ! The length of the value of the environment variable.
   INTEGER, OPTIONAL, INTENT(OUT)      :: Status                                ! The status indication what happened.

   LOGICAL, OPTIONAL, INTENT(IN)       :: Trim_Name                             ! Treat trailing blanks in Name as significant if true.

   CHARACTER(*), INTENT(IN)            :: Name                                  ! The environment variable to look up.
   CHARACTER(*), OPTIONAL, INTENT(OUT) :: Value                                 ! The found value of the environment variable, Name.


      ! Local parameter declarations.

   INTEGER, PARAMETER                  :: MaxLen = 500                          ! The maximum length permitted for an environment variable value.


      ! Function declaration.

   CHARACTER(MaxLen)                   :: GET_ENVIRONMENT_VARIABLE              ! This function.  The value of the environment variable.


      ! Local declarations.

   CHARACTER(MaxLen)                   :: ReturnVal                             ! The value that will be returned.



      ! When asking the OS about the variable, trim the name unless Trim_Name is false.

   IF ( PRESENT( Trim_Name ) )  THEN
      IF ( Trim_Name )  THEN
         CALL GetEnv ( TRIM( Name ), ReturnVal )
      ELSE
         CALL GetEnv ( Name, ReturnVal )
      END IF
   ELSE
      CALL GetEnv ( TRIM( Name ), ReturnVal )
   END IF

   IF ( PRESENT( Value ) )  Value = ReturnVal

   IF ( PRESENT( Length ) )  Length = LEN_TRIM( ReturnVal )


      ! If requested, set the status of the OS request.

      ! Because the VF-specific GetEnv() is less capable than the Fortran 2000 intrinsic, we can't distinguish
      ! between a variable whose value is all blanks and one that is not set.

   IF ( PRESENT( Status ) )  THEN
      IF ( LEN_TRIM( ReturnVal ) == 0 )  THEN
         Status = 1
      ELSE
         Status = 0
      END IF
   END IF

   GET_ENVIRONMENT_VARIABLE = ReturnVal


   RETURN
   END FUNCTION GET_ENVIRONMENT_VARIABLE ! ( Name, Value, Length, Status, Trim_Name )
!=======================================================================
   FUNCTION Is_NaN( DblNum )


      ! This routine determines if a REAL(DbKi) variable holds a proper number.


      ! Argument declarations.

   REAL(DbKi), INTENT(IN)       :: DblNum


      ! Function declaration.

   LOGICAL                      :: Is_Nan



   Is_NaN = IsNaN( DblNum )


   RETURN
   END FUNCTION Is_NaN ! ( DblNum )
!=======================================================================
   SUBROUTINE OpenBinFile ( Un, OutFile, RecLen, Error )


      ! This routine opens a binary output file.


      ! Argument declarations.

   INTEGER, INTENT(IN)          :: Un                                           ! Logical unit for the output file.
   INTEGER, INTENT(IN)          :: RecLen                                       ! Length of binary record.

   LOGICAL, INTENT(OUT)         :: Error                                        ! Flag to indicate the open failed.

   CHARACTER(*), INTENT(IN)     :: OutFile                                      ! Name of the output file.


      ! Local declarations.

   INTEGER                      :: IOS                                          ! I/O status of OPEN.



      ! Open output file.  Make sure it worked.

!20110512 jm Change ACCESS from Sequential to F03std 'stream' to avoid reading/writing record block control words
   OPEN( Un, FILE=TRIM( OutFile ), STATUS='UNKNOWN', FORM='UNFORMATTED' , ACCESS='STREAM', IOSTAT=IOS )

   IF ( IOS /= 0 )  THEN
      Error = .TRUE.
   ELSE
      Error = .FALSE.
   END IF


   RETURN
   END SUBROUTINE OpenBinFile ! ( Un, OutFile, RecLen, Error )
!=======================================================================
   SUBROUTINE OpenBinInpFile ( Un, InFile, Error )


      ! This routine opens a binary input file.

   IMPLICIT                        NONE



      ! Argument declarations.

   INTEGER, INTENT(IN)          :: Un                                           ! Logical unit for the input file.

   CHARACTER(*), INTENT(IN)     :: InFile                                       ! Name of the input file.

   LOGICAL, INTENT(OUT)         :: Error                                        ! Flag to indicate the open failed.


      ! Local declarations.

   INTEGER                      :: IOS                                          ! I/O status of OPEN.

      ! NOTE: Do not explicitly declare the precision of this variable [as in
      !       LOGICAL(1)] so that the statements using this variable work with
      !       any compiler:


      ! Open input file.  Make sure it worked.

!20110512 jm Change ACCESS from Sequential to F03std 'stream' to avoid reading/writing record block control words
   OPEN( Un, FILE=TRIM( InFile ), STATUS='OLD', FORM='UNFORMATTED', ACCESS='STREAM', IOSTAT=IOS, ACTION='READ' )

   IF ( IOS /= 0 )  THEN
      Error = .TRUE.
   ELSE
      Error = .FALSE.
   END IF


   RETURN
   END SUBROUTINE OpenBinInpFile
!=======================================================================
   SUBROUTINE OpenUnfInpBEFile ( Un, InFile, RecLen, Error )


      ! This routine opens a binary input file with data stored in Big Endian format (created on a UNIX machine.)
      ! Data are stored in RecLen-byte records.

   IMPLICIT                        NONE



      ! Argument declarations.

   INTEGER, INTENT(IN)          :: Un                                           ! Logical unit for the input file.

   CHARACTER(*), INTENT(IN)     :: InFile                                       ! Name of the input file.

   INTEGER, INTENT(IN)          :: RecLen                                       ! Size of records in the input file, in bytes.

   LOGICAL, INTENT(OUT)         :: Error                                        ! Flag to indicate the open failed.


      ! Local declarations.

   INTEGER                      :: IOS                                          ! I/O status of OPEN.



      ! Open input file.  Make sure it worked.

   ! The non-standard CONVERT keyword allows us to read UNIX binary files, whose bytes are in reverse order (i.e., stored in BIG ENDIAN format).

   ! NOTE: using RecLen in bytes requires using the /assume:byterecl compiler option!

   OPEN ( Un, FILE=TRIM( InFile ), STATUS='OLD', FORM='UNFORMATTED', ACCESS='DIRECT', RECL=RecLen, IOSTAT=IOS, &
                   ACTION='READ'  )                                              ! Use this for UNIX systems.
!                  ACTION='READ', CONVERT='BIG_ENDIAN' )                         ! Use this for PC systems.


   IF ( IOS /= 0 )  THEN
      Error = .TRUE.
   ELSE
      Error = .FALSE.
   END IF


   RETURN
   END SUBROUTINE OpenUnfInpBEFile
!=======================================================================
   SUBROUTINE OpenCon


      ! This routine opens the console for standard output.



   OPEN ( CU , FILE='CON' , STATUS='UNKNOWN' , RECL=ConRecL )

   CALL FlushOut ( CU )


   RETURN
   END SUBROUTINE OpenCon
!=======================================================================
   SUBROUTINE ProgExit ( StatCode )


      ! This routine stops the program.  If the compiler supports the EXIT routine,
      ! pass the program status to it.  Otherwise, do a STOP.


      ! Argument declarations.

   INTEGER, INTENT(IN)          :: StatCode                                      ! The status code to pass to the OS.



   CALL EXIT ( StatCode )

!   IF ( StatCode == 0 ) THEN
!      STOP 0
!   ELSE
!      IF ( StatCode < 0 ) THEN
!         CALL WrScr( 'Invalid STOP code.' )
!      END IF
!
!      STOP 1
!   END IF


   RETURN
   END SUBROUTINE ProgExit ! ( StatCode )
!=======================================================================
   SUBROUTINE UsrAlarm


      ! This routine generates an alarm to warn the user that something went wrong.



   CALL WrOver ( CHAR( 7 ) )


   RETURN
   END SUBROUTINE UsrAlarm
!=======================================================================
!   FUNCTION UserTime()
!
!
!      ! This function returns the user CPU time.
!
!      ! The functionality of this routine was replaced by the F95 intrinsic, CPU_TIME().
!
!
!      ! Passed variables.
!
!   REAL(4)                      :: UserTime                                        ! User CPU time.
!
!
!      ! Local variables.
!
!   REAL(4)                      :: TimeAry (2)                                     ! TimeAry(1): User CPU time, TimeAry(2): System CPU time.
!   REAL(4)                      :: TotTime                                         ! User CPU time plus system CPU time.
!
!
!
!
!   TotTime  = DTIME( TimeAry )
!   UserTime = TimeAry(1)
!
!
!   RETURN
!   END FUNCTION UserTime
!=======================================================================
   SUBROUTINE WrNR ( Str )


      ! This routine writes out a string to the screen without following it with a new line.


      ! Argument declarations.

   CHARACTER(*), INTENT(IN)     :: Str                                          ! The string to write to the screen.



   WRITE (CU,'(1X,A)',ADVANCE='NO')  Str


   RETURN
   END SUBROUTINE WrNR ! ( Str )
!=======================================================================
   SUBROUTINE WrOver ( Str )


      ! This routine writes out a string that overwrites the previous line


      ! Argument declarations.

   CHARACTER(*), INTENT(IN)     :: Str                                          ! The string to write to the screen.



   WRITE (CU,'(''+'',A)')  Str


   RETURN
   END SUBROUTINE WrOver ! ( Str )
!=======================================================================
   SUBROUTINE WrScr ( Str )


      ! This routine writes out a string to the screen.


   IMPLICIT                        NONE


      ! Argument declarations.

   CHARACTER(*), INTENT(IN)     :: Str                                          ! The string to write to the screen.


      ! Local declarations.

   INTEGER                      :: Beg                                          ! The beginning of the next line of text.
   INTEGER                      :: Indent                                       ! The amunt to be indented.
   INTEGER                      :: LStr                                         ! The length of the remaining portion of the string.
   INTEGER                      :: MaxLen                                       ! Maximum number of columns to be written to the screen.

   CHARACTER(10)                :: Frm                                          ! Format specifier for the output.



      ! Find the amount of indent.  Create format.

   MaxLen = 98
   Indent = LEN_TRIM( Str ) - LEN_TRIM( ADJUSTL( Str ) )
   MaxLen = MaxLen - Indent
   Frm    = '(1X,  X,A)'

   IF ( Indent > 0 )  THEN
      Frm    = '(1X,  X,A)'
      WRITE (Frm(5:6),'(I2)')  Indent
   ELSE
      Frm    = '(1X,A)'
   END IF



   !  Break long messages into multiple lines.

   Beg  = Indent + 1
   LStr = LEN_TRIM( Str(Beg:) )

   DO WHILE ( Lstr > MaxLen )

      CALL FindLine ( Str(Beg:) , MaxLen , LStr )

      WRITE (CU,Frm)  TRIM( ADJUSTL( Str(Beg:Beg+LStr-1) ) )

      Beg = Beg + LStr


         ! If we have a space at the beginning of the string, let's get rid of it

      DO WHILE ( Beg < LEN_TRIM( Str ) .AND. Str(Beg:Beg) == ' ' )
         Beg = Beg + 1
      ENDDO

      LStr = LEN_TRIM( Str(Beg:) )

   ENDDO

   WRITE (CU,Frm)  TRIM( ADJUSTL( Str(Beg:Beg+LStr-1) ) )


   RETURN
   END SUBROUTINE WrScr ! ( Str )
!=======================================================================

END MODULE SysSubs
MODULE NWTC_IO


   ! This module contains I/O-related variables and routines with non-system-specific logic.


   ! It contains the following routines:

   !     SUBROUTINE CheckArgs    ( InputFile [, ErrStat] )
   !     SUBROUTINE CheckIOS     ( IOS, Fil, Variable, VarType [, TrapErrors] )
   !     SUBROUTINE CloseEcho    ( )
   !     SUBROUTINE Conv2UC      ( Str )
   !     FUNCTION   CountWords   ( Line )
   !     FUNCTION   CurDate      ( )
   !     FUNCTION   CurTime      ( )
   !     SUBROUTINE DispNVD      ( )
   !     FUNCTION   Flt2LStr     ( FltNum )
   !     SUBROUTINE GetPath      ( GivenFil, PathName )
   !     SUBROUTINE GetRoot      ( GivenFil, RootName )
   !     SUBROUTINE GetTokens    ( Line, NumTok, Tokens, Error )
   !     SUBROUTINE GetWords     ( Line, Words, NumWords )
   !     FUNCTION   Int2LStr     ( Intgr )
   !     SUBROUTINE NameOFile    ( InArg, OutExten, OutFile )
   !     SUBROUTINE NormStop     ( )
   !     SUBROUTINE OpenBin      ( Un, OutFile, RecLen [, ErrStat] )
   !     SUBROUTINE OpenBInpFile ( Un, InFile [, ErrStat] )
   !     SUBROUTINE OpenEcho     ( Un, InFile [, ErrStat] )
   !     SUBROUTINE OpenFInpFile ( Un, InFile [, ErrStat] )
   !     SUBROUTINE OpenFOutFile ( Un, OutFile [, ErrStat] )
   !     SUBROUTINE OpenFUnkFile ( Un, OutFile, FailAbt, Failed, Exists [, ErrStat] )
   !     SUBROUTINE OpenUInfile  ( Un, InFile [, ErrStat] )
   !     SUBROUTINE OpenUInBEFile( Un, InFile, RecLen [, ErrStat] )
   !     SUBROUTINE OpenUOutfile ( Un, OutFile [, ErrStat] )
   !     SUBROUTINE PremEOF      ( Fil , Variable [, TrapErrors] )
   !     SUBROUTINE ProgAbort    ( Message [, TrapErrors] )
   !     SUBROUTINE ProgWarn     ( Message )
   !     SUBROUTINE ReadAry      ( UnIn, Fil, Ary, AryLen, AryName, AryDescr [, ErrStat] )         ! Generic interface for ReadCAry, ReadIAry, ReadLAry, and ReadRAry.
   !     SUBROUTINE ReadAryLines ( UnIn, Fil, Ary, AryLen, AryName, AryDescr [, ErrStat] )         ! Generic interface for ReadCAryLines, and ReadRAryLines.
   !     SUBROUTINE ReadCAry     ( UnIn, Fil, CharAry, AryLen, AryName, AryDescr [, ErrStat] )
   !     SUBROUTINE ReadCAryLines( UnIn, Fil, CharAry, AryLen, AryName, AryDescr [, ErrStat] )
   !     SUBROUTINE ReadCom      ( UnIn, Fil, ComName [, ErrStat] )
   !     SUBROUTINE ReadCVar     ( UnIn, Fil, CharVar, VarName, VarDescr [, ErrStat] )
   !     SUBROUTINE ReadIAry     ( UnIn, Fil, IntAry, AryLen, AryName, AryDescr [, ErrStat] )
   !     SUBROUTINE ReadIVar     ( UnIn, Fil, IntVar, VarName, VarDescr [, ErrStat] )
   !     SUBROUTINE ReadLAry     ( UnIn, Fil, LogAry, AryLen, AryName, AryDescr [, ErrStat] )
   !     SUBROUTINE ReadLVar     ( UnIn, Fil, LogVar, VarName, VarDescr [, ErrStat] )
   !     SUBROUTINE ReadRAry     ( UnIn, Fil, RealAry, AryLen, AryName, AryDescr [, ErrStat] )
   !     SUBROUTINE ReadRAryLines( UnIn, Fil, RealAry, AryLen, AryName, AryDescr [, ErrStat] )
   !     SUBROUTINE ReadRVar     ( UnIn, Fil, RealVar, VarName, VarDescr [, ErrStat] )
   !     SUBROUTINE ReadStr      ( UnIn, Fil, CharVar, VarName, VarDescr [, ErrStat] )
   !     SUBROUTINE ReadVar      ( UnIn, Fil, Var, VarName, VarDescr [, ErrStat] )                 ! Generic interface for ReadCVar, ReadIVar, ReadLVar, and ReadRVar.
   !     SUBROUTINE WaitTime     ( WaitSecs )
   !     SUBROUTINE WrPr         ( Str )
   !     SUBROUTINE WrFileNR     ( Unit, Str )
   !     SUBROUTINE WrML         ( Str )
   !     SUBROUTINE WrScr1       ( Str )


   USE                             SysSubs


!=======================================================================


      ! Global I/O-related variables.

   INTEGER, PARAMETER           :: FlgType  = 1                                 ! Switch for telling if a variable is a flag.
   INTEGER, PARAMETER           :: NumType  = 2                                 ! Switch for telling if a variable is a number.
   INTEGER, PARAMETER           :: StrType  = 3                                 ! Switch for telling if a variable is a string.
   INTEGER                      :: UnEc     = 19                                ! I/O unit number for the echo file.

   LOGICAL                      :: Beep     = .TRUE.                            ! Flag that specifies whether or not to beep for error messages and program terminations.
   LOGICAL                      :: Echo     = .FALSE.                           ! Flag that specifies whether or not to produce an echo file.

   CHARACTER(23)                :: NWTCName = 'NWTC Subroutine Library'         ! The name of the NWTC subroutine library.
   CHARACTER(29)                :: NWTCVer  = ' (v1.03.01, 13-Jan-2011)'        ! The version (including date) of the NWTC Subroutine Library.
   CHARACTER(20)                :: ProgName = ' '                               ! The name of the calling program.
   CHARACTER(99)                :: ProgVer                                      ! The version (including date) of the calling program.
   CHARACTER(1), PARAMETER      :: Tab      = CHAR( 9 )                         ! The tab character.


!=======================================================================


      ! Create interface for a generic AllocAry that actually uses specific routines.

   INTERFACE AllocAry
      MODULE PROCEDURE AllCAry1
      MODULE PROCEDURE AllCAry2
      MODULE PROCEDURE AllCAry3
   !   MODULE PROCEDURE AllCAry4                               Not yet coded.
      MODULE PROCEDURE AllIAry1
      MODULE PROCEDURE AllIAry2
      MODULE PROCEDURE AllIAry3
   !   MODULE PROCEDURE AllIAry4                               Not yet coded.
      MODULE PROCEDURE AllLAry1
      MODULE PROCEDURE AllLAry2
      MODULE PROCEDURE AllLAry3
   !   MODULE PROCEDURE AllLAry4                               Not yet coded.
      MODULE PROCEDURE AllRAry1
      MODULE PROCEDURE AllRAry2
      MODULE PROCEDURE AllRAry3
      MODULE PROCEDURE AllRAry4
   END INTERFACE


      ! Create interface for a generic ReadVar that actually uses specific routines.

   INTERFACE ReadVar
      MODULE PROCEDURE ReadCVar
      MODULE PROCEDURE ReadIVar
      MODULE PROCEDURE ReadLVar
      MODULE PROCEDURE ReadRVar
   END INTERFACE


      ! Create interface for a generic ReadAry that actually uses specific routines.

   INTERFACE ReadAry
      MODULE PROCEDURE ReadCAry
      MODULE PROCEDURE ReadIAry
      MODULE PROCEDURE ReadLAry
      MODULE PROCEDURE ReadRAry
   END INTERFACE

   INTERFACE ReadAryLines
      MODULE PROCEDURE ReadCAryLines
!     MODULE PROCEDURE ReadIAryLines         ! Not coded yet
!     MODULE PROCEDURE ReadLAryLines         ! Not coded yet
      MODULE PROCEDURE ReadRAryLines
   END INTERFACE


CONTAINS

!=======================================================================
   SUBROUTINE AllCAry1 ( Ary, AryDim, Descr, ErrStat )

      ! This routine allocates a 1-D CHARACTER array.


      ! Argument declarations.

   INTEGER, INTENT(IN)          :: AryDim                                      ! The size of the array.
   INTEGER, INTENT(OUT),OPTIONAL:: ErrStat                                     ! Error status; if present, program does not abort on error


   CHARACTER(*), ALLOCATABLE    :: Ary    (:)                                 ! Array to be allocated
   CHARACTER(*), INTENT(IN)     :: Descr                                       ! Brief array description.


      ! Local declarations.

   INTEGER                      :: Sttus                                       ! Status of allocation attempt.



   ALLOCATE ( Ary(AryDim) , STAT=Sttus )


   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the '//TRIM( Descr )//' array.', PRESENT(ErrStat) )
   END IF

   IF ( PRESENT(ErrStat) ) ErrStat = Sttus

   RETURN
   END SUBROUTINE AllCAry1 ! ( Ary, AryDim, Descr )
!=======================================================================
   SUBROUTINE AllCAry2 (  Ary, AryDim1, AryDim2, Descr, ErrStat )

      ! This routine allocates a 2-D CHARACTER array.


      ! Argument declarations.

   INTEGER, INTENT(IN)          :: AryDim1                                     ! The size of the first dimension of the array.
   INTEGER, INTENT(IN)          :: AryDim2                                     ! The size of the second dimension of the array.
   INTEGER, INTENT(OUT),OPTIONAL:: ErrStat                                     ! Error status; if present, program does not abort on error

   CHARACTER(*), ALLOCATABLE    :: Ary    (:,:)                                ! Array to be allocated
   CHARACTER(*), INTENT(IN)     :: Descr                                       ! Brief array description.


      ! Local declarations.

   INTEGER                      :: Sttus                                       ! Status of allocation attempt.



   ALLOCATE ( Ary(AryDim1,AryDim2) , STAT=Sttus )

   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the '//TRIM( Descr )//' array.', PRESENT(ErrStat) )
   END IF

   IF ( PRESENT(ErrStat) ) ErrStat = Sttus


   RETURN
   END SUBROUTINE AllCAry2 ! (  Ary, AryDim1, AryDim2, Descr )
!=======================================================================
   SUBROUTINE AllCAry3 (  Ary, AryDim1, AryDim2, AryDim3, Descr, ErrStat )


      ! This routine allocates a 3-D CHARACTER array.


      ! Argument declarations.

   INTEGER, INTENT(IN)          :: AryDim1                                     ! The size of the first dimension of the array.
   INTEGER, INTENT(IN)          :: AryDim2                                     ! The size of the second dimension of the array.
   INTEGER, INTENT(IN)          :: AryDim3                                     ! The size of the third dimension of the array.
   INTEGER, INTENT(OUT),OPTIONAL:: ErrStat                                     ! Error status; if present, program does not abort on error


   CHARACTER(*), ALLOCATABLE    :: Ary    (:,:,:)                              ! Array to be allocated
   CHARACTER(*), INTENT(IN)     :: Descr                                       ! Brief array description.


      ! Local declarations.

   INTEGER                      :: Sttus                                       ! Status of allocation attempt.



   ALLOCATE ( Ary(AryDim1,AryDim2,AryDim3) , STAT=Sttus )

   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the '//TRIM( Descr )//' array.', PRESENT(ErrStat) )
   END IF

   IF ( PRESENT(ErrStat) ) ErrStat = Sttus


   RETURN
   END SUBROUTINE AllCAry3 ! (  Ary, AryDim1, AryDim2, AryDim3, Descr )
!=======================================================================
   SUBROUTINE AllIAry1 ( Ary, AryDim, Descr, ErrStat )


      ! This routine allocates a 1-D INTEGER array.


      ! Argument declarations.

   INTEGER, ALLOCATABLE         :: Ary    (:)                                  ! Array to be allocated
   INTEGER, INTENT(IN)          :: AryDim                                      ! The size of the array.
   INTEGER, INTENT(OUT),OPTIONAL:: ErrStat                                     ! Error status; if present, program does not abort on error

   CHARACTER(*), INTENT(IN)     :: Descr                                       ! Brief array description.


      ! Local declarations.

   INTEGER                      :: Sttus                                       ! Status of allocation attempt.



   ALLOCATE ( Ary(AryDim) , STAT=Sttus )

   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the '//TRIM( Descr )//' array.', PRESENT(ErrStat) )
   END IF

   IF ( PRESENT(ErrStat) ) ErrStat = Sttus

   RETURN
   END SUBROUTINE AllIAry1 ! ( Ary, AryDim, Descr )
!=======================================================================
   SUBROUTINE AllIAry2 (  Ary, AryDim1, AryDim2, Descr, ErrStat )


      ! This routine allocates a 2-D INTEGER array.


      ! Argument declarations.

   INTEGER, ALLOCATABLE         :: Ary    (:,:)                                ! Array to be allocated
   INTEGER, INTENT(IN)          :: AryDim1                                     ! The size of the first dimension of the array.
   INTEGER, INTENT(IN)          :: AryDim2                                     ! The size of the second dimension of the array.
   INTEGER, INTENT(OUT),OPTIONAL:: ErrStat                                     ! Error status; if present, program does not abort on error

   CHARACTER(*), INTENT(IN)     :: Descr                                       ! Brief array description.


      ! Local declarations.

   INTEGER                      :: Sttus                                       ! Status of allocation attempt.



   ALLOCATE ( Ary(AryDim1,AryDim2) , STAT=Sttus )

   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the '//TRIM( Descr )//' array.', PRESENT(ErrStat) )
   END IF

   IF ( PRESENT(ErrStat) ) ErrStat = Sttus

   RETURN
   END SUBROUTINE AllIAry2 ! (  Ary, AryDim1, AryDim2, Descr )
!=======================================================================
   SUBROUTINE AllIAry3 (  Ary, AryDim1, AryDim2, AryDim3, Descr, ErrStat )


      ! This routine allocates a 3-D INTEGER array.


      ! Argument declarations.

   INTEGER, ALLOCATABLE         :: Ary    (:,:,:)                              ! Array to be allocated
   INTEGER, INTENT(IN)          :: AryDim1                                     ! The size of the first dimension of the array.
   INTEGER, INTENT(IN)          :: AryDim2                                     ! The size of the second dimension of the array.
   INTEGER, INTENT(IN)          :: AryDim3                                     ! The size of the third dimension of the array.
   INTEGER, INTENT(OUT),OPTIONAL:: ErrStat                                     ! Error status; if present, program does not abort on error

   CHARACTER(*), INTENT(IN)     :: Descr                                       ! Brief array description.


      ! Local declarations.

   INTEGER                      :: Sttus                                       ! Status of allocation attempt.



   ALLOCATE ( Ary(AryDim1,AryDim2,AryDim3) , STAT=Sttus )

   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the '//TRIM( Descr )//' array.', PRESENT(ErrStat) )
   END IF

   IF ( PRESENT(ErrStat) ) ErrStat = Sttus


   RETURN
   END SUBROUTINE AllIAry3 ! (  Ary, AryDim1, AryDim2, AryDim3, Descr )
!=======================================================================
   SUBROUTINE AllLAry1 ( Ary, AryDim, Descr, ErrStat )


      ! This routine allocates a 1-D LOGICAL array.


      ! Argument declarations.

   INTEGER, INTENT(IN)          :: AryDim                                      ! The size of the array.
   INTEGER, INTENT(OUT),OPTIONAL:: ErrStat                                     ! Error status; if present, program does not abort on error

   LOGICAL, ALLOCATABLE         :: Ary    (:)                                  ! Array to be allocated

   CHARACTER(*), INTENT(IN)     :: Descr                                       ! Brief array description.


      ! Local declarations.

   INTEGER                      :: Sttus                                       ! Status of allocation attempt.



   ALLOCATE ( Ary(AryDim) , STAT=Sttus )

   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the '//TRIM( Descr )//' array.', PRESENT(ErrStat) )
   END IF

   IF ( PRESENT(ErrStat) ) ErrStat = Sttus


   RETURN
   END SUBROUTINE AllLAry1 ! ( Ary, AryDim, Descr )
!=======================================================================
   SUBROUTINE AllLAry2 (  Ary, AryDim1, AryDim2, Descr, ErrStat )


      ! This routine allocates a 2-D LOGICAL array.


      ! Argument declarations.

   INTEGER, INTENT(IN)          :: AryDim1                                     ! The size of the first dimension of the array.
   INTEGER, INTENT(IN)          :: AryDim2                                     ! The size of the second dimension of the array.
   INTEGER, INTENT(OUT),OPTIONAL:: ErrStat                                     ! Error status; if present, program does not abort on error

   LOGICAL, ALLOCATABLE         :: Ary    (:,:)                                ! Array to be allocated

   CHARACTER(*), INTENT(IN)     :: Descr                                       ! Brief array description.


      ! Local declarations.

   INTEGER                      :: Sttus                                       ! Status of allocation attempt.



   ALLOCATE ( Ary(AryDim1,AryDim2) , STAT=Sttus )

   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the '//TRIM( Descr )//' array.', PRESENT(ErrStat) )
   END IF

   IF ( PRESENT(ErrStat) ) ErrStat = Sttus


   RETURN
   END SUBROUTINE AllLAry2 ! (  Ary, AryDim1, AryDim2, Descr )
!=======================================================================
   SUBROUTINE AllLAry3 (  Ary, AryDim1, AryDim2, AryDim3, Descr, ErrStat )


      ! This routine allocates a 3-D LOGICAL array.


      ! Argument declarations.

   INTEGER, INTENT(IN)          :: AryDim1                                     ! The size of the first dimension of the array.
   INTEGER, INTENT(IN)          :: AryDim2                                     ! The size of the second dimension of the array.
   INTEGER, INTENT(IN)          :: AryDim3                                     ! The size of the third dimension of the array.
   INTEGER, INTENT(OUT),OPTIONAL:: ErrStat                                     ! Error status; if present, program does not abort on error

   LOGICAL, ALLOCATABLE         :: Ary    (:,:,:)                              ! Array to be allocated

   CHARACTER(*), INTENT(IN)     :: Descr                                       ! Brief array description.


      ! Local declarations.

   INTEGER                      :: Sttus                                       ! Status of allocation attempt.



   ALLOCATE ( Ary(AryDim1,AryDim2,AryDim3) , STAT=Sttus )

   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the '//TRIM( Descr )//' array.', PRESENT(ErrStat) )
   END IF

   IF ( PRESENT(ErrStat) ) ErrStat = Sttus


   RETURN
   END SUBROUTINE AllLAry3 ! (  Ary, AryDim1, AryDim2, AryDim3, Descr )
!=======================================================================
   SUBROUTINE AllRAry1 ( Ary, AryDim, Descr, ErrStat )


      ! This routine allocates a 1-D REAL array.


      ! Argument declarations.

   REAL(ReKi), ALLOCATABLE      :: Ary    (:)                                  ! Array to be allocated

   INTEGER, INTENT(IN)          :: AryDim                                      ! The size of the array.
   INTEGER, INTENT(OUT),OPTIONAL:: ErrStat                                     ! Error status; if present, program does not abort on error

   CHARACTER(*), INTENT(IN)     :: Descr                                       ! Brief array description.


      ! Local declarations.

   INTEGER                      :: Sttus                                       ! Status of allocation attempt.



   ALLOCATE ( Ary(AryDim) , STAT=Sttus )

   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the '//TRIM( Descr )//' array.', PRESENT(ErrStat) )
   END IF

   IF ( PRESENT(ErrStat) ) ErrStat = Sttus


   RETURN
   END SUBROUTINE AllRAry1 ! ( Ary, AryDim, Descr )
!=======================================================================
   SUBROUTINE AllRAry2 (  Ary, AryDim1, AryDim2, Descr, ErrStat )


      ! This routine allocates a 2-D REAL array.


      ! Argument declarations.

   REAL(ReKi), ALLOCATABLE      :: Ary    (:,:)                                ! Array to be allocated

   INTEGER, INTENT(IN)          :: AryDim1                                     ! The size of the first dimension of the array.
   INTEGER, INTENT(IN)          :: AryDim2                                     ! The size of the second dimension of the array.
   INTEGER, INTENT(OUT),OPTIONAL:: ErrStat                                     ! Error status; if present, program does not abort on error

   CHARACTER(*), INTENT(IN)     :: Descr                                       ! Brief array description.


      ! Local declarations.

   INTEGER                      :: Sttus                                       ! Status of allocation attempt.



   ALLOCATE ( Ary(AryDim1,AryDim2) , STAT=Sttus )

   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the '//TRIM( Descr )//' array.', PRESENT(ErrStat) )
   END IF

   IF ( PRESENT(ErrStat) ) ErrStat = Sttus


   RETURN
   END SUBROUTINE AllRAry2 ! (  Ary, AryDim1, AryDim2, Descr )
!=======================================================================
   SUBROUTINE AllRAry3 (  Ary, AryDim1, AryDim2, AryDim3, Descr, ErrStat )


      ! This routine allocates a 3-D REAL array.


      ! Argument declarations.

   REAL(ReKi), ALLOCATABLE      :: Ary    (:,:,:)                              ! Array to be allocated

   INTEGER, INTENT(IN)          :: AryDim1                                     ! The size of the first dimension of the array.
   INTEGER, INTENT(IN)          :: AryDim2                                     ! The size of the second dimension of the array.
   INTEGER, INTENT(IN)          :: AryDim3                                     ! The size of the third dimension of the array.
   INTEGER, INTENT(OUT),OPTIONAL:: ErrStat                                     ! Error status; if present, program does not abort on error

   CHARACTER(*), INTENT(IN)     :: Descr                                       ! Brief array description.


      ! Local declarations.

   INTEGER                      :: Sttus                                       ! Status of allocation attempt.



   ALLOCATE ( Ary(AryDim1,AryDim2,AryDim3) , STAT=Sttus )

   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the '//TRIM( Descr )//' array.', PRESENT(ErrStat) )
   END IF

   IF ( PRESENT(ErrStat) ) ErrStat = Sttus


   RETURN
  END SUBROUTINE AllRAry3 ! (  Ary, AryDim1, AryDim2, AryDim3, Descr )
!=======================================================================
   SUBROUTINE AllRAry4 (  Ary, AryDim1, AryDim2, AryDim3, AryDim4, Descr, ErrStat )


      ! This routine allocates a 4-D REAL array.


      ! Argument declarations.

   REAL(ReKi), ALLOCATABLE      :: Ary    (:,:,:,:)                            ! Array to be allocated

   INTEGER, INTENT(IN)          :: AryDim1                                     ! The size of the first dimension of the array.
   INTEGER, INTENT(IN)          :: AryDim2                                     ! The size of the second dimension of the array.
   INTEGER, INTENT(IN)          :: AryDim3                                     ! The size of the third dimension of the array.
   INTEGER, INTENT(IN)          :: AryDim4                                     ! The size of the fourth dimension of the array.
   INTEGER, INTENT(OUT),OPTIONAL:: ErrStat                                     ! Error status; if present, program does not abort on error

   CHARACTER(*), INTENT(IN)     :: Descr                                       ! Brief array description.


      ! Local declarations.

   INTEGER                      :: Sttus                                       ! Status of allocation attempt.



   ALLOCATE ( Ary(AryDim1,AryDim2,AryDim3,AryDim4) , STAT=Sttus )

   IF ( Sttus /= 0 )  THEN
      CALL ProgAbort ( ' Error allocating memory for the '//TRIM( Descr )//' array.', PRESENT(ErrStat) )
   END IF

   IF ( PRESENT(ErrStat) ) ErrStat = Sttus


   RETURN
  END SUBROUTINE AllRAry4 ! (  Ary, AryDim1, AryDim2, AryDim3, AryDim4, Descr )
!=======================================================================
!bjj: shouldn't this come after the next subroutine, alphabetically?
   SUBROUTINE CheckIOS ( IOS, Fil, Variable, VarType, TrapErrors )


      ! This routine checks the I/O status and prints either an end-of-file or
      ! an invalid-input message, and then aborts the program.


      ! Argument declarations.

   INTEGER, INTENT(IN)          :: IOS                                          ! I/O status.
   INTEGER, INTENT(IN)          :: VarType                                      ! Type of variable.

   LOGICAL, INTENT(IN), OPTIONAL:: TrapErrors                                   ! Determines if the program should abort or return to calling function
   LOGICAL                      :: TrapThisError                                ! The local version of TrapErrors

   CHARACTER(*), INTENT(IN)     :: Fil                                          ! Name of input file.
   CHARACTER(*), INTENT(IN)     :: Variable                                     ! Variable name.


   IF ( PRESENT( TrapErrors ) ) THEN
      TrapThisError = TrapErrors
   ELSE
      TrapThisError = .FALSE.
   END IF


   IF ( IOS < 0 )  THEN

      CALL PremEOF ( TRIM( Fil ), Variable, TrapThisError )

   ELSE IF ( IOS > 0 )  THEN

      SELECTCASE ( VarType )

      CASE ( NumType )
         CALL WrScr1 ( ' Invalid numerical input for file "'//TRIM( Fil )//'".' )
      CASE ( FlgType )
         CALL WrScr1 ( ' Invalid logical input for file "'//TRIM( Fil )//'".' )
      CASE ( StrType )
         CALL WrScr1 ( ' Invalid character input for file "'//TRIM( Fil )//'".' )
      ENDSELECT

      CALL ProgAbort  ( ' The error occurred while trying to read '//TRIM( Variable )//'.', TrapThisError )

   END IF


   RETURN
   END SUBROUTINE CheckIOS ! ( IOS, Fil, Variable, VarType )
!=======================================================================
   SUBROUTINE CheckArgs ( InputFile, ErrStat )


      ! This subroutine is used to check for command-line arguments.


      ! Argument declarations:
   INTEGER, INTENT(OUT),OPTIONAL:: ErrStat                                      ! Error status; if present, program does not abort on error
   CHARACTER(*), INTENT(OUT)    :: InputFile                                    ! The name of the input file specified on the command line.


      ! Local declarations:

   INTEGER                      :: IArg                                         ! The argument number.
   INTEGER                      :: NumArg                                       ! The number of arguments on the command line.

   LOGICAL                      :: Error                                        ! Flag indicating if there was an error getting an argument.

   CHARACTER(LEN(InputFile))    :: Arg                                          ! A command-line argument.




      ! Find out how many arguments were entered on the command line.

   CALL Get_Arg_Num ( NumArg )


      ! Parse them.

   IF ( NumArg .GT. 0 )  THEN

      DO IArg=1,NumArg

         CALL Get_Arg ( IArg , Arg , Error )

         IF ( Error )  THEN
            CALL ProgAbort ( ' Error getting command-line argument #'//TRIM( Int2LStr( IArg ) )//'.', PRESENT(ErrStat) )
            IF ( PRESENT(ErrStat) ) THEN
               ErrStat = 1
               RETURN
            END IF
         END IF

         IF ( Arg(1:1) == SwChar )  THEN

            CALL WrScr1   ( ' Syntax is:' )
            CALL WrScr1   ( '    '//TRIM( ProgName )//' ['//SwChar//'h] [<InputFile>]' )
            CALL WrScr1   ( ' where:' )
            CALL WrScr1   ( '    '//SwChar//'h generates this help message.' )
            CALL WrScr    ( '    <InputFile> is the name of the primary input file ['//TRIM( InputFile )//'].' )
            CALL WrScr    ( ' ')

            IF ( INDEX( 'Hh?', Arg(2:2)  ) > 0 )  THEN
               IF ( PRESENT(ErrStat) ) THEN
                  ErrStat = -1
                  RETURN
               ELSE
                  CALL ProgExit ( 1 )
               END IF
            ELSE
               CALL ProgAbort ( ' Invalid command-line switch "'//SwChar//TRIM( Arg(2:) )//'".', PRESENT(ErrStat) )
               IF ( PRESENT(ErrStat) ) THEN
                  ErrStat = 1
                  RETURN
               END IF
            END IF

         ELSE
            InputFile = Arg
         END IF

      END DO

   END IF

   IF ( PRESENT( ErrStat ) ) ErrStat = 0

   RETURN
   END SUBROUTINE CheckArgs
!=======================================================================
   SUBROUTINE CloseEcho( )

      ! This subroutine closes the echo file and sets Echo to false

      CLOSE ( UnEc )

      Echo  = .FALSE.

   END SUBROUTINE CloseEcho
!=======================================================================
   SUBROUTINE Conv2UC ( Str )


      ! This routine converts all the text in a string to upper case.


      ! Argument declarations.

   CHARACTER(*), INTENT(INOUT)  :: Str                                          ! The string to be converted to UC.


      ! Local declarations.

   INTEGER                      :: IC                                           ! Character index



   DO IC=1,LEN_TRIM( Str )

      IF ( ( Str(IC:IC) >= 'a' ).AND.( Str(IC:IC) <= 'z' ) )  THEN
         Str(IC:IC) = CHAR( ICHAR( Str(IC:IC) ) - 32 )
      ELSE
         Str(IC:IC) = Str(IC:IC)
      END IF

   END DO ! IC


   RETURN
   END SUBROUTINE Conv2UC !  ( Str )
!=======================================================================
   FUNCTION CountWords ( Line )


      ! This subroutine is used to count the number of "words" in a line of text.
      ! It uses spaces, tabs, commas, semicolons, single quotes, and double quotes ("whitespace")
      !  as word separators.


      ! Function declaration.

   INTEGER                      :: CountWords                                   ! This function.


      ! Argument declarations.

   CHARACTER(*), INTENT(IN)     :: Line                                         ! Count the words in this text string.


      ! Local declarations.

   INTEGER                      :: Ch                                           ! Character position.
   INTEGER                      :: NextWhite                                    ! Position of the next white space.



      ! Let's initialize the number of columns and the character pointer.

   CountWords = 0


      ! Let's make sure we have text on this line.

   IF ( LEN_TRIM( Line ) == 0 )  RETURN


      ! Count words separated by any combination of spaces, tabs, commas,
      ! semicolons, single quotes, and double quotes ("whitespace").

   Ch = 0

   DO

      NextWhite = SCAN( Line(Ch+1:) , ' ,;''"'//Tab )
      Ch        = Ch + NextWhite

      IF ( NextWhite > 1 )  THEN
         CountWords = CountWords + 1
      ELSE IF ( NextWhite == 1 )  THEN
         CYCLE
      ELSE
         EXIT
      END IF

   END DO


   RETURN
   END FUNCTION CountWords ! ( Line )
!=======================================================================
   FUNCTION CurDate( )


      ! This function returns a character string encoded with the date in the form dd-mmm-ccyy.


      ! Function declaration.

   CHARACTER(11)                :: CurDate                                      ! This function


      ! Local declarations.

   CHARACTER(8)                 :: CDate                                        ! String to hold the returned value from the DATE_AND_TIME subroutine call.



   !  Call the system date function.

   CALL DATE_AND_TIME ( CDate )


   !  Parse out the day.

   CurDate(1:3) = CDate(7:8)//'-'


   !  Parse out the month.

   SELECT CASE ( CDate(5:6) )
      CASE ( '01' )
         CurDate(4:6) = 'Jan'
      CASE ( '02' )
         CurDate(4:6) = 'Feb'
      CASE ( '03' )
         CurDate(4:6) = 'Mar'
      CASE ( '04' )
         CurDate(4:6) = 'Apr'
      CASE ( '05' )
         CurDate(4:6) = 'May'
      CASE ( '06' )
         CurDate(4:6) = 'Jun'
      CASE ( '07' )
         CurDate(4:6) = 'Jul'
      CASE ( '08' )
         CurDate(4:6) = 'Aug'
      CASE ( '09' )
         CurDate(4:6) = 'Sep'
      CASE ( '10' )
         CurDate(4:6) = 'Oct'
      CASE ( '11' )
         CurDate(4:6) = 'Nov'
      CASE ( '12' )
         CurDate(4:6) = 'Dec'
   END SELECT


   !  Parse out the year.

   CurDate(7:11) = '-'//CDate(1:4)


   RETURN
   END FUNCTION CurDate ! ()
!=======================================================================
   FUNCTION CurTime( )


      ! This function returns a character string encoded with the time in the form "hh:mm:ss".


      ! Function declaration.

   CHARACTER(8)                 :: CurTime                                      ! This function.


      ! Local declarations.

   CHARACTER(10)                :: CTime                                        ! String to hold the returned value from the DATE_AND_TIME subroutine call.



   CALL DATE_AND_TIME ( TIME=CTime )

   CurTime = CTime(1:2)//':'//CTime(3:4)//':'//CTime(5:6)


   RETURN
   END FUNCTION CurTime ! ()
!=======================================================================
   SUBROUTINE DispNVD


      ! This routine displays the name of the program, it's version, and it's release date.


      ! Print out program name, version, and date.

   CALL WrScr1 ( ' Running '//TRIM( ProgName )//' '//Trim( ProgVer )//'.' )
   CALL WrScr  ( ' Linked with '//TRIM( NWTCName )//TRIM( NWTCVer )//'.' )


   RETURN
   END SUBROUTINE DispNVD
!=======================================================================
   FUNCTION Flt2LStr ( FltNum )


      ! This function converts a floating point number to a left-aligned
      ! string.  It eliminates trailing zeroes and even the decimal
      ! point if it is not a fraction.


      ! Function declaration.

   CHARACTER(15)                :: Flt2LStr                                        ! This function.


      ! Argument declarations.

   REAL(ReKi), INTENT(IN)       :: FltNum                                          ! The floating-point number to convert.


      ! Local declarations.

   INTEGER                      :: IC                                              ! Character index.



      ! Return a 0 if that's what we have.

   IF ( FltNum == 0.0 )  THEN
      Flt2LStr = '0'
      RETURN
   END IF


      ! Write the number into the string using G format and left justify it.

   WRITE (Flt2LStr,'(1PG15.5)')  FltNum

   Flt2LStr = ADJUSTL( Flt2LStr )


      ! Replace trailing zeros and possibly the decimal point with blanks.
      ! Stop trimming once we find the decimal point or a nonzero.

   IF (INDEX( Flt2Lstr, "E" ) > 0 ) RETURN
   IF (INDEX( Flt2Lstr, "e" ) > 0 ) RETURN


   DO IC=LEN_TRIM( Flt2LStr ),1,-1

      IF ( Flt2LStr(IC:IC) == '.' )  THEN
         Flt2LStr(IC:IC) = ' '
         RETURN
      ELSE IF ( Flt2LStr(IC:IC) /= '0' )  THEN
         RETURN
      END IF

      Flt2LStr(IC:IC) = ' '

   END DO ! IC


   RETURN
   END FUNCTION Flt2LStr !  ( FltNum )
!=======================================================================
   SUBROUTINE GetPath ( GivenFil, PathName )


      ! Let's parse the path name from the name of the given file.
      ! We'll count everything before (and including) the last "\" or "/".


      ! Argument declarations.

   CHARACTER(*), INTENT(IN)     :: GivenFil                                     ! The name of the given file.
   CHARACTER(*), INTENT(OUT)    :: PathName                                     ! The path name of the given file.


      ! Local declarations.

   INTEGER                      :: I                                            ! DO index for character position.


      ! Look for path separators

   I = INDEX( GivenFil, '\', BACK=.TRUE. )
   I = MAX( I, INDEX( GivenFil, '/', BACK=.TRUE. ) )

   IF ( I == 0 ) THEN
      ! we don't have a path specified, return '.'
      PathName = '.'//PathSep
   ELSE
      PathName = GivenFil(:I)
   END IF


   RETURN
   END SUBROUTINE GetPath ! ( GivenFil, PathName )
!=======================================================================
   SUBROUTINE GetRoot ( GivenFil, RootName )


      ! Let's parse the root file name from the name of the given file.
      ! We'll count everything after the last period as the extension.


      ! Argument declarations.

   CHARACTER(*), INTENT(IN)     :: GivenFil                                     ! The name of the given file.
   CHARACTER(*), INTENT(OUT)    :: RootName                                     ! The parsed root name of the given file.


      ! Local declarations.

   INTEGER                      :: I                                            ! DO index for character position.



      ! Deal with a couple of special cases.

   IF ( ( TRIM( GivenFil ) == "." ) .OR. (  TRIM( GivenFil ) == ".." ) )  THEN
      RootName = TRIM( GivenFil )
      RETURN
   END IF


      ! More-normal cases.

   DO I=LEN_TRIM( GivenFil ),1,-1


      IF ( GivenFil(I:I) == '.' )  THEN

         IF ( I == 1 ) THEN
            RootName = ''
            RETURN
         END IF

         IF ( I < LEN_TRIM( GivenFil ) ) THEN                   ! Make sure the index I is okay
            IF ( INDEX( '\/', GivenFil(I+1:I+1)) == 0 ) THEN    ! Make sure we don't have the RootName in a different directory
               RootName = GivenFil(:I-1)
            ELSE
               RootName = GivenFil                              ! This does not have a file extension
            end if
         ELSE
            RootName = GivenFil(:I-1)
         end if

         RETURN

      END IF
   END DO ! I

   RootName =  GivenFil


   RETURN
   END SUBROUTINE GetRoot ! ( GivenFil, RootName )
!=======================================================================
   SUBROUTINE GetTokens ( Line, NumTok, Tokens, Error )


      ! This routine will parse Line for NumTok "tokens" and return them in the Tokens array.
      ! THis routine differs from GetWords() in that it uses only spaces as token separators.


      ! Argument declarations.

   INTEGER, INTENT(IN)          :: NumTok                                       ! The number of "words" to look for.

   LOGICAL, INTENT(OUT)         :: Error                                        ! Error flag to indicate an insuffient number of tokens were found.

   CHARACTER(*), INTENT(INOUT)  :: Line                                         ! The string to search.
   CHARACTER(*), INTENT(OUT)    :: Tokens  (NumTok)                             ! The tokens that were found.


      ! Local declarations.

   INTEGER                      :: IT                                           ! Token index.
   INTEGER                      :: NextBlank                                    ! The location of the next blank character.



   NextBlank = 0

   DO IT=1,NumTok

      Line      = ADJUSTL( Line(NextBlank+1:) )
      NextBlank = INDEX  ( Line , ' ' )

      IF ( NextBlank == 0 )  THEN
        Error = .TRUE.
        RETURN
      END IF

      Tokens(IT) = Line(1:NextBlank-1)

   END DO ! IT

   Error = .FALSE.


   RETURN
   END SUBROUTINE GetTokens ! ( Line, NumTok, Tokens, Error )
!=======================================================================
   SUBROUTINE GetWords ( Line, Words, NumWords )


      ! This subroutine is used to get NumWords "words" from a line of text.


      ! Argument declarations.

   INTEGER, INTENT(IN)          :: NumWords                                     ! The number of words to look for.

   CHARACTER(*), INTENT(IN)     :: Line                                         ! The string to search.
   CHARACTER(*), INTENT(OUT)    :: Words(NumWords)                              ! The array of found words.


      ! Local declarations.

   INTEGER                      :: Ch                                           ! Character position within the string.
   INTEGER                      :: IW                                           ! Word index.
   INTEGER                      :: NextWhite                                    ! The location of the next whitespace in the string.



      ! Let's prefill the array with blanks.

   DO IW=1,NumWords
      Words(IW) = ' '
   END DO ! IW


      ! Let's make sure we have text on this line.

   IF ( LEN_TRIM( Line ) == 0 )  RETURN


      ! Parse words separated by any combination of spaces, tabs, commas,
      ! semicolons, single quotes, and double quotes ("whitespace").

   Ch = 0
   IW = 0

   DO

      NextWhite = SCAN( Line(Ch+1:) , ' ,;''"'//Tab )

      IF ( NextWhite > 1 )  THEN

         IW        = IW + 1
         Words(IW) = Line(Ch+1:Ch+NextWhite-1)

         IF ( IW == NumWords )  EXIT

         Ch = Ch + NextWhite

      ELSE IF ( NextWhite == 1 )  THEN

         Ch = Ch + 1

         CYCLE

      ELSE

         EXIT

      END IF

   END DO


   RETURN
   END SUBROUTINE GetWords ! ( Line, Words, NumWords )
!=======================================================================
   FUNCTION Int2LStr ( Intgr )


      ! This function returns a left-adjusted string representing the passed integer.



   CHARACTER(11)                :: Int2LStr                                     ! This function.


      ! Argument declarations.

   INTEGER, INTENT(IN)          :: Intgr                                        ! The integer to convert to a left-justified string.



   WRITE (Int2LStr,'(I11)')  Intgr

   Int2Lstr = ADJUSTL( Int2LStr )


   RETURN
   END FUNCTION Int2LStr ! ( Intgr )
!=======================================================================
   SUBROUTINE NameOFile ( InArg, OutExten, OutFile, ErrStat )


      ! Get the name of the input file from the InArgth command-line argument.
      ! Remove the extension if there is one, and append OutExten to the end.


      ! Argument declarations.

   INTEGER, INTENT(OUT), OPTIONAL :: ErrStat                                     ! Error status; if present, program does not abort on error
   INTEGER, INTENT(IN)          :: InArg                                        ! The number of the command-line argument that should hold the input file name.

   CHARACTER(*), INTENT(IN)     :: OutExten                                     ! The requested extension for the output file.
   CHARACTER(*), INTENT(OUT)    :: OutFile                                      ! The name of the output file.


      ! Local declarations.

   CHARACTER(100)               :: InFile                                       ! The name of the input file.
   CHARACTER(100)               :: RootName                                     ! The root name of the input file.



      ! See if the command line has enough arguments.

   IF ( InArg > COMMAND_ARGUMENT_COUNT() )  THEN
      CALL ProgAbort ( 'Insufficient arguments on the command line (at least '//&
                         TRIM( Int2LStr( InArg ) )//' were expected).', PRESENT(ErrStat) )
      IF ( PRESENT( ErrStat ) ) ErrStat = 1
      RETURN
   END IF


      ! Get the root of the input file name (strip off the extension).

   CALL GET_COMMAND_ARGUMENT( InArg, InFile )
   CALL GetRoot ( TRIM( InFile ), RootName )

   OutFile = TRIM( RootName )//'.'//OutExten

   IF ( PRESENT( ErrStat ) ) ErrStat = 0

   RETURN
   END SUBROUTINE NameOFile ! ( InArg, OutExten, OutFile [, ErrStat])
!=======================================================================
   SUBROUTINE NormStop


      ! This routine performs a normal termination of the program.


   CALL WrScr1   ( ' '//TRIM( ProgName )//' terminated normally.' )
   CALL WrScr    ( '' )
   CALL ProgExit ( 0 )


   END SUBROUTINE NormStop
!=======================================================================
   SUBROUTINE OpenBin ( Un, OutFile, RecLen, ErrStat )

      ! This routine opens a binary output file.


      ! Argument declarations.

   INTEGER, INTENT(IN)          :: Un                                           ! Logical unit for the output file.
   INTEGER, INTENT(IN)          :: RecLen                                       ! Length of binary record.
   INTEGER, INTENT(OUT), OPTIONAL :: ErrStat                                     ! Error status; if present, program does not abort on error


   CHARACTER(*), INTENT(IN)     :: OutFile                                      ! Name of the output file.


      ! Local declarations.

   LOGICAL                      :: Error                                        ! Flag to indicate the open failed.


      ! Open output file.  Make sure it worked.

   CALL OpenBinFile ( Un, OutFile, RecLen, Error )

   IF ( Error )  THEN
      CALL ProgAbort ( ' Cannot open file "'//TRIM( OutFile )// &
                       '".  Another program may have locked it for writing.', PRESENT(ErrStat) )
      IF ( PRESENT(ErrStat) ) ErrStat = 1
      RETURN
   ELSE
      IF ( PRESENT(ErrStat) ) ErrStat = 0
   ENDIF


   RETURN
   END SUBROUTINE OpenBin ! ( Un, OutFile, RecLen [, ErrStat] )
!=======================================================================
   SUBROUTINE OpenBInpFile ( Un, InFile, ErrStat )


      ! This routine opens a binary input file.

   IMPLICIT                        NONE



      ! Argument declarations.

   INTEGER, INTENT(IN)            :: Un                                          ! Logical unit for the input file.
   INTEGER, INTENT(OUT), OPTIONAL :: ErrStat                                     ! Error status; if present, program does not abort on error

   CHARACTER(*), INTENT(IN)       :: InFile                                      ! Name of the input file.


      ! Local declarations.

      ! NOTE: Do not explicitly declare the precision of this variable [as in
      !       LOGICAL(1)] so that the statements using this variable work with
      !       any compiler:
   LOGICAL                      :: Exists                                       ! Flag indicating whether or not a file Exists.
   LOGICAL                      :: Error                                        ! Flag to indicate the open failed.



      ! See if input file Exists.

   INQUIRE ( FILE=TRIM( InFile ) , EXIST=Exists )

   IF ( .NOT. Exists )  THEN
      CALL ProgAbort ( ' The input file, "'//TRIM( InFile )//'", was not found.', PRESENT( ErrStat) )
      IF ( PRESENT( ErrStat) ) ErrStat = -1
      RETURN
   END IF


      ! Open input file.  Make sure it worked.

   CALL OpenBinInpFile ( Un, InFile, Error )

   IF ( Error )  THEN

      CALL ProgAbort ( ' Cannot open file "'//TRIM( InFile )//'".  Another program may have locked it.', PRESENT( ErrStat ) )
      IF ( PRESENT( ErrStat) ) ErrStat = 1
      RETURN

   ELSE

      IF ( PRESENT( ErrStat) ) ErrStat = 0

   END IF


   RETURN
   END SUBROUTINE OpenBInpFile
!=======================================================================
   SUBROUTINE OpenEcho ( Un, OutFile, ErrStat )


      ! This routine opens a formatted output file for the echo file.


      ! Argument declarations.

   INTEGER, INTENT(OUT), OPTIONAL :: ErrStat                                   ! Error status; if present, program does not abort on error
   INTEGER, INTENT(IN)            :: Un                                        ! Logical unit for the input file.

   CHARACTER(*), INTENT(IN)       :: OutFile                                   ! Name of the input file.




   UnEc = Un

   IF ( PRESENT(ErrStat) ) THEN

      CALL OpenFOutFile( UnEc, OutFile, ErrStat )

   ELSE

      CALL OpenFOutFile( UnEc, OutFile )

   ENDIF

   Echo = .TRUE.

   RETURN
   END SUBROUTINE OpenEcho ! ( Un, OutFile [, ErrStat]  )
!=======================================================================
   SUBROUTINE OpenFInpFile ( Un, InFile, ErrStat )


      ! This routine opens a formatted input file.


      ! Argument declarations.

   INTEGER, INTENT(IN)          :: Un                                           ! Logical unit for the input file.
   INTEGER, INTENT(OUT),OPTIONAL:: ErrStat                                      ! Error status; if present, program does not abort on error
   CHARACTER(*), INTENT(IN)     :: InFile                                       ! Name of the input file.


      ! Local declarations.

   INTEGER                      :: IOS                                          ! I/O status of OPEN.

   LOGICAL                      :: Exists                                       ! Flag indicating whether or not a file Exists.



      ! See if input file Exists.

   INQUIRE ( FILE=TRIM( InFile ) , EXIST=Exists )

   IF ( .NOT. Exists )  THEN
      CALL ProgAbort ( ' The input file, "'//TRIM( InFile )//'", was not found.', PRESENT(ErrStat) )
      IF (PRESENT(ErrStat)) ErrStat = -1
      RETURN
   END IF


      ! Open input file.  Make sure it worked.

   OPEN( Un, FILE=TRIM( InFile ), STATUS='OLD', FORM='FORMATTED', IOSTAT=IOS, ACTION='READ' )

   IF ( IOS /= 0 )  THEN
      CALL ProgAbort ( ' Cannot open file "'//TRIM( InFile ) &
                      //'".  Another program like MS Excel may have locked it for writing.',PRESENT(ErrStat)  )
      IF (PRESENT(ErrStat)) ErrStat = 1
      RETURN
   ELSE
      IF (PRESENT(ErrStat)) ErrStat = 0
   END IF


   RETURN
   END SUBROUTINE OpenFInpFile ! ( Un, InFile [, ErrStat] )
!=======================================================================
   SUBROUTINE OpenFOutFile ( Un, OutFile, ErrStat )


      ! This routine opens a formatted output file.


      ! Argument declarations.

   INTEGER, INTENT(OUT), OPTIONAL :: ErrStat                                    ! Error status; if present, program does not abort on error
   INTEGER, INTENT(IN)          :: Un                                           ! Logical unit for the output file.

   CHARACTER(*), INTENT(IN)     :: OutFile                                      ! Name of the output file.


      ! Local declarations.

   INTEGER                      :: IOS                                          ! I/O status of OPEN.



      ! Open output file.  Make sure it worked.

   OPEN( Un, FILE=TRIM( OutFile ), STATUS='UNKNOWN', FORM='FORMATTED', IOSTAT=IOS, ACTION="WRITE" )



   IF ( PRESENT(ErrStat) ) ErrStat = IOS

   IF ( IOS /= 0 )  CALL ProgAbort( ' Cannot open file "'//TRIM( OutFile )// &
                                    '".  Another program like MS Excel may have locked it for writing.', PRESENT(ErrStat) )


   RETURN
   END SUBROUTINE OpenFOutFile ! ( Un, OutFile [, ErrStat] )
!=======================================================================
   SUBROUTINE OpenFUnkFile ( Un, OutFile, FailAbt, Failed, Exists, ErrStat )


      ! This routine opens a formatted output file and returns a flag telling if it already existed.


      ! Argument declarations.

   INTEGER, INTENT(OUT), OPTIONAL :: ErrStat                                    ! Error status; if present, program does not abort on error
   INTEGER, INTENT(IN)          :: Un                                           ! Logical unit for the output file.

   LOGICAL, INTENT(OUT)         :: Exists                                       ! Flag that indicates if the file already existedo.
   LOGICAL, INTENT(IN)          :: FailAbt                                      ! Flag that tells this routine to abort if the open fails.
   LOGICAL, INTENT(OUT)         :: Failed                                       ! Flag that indicates if the open failed.

   CHARACTER(*), INTENT(IN)     :: OutFile                                      ! Name of the output file.


      ! Local declarations.

   INTEGER                      :: IOS                                          ! I/O status of OPEN.



      ! Check to see if the file already exists.

   INQUIRE ( FILE=TRIM( OutFile ) , EXIST=Exists )

!bjj: should we be checking something here?


      ! Open output file.  Make sure it worked.

   OPEN( Un, FILE=TRIM( OutFile ), STATUS='UNKNOWN', FORM='FORMATTED', IOSTAT=IOS )

   IF ( PRESENT(ErrStat) ) ErrStat = IOS

   IF ( IOS /= 0 )  THEN
      Failed = .TRUE.
      IF ( FailAbt )  CALL ProgAbort ( ' Cannot open file "'//TRIM( OutFile ) &
                                 //'".  Another program like MS Excel may have locked it for writing.', PRESENT(ErrStat) )
   ELSE
      Failed = .FALSE.
   END IF


   RETURN
   END SUBROUTINE OpenFUnkFile ! ( Un, OutFile, FailAbt, Failed, Exists [,ErrStat] )
!=======================================================================
   SUBROUTINE OpenUInfile ( Un, InFile, ErrStat )


      !  This routine opens an unformatted input file.


      ! Argument declarations.

   INTEGER, INTENT(OUT), OPTIONAL :: ErrStat                                    ! Error status; if present, program does not abort on error
   INTEGER, INTENT(IN)         ::  Un                                           ! Logical unit for the input file

   CHARACTER(*), INTENT(IN)    ::  InFile                                       ! Name of the input file


      ! Local declarations.

   INTEGER                     ::  IOS                                          ! Returned input/output status.

   LOGICAL                      :: Exists                                       ! Flag indicating whether or not a file Exists.



      ! See if input file Exists.

   INQUIRE ( FILE=TRIM( InFile ) , EXIST=Exists )

   IF ( .NOT. Exists )  THEN
      CALL ProgAbort ( ' The input file, "'//TRIM( InFile )//'", was not found.', PRESENT(ErrStat) )
      IF ( PRESENT(ErrStat) ) ErrStat = -1
      RETURN
   END IF


      ! Open the file.

   OPEN ( Un, FILE=TRIM( InFile ), STATUS='UNKNOWN', FORM=UnfForm, ACCESS='SEQUENTIAL', IOSTAT=IOS, ACTION='READ' )

   IF ( PRESENT(ErrStat) ) ErrStat = IOS

   IF ( IOS /= 0 )  CALL ProgAbort( ' Cannot open file "'//TRIM( InFile )// &
                                    '".  Another program may have locked it for writing.', PRESENT(ErrStat) )



   RETURN
   END SUBROUTINE OpenUInfile ! ( Un, InFile [,ErrStat] )
!=======================================================================
SUBROUTINE OpenUInBEFile( Un, InFile, RecLen, ErrStat )

      !  This routine opens an unformatted input file of RecLen-byte data records
      !  stored in Big Endian format.


      ! Argument declarations.

   INTEGER, INTENT(IN)           ::  Un                                         ! Logical unit for the input file
   CHARACTER(*), INTENT(IN)      ::  InFile                                     ! Name of the input file
   INTEGER, INTENT(IN)           ::  RecLen                                     ! The input file's record length in bytes
   INTEGER, INTENT(OUT),OPTIONAL ::  ErrStat                                    ! Error status; if present, program does not abort on error



      ! Local declarations.

   LOGICAL                       :: Exists                                       ! Flag to indicate if a file exists
   LOGICAL                       :: Error                                        ! Flag to indicate the open failed



      ! See if input file Exists.

   INQUIRE ( FILE=TRIM( InFile ) , EXIST=Exists )

   IF ( .NOT. Exists )  THEN
      CALL ProgAbort ( ' The input file, "'//TRIM( InFile )//'", was not found.', PRESENT(ErrStat) )
      IF ( PRESENT(ErrStat) ) ErrStat = -1
      RETURN
   END IF


      ! Open the file.

   CALL OpenUnfInpBEFile ( Un, InFile, RecLen, Error )

   IF ( Error )  THEN

      CALL ProgAbort ( ' Cannot open file "'//TRIM( InFile )//'".  Another program may have locked it.', PRESENT( ErrStat ) )
      IF ( PRESENT( ErrStat) ) ErrStat = 1
      RETURN

   ELSE

      IF ( PRESENT( ErrStat) ) ErrStat = 0

   END IF


   RETURN

END SUBROUTINE OpenUInBEFile !( Un, InFile, RecLen [, ErrStat] )
!=======================================================================
   SUBROUTINE OpenUOutfile ( Un, OutFile, ErrStat )


      !  This routine opens an unformatted output file.


      ! Argument declarations.

   INTEGER, INTENT(IN)            ::  Un                                        ! Logical unit for the output file
   INTEGER, INTENT(OUT), OPTIONAL :: ErrStat                                    ! Error status; if present, program does not abort on error

   CHARACTER(*), INTENT(IN)       ::  OutFile                                   ! Name of the output file


      ! Local declarations.

   INTEGER                        ::  IOS                                       ! Returned input/output status.



      ! Open the file.

   OPEN ( Un, FILE=TRIM( OutFile ), STATUS='UNKNOWN', FORM=UnfForm, ACCESS='SEQUENTIAL', IOSTAT=IOS, ACTION='WRITE' )


   IF ( PRESENT( ErrStat ) )   ErrStat = IOS

   IF ( IOS /= 0 )  CALL ProgAbort( ' Cannot open file "'//TRIM( OutFile )// &
                                    '".  Another program may have locked it for writing.', PRESENT( ErrStat ) )


   RETURN
   END SUBROUTINE OpenUOutfile ! ( Un, InFile [,ErrStat] )
!=======================================================================
   SUBROUTINE PremEOF ( Fil , Variable, TrapErrors )


      ! This routine prints out an EOF message and aborts the program.


      ! Argument declarations.

   LOGICAL, INTENT(IN), OPTIONAL:: TrapErrors                                   ! Determines if the program should abort or return to calling function
   LOGICAL                      :: TrapThisError                                ! The local version of TrapErrors

   CHARACTER(*), INTENT(IN)     :: Fil                                          ! The name of the file that ran out of data.
   CHARACTER(*), INTENT(IN)     :: Variable                                     ! The name of the variable we were trying to read at the time.


   IF ( PRESENT( TrapErrors ) ) THEN
      TrapThisError = TrapErrors
   ELSE
      TrapThisError = .FALSE.
   END IF

   CALL WrScr1 ( ' Premature EOF for file "'//TRIM( Fil )//'".' )

   CALL ProgAbort  ( ' The error occurred while trying to read '//TRIM( Variable )//'.', TrapThisError )


   RETURN
   END SUBROUTINE PremEOF ! ( Fil , Variable [, TrapErrors] )
!=======================================================================
   SUBROUTINE ProgAbort ( Message, TrapErrors )


      ! This routine outputs fatal error messages and stops the program.


      ! Argument declarations.

   LOGICAL, INTENT(IN), OPTIONAL:: TrapErrors                                   ! Determines if the program should abort or return to calling function
   CHARACTER(*), INTENT(IN)     :: Message                                      ! Error message.



   IF ( Beep )  CALL UsrAlarm

   CALL WrScr    ( Message )
   IF ( PRESENT(TrapErrors) )  THEN
      IF ( TrapErrors ) RETURN
   END IF

   IF ( LEN_TRIM(ProgName) > 0 ) THEN
      CALL WrScr1   ( ' Aborting '//TRIM( ProgName )//'.' )
   ELSE
      CALL WrScr1   ( ' Aborting program.' )
   END IF
   CALL WrScr    ( ' ' )
   CALL ProgExit ( 1 )


   END SUBROUTINE ProgAbort ! ( Message [, TrapErrors] )
!=======================================================================
   SUBROUTINE ProgWarn ( Message )


      ! This routine outputs non-fatal warning messages and returns to the calling routine.


      ! Argument declarations.

   CHARACTER(*), INTENT(IN)     :: Message                                      ! Warning message.



   IF ( Beep )  CALL UsrAlarm
   CALL WrScr ( ' WARNING:  '//Message )


   RETURN
   END SUBROUTINE ProgWarn ! ( Message )
!=======================================================================
   SUBROUTINE ReadCAry ( UnIn, Fil, CharAry, AryLen, AryName, AryDescr, ErrStat )


      ! This routine reads a AryLen values into a character array from the next AryLen lines of the input file.


      ! Argument declarations:

   INTEGER, INTENT(IN)          :: AryLen                                          ! Length of the array.
   INTEGER, INTENT(IN)          :: UnIn                                            ! I/O unit for input file.
   INTEGER, INTENT(OUT),OPTIONAL:: ErrStat                                         ! Error status; if present, program does not abort on error

   CHARACTER(*), INTENT(OUT)    :: CharAry(AryLen)                                 ! Real variable being read.
   CHARACTER(*), INTENT(IN)     :: AryDescr                                        ! Text string describing the variable.
   CHARACTER(*), INTENT(IN)     :: AryName                                         ! Text string containing the variable name.
   CHARACTER(*), INTENT(IN)     :: Fil                                             ! Name of the input file.


      ! Local declarations:

   INTEGER                      :: Ind                                             ! Index into the string array.  Assumed to be one digit.
   INTEGER                      :: IOS                                             ! I/O status returned from the read statement.

   CHARACTER(44)                :: Frmt = "(15X,A,T30,' - ',A,/,2X,100('""',A,'""',:,1X))"    ! Output format for string parameters.



   READ (UnIn,*,IOSTAT=IOS)  ( CharAry(Ind), Ind=1,AryLen )

   CALL CheckIOS ( IOS, Fil, TRIM( AryName ), StrType, PRESENT(ErrStat) )

   IF (PRESENT(ErrStat) ) THEN
      ErrStat = IOS
      IF (IOS /= 0) RETURN
   ENDIF

   IF ( Echo )  THEN
      WRITE (UnEc,Frmt)  TRIM( AryName ), AryDescr, ( TRIM( CharAry(Ind) ), Ind=1,AryLen )
   END IF


   RETURN
   END SUBROUTINE ReadCAry ! ( UnIn, Fil, CharAry, AryLen, AryName, AryDescr [, ErrStat] )
!=======================================================================
   SUBROUTINE ReadCAryLines ( UnIn, Fil, CharAry, AryLen, AryName, AryDescr, ErrStat )


      ! This routine reads a AryLen values into a real array from the next AryLen lines of the input file.


      ! Argument declarations:

   INTEGER, INTENT(IN)          :: AryLen                                          ! Length of the array.
   INTEGER, INTENT(IN)          :: UnIn                                            ! I/O unit for input file.
   INTEGER, INTENT(OUT),OPTIONAL:: ErrStat                                         ! Error status; if present, program does not abort on error

   CHARACTER(*), INTENT(OUT)    :: CharAry(AryLen)                                 ! Char variable being read.

   CHARACTER(*), INTENT(IN)     :: Fil                                             ! Name of the input file.
   CHARACTER(*), INTENT(IN)     :: AryDescr                                        ! Text string describing the variable.
   CHARACTER(*), INTENT(IN)     :: AryName                                         ! Text string containing the variable name.


      ! Local declarations:

   INTEGER                      :: Ind                                             ! Index into the real array.  Assumed to be one digit.
   INTEGER                      :: IOS                                             ! I/O status returned from the read statement.

   CHARACTER(35)                :: Frmt = "( 15X, A, T30, ' - ', A, /, 2X, A )"    ! Output format for string parameters.


   IF ( PRESENT(ErrStat) ) ErrStat = 0

   DO Ind=1,AryLen
      READ (UnIn,*,IOSTAT=IOS)  CharAry(Ind)

      CALL CheckIOS ( IOS, Fil, TRIM( AryName )//'('//TRIM( Int2LStr( Ind ) )//')', StrType, PRESENT(ErrStat) )

      IF (IOS /= 0) THEN
         IF ( PRESENT(ErrStat) ) ErrStat = IOS
         RETURN
      ENDIF

      IF ( Echo )  THEN
         WRITE (UnEc,Frmt)  TRIM( AryName )//'('//TRIM( Int2LStr( Ind ) )//')', AryDescr, TRIM(CharAry(Ind))
      END IF
   END DO

   RETURN
   END SUBROUTINE ReadCAryLines ! ( UnIn, Fil, RealAry, AryLen, AryName, AryDescr [, ErrStat] )
!=======================================================================
   SUBROUTINE ReadCom ( UnIn, Fil, ComName, ErrStat )

      ! This routine reads a comment from the next line of the input file.


      ! Argument declarations:

   INTEGER, INTENT(IN)          :: UnIn                                            ! I/O unit for input file.
   INTEGER, INTENT(OUT),OPTIONAL:: ErrStat                                         ! Error status; if present, program does not abort on error

   CHARACTER(*), INTENT(IN)   :: Fil                                             ! Name of the input file.
   CHARACTER(*), INTENT(IN)   :: ComName                                         ! Text string containing the comment name.


      ! Local declarations:

   INTEGER                      :: IOS                                             ! I/O status returned from the read statement.

   CHARACTER(200)               :: Comment                                         ! Text string containing the comment.



   READ (UnIn,'(A)',IOSTAT=IOS)  Comment

   CALL CheckIOS ( IOS, Fil, ComName, StrType, PRESENT(ErrStat) )

   IF ( PRESENT(ErrStat) ) THEN
      ErrStat = IOS
      IF (IOS /= 0) RETURN
   ENDIF

   IF ( Echo )  THEN
      WRITE (UnEc,'(A)')  Comment
   END IF


   RETURN
   END SUBROUTINE ReadCom ! ( UnIn, Fil, ComName [, ErrStat] )
!=======================================================================
   SUBROUTINE ReadCVar ( UnIn, Fil, CharVar, VarName, VarDescr, ErrStat )


      ! This routine reads a single character variable from the next line of the input file.


      ! Argument declarations:

   INTEGER, INTENT(IN)          :: UnIn                                            ! I/O unit for input file.
   INTEGER, INTENT(OUT),OPTIONAL:: ErrStat                                         ! Error status; if present, program does not abort on error

   CHARACTER(*), INTENT(OUT)    :: CharVar                                         ! Integer variable being read.
   CHARACTER(*), INTENT(IN)     :: Fil                                             ! Name of the input file.
   CHARACTER(*), INTENT(IN)     :: VarDescr                                        ! Text string describing the variable.
   CHARACTER(*), INTENT(IN)     :: VarName                                         ! Text string containing the variable name.


      ! Local declarations:

   INTEGER                      :: IOS                                             ! I/O status returned from the read statement.

   CHARACTER(35)                :: Frmt = "( 15X, A, T30, ' - ', A, /, 2X, A )"    ! Output format for string parameters.




   READ (UnIn,*,IOSTAT=IOS)  CharVar

   CALL CheckIOS ( IOS, Fil, VarName, StrType, PRESENT(ErrStat) )

   IF ( PRESENT(ErrStat) ) THEN
      ErrStat = IOS
      IF (IOS /= 0) RETURN
   ENDIF

   IF ( Echo )  THEN
      WRITE (UnEc,Frmt)  VarName, VarDescr, '"'//TRIM( CharVar )//'"'
   END IF


   RETURN
   END SUBROUTINE ReadCVar ! ( UnIn, Fil, CharVar, VarName, VarDescr [, ErrStat] )
!=======================================================================
   SUBROUTINE ReadIAry ( UnIn, Fil, IntAry, AryLen, AryName, AryDescr, ErrStat )


      ! This routine reads a AryLen values into an integer array from the next AryLen lines of the input file.


      ! Argument declarations:

   INTEGER, INTENT(IN)          :: AryLen                                          ! Length of the array.
   INTEGER, INTENT(OUT)         :: IntAry(AryLen)                                  ! Integer array being read.
   INTEGER, INTENT(IN)          :: UnIn                                            ! I/O unit for input file.
   INTEGER, INTENT(OUT),OPTIONAL:: ErrStat                                         ! Error status; if present, program does not abort on error

   CHARACTER(*), INTENT(IN)     :: Fil                                             ! Name of the input file.
   CHARACTER(*), INTENT(IN)     :: AryDescr                                        ! Text string describing the variable.
   CHARACTER(*), INTENT(IN)     :: AryName                                         ! Text string containing the variable name.


      ! Local declarations:

   INTEGER                      :: Ind                                             ! Index into the integer array.  Assumed to be one digit.
   INTEGER                      :: IOS                                             ! I/O status returned from the read statement.

   CHARACTER(38)                :: Frmt = "( 2X, I11, 2X, A, T30, ' - ', A )"      ! Output format for integer array parameters



   READ (UnIn,*,IOSTAT=IOS)  ( IntAry(Ind), Ind=1,AryLen )

   CALL CheckIOS ( IOS, Fil, TRIM( AryName ), NumType, PRESENT(ErrStat) )

   IF ( PRESENT(ErrStat) ) THEN
      ErrStat = IOS
      IF (IOS /= 0) RETURN
   ENDIF

   IF ( Echo )  THEN
      DO Ind=1,AryLen
         WRITE (UnEc,Frmt)  IntAry(Ind), TRIM( AryName ), AryDescr
      END DO ! Ind
   END IF


   RETURN
   END SUBROUTINE ReadIAry ! ( UnIn, Fil, IntAry, AryLen, AryName, AryDescr [, ErrStat])
!=======================================================================
   SUBROUTINE ReadIVar ( UnIn, Fil, IntVar, VarName, VarDescr, ErrStat )


      ! This routine reads a single integer variable from the next line of the input file.


      ! Argument declarations:

   INTEGER, INTENT(OUT)         :: IntVar                                          ! Integer variable being read.
   INTEGER, INTENT(IN)          :: UnIn                                            ! I/O unit for input file.
   INTEGER, INTENT(OUT),OPTIONAL:: ErrStat                                         ! Error status; if present, program does not abort on error

   CHARACTER(*), INTENT(IN)     :: Fil                                             ! Name of the input file.
   CHARACTER(*), INTENT(IN)     :: VarDescr                                        ! Text string describing the variable.
   CHARACTER(*), INTENT(IN)     :: VarName                                         ! Text string containing the variable name.


      ! Local declarations:

   INTEGER                      :: IOS                                             ! I/O status returned from the read statement.

   CHARACTER(33)                :: Frmt = "( 2X, I11, 2X, A, T30, ' - ', A )"      ! Output format for integer parameters.
   CHARACTER(30)                :: Word                                            ! String to hold the first word on the line.



   IF ( PRESENT(ErrStat) ) THEN
      CALL ReadNum ( UnIn, Fil, Word, VarName, ErrStat )
      IF (ErrStat /= 0) RETURN
   ELSE
      CALL ReadNum ( UnIn, Fil, Word, VarName )
   END IF

   READ (Word,*,IOSTAT=IOS)  IntVar

   CALL CheckIOS ( IOS, Fil, VarName, NumType, PRESENT(ErrStat) )

   IF ( PRESENT(ErrStat) ) THEN
      ErrStat = IOS
      IF (IOS /= 0) RETURN
   ENDIF

   IF ( Echo )  THEN
      WRITE (UnEc,Frmt)  IntVar, VarName, VarDescr
   END IF


   RETURN
   END SUBROUTINE ReadIVar ! ( UnIn, Fil, IntVar, VarName, VarDescr [, ErrStat])
!=======================================================================
   SUBROUTINE ReadLAry ( UnIn, Fil, LogAry, AryLen, AryName, AryDescr, ErrStat )


      ! This routine reads a AryLen values into an logical array from the next AryLen lines of the input file.


      ! Argument declarations:

   INTEGER, INTENT(IN)          :: AryLen                                          ! Length of the array.
   INTEGER, INTENT(IN)          :: UnIn                                            ! I/O unit for input file.
   INTEGER, INTENT(OUT),OPTIONAL:: ErrStat                                         ! Error status; if present, program does not abort on error

   LOGICAL, INTENT(OUT)         :: LogAry(AryLen)                                  ! Logical array being read.

   CHARACTER(*), INTENT(IN)     :: Fil                                             ! Name of the input file.
   CHARACTER(*), INTENT(IN)     :: AryDescr                                        ! Text string describing the variable.
   CHARACTER(*), INTENT(IN)     :: AryName                                         ! Text string containing the variable name.


      ! Local declarations:

   INTEGER                      :: Ind                                             ! Index into the integer array.  Assumed to be one digit.
   INTEGER                      :: IOS                                             ! I/O status returned from the read statement.

   CHARACTER(38)                :: Frmt = "( 2X, I11, 2X, A, T30, ' - ', A )"      ! Output format for integer array parameters



   READ (UnIn,*,IOSTAT=IOS)  ( LogAry(Ind), Ind=1,AryLen )

   CALL CheckIOS ( IOS, Fil, TRIM( AryName ), NumType, PRESENT(ErrStat) )

   IF ( PRESENT(ErrStat) ) THEN
      ErrStat = IOS
      IF (IOS /= 0) RETURN
   ENDIF

   IF ( Echo )  THEN
      DO Ind=1,AryLen
         WRITE (UnEc,Frmt)  LogAry(Ind), TRIM( AryName ), AryDescr
      END DO ! Ind
   END IF


   RETURN
   END SUBROUTINE ReadLAry ! ( UnIn, Fil, LogAry, AryLen, AryName, AryDescr [, ErrStat])
!=======================================================================
   SUBROUTINE ReadLVar ( UnIn, Fil, LogVar, VarName, VarDescr, ErrStat )


      ! This routine reads a single logical variable from the next line of the input file.


      ! Argument declarations:

   INTEGER, INTENT(IN)          :: UnIn                                            ! I/O unit for input file.
   INTEGER, INTENT(OUT),OPTIONAL:: ErrStat                                         ! Error status; if present, program does not abort on error

   LOGICAL, INTENT(OUT)         :: LogVar                                          ! Logical variable being read.

   CHARACTER(*), INTENT(IN)     :: Fil                                             ! Name of the input file.
   CHARACTER(*), INTENT(IN)     :: VarDescr                                        ! Text string describing the variable.
   CHARACTER(*), INTENT(IN)     :: VarName                                         ! Text string containing the variable name.


      ! Local declarations:

   INTEGER                      :: IOS                                             ! I/O status returned from the read statement.

   CHARACTER(33)                :: Frmt  = "( 2X, L11, 2X, A, T30, ' - ', A )"     ! Output format for logical parameters.
   CHARACTER( 4)                :: VName                                           ! Temporary holder for the variable name.




   READ (UnIn,*,IOSTAT=IOS)  LogVar

   CALL CheckIOS ( IOS, Fil, VarName, FlgType, PRESENT(ErrStat) )

   IF ( PRESENT(ErrStat) ) THEN
      ErrStat = IOS
      IF (IOS /= 0) RETURN
   ENDIF

   VName = VarName

   CALL Conv2UC ( VName )

   IF ( Echo .AND. ( VName /= 'ECHO' ) )  THEN
      WRITE (UnEc,Frmt)  LogVar, VarName, VarDescr
   END IF


   RETURN
   END SUBROUTINE ReadLVar ! ( UnIn, Fil, LogVar, VarName, VarDescr [, ErrStat])
!=======================================================================
   SUBROUTINE ReadNum ( UnIn, Fil, Word, VarName, ErrStat )


      ! This routine reads a single word from a file and tests to see if it's a pure number (no true or false).


      ! Argument declarations:

   INTEGER, INTENT(IN)            :: UnIn                                          ! I/O unit for input file.
   INTEGER, INTENT(OUT), OPTIONAL :: ErrStat                                       ! Error status; if present, program does not abort on error

   CHARACTER(*), INTENT(IN)       :: Fil                                           ! Name of the input file.
   CHARACTER(*), INTENT(IN)       :: VarName                                       ! Text string containing the variable name.
   CHARACTER(*), INTENT(Out)      :: Word                                          ! Text string containing the first word from the input line.


      ! Local declarations:

   INTEGER                        :: IOS                                           ! I/O status returned from the read statement.


   IF ( PRESENT(ErrStat) ) ErrStat = 0


      ! Read in the first word of the input line.  Check I/O status.

   READ (UnIn,*,IOSTAT=IOS)  Word

   CALL CheckIOS ( IOS, Fil, VarName, NumType, PRESENT(ErrStat) )

   IF ( PRESENT(ErrStat) ) THEN
      ErrStat = IOS
      IF (IOS /= 0) RETURN
   ENDIF


      ! See if the word starts with a T or F.  If so, flag it as an invalid number.

   IF ( INDEX( 'FTft', Word(:1) ) > 0 )  THEN
      CALL WrScr ( '' )
      CALL ProgAbort( ' Invalid numeric input.  "'//TRIM( Word )//'" found when trying to read the number, '// &
                      TRIM( VarName )//'.', PRESENT(ErrStat) )

      IF ( PRESENT(ErrStat) ) ErrStat = 1
   END IF



   RETURN
   END SUBROUTINE ReadNum ! ( UnIn, Fil, Word, VarName [, ErrStat] )
!=======================================================================
   SUBROUTINE ReadRAry ( UnIn, Fil, RealAry, AryLen, AryName, AryDescr, ErrStat )


      ! This routine reads a AryLen values into a real array separated by white space (possibly on the same line of the input file).


      ! Argument declarations:

   INTEGER, INTENT(IN)          :: AryLen                                          ! Length of the array.
   INTEGER, INTENT(IN)          :: UnIn                                            ! I/O unit for input file.
   INTEGER, INTENT(OUT),OPTIONAL:: ErrStat                                         ! Error status; if present, program does not abort on error

   REAL(ReKi), INTENT(INOUT)    :: RealAry(AryLen)                                 ! Real array being read.

   CHARACTER(*), INTENT(IN)     :: Fil                                             ! Name of the input file.
   CHARACTER(*), INTENT(IN)     :: AryDescr                                        ! Text string describing the variable.
   CHARACTER(*), INTENT(IN)     :: AryName                                         ! Text string containing the variable name.


      ! Local declarations:

   INTEGER                      :: Ind                                             ! Index into the real array.  Assumed to be one digit.
   INTEGER                      :: IOS                                             ! I/O status returned from the read statement.

   CHARACTER(38)                :: Frmt = "( 2X, ES11.4e2, 2X, A, T30, ' - ', A )" ! Output format for real array parameters



   READ (UnIn,*,IOSTAT=IOS)  ( RealAry(Ind), Ind=1,AryLen )

   CALL CheckIOS ( IOS, Fil, TRIM( AryName ), NumType, PRESENT(ErrStat) )

   IF ( PRESENT(ErrStat) ) THEN
      ErrStat = IOS
      IF (IOS /= 0) RETURN
   ENDIF

   IF ( Echo )  THEN
      DO Ind=1,AryLen
         WRITE (UnEc,Frmt)  RealAry(Ind), TRIM( AryName ), AryDescr
      END DO ! Ind
   END IF


   RETURN
   END SUBROUTINE ReadRAry ! ( UnIn, Fil, RealAry, AryLen, AryName, AryDescr [, ErrStat] )
!=======================================================================
   SUBROUTINE ReadRAryLines ( UnIn, Fil, RealAry, AryLen, AryName, AryDescr, ErrStat )


      ! This routine reads a AryLen values into a real array from the next AryLen lines of the input file.


      ! Argument declarations:

   INTEGER, INTENT(IN)          :: AryLen                                          ! Length of the array.
   INTEGER, INTENT(IN)          :: UnIn                                            ! I/O unit for input file.
   INTEGER, INTENT(OUT),OPTIONAL:: ErrStat                                         ! Error status; if present, program does not abort on error

   REAL(ReKi), INTENT(OUT)      :: RealAry(AryLen)                                 ! Real array being read.

   CHARACTER(*), INTENT(IN)     :: Fil                                             ! Name of the input file.
   CHARACTER(*), INTENT(IN)     :: AryDescr                                        ! Text string describing the variable.
   CHARACTER(*), INTENT(IN)     :: AryName                                         ! Text string containing the variable name.


      ! Local declarations:

   INTEGER                      :: Ind                                             ! Index into the real array.  Assumed to be one digit.
   INTEGER                      :: IOS                                             ! I/O status returned from the read statement.

   CHARACTER(38)                :: Frmt = "( 2X, ES11.4e2, 2X, A, T30, ' - ', A )" ! Output format for real array parameters


   IF ( PRESENT(ErrStat) ) ErrStat = 0

   DO Ind=1,AryLen
      READ (UnIn,*,IOSTAT=IOS)  RealAry(Ind)

      CALL CheckIOS ( IOS, Fil, TRIM( AryName )//'('//TRIM( Int2LStr( Ind ) )//')', NumType, PRESENT(ErrStat) )

      IF (IOS /= 0) THEN
         IF ( PRESENT(ErrStat) ) ErrStat = IOS
         RETURN
      ENDIF

      IF ( Echo )  THEN
         WRITE (UnEc,Frmt)  RealAry(Ind), TRIM( AryName )//'('//TRIM( Int2LStr( Ind ) )//')', AryDescr
      END IF
   END DO

   RETURN
   END SUBROUTINE ReadRAryLines ! ( UnIn, Fil, RealAry, AryLen, AryName, AryDescr [, ErrStat] )
!=======================================================================
  SUBROUTINE ReadRVar ( UnIn, Fil, RealVar, VarName, VarDescr, ErrStat )


      ! This routine reads a single real variable from the next line of the input file.


      ! Argument declarations:

   REAL(ReKi), INTENT(OUT)      :: RealVar                                         ! Real variable being read.
   INTEGER, INTENT(OUT),OPTIONAL:: ErrStat                                         ! Error status; if present, program does not abort on error

   INTEGER, INTENT(IN)          :: UnIn                                            ! I/O unit for input file.

   CHARACTER( *), INTENT(IN)    :: Fil                                             ! Name of the input file.
   CHARACTER( *), INTENT(IN)    :: VarDescr                                        ! Text string describing the variable.
   CHARACTER( *), INTENT(IN)    :: VarName                                         ! Text string containing the variable name.


      ! Local declarations:

   INTEGER                      :: IOS                                             ! I/O status returned from the read statement.

   CHARACTER(38)                :: Frmt = "( 2X, ES11.4e2, 2X, A, T30, ' - ', A )" ! Output format for real parameters
   CHARACTER(30)                :: Word                                            ! String to hold the first word on the line.



   IF ( PRESENT(ErrStat) ) THEN
      CALL ReadNum ( UnIn, Fil, Word, VarName, ErrStat )
      IF (ErrStat /= 0) RETURN
   ELSE
      CALL ReadNum ( UnIn, Fil, Word, VarName )
   END IF

   READ (Word,*,IOSTAT=IOS)  RealVar

   CALL CheckIOS ( IOS, Fil, VarName, NumType, PRESENT(ErrStat) )

   IF ( PRESENT(ErrStat) ) THEN
      ErrStat = IOS
      IF (IOS /= 0) RETURN
   END IF

   IF ( Echo )  THEN
      WRITE (UnEc,Frmt)  RealVar, VarName, VarDescr
   END IF


   RETURN
   END SUBROUTINE ReadRVar ! ( UnIn, Fil, RealVar, VarName, VarDescr [, ErrStat] )
!=======================================================================
   SUBROUTINE ReadStr ( UnIn, Fil, CharVar, VarName, VarDescr, ErrStat )


      ! This routine reads a string from the next line of the input file.


      ! Argument declarations:

   INTEGER, INTENT(IN)          :: UnIn                                            ! I/O unit for input file.
   INTEGER, INTENT(OUT),OPTIONAL:: ErrStat                                         ! Error status; if present, program does not abort on error

   CHARACTER(*), INTENT(OUT)    :: CharVar                                         ! Integer variable being read.
   CHARACTER(*), INTENT(IN)     :: Fil                                             ! Name of the input file.
   CHARACTER(*), INTENT(IN)     :: VarDescr                                        ! Text string describing the variable.
   CHARACTER(*), INTENT(IN)     :: VarName                                         ! Text string containing the variable name.


      ! Local declarations:

   INTEGER                      :: IOS                                             ! I/O status returned from the read statement.

   CHARACTER(35)                :: Frmt = "( 15X, A, T30, ' - ', A, /, 2X, A )"    ! Output format for string parameters.




   READ (UnIn,'(A)',IOSTAT=IOS)  CharVar

   CALL CheckIOS ( IOS, Fil, VarName, StrType )

   IF ( PRESENT(ErrStat) ) THEN
      ErrStat = IOS
      IF (IOS /= 0) RETURN
   END IF

   IF ( Echo )  THEN
      WRITE (UnEc,Frmt)  VarName, VarDescr, '"'//TRIM( CharVar )//'"'
   END IF


   RETURN
   END SUBROUTINE ReadStr ! ( UnIn, Fil, CharVar, VarName, VarDescr [, ErrStat] )
!=======================================================================
   SUBROUTINE WaitTime ( WaitSecs )


      ! This routine writes out a prompt to the screen without
      ! following it with a new line, though a new line precedes it.


   IMPLICIT NONE


      ! Argument declarations:

   REAL(ReKi), INTENT(IN)       :: WaitSecs                                        ! The number of seconds to wait.


      ! Local declarations:

   REAL(ReKi)                   :: EndCounts                                       ! The number of counts when wait time is over.

   INTEGER                      :: Counts                                          ! Current number of counts on the system clock.
   INTEGER                      :: CountMax                                        ! Maximum number of counts possible on the system clock.
   INTEGER                      :: CountRate                                       ! Number of counts per second on the system clock.



   CALL SYSTEM_CLOCK ( Counts, CountRate, CountMax )
   EndCounts = Counts + INT( WaitSecs*CountRate )

   DO
      CALL SYSTEM_CLOCK ( Counts, CountRate, CountMax )
      IF ( Counts > EndCounts )  EXIT
   END DO


   RETURN
   END SUBROUTINE WaitTime ! ( Seconds )
!=======================================================================
   SUBROUTINE WrPr ( Str )


      ! This routine writes out a prompt to the screen without
      ! following it with a new line, though a new line precedes it.


      ! Argument declarations:

   CHARACTER(*), INTENT(IN)     :: Str                                          ! The prompt string to print.



   CALL WrScr ( ' ' )
   CALL WrNR  ( TRIM( Str )//' > ' )


   RETURN
   END SUBROUTINE WrPr ! ( Str )
!=======================================================================
   SUBROUTINE WrFileNR ( Unit, Str )


      ! This routine writes out a string to the file connected to Unit without following it with a new line.


      ! Argument declarations.

   INTEGER, INTENT(IN)          :: Unit                                         ! I/O unit for input file.

   CHARACTER(*), INTENT(IN)     :: Str                                          ! String to be written without a newline at the end.



   WRITE (Unit,'(A)',ADVANCE='NO')  Str


   RETURN
   END SUBROUTINE WrFileNR ! ( Unit, Str )
!=======================================================================
   SUBROUTINE WrML ( Str )


      ! This routine writes out a string in the middle of a line.


      ! Argument declarations.

   CHARACTER(*)                 :: Str



   CALL WrNR ( Str )


   RETURN
   END SUBROUTINE WrML ! ( Str )
!=======================================================================
   SUBROUTINE WrScr1 ( Str )


      ! This routine writes out a string to the screen after a blank line.


      ! Argument declarations.

   CHARACTER(*)                 :: Str                                         ! The string to print.



   CALL WrScr ( ' ' )
   CALL WrScr ( TRIM( Str ) )


   RETURN
   END SUBROUTINE WrScr1 ! ( Str )
!=======================================================================

END MODULE NWTC_IO
MODULE NWTC_Num


   ! This module contains numeric-type routines with non-system-specific logic and references.


   ! It contains the following routines:

   !     SUBROUTINE AddOrSub2Pi   ( OldAngle, NewAngle )
   !     SUBROUTINE BSortReal     ( RealAry, NumPts )
   !     FUNCTION   CROSS_PRODUCT ( Vector1, Vector2 )
   !     SUBROUTINE GL_Pts        ( IPt, NPts, Loc, Wt [, ErrStat] )
   !     FUNCTION   IndexCharAry  ( CVal, CAry )
   !     FUNCTION   InterpBin     ( XVal, XAry, YAry, ILo, AryLen )             ! Generic interface for InterpBinComp and InterpBinReal.
   !     FUNCTION   InterpBinComp ( XVal, XAry, YAry, ILo, AryLen )
   !     FUNCTION   InterpBinReal ( XVal, XAry, YAry, ILo, AryLen )
   !     FUNCTION   InterpStp     ( XVal, XAry, YAry, ILo, AryLen )             ! Generic interface for InterpStpComp and InterpStpReal.
   !     FUNCTION   InterpStpComp ( XVal, XAry, YAry, Ind, AryLen )
   !     FUNCTION   InterpStpReal ( XVal, XAry, YAry, Ind, AryLen )
   !     SUBROUTINE LocateStp     ( XVal, XAry, Ind, AryLen )
   !     FUNCTION   Mean          ( Ary, AryLen )                               ! Function to calculate the mean value of a vector array.
   !     SUBROUTINE MPi2Pi        ( Angle )
   !     SUBROUTINE PiConsts
   !     SUBROUTINE SmllRotTrans  ( RotationType, Theta1, Theta2, Theta3, TransMat )
   !     SUBROUTINE SortUnion     ( Ary1, N1, Ary2, N2, Ary, N )
   !     FUNCTION   StdDevFn      ( Ary, AryLen, Mean )                         ! Function to calculate the standard deviation of a vector array.


   USE                             NWTC_IO


!=======================================================================


      ! Global numeric-related variables.

   REAL(DbKi)                   :: D2R_D                                        ! Factor to convert degrees to radians in double precision.
   REAL(DbKi)                   :: Pi_D                                         ! Ratio of a circle's circumference to its diameter in double precision.
   REAL(DbKi)                   :: PiBy2_D                                      ! Pi/2 in double precision.
   REAL(DbKi)                   :: R2D_D                                        ! Factor to convert radians to degrees in double precision.
   REAL(DbKi)                   :: RPM2RPS_D                                    ! Factor to convert revolutions per minute to radians per second in double precision.
   REAL(DbKi)                   :: RPS2RPM_D                                    ! Factor to convert radians per second to revolutions per minute in double precision.
   REAL(DbKi)                   :: TwoByPi_D                                    ! 2/Pi in double precision.
   REAL(DbKi)                   :: TwoPi_D                                      ! 2*Pi in double precision.

   REAL(ReKi)                   :: D2R                                          ! Factor to convert degrees to radians.
   REAL(ReKi)                   :: Pi                                           ! Ratio of a circle's circumference to its diameter.
   REAL(ReKi)                   :: PiBy2                                        ! Pi/2.
   REAL(ReKi)                   :: R2D                                          ! Factor to convert radians to degrees.
   REAL(ReKi)                   :: RPM2RPS                                      ! Factor to convert revolutions per minute to radians per second.
   REAL(ReKi)                   :: RPS2RPM                                      ! Factor to convert radians per second to revolutions per minute.
   REAL(ReKi)                   :: TwoByPi                                      ! 2/Pi.
   REAL(ReKi)                   :: TwoPi                                        ! 2*Pi.

   INTEGER, ALLOCATABLE         :: IntIndx  (:,:)                               ! The array of indices holding that last index used for interpolation in IntBlade().


!=======================================================================


      ! Create interface for a generic InterpBin that actually uses specific routines.

   INTERFACE InterpBin
      MODULE PROCEDURE InterpBinComp
      MODULE PROCEDURE InterpBinReal
   END INTERFACE


      ! Create interface for a generic InterpStp that actually uses specific routines.

   INTERFACE InterpStp
      MODULE PROCEDURE InterpStpComp
      MODULE PROCEDURE InterpStpReal
   END INTERFACE


CONTAINS

!=======================================================================
   SUBROUTINE AddOrSub2Pi ( OldAngle, NewAngle )


      ! This routine is used to convert NewAngle to an angle within 2*Pi of
      !   OldAngle by adding or subtracting 2*Pi accordingly; it then sets
      !   OldAngle equal to NewAngle.  This routine is useful for converting
      !   angles returned from a call to the ATAN2() FUNCTION into angles that may
      !   exceed the -Pi to Pi limit of ATAN2().  For example, if the nacelle yaw
      !   angle was 179deg in the previous time step and the yaw angle increased
      !   by 2deg in the new time step, we want the new yaw angle returned from a
      !   call to the ATAN2() FUNCTION to be 181deg instead of -179deg.  This
      !   routine assumes that the angle change between calls is not more than
      !   2*Pi in absolute value.  OldAngle should be SAVEd in the calling
      !   routine.


      ! Argument declarations:

   REAL(ReKi), INTENT(INOUT)    :: OldAngle                                     ! Angle from which NewAngle will be converted to within 2*Pi of, rad.
   REAL(ReKi), INTENT(INOUT)    :: NewAngle                                     ! Angle to be converted to within 2*Pi of OldAngle, rad.


      ! Local declarations:

   REAL(ReKi)                   :: DelAngle                                     ! The difference between OldAngle and NewAngle, rad.



      ! Add or subtract 2*Pi in order to convert NewAngle two within 2*Pi of
      !   OldAngle:

   DelAngle = OldAngle - NewAngle

   DO WHILE ( ABS( DelAngle ) >= TwoPi )

      NewAngle = NewAngle + SIGN( TwoPi, DelAngle )
      DelAngle = OldAngle - NewAngle

   END DO


      ! Set OldAngle to equal NewAngle:

   OldAngle = NewAngle



   RETURN
   END SUBROUTINE AddOrSub2Pi
!=======================================================================
   SUBROUTINE BSortReal ( RealAry, NumPts )


      ! This routine sorts a list of real numbers.  It uses the buble sort algorithm,
      ! which is only suitable for short lists.


      ! Argument declarations:

   INTEGER, INTENT(IN)          :: NumPts                                       ! The length of the list to be sorted.

   REAL(ReKi), INTENT(INOUT)    :: RealAry(NumPts)                              ! The list of real numbers to be sorted.


      ! Local declarations:

   REAL(ReKi)                   :: Temp                                         ! Temporary variable to hold the current element.

   INTEGER                      :: I                                            ! Index into the array.

   LOGICAL                      :: Change                                       ! Flag to indicate if a change of order was made.


      ! Sort the list

   Change = .TRUE.

   DO WHILE ( Change )

      Change = .FALSE.

      DO I=2,NumPts
         IF ( RealAry(I) < RealAry(I-1) )  THEN
            Temp           = RealAry(I)
            RealAry(I)   = RealAry(I-1)
            RealAry(I-1) = Temp
            Change         = .TRUE.
         END IF
      END DO ! I

   END DO ! WHILE


   RETURN
   END SUBROUTINE BSortReal ! ( RealAry, NumPts )
!=======================================================================
   FUNCTION Cross_Product(Vector1, Vector2)

      ! This function computes the cross product of two 3-element arrays:
      ! Cross_Product = Vector1 X Vector2 (resulting in a vector)


      ! Argument declarations.

   REAL(ReKi), INTENT(IN )         :: Vector1       (3)
   REAL(ReKi), INTENT(IN )         :: Vector2       (3)

      ! Function definition
   REAL(ReKi)                      :: Cross_Product (3)        ! = Vector1 X Vector2 (resulting in a vector)


   Cross_Product(1) = Vector1(2)*Vector2(3) - Vector1(3)*Vector2(2)
   Cross_Product(2) = Vector1(3)*Vector2(1) - Vector1(1)*Vector2(3)
   Cross_Product(3) = Vector1(1)*Vector2(2) - Vector1(2)*Vector2(1)


   RETURN
   END FUNCTION Cross_Product
!=======================================================================
   SUBROUTINE GL_Pts ( IPt, NPts, Loc, Wt, ErrStat )

      ! This funtion returns the non-dimensional (-1:+1) location of the given Gauss-Legendre Quadrature point and its weight.
      ! The values came from Carnahan, Brice; Luther, H.A.; Wilkes, James O.  (1969)  "Applied Numerical Methods."


      ! Argument declarations.

   REAL(ReKi)                     :: Loc                                         ! The location of the specified point.
   REAL(ReKi)                     :: Wt                                          ! The weight for the specified point.

   INTEGER, INTENT(OUT), OPTIONAL :: ErrStat                                     ! Error status; if present, program does not abort on error
   INTEGER, INTENT(INOUT)         :: IPt                                         ! The quadrature point in question.
   INTEGER, INTENT(INOUT)         :: NPts                                        ! The number of points used in the quadrature.


   IF ( PRESENT(ErrStat) ) ErrStat = 0


      ! Check to see if the number of points and the specific point are valid values.

   IF ( ( NPts < 1 ) .OR. ( NPts > 6 ) )  THEN
      CALL ProgAbort ( ' In function GL_Loc, the number of points used for Gauss-Legendre Quadrature must be between 1 and 6' &
                    //' (inclusive).  Instead, it is "'//TRIM( Int2LStr( NPts ) )//'".', PRESENT(ErrStat) )
      IF ( PRESENT(ErrStat) ) THEN ! this should always be true here
         ErrStat = 1
         RETURN
      END IF
   END IF

   IF ( ( Ipt < 1 ) .OR. ( Ipt > NPts ) )  THEN
      CALL ProgAbort ( ' In function GL_Loc, the point being used for Gauss-Legendre Quadrature must be between 1 and ' &
                   //TRIM( Int2LStr( NPts ) )//' (inclusive).  Instead, it is "'//TRIM( Int2LStr( Ipt ) )//'".', PRESENT(ErrStat) )
      IF ( PRESENT(ErrStat) ) THEN
         ErrStat = 1
         RETURN
      END IF
   END IF


      ! Set the location and weight of the point.

   SELECT CASE ( NPts )
      CASE ( 1 )                         ! Case 1 is really just rectangular integration.
         Loc = 0.0
         Wt  = 2.0
      CASE ( 2 )
         SELECT CASE ( Ipt )
            CASE ( 1 )
               Loc = -0.5773503
               Wt  =  1.0
            CASE ( 2 )
               Loc = 0.5773503
               Wt  = 1.0
          END SELECT ! Ipt
      CASE ( 3 )
         SELECT CASE ( Ipt )
            CASE ( 1 )
               Loc = -0.7745967
               Wt  =  0.5555556
            CASE ( 2 )
               Loc =  0.0
               Wt  =  0.8888889
            CASE ( 3 )
               Loc =  0.7745967
               Wt  =  0.5555556
         END SELECT ! Ipt
      CASE ( 4 )
         SELECT CASE ( Ipt )
            CASE ( 1 )
               Loc = -0.8611363
               Wt  =  0.3478548
            CASE ( 2 )
               Loc = -0.3399810
               Wt  =  0.6521452
            CASE ( 3 )
               Loc =  0.3399810
               Wt  =  0.6521452
            CASE ( 4 )
               Loc =  0.8611363
               Wt  =  0.3478548
         END SELECT ! Ipt
      CASE ( 5 )
         SELECT CASE ( Ipt )
            CASE ( 1 )
               Loc = -0.9061798
               Wt  =  0.2369269
            CASE ( 2 )
               Loc = -0.5384693
               Wt  =  0.4786287
            CASE ( 3 )
               Loc =  0.0
               Wt  =  0.5688889
            CASE ( 4 )
               Loc =  0.5384693
               Wt  =  0.4786287
            CASE ( 5 )
               Loc =  0.9061798
               Wt  =  0.2369269
         END SELECT ! Ipt
      CASE ( 6 )
         SELECT CASE ( Ipt )
            CASE ( 1 )
               Loc = -0.9324695
               Wt  =  0.1713245
            CASE ( 2 )
               Loc = -0.6612094
               Wt  =  0.3607616
            CASE ( 3 )
               Loc = -0.2386192
               Wt  =  0.4679139
            CASE ( 4 )
               Loc =  0.2386192
               Wt  =  0.4679139
            CASE ( 5 )
               Loc =  0.6612094
               Wt  =  0.3607616
            CASE ( 6 )
               Loc =  0.9324695
               Wt  =  0.1713245
         END SELECT ! Ipt
   END SELECT ! Npts

   RETURN
   END SUBROUTINE GL_Pts ! ( IPt, NPts, Loc, Wt [, ErrStat] )
!=======================================================================
   FUNCTION IndexCharAry( CVal, CAry )


      ! This funtion returns an integer index such that CAry(IndexCharAry) = CVal. If
      ! no element in the array matches CVal, the value -1 is returned.  The routine
      ! performs a binary search on the input array to determine if CVal is an
      ! element of the array; thus, CAry must be sorted and stored in increasing
      ! alphebetical (ASCII) order. The routine does not check that the array is
      ! sorted.  The routine assumes that CVal is type CHARACTER and CAry
      ! is an array of CHARACTERS.


      ! Function declaration.


   INTEGER                      :: IndexCharAry                                   ! This function

      ! Argument declarations.

   CHARACTER(*), INTENT(IN)     :: CVal                                           ! String to find.
   CHARACTER(*), INTENT(IN)     :: CAry(:)                                        ! Array of strings to search.



      ! Local declarations.

   INTEGER                      :: IHi                                             ! The high index into the arrays.
   INTEGER                      :: IMid                                            ! The mid-point index between IHi and ILo.
   INTEGER                      :: ILo


      ! Initialize some variables

   ILo = 1
   IHi = SIZE(CAry)

   IndexCharAry = -1



      ! Let's search!

   DO WHILE ( IHi-ILo > 1 )

      IMid = ( IHi + ILo )/2

      IF( CVal > CAry(IMid) ) THEN
         ILo = IMid
      ELSEIF (CVal < CAry(IMid) ) THEN
         IHi = IMid
      ELSE !Found it
         IndexCharAry = IMid
         EXIT
      END IF

   END DO


   RETURN

   END FUNCTION IndexCharAry
!=======================================================================
   FUNCTION InterpBinComp( XVal, XAry, YAry, ILo, AryLen )


      ! This funtion returns a y-value that corresponds to an input x-value by interpolating into the arrays.
      ! It uses a binary interpolation scheme that takes about log(AryLen)/log(2) steps to converge.
      ! It returns the first or last YAry() value if XVal is outside the limits of XAry().
      ! This routine assumes YAry is COMPLEX.


      ! Function declaration.


   COMPLEX(ReKi)                :: InterpBinComp                                   ! This function.


      ! Argument declarations.

   INTEGER, INTENT(IN)          :: AryLen                                          ! Length of the arrays.
   INTEGER, INTENT(INOUT)       :: ILo                                             ! The low index into the arrays.

   REAL(ReKi), INTENT(IN)       :: XAry    (AryLen)                                ! Array of X values to be interpolated.
   REAL(ReKi), INTENT(IN)       :: XVal                                            ! X value to be interpolated.

   COMPLEX(ReKi), INTENT(IN)    :: YAry    (AryLen)                                ! Array of Y values to be interpolated.


      ! Local declarations.

   INTEGER                      :: IHi                                             ! The high index into the arrays.
   INTEGER                      :: IMid                                            ! The mid-point index between IHi and ILo.



      ! Let's check the limits first.

   IF ( XVal <= XAry(1) )  THEN
      InterpBinComp = YAry(1)
      ILo           = 1
      RETURN
   ELSE IF ( XVal >= XAry(AryLen) )  THEN
      InterpBinComp = YAry(AryLen)
      ILo           = AryLen - 1
      RETURN
   END IF


      ! Let's interpolate!

   ILo  = 1
   IHi  = AryLen

   DO WHILE ( IHi-ILo > 1 )

      IMid = ( IHi + ILo )/2

      IF ( XVal >= XAry(IMid) ) THEN
         ILo = IMid
      ELSE
         IHi = IMid
      END IF

   END DO

   InterpBinComp = YAry(ILo) + ( YAry(IHi) - YAry(ILo) )*( XVal - XAry(ILo) )/( XAry(IHi) - XAry(ILo) )


   RETURN
   END FUNCTION InterpBinComp ! ( XVal, XAry, YAry, ILo, AryLen )
!=======================================================================
   FUNCTION InterpBinReal( XVal, XAry, YAry, ILo, AryLen )


      ! This funtion returns a y-value that corresponds to an input x-value by interpolating into the arrays.
      ! It uses a binary interpolation scheme that takes about log(AryLen)/log(2) steps to converge.
      ! It returns the first or last YAry() value if XVal is outside the limits of XAry().
      ! This routine assumes YAry is REAL.


      ! Function declaration.


   REAL(ReKi)                   :: InterpBinReal                                   ! This function.


      ! Argument declarations.

   INTEGER, INTENT(IN)          :: AryLen                                          ! Length of the arrays.
   INTEGER, INTENT(INOUT)       :: ILo                                             ! The low index into the arrays.

   REAL(ReKi), INTENT(IN)       :: XAry    (AryLen)                                ! Array of X values to be interpolated.
   REAL(ReKi), INTENT(IN)       :: XVal                                            ! X value to be interpolated.
   REAL(ReKi), INTENT(IN)       :: YAry    (AryLen)                                ! Array of Y values to be interpolated.


      ! Local declarations.

   INTEGER                      :: IHi                                             ! The high index into the arrays.
   INTEGER                      :: IMid                                            ! The mid-point index between IHi and ILo.



      ! Let's check the limits first.

   IF ( XVal <= XAry(1) )  THEN
      InterpBinReal = YAry(1)
      ILo           = 1
      RETURN
   ELSE IF ( XVal >= XAry(AryLen) )  THEN
      InterpBinReal = YAry(AryLen)
      ILo           = AryLen - 1
      RETURN
   END IF


      ! Let's interpolate!

   ILo  = 1
   IHi  = AryLen

   DO WHILE ( IHi-ILo > 1 )

      IMid = ( IHi + ILo )/2

      IF ( XVal >= XAry(IMid) ) THEN
         ILo = IMid
      ELSE
         IHi = IMid
      END IF

   END DO

   InterpBinReal = YAry(ILo) + ( YAry(IHi) - YAry(ILo) )*( XVal - XAry(ILo) )/( XAry(IHi) - XAry(ILo) )


   RETURN
   END FUNCTION InterpBinReal ! ( XVal, XAry, YAry, ILo, AryLen )
!=======================================================================
   FUNCTION InterpStpComp( XVal, XAry, YAry, Ind, AryLen )


      ! This funtion returns a y-value that corresponds to an input x-value by interpolating into the arrays.
      ! It uses the passed index as the starting point and does a stepwise interpolation from there.  This is
      ! especially useful when the calling routines save the value from the last time this routine was called
      ! for a given case where XVal does not change much from call to call.  When there is no correlation
      ! from one interpolation to another, InterpBin() may be a better choice.
      ! It returns the first or last YAry() value if XVal is outside the limits of XAry().
      ! This routine assumes YAry is COMPLEX.


      ! Function declaration.


   COMPLEX(ReKi)                :: InterpStpComp                                   ! This function.


      ! Argument declarations.

   INTEGER, INTENT(IN)          :: AryLen                                          ! Length of the arrays.
   INTEGER, INTENT(INOUT)       :: Ind                                             ! Initial and final index into the arrays.

   REAL(ReKi), INTENT(IN)       :: XAry    (AryLen)                                ! Array of X values to be interpolated.
   REAL(ReKi), INTENT(IN)       :: XVal                                            ! X value to be interpolated.

   COMPLEX(ReKi), INTENT(IN)    :: YAry    (AryLen)                                ! Array of Y values to be interpolated.



      ! Let's check the limits first.

   IF ( XVal <= XAry(1) )  THEN
      InterpStpComp = YAry(1)
      Ind           = 1
      RETURN
   ELSE IF ( XVal >= XAry(AryLen) )  THEN
      InterpStpComp = YAry(AryLen)
      Ind           = AryLen - 1
      RETURN
   END IF


     ! Let's interpolate!

   Ind = MAX( MIN( Ind, AryLen-1 ), 1 )

   DO

      IF ( XVal < XAry(Ind) )  THEN

         Ind = Ind - 1

      ELSE IF ( XVal >= XAry(Ind+1) )  THEN

         Ind = Ind + 1

      ELSE

         InterpStpComp = ( YAry(Ind+1) - YAry(Ind) )*( XVal - XAry(Ind) )/( XAry(Ind+1) - XAry(Ind) ) + YAry(Ind)
         RETURN

      END IF

   END DO


   RETURN
   END FUNCTION InterpStpComp ! ( XVal, XAry, YAry, Ind, AryLen )
!=======================================================================
   FUNCTION InterpStpReal( XVal, XAry, YAry, Ind, AryLen )


      ! This funtion returns a y-value that corresponds to an input x-value by interpolating into the arrays.
      ! It uses the passed index as the starting point and does a stepwise interpolation from there.  This is
      ! especially useful when the calling routines save the value from the last time this routine was called
      ! for a given case where XVal does not change much from call to call.  When there is no correlation
      ! from one interpolation to another, InterpBin() may be a better choice.
      ! It returns the first or last YAry() value if XVal is outside the limits of XAry().
      ! This routine assumes YAry is REAL.


      ! Function declaration.

   REAL(ReKi)                   :: InterpStpReal                                   ! This function.


      ! Argument declarations.

   INTEGER, INTENT(IN)          :: AryLen                                          ! Length of the arrays.
   INTEGER, INTENT(INOUT)       :: Ind                                             ! Initial and final index into the arrays.

   REAL(ReKi), INTENT(IN)       :: XAry    (AryLen)                                ! Array of X values to be interpolated.
   REAL(ReKi), INTENT(IN)       :: XVal                                            ! X value to be interpolated.
   REAL(ReKi), INTENT(IN)       :: YAry    (AryLen)                                ! Array of Y values to be interpolated.



      ! Let's check the limits first.

   IF ( XVal <= XAry(1) )  THEN
      InterpStpReal = YAry(1)
      Ind           = 1
      RETURN
   ELSE IF ( XVal >= XAry(AryLen) )  THEN
      InterpStpReal = YAry(AryLen)
      Ind           = AryLen - 1
      RETURN
   END IF


     ! Let's interpolate!

   Ind = MAX( MIN( Ind, AryLen-1 ), 1 )

   DO

      IF ( XVal < XAry(Ind) )  THEN

         Ind = Ind - 1

      ELSE IF ( XVal >= XAry(Ind+1) )  THEN

         Ind = Ind + 1

      ELSE

         InterpStpReal = ( YAry(Ind+1) - YAry(Ind) )*( XVal - XAry(Ind) )/( XAry(Ind+1) - XAry(Ind) ) + YAry(Ind)
         RETURN

      END IF

   END DO


   RETURN
   END FUNCTION InterpStpReal ! ( XVal, XAry, YAry, Ind, AryLen )
!=======================================================================
   SUBROUTINE LocateBin( XVal, XAry, Ind, AryLen )

      ! This subroutine finds the lower-bound index of an input x-value located in an array.
      ! On return, Ind has a value such that
      !           XAry(Ind) <= XVal < XAry(Ind+1), with the exceptions that
      !             Ind = 0 when XVal < XAry(1), and
      !          Ind = AryLen when XAry(AryLen) <= XVal.
      !
      ! It uses a binary interpolation scheme that takes about log(AryLen)/log(2) steps to converge.
      ! If the index doesn't change much between calls, LocateStp() may be a better option.


      ! Argument declarations.

   INTEGER, INTENT(IN)          :: AryLen                                          ! Length of the array.
   INTEGER, INTENT(OUT)         :: Ind                                             ! Final (low) index into the array.

   REAL(ReKi), INTENT(IN)       :: XAry    (AryLen)                                ! Array of X values to be interpolated.
   REAL(ReKi), INTENT(IN)       :: XVal                                            ! X value to be interpolated.


      ! Local declarations.

   INTEGER                      :: IHi                                             ! The high index into the arrays.
   INTEGER                      :: IMid                                            ! The mid-point index between IHi and Ind.



      ! Let's check the limits first.

   IF ( XVal < XAry(1) )  THEN
      Ind = 0
   ELSE IF ( XVal >= XAry(AryLen) )  THEN
      Ind = AryLen
   ELSE
         ! Let's interpolate!

      Ind  = 1
      IHi  = AryLen

      DO WHILE ( IHi-Ind > 1 )

         IMid = ( IHi + Ind )/2

         IF ( XVal >= XAry(IMid) ) THEN
            Ind = IMid
         ELSE
            IHi = IMid
         END IF

      END DO

   END IF

   RETURN
   END SUBROUTINE LocateBin
!=======================================================================
   SUBROUTINE LocateStp( XVal, XAry, Ind, AryLen )

      ! This subroutine finds the lower-bound index of an input x-value located in an array.
      ! On return, Ind has a value such that
      !           XAry(Ind) <= XVal < XAry(Ind+1), with the exceptions that
      !             Ind = 0 when XVal < XAry(1), and
      !          Ind = AryLen when XAry(AryLen) <= XVal.
      !
      ! It uses the passed index as the starting point and does a stepwise search from there.  This is
      ! especially useful when the calling routines save the value from the last time this routine was called
      ! for a given case where XVal does not change much from call to call.  When there is no correlation
      ! from one interpolation to another, a binary search may be a better choice.



      ! Argument declarations.

   INTEGER, INTENT(IN)          :: AryLen                                          ! Length of the array.
   INTEGER, INTENT(INOUT)       :: Ind                                             ! Initial and final index into the array.

   REAL(ReKi), INTENT(IN)       :: XAry    (AryLen)                                ! Array of X values to be interpolated.
   REAL(ReKi), INTENT(IN)       :: XVal                                            ! X value to be interpolated.



      ! Let's check the limits first.

   IF ( XVal < XAry(1) )  THEN
      Ind = 0
   ELSE IF ( XVal >= XAry(AryLen) )  THEN
      Ind = AryLen
   ELSE

      Ind = MAX( MIN( Ind, AryLen-1 ), 1 )

      DO

         IF ( XVal < XAry(Ind) )  THEN

            Ind = Ind - 1

         ELSE IF ( XVal >= XAry(Ind+1) )  THEN

            Ind = Ind + 1

         ELSE

            RETURN

         END IF

      END DO


   END IF

   RETURN

   END SUBROUTINE LocateStp
!=======================================================================
   FUNCTION Mean ( Ary, AryLen )


      ! This routine calculates the mean value of an array.


      ! Function declaration.

   REAL(ReKi)                   :: Mean                                         ! This function.


      ! Argument declarations:

   INTEGER, INTENT(IN)          :: AryLen                                       ! Length of the array.

   REAL(ReKi), INTENT(IN)       :: Ary  (AryLen)                                ! Input array.


      ! Local declarations.

   INTEGER                      :: I                                            ! The index into the array.



   Mean = 0.0

   DO I=1,AryLen
      Mean = Mean + Ary(I)
   END DO ! I

   Mean = Mean/AryLen


   RETURN
   END FUNCTION Mean ! ( Ary, AryLen )
!=======================================================================
   SUBROUTINE MPi2Pi ( Angle )


      ! This routine ensures that Angle lies between -pi and pi.


      ! Argument declarations:

   REAL(ReKi), INTENT(INOUT)    :: Angle



      ! Get the angle between 0 and 2Pi.

   Angle = MODULO( Angle, TwoPi )


      ! Get the angle between -Pi and Pi.

   IF ( Angle > Pi )  THEN
      Angle = Angle - TwoPi
   END IF


   RETURN
   END SUBROUTINE MPi2Pi
!=======================================================================
   SUBROUTINE PiConsts


      ! This routine computes constants based upon Pi.


   Pi_D      = ACOS( -1.0_DbKi )
   D2R_D     = Pi_D/180.0_DbKi
   R2D_D     = 180.0_DbKi/Pi_D
   PiBy2_D   = Pi_D/2.0_DbKi
   RPM2RPS_D = Pi_D/30.0_DbKi
   RPS2RPM_D = 30.0_DbKi/Pi_D
   TwoByPi_D = 2.0_DbKi/Pi_D
   TwoPi_D   = 2.0_DbKi*Pi_D

   Pi      = ACOS( -1.0 )
   D2R     = Pi/180.0
   R2D     = 180.0/Pi
   PiBy2   = Pi/2.0
   RPM2RPS = Pi/30.0
   RPS2RPM = 30.0/Pi
   TwoByPi = 2.0/Pi
   TwoPi   = 2.0*Pi


   RETURN
   END SUBROUTINE PiConsts
!=======================================================================
   SUBROUTINE SmllRotTrans( RotationType, Theta1, Theta2, Theta3, TransMat )


      ! This routine computes the 3x3 transformation matrix, TransMat,
      !   to a coordinate system x (with orthogonal axes x1, x2, x3)
      !   resulting from three rotations (Theta1, Theta2, Theta3) about the
      !   orthogonal axes (X1, X2, X3) of coordinate system X.  All angles
      !   are assummed to be small, as such, the order of rotations does
      !   not matter and Euler angles do not need to be used.  This routine
      !   is used to compute the transformation matrix (TransMat) between
      !   undeflected (X) and deflected (x) coordinate systems.  In matrix
      !   form:
      !      {x1}   [TransMat(Theta1, ] {X1}
      !      {x2} = [         Theta2, ]*{X2}
      !      {x3}   [         Theta3 )] {X3}

      ! The transformation matrix, TransMat, is the closest orthonormal
      !   matrix to the nonorthonormal, but skew-symmetric, Bernoulli-Euler
      !   matrix:
      !          [   1.0    Theta3 -Theta2 ]
      !      A = [ -Theta3   1.0    Theta1 ]
      !          [  Theta2 -Theta1   1.0   ]
      !
      !   In the Frobenius Norm sense, the closest orthornormal matrix is:
      !      TransMat = U*V^T,
      !
      !   where the columns of U contain the eigenvectors of A*A^T and the
      !   columns of V contain the eigenvectors of A^T*A (^T = transpose).
      !   This result comes directly from the Singular Value Decomposition
      !   (SVD) of A = U*S*V^T where S is a diagonal matrix containing the
      !   singular values of A, which are SQRT( eigenvalues of A*A^T ) =
      !   SQRT( eigenvalues of A^T*A ).

      ! The algebraic form of the transformation matrix, as implemented
      !   below, was derived symbolically by J. Jonkman by computing U*V^T
      !   by hand with verification in Mathematica.



      ! Passed Variables:

   REAL(ReKi), INTENT(IN )      :: Theta1                                          ! The small rotation about X1, (rad).
   REAL(ReKi), INTENT(IN )      :: Theta2                                          ! The small rotation about X2, (rad).
   REAL(ReKi), INTENT(IN )      :: Theta3                                          ! The small rotation about X3, (rad).
   REAL(ReKi), INTENT(OUT)      :: TransMat (3,3)                                  ! The resulting transformation matrix from X to x, (-).

   CHARACTER(*), INTENT(IN)     :: RotationType                                    ! The type of rotation; used to inform the user where a large rotation is occuring upon such an event.


      ! Local Variables:

   REAL(ReKi)                   :: ComDenom                                        ! = ( Theta1^2 + Theta2^2 + Theta3^2 )*SQRT( 1.0 + Theta1^2 + Theta2^2 + Theta3^2 )
   REAL(ReKi), PARAMETER        :: LrgAngle  = 0.4                                 ! Threshold for when a small angle becomes large (about 23deg).  This comes from: COS(SmllAngle) ~ 1/SQRT( 1 + SmllAngle^2 ) and SIN(SmllAngle) ~ SmllAngle/SQRT( 1 + SmllAngle^2 ) results in ~5% error when SmllAngle = 0.4rad.
   REAL(ReKi)                   :: Theta11                                         ! = Theta1^2
   REAL(ReKi)                   :: Theta12S                                        ! = Theta1*Theta2*[ SQRT( 1.0 + Theta1^2 + Theta2^2 + Theta3^2 ) - 1.0 ]
   REAL(ReKi)                   :: Theta13S                                        ! = Theta1*Theta3*[ SQRT( 1.0 + Theta1^2 + Theta2^2 + Theta3^2 ) - 1.0 ]
   REAL(ReKi)                   :: Theta22                                         ! = Theta2^2
   REAL(ReKi)                   :: Theta23S                                        ! = Theta2*Theta3*[ SQRT( 1.0 + Theta1^2 + Theta2^2 + Theta3^2 ) - 1.0 ]
   REAL(ReKi)                   :: Theta33                                         ! = Theta3^2
   REAL(ReKi)                   :: SqrdSum                                         ! = Theta1^2 + Theta2^2 + Theta3^2
   REAL(ReKi)                   :: SQRT1SqrdSum                                    ! = SQRT( 1.0 + Theta1^2 + Theta2^2 + Theta3^2 )

   LOGICAL,    SAVE             :: FrstWarn  = .TRUE.                              ! When .TRUE., indicates that we're on the first warning.



      ! Display a warning message if at least one angle gets too large in
      !   magnitude:

   IF ( ( ( ABS(Theta1) > LrgAngle ) .OR. ( ABS(Theta2) > LrgAngle ) .OR. ( ABS(Theta3) > LrgAngle ) ) .AND. FrstWarn )  THEN

      CALL ProgWarn(' Small angle assumption violated in SUBROUTINE SmllRotTrans() due to'// &
                     ' a large '//TRIM(RotationType)//'. The solution may be inaccurate.'// &
                     ' Simulation continuing, but future warnings will be suppressed.')

      FrstWarn = .FALSE.   ! Don't enter here again!

   ENDIF



      ! Compute some intermediate results:

   Theta11      = Theta1*Theta1
   Theta22      = Theta2*Theta2
   Theta33      = Theta3*Theta3

   SqrdSum      = Theta11 + Theta22 + Theta33
   SQRT1SqrdSum = SQRT( 1.0 + SqrdSum )
   ComDenom     = SqrdSum*SQRT1SqrdSum

   Theta12S     = Theta1*Theta2*( SQRT1SqrdSum - 1.0 )
   Theta13S     = Theta1*Theta3*( SQRT1SqrdSum - 1.0 )
   Theta23S     = Theta2*Theta3*( SQRT1SqrdSum - 1.0 )


      ! Define the transformation matrix:

   IF ( ComDenom == 0.0 )  THEN  ! All angles are zero and matrix is ill-conditioned (the matrix is derived assuming that the angles are not zero); return identity

      TransMat(1,:) = (/ 1.0, 0.0, 0.0 /)
      TransMat(2,:) = (/ 0.0, 1.0, 0.0 /)
      TransMat(3,:) = (/ 0.0, 0.0, 1.0 /)

   ELSE                          ! At least one angle is nonzero

      TransMat(1,1) = ( Theta11*SQRT1SqrdSum + Theta22              + Theta33              )/ComDenom
      TransMat(2,2) = ( Theta11              + Theta22*SQRT1SqrdSum + Theta33              )/ComDenom
      TransMat(3,3) = ( Theta11              + Theta22              + Theta33*SQRT1SqrdSum )/ComDenom
      TransMat(1,2) = (  Theta3*SqrdSum + Theta12S )/ComDenom
      TransMat(2,1) = ( -Theta3*SqrdSum + Theta12S )/ComDenom
      TransMat(1,3) = ( -Theta2*SqrdSum + Theta13S )/ComDenom
      TransMat(3,1) = (  Theta2*SqrdSum + Theta13S )/ComDenom
      TransMat(2,3) = (  Theta1*SqrdSum + Theta23S )/ComDenom
      TransMat(3,2) = ( -Theta1*SqrdSum + Theta23S )/ComDenom

   ENDIF



   RETURN
   END SUBROUTINE SmllRotTrans
!=======================================================================
   SUBROUTINE SortUnion ( Ary1, N1, Ary2, N2, Ary, N )


      ! This routine takes two sorted arrays and finds the sorted union of the two.

      ! Note: If the same value is found in both arrays, only one is kept.  However, if either
      !       array as multiple occurances of the same value, the largest multiple will be
      !       kept.  Duplicates should be eliminated externally if this is not desirable.


      ! Argument declarations:

   INTEGER, INTENT(OUT)         :: N                                            ! The length of the output array.
   INTEGER, INTENT(IN)          :: N1                                           ! The length of the first input array.
   INTEGER, INTENT(IN)          :: N2                                           ! The length of the second input array.

   REAL(ReKi), INTENT(OUT)      :: Ary(N1+N2)                                   ! The sorted union.
   REAL(ReKi), INTENT(IN)       :: Ary1(N1)                                     ! The first list of sorted real numbers.
   REAL(ReKi), INTENT(IN)       :: Ary2(N2)                                     ! The second list of sorted real numbers.


      ! Local declarations:

   INTEGER                      :: I1                                           ! Index into the first array.
   INTEGER                      :: I2                                           ! Index into the second array.



   I1 = 1
   I2 = 1
   N  = 1

   DO WHILE ( ( I1 <= N1 ) .AND. ( I2 <= N2 ) )

      IF ( Ary1(I1) < Ary2(I2) )  THEN
         Ary(N) = Ary1(I1)
         I1 = I1 + 1
      ELSE IF ( Ary1(I1) > Ary2(I2) )  THEN
         Ary(N) = Ary2(I2)
         I2 = I2 + 1
      ELSE
         Ary(N) = Ary1(I1)
         I1 = I1 + 1
         I2 = I2 + 1
      END IF

      N  = N  + 1

   END DO ! WHILE


     ! We've reached the end of one array, but we need to add the end
     ! of the other array if we haven't reached the end of it yet.

   IF ( I1 <= N1 ) THEN
      Ary(N:N+N1-I1) = Ary1(I1:)
      N = N+N1-I1
   ELSEIF ( I2 <= N2 ) THEN
      Ary(N:N+N2-I2) = Ary2(I2:)
      N = N+N2-I2
   ELSE
      N = N - 1
   ENDIF


   RETURN
   END SUBROUTINE SortUnion ! ( Ary1, N1, Ary2, N2, Ary, N )
!=======================================================================
   FUNCTION StdDevFn ( Ary, AryLen, Mean )


      ! This routine calculates the standard deviation of a population contained in Ary.


      ! Function declaration.

   REAL(ReKi)                   :: StdDevFn                                     ! This function.


      ! Argument declarations:

   INTEGER, INTENT(IN)          :: AryLen                                       ! Length of the array.

   REAL(ReKi), INTENT(IN)       :: Ary  (AryLen)                                ! Input array.
   REAL(ReKi), INTENT(IN)       :: Mean                                         ! The previously calculated mean of the array.


      ! Local declarations.

   REAL(DbKi)                   :: Sum                                          ! A temporary sum.

   INTEGER                      :: I                                            ! The index into the array.



   Sum = 0.0_DbKi

   DO I=1,AryLen
      Sum = Sum + ( Ary(I) - Mean )**2
   END DO ! I

   StdDevFn = SQRT( Sum/( AryLen - 1 ) )


   RETURN
   END FUNCTION StdDevFn ! ( Ary, AryLen, Mean )
!=======================================================================

END MODULE NWTC_Num
MODULE NWTC_Aero


   ! This module contains aerodynamics routines with non-system-specific logic and references.


   ! It contains the following routines:

   !     SUBROUTINE AeroInt  ( ISeg, Alpha, Re, AF_Table, IntData, DoCl, DoCd, DoCm, DoCpmin [, ErrStat] )
   !     SUBROUTINE CompDR   ( NumSeg, RLoc, HubRad, RotorRad, DimenInp, DelRLoc [, ErrStat] )
   !     SUBROUTINE GetAF    ( AF_File, AF_Table, ISeg )
   !     FUNCTION   GetCoef  ( ISeg, Alpha, AlfaTab, CoefTab, NumRows, Ind [, ErrStat] )
   !     SUBROUTINE GetCoefs ( ISeg, Alpha, Re, AF_Table, ClInt, CdInt, CmInt, CpminInt, DoCl, DoCd, DoCm, DoCpmin [, ErrStat] )
   !     SUBROUTINE InterpAF ( Ratio, InTable1, InTable2, OutTable )


   USE                             NWTC_IO
   USE                             NWTC_Num


!=======================================================================


      ! Global aerodynamics-related variables.

   TYPE                            :: AeroData                                  ! Declare new type that holds the interpolated aero data from the big tables.
      REAL(ReKi)                   :: AlfaStal                                  ! The stall AoA for this table.
      REAL(ReKi)                   :: AOD                                       ! The AoA for minimum CD.
      REAL(ReKi)                   :: AOL                                       ! The zero-lift AoA.
      REAL(ReKi)                   :: Cd0                                       ! The minimum Cd value.
      REAL(ReKi)                   :: CnA                                       ! The Cn slope for zero-lift.
      REAL(ReKi)                   :: CnS                                       ! The Cn at stall value for positive AoA.
      REAL(ReKi)                   :: CnSL                                      ! Cn at stall value for negative AoA.
      REAL(ReKi)                   :: Cl                                        ! The lift coefficient.
      REAL(ReKi)                   :: Cd                                        ! The drag coefficient.
      REAL(ReKi)                   :: Cm                                        ! The pitching-moment coefficient.
      REAL(ReKi)                   :: Cpmin                                     ! The minimum pressure coefficient.
      REAL(ReKi)                   :: FTB                                       ! The normal-coefficient divided by the Cn slope at zero lift.
      REAL(ReKi)                   :: FTBC                                      ! The chordwise-coefficient divided by the Cn slope at zero lift.
   ENDTYPE AeroData

   TYPE                            :: AeroTable                                 ! Declare new type that is an allocatable table of data.
      REAL(ReKi)                   :: AlfaStal                                  ! The stall AoA for this table.
      REAL(ReKi)                   :: AOD                                       ! The AoA for minimum CD.
      REAL(ReKi)                   :: AOL                                       ! The zero-lift AoA.
      REAL(ReKi)                   :: Cd0                                       ! The minimum Cd value.
      REAL(ReKi)                   :: CnA                                       ! The Cn slope for zero-lift.
      REAL(ReKi)                   :: CnS                                       ! The Cn at stall value for positive AoA.
      REAL(ReKi)                   :: CnSL                                      ! Cn at stall value for negative AoA.
      REAL(ReKi)                   :: Re                                        ! The Re for this table.
      REAL(ReKi)                   :: Ctrl                                      ! The control setting for this table.
      INTEGER                      :: Ind      = 0                              ! Last-used index into table.  Zero at beginning.
      INTEGER                      :: NumAlf                                    ! Number of angles of attack in the table.
      REAL(ReKi), ALLOCATABLE      :: Alpha    (:)                              ! The angle of attack vector.
      REAL(ReKi), ALLOCATABLE      :: Cl       (:)                              ! The lift-coefficient vector.
      REAL(ReKi), ALLOCATABLE      :: Cd       (:)                              ! The drag-coefficient vector.
      REAL(ReKi), ALLOCATABLE      :: Cm       (:)                              ! The pitching-moment-coefficient vector.
      REAL(ReKi), ALLOCATABLE      :: Cpmin    (:)                              ! The minimum-pressure-coefficient vector.
      REAL(ReKi), ALLOCATABLE      :: FTB      (:)                              ! The normal-coefficient divided by the Cn slope at zero lift.
      REAL(ReKi), ALLOCATABLE      :: FTBC     (:)                              ! The chordwise-coefficient divided by the Cn slope at zero lift.
   ENDTYPE AeroTable

   TYPE                            :: AlfIndx                                   ! Declare new type that is an allocatable table of alpha indices.
      INTEGER                      :: NumBld                                    ! Number of blades in the table.
      INTEGER                      :: NumElm                                    ! Number of segments in the table.
      INTEGER, ALLOCATABLE         :: Ind      (:,:)                            ! The tables in this supertable.
   ENDTYPE AlfIndx

   TYPE                            :: ElmTable                                  ! Declare new type that is an allocatable table of data.
      INTEGER                      :: NumTabs                                   ! Number of tables in the supertable for an element.
      TYPE(AeroTable), ALLOCATABLE :: Tab      (:)                              ! The tables in this supertable.
   ENDTYPE ElmTable

   LOGICAL                         :: UseCm    = .FALSE.                        ! Flag to tell if there are Cm data in the airfoil files.
   LOGICAL                         :: UseCpmin = .FALSE.                        ! Flag to tell if there are Cp,min data in the airfoil files.


CONTAINS

!=======================================================================
   SUBROUTINE AeroInt ( ISeg, Alpha, Re, AF_Table, IntData, DoCl, DoCd, DoCm, DoCpmin, ErrStat )

      ! This routine finds the Re-bounding tables and then calls GetCoef() to get the
      ! desired coefficients for the two tables and then interpolates between them.

!NOTE: This routine needs to be modified to account for various control settings.  mlb  1-May-2010

      ! Argument declarations.

   REAL(ReKi), INTENT(IN)            :: Alpha                                   ! Angle of attack to get the coefficient for.
   REAL(ReKi), INTENT(IN)            :: Re                                      ! Reynolds number.

   INTEGER, INTENT(OUT), OPTIONAL    :: ErrStat                                 ! Error status; if present, program does not abort on error
   INTEGER, INTENT(IN)               :: ISeg                                    ! The current segment.

   LOGICAL, INTENT(IN)               :: DoCd                                    ! Get Cd.
   LOGICAL, INTENT(IN)               :: DoCl                                    ! Get Cl.
   LOGICAL, INTENT(IN)               :: DoCm                                    ! Get Cm.
   LOGICAL, INTENT(IN)               :: DoCpmin                                 ! Get Cp,min.

   TYPE (ElmTable), INTENT(INOUT)    :: AF_Table                                ! The table of airfoil data for the current segment.
   TYPE (AeroData), INTENT(OUT)      :: IntData                                 ! The interpolated airfoil data for the current segment.


      ! Local declarations.

   REAL(ReKi)                        :: CdHi                                    ! The drag coefficient for the higher Re.
   REAL(ReKi)                        :: ClHi                                    ! The lift coefficient for the higher Re.
   REAL(ReKi)                        :: CmHi                                    ! The pitching-moment coefficient for the higher Re.
   REAL(ReKi)                        :: CpminHi                                 ! The minimum pressure coefficient for the higher Re.
   REAL(ReKi)                        :: Fract                                   ! The fractional distance between tables.

   INTEGER                           :: ITab                                    ! An index for table number.
   INTEGER                           :: ITabLo                                  ! The table number that is the lower bound for Re.
   INTEGER                           :: ITabHi                                  ! The table number that is the lower bound for Re.

   LOGICAL                           :: OneTable                                ! Flag that tells if we need to read only one table (no interpolation).



      ! Find the bounding tables (if multiple) for this Re.  If there is only one table
      ! or if we are outside the range of tables, we won't need to interpolate.

   IF ( Re <= AF_Table%Tab(1)%Re )  THEN
      ITabLo   = 1
      OneTable = .TRUE.
   ELSE IF ( Re >= AF_Table%Tab(AF_Table%NumTabs)%Re )  THEN
      ITabLo   = AF_Table%NumTabs
      OneTable = .TRUE.
   ELSE IF ( AF_Table%NumTabs > 1 )  THEN
      DO ITab=1,AF_Table%NumTabs-1
         IF ( Re <= AF_Table%Tab(ITab+1)%Re )  THEN
            ITabLo = ITab
            ITabHi = ITab + 1
            EXIT
         END IF
      END DO
      OneTable = .FALSE.
   ELSE
      ITabLo   = 1
      OneTable = .TRUE.
   END IF


      ! Get the coefficients for ITabLo.

   IF ( DoCl )  THEN
      IF ( PRESENT( ErrStat ) ) THEN
         IntData%Cl = GetCoef( ISeg, Alpha, AF_Table%Tab(ITabLo)%Alpha, AF_Table%Tab(ITabLo)%Cl, AF_Table%Tab(ITabLo)%NumAlf &
                           , AF_Table%Tab(ITabLo)%Ind, ErrStat )
         IF (ErrStat /= 0) RETURN
      ELSE
         IntData%Cl = GetCoef( ISeg, Alpha, AF_Table%Tab(ITabLo)%Alpha, AF_Table%Tab(ITabLo)%Cl, AF_Table%Tab(ITabLo)%NumAlf &
                           , AF_Table%Tab(ITabLo)%Ind )
      END IF
   END IF

   IF ( DoCd )  THEN
      IF ( PRESENT( ErrStat ) ) THEN
         IntData%Cd  = GetCoef( ISeg, Alpha, AF_Table%Tab(ITabLo)%Alpha, AF_Table%Tab(ITabLo)%Cd, AF_Table%Tab(ITabLo)%NumAlf &
                              , AF_Table%Tab(ITabLo)%Ind, ErrStat )
         IF (ErrStat /= 0) RETURN
      ELSE
         IntData%Cd  = GetCoef( ISeg, Alpha, AF_Table%Tab(ITabLo)%Alpha, AF_Table%Tab(ITabLo)%Cd, AF_Table%Tab(ITabLo)%NumAlf &
                              , AF_Table%Tab(ITabLo)%Ind )
      END IF
   END IF

   IF ( DoCm )  THEN
      IF ( PRESENT( ErrStat ) ) THEN
         IntData%Cm = GetCoef( ISeg, Alpha, AF_Table%Tab(ITabLo)%Alpha, AF_Table%Tab(ITabLo)%Cm, AF_Table%Tab(ITabLo)%NumAlf &
                             , AF_Table%Tab(ITabLo)%Ind, ErrStat )
         IF (ErrStat /= 0) RETURN
      ELSE
         IntData%Cm = GetCoef( ISeg, Alpha, AF_Table%Tab(ITabLo)%Alpha, AF_Table%Tab(ITabLo)%Cm, AF_Table%Tab(ITabLo)%NumAlf &
                             , AF_Table%Tab(ITabLo)%Ind )
      END IF
   END IF

   IF ( DoCpmin )  THEN
      IF ( PRESENT( ErrStat ) ) THEN
         IntData%Cpmin = GetCoef( ISeg, Alpha, AF_Table%Tab(ITabLo)%Alpha, AF_Table%Tab(ITabLo)%Cpmin, AF_Table%Tab(ITabLo)%NumAlf &
                                , AF_Table%Tab(ITabLo)%Ind, ErrStat )
         IF (ErrStat /= 0) RETURN
      ELSE
         IntData%Cpmin = GetCoef( ISeg, Alpha, AF_Table%Tab(ITabLo)%Alpha, AF_Table%Tab(ITabLo)%Cpmin, AF_Table%Tab(ITabLo)%NumAlf &
                                , AF_Table%Tab(ITabLo)%Ind )
      END IF
   END IF

   IntData%AlfaStal = AF_Table%Tab(ITabLo)%AlfaStal
   IntData%AOD      = AF_Table%Tab(ITabLo)%AOD
   IntData%AOL      = AF_Table%Tab(ITabLo)%AOL
   IntData%Cd0      = AF_Table%Tab(ITabLo)%Cd0
   IntData%CnA      = AF_Table%Tab(ITabLo)%CnA
   IntData%CnS      = AF_Table%Tab(ITabLo)%CnS
   IntData%CnSL     = AF_Table%Tab(ITabLo)%CnSL


      ! If we don't need to interpolate, we don't need to make a second call and we are done.

   IF ( OneTable )  RETURN


      ! Get the coefficients for ITabHi.  Use step-wise interpolation for all but the first coefficient called.

   Fract = ( Re - AF_Table%Tab(ITabLo)%Re )/( AF_Table%Tab(ITabHi)%Re - AF_Table%Tab(ITabLo)%Re )


   IF ( DoCl )  THEN
      IF ( PRESENT( ErrStat ) ) THEN
         ClHi = GetCoef( ISeg, Alpha, AF_Table%Tab(ITabHi)%Alpha, AF_Table%Tab(ITabHi)%Cl, AF_Table%Tab(ITabHi)%NumAlf, &
                                      AF_Table%Tab(ITabHi)%Ind, ErrStat )
         IF (ErrStat /= 0) RETURN
      ELSE
         ClHi = GetCoef( ISeg, Alpha, AF_Table%Tab(ITabHi)%Alpha, AF_Table%Tab(ITabHi)%Cl, AF_Table%Tab(ITabHi)%NumAlf, &
                                      AF_Table%Tab(ITabHi)%Ind )
      END IF
      IntData%Cl  = IntData%Cl + Fract*( ClHi - IntData%Cl )
   END IF

   IF ( DoCd )  THEN
      IF ( PRESENT( ErrStat ) ) THEN
         CdHi = GetCoef( ISeg, Alpha, AF_Table%Tab(ITabHi)%Alpha, AF_Table%Tab(ITabHi)%Cd, AF_Table%Tab(ITabHi)%NumAlf, &
                                      AF_Table%Tab(ITabHi)%Ind, ErrStat )
         IF (ErrStat /= 0) RETURN
      ELSE
         CdHi = GetCoef( ISeg, Alpha, AF_Table%Tab(ITabHi)%Alpha, AF_Table%Tab(ITabHi)%Cd, AF_Table%Tab(ITabHi)%NumAlf, &
                                      AF_Table%Tab(ITabHi)%Ind )
      END IF
      IntData%Cd = IntData%Cd + Fract*( CdHi - IntData%Cd )
   END IF

   IF ( DoCm )  THEN
      IF ( PRESENT( ErrStat ) ) THEN
         CmHi = GetCoef( ISeg, Alpha, AF_Table%Tab(ITabHi)%Alpha, AF_Table%Tab(ITabHi)%Cm, AF_Table%Tab(ITabHi)%NumAlf, &
                                      AF_Table%Tab(ITabHi)%Ind, ErrStat )
         IF (ErrStat /= 0) RETURN
      ELSE
         CmHi = GetCoef( ISeg, Alpha, AF_Table%Tab(ITabHi)%Alpha, AF_Table%Tab(ITabHi)%Cm, AF_Table%Tab(ITabHi)%NumAlf, &
                                      AF_Table%Tab(ITabHi)%Ind )
      END IF
      IntData%Cm = IntData%Cm + Fract*( CmHi - IntData%Cm )
   END IF

   IF ( DoCpmin )  THEN
      IF ( PRESENT( ErrStat ) ) THEN
         CpminHi = GetCoef( ISeg, Alpha, AF_Table%Tab(ITabHi)%Alpha, AF_Table%Tab(ITabHi)%Cpmin, AF_Table%Tab(ITabHi)%NumAlf, &
                                         AF_Table%Tab(ITabHi)%Ind, ErrStat )
         IF (ErrStat /= 0) RETURN
      ELSE
         CpminHi = GetCoef( ISeg, Alpha, AF_Table%Tab(ITabHi)%Alpha, AF_Table%Tab(ITabHi)%Cpmin, AF_Table%Tab(ITabHi)%NumAlf, &
                                         AF_Table%Tab(ITabHi)%Ind )
      END IF
      IntData%Cpmin = IntData%Cpmin + Fract*( CpminHi - IntData%Cpmin )
   END IF

   IntData%AlfaStal = IntData%AlfaStal + Fract*( AF_Table%Tab(ITabHi)%AlfaStal - IntData%AlfaStal )
   IntData%AOD      = IntData%AOD      + Fract*( AF_Table%Tab(ITabHi)%AOD      - IntData%AOD      )
   IntData%AOL      = IntData%AOL      + Fract*( AF_Table%Tab(ITabHi)%AOL      - IntData%AOL      )
   IntData%Cd0      = IntData%Cd0      + Fract*( AF_Table%Tab(ITabHi)%Cd0      - IntData%Cd0      )
   IntData%CnA      = IntData%CnA      + Fract*( AF_Table%Tab(ITabHi)%CnA      - IntData%CnA      )
   IntData%CnS      = IntData%CnS      + Fract*( AF_Table%Tab(ITabHi)%CnS      - IntData%CnS      )
   IntData%CnSL     = IntData%CnSL     + Fract*( AF_Table%Tab(ITabHi)%CnSL     - IntData%CnSL     )


   RETURN
   END SUBROUTINE AeroInt ! ( ISeg, Alpha, Re, AF_Table, IntData, ClInt, CdInt, CmInt )
!=======================================================================
   SUBROUTINE CompDR ( NumSeg, RLoc, HubRad, RotorRad, DimenInp, DelRLoc, ErrStat )


      ! This routine computes the segment lengths from the local radii and the rotor radius.
      ! It prints and error if the list of radii is not realizable.


      ! Argument declarations.

   INTEGER, INTENT(IN)          :: NumSeg                                       ! Number of blade segments.
   INTEGER, INTENT(OUT),OPTIONAL:: ErrStat                                      ! Error status; if present, program does not abort on error

   REAL(ReKi), INTENT(OUT)      :: DelRLoc (NumSeg)                             ! The array of segment lengths.
   REAL(ReKi), INTENT(IN)       :: HubRad                                       ! The hub radius.
   REAL(ReKi), INTENT(IN)       :: RLoc    (NumSeg)                             ! The array of radii (segment centers).
   REAL(ReKi), INTENT(IN)       :: RotorRad                                     ! The rotor radius.

   LOGICAL, INTENT(IN)          :: DimenInp                                     ! Flag that tells if input is dimensional or not.


      ! Local declarations.

   REAL(ReKi)                   :: CompRad                                      ! The computed radius of the rotor.
   REAL(ReKi)                   :: ErrFact                                      ! The conversion to non-dimensional form if needed.
   REAL(ReKi)                   :: SegBeg                                       ! The beginning of the current segment.

   INTEGER                      :: ISeg                                         ! Segment index



   IF ( PRESENT(ErrStat) ) ErrStat = 0


      ! Determine the correct units for error messages.

   IF ( DimenInp )  THEN
      ErrFact = 1.0
   ELSE
      ErrFact = RotorRad
   END IF


      ! We will work our way from the root to the tip.

   SegBeg = HubRad

   DO ISeg=1,NumSeg

      IF ( RLoc(ISeg) <= SegBeg )  THEN
         CALL WrScr1 ( ' Your analysis nodes are incorrectly defined.  Please review the following forum topic for an' &
                     //' explaination of this error:' )
         CALL WrScr1 ( '    https://wind.nrel.gov/forum/wind/viewtopic.php?f=4&t=241' )
         CALL ProgAbort ( ' The radius for blade segment #'//Trim( Int2LStr( ISeg ) )//' is too far inboard for a physically' &
                    //' realizable blade.  It must be greater than '//Trim( Flt2LStr( SegBeg/ErrFact ) )//'.', PRESENT(ErrStat) )
         IF ( PRESENT(ErrStat) ) ErrStat = 1
         RETURN
      END IF

      DelRLoc(ISeg) = 2.0*( RLoc(ISeg) - SegBeg )
      SegBeg        = SegBeg + DelRLoc(ISeg)

   END DO ! ISeg


      ! Ensure that the segments (almost) exactly fill the blade.

   CompRad = RLoc(NumSeg) + 0.5*DelRLoc(NumSeg)

   IF ( ABS( CompRad - RotorRad )/RotorRad > 0.005 )  THEN
         CALL WrScr1 ( ' Your analysis nodes are incorrectly defined.  Please review the following forum topic for an' &
                     //' explaination of this error:' )
         CALL WrScr1 ( '    https://wind.nrel.gov/forum/wind/viewtopic.php?f=4&t=241' )
         CALL ProgAbort ( ' The sum of the lengths of the blade segments does not match the rotor radius.  The segments add up' &
                    //' to a rotor radius of '//Trim( Flt2LStr( CompRad ) )//' instead of the specified radius of ' &
                    //Trim( Flt2LStr( RotorRad ) )//'.  They must agree within 0.5%', PRESENT(ErrStat) )
         IF ( PRESENT( ErrStat ) )  ErrStat = 1
         RETURN
   ELSE IF ( ABS( CompRad - RotorRad )/RotorRad > 0.001 )  THEN
! Nice message, Marshall! ;-)  Thank you!  :-)  I don't even remember writing this.
         CALL WrScr1 ( ' Your analysis nodes are incorrectly defined.  Please review the following forum topic for an' &
                     //' explaination of this error:' )
         CALL WrScr1 ( '    https://wind.nrel.gov/forum/wind/viewtopic.php?f=4&t=241' )
         CALL WrScr1 ( ' The sum of the lengths of the blade segments does not match the rotor radius.  The segments add up to a' &
                     //' rotor radius of '//Trim( Flt2LStr( CompRad ) )//' instead of the specified radius of ' &
                     //Trim( Flt2LStr( RotorRad ) )//'.  They really should agree within 0.1%, but I''ll let you slide.' )
         CALL UsrAlarm
   END IF


   RETURN
   END SUBROUTINE CompDR ! ( NumSeg, RLoc, RotorRad, DimenInp, DelRLoc [, ErrStat] )
!=======================================================================
   SUBROUTINE GetAF ( AF_File, AF_Table, ISeg )
!bjj: note that this routine aborts instead of allowing an optional returned error code.

      ! Routine to get airfoil data from either a new NWTC-style or an old AeroDyn-style airfoil file.


      ! Argument declarations.

   TYPE (ElmTable), INTENT(OUT) :: AF_Table                                  ! The table of airfoil data for the current segment.

   INTEGER, INTENT(IN)          :: ISeg                                      ! The segment number.

   CHARACTER(*), INTENT(IN)     :: AF_File                                   ! Name of file containing AeroDyn-style airfoil data.


      ! Local declarations.

      ! Because of what seems to be a compiler bug, we cannot dynamically allocate the data arrays for the new-style
      ! airfoil files.  We really need to do it for the old-style files because there is no limit on the number of points.

!   TYPE                            :: DataRowO                                  ! Declare new type that is an allocatable table of data using a linked list.
!      REAL(ReKi), ALLOCATABLE      :: Data      (:)
!      TYPE(DataRowO), POINTER      :: Next            => NULL()
!   ENDTYPE DataRowO

   REAL(ReKi)                      :: AF_Data   (5)                             ! The values from one line of airfol data.
   REAL(ReKi), ALLOCATABLE         :: AF_DataO  (:)                             ! The values from one line of airfol data.
   REAL(ReKi), ALLOCATABLE         :: RnAry     (:)                             ! The temporary array for Re.
   REAL(ReKi), ALLOCATABLE         :: ASAry     (:)                             ! The temporary array for Stall AoA.
   REAL(ReKi), ALLOCATABLE         :: AOLAry    (:)                             ! The temporary array for zero-lift AoA.
   REAL(ReKi)                      :: Cc                                        ! The chordwise force coefficient.
   REAL(ReKi)                      :: Cn                                        ! The normal force coefficient.
   REAL(ReKi), ALLOCATABLE         :: CnAAry    (:)                             ! The temporary array for Cn slope for zero lift.
   REAL(ReKi), ALLOCATABLE         :: CnSAry    (:)                             ! The temporary array for Cn at stall value for positive AoA.
   REAL(ReKi), ALLOCATABLE         :: CnSLAry   (:)                             ! The temporary array for Cn at stall value for negative AoA.
   REAL(ReKi), ALLOCATABLE         :: AODAry    (:)                             ! The temporary array for AoA for minimum Cd.
   REAL(ReKi), ALLOCATABLE         :: CDOAry    (:)                             ! The temporary array for minimum Cd value.

   INTEGER                         :: IAlf                                      ! A generic array index for angle of attack.
   INTEGER                         :: Ind                                       ! A generic array index.
   INTEGER                         :: IOS                                       ! The status of an I/O operation.
   INTEGER                         :: ITab                                      ! The table index.
   INTEGER                         :: NumAlf                                    ! The number of lines in an old-style airfoil table.
   INTEGER                         :: NumAlpha                                  ! The number of non--blank lines in an old-style airfoil table.
   INTEGER                         :: NumCoef                                   ! The number of coefficiants in an airfoil table.
   INTEGER                         :: NumVals                                   ! The total number of values on one line of airfoil data.
   INTEGER                         :: Sttus                                     ! The status returned from the allocation.
   INTEGER                         :: UnAF     = 20                             ! I/O unit number for the airfoil file.

   CHARACTER( 15)                  :: Frmt = "(1000(F11.4,:))"                  ! Output format for a line of airfoil data.
   CHARACTER(999)                  :: Line                                      ! A line of text.
   CHARACTER(  3)                  :: Line3                                     ! The first three characters of a line of text.



      ! Open the airfoil data file.

   CALL OpenFInpFile ( UnAF, AF_File )


      ! Read the header block of the airfoil file.  Look to see if this is a new-format file.

   READ (UnAF,'(A)',IOSTAT=IOS)  Line

   CALL CheckIOS ( IOS, AF_File, 'FirstHead', StrType )

   IF ( Echo )  THEN
      WRITE (UnEc,"(15X,A,T30,' - ',A,/,2X,A)")  'FirstHead', 'First line in the airfoil file.', TRIM( Line )
   END IF

   CALL Conv2UC  ( Line )

   IF ( Line(:21) == 'AERODYN AIRFOIL FILE.' )  THEN


         ! This is new style of AeroDyn file.

      CALL ReadCom  ( UnAF, AF_File, 'the first title' )
      CALL ReadCom  ( UnAF, AF_File, 'the second title' )
      CALL ReadIVar ( UnAF, AF_File, AF_Table%NumTabs, 'NumTabs', 'Number of airfoil tables for segment #' &
                                                                 //TRIM( Int2LStr( ISeg ) )//'.' )

      IF ( AF_Table%NumTabs < 1 )  CALL ProgAbort ( ' Number of tables in airfoil file, "'//TRIM( AF_File ) &
                                              //'", must be > 0 for segment #'//TRIM( Int2LStr( ISeg ) )//' in GetAF.' )


         ! Are we expecting Cm data in the file?  Allocate the temporary data array.

      IF ( UseCm )  THEN
         NumVals = 4
      ELSE
         NumVals = 3
      END IF


         ! Are we expecting Cp,min data in the file?  Allocate the temporary data array.

      IF ( UseCpmin )  THEN
         NumVals = NumVals + 1
      END IF


         ! Allocate the AF_Table of pointers for this element.

      ALLOCATE ( AF_Table%Tab(AF_Table%NumTabs) , STAT=Sttus )
      IF ( Sttus /= 0 )  THEN
         CALL ProgAbort ( ' Error allocating memory for the Tab subtable of the AF_Table of pointers for segment #' &
                    //TRIM( Int2LStr( ISeg ) )//' in GetAF.' )
      END IF


         ! Read the NumTabs airfoil tables.

      DO ITab=1,AF_Table%NumTabs


            ! Read in the Table ID (Re), control setting, and stall parameters for this table.

         CALL ReadRVar ( UnAF, AF_File, AF_Table%Tab(ITab)%Re      , 'Re('      //TRIM( Int2LStr( ITab ) )//')'   &
                                                                   , 'Reynolds number for this airfoil table.'    )
         CALL ReadRVar ( UnAF, AF_File, AF_Table%Tab(ITab)%Ctrl    , 'Ctrl('    //TRIM( Int2LStr( ITab ) )//')'   &
                                                                   , 'Control setting for this airfoil table.'    )
         CALL ReadRVar ( UnAF, AF_File, AF_Table%Tab(ITab)%AlfaStal, 'AlfaStal('//TRIM( Int2LStr( ITab ) )//')'   &
                                                                   , 'stall AoA for this airfoil table.'          )
         CALL ReadRVar ( UnAF, AF_File, AF_Table%Tab(ITab)%AOL     , 'AOL('     //TRIM( Int2LStr( ITab ) )//')'   &
                                                                   , 'zero-lift AoA.'                             )
         CALL ReadRVar ( UnAF, AF_File, AF_Table%Tab(ITab)%CnA     , 'CnA('     //TRIM( Int2LStr( ITab ) )//')'   &
                                                                   , 'Cn slope for zero-lift.'                    )
         CALL ReadRVar ( UnAF, AF_File, AF_Table%Tab(ITab)%CnS     , 'CnS('     //TRIM( Int2LStr( ITab ) )//')'   &
                                                                   , 'Cn at stall value for positive AoA.'        )
         CALL ReadRVar ( UnAF, AF_File, AF_Table%Tab(ITab)%CnSL    , 'CnSL('    //TRIM( Int2LStr( ITab ) )//')'   &
                                                                   , 'Cn at stall value for negative AoA.'        )
         CALL ReadRVar ( UnAF, AF_File, AF_Table%Tab(ITab)%AOD     , 'AOD('     //TRIM( Int2LStr( ITab ) )//')'   &
                                                                   , 'AoA for minimum CD.'                        )
         CALL ReadRVar ( UnAF, AF_File, AF_Table%Tab(ITab)%Cd0     , 'Cd0('     //TRIM( Int2LStr( ITab ) )//')'   &
                                                                   , 'minimum Cd value.'                          )


            ! Convert to proper units.

         AF_Table%Tab(ITab)%AlfaStal = AF_Table%Tab(ITab)%AlfaStal*D2R
         AF_Table%Tab(ITab)%AOD      = AF_Table%Tab(ITab)%AOD     *D2R
         AF_Table%Tab(ITab)%AOL      = AF_Table%Tab(ITab)%AOL     *D2R
         AF_Table%Tab(ITab)%Re       = AF_Table%Tab(ITab)%Re      *1.0e6


            ! Find the length of this table.

         AF_Table%Tab(ITab)%NumAlf = 0

         DO

            READ (UnAF,'(A)',IOSTAT=IOS)  Line3

            IF ( IOS < 0 )  THEN
               CALL PremEOF ( AF_File , 'the "EOT" end-of-table mark for airfoil table #'//TRIM( Int2LStr( ITab ) ) &
                                      //' and segment #'//TRIM( Int2LStr( ISeg ) ) )
            ELSE IF ( IOS > 0 )  THEN
               CALL WrScr1 ( ' Invalid character input for file "'//TRIM( AF_File )//'.' )
               CALL ProgAbort  ( ' The error occurred while trying to read line #'//TRIM( Int2LStr( AF_Table%Tab(ITab)%NumAlf+1 ) )&
                           //' of airfoil table #'//TRIM( Int2LStr( ITab ) )//' for segment #'//TRIM( Int2LStr( ISeg ) )//'.' )
            END IF

            CALL Conv2UC ( Line3 )
            IF ( Line3 == 'EOT' )  EXIT
            AF_Table%Tab(ITab)%NumAlf = AF_Table%Tab(ITab)%NumAlf + 1

         END DO


            ! Rewind the file to the beginning of this table.

         DO IAlf=1,AF_Table%Tab(ITab)%NumAlf+1
            BACKSPACE UnAF
         END DO ! IAlf


            ! Let's allocate the permanent table.

         ALLOCATE ( AF_Table%Tab(ITab)%Alpha(AF_Table%Tab(ITab)%NumAlf) , STAT=Sttus )
         IF ( Sttus /= 0 )  THEN
            CALL ProgAbort ( ' Error allocating memory for the Alpha subtable for segment #'//TRIM( Int2LStr( ISeg) ) &
                       //' and table #'//TRIM( Int2LStr( ITab) )//').' )
         END IF

         ALLOCATE ( AF_Table%Tab(ITab)%Cl(AF_Table%Tab(ITab)%NumAlf) , STAT=Sttus )
         IF ( Sttus /= 0 )  THEN
            CALL ProgAbort ( ' Error allocating memory for the Cl subtable for segment #'//TRIM( Int2LStr( ISeg) ) &
                       //' and table #'//TRIM( Int2LStr( ITab) )//').' )
         END IF

         ALLOCATE ( AF_Table%Tab(ITab)%Cd(AF_Table%Tab(ITab)%NumAlf) , STAT=Sttus )
         IF ( Sttus /= 0 )  THEN
            CALL ProgAbort ( ' Error allocating memory for the Cd subtable for segment #'//TRIM( Int2LStr( ISeg) ) &
                       //' and table #'//TRIM( Int2LStr( ITab) )//').' )
         END IF

         IF ( UseCm )  THEN
            ALLOCATE ( AF_Table%Tab(ITab)%Cm(AF_Table%Tab(ITab)%NumAlf) , STAT=Sttus )
            IF ( Sttus /= 0 )  THEN
               CALL ProgAbort ( ' Error allocating memory for the Cm subtable for segment #'//TRIM( Int2LStr( ISeg) ) &
                       //' and table #'//TRIM( Int2LStr( ITab) )//').' )
            END IF
         END IF

         IF ( UseCpmin )  THEN
            ALLOCATE ( AF_Table%Tab(ITab)%Cpmin(AF_Table%Tab(ITab)%NumAlf) , STAT=Sttus )
            IF ( Sttus /= 0 )  THEN
               CALL ProgAbort ( ' Error allocating memory for the Cpmin subtable for segment #'//TRIM( Int2LStr( ISeg) ) &
                       //' and table #'//TRIM( Int2LStr( ITab) )//').' )
            END IF
         END IF

         ALLOCATE ( AF_Table%Tab(ITab)%FTB(AF_Table%Tab(ITab)%NumAlf) , STAT=Sttus )
         IF ( Sttus /= 0 )  THEN
            CALL ProgAbort ( ' Error allocating memory for the FTB subtable for segment #'//TRIM( Int2LStr( ISeg) ) &
                       //' and table #'//TRIM( Int2LStr( ITab) )//').' )
         END IF

         ALLOCATE ( AF_Table%Tab(ITab)%FTBC(AF_Table%Tab(ITab)%NumAlf) , STAT=Sttus )
         IF ( Sttus /= 0 )  THEN
            CALL ProgAbort ( ' Error allocating memory for the FTBC subtable for segment #'//TRIM( Int2LStr( ISeg) ) &
                       //' and table #'//TRIM( Int2LStr( ITab) )//').' )
         END IF


            ! Read in the airfoil data for this table.

         DO IAlf=1,AF_Table%Tab(ITab)%NumAlf

            READ (UnAF,*,IOSTAT=IOS)  ( AF_Data(Ind), Ind=1,NumVals )

            CALL CheckIOS ( IOS, AF_File, 'AF_Data', NumType )

            IF ( Echo )  WRITE (UnEc,Frmt)  ( AF_Data(Ind), Ind=1,NumVals )

            AF_Table%Tab(ITab)%Alpha(IAlf) = AF_Data(1)

            AF_Table%Tab(ITab)%Cl(IAlf) = AF_Data(2)
            AF_Table%Tab(ITab)%Cd(IAlf) = AF_Data(3)

            IF ( ABS( AF_Table%Tab(ITab)%CnA ) .GT. 1.0e-6 )  THEN

               Cn = AF_Data(2)*COS( AF_Data(1) ) + ( AF_Data(3) - AF_Table%Tab(ITab)%Cd0 )*SIN( AF_Data(1) )
               Cc = AF_Data(2)*SIN( AF_Data(1) ) - ( AF_Data(3) - AF_Table%Tab(ITab)%Cd0 )*COS( AF_Data(1) )

               AF_Table%Tab(ITab)%FTB (IAlf) = Cn/AF_Table%Tab(ITab)%CnA
               AF_Table%Tab(ITab)%FTBC(IAlf) = Cc/AF_Table%Tab(ITab)%CnA

            ELSE

               AF_Table%Tab(ITab)%FTB (IAlf) = 1.0
               AF_Table%Tab(ITab)%FTBC(IAlf) = 1.0

            END IF

            IF ( UseCm )  THEN
               AF_Table%Tab(ITab)%Cm(IAlf) = AF_Data(4)
            END IF

            IF ( UseCpmin )  THEN
               AF_Table%Tab(ITab)%Cpmin(IAlf) = AF_Data(NumVals)
            END IF

         END DO ! IAlf


            ! Check AoA range.

         IF ( ( AF_Table%Tab(ITab)%Alpha(1)                         > -180.0 ) .OR. &
              ( AF_Table%Tab(ITab)%Alpha(AF_Table%Tab(ITab)%NumAlf) <  180.0 ) )  THEN
            CALL ProgAbort ( 'Angle of attack range for airfoil table #'//TRIM( Int2LStr( ITab ) )//' of segment #' &
                       //TRIM( Int2LStr( ISeg ) )//' must be from -180 to 180.' )
         END IF


            ! Skip this EOT mark.

         READ (UnAF,'()')

      END DO ! ITab

   ELSE


         ! This is old style of AeroDyn file.

      CALL ReadCom  ( UnAF, AF_File, 'the second title' )
      CALL ReadIVar ( UnAF, AF_File, AF_Table%NumTabs, 'NumTabs', 'Number of airfoil tables for segment #' &
                                                                //TRIM( Int2LStr( ISeg ) )//'.' )

      ALLOCATE ( RnAry(AF_Table%NumTabs) , STAT=Sttus )
      IF ( Sttus /= 0 )  THEN
         CALL ProgAbort ( ' Error allocating memory for the RnAry array for segment #'//TRIM( Int2LStr( ISeg ) )//' in GetAF.' )
      END IF

      ALLOCATE ( ASAry(AF_Table%NumTabs) , STAT=Sttus )
      IF ( Sttus /= 0 )  THEN
         CALL ProgAbort ( ' Error allocating memory for the ASAry array for segment #'//TRIM( Int2LStr( ISeg ) )//' in GetAF.' )
      END IF

      ALLOCATE ( AOLAry(AF_Table%NumTabs) , STAT=Sttus )
      IF ( Sttus /= 0 )  THEN
         CALL ProgAbort ( ' Error allocating memory for the AOLAry array for segment #'//TRIM( Int2LStr( ISeg ) )//' in GetAF.' )
      END IF

      ALLOCATE ( CnAAry(AF_Table%NumTabs) , STAT=Sttus )
      IF ( Sttus /= 0 )  THEN
         CALL ProgAbort ( ' Error allocating memory for the CnAAry array for segment #'//TRIM( Int2LStr( ISeg ) )//' in GetAF.' )
      END IF

      ALLOCATE ( CnSAry(AF_Table%NumTabs) , STAT=Sttus )
      IF ( Sttus /= 0 )  THEN
         CALL ProgAbort ( ' Error allocating memory for the CnSAry array for segment #'//TRIM( Int2LStr( ISeg ) )//' in GetAF.' )
      END IF

      ALLOCATE ( CnSLAry(AF_Table%NumTabs) , STAT=Sttus )
      IF ( Sttus /= 0 )  THEN
         CALL ProgAbort ( ' Error allocating memory for the CnSLAry array for segment #'//TRIM( Int2LStr( ISeg ) )//' in GetAF.' )
      END IF

      ALLOCATE ( AODAry(AF_Table%NumTabs) , STAT=Sttus )
      IF ( Sttus /= 0 )  THEN
         CALL ProgAbort ( ' Error allocating memory for the AODAry array for segment #'//TRIM( Int2LStr( ISeg ) )//' in GetAF.' )
      END IF

      ALLOCATE ( CDOAry(AF_Table%NumTabs) , STAT=Sttus )
      IF ( Sttus /= 0 )  THEN
         CALL ProgAbort ( ' Error allocating memory for the CDOAry array for segment #'//TRIM( Int2LStr( ISeg ) )//' in GetAF.' )
      END IF

      CALL ReadRAry ( UnAF, AF_File, RnAry  , AF_Table%NumTabs, 'RnAry'  , 'Reynolds number values for the airfoil tables.' )
      CALL ReadRAry ( UnAF, AF_File, ASAry  , AF_Table%NumTabs, 'ASAry'  , 'Stall AoA for this airfoil table.'              )
      CALL ReadCom  ( UnAF, AF_File,                                       'the unused first obsolete stall parameter'      )
      CALL ReadCom  ( UnAF, AF_File,                                       'the unused second obsolete stall parameter'     )
      CALL ReadCom  ( UnAF, AF_File,                                       'the unused third obsolete stall parameter'      )
      CALL ReadRAry ( UnAF, AF_File, AOLAry , AF_Table%NumTabs, 'AOLAry' , 'zero-lift AoA'                                  )
      CALL ReadRAry ( UnAF, AF_File, CnAAry , AF_Table%NumTabs, 'CnAAry' , 'Cn slope for zero lift'                         )
      CALL ReadRAry ( UnAF, AF_File, CnSAry , AF_Table%NumTabs, 'CnSAry' , 'Cn at stall value for positive AoA'             )
      CALL ReadRAry ( UnAF, AF_File, CnSLAry, AF_Table%NumTabs, 'CnSLAry', 'Cn at stall value for negative AoA'             )
      CALL ReadRAry ( UnAF, AF_File, AODAry , AF_Table%NumTabs, 'AODAry' , 'AoA for minimum Cd'                             )
      CALL ReadRAry ( UnAF, AF_File, CDOAry , AF_Table%NumTabs, 'CDOAry' , 'minimum Cd value'                               )


         ! Are we expecting Cm data in the file?  Allocate the temporary data array.

      IF ( UseCm )  THEN
         NumCoef = 3
      ELSE
         NumCoef = 2
      END IF

      NumVals = 1 + NumCoef*AF_Table%NumTabs

      ALLOCATE ( AF_DataO(NumVals) , STAT=Sttus )
      IF ( Sttus /= 0 )  THEN
         CALL ProgAbort ( ' Error allocating memory for the AF_DataO array for segment #'//TRIM( Int2LStr( ISeg ) )//' in GetAF.' )
      END IF


         ! Allocate the AF_Table of pointers for this element.

      ALLOCATE ( AF_Table%Tab(AF_Table%NumTabs) , STAT=Sttus )
      IF ( Sttus /= 0 )  THEN
         CALL ProgAbort ( ' Error allocating memory for the AF_Table of pointers in GetAF.' )
      END IF


            ! Find the length of this table.

      NumAlf = 0

      DO
         READ (UnAF,'()',IOSTAT=IOS)
         IF ( IOS < 0 )  EXIT
         NumAlf = NumAlf + 1
      END DO


         ! Rewind the file to the beginning of this table.

      DO IAlf=1,NumAlf+1
         BACKSPACE UnAF
      END DO ! IAlf


         ! Let's allocate the tables.

      DO ITab=1,AF_Table%NumTabs

         ALLOCATE ( AF_Table%Tab(ITab)%Alpha(NumAlf) , STAT=Sttus )
         IF ( Sttus /= 0 )  THEN
            CALL ProgAbort ( ' Error allocating memory for the Alpha vector of airfoil table #'//TRIM( Int2LStr( ITab) ) &
                       //' for element #'//TRIM( Int2LStr( ISeg) )//'.' )
         END IF

         ALLOCATE ( AF_Table%Tab(ITab)%Cl(NumAlf) , STAT=Sttus )
         IF ( Sttus /= 0 )  THEN
            CALL ProgAbort ( ' Error allocating memory for the Cl vector of airfoil table #'//TRIM( Int2LStr( ITab) ) &
                       //' for element #'//TRIM( Int2LStr( ISeg) )//'.' )
         END IF

         ALLOCATE ( AF_Table%Tab(ITab)%Cd(NumAlf) , STAT=Sttus )
         IF ( Sttus /= 0 )  THEN
            CALL ProgAbort ( ' Error allocating memory for the Cd vector of airfoil table #'//TRIM( Int2LStr( ITab) ) &
                       //' for element #'//TRIM( Int2LStr( ISeg) )//'.' )
         END IF

         IF ( UseCm )  THEN
            ALLOCATE ( AF_Table%Tab(ITab)%Cm(NumAlf) , STAT=Sttus )
            IF ( Sttus /= 0 )  THEN
               CALL ProgAbort ( ' Error allocating memory for the Cm vector of airfoil table #'//TRIM( Int2LStr( ITab) ) &
                          //' for element #'//TRIM( Int2LStr( ISeg) )//'.' )
            END IF
         END IF

         ALLOCATE ( AF_Table%Tab(ITab)%FTB(NumAlf) , STAT=Sttus )
         IF ( Sttus /= 0 )  THEN
            CALL ProgAbort ( ' Error allocating memory for the FTB subtable for segment #'//TRIM( Int2LStr( ISeg) ) &
                       //' and table #'//TRIM( Int2LStr( ITab) )//').' )
         END IF

         ALLOCATE ( AF_Table%Tab(ITab)%FTBC(NumAlf) , STAT=Sttus )
         IF ( Sttus /= 0 )  THEN
            CALL ProgAbort ( ' Error allocating memory for the FTBC subtable for segment #'//TRIM( Int2LStr( ISeg) ) &
                      //' and table #' //TRIM( Int2LStr( ITab) )//').' )
         END IF

      END DO ! ITab


         ! Let's read the data this time through.

      NumAlpha = NumAlf

      DO IAlf=1,NumAlf


            ! Let's skip blank lines.  Decrement the number of alphas when we find them.

         READ (UnAF,'(A)')  Line

         IF ( LEN_TRIM( Line ) == 0 )  THEN
            NumAlpha = NumAlpha - 1
            CYCLE
         END IF


            ! Let's get the data from the non-blank line.

         READ (Line,*,IOSTAT=IOS)  ( AF_DataO(Ind), Ind=1,NumVals )

         CALL CheckIOS ( IOS, AF_File, 'AF_DataO', NumType )

         IF ( Echo )  THEN
            WRITE (UnEc,Frmt)  ( AF_DataO(Ind), Ind=1,NumVals )
         END IF


            ! Let's move this good data into permanent storage.

         DO ITab=1,AF_Table%NumTabs

            AF_Table%Tab(ITab)%Alpha(IAlf) = AF_DataO(1)
            AF_Table%Tab(ITab)%Cl   (IAlf) = AF_DataO(NumCoef*(ITab-1)+2)
            AF_Table%Tab(ITab)%Cd   (IAlf) = AF_DataO(NumCoef*(ITab-1)+3)

            IF ( UseCm )  THEN
               AF_Table%Tab(ITab)%Cm(IAlf) = AF_DataO(NumCoef*(ITab-1)+4)
            END IF

            IF ( ABS( AF_Table%Tab(ITab)%CnA ) .GT. 1.0e-6 )  THEN

               Cn = AF_Data(2)*COS( AF_Data(1) ) + ( AF_Data(3) - AF_Table%Tab(ITab)%Cd0 )*SIN( AF_Data(1) )
               Cc = AF_Data(2)*SIN( AF_Data(1) ) - ( AF_Data(3) - AF_Table%Tab(ITab)%Cd0 )*COS( AF_Data(1) )

               AF_Table%Tab(ITab)%FTB (IAlf) = Cn/AF_Table%Tab(ITab)%CnA
               AF_Table%Tab(ITab)%FTBC(IAlf) = Cc/AF_Table%Tab(ITab)%CnA

            ELSE

               AF_Table%Tab(ITab)%FTB (IAlf) = 1.0
               AF_Table%Tab(ITab)%FTBC(IAlf) = 1.0

            END IF

         END DO ! ITab

      END DO ! IAlf


         ! Check AoA range.  AoAs are the same for all tables in a given segment.

      IF ( ( AF_Table%Tab(1)%Alpha(1)      > -180.0 ) .OR. &
           ( AF_Table%Tab(1)%Alpha(NumAlf) <  180.0 ) )  THEN
!           ( AF_Table%Tab(1)%Alpha(AF_Table%Tab(ITab)%NumAlf) <  180.0 ) )  THEN
         CALL ProgAbort ( 'Angle of attack range of airfoil tables for segment #'//TRIM( Int2LStr( ISeg ) ) &
                    //' must be from -180 to 180.' )
      END IF


           ! Store the header data in the permanent structure.

      DO ITab=1,AF_Table%NumTabs
         AF_Table%Tab(ITab)%AlfaStal = ASAry  (ITab)*D2R
         AF_Table%Tab(ITab)%Re       = RnAry  (ITab)*1.0e6
         AF_Table%Tab(ITab)%AOD      = AODAry (ITab)*D2R
         AF_Table%Tab(ITab)%AOL      = AOLAry (ITab)*D2R
         AF_Table%Tab(ITab)%Cd0      = CDOAry (ITab)
         AF_Table%Tab(ITab)%CnA      = CnAAry (ITab)
         AF_Table%Tab(ITab)%CnS      = CnSAry (ITab)
         AF_Table%Tab(ITab)%CnSL     = CnSLAry(ITab)
         AF_Table%Tab(ITab)%NumAlf   = NumAlpha
      END DO ! ITab


         ! Deallocate the temporary Re array.

      DEALLOCATE ( RnAry, STAT=Sttus )
      IF ( Sttus /= 0 )  THEN
         CALL ProgAbort ( ' Error deallocating memory for the RnAry array in GetAF.' )
      END IF


         ! Deallocate the temporary data array.

      DEALLOCATE ( AF_DataO, STAT=Sttus )
      IF ( Sttus /= 0 )  THEN
         CALL ProgAbort ( ' Error deallocating memory for the AF_DataO array in GetAF.' )
      END IF

   END IF

   CLOSE ( UnAF )


   RETURN
   END SUBROUTINE GetAF !  ( AF_File, AF_Table, ISeg )
!=======================================================================
   FUNCTION GetCoef( ISeg, Alpha, AlfaTab, CoefTab, NumRows, Ind, ErrStat )


      ! Interpolation routine for airfoil section coefficients.


      ! Function declaration.

   REAL(ReKi)                        :: GetCoef                                 ! The value returned by this function.


      ! Argument declarations.

   INTEGER, INTENT(INOUT)            :: Ind                                     ! The starting/resulting index into the tables.
   INTEGER, INTENT(IN)               :: ISeg                                    ! The current segment.
   INTEGER, INTENT(IN)               :: NumRows                                 ! The length of the arrays.
   INTEGER, INTENT(OUT), OPTIONAL    :: ErrStat                                 ! Error status; if present, program does not abort on error

   REAL(ReKi), INTENT(IN)            :: AlfaTab   (NumRows)                     ! Table of AoAs.
   REAL(ReKi), INTENT(IN)            :: Alpha                                   ! Angle of attack to get the coefficient for.
   REAL(ReKi), INTENT(IN)            :: CoefTab   (NumRows)                     ! Table of coefficients.


   IF ( PRESENT(ErrStat) ) ErrStat = 1


      ! If Alpha is to the outside the table, the user needs to make up some data.  Warn the user and stop the program.

   IF ( ( Alpha < AlfaTab(1) ) .OR. ( AlfaTab(NumRows) < Alpha ) )  THEN

      CALL ProgAbort ( ' For segment '//TRIM( Int2LStr( ISeg ) )//', the current angle of attack ('//TRIM( Flt2LStr( Alpha ) ) &
                 //' degrees) is outside the domain of your data table (' //TRIM( Flt2LStr( AlfaTab(1) ) )//' to ' &
                 //TRIM( Flt2LStr( AlfaTab(NumRows) ) )//' degrees).  Please extend your data table.', PRESENT(ErrStat) )
      IF ( PRESENT(ErrStat) ) ErrStat = 1
      RETURN

   END IF


      ! Alpha is in range.  Interpolate.  Use binary interpolation if this is the first time to access this table.

   IF ( Ind == 0 )  THEN
      GetCoef = InterpBin( Alpha, AlfaTab, CoefTab, Ind, NumRows )
   ELSE
      GetCoef = InterpStp( Alpha, AlfaTab, CoefTab, Ind, NumRows )
   END IF


   RETURN
   END FUNCTION GetCoef ! ( ISeg, Alpha, AlfaTab, CoefTab, NumRows, Ind )
!=======================================================================
   SUBROUTINE GetCoefs ( ISeg, Alpha, Re, AF_Table, ClInt, CdInt, CmInt, CpminInt, DoCl, DoCd, DoCm, DoCpmin, ErrStat )


      ! This routine finds the Re-bounding tables and then calls GetCoef() to get the
      ! desired coefficients for the two tables and then interpolates between them.


      ! Argument declarations.

   TYPE (ElmTable), INTENT(INOUT)    :: AF_Table                                ! The table of airfoil data for the current segment.

   INTEGER, INTENT(IN)               :: ISeg                                    ! The current segment.
   INTEGER, INTENT(OUT), OPTIONAL    :: ErrStat                                 ! Error status; if present, program does not abort on error

   LOGICAL, INTENT(IN)               :: DoCd                                    ! Get Cd.
   LOGICAL, INTENT(IN)               :: DoCl                                    ! Get Cl.
   LOGICAL, INTENT(IN)               :: DoCm                                    ! Get Cm.
   LOGICAL, INTENT(IN)               :: DoCpmin                                 ! Get Cp,min.

   REAL(ReKi), INTENT(IN)            :: Alpha                                   ! Angle of attack to get the coefficient for.
   REAL(ReKi), INTENT(OUT)           :: CdInt                                   ! Interpolated drag coefficient.
   REAL(ReKi), INTENT(OUT)           :: ClInt                                   ! Interpolated lift coefficient.
   REAL(ReKi), INTENT(OUT)           :: CmInt                                   ! Interpolated pitching-moment coefficient.
   REAL(ReKi), INTENT(OUT)           :: CpminInt                                ! Interpolated minimum-pressure coefficient.
   REAL(ReKi), INTENT(IN)            :: Re                                      ! Reynolds number.


      ! Local declarations.

   REAL(ReKi)                        :: CdHi                                    ! The drag coefficient for the higher Re.
   REAL(ReKi)                        :: ClHi                                    ! The lift coefficient for the higher Re.
   REAL(ReKi)                        :: CmHi                                    ! The pitching-moment coefficient for the higher Re.
   REAL(ReKi)                        :: CpminHi                                 ! The minimum-pressure coefficient for the higher Re.
   REAL(ReKi)                        :: Fract                                   ! The fractional distance between tables.

   INTEGER                           :: ITab                                    ! An index for table number.
   INTEGER                           :: ITabLo                                  ! The table number that is the lower bound for Re.
   INTEGER                           :: ITabHi                                  ! The table number that is the lower bound for Re.

   LOGICAL                           :: OneTable                                ! Flag that tells if we need to read only one table (no interpolation).


   IF ( PRESENT(ErrStat) ) ErrStat = 0

      ! Find the bounding tables (if multiple) for this Re.  If there is only one table
      ! or if we are outside the range of tables, we won't need to interpolate.

   IF ( Re <= AF_Table%Tab(1)%Re )  THEN
      ITabLo   = 1
      OneTable = .TRUE.
   ELSE IF ( Re >= AF_Table%Tab(AF_Table%NumTabs)%Re )  THEN
      ITabLo   = AF_Table%NumTabs
      OneTable = .TRUE.
   ELSE IF ( AF_Table%NumTabs > 1 )  THEN
      DO ITab=1,AF_Table%NumTabs-1
         IF ( Re <= AF_Table%Tab(ITab+1)%Re )  THEN
            ITabLo = ITab
            ITabHi = ITab + 1
            EXIT
         END IF
      END DO
      OneTable = .FALSE.
   ELSE
      ITabLo   = 1
      OneTable = .TRUE.
   END IF


      ! Get the coefficients for ITabLo.


   IF ( PRESENT(ErrStat) ) THEN
      IF ( DoCl )  THEN
         ClInt = GetCoef( ISeg, Alpha, AF_Table%Tab(ITabLo)%Alpha, AF_Table%Tab(ITabLo)%Cl, AF_Table%Tab(ITabLo)%NumAlf, &
                                       AF_Table%Tab(ITabLo)%Ind, ErrStat )
         IF (ErrStat > 0) RETURN
      END IF

      IF ( DoCd )  THEN
         CdInt = GetCoef( ISeg, Alpha, AF_Table%Tab(ITabLo)%Alpha, AF_Table%Tab(ITabLo)%Cd, AF_Table%Tab(ITabLo)%NumAlf, &
                                       AF_Table%Tab(ITabLo)%Ind, ErrStat )
         IF (ErrStat > 0) RETURN
      END IF

      IF ( DoCm )  THEN
         CmInt = GetCoef( ISeg, Alpha, AF_Table%Tab(ITabLo)%Alpha, AF_Table%Tab(ITabLo)%Cm, AF_Table%Tab(ITabLo)%NumAlf, &
                                       AF_Table%Tab(ITabLo)%Ind, ErrStat )
         IF (ErrStat > 0) RETURN
      END IF

      IF ( DoCpmin )  THEN
         CpminInt = GetCoef( ISeg, Alpha, AF_Table%Tab(ITabLo)%Alpha, AF_Table%Tab(ITabLo)%Cpmin, AF_Table%Tab(ITabLo)%NumAlf, &
                                          AF_Table%Tab(ITabLo)%Ind, ErrStat )
         IF (ErrStat > 0) RETURN
      END IF

   ELSE  ! Abort the program when errors are found

      IF ( DoCl )  THEN
         ClInt = GetCoef( ISeg, Alpha, AF_Table%Tab(ITabLo)%Alpha, AF_Table%Tab(ITabLo)%Cl, AF_Table%Tab(ITabLo)%NumAlf, &
                                       AF_Table%Tab(ITabLo)%Ind )
      END IF

      IF ( DoCd )  THEN
         CdInt = GetCoef( ISeg, Alpha, AF_Table%Tab(ITabLo)%Alpha, AF_Table%Tab(ITabLo)%Cd, AF_Table%Tab(ITabLo)%NumAlf, &
                                       AF_Table%Tab(ITabLo)%Ind )
      END IF

      IF ( DoCm )  THEN
         CmInt = GetCoef( ISeg, Alpha, AF_Table%Tab(ITabLo)%Alpha, AF_Table%Tab(ITabLo)%Cm, AF_Table%Tab(ITabLo)%NumAlf, &
                                       AF_Table%Tab(ITabLo)%Ind )
      END IF

      IF ( DoCpmin )  THEN
         CpminInt = GetCoef( ISeg, Alpha, AF_Table%Tab(ITabLo)%Alpha, AF_Table%Tab(ITabLo)%Cpmin, AF_Table%Tab(ITabLo)%NumAlf, &
                                          AF_Table%Tab(ITabLo)%Ind )
      END IF

   END IF


      ! If we don't need to interpolate, we don't need to make a second call and we are done.

   IF ( OneTable )  RETURN                 ! We probably shouldn't do this.  We should probably do a block IF.  mlb


      ! Get the coefficients for ITabHi.  Use step-wise interpolation for all but the first coefficient called.

   Fract = ( Re - AF_Table%Tab(ITabLo)%Re )/( AF_Table%Tab(ITabHi)%Re - AF_Table%Tab(ITabLo)%Re )

   IF ( DoCl )  THEN
      IF ( PRESENT(ErrStat) ) THEN
         ClHi = GetCoef( ISeg, Alpha, AF_Table%Tab(ITabHi)%Alpha, AF_Table%Tab(ITabHi)%Cl, AF_Table%Tab(ITabHi)%NumAlf, &
                                       AF_Table%Tab(ITabHi)%Ind, ErrStat )
         IF (ErrStat > 0) RETURN
      ELSE
         ClHi = GetCoef( ISeg, Alpha, AF_Table%Tab(ITabHi)%Alpha, AF_Table%Tab(ITabHi)%Cl, AF_Table%Tab(ITabHi)%NumAlf, &
                                       AF_Table%Tab(ITabHi)%Ind )
      END IF
      ClInt = ClInt + Fract*( ClHi - ClInt )
   END IF

   IF ( DoCd )  THEN
      IF ( PRESENT(ErrStat) ) THEN
         CdHi = GetCoef( ISeg, Alpha, AF_Table%Tab(ITabHi)%Alpha, AF_Table%Tab(ITabHi)%Cd, AF_Table%Tab(ITabHi)%NumAlf, &
                                       AF_Table%Tab(ITabHi)%Ind, ErrStat )
         IF (ErrStat > 0) RETURN
      ELSE
         CdHi = GetCoef( ISeg, Alpha, AF_Table%Tab(ITabHi)%Alpha, AF_Table%Tab(ITabHi)%Cd, AF_Table%Tab(ITabHi)%NumAlf, &
                                       AF_Table%Tab(ITabHi)%Ind )
      END IF
      CdInt = CdInt + Fract*( CdHi - CdInt )
   END IF

   IF ( DoCm )  THEN
      IF ( PRESENT(ErrStat) ) THEN
         CmHi = GetCoef( ISeg, Alpha, AF_Table%Tab(ITabHi)%Alpha, AF_Table%Tab(ITabHi)%Cm, AF_Table%Tab(ITabHi)%NumAlf, &
                                       AF_Table%Tab(ITabHi)%Ind, ErrStat )
         IF (ErrStat > 0) RETURN
      ELSE
         CmHi = GetCoef( ISeg, Alpha, AF_Table%Tab(ITabHi)%Alpha, AF_Table%Tab(ITabHi)%Cm, AF_Table%Tab(ITabHi)%NumAlf, &
                                       AF_Table%Tab(ITabHi)%Ind )
      END IF
      CmInt = CmInt + Fract*( CmHi - CmInt )
   END IF

   IF ( DoCpmin )  THEN
      IF ( PRESENT(ErrStat) ) THEN
         CpminHi = GetCoef( ISeg, Alpha, AF_Table%Tab(ITabHi)%Alpha, AF_Table%Tab(ITabHi)%Cpmin, AF_Table%Tab(ITabHi)%NumAlf, &
                                          AF_Table%Tab(ITabHi)%Ind, ErrStat )
         IF (ErrStat > 0) RETURN
      ELSE
         CpminHi = GetCoef( ISeg, Alpha, AF_Table%Tab(ITabHi)%Alpha, AF_Table%Tab(ITabHi)%Cpmin, AF_Table%Tab(ITabHi)%NumAlf, &
                                          AF_Table%Tab(ITabHi)%Ind )
      END IF
      CpminInt = CmInt + Fract*( CmHi - CmInt )
   END IF


   RETURN
   END SUBROUTINE GetCoefs ! ( ISeg, Alpha, Re, AF_Table, ClInt, CdInt, CmInt, CpminInt, DoCl, DoCd, DoCm, DoCpmin, ErrStat )
!=======================================================================
   SUBROUTINE InterpAF !( Ratio, InTable1, InTable2, OutTable )


      ! This routine interpolates airfoil tables.  It generates interpolated tables for all Re that occur in either of the input
      ! tables and data for all angles of attack that occur in any of the tables.

      ! For now, we'll take the easy route and just copy the airfoil table that is nearest the analysis node.


!      ! Argument declarations.
!
!   REAL(ReKi), INTENT(IN)       :: Ratio                                        ! (x-x1)/(x2-x1).
!
!   TYPE(ElmTable), INTENT(IN)   :: InTable1                                     ! The input airfoil set for the lower bound.
!   TYPE(ElmTable), INTENT(IN)   :: InTable2                                     ! The input airfoil set for the upper bound.
!   TYPE(ElmTable), INTENT(OUT)  :: OutTable                                     ! The out airfoil set.
!
!
!      ! Local declarations.
!
!   REAL(ReKi), ALLOCATABLE      :: Alfas  (:)                                   ! The union of the alphas from all input tables.
!   REAL(ReKi), ALLOCATABLE      :: Re     (:)                                   ! The union of the Re from both input tables.
!   REAL(ReKi), ALLOCATABLE      :: TmpAry1(:)                                   ! A temporary array to hold the Re's from the first table.
!   REAL(ReKi), ALLOCATABLE      :: TmpAry2(:)                                   ! A temporary array to hold the Re's from the second table.
!
!   INTEGER                      :: IT1                                          ! The index for table 1.
!   INTEGER                      :: IT2                                          ! The index for table 2.
!   INTEGER                      :: NumRe                                        ! The number of unique Reynolds numbers between the two tables.
!   INTEGER                      :: Sttus                                        ! The status returned from an allocation attempt.
!
!
!
!      ! Store the Reynolds Numbers for the two tables in temporary arrays.
!
!   ALLOCATE ( TmpAry1( InTable1%NumTabs ) , STAT=Sttus )
!   IF ( Sttus /= 0 )  THEN
!      CALL ProgAbort( ' Error allocating memory for TmpAry1 in InterpAF.' )
!   END IF
!
!   ALLOCATE ( TmpAry2( InTable2%NumTabs ) , STAT=Sttus )
!   IF ( Sttus /= 0 )  THEN
!      CALL ProgAbort( ' Error allocating memory for TmpAry2 in InterpAF.' )
!   END IF
!
!   DO IT1=1,InTable1%NumTabs
!      TmpAry1(IT1) = InTable1%Tab(IT1)%Re
!   END DO ! IT1
!
!   DO IT2=1,InTable2%NumTabs
!      TmpAry2(IT1) = InTable2%Tab(IT1)%Re
!   END DO ! IT2
!
!
!      ! Allocate a temporary array and fill it with the union of the two input arrays.
!
!   ALLOCATE ( Re( InTable1%NumTabs + InTable2%NumTabs ) , STAT=Sttus )
!   IF ( Sttus /= 0 )  THEN
!      CALL ProgAbort( ' Error allocating memory for Re in InterpAF.' )
!   END IF
!
!   CALL SortUnion ( TmpAry1, InTable1%NumTabs, TmpAry2, InTable2%NumTabs, Re, NumRe )
!
!
!      ! Copy the temporary array into the final structure.  Release the old array.
!
!   ALLOCATE ( OutTable%Tab( NumRe ) , STAT=Sttus )
!   IF ( Sttus /= 0 )  THEN
!      CALL ProgAbort( ' Error allocating memory for OutTable%Tab in InterpAF.' )
!   END IF
!
!   DO IRe=1,NumRe
!
!
!
!
!      ! Find the union of all the alphas in all the tables.


   RETURN
   END SUBROUTINE InterpAF ! ( Ratio, InTable1, InTable2, OutTable )
!=======================================================================

END MODULE NWTC_Aero
MODULE NWTC_Library


      ! Notes:

         ! Your project must include the following files:
         !     NWTC_Aero.f90
         !     NWTC_IO.f90
         !     NWTC_Library.f90
         !     NWTC_Num.f90

         ! Your project must include one, but not both, of the following files:
         !     DoubPrec.f90 - for double-precision arithmetic for floating-points variables.  You may have to set a compiler option to have constants use double precision. (bjj Note: Double-precision variables are now Quad-precision.)
         !     SingPrec.f90 - for single-precision arithmetic for floating-points variables.

         ! Your project must include one, and only one, of the following files:
         !     SysVF.f90     - for DEC, CVF, or IVF compilers for Windows.
         !     SysIVF.f90    - for Intel Visual Fortran for Windows compiler
         !     SysCVF.f90    - for Compaq Visual Fortran for Windows compiler
         !     SysGnu.f90    - for Gnu Fortran for Linux compiler
         !     SysIFL.f90    - for Intel Fortran for Linux compiler
         !     SysMatlab.f90 - for Intel Visual Fortran for Windows compiler with Matlab's mex functions


         ! Compilation order for command-line compilation:
         !     SingPrec.f90 or DoubPrec.f90
         !     SysVF.f90 (or other Sys*.f90 file)
         !     NWTC_IO.f90
         !     NWTC_Num.f90
         !     NWTC_Aero.f90
         !     NWTC_Library.f90

         ! Invoking programs should call NWTC_Init() to initialize data important to the use of the library.  Currently,
         !  this is used only for the Pi-based constants.


   USE NWTC_Aero   ! The other modules (NWTC_IO, NWTC_Num, Precision, SysSubs, and F2kCLI) are already included in NWTC_Aero.

CONTAINS

!=======================================================================
   SUBROUTINE NWTC_Init


      ! This routine calls all required initialization routines.



   CALL PiConsts

!mlb Let's get rid of this once FLUSH works.
   CALL OpenCon( )


   RETURN
   END SUBROUTINE NWTC_Init
!=======================================================================

END MODULE NWTC_Library
!=======================================================================
MODULE ProgGen


USE                             NWTC_Library

IMPLICIT                        NONE

REAL(ReKi)                   :: ATol                                            ! Error tolerance for induction iteration.
!Start of proposed change.  v3.02.00a-mlb 03-August-2006  M. Buhl
REAL(ReKi)                   :: ATol2                                           ! The squared error tolerance for induction iteration.
!End of proposed change.  v3.02.00a-mlb 03-August-2006  M. Buhl
!Start of proposed change.  v3.03.02a-mlb 16-Nov-2009  M. Buhl
!Moved from NWTC Library
REAL(ReKi)                   :: AirDens                                         ! The air density at hub height.
REAL(ReKi)                   :: KinVisc                                         ! The kinematic viscosity at hub height.
!End of proposed change.  v3.03.02a-mlb 16-Nov-2009  M. Buhl
REAL(ReKi)                   :: SWTol                                           ! Error tolerance for skewed-wake iteration.

!Start Proposed Changes 3.03.01a00-dcm, 27-Jul-2009
!INTEGER                      :: IndType                                         ! Switch that tells which, if any, induction method is to be used.
LOGICAL                      :: IndType                                         ! Switch that tells which, if any, induction method is to be used.
!End Proposed Changes 3.03.01a00-dcm, 27-Jul-2009

LOGICAL                      :: AdvBrake                                        ! Flag that tells if to use the advanced brake state.
LOGICAL                      :: AIDrag                                          ! Flag that tells if to inclde the drag term in the axial-induction calculation.
!Start of proposed change.  v3.02.00c-mlb 24-August-2006  M. Buhl
!v3.02.00c-mlbLOGICAL                      :: Converge                                        ! Flag that says if we converged the induction loop.
LOGICAL                      :: Converge                                        ! Flag that says if we converged the induction loop enough to satisfy the convergence criterion.
!End of proposed change.  v3.02.00c-mlb 24-August-2006  M. Buhl
LOGICAL                      :: DimenInp                                        ! Flag that tells if input is dimensional or not.
LOGICAL                      :: DoSkew                                          ! Flag that says if we should calculate skewed-wake corrections for this case.
LOGICAL                      :: HubLoss                                         ! Flag that tells if to calculate the hub-loss correction factor.
!Start of proposed change.  v3.02.00a-mlb 03-August-2006  M. Buhl
!v3.02.00a-mlbLOGICAL                      :: IndProp                                         ! Flag that tells if PROP-PC induction is to be used instead of PROPX method.
!End of proposed change.  v3.02.00a-mlb 03-August-2006  M. Buhl
LOGICAL                      :: InputTSR                                        ! Flag that tells if wind parameters are for TSR or wind speed.
LOGICAL                      :: KFact                                           ! Flag that tells if input is in K (e.g., kN instead of N).
LOGICAL                      :: Metric                                          ! Flag that tells if input is in metric units.
LOGICAL                      :: OutCp                                           ! Flag that tells if to output the Cp.
LOGICAL                      :: OutFlp                                          ! Flag that tells if to output the flap bending moment.
!Start of proposed change.  v3.03.02a-mlb, 10-Dec-2009  M. Buhl
LOGICAL                      :: OutMaxCp                                        ! Flag that tells if to output the conditions leading to the maximum Cp.
!End of proposed change.  v3.03.02a-mlb, 10-Dec-2009  M. Buhl
!Start of proposed change.  v3.02.00c-mlb 24-August-2006  M. Buhl
LOGICAL                      :: OutNines                                        ! Flag that tells if to output nines for solutions the don't fully converge.
!End of proposed change.  v3.02.00c-mlb 24-August-2006  M. Buhl
LOGICAL                      :: OutPwr                                          ! Flag that tells if to output the power.
LOGICAL                      :: OutThr                                          ! Flag that tells if to output the thrust.
LOGICAL                      :: OutTrq                                          ! Flag that tells if to output the torque.
LOGICAL, ALLOCATABLE         :: PrntElem (:)                                    ! Array of flags to indicate which elements are to be printed to the blade-element file.
LOGICAL                      :: SkewWake                                        ! Flag that tells if to correct for skewed wakes.
!Start of proposed change.  v3.02.00c-mlb 24-August-2006  M. Buhl
LOGICAL                      :: Solution                                        ! Flag that says if we found a solution.
!End of proposed change.  v3.02.00c-mlb 24-August-2006  M. Buhl
LOGICAL                      :: SWconv                                          ! Flag that says if we converged the skewed-wake loop.
LOGICAL                      :: Swirl                                           ! Flag that tells if to calculate the tangential induction factor.
LOGICAL                      :: TabDel                                          ! Flag that tells if to delimit data using tabs.
LOGICAL                      :: TIDrag                                          ! Flag that tells if to inclde the drag term in the tangential-induction calculation.
LOGICAL                      :: TipLoss                                         ! Flag that tells if to calculate the tip-loss correction factor.
!Start Proposed Changes 3.03.01a00-dcm, 27-Jul-2009
LOGICAL                      :: TISingularity                                   ! Use the singularity avoidance method in the tangential-induction calculation?
!End Proposed Changes 3.03.01a00-dcm, 27-Jul-2009
!Start of proposed change.  v3.03.02a-mlb, 04-Dec-2009  M. Buhl
LOGICAL                      :: UnfPower                                        ! Flag that says if we should output power to an unformatted file for use with HARP_Opt.
!End of proposed change.  v3.03.02a-mlb, 04-Dec-2009  M. Buhl
LOGICAL                      :: WriteBED                                        ! Flag that tells if to output blade-element data.

CHARACTER( 11)               :: DateNow                                         ! Date shortly after the start of execution.
CHARACTER(  6)               :: FpLUnits                                        ! The string containing the units of force per unit length.
CHARACTER(  4)               :: FrcUnits                                        ! The string containing the units of force.
CHARACTER(200)               :: InpFile
CHARACTER(  2)               :: LenUnits                                        ! The string containing the units of length.
CHARACTER(  7)               :: MomUnits                                        ! The string containing the units of moment.
CHARACTER(  3)               :: MpLUnits                                        ! The string containing the units of mass per unit length.
CHARACTER(  1)               :: NullChar = CHAR( 0 )
CHARACTER(  5)               :: ParamStr (3) = (/'Omega' ,'Pitch' ,'TSR  '/)    ! The strings for the variable-parameter names.
CHARACTER(  2)               :: PwrUnits                                        ! The string containing the units of power.
CHARACTER( 13)               :: RealFmt
CHARACTER(200)               :: RootName                                        ! Root name of the input file.
CHARACTER(200)               :: RunTitle                                        ! The title of the run from the input file.
CHARACTER(  4)               :: SpdUnits                                        ! The string containing the units of speed.
CHARACTER(  9)               :: TextFmt  = '(  X,A10)'
CHARACTER(  9)               :: TextFmtI = '(  X,A12)'
CHARACTER(  8)               :: TimeNow                                         ! Time of day shortly after the start of execution.
CHARACTER(3)                 :: UnitsStr  (3) = (/ 'rpm', 'deg', '   ' /)       ! The strings for the variable-parameter units.


END MODULE ProgGen
!=======================================================================
MODULE Parameters


   ! This module stores constants.


USE                                NWTC_Library

IMPLICIT                           NONE

REAL(ReKi), PARAMETER           :: BadCp        =   -9.9999                     ! Value to display for Cp when induction iteration fails.
REAL(ReKi), PARAMETER           :: BadPwr       = -999.999                      ! Value to display for power when induction iteration fails.
REAL(ReKi), PARAMETER           :: Cos15        = 2.0**0.5*(3.0**0.5 - 1.0)/4.0 ! COS(15 degrees)
REAL(ReKi), PARAMETER           :: Cos15_2      = Cos15**2                      ! Square of COS(15 degrees)
!v3.01.01REAL(ReKi), PARAMETER           :: D2R          =    0.01745329                 ! Convertion from degrees to radians.
!Start of proposed change.  v3.02.00a-mlb 03-August-2006  M. Buhl
!End of proposed change.  v3.02.00a-mlb 03-August-2006  M. Buhl
!v3.02.00aREAL(ReKi), PARAMETER           :: Epsilon      =    1.0E-6                     ! A small number.
!End of proposed change.  v3.02.00a-mlb 03-August-2006  M. Buhl
REAL(ReKi), PARAMETER           :: FP2Nm        =    1.35582                    ! Convertion from foot-pounds to N-m.
REAL(ReKi), PARAMETER           :: M2Ft         =    3.280840                   ! Convertion from meters to feet.
!v3.01.01REAL(ReKi), PARAMETER           :: Pi           =    3.141593                   ! Pi
!v3.01.01REAL(ReKi), PARAMETER           :: R2D          =   57.29578                    ! Convertion from radians to degrees.
!v3.01.01REAL(ReKi), PARAMETER           :: RPM2RPS      =    0.1047198                  ! Convertion from rpm to rad/sec.
REAL(ReKi), PARAMETER           :: Sin15        = 2.0**0.5*(3.0**0.5 - 1.0)/4.0 ! SIN(15 degrees)
REAL(ReKi), PARAMETER           :: Sin15_2      = Sin15**2                      ! Square of SIN(15 degrees)
!v3.01.01REAL(ReKi), PARAMETER           :: TwoByPi      =    0.6366198                  ! 2/Pi.
!v3.01.01REAL(ReKi), PARAMETER           :: TwoPi        =    6.283185                   ! 2*Pi.

INTEGER, PARAMETER              :: ParStLen (3) = (/ 5, 5, 3 /)                 ! Length of the strings containing the parameter names (ParamStr).
INTEGER, PARAMETER              :: UnAF         = 3                             ! Unit for airfoil-data input files.
INTEGER, PARAMETER              :: UnBE         = 4                             ! Unit for blade-element output file.
!Start proposed changes. DCM 6-22-09  Version = 'v3.02.00k04-dcm, 22-Jun-2009'
INTEGER, PARAMETER              :: UnDB         = 11                            ! Unit for Debug Output File, 3.01.02d01-dcm, 09-Jun-2009
!End proposed changes. DCM 6-22-09  Version = 'v3.02.00k04-dcm, 22-Jun-2009'
INTEGER, PARAMETER              :: UnIn         = 1                             ! Unit for input file.
INTEGER, PARAMETER              :: UnOu         = 2                             ! Unit for main output file.
INTEGER, PARAMETER              :: UnSc         = 6                             ! Unit for the screen.
!Start of proposed change.  v3.03.02a-mlb, 04-Dec-2009  M. Buhl
INTEGER, PARAMETER              :: UnUn         = 8                             ! Unit for unformatted output file.
!End of proposed change.  v3.03.02a-mlb, 04-Dec-2009  M. Buhl

!Start proposed changes. DCM 6-22-09  Version = 'v3.02.00k04-dcm, 22-Jun-2009'
LOGICAL                         :: WriteDebug   = 1                             ! Logical for Debug Output File, 3.01.02d01-dcm,
!End proposed changes. DCM 6-22-09  Version = 'v3.02.00k04-dcm, 22-Jun-2009'


END MODULE Parameters
!=======================================================================
MODULE WTP_Data


   ! This module stores constants to specify the KIND of variables.


USE                                NWTC_Library

IMPLICIT                           NONE

TYPE                            :: Case                                         ! Declare new type that is an allocatable table of combined cases.
   REAL(ReKi)                   :: Cp                                           ! The output power coefficient for a given case.
   REAL(ReKi)                   :: FlapMom                                      ! The output flap moment speed for a given case.
   REAL(ReKi)                   :: Pitch                                        ! The input pitch for a given case.
   REAL(ReKi)                   :: Power                                        ! The output power for a given case.
   REAL(ReKi)                   :: RotSpeed                                     ! The input rotor speed for a given case.
   REAL(ReKi)                   :: Thrust                                       ! The output thrust speed for a given case.
   REAL(ReKi)                   :: Torque                                       ! The output torque for a given case.
   REAL(ReKi)                   :: TSR                                          ! The input tip-speed ratio for a given case.
   REAL(ReKi)                   :: WndSpeed                                     ! The input wind speed for a given case.
ENDTYPE Case

TYPE(Case), ALLOCATABLE         :: Cases    (:)                                 ! The table of combined cases.

TYPE(ElmTable), ALLOCATABLE     :: AF_Table (:)                                 ! The super-supertable of element supertables of tables with airfoil data.

REAL(ReKi)                      :: AFangD                                       ! The angle between the wind vector and the cone of rotation in degrees.
REAL(ReKi)                      :: AIDragM                                      ! Multiplier for Drag term in AI calculation.
REAL(ReKi)                      :: AlfaD                                        ! The angle of attack in degrees.
REAL(ReKi), ALLOCATABLE         :: AlfaStal (:)                                 ! The array describing the stall angle of attack for each segment.
REAL(ReKi), ALLOCATABLE         :: AlfShift (:)                                 ! The angle we should shift the potential Cl curve to make it tangent to the input Cl curve.
REAL(ReKi)                      :: AxInd                                        ! The axial induction factor.
REAL(ReKi)                      :: CdLoc                                        ! The coefficient of drag at the current analysis point.
REAL(ReKi), ALLOCATABLE         :: CdMin    (:)                                 ! The minimum Cd value in each Cd table.
REAL(ReKi), ALLOCATABLE         :: Chord    (:)                                 ! The array describing the chord distribution.
REAL(ReKi)                      :: ClLoc                                        ! The coefficient of lift at the current analysis point.
REAL(ReKi), POINTER             :: ColAry   (:)                                 ! The array holding all the column-parameter values.
REAL(ReKi)                      :: ConvFact                                     ! The wind-speed units conversion factor.
REAL(ReKi)                      :: ConvFrc                                      ! The conversion factor for force.
REAL(ReKi)                      :: ConvPwr                                      ! The conversion factor for power.
REAL(ReKi)                      :: ConvTrq                                      ! The conversion factor for torque.
REAL(ReKi)                      :: CosAF                                        ! The cosine of the airflow angle.
REAL(ReKi)                      :: CosAzim                                      ! The cosine of the current section.
REAL(ReKi)                      :: CosCone                                      ! The cosine of the precone angle.
REAL(ReKi)                      :: CosTilt                                      ! The cosine of the tilt angle.
REAL(ReKi)                      :: CosYaw                                       ! The cosine of the yaw error.
REAL(ReKi), ALLOCATABLE         :: CpAry    (:,:,:)                             ! The array to hold power coefficient for all parametric cases.
REAL(ReKi), ALLOCATABLE         :: DClShift (:)                                 ! The amount the Cl curve shifts upward when making it tangent to the input Cl curve.
REAL(ReKi), ALLOCATABLE         :: DelRLoc  (:)                                 ! The array of segment lengths.
REAL(ReKi)                      :: FlapMom                                      ! Flap bending moment.
REAL(ReKi), ALLOCATABLE         :: FlpAry   (:,:,:)                             ! The array to hold flap bending moment for all parametric cases.
REAL(ReKi)                      :: HalfRho                                      ! Half of the air density.
REAL(ReKi)                      :: HubHt                                        ! The height of the hub above ground level.
REAL(ReKi)                      :: HubHtND                                      ! The non-dimensional height of the hub above ground level.
REAL(ReKi)                      :: HubRad                                       ! The hub radius.
REAL(ReKi)                      :: HubRadND                                     ! The non-dimensional hub radius.
REAL(ReKi)                      :: IncidAng                                     ! The incidence angle between the chord line and the cone of rotation
REAL(ReKi)                      :: Loss                                         ! The product of the tip- and hub-loss correction factor (total loss).
REAL(ReKi)                      :: Omega                                        ! Rotor speed in radians per second.
REAL(ReKi), ALLOCATABLE, TARGET :: OmgAry   (:)                                 ! The array of rotor-speed settings.
REAL(ReKi)                      :: OmgDel                                       ! The difference in rotor-speed settings.
REAL(ReKi)                      :: OmgEnd                                       ! The final rotor-speed setting.
REAL(ReKi)                      :: OmgRPM                                          ! Rotor speed in rpm.
REAL(ReKi)                      :: OmgSt                                        ! The initial rotor-speed setting.
REAL(ReKi), ALLOCATABLE, TARGET :: PitAry   (:)                                 ! The array of pitch settings.
REAL(ReKi)                      :: Pitch                                        ! The pitch angle in radians.
REAL(ReKi)                      :: PitDeg                                          ! The pitch angle in degrees.
REAL(ReKi)                      :: PitDel                                       ! The difference in pitch settings.
REAL(ReKi)                      :: PitEnd                                       ! The final pitch setting.
REAL(ReKi)                      :: PitSt                                        ! The initial pitch setting.
REAL(ReKi)                      :: Power                                        ! Rotor power.
REAL(ReKi)                      :: PreCone                                      ! The precone half angle.
REAL(ReKi), ALLOCATABLE         :: PwrAry   (:,:,:)                             ! The array to hold power for all parametric cases.
REAL(ReKi)                      :: PwrC                                         ! Power coefficient.
REAL(ReKi)                      :: PwrConv                                      ! The conversion factor for power.
REAL(ReKi), ALLOCATABLE         :: RLoc     (:)                                 ! The array of segment centers (distance from hub).
REAL(ReKi), ALLOCATABLE         :: RLocND   (:)                                 ! The non-dimensional array of segment centers.
REAL(ReKi)                      :: RotorRad                                     ! The rotor radius.
REAL(ReKi), POINTER             :: RowAry   (:)                                 ! The array holding all the row-parameter values.
REAL(ReKi)                      :: Segs                                         ! NumSeg converted to REAL.
REAL(ReKi)                      :: ShearExp                                     ! The exponent of the wind shear.
REAL(ReKi)                      :: SinAF                                        ! The sine of the airflow angle.
REAL(ReKi)                      :: SinAzim                                      ! The sine of the current section.
REAL(ReKi)                      :: SinCone                                      ! The sine of the precone angle.
REAL(ReKi)                      :: SinTilt                                      ! The sine of the tilt angle.
REAL(ReKi)                      :: SinYaw                                       ! The sine of the yaw error.
REAL(ReKi)                      :: Solidity                                     ! The solidity of the current segment.
REAL(ReKi)                      :: Spd                                          ! Current value of wind speed or TSR.
REAL(ReKi), ALLOCATABLE, TARGET :: SpdAry   (:)                                 ! The array of wind-speed/TSR settings.
REAL(ReKi)                      :: SpdDel                                       ! The difference in wind-speed/TSR settings.
REAL(ReKi)                      :: SpdEnd                                       ! The final wind-speed/TSR setting.
REAL(ReKi)                      :: SpdRatio                                     ! The local speed ratio (wind speed/rotational speed).
REAL(ReKi)                      :: SpdSt                                        ! The initial wind-speed/TSR setting.
REAL(ReKi)                      :: SWconst                                      ! A constant used in the skewed-wake correction.
REAL(ReKi)                      :: SWcorr                                       ! The correction to axial induction due to skewed wake.
REAL(ReKi)                      :: SweptRad                                     ! Swept radius.
REAL(ReKi)                      :: SwpRadIn                                     ! Inverse of the swept radius.
REAL(ReKi)                      :: SwptArea                                     ! Swept area
REAL(ReKi), POINTER             :: TabAry   (:)                                 ! The array holding all the table-parameter values.
REAL(ReKi)                      :: TanInd                                       ! The tangential induction factor (swirl).
REAL(ReKi), ALLOCATABLE         :: Thick    (:)                                 ! The array describing the thickness distribution.
REAL(ReKi), ALLOCATABLE         :: ThrAry   (:,:,:)                             ! The array to hold thrust for all parametric cases.
REAL(ReKi)                      :: Thrust                                       ! Rotor thrust.
REAL(ReKi)                      :: TIDragM                                      ! Multiplier for Drag term in TI calculation.
REAL(ReKi)                      :: TipSpeed                                     ! Tip speed: rotor speed times swept rotor radius.
REAL(ReKi)                      :: Tilt                                         ! The tilt of the shaft with respect to horizontal.
REAL(ReKi)                      :: Torque                                       ! Rotor torque
REAL(ReKi), ALLOCATABLE         :: TrqAry   (:,:,:)                             ! The array to hold torque for all parametric cases.
REAL(ReKi)                      :: TrqC                                         ! Torque coefficient.
REAL(ReKi)                      :: TSR                                          ! Tip-speed ratio
REAL(ReKi), ALLOCATABLE         :: Twist    (:)                                 ! The array describing the twist distribution.
REAL(ReKi)                      :: VBodTang                                     ! The local tangential velocity in the body frame.
REAL(ReKi)                      :: VelHH                                        ! Hub-height wind speed.
REAL(ReKi)                      :: VInd2                                        ! The square of the total relative, induced (local) velocity.
REAL(ReKi)                      :: VIndNorm                                     ! Induced normal wind speed.
REAL(ReKi)                      :: VIndNrm2                                     ! Induced normal wind speed squared.
REAL(ReKi)                      :: VIndTang                                     ! Induced tangential wind speed.
REAL(ReKi)                      :: VTotNorm                                     ! The total relative wind speed normal the the chordline.
REAL(ReKi)                      :: VTotTang                                     ! The total relative wind speed parallel the the chordline.
REAL(ReKi)                      :: VWndGnd                                      ! The local wind speed in the ground reference system.
REAL(ReKi)                      :: Yaw                                          ! The rotor yaw error.

INTEGER, ALLOCATABLE            :: AFfile    (:)                                ! List of unique airfoil files.
!Start of proposed change.  v3.02.00c-mlb 24-August-2006  M. Buhl
INTEGER, SAVE                   :: NCases     = 0                               ! Number of cases analyzed.
!End of proposed change.  v3.02.00c-mlb 24-August-2006  M. Buhl
INTEGER                         :: DataSize (3)                                 ! Number of rows, columns, and grids of standard data output.
INTEGER, ALLOCATABLE            :: IndClBrk (:)                                 ! The index pointing to the break in the Cl curve for each aerodynamic table.
INTEGER                         :: MaxCol                                       ! Maximum number of columns in the output matrix.
INTEGER                         :: MaxIter                                      ! Maximum number of iterations to converge on the induction factor.
INTEGER                         :: MaxRow                                       ! Maximum number of rows in the output matrix.
INTEGER                         :: MaxTab                                       ! Maximum number of tables in the output matrix.
INTEGER                         :: NumBlade                                     ! Number of blades.
INTEGER                         :: NumCases                                     ! Number of combined-analysis cases.
INTEGER, ALLOCATABLE            :: NumCd    (:)                                 ! Number of points in each Cd table.
INTEGER, ALLOCATABLE            :: NumCl    (:)                                 ! Number of points in each Cl table.
INTEGER                         :: NumElmPr                                     ! Number of elements that will be printed to the blade-element file.
!Start of proposed change.  v3.02.00c-mlb 24-August-2006  M. Buhl
!v3.02.00c-mlb INTEGER                         :: NmItFail = 0                                 ! The number of cases in which the iteration did not converge.
INTEGER                         :: NmCaseFail = 0                               ! The number of cases in which no solution was found.
INTEGER                         :: NmCaseNC   = 0                               ! The number of cases in which the iteration did not fully converge on the exact solution.
!End of proposed change.  v3.02.00c-mlb 24-August-2006  M. Buhl
INTEGER                         :: NumOmg                                       ! Number of rotor speeds.
INTEGER                         :: NumPit                                       ! Number of Pitch settings.
INTEGER                         :: NumSect                                      ! Number of swept-area sectors.
INTEGER                         :: NumSeg                                       ! Number of blade segments.
INTEGER                         :: NumSpd                                       ! Number of wind speeds or TSRs.
INTEGER                         :: ParCol                                       ! The parameter to vary by columns.
INTEGER                         :: ParRow                                       ! The parameter to vary by rows.
INTEGER                         :: ParTab                                       ! The parameter to vary by tables.


END MODULE WTP_Data
!=======================================================================
MODULE WTP_Subs

!  DAVE: Clean up your code!  It is hard to read.  Make indents a consistent 3 characters--not 4, not 2, not 8, and especially not a negative value(!).  Ouch!
!        Line up equal signs.  Line up parts of equations so differences stand out.

   ! This module contains WT_Perf-specific subroutines and functions.

   ! It containes the following routines:

   !     SUBROUTINE AllocPar
   !     SUBROUTINE AllocProp
   !     SUBROUTINE CombCase
   !     SUBROUTINE GetAero      ( ISeg, AInd, TInd )
   !     SUBROUTINE GetData
   !     SUBROUTINE GetInds      ( IOmg, IPit, ISpd, IRow, ICol, ITab )
   !     SUBROUTINE GetVel       ( VWndNorm )
!Start of proposed change.  v3.03.02a-mlb, 10-Apr-2010,  M. Buhl
!v3.03.02a-mlb   !     SUBROUTINE InductBEM    ( ISeg, ZFound, Converg )                Dave didn't change this...
   !     SUBROUTINE InductBEM    ( ISeg, ZFound, Converg, AxIndPrevSeg, TanIndPrevSeg )
!End of proposed change.  v3.03.02a-mlb, 10-Apr-2010,  M. Buhl
   !        FUNCTION   AxIndErr     ( AInd )
   !        FUNCTION   FindZC       ( AxMin, AxMax, NumStep, ZFound, AxIndLo, AxIndHi )
   !        SUBROUTINE GetAFTI ( AInd, TInd, AF )
   !        SUBROUTINE NewtRaph     ( AxInd, TanInd )
   !        FUNCTION   TanIndErr    ( TInd )
   !     SUBROUTINE IOInit
   !     SUBROUTINE ParmAnal
   !     FUNCTION   Prandtl      ( R1, R2, SinAFA )
   !     SUBROUTINE RotAnal
   !     SUBROUTINE SetConv

USE                                NWTC_Library


CONTAINS

!=======================================================================
   SUBROUTINE AllocPar


      ! This routine allocates the WT_Perf parameter arrays.


   USE                             WTP_Data

   IMPLICIT                        NONE


      ! Local declarations.

   INTEGER                      :: Sttus                                        ! Status returned from an allocation attempt.



      ! Calculate the size of the parameter arrays and allocate them.

   IF ( OmgDel /= 0.0 )  THEN
      NumOmg = NINT( ( OmgEnd - OmgSt )/OmgDel ) + 1
   ELSE
      NumOmg = 1
   ENDIF

   IF ( PitDel /= 0.0 )  THEN
      NumPit = NINT( ( PitEnd - PitSt )/PitDel ) + 1
   ELSE
      NumPit = 1
   ENDIF

   IF ( SpdDel /= 0.0 )  THEN
      NumSpd = NINT( ( SpdEnd - SpdSt )/SpdDel ) + 1
   ELSE
      NumSpd = 1
   ENDIF

   CALL GetInds ( NumOmg, NumPit, NumSpd, MaxRow, MaxCol, MaxTab )

   ALLOCATE ( CpAry(MaxRow, MaxCol, MaxTab) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL Abort(' Error allocating memory for the CpAry array.')
   ENDIF

   ALLOCATE ( FlpAry(MaxRow, MaxCol, MaxTab) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL Abort(' Error allocating memory for the FlpAry array.')
   ENDIF

   ALLOCATE ( PwrAry(MaxRow, MaxCol, MaxTab) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL Abort(' Error allocating memory for the PwrAry array.')
   ENDIF

   ALLOCATE ( ThrAry(MaxRow, MaxCol, MaxTab) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL Abort(' Error allocating memory for the ThrAry array.')
   ENDIF

   ALLOCATE ( TrqAry(MaxRow, MaxCol, MaxTab) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL Abort(' Error allocating memory for the TrqAry array.')
   ENDIF

   ALLOCATE ( RowAry(MaxRow) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL Abort(' Error allocating memory for the RowAry array.')
   ENDIF

   ALLOCATE ( ColAry(MaxCol) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL Abort(' Error allocating memory for the ColAry array.')
   ENDIF

   ALLOCATE ( TabAry(MaxTab) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL Abort(' Error allocating memory for the TabAry array.')
   ENDIF

   ALLOCATE ( OmgAry(NumOmg) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL Abort(' Error allocating memory for the OmgAry array.')
   ENDIF

   ALLOCATE ( PitAry(NumPit) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL Abort(' Error allocating memory for the PitAry array.')
   ENDIF

   ALLOCATE ( SpdAry(NumSpd) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL Abort(' Error allocating memory for the SpdAry array.')
   ENDIF


   RETURN
   END SUBROUTINE AllocPar
!=======================================================================
   SUBROUTINE AllocProp


      ! This routine allocates the WT_Perf property arrays.


   USE                             ProgGen
   USE                             WTP_Data

   IMPLICIT                        NONE


      ! Local declarations.

   INTEGER                      :: Sttus                                        ! Status returned from an allocation attempt.



   ALLOCATE ( AlfaStal(NumSeg) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL Abort(' Error allocating memory for the AlfaStal array.')
   ENDIF

   ALLOCATE ( AlfShift(NumSeg) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL Abort(' Error allocating memory for the AlfShift array.')
   ENDIF

   ALLOCATE ( CdMin(NumSeg) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL Abort(' Error allocating memory for the CdMin array.')
   ENDIF

   ALLOCATE ( Chord(NumSeg) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL Abort(' Error allocating memory for the Chord array.')
   ENDIF

   ALLOCATE ( DClShift(NumSeg) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL Abort(' Error allocating memory for the DClShift array.')
  ENDIF

   ALLOCATE ( DelRLoc(NumSeg) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL Abort(' Error allocating memory for the DelRLoc array.')
   ENDIF

   ALLOCATE ( IndClBrk(NumSeg) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL Abort(' Error allocating memory for the IndClBrk array.')
  ENDIF

   ALLOCATE ( NumCd(NumSeg) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL Abort(' Error allocating memory for the NumCd array.')
   ENDIF

   ALLOCATE ( NumCl(NumSeg) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL Abort(' Error allocating memory for the NumCl array.')
   ENDIF

   ALLOCATE ( PrntElem(NumSeg) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL Abort(' Error allocating memory for the PrntElem array.')
   ENDIF

   ALLOCATE ( RLoc(NumSeg) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL Abort(' Error allocating memory for the RLoc array.')
   ENDIF

   ALLOCATE ( RLocND(NumSeg) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL Abort(' Error allocating memory for the RLocND array.')
   ENDIF

   ALLOCATE ( Thick(NumSeg) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL Abort(' Error allocating memory for the Thick array.')
   ENDIF

   ALLOCATE ( Twist(NumSeg) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL Abort(' Error allocating memory for the Twist array.')
   ENDIF


   RETURN
   END SUBROUTINE AllocProp
!=======================================================================
   SUBROUTINE CombCase


      ! This routine performs the analysis for the combined cases.


   USE                             Parameters
   USE                             ProgGen
   USE                             WTP_Data

   IMPLICIT                        NONE


      ! Local declarations.

   INTEGER                      :: ICase                                        ! Index for the combined case.

   CHARACTER( 17), PARAMETER    :: FmtDaSpc = "( 8F12.3,2F12.4 )"
   CHARACTER( 29), PARAMETER    :: FmtDaTab = "( 8(F12.3,'"//Tab//"'),2(F12.4,'"//Tab//"') )"
   CHARACTER(126), PARAMETER    :: FmtHdSpc = "( '   WindSpeed         TSR  RotorSpeed       Pitch       Power      Torque" &
                                            //"      Thrust  FlapMoment          Cp          Cq' )"
   CHARACTER( 73), PARAMETER    :: FmtHdTab = "( 'WindSpeed"//Tab//"TSR"//Tab//"RotorSpeed"//Tab//"Pitch"//Tab//"Power"//Tab &
                                            //"Torque"//Tab//"Thrust"//Tab//"FlapMoment"//Tab//"Cp"//Tab//"Cq' )"
   CHARACTER( 64), PARAMETER    :: FmtUnSpc = "(8X,A,11X,'-',9X,'rpm',9X,'deg',10X,A,5X,A,8X,A,5X,A,2(11X,'-'))"
   CHARACTER( 44), PARAMETER    :: FmtUnTab = "(A,'"//Tab//"-"//Tab//"rpm"//Tab//"deg"//Tab//"',A,'"//Tab//"',A,'"//Tab//"',A,'" &
                                            //Tab//"',A,'"//Tab//"-"//Tab//"-')"



      ! Write header to output file.

   WRITE (UnOu,'(A)')  'Results generated by '//TRIM( ProgName )//TRIM( ProgVer )//' for input file "'//TRIM( InpFile )//'".'
   WRITE (UnOu,'(A)')  'Generated on '//TRIM( DateNow )//' at '//TRIM( TimeNow )//'.'
   WRITE (UnOu,'(A)')  'Input file title:'
   WRITE (UnOu,'(A)')  '  '//TRIM( RunTitle )
   WRITE (UnOu,'(A)')  ' '
   WRITE (UnOu,'(A)')  ' '

   IF ( TabDel )  THEN
      WRITE (UnOu,FmtHdTab)
      WRITE (UnOu,FmtUnTab)  TRIM( ADJUSTL( SpdUnits ) ), TRIM( ADJUSTL( PwrUnits ) ), TRIM( ADJUSTL( MomUnits ) ), &
                             TRIM( ADJUSTL( FrcUnits ) ), TRIM( ADJUSTL( MomUnits ) )
   ELSE
      WRITE (UnOu,FmtHdSpc)
      WRITE (UnOu,FmtUnSpc)  SpdUnits, PwrUnits, MomUnits, FrcUnits, MomUnits
   ENDIF


      ! Run all the combined cases.

   DO ICase=1,NumCases

      Spd      = Cases(ICase)%WndSpeed
      TSR      = Cases(ICase)%TSR
      OmgRPM   = Cases(ICase)%RotSpeed
      PitDeg   = Cases(ICase)%Pitch

      VelHH    = Spd*ConvFact
      Omega    = OmgRPM*RPM2RPS
      TipSpeed = Omega*SweptRad
      Pitch    = PitDeg*D2R

      Call RotAnal


         ! Write results to output file.

      IF ( TabDel )  THEN
         WRITE (UnOu,FmtDaTab)  Spd, TSR, OmgRPM, PitDeg, ConvPwr*Power, ConvTrq*Torque, ConvFrc*Thrust, ConvTrq*FlapMom, PwrC, TrqC
      ELSE
         WRITE (UnOu,FmtDaSpc)  Spd, TSR, OmgRPM, PitDeg, ConvPwr*Power, ConvTrq*Torque, ConvFrc*Thrust, ConvTrq*FlapMom, PwrC, TrqC
      ENDIF

   ENDDO ! ICase



   RETURN
   END SUBROUTINE CombCase
!End of proposed change.  v3.02.00h-mlb 11-Sept-2006  M. Buhl
!Moved from inside InductBEM and modified.
!-----------------------------------------------------------------------
   SUBROUTINE GetAero ( ISeg, AInd, TInd )


      ! Get some aerodynamic information.


   USE                             ProgGen
   USE                             WTP_Data


      ! Argument declarations.

   REAL(ReKi), INTENT(IN)       :: AInd                                         ! The axial-induction factor.
   REAL(ReKi), INTENT(IN)       :: TInd                                         ! The tangential-induction factor.

   INTEGER, INTENT(IN)          :: ISeg                                         ! The segment number.


      ! Local declarations.

   REAL(ReKi)                   :: AFang                                        ! The angle between the wind vector and the cone of rotation in radians.
   REAL(ReKi)                   :: AlfaR                                        ! The angle of attack in radians.
   REAL(ReKi)                   :: CmLoc                                        ! The local pitching-moment coefficient (unused).
   REAL(ReKi)                   :: CnLoc                                        ! The local normal coefficient.
   REAL(ReKi)                   :: CtLoc                                        ! The local tangential coefficient.
   REAL(ReKi)                   :: LossHub  = 1.0                               ! Hub-loss correction factor.
   REAL(ReKi)                   :: LossTip  = 1.0                               ! Tip-loss correction factor.
   REAL(ReKi)                   :: Re                                           ! Reynolds number.
   REAL(ReKi)                   :: VInd                                         ! The total relative wind speed.


      ! External references.

  ! REAL(ReKi)                   :: Prandtl                                      ! The Prandtl tip.hub-loss function. -Commented 6-4-09 DCM



      ! Apply the induction factors to the speeds.

   VIndTang = VTotTang*( 1.0 + TInd )
   VIndNorm = VTotNorm*( 1.0 - AInd*SWcorr )
   VIndNrm2 = VIndNorm**2
   VInd2    = VIndTang**2 + VIndNrm2
   VInd     = SQRT( VInd2 )
!v3.02.00c-mlb print *, "     GetAero:", VIndNorm, VTotNorm, AInd,SWcorr


      ! Calculate the airflow angle.

   AFang  = ATAN2( VIndNorm, VIndTang )
   AFangD = AFang*R2D
   CosAF  = COS( AFang )
   SinAF  = SIN( AFang )


      ! Calculate the angle of attack.  Ensure that it is within +/- 180 degrees.

   AlfaR   = AFang - IncidAng
   AlfaD   = AlfaR*R2D

   IF ( AlfaD > 180.0 )  THEN
      AlfaD = AlfaD - 360.0
   ELSEIF ( AlfaD < -180.0 )  THEN
      AlfaD = AlfaD + 360.0
   ENDIF


      ! Tip-loss calculation.

   IF ( TipLoss )  LossTip = Prandtl( 1.0, RLocND(ISeg), SinAF )


      ! Don't calculate the hub loss if there is no hub.

   IF ( HubLoss .AND. ( HubRadND .GT. EPSILON( HubRadND ) ) )  LossHub = Prandtl( RLocND(ISeg), HubRadND, SinAF )


      ! Total the losses.

   Loss = LossTip*LossHub


      ! Calculate Reynolds number.

   Re = VInd*Chord(ISeg)*RotorRad/KinVisc


      ! Calculate Cn and Ct for the local segment.

!Start of proposed change.  v3.04.00a-mlb, 12-Apr-2010,  M. Buhl
!v3.04.00a   IF ( AIDrag .OR. TIDrag )  THEN
!v3.04.00a      CALL GetCoefs ( ISeg, AlfaD, Re, AF_Table(AFfile(ISeg)), ClLoc, CdLoc, CmLoc, CmLoc, .TRUE., .TRUE., .FALSE. )
!v3.04.00a   ELSE
!v3.04.00a      CALL GetCoefs ( ISeg, AlfaD, Re, AF_Table(AFfile(ISeg)), ClLoc, CdLoc, CmLoc, .TRUE., .FALSE., .FALSE. )
!v3.04.00a      CdLoc = 0.0
!v3.04.00a   ENDIF
   IF ( AIDrag .OR. TIDrag )  THEN
      CALL GetCoefs ( ISeg, AlfaD, Re, AF_Table(AFfile(ISeg)), ClLoc, CdLoc, CmLoc, CpminLoc, .TRUE., .TRUE. , .FALSE., .FALSE. )
   ELSE
      CALL GetCoefs ( ISeg, AlfaD, Re, AF_Table(AFfile(ISeg)), ClLoc, CdLoc, CmLoc, CpminLoc, .TRUE., .FALSE., .FALSE., .FALSE. )
      CdLoc = 0.0
   ENDIF
!End of proposed change.  v3.04.00a-mlb, 12-Apr-2010,  M. Buhl

   IF ( AIDrag )  THEN
      CnLoc = ClLoc*CosAF + CdLoc*SinAF
   ELSE
      CnLoc = ClLoc*CosAF
   ENDIF

   IF ( TIDrag )  THEN
      CtLoc = ClLoc*SinAF - CdLoc*CosAF
   ELSE
      CtLoc = ClLoc*SinAF
   ENDIF


   RETURN
   END SUBROUTINE GetAero ! ( ISeg, AInd, TInd )
!End of proposed change.  v3.02.00h-mlb 11-Sept-2006  M. Buhl
!=======================================================================
   SUBROUTINE GetData


      ! This routine opens the gets the data from the input files.


   USE                             Parameters
   USE                             ProgGen
   USE                             WTP_Data

   IMPLICIT                        NONE


      ! Local declarations.

   REAL(ReKi)                   :: BldLen                                       ! Blade length.
   REAL(ReKi)                   :: InpCase   (3)                                ! Temporary array to hold combined-case input parameters.
   REAL(ReKi)                   :: OmgSets   (3)                                ! Temporary array to hold omega-setting parameters.
   REAL(ReKi)                   :: PitSets   (3)                                ! Temporary array to hold pitch-setting parameters.
   REAL(ReKi)                   :: SpdSets   (3)                                ! Temporary array to hold speed-setting parameters.

   INTEGER                      :: IAF                                          ! Index for input airfoil table.
   INTEGER                      :: ICase                                        ! Index for combined-analysis case.
   INTEGER                      :: IOmg                                         ! Index into the OmgAry array.
   INTEGER                      :: IOS                                          ! I/O status result.
   INTEGER                      :: IPit                                         ! Index into the PitAry array.
   INTEGER                      :: ISeg                                         ! The blade-segment number.
   INTEGER                      :: ISpd                                         ! Index into the SpdAry array.
   INTEGER                      :: NumAF                                        ! The number of unique (input) airfoil files.
   INTEGER                      :: Sttus                                        ! The status returned from allocation attempts.

   CHARACTER(200)               :: AF_File                                      ! String containing the name of an aifoil file.
   CHARACTER(200)               :: InpVersn                                     ! String containing the input-version information.
   CHARACTER(200)               :: Line                                         ! String containing a line of input.
   CHARACTER(200)               :: SubTitle                                     ! String containing the RunTitle of a subsection.



      ! Skip a line, read the run title and the version information.

   READ (UnIn,'(/,A,/,A)',IOSTAT=IOS)  RunTitle, InpVersn

   IF ( IOS < 0 )  CALL PremEOF ( InpFile , 'RunTitle' )

!Start of proposed change.  v3.02.00c-mlb 24-August-2006  M. Buhl
!v3.02.00c-mlb   CALL WrScr1   ( RunTitle )
   CALL WrScr1 ( ' '//RunTitle )
!End of proposed change.  v3.02.00c-mlb 24-August-2006  M. Buhl


      ! Read in the title line for the input-configuration subsection.

   READ (UnIn,'(A)',IOSTAT=IOS)  SubTitle

   IF ( IOS < 0 )  CALL PremEOF ( InpFile , 'the input-configuration subtitle' )

      ! See if we should echo the output.

    READ (UnIn,*,IOSTAT=IOS)  Echo

   IF ( Echo )  THEN
      CALL OpenFOutFile ( UnEc, TRIM( RootName )//'.ech' )
      WRITE (UnEc,'(A)')                        'Echo of WT_Perf Input File:'
      WRITE (UnEc,'(A)')                        ' "'//TRIM( InpFile )//'"'
      WRITE (UnEc,'(A)')                        'Generated on: '//TRIM( DateNow )//' at '//TRIM( TimeNow )//'.'
      WRITE (UnEc,'(A,/,A)')                    RunTitle, InpVersn
      WRITE (UnEc,'(A)')                        SubTitle
      WRITE (UnEc,"(2X,L11,2X,A,T27,' - ',A)")  Echo, 'Echo', 'Echo input parameters to "echo.out"?'
   ENDIF


      ! Read the rest of input-configuration section.

   CALL ReadVar ( UnIn, InpFile, DimenInp, 'DimenInp', 'Turbine parameters are dimensional?'         )
   CALL ReadVar ( UnIn, InpFile, Metric,   'Metric',   'Turbine parameters are Metric (MKS vs FPS)?' )


      ! Read the model-configuration section.

   CALL ReadCom  ( UnIn, InpFile,                       'the model-configuration subtitle'                )
   CALL ReadVar ( UnIn, InpFile, NumSect,  'NumSect',  'Number of circumferential sectors.'              )
   CALL ReadVar ( UnIn, InpFile, MaxIter,  'MaxIter',  'Max number of iterations for induction factor.'  )
   CALL ReadVar ( UnIn, InpFile, ATol,     'ATol',     'Error tolerance for induction iteration.'        )
   CALL ReadVar ( UnIn, InpFile, SWTol,    'SWTol',    'Error tolerance for skewed-wake iteration.'      )

   ATol2 = ATol**2


      ! Check for valid choices.

   IF ( NumSect < 1 )  THEN
      CALL Abort ( ' Variable "NumSect" must be greater than 0.  Instead, it is "'//Trim( Int2LStr( NumSect ) )//'".' )
   ENDIF
   IF ( MaxIter < 1 )  THEN
      CALL Abort ( ' Variable "MaxIter" must be greater than 0.  Instead, it is "'//Trim( Int2LStr( MaxIter ) )//'".' )
   ENDIF

   IF ( ATol <= 0.0 )  THEN
      CALL Abort ( ' Variable "ATol" must be greater than 0.  Instead, it is "'//Trim( Flt2LStr( ATol ) )//'".' )
   ENDIF

   IF ( SWTol <= 0.0 )  THEN
      CALL Abort ( ' Variable "SWTol" must be greater than 0.  Instead, it is "'//Trim( Flt2LStr( SWTol ) )//'".' )
   ENDIF


      ! Read the algorithm-configuration section.

   CALL ReadCom  ( UnIn, InpFile,                       'the algorithm-configuration subtitle'                           )
   CALL ReadVar ( UnIn, InpFile, TipLoss,  'TipLoss',  'Use the Prandtl tip-loss model?'                                )
   CALL ReadVar ( UnIn, InpFile, HubLoss,  'HubLoss',  'Use the Prandtl hub-loss model?'                                )
   CALL ReadVar ( UnIn, InpFile, Swirl,    'Swirl',    'Include Swirl effects?'                                         )
   CALL ReadVar ( UnIn, InpFile, SkewWake, 'SkewWake', 'Apply skewed-wake correction?'                                  )
!Start of proposed change.  v3.02.00b-mlb 22-August-2006  M. Buhl
!v3.02.00b-mlb   CALL ReadVar ( UnIn, InpFile, AdvBrake, 'AdvBrake', 'Use the advanced brake-state model?'                            )
!End of proposed change.  v3.02.00b-mlb 22-August-2006  M. Buhl
!Start of proposed change.  v3.02.00a-mlb 03-August-2006  M. Buhl
!v3.02.00a-mlb   CALL ReadVar ( UnIn, InpFile, IndProp,  'IndProp',  'Use PROP-PC instead of PROPX induction algorithm?'              )
!Start of proposed change.  v3.02.00b-mlb 22-August-2006  M. Buhl
!v3.02.00b-mlb   CALL ReadVar ( UnIn, InpFile, IndType,  'IndType',  'Use: 0-None, 1-PROPPC, 2-PROPX induction algorithm.'            )
  ! CALL ReadVar ( UnIn, InpFile, IndType,  'IndType',  'Use: 0-None, 1-BEM induction algorithm.'                        )
!3.03.01a00-dcm
             CALL ReadVar ( UnIn, InpFile, IndType,  'IndType',  'Use BEM induction algorithm?'                        )
!End of proposed change.  v3.02.00b-mlb 22-August-2006  M. Buhl
!End of proposed change.  v3.02.00a-mlb 03-August-2006  M. Buhl
   CALL ReadVar ( UnIn, InpFile, AIDrag,   'AIDrag',   'Include the drag term in the axial-induction calculation?'      )
   CALL ReadVar ( UnIn, InpFile, TIDrag,   'TIDrag',   'Include the drag term in the tangential-induction calculation?' )


   IF ( AiDrag )  THEN
      AiDragM = 1.0
   ELSE
      AiDragM = 0.0
   ENDIF

   IF ( TiDrag )  THEN
      TiDragM = 1.0
   ELSE
      TiDragM = 0.0
   ENDIF

CALL ReadVar ( UnIn, InpFile, TISingularity,   'TISingularity',   'Use the singularity avoidance method in the tangential-induction calculation?' )

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

IF ( IndType == 0 )  THEN    ! Don't do skewed wakes if induction calculations are disabled.
      SkewWake = .FALSE.
   ENDIF

!End Proposed Changes 3.03.01a00-dcm, 27-Jul-2009

      ! Read the turbine-data section.

   CALL ReadCom ( UnIn, InpFile, 'the turbine-data subtitle' )
   CALL ReadVar ( UnIn, InpFile, NumBlade, 'NumBlade', 'Number of blades.'     )
   CALL ReadVar ( UnIn, InpFile, RotorRad, 'RotorRad', 'Unconed rotor radius.' )
   CALL ReadVar ( UnIn, InpFile, HubRad,   'HubRad',   'Hub radius.' )
   CALL ReadVar ( UnIn, InpFile, PreCone,  'PreCone',  'Cone angle.' )
   CALL ReadVar ( UnIn, InpFile, Tilt,     'Tilt',     'Shaft tilt.' )
   CALL ReadVar ( UnIn, InpFile, Yaw,      'Yaw',      'Yaw error.' )
   CALL ReadVar ( UnIn, InpFile, HubHt,    'HubHt',    'Hub height.' )

   IF ( DimenInp )  THEN
      HubRadND = HubRad/RotorRad
      HubHtND  = HubHt /RotorRad
   ELSE
!Start of proposed change.  v3.04.00c-mlb, 12-Jan-2011,  M. Buhl
      HubRadND = HubRad
      HubHtND  = HubHt
!End of proposed change.  v3.04.00c-mlb, 12-Jan-2011,  M. Buhl
      HubRad = HubRadND*RotorRad
      HubHt  = HubHtND *RotorRad
   ENDIF

   BldLen  = RotorRad - HubRad
   PreCone = PreCone*D2R
   SinCone = SIN( PreCone )
   CosCone = COS( PreCone )

   Tilt    = Tilt*D2R
   CosTilt = COS( Tilt )
   SinTilt = SIN( Tilt )

   Yaw    = Yaw*D2R
   CosYaw = COS( Yaw )
   SinYaw = SIN( Yaw )

   IF ( .NOT. SkewWake .OR. ( ( Yaw == 0.0 ) .AND. ( Tilt == 0.0 ) ) )  THEN
      DoSkew   = .FALSE.
      SkewWake = .FALSE.
      SWconst  = 0.0
      SWconv   = .TRUE.
      SWcorr   = 1.0
   ENDIF


      ! Read in the number of segments and allocate the property arrays.

   CALL ReadVar ( UnIn, InpFile, NumSeg,   'NumSeg',   'Number of blade segments (entire rotor radius).' )

   IF ( NumSeg < 1 )  CALL Abort ( ' Variable "NumSeg" must be greater than 0.  Instead, it is "'//Int2LStr( NumSeg )//'".' )

   CALL AllocProp

   ALLOCATE ( AFfile(NumSeg) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL Abort(' Error allocating memory for the AFfile array.')
   ENDIF


      ! Read in the distributed blade properties.

   CALL ReadCom  ( UnIn, InpFile, 'the header for the blade-element data' )

   NumElmPr = 0

   DO ISeg=1,NumSeg

      READ (UnIn,'(A)',IOSTAT=IOS)  Line

      CALL CheckIOS ( IOS, InpFile, 'line #'//TRIM( Int2LStr( ISeg ) )//' of the blade-element data table.' , StrType )

      READ (Line,*,IOSTAT=IOS)  RLoc(ISeg), Twist(ISeg), Chord(ISeg), AFfile(ISeg), PrntElem(ISeg)

      CALL CheckIOS ( IOS, InpFile, 'line #'//TRIM( Int2LStr( ISeg ) )//' of the blade-element data table.' , NumType )

      IF ( Chord(ISeg) <= 0.0 )  THEN
         CALL Abort ( ' The chord for segment #'//Trim( Int2LStr( NumSect ) )//' must be > 0.  Instead, it is "' &
                    //Trim( Flt2LStr( Chord(ISeg) ) )//'".' )
      ENDIF


         ! Convert to or from dimensional data.

      IF ( DimenInp )  THEN
         RLocND(ISeg) = RLoc  (ISeg)/RotorRad
         Chord (ISeg) = Chord (ISeg)/RotorRad
      ELSE
         RLocND(ISeg) = RLoc(ISeg)
         RLoc  (ISeg) = RLocND(ISeg)*RotorRad
      ENDIF

      Twist(ISeg) = Twist(ISeg)*D2R

      IF ( PrntElem(ISeg) )  NumElmPr = NumElmPr +1

   ENDDO ! ISeg


      ! Compute the segment lengths and check their validity.

   CALL CompDR ( NumSeg, RLoc, HubRad, RotorRad, DimenInp, DelRLoc )


       ! The Tim Olsen memorial hub-radius check.

   IF ( ( HubRadND < 0.0 ) .OR. ( 1.0 <= HubRadND ) )  THEN
      CALL Abort ( ' The hub radius must be positive and less than the rotor radius.  Instead it is ' &
                 //TRIM( Flt2LStr( HubRad ) )//' '//TRIM( LenUnits )//'.' )
   ENDIF


      ! Make sure hub is high enough so the blade doesn't hit the ground.  We wouldn't want to get it dirty.  :-)

   IF ( HubHt*CosCone*CosTilt .LT. 1.0 )  CALL Abort ( ' The hub is so low, the blade will hit the ground.' )


      ! Read the aerodynamic-data section.

   CALL ReadCom ( UnIn, InpFile, 'the aerodynamic-data subtitle'   )
   CALL ReadVar ( UnIn, InpFile, AirDens,  'AirDens',  'Air density.' )
   CALL ReadVar ( UnIn, InpFile, KinVisc,  'KinVisc',  'Kinesmatic viscosity.' )
   CALL ReadVar ( UnIn, InpFile, ShearExp, 'ShearExp', 'Shear exponent.' )
   CALL ReadVar ( UnIn, InpFile, UseCm,    'UseCm',    'Cm data included in airfoil tables?' )
!Start of proposed change.  v3.03.02a-mlb, 10-Apr-2010,  M. Buhl
   CALL ReadVar ( UnIn, InpFile, UseCpmin, 'UseCpmin', 'Cp,min data included in airfoil tables?' )
!End of proposed change.  v3.03.02a-mlb, 10-Apr-2010,  M. Buhl
   CALL ReadVar ( UnIn, InpFile, NumAF,    'NumAF',    'Number of unique airfoil tables.'     )

   IF ( AirDens <= 0.0 )  CALL Abort ( ' The air density must be greater than zero.' )
   IF ( KinVisc <= 0.0 )  CALL Abort ( ' The kinesmatic viscosity must be greater than zero.' )


      ! Check the list of airfoil tables to make sure they are all within limits.

   IF ( NumAF < 1 )  CALL Abort ( ' The number of unique airfoil tables (NumAF) must be greater than zero.' )

   DO ISeg=1,NumSeg
      IF ( ( AFfile(ISeg) < 1 ) .OR. ( AFfile(ISeg) > NumAF ) )  THEN
         CALL Abort ( ' Segment #'//TRIM( Int2LStr( ISeg ) )//' requested airfoil input table #'//TRIM( Int2LStr( AFfile(ISeg) ) ) &
                    //'.  However, it must be between 1 and NumAF (='//TRIM( Int2LStr( NumAF ) )//'), inclusive.' )
      ENDIF
   ENDDO ! ISeg


      ! Allocate the airfoil data super-supertables for both unique data and complete data.

   ALLOCATE ( AF_Table(NumAF) , STAT=Sttus )
   IF ( Sttus /= 0 )  THEN
      CALL Abort ( ' Error allocating memory for the AF_Uniq super-supertable in T_GetAF.' )
   ENDIF


      ! Read in NumAF unique airfoil data files.

   DO IAF=1,NumAF

      CALL ReadVar ( UnIn, InpFile, AF_File, 'AF_File', 'Airfoil file #'//TRIM( Int2LStr( IAF ) )//'.' )
      CALL GetAF   ( AF_File, AF_Table(IAF), IAF )

   ENDDO


      ! Make sure we have a minimum of four sectors if we have shear, shaft tilt, or yaw.

   IF (  ( Tilt /= 0.0 ) .OR. ( Yaw /= 0.0 ) .OR. ( ShearExp /= 0.0 ) )  THEN
      NumSect = MAX( NumSect, 4 )
   ELSE
      NumSect = 1
   ENDIF


      ! Read the I/O-configuration section.

   CALL ReadCom ( UnIn, InpFile, 'the I/O-configuration subtitle'                                                                )
!Start of proposed change.  v3.03.02a-mlb, 04-Dec-2009  M. Buhl
   CALL ReadVar ( UnIn, InpFile, UnfPower, 'UnfPower', 'Write Power to an unformatted file?'                                     )
!End of proposed change.  v3.03.02a-mlb, 04-Dec-2009  M. Buhl
   CALL ReadVar ( UnIn, InpFile, TabDel,   'TabDel',   'Make output tab-delimited (fixed-width otherwise)?'                      )
!Start of proposed change.  v3.02.00c-mlb 24-August-2006  M. Buhl
   CALL ReadVar ( UnIn, InpFile, OutNines, 'OutNines', 'Output nines for cases that fail to satisfy the convergence criterion?'  )
!Start of proposed change.  v3.03.02a-mlb, 01-Dec-2009  M. Buhl
   CALL ReadVar ( UnIn, InpFile, Beep,     'Beep',     'Beep on exit?'                                                           )
!End of proposed change.  v3.03.02a-mlb, 01-Dec-2009  M. Buhl
!#IFDEF debug2
!#print *, "GetData:   Solution, Converge, OutNines = ", Solution, Converge, OutNines
!#ENDIF
!End of proposed change.  v3.02.00c-mlb 24-August-2006  M. Buhl
   CALL ReadVar ( UnIn, InpFile, KFact,    'KFact',    'Output dimensional parameters in K?'                                     )
   CALL ReadVar ( UnIn, InpFile, WriteBED, 'WriteBED', 'Write out blade element data to "bladelem.dat"?'                         )
   CALL ReadVar ( UnIn, InpFile, InputTSR, 'InputTSR', 'Input speeds as TSRs?'                                                   )
!Start of proposed change.  v3.03.02a-mlb, 10-Dec-2009  M. Buhl
   CALL ReadVar ( UnIn, InpFile, OutMaxCp, 'OutMaxCp', 'Output conditions leading to maximum Cp?'                                )
!End of proposed change.  v3.03.02a-mlb, 10-Dec-2009  M. Buhl
   CALL ReadVar ( UnIn, InpFile, SpdUnits, 'SpdUnits', 'Wind-speed units (mps, fps, mph).'                                       )


      ! Set units conversion and compute TSR parameters.

   CALL SetConv


         ! No sense creating a BED file if we're not putting anything in it.

      IF ( NumElmPr == 0 )  WriteBED = .FALSE.


      ! Read the combined-case section.

   CALL ReadCom  ( UnIn, InpFile,                       'the combined-case subtitle'     )
   CALL ReadVar  ( UnIn, InpFile, NumCases, 'NumCases', 'Number of cases to run.'        )
   CALL ReadCom  ( UnIn, InpFile,                       'the combined-case-block header' )

   IF ( NumCases < 0 )  THEN

      CALL Abort ( ' Variable "NumCases" must be >= 0.  Instead, it is "'//TRIM( Int2LStr( NumCases ) )//'".' )

   ELSEIF ( NumCases > 0 )  THEN

      ALLOCATE ( Cases(NumCases) , STAT=Sttus )
      IF ( Sttus /= 0 )  THEN
         CALL Abort(' Error allocating memory for the Cases array.')
      ENDIF

      DO ICase=1,NumCases

         CALL ReadRAry ( UnIn, InpFile, InpCase,  3, 'InpCase',  'Wind Speed or TSR, Rotor Speed, and Pitch for Case #' &
                       //TRIM( Int2LStr( ICase ) )//'.' )

         IF ( InputTSR )  THEN
            Cases(ICase)%TSR      = InpCase(1)
            Cases(ICase)%WndSpeed = RotorRad*InpCase(2)*Pi/( 30.0*InpCase(1) )
         ELSE
            Cases(ICase)%TSR      = RotorRad*InpCase(2)*Pi/( 30.0*InpCase(1) )
            Cases(ICase)%WndSpeed = InpCase(1)
         ENDIF

         Cases(ICase)%RotSpeed = InpCase(2)
         Cases(ICase)%Pitch    = InpCase(3)

      ENDDO ! ICase

   ELSE ! ( NumCases ==0 )


         ! Read the parametric-analysis-configuration section.

      CALL ReadCom ( UnIn, InpFile,                   'the parametric-analysis-configuration subtitle'  )
      CALL ReadVar ( UnIn, InpFile, ParRow, 'ParRow', 'Row parameter    (1-rpm, 2-pitch, 3-TSR/speed).' )
      CALL ReadVar ( UnIn, InpFile, ParCol, 'ParCol', 'Column parameter (1-rpm, 2-pitch, 3-TSR/speed).' )
      CALL ReadVar ( UnIn, InpFile, ParTab, 'ParTab', 'Sheet parameter  (1-rpm, 2-pitch, 3-TSR/speed).' )
      CALL ReadVar ( UnIn, InpFile, OutPwr, 'OutPwr', 'Request output of rotor power?'                  )
      CALL ReadVar ( UnIn, InpFile, OutCp,  'OutCp',  'Request output of Cp?'                           )
      CALL ReadVar ( UnIn, InpFile, OutTrq, 'OutTrq', 'Request output of shaft torque?'                 )
      CALL ReadVar ( UnIn, InpFile, OutFlp, 'OutFlp', 'Request output of flap bending moment?'          )
      CALL ReadVar ( UnIn, InpFile, OutThr, 'OutThr', 'Request output of rotor thrust?'                 )


         ! Check for valid choices.

      IF ( ( ParRow < 1 ) .OR. ( ParRow > 3 ) )  THEN
         CALL Abort ( ' Variable "ParRow" must be between 1 and 3 (inclusive).  Instead, it is "'//TRIM( Int2LStr( ParRow ) ) &
                      //'".' )
      ENDIF

      IF ( ( ParCol < 1 ) .OR. ( ParCol > 3 ) )  THEN
         CALL Abort ( ' Variable "ParCol" must be between 1 and 3 (inclusive).  Instead, it is "'//TRIM( Int2LStr( ParCol ) ) &
                      //'".' )
      ENDIF

      IF ( ( ParTab < 1 ) .OR. ( ParTab > 3 ) )  THEN
         CALL Abort ( ' Variable "ParTab" must be between 1 and 3 (inclusive).  Instead, it is "'//TRIM( Int2LStr( ParTab ) ) &
                      //'".' )
      ENDIF

      IF ( ParCol == ParRow )  THEN
         CALL Abort ( ' Variable "ParCol" must differ from "ParRow".  Both are "'//TRIM( Int2LStr( ParRow ) )//'".' )
      ENDIF

      IF ( ParTab == ParRow )  THEN
         CALL Abort ( ' Variable "ParTab" must differ from "ParRow".  Both are "'//TRIM( Int2LStr( ParRow ) )//'".' )
      ELSEIF ( ParTab == ParCol )  THEN
         CALL Abort ( ' Variable "ParTab" must differ from "ParCol".  Both are "'//TRIM( Int2LStr( ParCol ) )//'".' )
      ENDIF


         ! Make sure at least on request is made for output.

      IF ( ( .NOT. OutCp  ) .AND. &
           ( .NOT. OutFlp ) .AND. &
           ( .NOT. OutPwr ) .AND. &
           ( .NOT. OutThr ) .AND. &
           ( .NOT. OutTrq ) )  THEN

            CALL Abort ( ' No output requested.  At least one of OutCp, OutFlp, OutPwr, OutThr, OutTrq must be TRUE.' )

      ENDIF

      CALL ReadRAry ( UnIn, InpFile, PitSets,  3, 'PitSets',  'First, last, delta blade pitch (deg).' )
      CALL ReadRAry ( UnIn, InpFile, OmgSets,  3, 'OmgSets',  'First, last, delta rotor speed (rpm).' )
      CALL ReadRAry ( UnIn, InpFile, SpdSets,  3, 'SpdSets',  'First, last, delta speeds.'            )


         ! Check for valid choices.

      IF ( ( PitSets(3) /= 0.0 ) .AND. ( PitSets(2) - PitSets(1) )/PitSets(3) < 0.0 ) &
                                       CALL Abort ( ' Your pitch settings (PitSt, PitEnd, PitDel) are not consistent.' )
      IF ( ( OmgSets(3) /= 0.0 ) .AND. ( OmgSets(2) - OmgSets(1) )/OmgSets(3) < 0.0 ) &
                                       CALL Abort ( ' Your rotor-speed settings (OmgSt, OmgEnd, OmgDel) are not consistent.' )
      IF ( ( SpdSets(3) /= 0.0 ) .AND. ( SpdSets(2) - SpdSets(1) )/SpdSets(3) < 0.0 ) &
                                       CALL Abort ( ' Your speed settings (SpdSt, SpdEnd, SpdDel) are not consistent.' )

      PitSt  = PitSets(1)
      PitEnd = PitSets(2)
      PitDel = PitSets(3)

      OmgSt  = OmgSets(1)
      OmgEnd = OmgSets(2)
      OmgDel = OmgSets(3)

      SpdSt  = SpdSets(1)
      SpdEnd = SpdSets(2)
      SpdDel = SpdSets(3)

      IF ( .NOT. InputTSR )  ParamStr(3) = 'WndSp'


         ! Allocate the parameter arrays.

      CALL AllocPar


         ! Load the parameter arrays.

      DO IPit=1,NumPit
         PitAry(IPit) = PitSt + PitDel*( IPit - 1 )
      ENDDO ! IPit

      DO IOmg=1,NumOmg
         OmgAry(IOmg) = OmgSt + OmgDel*( IOmg - 1 )
      ENDDO ! IOmg

      DO ISpd=1,NumSpd
         SpdAry(ISpd) = SpdSt + SpdDel*( ISpd - 1 )
      ENDDO ! ISpd


         ! Point the row, column, and table parameter arrays to the appropiate pitch, omega, and speed parameter arrays.

      SELECT CASE ( ParTab )
      CASE ( 1 )
         IF ( ParCol == 2 )  THEN
            RowAry => SpdAry
            ColAry => PitAry
            TabAry => OmgAry
         ELSE
            RowAry => PitAry
            ColAry => SpdAry
            TabAry => OmgAry
         ENDIF
      CASE ( 2 )
         IF ( ParCol == 1 )  THEN
            RowAry => SpdAry
            ColAry => OmgAry
            TabAry => PitAry
         ELSE
            RowAry => OmgAry
            ColAry => SpdAry
           TabAry => PitAry
         ENDIF
      CASE ( 3 )
         IF ( ParCol == 1 )  THEN
            RowAry => PitAry
            ColAry => OmgAry
            TabAry => SpdAry
         ELSE
            RowAry => OmgAry
            ColAry => PitAry
            TabAry => SpdAry
         ENDIF
      END SELECT

   ENDIF ! ( NumCases > 0 )

      ! Close the input and echo files.

   CLOSE ( UnIn )

   IF ( Echo )  CLOSE ( UnEc )


   RETURN
   END SUBROUTINE GetData
!=======================================================================
   SUBROUTINE GetInds ( IOmg, IPit, ISpd, IRow, ICol, ITab )


      ! This subroutine gets the output indices for given omega, pitch, and speed indices.


   USE                             WTP_Data

   IMPLICIT                        NONE


      ! Argument declarations.

   INTEGER, INTENT(OUT)         :: ICol                                         ! The column index.
   INTEGER, INTENT(IN)          :: IOmg                                         ! The rotor-speed index.
   INTEGER, INTENT(IN)          :: IPit                                         ! The blade-pitch index.
   INTEGER, INTENT(OUT)         :: IRow                                         ! The row index.
   INTEGER, INTENT(IN)          :: ISpd                                         ! The TSR/wind-speed index.
   INTEGER, INTENT(OUT)         :: ITab                                         ! The table index.



   SELECT CASE ( ParTab )

   CASE ( 1 )

      IF ( ParCol == 2 )  THEN

         IRow = ISpd
         ICol = IPit
         ITab = IOmg

      ELSE

         IRow = IPit
         ICol = ISpd
         ITab = IOmg

      ENDIF

   CASE ( 2 )

      IF ( ParCol == 1 )  THEN

         IRow = ISpd
         ICol = IOmg
         ITab = IPit

      ELSE

         IRow = IOmg
         ICol = ISpd
         ITab = IPit

      ENDIF

   CASE ( 3 )

      IF ( ParCol == 1 )  THEN

         IRow = IPit
         ICol = IOmg
         ITab = ISpd

      ELSE

         IRow = IOmg
         ICol = IPit
         ITab = ISpd

      ENDIF

   END SELECT


   RETURN
   END SUBROUTINE GetInds ! ( IOmg, IPit, ISpd, IRow, ICol, ITab )
!=======================================================================
   SUBROUTINE GetVel ( VWndNorm )

      ! This function return the value of the local dynamic pressure.


   USE                             WTP_Data

   IMPLICIT                        NONE


      ! Argument declarations.

   REAL(ReKi), INTENT(OUT)      :: VWndNorm


      ! Local declarations.

   REAL(ReKi)                   :: VWndTang



      ! Calculate the normal and tangential wind speed in the local reference system.

   VWndNorm =  VWndGnd*( CosCone*CosYaw*CosTilt - SinCone*( CosYaw*CosAzim*SinTilt - SinYaw*SinAzim ) )
   VWndTang = -VWndGnd*( CosAzim*SinYaw + CosYaw*SinTilt*SinAzim )


      ! Calculate total tangential wind speed.

   VTotTang = VWndTang + VBodTang


   RETURN
   END SUBROUTINE GetVel ! ( VBodTang, VWndGnd, CosAzim, SinAzim, VTotTang, VWndNorm )
!=======================================================================
!Start of proposed change.  v3.02.00a-mlb 03-August-2006  M. Buhl
!Start of proposed change.  v3.02.00b-mlb 22-August-2006  M. Buhl
!major rewrite.
!Start of proposed change.  v3.02.00c-mlb 24-August-2006  M. Buhl
!   SUBROUTINE Ind_Prop ( ISeg, Converg )
!Start of proposed change.  v3.02.00g-mlb 7-September-2006  M. Buhl
!v3.02.00g   SUBROUTINE Ind_Prop ( ISeg, ZFound, Converg )
   SUBROUTINE InductBEM ( ISeg, ISect, ZFound, Converg, AxIndPrevSeg, TanIndPrevSeg  )
!End of proposed change.  v3.02.00g-mlb 7-September-2006  M. Buhl
!End of proposed change.  v3.02.00c-mlb 24-August-2006  M. Buhl


      ! This routine calculates the induction factors using the old Prop method.
      ! It will also allow the addition of the drag terms in either or both of
      ! the induction factors to (possibly) mimic the BLADED algorithm.


   USE                             Parameters
   USE                             ProgGen
   USE                             WTP_Data

   IMPLICIT                        NONE


      ! Argument declarations.

   INTEGER, INTENT(IN)          :: ISeg                                         ! The segment number.
   INTEGER, INTENT(IN)          :: ISect                                         ! The sector number.

   LOGICAL, INTENT(OUT)         :: Converg                                      ! Flag that says if we fully converged the induction loop.
!Start of proposed change.  v3.02.00c-mlb 24-August-2006  M. Buhl
   LOGICAL, INTENT(OUT)         :: ZFound                                       ! Flag that says if we even found a possible solution (zero crossing).
!End of proposed change.  v3.02.00c-mlb 24-August-2006  M. Buhl

!TempDCM Start 3.03.01a01-dcm, 27-Jul-2009
!New Variables for Convergence Summer 2009
   REAL(ReKi), INTENT(INOUT)       :: AxIndPrevSeg                            !Value of AxInd from previous converged segment, if not converged, uses these default values.
   REAL(ReKi), INTENT(INOUT)       :: TanIndPrevSeg                           !Value of TanInd from previous converged segment, if not converged, uses these default values.

!TempDCM End 3.03.01a01-dcm, 27-Jul-2009


      ! Local declarations.

!Start of proposed change.  v3.02.00g-mlb 9-Sept-2006  M. Buhl
   REAL(ReKi)                   :: AFang                                        ! The angle between the wind vector and the cone of rotation in radians.
!End of proposed change.  v3.02.00g-mlb 9-Sept-2006  M. Buhl
   REAL(ReKi)                   :: AxIndHi                                      ! The upper value of AxInd for the two points defining a zero crossing.
   REAL(ReKi)                   :: AxIndLo                                      ! The lower value of AxInd for the two points defining a zero crossing.
!Start of proposed change.  v3.02.00g-mlb 11-Sept-2006  M. Buhl
!v3.02.00g   REAL(ReKi), PARAMETER        :: AxOneMin =  0.9                              ! The lower value of AxInd range around 1.0 (1-delta).
!v3.02.00g   REAL(ReKi), PARAMETER        :: AxOnePlu =  1.1                              ! The upper value of AxInd range around 1.0 (1+delta).
   REAL(ReKi), PARAMETER        :: AxOneMin =  0.9999999999999                             ! The lower value of AxInd range around 1.0 (1-delta).
   REAL(ReKi), PARAMETER        :: AxOnePlu =  1.0000000000001                             ! The upper value of AxInd range around 1.0 (1+delta).
!End of proposed change.  v3.02.00g-mlb 11-Sept-2006  M. Buhl

!TempDCM Start 3.02.00k02-dcm, 18-Jun-2009
   REAL(ReKi)                   :: ClBEM                                        ! The local life-coefficient from BEm theory. DCM 6-18-09
!TempDCM End 3.02.00k02-dcm, 18-Jun-2009
   REAL(ReKi)                   :: CmLoc                                        ! The local pitching-moment coefficient (unused).
   REAL(ReKi)                   :: CnLoc                                        ! The local normal coefficient.
!Start of proposed change.  v3.04.00a-mlb, 01-May-2010,  M. Buhl
   REAL(ReKi)                   :: CpminLoc                                     ! The local minimum-pressure coefficient.
!End of proposed change.  v3.04.00a-mlb, 01-May-2010,  M. Buhl
   REAL(ReKi)                   :: CtLoc                                        ! The local tangential coefficient.
!TempDCM Start 3.02.00k04-dcm, 22-Jun-2009
   REAL(ReKi)                   :: DctZero
!TempDCM End 3.02.00k04-dcm, 22-Jun-2009
!Temp Start proposed changes. 3.02.00m02-dcm, 7-Jul-2009
   REAL(ReKi)                   :: DctLs                                        !Thrust coeffecient from momentum theory(aka axial Head loss), < 0.96 and AInd < 0.4, for Dct
   REAL(ReKi)                   :: DctGr_Glau                                   !Thrust coeffecient from Glauert empirical, > 0.96 and AInd > 0.4, for Dct
   REAL(ReKi)                   :: DctBem                                       !Dct from momentum theory or Glauert's empirical curve for AInd
!Temp End proposed changes. 3.02.00m02-dcm, 7-Jul-2009

!Temp Start proposed changes. 3.02.00n01-dcm, 7-Jul-2009
   REAL(ReKi)                   :: DctNew                                       !Dct from momentum theory or Glauert's empirical curve for AInd                                                                               !New formula to calculate Dct, removes singularity due to SinAF going to zero
   REAL(ReKi)                   :: SwNew                                        !Dct from momentum theory or Glauert's empirical curve for AInd                                                                               !New part of formula to calculate Dct, removes singularity due to SinAF going to zero
!Temp End proposed changes. 3.02.00n01-dcm, 7-Jul-2009
!Temp Start proposed changes. 3.02.00n02-dcm, 8-Jul-2009
   !Integer                     :: TanIndNew                                    !New equation for Tangentional Induction factor
   REAL(ReKi)                   :: TanIndCoef                                   !Coefficient for New equation for Tangentional Induction factor
   REAL(ReKi)                   :: TanIndCoef_Cd                                !Cd term of Coefficient for New equation for Tangentional Induction factor 16-Jul-2009
   REAL(ReKi)                   :: TanIndCoef_Cl                                !Cl term of Coefficient for New equation for Tangentional Induction factor 16-Jul-2009
   REAL(ReKi)                   :: TanIndCoef_Old
   !REAL(ReKi),PARAMETER         :: SingAminSlope = -0.0594                      !Slope for equation for lower bound of Correction for (1-a) singularity
   !REAL(ReKi),PARAMETER         :: SingAminOffset = 0.9655                      !Offset for equation for lower bound of Correction for (1-a) singularity
   !REAL(ReKi),PARAMETER         :: SingAmaxSlope = 0.8292                       !Slope for equation for upper bound of Correction for (1-a) singularity
   !REAL(ReKi),PARAMETER         :: SingAmaxOffset = 1.0225                      !Offset for equation for upper bound of Correction for (1-a) singularity
   !REAL(ReKi),PARAMETER         :: DelSing = 0.45                              !Delta for the transition region, from Amax to Amax*(1+DelSing) and Amin*(1-DelSing) to Amin
   !REAL(ReKi),PARAMETER         :: DelSingZero = 0.0                           !Delta for the transition region about AInd = 1.0, lower A1Sing = 1.0-DelSingZero, upper A0Sing = 1.0+DelSingZero
   REAL(ReKi)                   :: A0sing
   REAL(ReKi)                   :: A1Sing
  ! REAL(ReKi)                   :: AminSing
 !  REAL(ReKi)                   :: AmaxSing
   REAL(ReKi)                   :: SingTransition
   REAL(ReKi)                   :: SingUpper
   REAL(ReKi),PARAMETER         :: SingOffset = 0.2
!Temp End proposed changes. 3.02.00n02-dcm, 8-Jul-2009

   REAL(ReKi), SAVE             :: Del                                          ! Variable to hold the delta for finite differencing.
   REAL(ReKi)                   :: LSR                                          ! The local speed ratio.
   REAL(ReKi)                   :: Re                                           ! Reynolds number.
   REAL(ReKi)                   :: Step                                         ! Step size for stepping through the possible range of axial-induction factor.
   REAL(ReKi)                   :: Sw                                           ! Windmill-state equation.

   INTEGER                      :: Iter                                         ! The iteration counter.
!Start of proposed change.  v3.02.00c-mlb 24-August-2006  M. Buhl
!#IFDEF debug
   INTEGER                      :: Iters                                        ! Number of iterations taken for this case.
!#ENDIF
!Start of proposed change.  v3.02.00g-mlb 9-Sept-2006  M. Buhl
   INTEGER                      :: NSplit                                       ! The number of binary splits to take to get within the induction tolerance.
!End of proposed change.  v3.02.00g-mlb 9-Sept-2006  M. Buhl
!v3.02.00c-mlb   INTEGER                      :: NumCStp  = 15                                ! The maximum number of coarse steps to take.
!v3.02.00c-mlb   INTEGER                      :: NumFStp  = 1500                              ! The maximum number of fine steps to take.

!v3.02.00c-mlb   LOGICAL                      :: ZFound                                       ! Flag that says if we even found a zero crossing.
!v3.02.00c-mlb
!End of proposed change.  v3.02.00c-mlb 24-August-2006  M. Buhl




      ! Initialize induction-factor variables.

   LSR      = VTotTang/VTotNorm
   TanInd   = 0.0
   Del      = SQRT( EPSILON( 0.0 ) )
   Converg  = .FALSE.

!#IFDEF debug
   Iters = 0
   Iter = 0
  ! PRINT *, " "
  ! PRINT *, "ISeg, ISect = ", ISeg,ISect
!#ENDIF


      ! Try to find a zero crossing for the axial induction between 0 and 2 using a coarse grid.
      ! If we are close to one, let's consider ourselves converged.

!Start 3.03.01a05-dcm, 29-Jul-2009
!Start of proposed change.  v3.02.00h-mlb 12-Sept-2006  M. Buhl
!#IFDEF debugg
 !  IF ( ISeg == 14 )  STOP
!#ENDIF
!End of proposed change.  v3.02.00h-mlb 12-Sept-2006  M. Buhl
!End 3.03.01a05-dcm, 29-Jul-2009

!Start of proposed change.  v3.02.00i-mlb 18-Sept-2006  M. Buhl
! We're going to try a coarser grid to improve performance, but look for minima and maxima.
!v3.02.00i   AxInd = FindZC( 0.0, 2.0, 100, ZFound, AxIndLo, AxIndHi )
!temp   AxInd = FindZC( .TRUE., 0.0, 2.0, 50, ZFound, AxIndLo, AxIndHi )

!TempDCM Start 3.03.01a02-dcm, 27-Jul-2009
!#IFDEF debug
 !  PRINT *, "  Newton-Raphson #1"
!#ENDIF

 !  DO Iter=1,MaxIter

!#IFDEF debug
 !     Iters = Iters + 1
!#ENDIF


!Set initial guess
IF (ISeg .EQ. 1) THEN
    !Find Closed form soln for AxInd and TanInd
    AxInd = 0 !1.0/3.0 !AxIndclosed
    TanInd = AxInd*(1.0-AxInd)/(LSR*LSR) !TanIndclosed
ELSE
 !Set inital guess to last converged values
    AxInd = AxIndPrevSeg
    TanInd = TanIndPrevSeg
ENDIF ! (ISeg .EQ. 1)
    !AxInd = 1.0/3.0 !AxIndclosed
    !TanInd = AxInd*(1.0-AxInd)/(LSR*LSR) !TanIndclosed

!TEmp Start 3.03.01a03-dcm, 28-Jul-2009
DO WHILE (Converg .EQ. .FALSE. .AND. Iters .LT. MaxIter)
   Iters = Iters + 1

   !PRINT *, "  Newton-Raphson #",Iters

   CALL NewtRaph ( AxInd, TanInd,.False. )
END DO

IF (Converg) THEN
!PRINT *, "  Converg! #",Iters
   TanIndPrevSeg = TanInd
   AxIndPrevSeg = AxInd
   Solution = .TRUE.

      ! Start: Add skewed-wake correction to axial induction.

   AxInd = AxInd*SWcorr

   IF ( .NOT. ( AIDrag .OR. TIDRAG ) )  THEN
!Start of proposed change.  v3.04.00a-mlb, 12-Apr-2010,  M. Buhl
!v3.04.00a            CALL GetCoefs ( ISeg, AlfaD, Re, AF_Table(AFfile(ISeg)), ClLoc, CdLoc, CmLoc, .FALSE., .TRUE., .FALSE. )
      CALL GetCoefs ( ISeg, AlfaD, Re, AF_Table(AFfile(ISeg)), ClLoc, CdLoc, CmLoc, CpminLoc, .FALSE., .TRUE., .FALSE., .FALSE. )
!End of proposed change.  v3.04.00a-mlb, 12-Apr-2010,  M. Buhl
   ENDIF ! ( .NOT. ( AIDrag .OR. TIDRAG ) )
                ! End: Add skewed-wake correction to axial induction.
   RETURN
ENDIF ! (Converg)

AxIndLo = AxInd - 1.0
AxIndHi = AxInd + 1.0
   !AxInd = FindZC( .TRUE., AxIndLo, AxIndHi, 200, ZFound, AxIndLo, AxIndHi )
AxInd = FindZC( .TRUE., -1.0, 1.0, 200, ZFound, AxIndLo, AxIndHi )

IF (ZFound) THEN
    !    write (*,'(A)') "ZFound:"
   Iters=0
   DO WHILE (Converg .EQ. .FALSE. .AND. Iters .LT. MaxIter)
      Iters = Iters + 1

   !PRINT *, "  Newton-Raphson #",Iters

      CALL NewtRaph ( AxInd, TanInd,.False. )
   END DO
   !Try  Binary Search
   !#IFDEF debug
   !PRINT *, "  Binary search"
    !#ENDIF
   IF (.NOT. Converg) THEN
      NSplit = 10
      !write (*,'(A,1F13.6,L3,2F13.6)') "AxInd,ZFound, AxIndLo, AxIndHi:",AxInd,ZFound, AxIndLo, AxIndHi
      AxInd = BinSearch( AxIndLo, AxIndHi, NSplit,.False. )
   ENDIF ! (.NOT. Converg)

   IF (Converg) THEN
    !PRINT *, "  Converg!Binary #",Iter, AxInd,TSR,ISeg,ISect
      TanIndPrevSeg = TanInd
      AxIndPrevSeg = AxInd
      Solution = .TRUE.

         ! Add skewed-wake correction to axial induction.

      AxInd = AxInd*SWcorr

      IF ( .NOT. ( AIDrag .OR. TIDRAG ) )  THEN
!Start of proposed change.  v3.04.00a-mlb, 12-Apr-2010,  M. Buhl
!v3.04.00a            CALL GetCoefs ( ISeg, AlfaD, Re, AF_Table(AFfile(ISeg)), ClLoc, CdLoc, CmLoc, .FALSE., .TRUE., .FALSE. )
         CALL GetCoefs ( ISeg, AlfaD, Re, AF_Table(AFfile(ISeg)), ClLoc, CdLoc, CmLoc, CpminLoc, .FALSE., .TRUE., .FALSE., .FALSE. )
!End of proposed change.  v3.04.00a-mlb, 12-Apr-2010,  M. Buhl
      ENDIF ! ( .NOT. ( AIDrag .OR. TIDRAG ) )

      Return
   ENDIF !(Converg)
ENDIF !(ZFound)

 !NSplit = 35
 ! AxIndLo = -1.0
!AxIndHi =  2.5
!AxInd = BinSearch( AxIndLo, AxIndHi, NSplit,.False. )
!PRINT *, "  Not-Converg! #",Iters
 !write (*,'(A,1F13.6)') "1stTime:",AxInd
NSplit  = 20
AxIndLo = -.5
AxIndHi =  0.6
AxInd   = BinSearch( AxIndLo, AxIndHi, NSplit,.False. )

IF (.NOT. Converg) THEN
  !write (*,'(A,1F13.6)') "2ndTime:",AxInd
   NSplit  = 10
   AxIndLo = -1.0
   AxIndHi = -0.4
!write (*,'(A,1F13.6,L3,2F13.6)') "AxInd,ZFound, AxIndLo, AxIndHi:",AxInd,ZFound, AxIndLo, AxIndHi
   AxInd   = BinSearch( AxIndLo, AxIndHi, NSplit,.False. )
ENDIF ! (.NOT. Converg)

IF (.NOT. Converg) THEN
       !write (*,'(A,1F13.6)') "3rdTime:",AxInd
   NSplit  = 15
   AxIndLo =  0.59
   AxIndHi = 2.5
!write (*,'(A,1F13.6,L3,2F13.6)') "AxInd,ZFound, AxIndLo, AxIndHi:",AxInd,ZFound, AxIndLo, AxIndHi
   AxInd   = BinSearch( AxIndLo, AxIndHi, NSplit,.False. )
ENDIF ! (.NOT. Converg)

IF (Converg) THEN
    !PRINT *, "  Converg!Binary #",Iter, AxInd,TSR,ISeg,ISect
   TanIndPrevSeg = TanInd
   AxIndPrevSeg = AxInd
   Solution = .TRUE.

      ! Add skewed-wake correction to axial induction.

   AxInd = AxInd*SWcorr

   IF ( .NOT. ( AIDrag .OR. TIDRAG ) )  THEN
!Start of proposed change.  v3.04.00a-mlb, 12-Apr-2010,  M. Buhl
!v3.04.00a            CALL GetCoefs ( ISeg, AlfaD, Re, AF_Table(AFfile(ISeg)), ClLoc, CdLoc, CmLoc, .FALSE., .TRUE., .FALSE. )
      CALL GetCoefs ( ISeg, AlfaD, Re, AF_Table(AFfile(ISeg)), ClLoc, CdLoc, CmLoc, CpminLoc, .FALSE., .TRUE., .FALSE., .FALSE. )
!End of proposed change.  v3.04.00a-mlb, 12-Apr-2010,  M. Buhl
   ENDIF ! ( .NOT. ( AIDrag .OR. TIDRAG ) )

   Return
ENDIF ! (Converg)

PRINT *, "  Binary search Robust"
!#ENDIF
NSplit = 35
AxIndLo = -1.0
AxIndHi = 2.5
!write (*,'(A,1F13.6,L3,2F13.6)') "AxInd,ZFound, AxIndLo, AxIndHi:",AxInd,ZFound, AxIndLo, AxIndHi
AxInd = BinSearch( AxIndLo, AxIndHi, NSplit,.False. )
IF (Converg) THEN
   !PRINT *, "  Converg!Binary #",Iter, AxInd,TSR,ISeg,ISect
   TanIndPrevSeg = TanInd
   AxIndPrevSeg = AxInd
   Solution = .TRUE.

      ! Add skewed-wake correction to axial induction.

   AxInd = AxInd*SWcorr

   IF ( .NOT. ( AIDrag .OR. TIDRAG ) )  THEN
!Start of proposed change.  v3.04.00a-mlb, 12-Apr-2010,  M. Buhl
!v3.04.00a            CALL GetCoefs ( ISeg, AlfaD, Re, AF_Table(AFfile(ISeg)), ClLoc, CdLoc, CmLoc, .FALSE., .TRUE., .FALSE. )
      CALL GetCoefs ( ISeg, AlfaD, Re, AF_Table(AFfile(ISeg)), ClLoc, CdLoc, CmLoc, CpminLoc, .FALSE., .TRUE., .FALSE., .FALSE. )
!End of proposed change.  v3.04.00a-mlb, 12-Apr-2010,  M. Buhl
   ENDIF ! ( .NOT. ( AIDrag .OR. TIDRAG ) )

   Return
ENDIF ! (Converg)

Return  !End of iteration routine


  ! ENDDO ! Iter

!Start of proposed change.  v3.02.00g-mlb 8-September-2006  M. Buhl

      ! The Newton-Raphson didn't converge, let's do a binary search.

!#IFDEF debug
  ! PRINT *, "  Binary search"
!#ENDIF

   !AxInd = BinSearch( AxIndLo, AxIndHi, NSplit )

!End of proposed change.  v3.02.00g-mlb 8-September-2006  M. Buhl

!Start of proposed change.  v3.02.00c-mlb 24-August-2006  M. Buhl
!v3.02.00c-mlb      ! Looks like we didn't converge.  Let's take smaller steps starting at the low AxInd we got from the previous grid search.
      ! Looks like we didn't converge.  If the UseNines flag is not set, we'll use the last guess
      ! for the induction values in further calculations.

!v3.02.00c-mlbprint *, "  Narrow grid"
!v3.02.00c-mlb   AxInd = FindZC( AxIndLo, AxIndHi, 20, ZFound, AxIndLo, AxIndHi )
!v3.02.00c-mlb
!v3.02.00c-mlb! we really shouldn't need this.
!v3.02.00c-mlb!   IF ( ( AxOneMin < AxInd ) .AND. ( AxInd < AxOnePlu ) )  THEN
!v3.02.00c-mlb!      Converg = .TRUE.
!v3.02.00c-mlb!      RETURN
!v3.02.00c-mlb!   END IF
!v3.02.00c-mlb
!v3.02.00c-mlb
!v3.02.00c-mlb      ! Let's give Newton-Raphson just five more tries for a full solution.
!v3.02.00c-mlb
!v3.02.00c-mlbprint *, "  Newton-Raphson #2"
!v3.02.00c-mlb!   DO Iter=1,5
!v3.02.00c-mlb   DO Iter=1,10
!v3.02.00c-mlb
!v3.02.00c-mlb      Iters = Iters + 1
!v3.02.00c-mlb
!v3.02.00c-mlb      CALL NewtRaph ( AxInd, TanInd )
!v3.02.00c-mlb
!v3.02.00c-mlb      IF ( Converg )  THEN
!v3.02.00c-mlb
!v3.02.00c-mlb
!v3.02.00c-mlb            ! Add skewed-wake correction to axial induction.
!v3.02.00c-mlb
!v3.02.00c-mlb         AxInd = AxInd*SWcorr
!v3.02.00c-mlb
!v3.02.00c-mlb         IF ( .NOT. ( AIDrag .OR. TIDRAG ) )  THEN
!v3.02.00c-mlb            CALL GetCoefs ( ISeg, AlfaD, Re, AF_Table(AFfile(ISeg)), ClLoc, CdLoc, CmLoc, .FALSE., .TRUE., .FALSE. )
!v3.02.00c-mlb         ENDIF
!v3.02.00c-mlb
!v3.02.00c-mlb         RETURN
!v3.02.00c-mlb
!v3.02.00c-mlb      END IF
!v3.02.00c-mlb
!v3.02.00c-mlb   END DO ! Iter
!End of proposed change.  v3.02.00c-mlb 24-August-2006  M. Buhl

!TempDCM Start 3.03.01a02-dcm, 27-Jul-2009
!IF (Converg) THEN
!TanIndPrevSeg = TanInd
!AxIndPrevSeg = AxInd
!ENDIF
!TempDCM End 3.03.01a02-dcm, 27-Jul-2009

!print *, "Ind_Prop-5: Solution, Converge = ", ZFound, Converg
   RETURN                                                              !mlb: redundant
   !-----------------------------------------------------------------------
   CONTAINS
      FUNCTION AxIndErr ( AInd )


         ! Compute the error in the axial induction.


         ! Function declaration.

      REAL(ReKi)                   :: AxIndErr                                     ! This function.


         ! Argument declarations.

      REAL(ReKi), INTENT(IN)       :: AInd                                         ! The axial-induction factor.


         ! Local declarations.

      REAL(ReKi)                   :: B0                                           ! Polynomial coefficient used to calculate Dct in the advanced brake state.
      REAL(ReKi)                   :: B1                                           ! Polynomial coefficient used to calculate Dct in the advanced brake state.
      REAL(ReKi)                   :: B2                                           ! Polynomial coefficient used to calculate Dct in the advanced brake state.
      REAL(ReKi)                   :: Dct                                          ! Head-loss coefficient.




!Start of proposed change.  v3.02.00g-mlb 9-September-2006  M. Buhl

         ! For this axial induction, iterate on airflow angle and tangential induction to get better values for them.

      CALL GetAFTI ( AInd, TanInd, AFang )

!End of proposed change.  v3.02.00g-mlb 9-September-2006  M. Buhl

         ! Calculate the error in axial induction.  Dct = Head-loss coefficient.
         ! Deal with the airflow angle being zero by forcing it to use the imperical expression.


!      IF ( SinAF == 0.0 )  THEN
!         Dct = 1.0
!      ELSE
!         Sw = Solidity*CnLoc*CosCone*CosCone/( 4.0*SinAF*SinAF )
!         Dct = Sw*4.0*( 1.0 - AInd )**2
!      END IF

      IF ( AInd .GT. 0.97 .and. AInd .LT. 1.03 )  THEN
         SwNew = ((VtotTang*VTotTang*(1+TanInd)*(1+TanInd))/(VTotNorm*VTotNorm))+ ((1-AInd)*(1-AInd))
         Dct   = Solidity*CnLoc *SwNew / CosCone
     ELSE
         Sw  = Solidity*CnLoc*CosCone*CosCone/( 4.0*SinAF*SinAF )
         Dct = Sw*4.0*( 1.0 - AInd )**2
      END IF
!TempDCM End 3.02.00q05-dcm, 20-Jul-2009


      IF ( Dct < 0.96*Loss )  THEN
!Start of proposed change.  v3.02.00h-mlb 12-September-2006  M. Buhl
!v3.02.00h         AxIndErr = ( 1.0 - AInd )*Sw/Loss - AInd
         AxIndErr = 0.5*( 1.0 - SQRT( 1.0 - Dct/Loss ) ) - AInd
!#IFDEF debugg
         B2       = 1/0.18 - 4.0*Loss                                              !  1.555 w/o loss
         B1       = 0.8*( Loss - B2 )                                              ! -1.244 w/o loss
         B0       = 2.0 - B1 - B2                                                  !  1.688 w/o loss


!Replaced AInd with (AInd+AxIndErr) for DctZero and ClBem, since we want DctZero for AxIndComp, not AInd;3.02.00j01-dcm, 6-Jul-2009
!DctZero = ((1-((1-2*(AInd))*(1-2*(AInd))))*Loss)!!KEEP Used for plotting.  DCM 3.02.00k04-dcm, 22-Jun-2009

!End of proposed change.  v3.02.00h-mlb 12-September-2006  M. Buhl

      ELSE
         B2       = 1/0.18 - 4.0*Loss                                              !  1.555 w/o loss
         B1       = 0.8*( Loss - B2 )                                              ! -1.244 w/o loss
         B0       = 2.0 - B1 - B2                                                  !  1.688 w/o loss
         AxIndErr = 0.5*( -B1 + SQRT( B1*B1 - 4.0*B2*( B0 - Dct ) ) )/B2 - AInd
!Start of proposed change.  v3.02.00h-mlb 12-September-2006  M. Buhl
!#IFDEF debugg

!DctZero = B0-((((2*(AInd)*B2+B1)**2)-(B1*B1))/(-4*B2))!KEEP Used for plotting.  DCM 3.02.00k04-dcm, 22-Jun-2009

!End of proposed change.  v3.02.00h-mlb 12-September-2006  M. Buhl
      ENDIF

!Start proposed changes. 3.02.00m02-dcm, 7-Jul-2009
!!Keep the following for future plotting options: Start
!DctLs = 4.0*AInd*Loss*(1.0 - AInd);
!DctGr_Glau = B0 + B2*(AInd*AInd) + B1*AInd;

!IF (DctLs .LT. 0.96*Loss .AND. AInd .LT. 0.4) THEN
!DctBem = DctLs
!ELSE
!DctBem = DctGr_Glau
!ENDIF

!ClBem = (1.0/CosAF)*(((DctBem*SinAF*SinAF)/((1-(AInd))*(1-(AInd))*Solidity*CosCone*CosCone))-CdLoc*SinAF)
!End proposed changes. 3.02.00m02-dcm, 7-Jul-2009



!Start proposed changes. DCM 6-22-09 3.02.00k04-dcm, 22-Jun-2009
!IF (.FALSE. .and. WriteDebug) THEN
!write (UnDB,'(1X,1F20.13,2F24.6,4F13.6,1F20.4,10F16.6,8F24.6)') AInd, Sw, Dct, AxIndErr + AInd, AxIndErr,AlfaD,ClLoc,ClBem,DctZero,Loss,DctBem,SinAF,DctNew,SwNew,TanInd,TISingularity,AFang,CtLoc,TanIndCoef,TanIndCoef_Cd,TanIndCoef_Old,TanIndCoef_Cl,SingTransition,Solidity,LSR,SingUpper
!ENDIF

!!Keep the following for future plotting options: END
!End proposed changes. DCM 6-22-09 3.02.00k04-dcm, 22-Jun-2009

!#ENDIF


      RETURN
      END FUNCTION AxIndErr ! ( AInd )
!Start of proposed change.  v3.02.00g-mlb 9-Sept-2006  M. Buhl
   !-----------------------------------------------------------------------
      FUNCTION BinSearch( XLo, XHi, NSplit,Display )


         ! Do a binary search of function Funct between XLo and XHi by splitting the region NSplit times.


         ! Function declaration.

      REAL(ReKi)                   :: BinSearch                                    ! This function.


         ! Argument declarations.

      REAL(ReKi), INTENT(INOUT)    :: XLo                                          ! The lower X.
      REAL(ReKi), INTENT(INOUT)    :: XHi                                          ! The upper X.

      INTEGER, INTENT(IN)          :: NSplit                                       ! The number of binary splits to take to get within 0.01 degrees.

      Logical, INTENT(IN)    :: Display                                       ! Display output?

         ! Local declarations.

      REAL(ReKi)                   :: F                                            ! The function evaluated at the midpoint of the current segment.
      REAL(ReKi)                   :: FHi                                          ! The function evaluated at the higher endpoint of the current segment.
      REAL(ReKi)                   :: FLo                                          ! The function evaluated at the lower endpoint of the current segment.
      REAL(ReKi)                   :: X                                            ! The midpoint of the current segment.

      INTEGER                      :: MaxBi   = 15                                 ! The maximum number of bifurcations.


         ! Let's see if the function evaluates to zero for either endpoint.  If so, we are done.

      FLo = AxIndErr( XLo )
      FHi = AxIndErr( XHi )

    !  IF( Display) THEN
!#IFDEF DEBUGB
    !  PRINT *, "    Bin1: Iters = ", Iters + Iter
   !   PRINT *, "    Bin1: XLo, XHi = ", XLo, XHi
  !    PRINT *, "    Bin1: FLo, FHi = ", FLo, FHi
!#ENDIF
!EndIF

      IF ( ABS( FLo ) <= ATol )  THEN
         BinSearch = Xlo
         Converg   = .TRUE.
         RETURN
      ELSE IF ( ABS( FHi ) <= ATol )  THEN
         BinSearch = XHi
         Converg   = .TRUE.
         RETURN
      END IF


         ! Let's make sure we bound the function zero.

      IF ( FHi/FLo > 0.0 )  THEN
      !Temp Start 3.03.01c02-dcm, 7-Aug-2009
         !CALL Abort ( ' Call to BinSearch cannot allow the function evaluations of both X values to have the same sign.' )

         !Print *,"Warning: Call to BinSearch cannot allow the function evaluations of both X values to have the same sign."
         BinSearch = 0.0
         Return
         !Temp End 3.03.01c02-dcm, 7-Aug-2009
      END IF


         ! Let's split at most MaxBi times.  One last split done later.

      DO Iter=1,MaxBi-1

         X = 0.5*( XHi + XLo )
         F = AxIndErr( X )

!IF( Display) THEN
!#IFDEF DEBUGB
   !   PRINT *, "    Bin2: Iters = ", Iters + Iter
   !   PRINT *, "    Bin2: XLo, X, XHi = ", XLo, X, XHi
    !  PRINT *, "    Bin2: FLo, F, FHi = ", FLo, F, FHi
!#ENDIF
!EndIF

         IF ( ABS( F ) <= ATol )  THEN
            BinSearch = X
            Converg   = .TRUE.
            RETURN
         END IF

         IF ( FHi/F < 0.0 )  THEN
            XLo = X
            FLo = F
         ELSE
            XHi = X
            FHi = F
         END IF

      END DO


         ! And one last split.

      BinSearch = 0.5*( XHi + XLo )

      CALL GetAFTI ( BinSearch, TanInd, AFang )

!      Converg = .FALSE.
      Converg = .TRUE.

!IF( Display) THEN
!#IFDEF DEBUGB
      F = AxIndErr( BinSearch )
     ! PRINT *, "    Bin3: Iters = ", Iters + MaxBi
     ! PRINT *, "    Bin3: XLo, X, XHi = ", XLo, BinSearch, XHi
     ! PRINT *, "    Bin3: FLo, F, FHi = ", FLo, F, FHi
!#ENDIF
!ENDIF

      RETURN
      END FUNCTION BinSearch ! ( XLo, XHi, NSplit )
!End of proposed change.  v3.02.00g-mlb 9-Sept-2006  M. Buhl
   !-----------------------------------------------------------------------
!Start of proposed change.  v3.02.00i-mlb 18-September-2006  M. Buhl
!v3.02.00i      FUNCTION FindZC( AxMin, AxMax, NumStep, ZFound, AxIndLo, AxIndHi )
      RECURSIVE FUNCTION FindZC( PriCall, AxMin, AxMax, NumStep, ZFound, AxIndLo, AxIndHi )
!End of proposed change.  v3.02.00i-mlb 18-September-2006  M. Buhl


         ! Find the zero crossing of the function whose zero we are trying to find.
         ! Return the interpolated value between the two bounding points.


         ! Function declaration.

      REAL(ReKi)                   :: FindZC                                       ! This function.


         ! Argument declarations.

      REAL(ReKi), INTENT(OUT)      :: AxIndHi                                      ! The upper of the two bounding points.
      REAL(ReKi), INTENT(OUT)      :: AxIndLo                                      ! The lower of the two bounding points.
      REAL(ReKi), INTENT(IN)       :: AxMax                                        ! The upper bound for axial induction.
      REAL(ReKi), INTENT(IN)       :: AxMin                                        ! The lower bound for axial induction.

      INTEGER, INTENT(IN)          :: NumStep                                      ! The number of steps to take.

!Start of proposed change.  v3.02.00i-mlb 18-September-2006  M. Buhl
      LOGICAL, INTENT(IN)          :: PriCall                                      ! Flag to indicate if the current call to the routine is a primary or secondary (recursive) call.
!End of proposed change.  v3.02.00i-mlb 18-September-2006  M. Buhl
      LOGICAL, INTENT(OUT)         :: ZFound                                       ! Flag to indicate if we found a zero crossing.


         ! Local declarations.

      REAL(ReKi)                   :: AxErr                                        ! Current error in axial induction.
      REAL(ReKi)                   :: AxErrMin                                     ! The error for the 1-delta case.
      REAL(ReKi)                   :: AxErrOld                                     ! Previous error in axial induction.
      REAL(ReKi)                   :: AxErrPlu                                     ! The error for the 1+delta case.
      REAL(ReKi)                   :: AxInd                                        ! Current value of axial induction.
      REAL(ReKi)                   :: AxPrev                                       ! Previous value of axial induction.
!Start of proposed change.  v3.02.00i-mlb 18-Sept-2006  M. Buhl
      REAL(ReKi)                   :: DeltaErr                                     ! The change in the error function between two tests.
!End of proposed change.  v3.02.00i-mlb 18-Sept-2006  M. Buhl
      REAL(ReKi)                   :: Step                                         ! Step size for axial induction.

      INTEGER                      :: Iter                                         ! The iteration counter.
!Start of proposed change.  v3.02.00i-mlb 14-Sept-2006  M. Buhl

      LOGICAL                      :: Init                                         ! Flag to indicate if we are in the initialization phase.
!      LOGICAL                      :: MinMax                                       ! Flag to indicate if a minimum or maximum was found.
!End of proposed change.  v3.02.00i-mlb 14-Sept-2006  M. Buhl



         ! Initializations.

      Step   = ( AxMax - AxMin )/REAL( NumStep )
!Start of proposed change.  v3.02.00i-mlb 13-Sept-2006  M. Buhl
      Init   = .TRUE.
!End of proposed change.  v3.02.00i-mlb 13-Sept-2006  M. Buhl
      ZFound = .FALSE.

!Start of proposed change.  v3.02.00g-mlb 7-September-2006  M. Buhl
!v3.02.00g      CALL GetAero ( AxMin, 0.0 )
!End of proposed change.  v3.02.00g-mlb 7-September-2006  M. Buhl

!Start of proposed change.  v3.02.00i-mlb 20-Sept-2006  M. Buhl

!#IFDEF debugz
      !IF ( PriCall )  THEN
      !   WRITE (CU,*)  'Primary call to FindZC.'
      !ELSE
       !  WRITE (CU,*)  'Recursive call to FindZC.'
!stop
      !END IF
!#ENDIF

!End of proposed change.  v3.02.00i-mlb 20-Sept-2006  M. Buhl
!#IFDEF debug
      !Iters    = Iters + 1
!#ENDIF

!#IFDEF debugz
      !PRINT *, "     FindZC1: ", iters, AxMin, AxErrOld
!#ENDIF

      AxErrOld = AxIndErr( AxMin )
      !PRINT *, "     FindZC1: ", iters, AxMin, AxErrOld
      IF ( AxErrOld == 0.0 )  THEN
         AxIndHi = AxMin + Step
         AxIndLo = AxMin
         FindZC  = AxMin
         ZFound  = .TRUE.
         RETURN
      END IF

      AxPrev = AxMin


      DO Iter=1,NumStep

         AxInd = AxMin + Iter*Step



        ! IF ( ( AxOneMin < AxInd ) .AND. ( AxInd < AxOnePlu ) )  AxInd = AxOnePlu    ! Let's avoid a singulatity.

!Start of proposed change.  v3.02.00g-mlb 7-September-2006  M. Buhl
!v3.02.00g         CALL GetAero ( AxInd, 0.0 )
!End of proposed change.  v3.02.00g-mlb 7-September-2006  M. Buhl

         AxErr = AxIndErr( AxInd )

         If (Iter .EQ. 1) Then
         DeltaErr = AxErr - AxErrOld  !Initialize First Point
         ENDIF

               !PRINT *, "     FindZC1: ", iters, AxInd, AxErr,AxErrOld
!#IFDEF debug
     !    Iters = Iters + 1
!#ENDIF
!#IFDEF debugz
  !       PRINT *, "     FindZC2: ", iters, AxInd, AxErr
         !write(8,'(f9.3,A,F9.3)') AxInd, Tab, AxErr
!#ENDIF

!#IFDEF debugza
!         IF ( .NOT. ZFound .AND. AxErr/AxErrOld <= 0.0 )  THEN
!#ELSE
         IF ( AxErr/AxErrOld <= 0.0 )  THEN
!#ENDIF
!
!
!               ! Deal with being close to one.
!
!            IF ( ( AxPrev < 1.0 ) .AND. ( 1.0 < AxInd ) )  THEN
!
!!Start of proposed change.  v3.02.00g-mlb 7-September-2006  M. Buhl
!!v3.02.00g               CALL GetAero ( AxOneMin, 0.0 )
!!End of proposed change.  v3.02.00g-mlb 7-September-2006  M. Buhl
!
!               AxErrMin = AxIndErr( AxOneMin )
!
!               IF ( AxErrMin/AxErrOld <= 0.0 )  THEN     ! The zero is between AxPrev and AxOneMin.
!                  AxIndHi = AxOneMin
!                  AxIndLo = AxPrev
!                  FindZC  = AxErrOld*( AxIndHi - AxIndLo )/( AxErrOld - AxErrMin ) + AxIndLo
!                  ZFound  = .TRUE.
!#IFDEF debugz
! !                 AxErr = AxIndErr( FindZC )
!                  PRINT *, "     FindZC3:         min", AxOneMin, AxErrMin
!                  PRINT *, "     FindZC4:         low", FindZC, AxErr
!#ENDIF
!                  RETURN
!               END IF
!
!!Start of proposed change.  v3.02.00g-mlb 7-September-2006  M. Buhl
!!v3.02.00g               CALL GetAero ( AxOnePlu, 0.0 )
!!End of proposed change.  v3.02.00g-mlb 7-September-2006  M. Buhl
!
!               AxErrPlu = AxIndErr( AxOnePlu )
!
!               IF ( AxErrPlu/AxErr <= 0.0 )  THEN     ! The zero is between AxOnePlu and AxInd.
!                  AxIndHi = AxInd
!                  AxIndLo = AxOnePlu
!                  FindZC  = AxErrOld*( AxIndHi - AxIndLo )/( AxErrOld - AxErrMin ) + AxIndLo
!                  ZFound  = .TRUE.
!
!#IFDEF debugz
!!Start of proposed change.  v3.02.00g-mlb 7-September-2006  M. Buhl
!!v3.02.00g  CALL GetAero ( FindZC, 0.0 )
!                  !End of proposed change.  v3.02.00g-mlb 7-September-2006  M. Buhl
! !                 AxErr = AxIndErr( FindZC )
!                  PRINT *, "     FindZC5:        plus", AxOnePlu, AxErrPlu
!                  PRINT *, "     FindZC6:        high", FindZC, AxErr
!#ENDIF
!#IFNDEF debugza
!                  RETURN
!#ENDIF
!               END IF
!
!
!                  ! The zero is between AxOneMin and AxOnePlu (near 1).
!
!               AxIndHi = AxOnePlu
!               AxIndLo = AxOneMin
!               FindZC  = AxErrMin*( AxIndHi - AxIndLo )/( AxErrMin - AxErrPlu ) + AxIndLo
!#IFDEF debugz
!!Start of proposed change.  v3.02.00g-mlb 7-September-2006  M. Buhl
!!v3.02.00g  CALL GetAero ( FindZC, 0.0 )
!!End of proposed change.  v3.02.00g-mlb 7-September-2006  M. Buhl
! !              AxErr = AxIndErr( FindZC )
!               print *, "   FindZC7:        near", FindZC, AxErr
!#ENDIF
!
!            ELSE
!
!               AxIndHi = AxInd
!               AxIndLo = AxPrev
!               FindZC  = AxErrOld*( AxIndHi - AxIndLo )/( AxErrOld - AxErr ) + AxIndLo
!#IFDEF debugz
!!Start of proposed change.  v3.02.00g-mlb 7-September-2006  M. Buhl
!!v3.02.00g  CALL GetAero ( FindZC, 0.0 )
!!End of proposed change.  v3.02.00g-mlb 7-September-2006  M. Buhl
! !              AxErr = AxIndErr( FindZC )
!               PRINT *, "     FindZC7: ", iters, FindZC, AxErr
!#ENDIF
!
!            END IF
!
            ZFound  = .TRUE.
!
!#IFNDEF debugza
            !RETURN
!#ENDIF
!!Start of proposed change.  v3.02.00i-mlb 18-September-2006  M. Buhl
         !ELSE
          Else IF ( PriCall .AND. ( AxErr == AxErrOld ) )  THEN   ! We don't want to do this on a recursive call.  It may not ever touch the zero line!
!
!
!               ! We are at a minimum or maximum.  Let's do a finer search here to see if we just kissed the zero line.
!
            FindZC = FindZC( .FALSE., AxInd-Step, AxInd+Step, 1*NumStep, ZFound, AxIndLo, AxIndHi )  !Changed 8-3-09 DCM
!            FindZC = FindZC( .FALSE., AxInd, AxInd+Step, 10*NumStep, ZFound, AxIndLo, AxIndHi )

         ELSE IF ( PriCall .AND. ( DeltaErr/( AxErr - AxErrOld ) < 0.0 ) )  THEN   ! This was done as two tests because not all compilers shortcut.
!
!
!               ! We just passed a minimum or maximum.  Let's do a finer search here to see if we just kissed the zero line.
!
            FindZC = FindZC( .FALSE., AxInd-Step, AxInd+Step, 1*NumStep, ZFound, AxIndLo, AxIndHi )!Changed 8-3-09 DCM
!            FindZC = FindZC( .FALSE., AxInd, AxInd+Step, 10*NumStep, ZFound, AxIndLo, AxIndHi )
!
!!End of proposed change.  v3.02.00i-mlb 18-September-2006  M. Buhl
!
         END IF

!!Start of proposed change.  v3.02.00i-mlb 18-September-2006  M. Buhl
!#IFNDEF debugza
         IF ( ZFound )  THEN

                          AxIndHi = AxInd
              AxIndLo = AxPrev
               FindZC  = AxErrOld*( AxIndHi - AxIndLo )/( AxErrOld - AxErr ) + AxIndLo
               RETURN
         END IF
!#ENDIF
!
         DeltaErr = AxErr - AxErrOld
         Init     = .FALSE.
!!End of proposed change.  v3.02.00i-mlb 18-September-2006  M. Buhl
         AxPrev   = AxInd
         AxErrOld = AxErr

      ENDDO ! Iter


      RETURN
      END FUNCTION FindZC ! ( FirstCall, AxMin, AxMax, NumStep, ZFound, AxIndLo, AxIndHi )
!End of proposed change.  v3.02.00h-mlb 11-Sept-2006  M. Buhl
!Moved outside of InductBEM.
!v3.02.00h   !-----------------------------------------------------------------------
!v3.02.00h      SUBROUTINE GetAero ( AInd, TInd )
!v3.02.00h
!v3.02.00h
!v3.02.00h         ! Get some aerodynamic information..
!v3.02.00h
!v3.02.00h
!v3.02.00h         ! Argument declarations.
!v3.02.00h
!v3.02.00h      REAL(ReKi), INTENT(IN)       :: AInd                                         ! The axial-induction factor.
!v3.02.00h      REAL(ReKi), INTENT(IN)       :: TInd                                         ! The tangential-induction factor.
!v3.02.00h
!v3.02.00h
!v3.02.00h         ! Local declarations.
!v3.02.00h
!v3.02.00h!Start of proposed change.  v3.02.00g-mlb 9-Sept-2006  M. Buhl
!v3.02.00h!moved to InductBEM
!v3.02.00h!v3.02.00g      REAL(ReKi)                   :: AFang                                        ! The angle between the wind vector and the cone of rotation in radians.
!v3.02.00h!End of proposed change.  v3.02.00g-mlb 9-Sept-2006  M. Buhl
!v3.02.00h      REAL(ReKi)                   :: AlfaR                                        ! The angle of attack in radians.
!v3.02.00h      REAL(ReKi)                   :: LossHub  = 1.0                               ! Hub-loss correction factor.
!v3.02.00h      REAL(ReKi)                   :: LossTip  = 1.0                               ! Tip-loss correction factor.
!v3.02.00h      REAL(ReKi)                   :: Re                                           ! Reynolds number.
!v3.02.00h      REAL(ReKi)                   :: VInd                                         ! The total relative wind speed.
!v3.02.00h
!v3.02.00h
!v3.02.00h         ! External references.
!v3.02.00h
!v3.02.00h      REAL(ReKi)                   :: Prandtl                                      ! The Prandtl tip.hub-loss function.
!v3.02.00h
!v3.02.00h
!v3.02.00h
!v3.02.00h         ! Apply the induction factors to the speeds.
!v3.02.00h
!v3.02.00h      VIndTang = VTotTang*( 1.0 + TInd )
!v3.02.00h      VIndNorm = VTotNorm*( 1.0 - AInd*SWcorr )
!v3.02.00h      VIndNrm2 = VIndNorm**2
!v3.02.00h      VInd2    = VIndTang**2 + VIndNrm2
!v3.02.00h      VInd     = SQRT( VInd2 )
!v3.02.00h!v3.02.00c-mlb print *, "     GetAero:", VIndNorm, VTotNorm, AInd,SWcorr
!v3.02.00h
!v3.02.00h
!v3.02.00h         ! Calculate the airflow angle.
!v3.02.00h
!v3.02.00h      AFang  = ATAN2( VIndNorm, VIndTang )
!v3.02.00h!Start of proposed change.  v3.02.00b-mlb 22-August-2006  M. Buhl
!v3.02.00h!v3.02.00b-mlb      IF ( AFang == 0.0 )  AFang = SQRT( Epsilon( AFang ) )
!v3.02.00h!End of proposed change.  v3.02.00b-mlb 22-August-2006  M. Buhl
!v3.02.00h      AFangD = AFang*R2D
!v3.02.00h      CosAF  = COS( AFang )
!v3.02.00h      SinAF  = SIN( AFang )
!v3.02.00h
!v3.02.00h
!v3.02.00h         ! Calculate the angle of attack.  Ensure that it is within +/- 180 degrees.
!v3.02.00h
!v3.02.00h      AlfaR   = AFang - IncidAng
!v3.02.00h      AlfaD   = AlfaR*R2D
!v3.02.00h
!v3.02.00h      IF ( AlfaD > 180.0 )  THEN
!v3.02.00h         AlfaD = AlfaD - 360.0
!v3.02.00h      ELSEIF ( AlfaD < -180.0 )  THEN
!v3.02.00h         AlfaD = AlfaD + 360.0
!v3.02.00h      ENDIF
!v3.02.00h
!v3.02.00h
!v3.02.00h         ! Tip-loss calculation.
!v3.02.00h
!v3.02.00h      IF ( TipLoss )  LossTip = Prandtl( 1.0, RLocND(ISeg), SinAF )
!v3.02.00h
!v3.02.00h
!v3.02.00h         ! Don't calculate the hub loss if there is no hub.
!v3.02.00h
!v3.02.00h      IF ( HubLoss .AND. ( HubRadND .GT. EPSILON( HubRadND ) ) )  LossHub = Prandtl( RLocND(ISeg), HubRadND, SinAF )
!v3.02.00h
!v3.02.00h
!v3.02.00h         ! Total the losses.
!v3.02.00h
!v3.02.00h      Loss = LossTip*LossHub
!v3.02.00h
!v3.02.00h
!v3.02.00h         ! Calculate Reynolds number.
!v3.02.00h
!v3.02.00h      Re = VInd*Chord(ISeg)*RotorRad/KinVisc
!v3.02.00h
!v3.02.00h
!v3.02.00h         ! Calculate Cn and Ct for the local segment.
!v3.02.00h
!v3.02.00h      IF ( AIDrag .OR. TIDrag )  THEN
!v3.02.00h         CALL GetCoefs ( ISeg, AlfaD, Re, AF_Table(AFfile(ISeg)), ClLoc, CdLoc, CmLoc, .TRUE., .TRUE., .FALSE. )
!v3.02.00h      ELSE
!v3.02.00h         CALL GetCoefs ( ISeg, AlfaD, Re, AF_Table(AFfile(ISeg)), ClLoc, CdLoc, CmLoc, .TRUE., .FALSE., .FALSE. )
!v3.02.00h         CdLoc = 0.0
!v3.02.00h      ENDIF
!v3.02.00h
!v3.02.00h      IF ( AIDrag )  THEN
!v3.02.00h         CnLoc = ClLoc*CosAF + CdLoc*SinAF
!v3.02.00h      ELSE
!v3.02.00h         CnLoc = ClLoc*CosAF
!v3.02.00h      ENDIF
!v3.02.00h
!v3.02.00h      IF ( TIDrag )  THEN
!v3.02.00h         CtLoc = ClLoc*SinAF - CdLoc*CosAF
!v3.02.00h      ELSE
!v3.02.00h         CtLoc = ClLoc*SinAF
!v3.02.00h      ENDIF
!v3.02.00h
!v3.02.00h
!v3.02.00h      RETURN
!v3.02.00h      END SUBROUTINE GetAero ! ( AInd, TInd )
!End of proposed change.  v3.02.00h-mlb 11-Sept-2006  M. Buhl
!Start of proposed change.  v3.02.00g-mlb 9-Sept-2006  M. Buhl
!v3.02.00g Added new routine.
   !-----------------------------------------------------------------------
      SUBROUTINE GetAFTI ( AInd, TInd, AF )


         ! Get the airflow angle and tangential induction factor.


         ! Argument declarations.

      REAL(ReKi), INTENT(INOUT)    :: AF                                           ! The airflow angle.
      REAL(ReKi), INTENT(IN)       :: AInd                                         ! The axial-induction factor.
      REAL(ReKi), INTENT(INOUT)    :: TInd                                         ! The tangential-induction factor.


         ! Local declarations.

      REAL(ReKi)                   :: AlfaR                                        ! The angle of attack in radians.
      REAL(ReKi)                   :: LossHub  = 1.0                               ! Hub-loss correction factor.
      REAL(ReKi)                   :: LossTip  = 1.0                               ! Tip-loss correction factor.
      REAL(ReKi)                   :: Re                                           ! Reynolds number.
      REAL(ReKi)                   :: VInd                                         ! The total relative wind speed.

      INTEGER                      :: I                                            ! DO loop counter.
      INTEGER, PARAMETER           :: NAFTIit  = 10                                ! Number of iterations for airflow angle and tangential induction for a given axial induction.


         ! External references.

     ! REAL(ReKi)                   :: Prandtl                                      ! The Prandtl tip.hub-loss function. -Commented 6-4-09 DCM



      VIndTang = VTotTang
      VIndNrm2 = ( VTotNorm*( 1.0 - AInd*SWcorr ) )**2

      DO I=1,NAFTIit

         VIndTang = VTotTang*( 1.0 + TInd )
         VInd     = SQRT( VIndTang**2 + VIndNrm2 )
         AF       = ATAN2( 1.0-AInd, (1.0+TInd)*LSR )
         CosAF    = COS( AF )
         SinAF    = SIN( AF )
         AlfaR    = AF - IncidAng
         AlfaD    = AlfaR*R2D

         IF ( AlfaD > 180.0 )  THEN
            AlfaD = AlfaD - 360.0
         ELSEIF ( AlfaD < -180.0 )  THEN
            AlfaD = AlfaD + 360.0
         ENDIF

!Start of proposed change.  v3.04.00a-mlb, 12-Apr-2010,  M. Buhl
!v3.04.00a         IF ( TIDrag )  THEN
!v3.04.00a            CALL GetCoefs ( ISeg, AlfaD, Re, AF_Table(AFfile(ISeg)), ClLoc, CdLoc, CmLoc, .TRUE., .TRUE., .FALSE. )
!v3.04.00a         ELSE
!v3.04.00a            CALL GetCoefs ( ISeg, AlfaD, Re, AF_Table(AFfile(ISeg)), ClLoc, CdLoc, CmLoc, .TRUE., .FALSE., .FALSE. )
!v3.04.00a            CdLoc = 0.0
!v3.04.00a         ENDIF
         IF ( TIDrag )  THEN
            CALL GetCoefs ( ISeg, AlfaD, Re, AF_Table(AFfile(ISeg)), ClLoc, CdLoc, CmLoc, CpminLoc, .TRUE.,.TRUE. ,.FALSE.,.FALSE. )
         ELSE
            CALL GetCoefs ( ISeg, AlfaD, Re, AF_Table(AFfile(ISeg)), ClLoc, CdLoc, CmLoc, CpminLoc, .TRUE.,.FALSE.,.FALSE.,.FALSE. )
            CdLoc = 0.0
         ENDIF
!End of proposed change.  v3.04.00a-mlb, 12-Apr-2010,  M. Buhl

         CtLoc = ClLoc*SinAF - CdLoc*CosAF



            ! Calculate the losses.

         IF ( TipLoss )                                              LossTip = Prandtl( 1.0,          RLocND(ISeg), SinAF )
         IF ( HubLoss .AND. ( HubRadND .GT. EPSILON( HubRadND ) ) )  LossHub = Prandtl( RLocND(ISeg), HubRadND,     SinAF )

         Loss = LossTip*LossHub

!Temp Start 3.03.01b03-dcm, 4-Aug-2009
TInd = TanIndErr( AInd,TInd ) + TInd

!Temp Start 3.02.01a00-dcm, 27-Jul-2009


!SingUpper =((1.0718*ABS(CtLoc*Solidity) - 0.003)* ABS(LSR) + 1.01)* (SingOffset+1.0)

!IF (TISingularity .And. AInd .GE. 2.0-SingUpper .And. AInd .LE. SingUpper) THEN
!A0Sing = 1.0
!A1Sing = SingUpper
!SingTransition = cos((Pi/2.0)*(AInd-A0Sing)/(A1Sing - A0Sing) - (Pi/2.0))!Cosine Transition
!SingTransition = SingTransition*SingTransition
    !     PRINT '(A,2ES14.6)',  " SingAvoidance Activated!A,ST ", AInd, SingTransition
!ELSE
!SingTransition = 1.0
!END IF

!TanIndCoef_Cd = Solidity*CdLoc / (4.0*Loss*SinAF*CosCone)
!TanIndCoef_Cl = Solidity*ClLoc / (4.0*Loss*CosAF*CosCone)
!TanIndCoef = TanIndCoef_Cl - SingTransition*TanIndCoef_Cd
!TanIndCoef_Old = TanIndCoef_Cl - TanIndCoef_Cd
!TInd = TanIndCoef/(1-TanIndCoef)
!Temp End 3.02.01a00-dcm, 27-Jul-2009

!Temp End 3.03.01b03-dcm, 4-Aug-2009

      END DO

      IF ( AIDrag )  THEN
         CnLoc = ClLoc*CosAF + CdLoc*SinAF
      ELSE
         CnLoc = ClLoc*CosAF
      ENDIF


      RETURN
      END SUBROUTINE GetAFTI ! ( AInd, TInd, AF )
!End of proposed change.  v3.02.00g-mlb 9-Sept-2006  M. Buhl
   !-----------------------------------------------------------------------
      SUBROUTINE NewtRaph ( AxInd, TanInd,Display )


         ! Use Newton_Raphson to find the induction factors.


         ! Argument declarations.

      REAL(ReKi), INTENT(INOUT)    :: AxInd                                        ! The axial-induction factor.
      REAL(ReKi), INTENT(INOUT)    :: TanInd                                       ! The tangential-induction factor.
      Logical, INTENT(IN)    :: Display                                       ! Display screen output?

         ! Local declarations.

      REAL(ReKi)                   :: AIdel                                        ! The iterative change in the axial induction factor.
      REAL(ReKi)                   :: AxErrAP                                      ! The error in the axial induction after perturbing the old value of the axial induction.
      REAL(ReKi)                   :: AxErrSlp                                     ! The slope of the axial-only error function.
      REAL(ReKi)                   :: AxErrTP                                      ! The error in the axial induction after perturbing the old value of the tangential induction.
      REAL(ReKi)                   :: AxErrUP                                      ! The unperturbed error in the axial induction.
      REAL(ReKi)                   :: DelAx                                        ! THe amount of change for the axial-induction finite differencing.
      REAL(ReKi)                   :: DelTan                                       ! THe amount of change for the tangential-induction finite differencing.
      REAL(ReKi)                   :: FunctAP                                      ! The axial-only error function using a finite differenced value for the old axial induction.
      REAL(ReKi)                   :: FunctUP                                      ! The unperturbed axial-only error function.
      REAL(ReKi)                   :: JacDet                                       ! The determinant of the Jacobian matrix.
      REAL(ReKi)                   :: JacInv   (2,2)                               ! The inverse of the Jacobian matrix.
      REAL(ReKi)                   :: Jacob    (2,2)                               ! The Jacobian matrix.
      REAL(ReKi)                   :: SumSqChng                                    ! The sum of the squares of the changes in the inductions.
      REAL(ReKi)                   :: TanErrAP                                     ! The error in the tangential induction after perturbing the old value of the axial induction.
      REAL(ReKi)                   :: TanErrTP                                     ! The error in the tangential induction after perturbing the old value of the tangential induction.
      REAL(ReKi)                   :: TanErrUP                                     ! The unperturbed error in the tangential induction.
      REAL(ReKi)                   :: TIdel                                        ! The iterative change in the tangential induction factor.

 !Temp Start 3.03.01c01-dcm, 6-Aug-2009
      REAL(ReKi)                   :: AxIndPrev1                                      ! The error in the axial induction after perturbing the old value of the axial induction.
      REAL(ReKi)                   :: AxIndPrev2                                      ! The error in the axial induction after perturbing the old value of the axial induction.
 !Temp End 3.03.01c01-dcm, 6-Aug-2009

         ! Are we including swirl effects?

      IF ( Swirl )  THEN                        ! Include tangential-induction effects.

  !PRINT '(A,2ES18.10)',  "1 AIdel,TIdel ", AxInd,TanInd
            ! Compute unperturbed induction,

         CALL GetAero ( ISeg, AxInd, TanInd )

         AxErrUP  =  AxIndErr(  AxInd )
         TanErrUP = TanIndErr( AxInd,TanInd )

!#IFDEF debugn
         !PRINT '(A,I3,2(ES18.10,ES14.6))',  "    N-R UP: ", Iters, AxInd, AxErrUP, TanInd, TanErrUP
!#ENDIF

!Start of proposed change.  v3.02.00g-mlb 9-Sept-2006  M. Buhl
! This test is more consistent with the others.

         ! See if convergnce is achieved.

      IF ( ( AxErrUP**2 + TanErrUP**2 ) <= ATol2 )  THEN
         Converg = .TRUE.
         RETURN
      ENDIF

!End of proposed change.  v3.02.00g-mlb 9-Sept-2006  M. Buhl

            ! Perturb axial induction for finite differencing.

         DelAx = SIGN( MAX( ABS( AxInd*Del ), SQRT( Epsilon( AxInd ) ) ), AxInd )

         CALL GetAero ( ISeg, AxInd + DelAx, TanInd )

         AxErrAP  =  AxIndErr(  AxInd + DelAx )
         TanErrAP = TanIndErr( AxInd + DelAx,TanInd )

!#IFDEF debugn
         !PRINT '(A,I3,2(F14.10,ES14.6))',  "    N-R DA: ", Iters, AxInd + DelAx, AxErrAP, TanInd, TanErrUP
!#ENDIF

            ! Perturb tangential induction for finite differencing.

         DelTan = SIGN( MAX( ABS( TanInd*Del ), SQRT( Epsilon( TanInd ) ) ), TanInd )

         CALL GetAero ( ISeg, AxInd, TanInd + DelTan )

         AxErrTP  =  AxIndErr(  AxInd )
         TanErrTP = TanIndErr( AxInd,TanInd + DelTan )

!#IFDEF debugn
         !PRINT '(A,I3,2(F14.10,ES14.6))',  "    N-R DT: ", Iters, AxInd, AxErrTP, TanInd + DelTan, TanErrTP
!#ENDIF

            ! Compute the Jacobian.

         Jacob(1,1) = (  AxErrAP -  AxErrUP )/DelAx
         Jacob(1,2) = (  AxErrTP -  AxErrUP )/DelTan
         Jacob(2,1) = ( TanErrAP - TanErrUP )/DelAx
         Jacob(2,2) = ( TanErrTP - TanErrUP )/DelTan


            ! Invert the Jacobian.

         JacDet = Jacob(1,1)*Jacob(2,2) - Jacob(1,2)*Jacob(2,1)

         JacInv(1,1) =  Jacob(2,2)/JacDet
         JacInv(1,2) = -Jacob(1,2)/JacDet
         JacInv(2,1) = -Jacob(2,1)/JacDet
         JacInv(2,2) =  Jacob(1,1)/JacDet


            ! Calculate the required changes, the magnitude of the change squared, and the new values.

         AIdel = -JacInv(1,1)*AxErrUP - JacInv(1,2)*TanErrUP
         TIdel = -JacInv(2,1)*AxErrUP - JacInv(2,2)*TanErrUP

         SumSqChng = AIdel**2 + TIdel**2

         !!Temp Start 3.03.01c01-dcm, 6-Aug-2009
         !If (AxIndPrev2 .EQ. AxInd) THEN
         !AIdel = AIdel/20 !What it should do is if this is true and it is beyond, say 5 iterations, it should jump to a simple marching routine
         !ENDIF
         !Temp End 3.03.01c01-dcm, 6-Aug-2009

         AxInd  =  AxInd + AIdel
!         AxInd  =  AxInd + SIGN( MIN( ABS( AIdel ), 0.02 ), AIdel )
         TanInd = TanInd + TIdel
!         TanInd = TanInd + SIGN( MIN( ABS( TIdel ), 0.01 ), TIdel )

 !PRINT '(A,2ES18.10)',  " AIdel,TIdel ", AIdel,TIdel
!IF( Display) THEN
 ! PRINT '(A,4ES18.10)',  "2 AIdel,TIdel ", AxInd,AIdel,TanInd,TIdel
!ENDIF

!Temp Start 3.03.01c01-dcm, 6-Aug-2009
!Store values for "stuck iteration checking"
AxIndPrev1     = AxInd
AxIndPrev2 = AxIndPrev1
 !Temp End 3.03.01c01-dcm, 6-Aug-2009

      ELSE                                      ! Exclude tangential-induction effects.


            ! Compute unperturbed induction,

         CALL GetAero ( ISeg, AxInd, 0.0 )

         FunctUP = AxIndErr( AxInd )

!#IFDEF debugn
         !PRINT '(A,I3,F10.6,ES11.3)',  "    N-R UP: ", Iters, AxInd, FunctUP
!#ENDIF

!Start of proposed change.  v3.02.00g-mlb 9-Sept-2006  M. Buhl
! This test is more consistent with the others.

            ! See if convergnce is achieved.

         IF ( FunctUP**2 <= ATol2 )  THEN
            Converg = .TRUE.
            RETURN
         ENDIF

!End of proposed change.  v3.02.00g-mlb 9-Sept-2006  M. Buhl

            ! Perturb axial induction for finite differencing.

         CALL GetAero ( ISeg, AxInd+Del, 0.0 )

         FunctAP  = AxIndErr( AxInd+Del )



            ! Compute slope.

         AxErrSlp  = ( FunctAP - FunctUP )/del
         AIdel     = -FunctUP/AxErrSlp
         SumSqChng = AIdel**2



         AxInd     = AxInd + AIdel
!#IFDEF debugn
  !       PRINT '(A,I3,F10.6,ES11.3)',  "    N-R DA: ", Iters, AxInd+Del, FunctAP
!#ENDIF



      ENDIF

!Start of proposed change.  v3.02.00g-mlb 9-Sept-2006  M. Buhl
! Test is now done near the top of the routine.
!v3.02.00g         ! See if convergnce is achieved.
!v3.02.00g
!v3.02.00g      IF ( SumSqChng <= ATol2 )  THEN
!v3.02.00g
!v3.02.00g         Converg = .TRUE.
!v3.02.00g
!v3.02.00g      ENDIF
!v3.02.00g
!End of proposed change.  v3.02.00g-mlb 9-Sept-2006  M. Buhl

      RETURN
      END SUBROUTINE NewtRaph ! ( AxInd, TanInd )
   !-----------------------------------------------------------------------
      FUNCTION TanIndErr ( AInd,TInd )


         ! Compute the error in the tangential induction.


         ! Function declaration.

      REAL(ReKi)                   :: TanIndErr                                    ! This function.


         ! Argument declarations.

      REAL(ReKi), INTENT(IN)       :: TInd                                         ! The tangential-induction factor.
      REAL(ReKi), INTENT(IN)       :: AInd                                         ! The tangential-induction factor.
      REAL(ReKi)       :: TanIndErrOld

         ! Calcaluate error in tangential induction.
         ! Account for case where CtLoc is zero (means SinAF is zero).

!Start of proposed change.  v3.02.00b-mlb 22-August-2006  M. Buhl
!v3.02.00b-mlb      TanIndErr = 1.0/( ( 4.0*Loss*SinAF*CosAF )/( Solidity*CtLoc ) - 1.0 ) - TInd
      IF ( CtLoc == 0.0 )  THEN
         TanIndErr = 1.0/( 4.0*Loss/Solidity - 1.0 ) - TInd
      ELSE
!
!Temp Start 3.03.01a04-dcm, 29-Jul-2009
         !TanIndErr = 1.0/( ( 4.0*Loss*SinAF*CosAF )/( Solidity*CtLoc ) - 1.0 ) - TInd

         !Temp Start 3.02.01a00-dcm, 27-Jul-2009


    SingUpper =((1.0718*ABS(CtLoc*Solidity) - 0.003)* ABS(LSR) + 1.01)* (SingOffset+1.0)

    IF (TISingularity .And. AInd .GE. 2.0-SingUpper .And. AInd .LE. SingUpper) THEN
    A0Sing = 1.0
    A1Sing = SingUpper
    SingTransition = cos((Pi/2.0)*(AInd-A0Sing)/(A1Sing - A0Sing) - (Pi/2.0))!Cosine Transition
    SingTransition = SingTransition*SingTransition
    !Print *," SingAvoidance Activated!"
    ELSE
    SingTransition = 1.0
    END IF

    TanIndCoef_Cd = Solidity*CdLoc / (4.0*Loss*SinAF*CosCone)
    TanIndCoef_Cl = Solidity*ClLoc / (4.0*Loss*CosAF*CosCone)
    TanIndCoef = TanIndCoef_Cl - SingTransition*TanIndCoef_Cd
    !TanIndCoef_Old = TanIndCoef_Cl - TanIndCoef_Cd
    !TInd = TanIndCoef/(1-TanIndCoef)

    TanIndErr = (TanIndCoef/(1-TanIndCoef)) - TInd
    !Temp End 3.02.01a00-dcm, 27-Jul-2009

     !TanIndErrOld = 1.0/( ( 4.0*Loss*SinAF*CosAF*CosCone)/( Solidity*CtLoc ) - 1.0 ) - TInd
     !NOTE!  CosCone should be in the TanInd equation, see EQN. 7.7 , pg 52, Wilson, R.  "Performance Analysis of HAWT", Sept. 1984
ENDIF
!Equation from 2004 version:
    !TanIndErr = 1.0/( ( 4.0*Loss*SinAF*CosAF*CosCone )/( Solidity*CtLoc ) - 1.0 ) - TInd



      RETURN
      END FUNCTION TanIndErr ! ( TInd )
   !-----------------------------------------------------------------------

!Start of proposed change.  v3.02.00c-mlb 24-August-2006  M. Buhl
!v3.02.00c   END SUBROUTINE Ind_Prop ! ( ISeg, Converg )
!Start of proposed change.  v3.02.00g-mlb 7-September-2006  M. Buhl
!v3.02.00g   END SUBROUTINE Ind_Prop ! ( ISeg, ZFound, Converg )
   END SUBROUTINE InductBEM ! ( ISeg, ZFound, Converg )
!End of proposed change.  v3.02.00g-mlb 7-September-2006  M. Buhl
!End of proposed change.  v3.02.00c-mlb 24-August-2006  M. Buhl
!End of proposed change.  v3.02.00b-mlb 22-August-2006  M. Buhl
!End of proposed change.  v3.02.00a-mlb 03-August-2006  M. Buhl
!Start of proposed change.  v3.02.00b-mlb 22-August-2006  M. Buhl
!v3.02.00b-mlb!=======================================================================
!v3.02.00b-mlb   SUBROUTINE Ind_PropX ( ISeg, Converge )
!v3.02.00b-mlb
!v3.02.00b-mlb
!v3.02.00b-mlb      ! This routine calculates the induction factors using the PROPX/AeroDyn method.
!v3.02.00b-mlb
!v3.02.00b-mlb
!v3.02.00b-mlb   USE                             Parameters
!v3.02.00b-mlb   USE                             ProgGen
!v3.02.00b-mlb   USE                             WTP_Data
!v3.02.00b-mlb
!v3.02.00b-mlb   IMPLICIT                        NONE
!v3.02.00b-mlb
!v3.02.00b-mlb
!v3.02.00b-mlb      ! Argument declarations.
!v3.02.00b-mlb
!v3.02.00b-mlb   INTEGER, INTENT(IN)          :: ISeg                                         ! The segment number.
!v3.02.00b-mlb
!v3.02.00b-mlb   LOGICAL, INTENT(OUT)         :: Converge                                     ! Flag that says if we converged the induction loop.
!v3.02.00b-mlb
!v3.02.00b-mlb
!v3.02.00b-mlb      ! Local declarations.
!v3.02.00b-mlb
!v3.02.00b-mlb   REAL(ReKi)                   :: AFang                                        ! The angle between the wind vector and the cone of rotation in radians.
!v3.02.00b-mlb   REAL(ReKi)                   :: AIdel                                        ! The iterative change in the axial induction factor.
!v3.02.00b-mlb   REAL(ReKi)                   :: AIdelT                                       ! The change in the axial induction factor tha tis tested against the convergenge criterion.
!v3.02.00b-mlb   REAL(ReKi)                   :: AIdel_O                                      ! The previous change in the axial induction factor.
!v3.02.00b-mlb   REAL(ReKi)                   :: AInew                                        ! The proposed new axial induction factor.
!v3.02.00b-mlb   REAL(ReKi)                   :: AIstep                                       ! The maximum amount to change the axial induction factor during one iteration.
!v3.02.00b-mlb   REAL(ReKi)                   :: AlfaR                                        ! The angle of attack in radians.
!v3.02.00b-mlb   REAL(ReKi)                   :: CH                                           ! The head-loss coefficient.
!v3.02.00b-mlb   REAL(ReKi)                   :: CmLoc                                        ! The local pitching-moment coefficient (unused).
!v3.02.00b-mlb   REAL(ReKi)                   :: CnLoc                                        ! The local normal coefficient.
!v3.02.00b-mlb   REAL(ReKi)                   :: LossHub  = 1.0                               ! Hub-loss correction factor.
!v3.02.00b-mlb   REAL(ReKi)                   :: LossTip  = 1.0                               ! Tip-loss correction factor.
!v3.02.00b-mlb   REAL(ReKi)                   :: Re                                           ! Reynolds number.
!v3.02.00b-mlb   REAL(ReKi)                   :: SwrlArg                                      ! An argument in the swirl calculation.
!v3.02.00b-mlb   REAL(ReKi)                   :: TIdel                                        ! The iterative change in the tangential induction factor.
!v3.02.00b-mlb   REAL(ReKi)                   :: TIdel_O                                      ! The previous change in the tangential induction factor.
!v3.02.00b-mlb   REAL(ReKi)                   :: TIstep                                       ! The maximum amount to change the tangential induction factor during one iteration.
!v3.02.00b-mlb   REAL(ReKi)                   :: VInd                                         ! The total relative wind speed.
!v3.02.00b-mlb   REAL(ReKi)                   :: VTotNrm2                                     ! Total (uninduced) normal wind speed squared.
!v3.02.00b-mlb
!v3.02.00b-mlb   INTEGER                      :: Iter                                         ! The iteration counter.
!v3.02.00b-mlb   INTEGER                      :: ItSLSC                                       ! The number of iterations since the last time the change in AI changed sign.
!v3.02.00b-mlb
!v3.02.00b-mlb
!v3.02.00b-mlb
!v3.02.00b-mlb      ! Initialize induction-factor variables.
!v3.02.00b-mlb
!v3.02.00b-mlb   AxInd   = 0.0
!v3.02.00b-mlb   AIdel   = 0.0
!v3.02.00b-mlb   AIdel_O = 0.0
!v3.02.00b-mlb   AIstep  = 0.5
!v3.02.00b-mlb
!v3.02.00b-mlb   TanInd  = 0.0
!v3.02.00b-mlb   TIdel   = 0.0
!v3.02.00b-mlb   TIdel_O = 0.0
!v3.02.00b-mlb   TIstep  = 0.5
!v3.02.00b-mlb
!v3.02.00b-mlb   Converge = .FALSE.
!v3.02.00b-mlb   ItSLSC   = 0
!v3.02.00b-mlb
!v3.02.00b-mlb
!v3.02.00b-mlb   InductionLoop: DO Iter=1,MaxIter
!v3.02.00b-mlb
!v3.02.00b-mlb
!v3.02.00b-mlb         ! Apply the induction factors to the speeds.
!v3.02.00b-mlb
!v3.02.00b-mlb      VTotNrm2 = VTotNorm**2
!v3.02.00b-mlb      VIndTang = VTotTang*( 1.0 + TanInd )
!v3.02.00b-mlb      VIndNorm = VTotNorm*( 1.0 - AxInd*SWcorr )
!v3.02.00b-mlb      VIndNrm2 = VIndNorm**2
!v3.02.00b-mlb      VInd2    = VIndTang**2 + VIndNrm2
!v3.02.00b-mlb      VInd     = SQRT( VInd2 )
!v3.02.00b-mlb
!v3.02.00b-mlb
!v3.02.00b-mlb         ! Calculate Reynolds number.
!v3.02.00b-mlb
!v3.02.00b-mlb      Re = VInd*Chord(ISeg)*RotorRad/KinVisc
!v3.02.00b-mlb
!v3.02.00b-mlb
!v3.02.00b-mlb         ! Calculate the airflow angle.
!v3.02.00b-mlb
!v3.02.00b-mlb      AFang  = ATAN2( VIndNorm, VIndTang )
!v3.02.00b-mlb      AFangD = AFang*R2D
!v3.02.00b-mlb      CosAF  = COS( AFang )
!v3.02.00b-mlb      SinAF  = SIN( AFang )
!v3.02.00b-mlb
!v3.02.00b-mlb
!v3.02.00b-mlb         ! Calculate the angle of attack.
!v3.02.00b-mlb
!v3.02.00b-mlb      AlfaR   = AFang - IncidAng
!v3.02.00b-mlb      AlfaD   = AlfaR*R2D
!v3.02.00b-mlb
!v3.02.00b-mlb
!v3.02.00b-mlb         !   Tip-loss calculation.
!v3.02.00b-mlb
!v3.02.00b-mlb      IF ( TipLoss )  LossTip = Prandtl( 1.0, RLocND(ISeg), SinAF )
!v3.02.00b-mlb
!v3.02.00b-mlb
!v3.02.00b-mlb         !   Don't calculate the hub loss if there is no hub.
!v3.02.00b-mlb
!v3.02.00b-mlb!Start of proposed change.  v3.02.00a-mlb 03-August-2006  M. Buhl
!v3.02.00b-mlb!v3.02.00a      IF ( HubLoss .AND. ( HubRadND .GT. Epsilon ) )  LossHub = Prandtl( RLocND(ISeg), HubRadND, SinAF )
!v3.02.00b-mlb      IF ( HubLoss .AND. ( HubRadND .GT. EPSILON( HubRadND ) ) )  LossHub = Prandtl( RLocND(ISeg), HubRadND, SinAF )
!v3.02.00b-mlb!End of proposed change.  v3.02.00a-mlb 03-August-2006  M. Buhl
!v3.02.00b-mlb
!v3.02.00b-mlb
!v3.02.00b-mlb         ! Total the losses.
!v3.02.00b-mlb
!v3.02.00b-mlb      Loss = LossTip*LossHub
!v3.02.00b-mlb
!v3.02.00b-mlb
!v3.02.00b-mlb         ! Calculate Cn for the local segment.
!v3.02.00b-mlb
!v3.02.00b-mlb      IF ( AIDrag .OR. TIDrag )  THEN
!v3.02.00b-mlb         CALL GetCoefs ( ISeg, AlfaD, Re, AF_Table(AFfile(ISeg)), ClLoc, CdLoc, CmLoc, .TRUE., .TRUE., .FALSE. )
!v3.02.00b-mlb      ELSE
!v3.02.00b-mlb         CALL GetCoefs ( ISeg, AlfaD, Re, AF_Table(AFfile(ISeg)), ClLoc, CdLoc, CmLoc, .TRUE., .FALSE., .FALSE. )
!v3.02.00b-mlb         CdLoc = 0.0
!v3.02.00b-mlb      ENDIF
!v3.02.00b-mlb
!v3.02.00b-mlb      IF ( AIDrag )  THEN
!v3.02.00b-mlb         CnLoc = ClLoc*CosAF + CdLoc*SinAF
!v3.02.00b-mlb      ELSE
!v3.02.00b-mlb         CnLoc = ClLoc*CosAF
!v3.02.00b-mlb      ENDIF
!v3.02.00b-mlb
!v3.02.00b-mlb
!v3.02.00b-mlb         ! Calculate the head-loss coefficient.  Ensure it lies between -2 and +2.
!v3.02.00b-mlb
!v3.02.00b-mlb      CH = VInd2*Solidity*CnLoc/VTotNrm2
!v3.02.00b-mlb
!v3.02.00b-mlb      CH = MIN( MAX( -2.0, CH ), 2.0 )
!v3.02.00b-mlb
!v3.02.00b-mlb
!v3.02.00b-mlb         ! Compute the new axial-induction factor.
!v3.02.00b-mlb
!v3.02.00b-mlb      IF ( CH < 0.96*Loss ) THEN
!v3.02.00b-mlb         AInew = 0.5*( 1 - SQRT( 1.0 - CH/Loss ) )
!v3.02.00b-mlb      ELSE
!v3.02.00b-mlb         AInew = 0.1432 + SQRT( -0.55106 + .6427*CH/Loss)
!v3.02.00b-mlb      ENDIF
!v3.02.00b-mlb
!v3.02.00b-mlb
!v3.02.00b-mlb         ! Propose new axial induction factor.  Limit the delta by half of
!v3.02.00b-mlb         ! what we think we need.  If we jump over the solution, reduce
!v3.02.00b-mlb         ! that fraction by half.  If we don't jump over the solution in 10
!v3.02.00b-mlb         ! attempts, double the step size.
!v3.02.00b-mlb
!v3.02.00b-mlb      AIdel = AInew - AxInd
!v3.02.00b-mlb
!v3.02.00b-mlb      IF ( ( AIdel_O /= 0.0 ) .AND. ( AIdel/AIdel_O < 0.0 ) )  THEN
!v3.02.00b-mlb         AIstep = 0.5*AIstep
!v3.02.00b-mlb         ItSLSC = 0
!v3.02.00b-mlb      ELSEIF ( ItSLSC == 10 )  THEN
!v3.02.00b-mlb         AIstep = 2.0*AIstep
!v3.02.00b-mlb         ItSLSC = 0
!v3.02.00b-mlb      ELSE
!v3.02.00b-mlb         ItSLSC = ItSLSC + 1
!v3.02.00b-mlb      ENDIF
!v3.02.00b-mlb
!v3.02.00b-mlb      AIdelT  = AIstep*AIdel
!v3.02.00b-mlb      AxInd   = AxInd + AIdelT
!v3.02.00b-mlb      AIdel_O = AIdelT
!v3.02.00b-mlb
!v3.02.00b-mlb
!v3.02.00b-mlb         ! Propose new tangential induction factor if we're doing Swirl.
!v3.02.00b-mlb
!v3.02.00b-mlb         ! NOTE:  According to Craig Hansen, this algorithm is not valid at high angles of attack (i.e., above stall).
!v3.02.00b-mlb
!v3.02.00b-mlb      IF ( Swirl ) THEN
!v3.02.00b-mlb
!v3.02.00b-mlb         SwrlArg = 1.0 + 4.0*AxInd*Loss*( 1.0 - AxInd )/SpdRatio**2
!v3.02.00b-mlb
!v3.02.00b-mlb         IF ( SwrlArg < 0.0 ) THEN
!v3.02.00b-mlb            TanInd = 0.0
!v3.02.00b-mlb         ELSE
!v3.02.00b-mlb            TanInd = 0.5*( SQRT( SwrlArg ) - 1.0  )
!v3.02.00b-mlb         ENDIF
!v3.02.00b-mlb
!v3.02.00b-mlb      ELSE
!v3.02.00b-mlb
!v3.02.00b-mlb         TanInd = 0.0
!v3.02.00b-mlb
!v3.02.00b-mlb      ENDIF
!v3.02.00b-mlb
!v3.02.00b-mlb
!v3.02.00b-mlb         ! See if convergence is achieved.  Get final Cd if it was not used in the induction calculation.
!v3.02.00b-mlb
!v3.02.00b-mlb      IF ( ABS( AIdelT ) <= ATol )  THEN
!v3.02.00b-mlb
!v3.02.00b-mlb         Converge = .TRUE.
!v3.02.00b-mlb
!v3.02.00b-mlb
!v3.02.00b-mlb            ! Add skewed-wake correction to axial induction.
!v3.02.00b-mlb
!v3.02.00b-mlb         AxInd = AxInd*SWcorr
!v3.02.00b-mlb
!v3.02.00b-mlb         IF ( .NOT. ( AIDrag .OR. TIDRAG ) )  THEN
!v3.02.00b-mlb            CALL GetCoefs ( ISeg, AlfaD, Re, AF_Table(AFfile(ISeg)), ClLoc, CdLoc, CmLoc, .FALSE., .TRUE., .FALSE. )
!v3.02.00b-mlb         ENDIF
!v3.02.00b-mlb
!v3.02.00b-mlb         EXIT InductionLoop
!v3.02.00b-mlb      ENDIF
!v3.02.00b-mlb
!v3.02.00b-mlb   ENDDO InductionLoop ! Iter
!v3.02.00b-mlb
!v3.02.00b-mlb
!v3.02.00b-mlb   RETURN
!v3.02.00b-mlb   END SUBROUTINE Ind_PropX ! ( ISeg, Converge )
!End of proposed change.  v3.02.00b-mlb 22-August-2006  M. Buhl
!=======================================================================
   SUBROUTINE IOInit


      ! This routine opens the I/O files.


   USE                             NWTC_Library
   USE                             Parameters
   USE                             ProgGen

   IMPLICIT                        NONE


      ! Local declarations.

   LOGICAL                      :: Error                                        ! Flag that says whether or not an error occurred.



      ! Get the input file name from the argument string.

   CALL Get_Arg ( 1, InpFile, Error )


      ! Check syntax.

   IF ( Error )  CALL Abort ( ' Syntax is: wt_perf <InpFile>' )


      ! Open the input file.

   CALL OpenFInpFile ( UnIn,    InpFile                  )
   CALL GetRoot      ( InpFile, RootName                 )
   CALL OpenFOutFile ( UnOu,    TRIM( RootName )//'.oup' )


   RETURN
   END SUBROUTINE IOInit
!=======================================================================
   SUBROUTINE ParmAnal


      ! This routine performs the rotor analysis.


   USE                             Parameters
   USE                             ProgGen
   USE                             WTP_Data

   IMPLICIT                        NONE


      ! Argument declarations.



      ! Local decarations.

!Start of proposed change.  v3.03.02a-mlb, 30-Dec-2009  M. Buhl
   REAL(ReKi)                   :: MaxCp                                           ! The maximum Cp for a given sheet.

!End of proposed change.  v3.03.02a-mlb, 30-Dec-2009  M. Buhl
   INTEGER                      :: ICol                                            ! Index for the column parameter.
   INTEGER                      :: IOmg                                            ! Index for rotor speed.
   INTEGER                      :: IPit                                            ! Index for pitch angle.
   INTEGER                      :: IRow                                            ! Index for the row parameter.
   INTEGER                      :: ISpd                                            ! Index for wind speed or TSR.
   INTEGER                      :: ITab                                            ! Index for the table parameter.

   CHARACTER( 20), PARAMETER    :: FmtCpSpc = "(F7.3,1000(F11.4,:))"
!mlb   CHARACTER( 24), PARAMETER    :: FmtCpTab = "(F7.3,1000('"//Tab//"',F7.4,:))"
   CHARACTER( 24), PARAMETER    :: FmtCpTab = "(F7.3,1000('"//Tab//"',F9.6,:))"
   CHARACTER( 20), PARAMETER    :: FmtDaSpc = "(F7.3,1000(F11.3,:))"
   CHARACTER( 24), PARAMETER    :: FmtDaTab = "(F7.3,1000('"//Tab//"',F11.3,:))"
   CHARACTER( 11), PARAMETER    :: FmtDbLin = "(80('='),/)"
   CHARACTER(107), PARAMETER    :: FmtHdSpc = "(80('-'),/," &
                                            //"A,' (', A,') for ',A,' = ',A,' ',A,'.',//," &
                                            //"2X,A,'   ',A,' (',A,')',/," &
                                            //"2X,'(',A,')',1000( F11.3,:))"
   CHARACTER( 99), PARAMETER    :: FmtHdTab = "(80('-'),/" &
                                            //"A,' (',A,') for ',A,' = ',A,' ',A,'.',//" &
                                            //"A,'"//Tab//"',A,' (',A,')',/" &
                                            //"'(',A,')',1000('"//Tab//"',F7.3,:))"


   OmegaLoop: DO IOmg=1,NumOmg      ! -------------------------  Start of rotor-speed loop  -------------------------

      OmgRPM   = OmgAry(IOmg)
      Omega    = OmgRPM*RPM2RPS
      TipSpeed = Omega*SweptRad


         ! Print out rotor speed.

      CALL WrScr ( ' Rotor speed = '//TRIM( Flt2LStr( OmgRPM ) )//' rpm.' )




         ! Start of Pitch-angle loop.

      PitchLoop: DO IPit=1,NumPit

         PitDeg = PitAry(IPit)
         Pitch  = PitDeg*D2R


            ! Print out pitch angle.

!Temp Start 3.03.01a06-dcm, 29-Jul-2009
!         CALL WrScr ( '    Pitch = '//TRIM( Flt2LStr( PitDeg ) )//' deg.' )
!Temp End 3.03.01a06-dcm, 29-Jul-2009

            ! Start of wind-speed or TSR loop.


         SpeedLoop: DO ISpd=1,NumSpd

            Spd = SpdAry(ISpd)

            CALL GetInds ( IOmg, IPit, ISpd, IRow, ICol, ITab )


               ! Make appropriate conversion between wind speed and tip-speed ratio.

            IF ( InputTSR )  THEN
               VelHH = TipSpeed/Spd
               TSR   = Spd
            ELSE
               VelHH = Spd*ConvFact
               TSR   = TipSpeed/VelHH
            ENDIF


               ! Compute rotor performance.

            CALL RotAnal


                 ! Store results.

            IF ( OutPwr .OR. UnfPower )  PwrAry(IRow,ICol,ITab) = Power
            IF ( OutCp  )                CpAry (IRow,ICol,ITab) = PwrC
            IF ( OutTrq )                TrqAry(IRow,ICol,ITab) = Torque
            IF ( OutFlp )                FlpAry(IRow,ICol,ITab) = FlapMom
            IF ( OutThr )                ThrAry(IRow,ICol,ITab) = Thrust

         ENDDO SpeedLoop ! ISpd


            ! Increment the blade Pitch angle.

         PitDeg = PitDeg + PitDel

      ENDDO PitchLoop ! IPit


         ! Increment the rotor speed.

      OmgRPM = OmgRPM + OmgDel

   ENDDO OmegaLoop ! IOmg

!Start of proposed change.  v3.03.02a-mlb, 04-Dec-2009  M. Buhl

      ! If requested, write out array dimensions and power to an unformatted file.

   IF ( UnfPower )  THEN

      CALL OpenUOutfile ( UnUn, TRIM( RootName )//'.pwr' )

      WRITE (UnUn)  MaxRow, MaxCol, MaxTab
      WRITE (UnUn)  ConvPwr*PwrAry(:,:,:)

      CLOSE ( UnUn )

   ENDIF

!End of proposed change.  v3.03.02a-mlb, 04-Dec-2009  M. Buhl

      ! If requested, write out results matrices to a formatted file.

   DO ITab=1,MaxTab


         ! Write out matrix header.

      WRITE (UnOu,'(A)')  'Results generated by '//TRIM( ProgName )//TRIM( ProgVer )//' for input file "'//TRIM( InpFile )//'".'
      WRITE (UnOu,'(A)')  'Generated on '//TRIM( DateNow )//' at '//TRIM( TimeNow )//'.'
      WRITE (UnOu,'(A)')  'Input file title:'
      WRITE (UnOu,'(A)')  '  '//TRIM( RunTitle )
      WRITE (UnOu,'(A)')  ' '

!Start of proposed change.  v3.03.02a-mlb, 30-Dec-2009  M. Buhl

         ! Output the maximum Cp case if requested.

      IF ( OutMaxCp )  THEN

         MaxCp = MAXVAL( CpAry(:,:,ITab) )
         print *, 'row = ', MAXLOC( MAXVAL( CpAry(:,:,ITab), dim=2 ) )
         print *, 'col = ', MAXLOC( MAXVAL( CpAry(:,:,ITab), dim=1 ) )
  !       IRow  = MAXLOC( MAXVAL( CpAry(:,:,ITab), dim=2 ) )
  !       ICol  = MAXLOC( MAXVAL( CpAry(:,:,ITab), dim=1 ) )
         IRow  = 1
         ICol  = 2

         WRITE (UnOu,'(A)')  'Conditions leading to the maximum Cp:'
         WRITE (UnOu,'("  ",A," = ",F7.3," ",A)')  ParamStr(ParRow), RowAry(IRow), UnitsStr(ParRow)
         WRITE (UnOu,'("  ",A," = ",F7.3," ",A)')  ParamStr(ParCol), ColAry(ICol), UnitsStr(ParCol)
         WRITE (UnOu,'("  ",A," = ",F7.3)'      )  'MaxCp', MaxCp
         WRITE (UnOu,'()')

      ENDIF

!End of proposed change.  v3.03.02a-mlb, 29-Dec-2009  M. Buhl

         ! Output power?

      IF ( OutPwr )  THEN

         IF ( TabDel )  THEN
            WRITE (UnOu,FmtHdTab)  'Power', TRIM( ADJUSTL( PwrUnits ) ), TRIM( ParamStr(ParTab) ), TRIM(Flt2LStr(TabAry(ITab))) &
                                  , UnitsStr(ParTab), TRIM( ParamStr(ParRow) ), TRIM( ParamStr(ParCol) ), UnitsStr(ParCol) &
                                  , UnitsStr(ParRow), ( ColAry(ICol), ICol=1,MaxCol )
         ELSE
            WRITE (UnOu,FmtHdSpc)  'Power', TRIM( ADJUSTL( PwrUnits ) ), TRIM( ParamStr(ParTab) ), TRIM(Flt2LStr(TabAry(ITab)))  &
                                  , UnitsStr(ParTab), TRIM( ParamStr(ParRow) ), TRIM( ParamStr(ParCol) ), UnitsStr(ParCol)  &
                                  , UnitsStr(ParRow), ( ColAry(ICol), ICol=1,MaxCol )
         ENDIF

         DO IRow=1,MaxRow

            IF ( TabDel )  THEN
               WRITE (UnOu,FmtDaTab)   RowAry(IRow), ( ConvPwr*PwrAry(IRow,ICol,ITab), ICol=1,MaxCol )
            ELSE
               WRITE (UnOu,FmtDaSpc)   RowAry(IRow), ( ConvPwr*PwrAry(IRow,ICol,ITab), ICol=1,MaxCol )
           ENDIF

         ENDDO ! IRow

         WRITE (UnOu,'(A)')  ' '

      ENDIF


         ! Output Cp?

      IF ( OutCp )  THEN

         IF ( TabDel )  THEN
            WRITE (UnOu,FmtHdTab)  'Cp', '-', TRIM( ParamStr(ParTab) ), TRIM(Flt2LStr(TabAry(ITab))), UnitsStr(ParTab) &
                                  , TRIM( ParamStr(ParRow) ), TRIM( ParamStr(ParCol) ), UnitsStr(ParCol), UnitsStr(ParRow) &
                                  , ( ColAry(ICol), ICol=1,MaxCol )
         ELSE
            WRITE (UnOu,FmtHdSpc)  'Cp', '-', TRIM( ParamStr(ParTab) ), TRIM(Flt2LStr(TabAry(ITab))), UnitsStr(ParTab) &
                                  , TRIM( ParamStr(ParRow) ), TRIM( ParamStr(ParCol) ), UnitsStr(ParCol), UnitsStr(ParRow) &
                                  , ( ColAry(ICol), ICol=1,MaxCol )
         ENDIF

         DO IRow=1,MaxRow

            IF ( TabDel )  THEN
               WRITE (UnOu,FmtCpTab)   RowAry(IRow), ( CpAry(IRow,ICol,ITab), ICol=1,MaxCol )
            ELSE
               WRITE (UnOu,FmtCpSpc)   RowAry(IRow), ( CpAry(IRow,ICol,ITab), ICol=1,MaxCol )
            ENDIF

         ENDDO ! IRow

         WRITE (UnOu,'(A)')  ' '

      ENDIF


         ! Output torque?

      IF ( OutTrq )  THEN

         IF ( TabDel )  THEN
            WRITE (UnOu,FmtHdTab)  'Torque', TRIM( ADJUSTL( MomUnits ) ), TRIM( ParamStr(ParTab) ), TRIM(Flt2LStr(TabAry(ITab))) &
                                  , UnitsStr(ParTab), TRIM( ParamStr(ParRow) ), TRIM( ParamStr(ParCol) ), UnitsStr(ParCol) &
                                  , UnitsStr(ParRow), ( ColAry(ICol), ICol=1,MaxCol )
         ELSE
            WRITE (UnOu,FmtHdSpc)  'Torque', TRIM( ADJUSTL( MomUnits ) ), TRIM( ParamStr(ParTab) ), TRIM(Flt2LStr(TabAry(ITab))) &
                                  , UnitsStr(ParTab), TRIM( ParamStr(ParRow) ), TRIM( ParamStr(ParCol) ), UnitsStr(ParCol) &
                                  , UnitsStr(ParRow), ( ColAry(ICol), ICol=1,MaxCol )
         ENDIF

         DO IRow=1,MaxRow

            IF ( TabDel )  THEN
               WRITE (UnOu,FmtDaTab)   RowAry(IRow), ( ConvTrq*TrqAry(IRow,ICol,ITab), ICol=1,MaxCol )
            ELSE
               WRITE (UnOu,FmtDaSpc)   RowAry(IRow), ( ConvTrq*TrqAry(IRow,ICol,ITab), ICol=1,MaxCol )
            ENDIF

         ENDDO ! IRow

         WRITE (UnOu,'(A)')  ' '

      ENDIF


         ! Output flap bending moment?

      IF ( OutFlp )  THEN

         IF ( TabDel )  THEN
            WRITE (UnOu,FmtHdTab)  'Flap bending moment', TRIM( ADJUSTL( MomUnits ) ), TRIM( ParamStr(ParTab) ) &
                                  , TRIM(Flt2LStr(TabAry(ITab))), UnitsStr(ParTab), TRIM( ParamStr(ParRow) ) &
                                  , TRIM( ParamStr(ParCol) ), UnitsStr(ParCol), UnitsStr(ParRow), ( ColAry(ICol), ICol=1,MaxCol )
         ELSE
            WRITE (UnOu,FmtHdSpc)  'Flap bending moment', TRIM( ADJUSTL( MomUnits ) ), TRIM( ParamStr(ParTab) ) &
                                  , TRIM(Flt2LStr(TabAry(ITab))), UnitsStr(ParTab), TRIM( ParamStr(ParRow) ) &
                                  , TRIM( ParamStr(ParCol) ), UnitsStr(ParCol), UnitsStr(ParRow), ( ColAry(ICol), ICol=1,MaxCol )
         ENDIF

         DO IRow=1,MaxRow

            IF ( TabDel )  THEN
               WRITE (UnOu,FmtDaTab)   RowAry(IRow), ( ConvTrq*FlpAry(IRow,ICol,ITab), ICol=1,MaxCol )
            ELSE
               WRITE (UnOu,FmtDaSpc)   RowAry(IRow), ( ConvTrq*FlpAry(IRow,ICol,ITab), ICol=1,MaxCol )
            ENDIF

         ENDDO ! IRow

         WRITE (UnOu,'(A)')  ' '

      ENDIF


         ! Output thrust?

      IF ( OutThr )  THEN

         IF ( TabDel )  THEN
            WRITE (UnOu,FmtHdTab)  'Thrust', TRIM( ADJUSTL( FrcUnits ) ), TRIM(ADJUSTL(ParamStr(ParTab))) &
                                  , TRIM(Flt2LStr(TabAry(ITab))), UnitsStr(ParTab), TRIM(ADJUSTL(ParamStr(ParRow))) &
                                  , TRIM(ADJUSTL(ParamStr(ParCol))), UnitsStr(ParCol), UnitsStr(ParRow) &
                                  , (ColAry(ICol), ICol=1,MaxCol)
        ELSE
            WRITE (UnOu,FmtHdSpc)  'Thrust', TRIM( ADJUSTL( FrcUnits ) ), TRIM( ParamStr(ParTab) ), TRIM(Flt2LStr(TabAry(ITab))) &
                                  , UnitsStr(ParTab), TRIM( ParamStr(ParRow) ), TRIM( ParamStr(ParCol) ), UnitsStr(ParCol) &
                                  , UnitsStr(ParRow), ( ColAry(ICol), ICol=1,MaxCol )
         ENDIF

         DO IRow=1,MaxRow

            IF ( TabDel )  THEN
               WRITE (UnOu,FmtDaTab)   RowAry(IRow), ( ConvFrc*ThrAry(IRow,ICol,ITab), ICol=1,MaxCol )
            ELSE
               WRITE (UnOu,FmtDaSpc)   RowAry(IRow), ( ConvFrc*ThrAry(IRow,ICol,ITab), ICol=1,MaxCol )
            ENDIF

         ENDDO ! IRow

         WRITE (UnOu,'(A)')  ' '

      ENDIF

      IF ( ITab .NE. MaxTab )  WRITE (UnOu,FmtDbLin)

   ENDDO ! ITab


   RETURN
   END SUBROUTINE ParmAnal
!=======================================================================
   FUNCTION Prandtl( R1, R2, SinAFA )


      ! Prandtl tip loss model.  The formulation of the algorithm was
      ! changed by M. Buhl to make it slightly more efficient.


   USE                             WTP_Data
   USE                             Parameters

   IMPLICIT                        NONE


      ! Function declaration.

   REAL(ReKi)                   :: Prandtl


      ! Argument declarations.

   REAL(ReKi), INTENT(IN)       :: R1                                           ! 1.0 (TL) or non-dimensional local radius (HL).
   REAL(ReKi), INTENT(IN)       :: R2                                           ! Non-dimensional local radius (TL) or Non-dimensional hub radius (HL).
   REAL(ReKi), INTENT(IN)       :: SinAFA                                       ! Sine of the airflow angle.


      ! Local declarations.

   REAL(ReKi)                   :: Expon                                        ! Exponent (f) in the calculation.
   REAL(ReKi)                   :: F                                            ! e^(-f).



   Expon = ABS( 0.5*NumBlade*( R1 - R2 )/( R2*SinAFA ) )

   IF ( Expon .LT. 7.0 )  THEN
      F       = EXP( -Expon )
      Prandtl = TwoByPi*ATAN( SQRT( 1.0 - F*F )/F )                             ! ACOS(F)=ATAN(SQRT((1-F^2)/F))
   ELSE
      Prandtl = 1.0
   ENDIF


   RETURN
   END FUNCTION Prandtl ! ( R1, R2, SinAFA )
!=======================================================================
   SUBROUTINE RotAnal


      ! This routine performs the rotor analysis.


   USE                             Parameters
   USE                             ProgGen
   USE                             WTP_Data

   IMPLICIT                        NONE

!TempDCM Start 3.03.01a01-dcm, 27-Jul-2009
!New Variables for Convergence Summer 2009
   REAL(ReKi)                   :: AxIndPrevSeg                            !Value of AxInd from previous converged segment, if not converged, uses these default values.
   REAL(ReKi)                   :: TanIndPrevSeg                           !Value of TanInd from previous converged segment, if not converged, uses these default values.

!TempDCM End 3.03.01a01-dcm, 27-Jul-2009


      ! Local decarations.

   REAL(ReKi)                   :: AreaAnn                                         ! The (parital) annulus area of the current local segment and section.
   REAL(ReKi)                   :: AreaLoc                                         ! The blade area of the current local segment.
   REAL(ReKi)                   :: AvgInfl                                         ! The average induced velocity deficit across the rotor.
   REAL(ReKi)                   :: Azim                                            ! The azimuth of the current section in degrees.
   REAL(ReKi)                   :: AzimR                                           ! The azimuth of the current section in radians.
   REAL(ReKi)                   :: CircLcND                                        ! Nondimensional circumference of the rotor cone at the middle of current segment.
   REAL(ReKi)                   :: CosFSkew                                        ! The cosine of the free-flow skew angle.
   REAL(ReKi)                   :: FlpLoc                                          ! Flap bending moment produced by the current local segment.
   REAL(ReKi)                   :: HtLocND                                         ! The local nondimensional height of the current analysis point.
   REAL(ReKi)                   :: NB_QA                                           ! Number of blades divided by the dynamic pressure times the swept are.
   REAL(ReKi)                   :: PwrCLoc                                         ! Power coefficient of the current local segment.
   REAL(ReKi)                   :: PwrLoc                                          ! Power produced by the current local segment.
   REAL(ReKi)                   :: Q_HH                                            ! Dynamic pressure at hub height.
   REAL(ReKi)                   :: QA                                              ! HH dymanic pressure times swept area.
   REAL(ReKi)                   :: QACC_NS                                         ! Temporary variable (QLoc*AreaLoc*CosCone/NumSect).
   REAL(ReKi)                   :: QLoc                                            ! The local dynamic pressure.
   REAL(ReKi)                   :: Re                                              ! The local Reynolds number.
   REAL(ReKi)                   :: ShearLoc                                        ! The local wind-shear effect on wind speed.
   REAL(ReKi)                   :: SumInfl                                         ! The sum of the induced velocity deficits times the ????.
   REAL(ReKi)                   :: SinFSkew                                        ! The sine of the free-flow skew angle.
   REAL(ReKi)                   :: SinISkew                                        ! The sine of the induced skew angle.
   REAL(ReKi)                   :: SWconstO                                        ! The old value of SWconst.
   REAL(ReKi)                   :: ThrCLoc                                         ! Thrust coefficient of the current local segment.
   REAL(ReKi)                   :: ThrLoc                                          ! Thrust produced by the current local segment.
   REAL(ReKi)                   :: ThrLocLn                                        ! Thrust produced by the current local segment divided by the segment length.
   REAL(ReKi)                   :: TrqCLoc                                         ! Torque coefficient of the current local segment.
   REAL(ReKi)                   :: TrqLoc                                          ! Torque produced by the current local segment.
   REAL(ReKi)                   :: TrqLocLn                                        ! Torque produced by the current local segment divided by the segment length.
   REAL(ReKi)                   :: VInPlan2                                        ! Square of the in-plane component of wind speed in the rotor system.
   REAL(ReKi)                   :: VInPlane                                        ! In-plane component of wind speed in the rotor system.
   REAL(ReKi)                   :: VNyaw                                           ! Freestream wind speed reduced by the yaw angle.
   REAL(ReKi)                   :: VRotorX                                         ! X component of wind speed in the rotor system.
   REAL(ReKi)                   :: VRotorY                                         ! Y component of wind speed in the rotor system.
   REAL(ReKi)                   :: VRotorZ                                         ! Z component of wind speed in the rotor system.
   REAL(ReKi)                   :: VTot                                            ! Total, induced, relative wind speed for a local element.
   REAL(ReKi)                   :: VTot2                                           ! Square of the total, induced, relative wind speed for a local element.

   INTEGER                      :: ISect                                           ! Index for rotor sector.
   INTEGER                      :: ISeg                                            ! Index for blade segment.

   LOGICAL                      :: CaseConv                                        ! Flag that says if we converged all the induction loops for a case.
!Start of proposed change.  v3.02.00c-mlb 24-August-2006  M. Buhl
   LOGICAL                      :: FoundSol                                        ! Flag that says if we found a solution for all the induction loops for a case.
   LOGICAL                      :: FullConv                                        ! Flag that says if we fully converged all the induction loops for a case.
!End of proposed change.  v3.02.00c-mlb 24-August-2006  M. Buhl

   CHARACTER( 46), PARAMETER    :: FmtBEDs  = "(I4,2F10.1,4F9.3,F10.1,F8.1,2F7.3,3F7.3,3F8.2)"
   CHARACTER( 88), PARAMETER    :: FmtBEDt  = "(I3,'"//Tab//"',2(F6.1,'"//Tab//"'),4(F6.3,'"//Tab//"'),2(F7.2,'"//Tab &
                                              //"'),5(F6.3,'"//Tab//"'),F10.1,'"//Tab//"',F11.1,'"//Tab//"',F12.3)"
   CHARACTER(288), PARAMETER    :: FmtBEHs  = "(/,' Elem     Azim    LocVel       Re     Loss    AxInd   TanInd    AFAngle  Alpha" &
                                              //"    Cl     Cd   ThrCo  TrqCo  PwrCo  Thr/Len Trq/Len  Power',/" &
                                              //",'  (-)    (deg)    (',A,')   (mill)      (-)      (-)      (-)      (deg)" &
                                              //"   (deg)   (-)    (-)    (-)    (-)    (-) (',A,')   (',A,')   (kW)' )"
!Start of proposed change.  v3.03.02a-mlb, 17-Nov-2009  M. Buhl
!   CHARACTER(231), PARAMETER    :: FmtBEHt  = "(/,'Element"//Tab//"Azimuth"//Tab//"Loc Vel"//Tab//"Re"//Tab//"Loss"//Tab &
!                                              //"Axial Ind."//Tab//"Tang. Ind."//Tab//"Airflow Angle"//Tab//"AlfaD"//Tab//"Cl" &
!                                              //Tab//"Cd"//Tab//"Thrust Coef"//Tab//"Torque Coef"//Tab//"Power Coef"//Tab &
!                                              //"Thrust/Len"//Tab//"Torque/Len"//Tab//"Power',/,'(-)"//Tab//"(deg)"//Tab &
!                                              //"(',A,')"//Tab//"(-)"//Tab//"(-)"//Tab//"(deg)"//Tab//"(deg)"//Tab//"(-)"//Tab &
!                                              //"(-)"//Tab//"(-)"//Tab//"(-)"//Tab//"(-)"//Tab//"(',A,')"//Tab//"(',A,')"//Tab &
!                                              //"(kW)')"
   CHARACTER(246), PARAMETER    :: FmtBEHt  = "(/,'Element"//Tab//"Azimuth"//Tab//"Loc Vel"//Tab//"Re"//Tab//"Loss"//Tab &
                                              //"Axial Ind."//Tab//"Tang. Ind."//Tab//"Airflow Angle"//Tab//"AlfaD"//Tab//"Cl" &
                                              //Tab//"Cd"//Tab//"Thrust Coef"//Tab//"Torque Coef"//Tab//"Power Coef"//Tab &
                                              //"Thrust/Len"//Tab//"Torque/Len"//Tab//"Power',/,'(-)"//Tab//"(deg)"//Tab &
                                              //"(',A,')"//Tab//"(millions)"//Tab//"(-)"//Tab//"(-)"//Tab//"(-)"//Tab//"(deg)" &
                                              //Tab//"(deg)"//Tab//"(-)"//Tab//"(-)"//Tab//"(-)"//Tab//"(-)"//Tab//"(-)"//Tab &
                                              //"(',A,')"//Tab//"(',A,')"//Tab//"(kW)')"
!End of proposed change.  v3.03.02a-mlb, 17-Nov-2009  M. Buhl
!Start of proposed change.  v3.03.02a-mlb, 1-Dec-2009,  M. Buhl
!   CHARACTER( 92)    :: FmtRPT   = "(/'Blade-element data for Rotation Rate = ',A,' rpm, Blade Pitch = ',A,' deg, TSR = ',A,'.')"
!   CHARACTER(105)    :: FmtRPW   = "(/'Blade-element data for Rotation Rate = ',A,' rpm, Blade Pitch = ',A,' deg, Wind Speed = '" &
!                                 //",A,' ',A,'.')"
   CHARACTER( 92)    :: FmtRPTs   = "(/'Blade-element data for Rotation Rate = ',A,' rpm, Blade Pitch = ',A,' deg, TSR = ',A,'.')"
   CHARACTER( 99)    :: FmtRPTt   = "(/'Blade-element data for:'" &
                                  //",/,A,'"//Tab//"rpm Rotation Rate'" &
                                  //",/,A,'"//Tab//"deg Blade Pitch'" &
                                  //",/,A,'"//Tab//"Tip-Speed Ratio')"
   CHARACTER(105)    :: FmtRPWs   = "(/'Blade-element data for Rotation Rate = ',A,' rpm, Blade Pitch = ',A,' deg, Wind Speed = '" &
                                  //",A,' ',A,'.')"
   CHARACTER(100)    :: FmtRPWt   = "(/'Blade-element data for:'" &
                                  //",/,A,'"//Tab//"rpm Rotation Rate'" &
                                  //",/,A,'"//Tab//"deg Blade Pitch'" &
                                  //",/,A,'"//Tab//"',A,' Wind Speed')"
!End of proposed change.  v3.03.02a-mlb, 1-Dec-2009,  M. Buhl


      ! Initialize the accumulators.

   TrqC    = 0.0
   PwrC    = 0.0
   Thrust  = 0.0
   FlapMom = 0.0
   Torque  = 0.0
   Power   = 0.0

!Start of proposed change.  v3.02.00c-mlb 24-August-2006  M. Buhl

      ! Increment the number of cases being run.

   NCases = NCases + 1

!End of proposed change.  v3.02.00c-mlb 24-August-2006  M. Buhl

      ! Hub-height dynamic pressure and related constants.

   Q_HH  = HalfRho*VelHH*VelHH
   QA    = Q_HH*SwptArea
   NB_QA = NumBlade/QA


      ! Write out header for the blade element file if requested.

   IF ( WriteBED )  THEN

!Start of proposed change.  v3.03.02a-mlb, 1-Dec-2009,  M. Buhl
!      IF ( InputTSR )  THEN
!         IF ( WriteBED )  WRITE (UnBE,FmtRPT)  TRIM( Flt2LStr( OmgRPM ) ), TRIM( Flt2LStr( PitDeg ) ), TRIM( Flt2LStr( Spd ) )
!      ELSE
!         IF ( WriteBED )  WRITE (UnBE,FmtRPW)  TRIM( Flt2LStr( OmgRPM ) ), TRIM( Flt2LStr( PitDeg ) ), TRIM( Flt2LStr( Spd ) ) &
!                          , SpdUnits
!      ENDIF
!
!      IF ( TabDel )  THEN
!         WRITE (UnBE,FmtBEHt)  TRIM( ADJUSTL( SpdUnits ) ), TRIM( ADJUSTL( FpLUnits ) ), TRIM( ADJUSTL( MpLUnits ) )
!      ELSE
!         WRITE (UnBE,FmtBEHs)  SpdUnits, FpLUnits, MpLUnits
!      ENDIF
      IF ( TabDel )  THEN

         IF ( InputTSR )  THEN
            IF ( WriteBED )  WRITE (UnBE,FmtRPTt)  TRIM( Flt2LStr( OmgRPM ) ), TRIM( Flt2LStr( PitDeg ) ), TRIM( Flt2LStr( Spd ) )
         ELSE
            IF ( WriteBED )  WRITE (UnBE,FmtRPWt)  TRIM( Flt2LStr( OmgRPM ) ), TRIM( Flt2LStr( PitDeg ) ), TRIM( Flt2LStr( Spd ) ) &
                             , SpdUnits
         ENDIF

         WRITE (UnBE,FmtBEHt)  TRIM( ADJUSTL( SpdUnits ) ), TRIM( ADJUSTL( FpLUnits ) ), TRIM( ADJUSTL( MpLUnits ) )

      ELSE

         IF ( InputTSR )  THEN
            IF ( WriteBED )  WRITE (UnBE,FmtRPTs)  TRIM( Flt2LStr( OmgRPM ) ), TRIM( Flt2LStr( PitDeg ) ), TRIM( Flt2LStr( Spd ) )
         ELSE
            IF ( WriteBED )  WRITE (UnBE,FmtRPWs)  TRIM( Flt2LStr( OmgRPM ) ), TRIM( Flt2LStr( PitDeg ) ), TRIM( Flt2LStr( Spd ) ) &
                             , SpdUnits
         ENDIF

         WRITE (UnBE,FmtBEHs)  SpdUnits, FpLUnits, MpLUnits

      ENDIF
!End of proposed change.  v3.03.02a-mlb, 1-Dec-2009,  M. Buhl

   ENDIF


!Start of proposed change.  v3.02.00c-mlb 24-August-2006  M. Buhl
!v3.02.00c-mlb      ! Initialize the case-convergence flag.  It will be set to false if any induction loop fails.
!v3.02.00c-mlb
!v3.02.00c-mlb   CaseConv = .TRUE.
      ! Initialize the case-convergence and full-convergenge flag.  They will be set to false if any induction loop fails.

   CaseConv = .TRUE.
   FoundSol = .TRUE.
   FullConv = .TRUE.
!End of proposed change.  v3.02.00c-mlb 24-August-2006  M. Buhl


      ! Initialize AeroDyn's skewed-wake correction.  If we are not doing skewed wake, set the skewed-wake
      ! convergence flag to true so we output the element data, if requested.

   IF ( SkewWake )  THEN

      SumInfl  = 0.0
      VNyaw    = VelHH*CosYaw
      VRotorX  = VNyaw*CosTilt
      VRotorY  = VelHH*SinYaw
      VRotorZ  = -VNyaw*SinTilt
      VInPlan2 = VRotorY**2 + VRotorZ**2
      VInPlane = SQRT( VInPlan2 )

      IF ( VInPlane > 0.001 )  THEN
         SinFSkew =  VRotorY/VInPlane
         CosFSkew = -VRotorZ/VInPlane
         SinISkew = ABS( VRotorX )/VelHH
         SWconst  = ( 15*Pi/64 )*SQRT( ( 1.0 - SinISkew )/( 1.0 + SinISkew ) )
         SWconv   = .FALSE.
         DoSkew   = .TRUE.
      ELSE
         SWconst  = 0.0
         SWconv   = .TRUE.
         DoSkew   = .FALSE.
      ENDIF

   ENDIF


      ! Loop indefinitely until we converge the skewed-wake algorithm.

   SkewedWakeLoop: DO


         ! Start of Segment Loop.  Work our way out the blade.

      SegmentLoop: DO ISeg=1,NumSeg

         IF ( WriteBED .AND. SWconv .AND. PrntElem(ISeg) .AND. ( NumSect .GT. 1 ) )  WRITE(UnBE,'()')


         AreaLoc  = DelRLoc(ISeg)*Chord(ISeg)*RotorRad                      ! Number of blades accounted for at the end.
         AreaAnn  = 2*Pi*RLoc(ISeg)*CosCone*DelRLoc(ISeg)/NumSect           ! Area of annulus in local sector over which forces are averaged
         CircLcND = 2.0*Pi*RLocND(ISeg)*CosCone
         Solidity = NumBlade*Chord(ISeg)/CircLcND


            ! Calculate the local tangential velocity (body frame).

         VBodTang = TipSpeed*RLocND(ISeg)


            ! Calculate the incidence angle between the chord line and the
            ! cone of rotation.

         IncidAng = Twist(ISeg) + Pitch

!Start of proposed change.  v3.02.00a-mlb 04-August-2006  M. Buhl

            ! Initlialize the inductions.

         AxInd  = 0.0
         TanInd = 0.0

!End of proposed change.  v3.02.00a-mlb 04-August-2006  M. Buhl

            !  ------------------  Beginning of section loop.  -----------------------


            ! Work our way around the cone of rotation.

         SectionLoop: DO ISect=1,NumSect


               ! Calculate the blade azimuth and related constants.

            Azim    = ( 360*ISect - 180.0 )/NumSect
            AzimR   = Azim*D2R
            CosAzim = COS( AzimR )
            SinAzim = SIN( AzimR )


               ! Calculate the local shear effect and the local wind speed.

            HtLocND  = RLocND(ISeg)*CosAzim*CosCone*CosTilt
!Start of proposed change.  v3.04.00c-mlb, 12-Jan-2011,  M. Buhl
            ShearLoc = ( 1.0 + HtLocND/HubHtND )**ShearExp
!End of proposed change.  v3.04.00c-mlb, 12-Jan-2011,  M. Buhl
            VWndGnd  = VelHH*ShearLoc


               ! Get the undisturbed normal and tangential total wind speeds.

            CALL Getvel ( VTotNorm )


               ! Calculate the local speed ratio.

            SpdRatio = VBodTang/VTotNorm


               ! Calculate the skewed-wake correction for axial induction.

            IF ( DoSkew )  THEN
               SWcorr = 1.0 - 2.0*RLocND(ISeg)*SWconst*( SinFSkew*SinAzim + CosFSkew*CosAzim )  ! WT_Perf uses different definition of Azim
            ELSE
               SWcorr = 1.0
            ENDIF


               ! Calculate the induction factors.  The Prop version is like the old
               ! PROP-PC and WT_Perf.  The PropX version is like PROPX and AeroDyn.

!print *, "iseg, ISect=", iseg, ISect

!Temp Start proposed changes. DCM 6-22-09 3.02.00k04-dcm, 22-Jun-2009
!Segment,Sector lines for plotting output
!IF (.false. .and. WriteDebug) THEN
!write (UnDB,'(1X,2I5)') Iseg, Isect
!ENDIF
!Temp End proposed changes. DCM 6-22-09 3.02.00k04-dcm, 22-Jun-2009

!Start of proposed change.  v3.02.00a-mlb 03-August-2006  M. Buhl
!v3.02.00a-mlb            IF ( IndProp )  THEN
!v3.02.00a-mlb               CALL Ind_Prop  ( ISeg, Converge )
!v3.02.00a-mlb            ELSE
!v3.02.00a-mlb               CALL Ind_PropX ( ISeg, Converge )
!v3.02.00a-mlb            ENDIF
            SELECT CASE ( IndType )

               CASE ( .False. )


                     ! No induction model.

                  AxInd  = 0.0
                  TanInd = 0.0
!Start of proposed change.  v3.02.00h-mlb 11-Sept-2006  M. Buhl

                  CALL GetAero ( ISeg, AxInd, TanInd )

!End of proposed change.  v3.02.00h-mlb 11-Sept-2006  M. Buhl
!Start of proposed change.  v3.02.00c-mlb 24-August-2006  M. Buhl
                  Solution = .TRUE.
!End of proposed change.  v3.02.00c-mlb 24-August-2006  M. Buhl
                  Converge = .TRUE.

               CASE ( .True. )


                     ! Use BEM induction model.

!Start of proposed change.  v3.02.00c-mlb 24-August-2006  M. Buhl
!v3.02.00c-mlb                  CALL Ind_Prop  ( ISeg, Converge )
!Start of proposed change.  v3.02.00g-mlb 7-September-2006  M. Buhl
!v3.02.00g                  CALL Ind_Prop  ( ISeg, Solution, Converge )
                  CALL InductBEM  ( ISeg, ISect,Solution, Converge, AxIndPrevSeg, TanIndPrevSeg )

!TempDCM Start 3.03.01a05-dcm, 29-Jul-2009
!TempDCM Start 3.02.00k02-dcm, 18-Jun-2009
!Cycle
!TempDCM End 3.02.00k02-dcm, 18-Jun-2009
!TempDCM End3.03.01a05-dcm, 29-Jul-2009

!End of proposed change.  v3.02.00g-mlb 7-September-2006  M. Buhl

!#IFDEF debug2
!print *, "RotAnal-1: Solution, Converge, OutNines = ", Solution, Converge, OutNines
!#ENDIF

!End of proposed change.  v3.02.00c-mlb 24-August-2006  M. Buhl

!Start of proposed change.  v3.02.00b-mlb 22-August-2006  M. Buhl
!v3.02.00b               CASE ( 2 )
!v3.02.00b
!v3.02.00b                  CALL Ind_PropX ( ISeg, Converge )
!End of proposed change.  v3.02.00b-mlb 22-August-2006  M. Buhl

            END SELECT
!End of proposed change.  v3.02.00a-mlb 03-August-2006  M. Buhl


!Start of proposed change.  v3.02.00c-mlb 24-August-2006  M. Buhl
            IF ( .NOT. Solution )  FoundSol = .FALSE.
            IF ( .NOT. Converge )  FullConv = .FALSE.

!v3.02.00c-mlb            IF ( .NOT. Converge )  THEN
            IF ( ( .NOT. Solution ) .OR. ( OutNines .AND. .NOT. Converge ) )  THEN

!#IFDEF debug2
!print *, "RotAnal-2: Solution, Converge, OutNines = ", Solution, Converge, OutNines
!#ENDIF

!v3.02.00c-mlb                  ! Give up on this case because we haven't been able to converge.
                  ! Give up on this case because we haven't been able to find a solution or reach a required convergence.
!End of proposed change.  v3.02.00c-mlb 24-August-2006  M. Buhl

               CaseConv = .FALSE.
!Start of proposed change.  v3.02.00c-mlb 24-August-2006  M. Buhl
!v3.02.00c-mlb               NmItFail = NmItFail + 1
!End of proposed change.  v3.02.00c-mlb 24-August-2006  M. Buhl

!Temp Start proposed changes. DCM 6-11-09  Version = 'v3.01.02d05-dcm, 09-Jun-2009'
IF (WriteDebug) THEN !DCM 6-8-09
write (*,'(A,7F13.6,I6,I6)') "Bad TSR,OmgRPM,PitDeg,AxInd,TanInd, AlfaD,ClLoc,iseg, ISect:",TSR,OmgRPM,PitDeg,AxInd,TanInd,AlfaD,ClLoc,iseg, ISect
write (UnDB,'(1X,7F16.9,I6,I6)') TSR,OmgRPM,PitDeg,AxInd,TanInd,AlfaD,ClLoc,iseg, ISect
ENDIF
!Temp End proposed changes. DCM 6-11-09    Version = 'v3.01.02d05-dcm, 09-Jun-2009'



               IF ( WriteBED .AND. PrntElem(ISeg) )  THEN                                 ! We didn't converge.


                     ! Output dummy blade element data if requested.

                  IF ( TabDel )  THEN
                     WRITE (UnBE,FmtBEDt) ISeg, Azim, 9999.9, 99.999, 99.999, 99.999, 99.999, 9999.9, 9999.9, 99.999, &
                                                              99.999, 99.999, 99.999, 99.999, 999.9, 999.9, 999.999
                  ELSE
                     WRITE (UnBE,FmtBEDs) ISeg, Azim, 9999.9, 99.999, 99.999, 99.999, 99.999, 9999.9, 9999.9, 99.999, &
                                                              99.999, 99.999, 99.999, 99.999, 999.9, 999.9, 999.999
                  ENDIF

               ENDIF


                  ! Use dummy values because we didn't converge.

               Power   = BadPwr/ConvPwr
               PwrC    = BadCp
               Torque  = Power
               FlapMom = Power
               Thrust  = Power

               IF ( .NOT. FoundSol )  NmCaseFail = NmCaseFail + 1
               IF ( .NOT. FullConv )  NmCaseNC   = NmCaseNC   + 1

               !RETURN

            ELSE ! ( ( .NOT. Solution ) .OR. ( OutNines .AND. .NOT. Converge ) )


                   ! Calculate results only after we've converged the skewed-wake correction.

               IF ( SWconv )  THEN


                    ! Update the velocities to use the last values of the inductions.

                  VIndTang = VTotTang*( 1.0 + TanInd )
                  VIndNorm = VTotNorm*( 1.0 -  AxInd )
                  VIndNorm = VTotNorm*( 1.0 - AxInd*SWcorr )


                     ! Local dynamic pressure, section area.

                  VTot2   = VIndTang*VIndTang + VIndNorm*VIndNorm
                  QLoc    = HalfRho*VTot2
                  QACC_NS = QLoc*AreaLoc*CosCone/NumSect


                      ! Local blade element data.

                  ThrLoc = QACC_NS*( ClLoc*CosAF + CdLoc*SinAF )
                  TrqLoc = QACC_NS*( ClLoc*SinAF - CdLoc*CosAF )*RLoc(ISeg)
                  PwrLoc = TrqLoc*Omega
                  FlpLoc = ThrLoc*( RLoc(ISeg) - HubRad )


                     ! Increment rotor dimensional data for single blade.

                  Thrust  = Thrust  + ThrLoc
                  Torque  = Torque  + TrqLoc
                  Power   = Power   + PwrLoc
                  FlapMom = FlapMom + FlpLoc


                    ! Output blade element data if requested.

                  IF ( WriteBED .AND. PrntElem(ISeg) )  THEN


                        ! Convert local thrust and torque to per unit length.

                     ThrLocLn = ThrLoc/DelRLoc(ISeg)
                     TrqLocLn = TrqLoc/DelRLoc(ISeg)


                        ! Local blade element coefficients.

                     ThrCLoc = ThrLoc*NumBlade/(Q_HH*AreaAnn)
                     TrqCLoc = TrqLoc*NumBlade/(Q_HH*AreaAnn*RLoc(ISeg)*CosCone)
                     PwrCLoc = PwrLoc*NumBlade/(Q_HH*VelHH*AreaAnn)             ! Ploc/Pwind


                        ! Calculate the Reynolds number in millions.

                     VTot = SQRT( VTot2 )
                     Re   = 1.0e-6*VTot*Chord(ISeg)*RotorRad/KinVisc


                        ! Output the blade-element data.

                     IF ( TabDel )  THEN
                        WRITE (UnBE,FmtBEDt) ISeg, Azim, VTot, Re, Loss, AxInd, TanInd, AFangD, AlfaD, ClLoc, CdLoc &
                                           , ThrCLoc, TrqCLoc,  PwrCLoc, ThrLocLn, TrqLocLn, NumBlade*PwrLoc*ConvPwr
                     ELSE
                        WRITE (UnBE,FmtBEDs) ISeg, Azim, VTot, Re, Loss, AxInd, TanInd, AFangD, AlfaD, ClLoc, CdLoc &
                                           , ThrCLoc, TrqCLoc,  PwrCLoc, ThrLocLn, TrqLocLn, NumBlade*PwrLoc*ConvPwr
                     ENDIF

                  ENDIF ! WriteBED .AND. PrntElem(ISeg)

               ELSEIF ( DoSkew )  THEN


                     ! Increment the sum of the area-weighted inflow values for AeroDyn's skewed-wake correction.

                  SumInfl = SumInfl + VTotNorm*AxInd*RLoc(ISeg)*DelRLoc(ISeg)

               ENDIF ! SWconv

            ENDIF ! ( ( .NOT. Solution ) .OR. ( OutNines .AND. .NOT. Converge ) )

         ENDDO SectionLoop ! ISect

      ENDDO SegmentLoop ! ISeg


         ! Exit skewed-wake loop if we converged the last pass.

      IF ( SWconv )  EXIT SkewedWakeLoop


         ! Update skewed-wake parameters.

      IF ( DoSkew )  THEN

         AvgInfl  = 2.0*SumInfl/( NumSect*RotorRad**2 )
         SumInfl  = 0.0
         SinISkew = ABS( VRotorX  - AvgInfl )/SQRT( VInPlan2 + ( VRotorX  - AvgInfl )**2 )
         SWconstO = SWconst
         SWconst  = ( 15*Pi/64 )*SQRT( ( 1.0 - SinISkew )/( 1.0 + SinISkew ) )

         IF ( ABS( SWconst - SWconstO ) > SWTol )  THEN
            SWconv = .FALSE.
         ELSE
            SWconv = .TRUE.
         ENDIF

      ENDIF

   ENDDO SkewedWakeLoop

!Start of proposed change.  v3.02.00c-mlb 24-August-2006  M. Buhl
!#IFDEF debug2
!print *, "RotAnal-3: NmCaseFail, NmCaseNC", NmCaseFail, NmCaseNC
!#ENDIF
   IF ( .NOT. FoundSol )  NmCaseFail = NmCaseFail + 1
   IF ( .NOT. FullConv )  NmCaseNC   = NmCaseNC   + 1
!#IFDEF debug2
!print *, "RotAnal-4: NmCaseFail, NmCaseNC", NmCaseFail, NmCaseNC
!#ENDIF

!End of proposed change.  v3.02.00c-mlb 24-August-2006  M. Buhl
   IF ( CaseConv )  THEN


          ! Calculate rotor nondimensional data.

      TrqC = Torque*NB_QA*SwpRadIn
      PwrC = TrqC*TSR


         ! Calculate rotor dimensional data for all blades.

      Thrust = Thrust*NumBlade
      Torque = Torque*NumBlade
      Power  = Power *NumBlade

   ENDIF ! CaseConv

!#IFDEF debug2
!print *, "RotAnal-5: NmCaseFail, NmCaseNC", NmCaseFail, NmCaseNC
!#ENDIF

   RETURN
   END SUBROUTINE RotAnal ! ( )
!=======================================================================
   SUBROUTINE SetConv


      ! This routine sets up conversion factors.


   USE                             Parameters
   USE                             ProgGen
   USE                             WTP_Data

   IMPLICIT                        NONE


      ! Local decarations.

   CHARACTER(3)                 :: SpdUns                                       ! Temporary variable to hold the speed units in uppercase.



   IF ( InputTSR )  THEN

      ConvFact = 1.0

   ELSE

      SpdUns = SpdUnits

      CALL Conv2UC ( SpdUns )

      SELECT CASE ( SpdUns )

         CASE ( 'FPS' )            ! Using speed in ft/s.

            IF ( Metric )  THEN
               ConvFact    = 1.0/M2Ft
            ELSE
               ConvFact    = 1.0
            ENDIF

            UnitsStr(3) = 'fps'

         CASE ( 'MPS' )            ! Using speed in m/s.

            IF ( Metric )  THEN
               ConvFact    = 1.0
            ELSE
               ConvFact    = M2Ft
            ENDIF

            UnitsStr(3) = 'mps'

         CASE ( 'MPH' )            ! Using speed in mph.

            ConvFact    = 22.0/15.0
            IF ( Metric )  THEN
               ConvFact    = 22.0/( 15.0*M2Ft )
            ELSE
               ConvFact    = 22.0/15.0
            ENDIF

            UnitsStr(3) = 'mph'

         CASE DEFAULT

            CALL Abort ( ' Your speed units must be either "mps", "fps", or "mph".' )

      END SELECT

   ENDIF


      ! Set up conversion factors and SpdUnits strings.

   IF ( KFact )  THEN
      IF ( Metric )  THEN
         ConvFrc  = 0.001
         ConvPwr  = 0.001
         ConvTrq  = 0.001
         FrcUnits = '  kN'
         FpLUnits = ' N/m'
         LenUnits = 'm'
         MomUnits = '   kN-m'
         MpLUnits = 'N'
         PwrUnits = 'kW'
         SpdUnits = ' m/s'
      ELSE
         ConvFrc  = 0.001
         ConvPwr  = 0.001*FP2Nm
         ConvTrq  = 0.001
         FrcUnits = 'klbf'
         FpLUnits = 'lbf/ft'
         LenUnits = 'ft'
         MomUnits = 'kft-lbf'
         MpLUnits = 'lbf'
         PwrUnits = 'kW'
         SpdUnits = 'ft/s'
      ENDIF
   ELSE ! ( not KFact )
      IF ( Metric )  THEN
         ConvFrc  = 1.0
         ConvPwr  = 1.0
         ConvTrq  = 1.0
         FrcUnits = '   N'
         FpLUnits = ' N/m'
         LenUnits = 'm'
         MomUnits = '    N-m'
         MpLUnits = 'N'
         PwrUnits = ' W'
         SpdUnits = ' m/s'
      ELSE
         ConvFrc  = 1.0
         ConvPwr  = FP2Nm
         ConvTrq  = 1.0
         FrcUnits = ' lbf'
         FpLUnits = 'lbf/ft'
         LenUnits = 'ft'
         MomUnits = ' ft-lbf'
         MpLUnits = 'lbf'
         PwrUnits = ' W'
         SpdUnits = 'ft/s'
      ENDIF
   ENDIF


   RETURN
   END SUBROUTINE SetConv
!=======================================================================

END MODULE WTP_Subs
!=======================================================================
SUBROUTINE SetProg


   ! This routine sets the version number.  By doing it this way instead
   !   of the old way of initializing it in a module, we will no longer
   !   have to recompile everything every time we change versions.


USE                                NWTC_Library


IMPLICIT                           NONE


   ! Local Variables:

CHARACTER(28)                   :: Version = 'v3.04.00c-mlb, 13-Jan-2011'             ! String containing the current version.



ProgName = 'WT_Perf'

IF ( ReKi == 4 )  THEN     ! Single precision
   ProgVer = ' ('//TRIM( Version )//')'
ELSEIF ( ReKi == 8 )  THEN     ! Double precision
   ProgVer = ' ('//TRIM( Version )//', compiled using double precision)'
ELSE                       ! Unknown precision - it should be impossible to compile using a KIND that is not 4 or 8, but I'll put this check here just in case.
   ProgVer = ' ('//TRIM( Version )//', compiled using '//TRIM( Int2LStr( ReKi ) )//'-byte precision)'
ENDIF


RETURN
END SUBROUTINE SetProg
PROGRAM WT_Perf


   ! This program is a variable speed version of a much-modified
   ! version of the original PROP code by Aerovironment Inc.


USE                                NWTC_Library
USE                                Parameters
USE                                ProgGen
USE                                WTP_Data
USE                                WTP_Subs

IMPLICIT                           NONE


   ! Local declarations.

REAL(ReKi)                      :: TimerBeg                                        ! Timer start time.
REAL(ReKi)                      :: TimerEnd                                        ! Timer end time.



   ! Get date and time.

DateNow = CurDate()
TimeNow = CurTime()


!Start of proposed change.  v3.04.00b-mlb, 05-Nov-2010,  M. Buhl
!mlb   ! Reopen the console for FORTRAN carriage control.
   ! Initialize the NWTC Library.

!mlbCALL OpenCon
CALL NWTC_Init
!End of proposed change.  v3.04.00b-mlb, 05-Nov-2010,  M. Buhl


   ! Print out program name, version, and date.

CALL SetProg
CALL DispNVD


   ! Compute Pi-based constants.

CALL PiConsts


   ! Initialize file system.

CALL IOInit


   ! Get input data.

CALL GetData


   ! If appropriate, open the blade-element data file and generatte the header.

IF ( WriteBED )  THEN

   CALL OpenFOutFile ( UnBE, TRIM( RootName )//'.bed' )

   WRITE (UnBE,'(A)')  'Blade-element data generated by '//TRIM( ProgName )//TRIM( ProgVer )//' for input file "' &
                       //TRIM( InpFile )//'".'
   WRITE (UnBE,'(A)')  'Generated on '//TRIM( DateNow )//' at '//TRIM( TimeNow )//'.'
   WRITE (UnBE,'(A)')  'Input file title:'
   WRITE (UnBE,'(A)')  '  '//TRIM( RunTitle )
   WRITE (UnBE,'(A)')  ' '

ENDIF

!Temp Start proposed changes. DCM 6-22-09  Version = 'v3.02.00k04-dcm, 22-Jun-2009'
IF (WriteDebug)  THEN

   CALL OpenFOutFile ( UnDB, TRIM( RootName )//'.dbg' )

   WRITE (UnDB,'(A)')  'Non-converged cases data generated by '//TRIM( ProgName )//TRIM( ProgVer )//' for input file "' &
                       //TRIM( InpFile )//'".'
   WRITE (UnDB,'(A)')  'Generated on '//TRIM( DateNow )//' at '//TRIM( TimeNow )//'.'
   WRITE (UnDB,'(A)')  'Input file title:'
   WRITE (UnDB,'(A)')  '  '//TRIM( RunTitle )
   WRITE (UnDB,'(A)')  ' '
   WRITE (UnDB,'(A)')  '    TSR,OmgRPM,PitDeg,AxInd,TanInd,AlfaD,ClLoc,iseg, ISect'
   WRITE (UnDB,'(A)')  ' '

ENDIF
!Temp Start proposed changes. DCM 6-22-09  Version = 'v3.02.00k04-dcm, 22-Jun-2009'



   ! Calculate some constants.

SweptRad = RotorRad*CosCone
SwpRadIn = 1.0/SweptRad
SwptArea = Pi*SweptRad*SweptRad
HalfRho  = 0.5*AirDens

CALL WrScr ( ' ' )


   ! Start timer.

 CALL CPU_TIME ( TimerBeg )


   ! What type of analysis do we do?

IF ( NumCases == 0 )  THEN            ! Do old-style parametric analysis if number of combined cases is zero.

   CALL ParmAnal

ELSE                                  ! Do new-style combined cases.

   CALL CombCase

ENDIF


   ! Stop timer

 CALL CPU_TIME ( TimerEnd )


   ! Close the output files.

CLOSE ( UnOu )

IF ( WriteBED )  CLOSE ( UnBE )
IF ( WriteDebug )  CLOSE ( UnDB )


   ! Display timer if non-zero.

IF ( TimerEnd > 0.0 )  CALL WrScr1 ( ' Main loop used '//TRIM( Flt2LStr( TimerEnd ) )//' seconds of User CPU time.' )


!Start of proposed change.  v3.02.00c-mlb 24-August-2006  M. Buhl
!v3.02.00c-mlb   ! Display the number of iteration failures.
!v3.02.00c-mlb
!v3.02.00c-mlbIF ( NmItFail == 1 )  THEN
!v3.02.00c-mlb   CALL WrScr ( ' ' )
!v3.02.00c-mlb   CALL Abort ( 'There was one case that failed to converge.' )
!v3.02.00c-mlbELSEIF ( NmItFail > 1 )  THEN
!v3.02.00c-mlb   CALL WrScr ( ' ' )
!v3.02.00c-mlb   CALL Abort ( 'There were '//TRIM( Int2LStr( NmItFail ) )//' cases that failed to converge.' )
!v3.02.00c-mlbENDIF
   ! Display the number of solution problems.

IF ( NmCaseNC == 1 )  THEN

   IF ( OutNines)  THEN
      IF ( NCases == 1 )  THEN
         CALL WrScr1 ( ' WARNING: The case analyzed did not fully meet the convergence criterion.  WT_Perf output nines'// &
                       ' instead of approximate values.' )
      ELSE
         CALL WrScr1 ( ' WARNING: Of the '//TRIM( Int2LStr( NCases ) )//  &
                       ' cases analyzed, there was one case where the solution did not fully meet the convergence criterion.  '// &
                       '  WT_Perf output nines for that case instead of approximate values.' )
      ENDIF
   ELSE
      IF ( NCases == 1 )  THEN
         CALL WrScr1 ( ' WARNING: The case analyzed did not fully meet the convergence criterion.  However, WT_Perf used the'// &
                       ' final guess in subsequent calculations.' )
      ELSE
         CALL WrScr1 ( ' WARNING: Of the '//TRIM( Int2LStr( NCases ) )//  &
                       ' cases analyzed, there was one case where the solution did not fully meet the convergence criterion.'// &
                       '  However, WT_Perf used the final guess in subsequent calculations.' )
      ENDIF
   ENDIF

   IF ( Beep .AND. NmCaseFail == 0 )  CALL UsrAlarm

ELSEIF ( NmCaseNC > 1 )  THEN

   IF ( OutNines)  THEN
      CALL WrScr1 ( ' WARNING: Of the '//TRIM( Int2LStr( NCases ) )//' cases analyzed, there were '//TRIM( Int2LStr( NmCaseNC ) )//  &
                    ' cases where the solution did not fully meet the convergence criterion.  WT_Perf output nines'// &
                    ' instead of using the final guesses in subsequent calculations.' )
   ELSE
      CALL WrScr1 ( ' WARNING: Of the '//TRIM( Int2LStr( NCases ) )//' cases analyzed, there were '//TRIM( Int2LStr( NmCaseNC ) )//  &
                    ' cases where the solution did not fully meet the convergence criterion.  However, WT_Perf used the final'// &
                    ' guesses in subsequent calculations.' )
   ENDIF

   IF ( Beep .AND. NmCaseFail == 0 )  CALL UsrAlarm

ENDIF

IF ( NmCaseFail == 1 )  THEN

   CALL WrScr ( ' ' )

   IF ( NCases == 1 )  THEN
      CALL Abort ( ' The case analyzed failed to find a solution.' )
   ELSE
      CALL Abort ( ' Of the '//TRIM( Int2LStr( NCases ) )//' cases analyzed, there was one case that failed to find a solution.' )
   ENDIF

ELSEIF ( NmCaseFail > 1 )  THEN

   CALL WrScr ( ' ' )
   CALL Abort ( ' Of the '//TRIM( Int2LStr( NCases ) )//' cases analyzed, there were '//TRIM( Int2LStr( NmCaseFail ) )// &
                ' cases that failed to find a solution.' )

ENDIF
!End of proposed change.  v3.02.00c-mlb 24-August-2006  M. Buhl


   ! Normal termination.

CALL NormStop


STOP
END PROGRAM WT_Perf
