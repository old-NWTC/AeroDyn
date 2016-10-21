!**********************************************************************************************************************************
! LICENSING
! Copyright (C) 2015-2016  National Renewable Energy Laboratory
!
!    This file is part of Noise.
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
module Noise_Driver_Subs

   use NWTC_Library
   use Noise_Driver_Types   
   use Noise
    
   implicit none   
   
   TYPE(ProgDesc), PARAMETER   :: version   = ProgDesc( 'Noise_driver', 'v1.00.00a', '21-June-2016' )  ! The version number of this program.
                                                    
   contains

!----------------------------------------------------------------------------------------------------------------------------------
subroutine Dvr_Init(DvrData,errStat,errMsg )

   type(Dvr_SimData),            intent(  out) :: DvrData       ! driver data
   integer(IntKi)              , intent(  out) :: errStat       ! Status of error message
   character(*)                , intent(  out) :: errMsg        ! Error message if ErrStat /= ErrID_None

      ! local variables
   integer(IntKi)                              :: errStat2      ! local status of error message
   character(ErrMsgLen)                        :: errMsg2       ! local error message if ErrStat /= ErrID_None
   character(*), parameter                     :: RoutineName = 'Dvr_Init'

   CHARACTER(1000)                             :: inputFile     ! String to hold the file name.
   real(reKi)                                   :: position(3)       ! node reference position
   real(reKi)                                   :: positionL(3)      ! node local position
   real(R8Ki)                                   :: theta(3)          ! Euler angles
   real(R8Ki)                                   :: orientation(3,3)  ! node reference orientation
   real(R8Ki)                                   :: orientationL(3,3) ! node local orientation
   
   integer(intKi)                               :: j                 ! counter for nodes
   integer(intKi)                               :: k                 ! counter for blades
   

   ErrStat = ErrID_None
   ErrMsg  = ""

   DvrData%OutFileData%unOutFile   = -1
   
      ! Initialize the library which handle file echos and WrScr, for example
   call NWTC_Init()
      
      ! Display the copyright notice
   CALL DispCopyrightLicense( version )
   
      ! Tell our users what they're running
   CALL WrScr( ' Running '//GetNVD( version )//NewLine//' linked with '//TRIM( GetNVD( NWTC_Ver ))//NewLine )

   InputFile = ""  ! initialize to empty string to make sure it's input from the command line
   CALL CheckArgs( InputFile, ErrStat2 )
   IF (LEN_TRIM(InputFile) == 0) THEN ! no input file was specified
      call SetErrStat(ErrID_Fatal, 'The required input file was not specified on the command line.', ErrStat, ErrMsg, RoutineName) 
      
         !bjj:  if people have compiled themselves, they should be able to figure out the file name, right?         
		 !EB_DTU: I will keep this for a while until  i add another 'if case' for operating system. 
      IF (BITS_IN_ADDR==32) THEN
         CALL NWTC_DisplaySyntax( InputFile, 'Noise_Driver_Win32.exe' )
      ELSEIF( BITS_IN_ADDR == 64) THEN
         CALL NWTC_DisplaySyntax( InputFile, 'Noise_Driver_x64.exe' )
      ELSE
         CALL NWTC_DisplaySyntax( InputFile, 'Noise_Driver.exe' )
      END IF
         
      return
   END IF        
         
      ! Read the  Driver input file
   call Dvr_ReadInputFile(inputFile, DvrData, errStat2, errMsg2 )
      call SetErrStat(errStat2, errMsg2, ErrStat, ErrMsg, RoutineName) 
      if (errStat >= AbortErrLev) return
      
      ! validate the inputs
   call ValidateInputs(DvrData, errStat2, errMsg2)      
      call SetErrStat(errStat2, errMsg2, ErrStat, ErrMsg, RoutineName) 
        
      ! set initialization data:
   call AllocAry( DvrData%BladeRootPosition, 3, DvrData%NumBlades, 'BladeRootPosition', errStat2, ErrMsg2 )
      call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
   call AllocAry( DvrData%BladeRootOrientation, 3, 3, DvrData%NumBlades, 'BladeRootOrientation', errStat2, ErrMsg2 )
      call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
         
  
   DvrData%HubPosition = (/ DvrData%Overhang * cos(DvrData%shftTilt), 0.0_ReKi, DvrData%HubHt /)
   theta(1) = 0.0_ReKi
   theta(2) = -DvrData%shftTilt
   theta(3) = 0.0_ReKi
   DvrData%HubOrientation = EulerConstruct( theta )
     
   
   do k=1,DvrData%numBlades
                     
      theta(1) = (k-1)*TwoPi/real(DvrData%numBlades,ReKi)
      theta(2) = DvrData%precone
      theta(3) = 0.0_ReKi
      DvrData%BladeRootOrientation(:,:,k) = matmul( EulerConstruct( theta ), DvrData%HubOrientation )
                  
      DvrData%BladeRootPosition(:,k)   = DvrData%HubPosition + DvrData%hubRad * DvrData%BladeRootOrientation(3,:,k)      
      
   end do 

      ! Arrays for InflowWind inputs:
   
   call AllocAry( DvrData%InflowOnBlade, 3_IntKi, DvrData%NumBlNds, DvrData%numBlades, 'DvrData%InflowOnBlade', ErrStat2, ErrMsg2 )
      call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
               
   if (errStat >= AbortErrLev) return      
      
   DvrData%InflowOnBlade = 0.0_ReKi
 
  
      call MeshCreate ( BlankMesh = DvrData%HubMotion     &
                       ,IOS       = COMPONENT_INPUT &
                       ,Nnodes    = 1               &
                       ,ErrStat   = ErrStat2        &
                       ,ErrMess   = ErrMsg2         &
                       ,Orientation     = .true.    &
                       ,TranslationDisp = .true.    &
                       ,RotationVel     = .true.    &
                      )
            call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )

      if (errStat >= AbortErrLev) return
                     
      call MeshPositionNode(DvrData%HubMotion, 1, DvrData%HubPosition, errStat2, errMsg2, DvrData%HubOrientation)
         call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
         
      call MeshConstructElement( DvrData%HubMotion, ELEMENT_POINT, errStat2, errMsg2, p1=1 )
         call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
            
      call MeshCommit(DvrData%HubMotion, errStat2, errMsg2 )
         call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
            
      if (errStat >= AbortErrLev) return

         
      DvrData%HubMotion%Orientation     = DvrData%HubMotion%RefOrientation
      DvrData%HubMotion%TranslationDisp = 0.0_R8Ki
      DvrData%HubMotion%RotationVel     = 0.0_ReKi   
      
   
         !................
         ! blade roots
         !................
         
      allocate( DvrData%BladeRootMotion(DvrData%NumBlades), STAT = ErrStat2 )
      if (ErrStat2 /= 0) then
         call SetErrStat( ErrID_Fatal, 'Error allocating DvrData%BladeRootMotion array.', ErrStat, ErrMsg, RoutineName )
         return
      end if      
      
      do k=1,DvrData%NumBlades
         call MeshCreate ( BlankMesh = DvrData%BladeRootMotion(k)                  &
                          ,IOS       = COMPONENT_INPUT                       &
                          ,Nnodes    = 1                                     &
                          ,ErrStat   = ErrStat2                              &
                          ,ErrMess   = ErrMsg2                               &
                          ,Orientation     = .true.                          &
                         )
               call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )

         if (errStat >= AbortErrLev) return
            
         call MeshPositionNode(DvrData%BladeRootMotion(k), 1, DvrData%BladeRootPosition(:,k), errStat2, errMsg2, DvrData%BladeRootOrientation(:,:,k))
            call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
                     
         call MeshConstructElement( DvrData%BladeRootMotion(k), ELEMENT_POINT, errStat2, errMsg2, p1=1 )
            call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
            
         call MeshCommit(DvrData%BladeRootMotion(k), errStat2, errMsg2 )
            call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
            
         if (errStat >= AbortErrLev) return

      
         DvrData%BladeRootMotion(k)%Orientation     = DvrData%BladeRootMotion(k)%RefOrientation
   
   end do !k=numBlades      
      
      
         !................
         ! blades
         !................
   
      allocate( DvrData%BladeMotion(DvrData%NumBlades), STAT = ErrStat2 )
      if (ErrStat2 /= 0) then
         call SetErrStat( ErrID_Fatal, 'Error allocating DvrData%BladeMotion array.', ErrStat, ErrMsg, RoutineName )
         return
      end if
      
      do k=1,DvrData%NumBlades
         call MeshCreate ( BlankMesh = DvrData%BladeMotion(k)                     &
                          ,IOS       = COMPONENT_INPUT                      &
                          ,Nnodes    = DvrData%NumBlNds &
                          ,ErrStat   = ErrStat2                             &
                          ,ErrMess   = ErrMsg2                              &
                          ,Orientation     = .true.                         &
                          ,TranslationDisp = .true.                         &
                          ,TranslationVel  = .true.                         &
                         )
               call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )

         if (errStat >= AbortErrLev) return
            
                        
         do j=1,DvrData%NumBlNds

               ! reference position of the jth node in the kth blade, relative to the root in the local blade coordinate system:
            positionL(1) = DvrData%BlCrvAC(j)
            positionL(2) = DvrData%BlSwpAC(j)
            positionL(3) = DvrData%BlSpn(  j)
            
               ! reference position of the jth node in the kth blade:
            position = DvrData%BladeRootMotion(k)%Position(:,1) + matmul(positionL,DvrData%BladeRootMotion(k)%RefOrientation(:,:,1))  ! note that because positionL is a 1-D array, we're doing the transpose of matmul(transpose(DvrData%BladeRootMotion(k)%RefOrientation),positionL)

            
               ! reference orientation of the jth node in the kth blade, relative to the root in the local blade coordinate system:
            theta(1)     =  0.0_R8Ki
            theta(2)     =  DvrData%BlCrvAng(j)
            theta(3)     = -DvrData%BlTwist( j)            
            orientationL = EulerConstruct( theta )
                                 
               ! reference orientation of the jth node in the kth blade
            orientation = matmul( orientationL, DvrData%BladeRootMotion(k)%RefOrientation(:,:,1) )

            
            call MeshPositionNode(DvrData%BladeMotion(k), j, position, errStat2, errMsg2, orientation)
               call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
               
         end do ! j=blade nodes
         
            ! create line2 elements
         do j=1,DvrData%NumBlNds-1
            call MeshConstructElement( DvrData%BladeMotion(k), ELEMENT_LINE2, errStat2, errMsg2, p1=j, p2=j+1 )
               call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
         end do !j
            
         call MeshCommit(DvrData%BladeMotion(k), errStat2, errMsg2 )
            call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
            
         if (errStat >= AbortErrLev) return

      
         DvrData%BladeMotion(k)%Orientation     = DvrData%BladeMotion(k)%RefOrientation
         DvrData%BladeMotion(k)%TranslationDisp = 0.0_R8Ki
         DvrData%BladeMotion(k)%TranslationVel  = 0.0_ReKi
   
   end do !k=numBlades


  CALL AllocAry( DvrData%BlTECo, DvrData%numBlades, DvrData%NumBlNds,3, 'BlTECo',  ErrStat2, ErrMsg2)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
  CALL AllocAry( DvrData%BlLECo, DvrData%numBlades, DvrData%NumBlNds,3, 'BlLECo',  ErrStat2, ErrMsg2)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )


   
end subroutine Dvr_Init 
!----------------------------------------------------------------------------------------------------------------------------------
subroutine Dvr_ReadInputFile(fileName, DvrData, errStat, errMsg )
   ! This routine opens the gets the data from the input files.

   character(*),                  intent( in    )   :: fileName
   type(Dvr_SimData),             intent(   out )   :: DvrData
   integer,                       intent(   out )   :: errStat              ! returns a non-zero value when an error occurs  
   character(*),                  intent(   out )   :: errMsg               ! Error message if errStat /= ErrID_None
   

      ! Local variables
   character(1024)              :: PriPath
   character(1024)              :: inpVersion                               ! String containing the input-version information.
   character(1024)              :: line                                     ! String containing a line of input.
   integer                      :: unIn, unEc
   integer                      :: ICase
   integer                      :: I
   integer                      :: Sttus
   character( 11)               :: DateNow                                  ! Date shortly after the start of execution.
   character(  8)               :: TimeNow                                  ! Time of day shortly after the start of execution.
   
   integer, parameter           :: NumCols = 7                              ! number of columns to be read from the input file
   integer, parameter           :: MaxBl = 3                              ! number of columns to be read from the input file
   real(ReKi)                   :: InpCase(NumCols)                         ! Temporary array to hold combined-case input parameters.
   logical                      :: TabDel      
   logical                      :: echo   

   INTEGER(IntKi)               :: ErrStat2,IOS                                 ! Temporary Error status
   CHARACTER(ErrMsgLen)         :: ErrMsg2                                  ! Temporary Err msg
   CHARACTER(*), PARAMETER      :: RoutineName = 'Dvr_ReadInputFile'
   CHARACTER(1024)              :: DvrBlFile(MaxBl) ! File that contains the blade information (specified in the primary input file)
   CHARACTER(1000)                             :: inputFile     ! String to hold the file name. 
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

   
   call WrScr( 'Opening input file:  '//fileName )

      ! Skip a line, read the run title information.

   CALL ReadStr( UnIn, fileName, inpVersion, 'inpVersion', 'File Header: (line 1)', ErrStat2, ErrMsg2, UnEc )
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
   
   CALL ReadStr( UnIn, fileName, DvrData%OutFileData%runTitle, 'runTitle', 'File Header: File Description (line 2)', ErrStat2, ErrMsg2, UnEc )
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
   
   call WrScr1 ( ' '//DvrData%OutFileData%runTitle )
   
      ! Read in the title line for the input-configuration subsection.
   CALL ReadStr( UnIn, fileName, line, 'line', 'File Header: (line 3)', ErrStat2, ErrMsg2, UnEc )
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )


      ! See if we should echo the output.     
   call ReadVar ( unIn, fileName, echo, 'Echo', 'Echo Input', errStat2, errMsg2, UnEc )
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )

   if ( echo )  then
         ! Get date and time.
      dateNow = CurDate()
      timeNow = CurTime()
      call GetNewUnit( unEc ) 
      call getroot(fileName,DvrData%OutFileData%Root)      
      call  OpenFOutFile ( unEc, trim( DvrData%OutFileData%Root )//'.ech', errStat2, errMsg2 )
         call setErrStat( errStat2, ErrMsg2 , errStat, ErrMsg , RoutineName )
         if ( errStat >= AbortErrLev ) then
            call cleanup()
            return
         end if
      
      write (unEc,'(A)')      'Echo of Input File:'
      write (unEc,'(A)')      ' "'//fileName//'"'
      write (unEc,'(A)')      'Generated on: '//trim( dateNow )//' at '//trim( timeNow )//'.'
      write (unEc,'(A)')      inpVersion
      write (unEc,'(A)')      DvrData%OutFileData%runTitle
      write (unEc,'(A)')      line
      write (unEc,Ec_LgFrmt)  echo, 'Echo', 'Echo input parameters to "rootname.ech"?'
   end if


      ! Read the rest of input-configuration section.
      
   call ReadVar ( unIn, fileName, DvrData%NN_InputFile,   'NN_InputFile',   'Name of the Noise input file', errStat2, errMsg2, UnEc )
      call setErrStat( errStat2, ErrMsg2 , errStat, ErrMsg , RoutineName )
      if ( errStat >= AbortErrLev ) then
         call cleanup()
         return
      end if
   IF ( PathIsRelative( DvrData%NN_InputFile ) ) DvrData%NN_InputFile = TRIM(PriPath)//TRIM(DvrData%NN_InputFile)

      ! DvrBlFile - Names of files containing distributed aerodynamic properties for each blade (see NN_BladeInputFile type):
   DO I = 1,MaxBl            
      CALL ReadVar ( UnIn, InputFile, DvrBlFile(I), 'DvrBlFile('//TRIM(Num2Lstr(I))//')', 'Name of file containing distributed aerodynamic properties for blade '//TRIM(Num2Lstr(I)), ErrStat2, ErrMsg2, UnEc )
         CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      IF ( PathIsRelative( DvrBlFile(I) ) ) DvrBlFile(I) = TRIM(PriPath)//TRIM(DvrBlFile(I))
   END DO   

      ! Read the turbine-data section.

   call ReadCom ( unIn, fileName, 'the turbine-data subtitle', errStat2, errMsg2, UnEc )
      call setErrStat( errStat2, ErrMsg2 , errStat, ErrMsg , RoutineName )
   call ReadVar ( unIn, fileName, DvrData%numBlades,'NumBlades','Number of blades', errStat2, errMsg2, UnEc )
      call setErrStat( errStat2, ErrMsg2 , errStat, ErrMsg , RoutineName )
   call ReadVar ( unIn, fileName, DvrData%HubRad,   'HubRad',   'Hub radius (m)', errStat2, errMsg2, UnEc )
      call setErrStat( errStat2, ErrMsg2 , errStat, ErrMsg , RoutineName )
   call ReadVar ( unIn, fileName, DvrData%HubHt,    'HubHt',    'Hub height (m)', errStat2, errMsg2, UnEc )
      call setErrStat( errStat2, ErrMsg2 , errStat, ErrMsg , RoutineName )
   call ReadVar ( unIn, fileName, DvrData%Overhang, 'Overhang',  'Overhang (m)', errStat2, errMsg2, UnEc )
      call setErrStat( errStat2, ErrMsg2 , errStat, ErrMsg , RoutineName )
   call ReadVar ( unIn, fileName, DvrData%ShftTilt, 'ShftTilt',  'Shaft tilt (deg)', errStat2, errMsg2, UnEc )
      call setErrStat( errStat2, ErrMsg2 , errStat, ErrMsg , RoutineName )
      DvrData%ShftTilt = DvrData%ShftTilt*D2R
   call ReadVar ( unIn, fileName, DvrData%precone, 'Precone',  'Precone (deg)', errStat2, errMsg2, UnEc )
      call setErrStat( errStat2, ErrMsg2 , errStat, ErrMsg , RoutineName )
      DvrData%precone = DvrData%precone*D2R
      
      if ( errStat >= AbortErrLev ) then
         call cleanup()
         return
      end if           


      ! Read the I/O-configuration section.

   call ReadCom ( unIn, fileName, 'the I/O-configuration subtitle', errStat2, errMsg2, UnEc )
      call setErrStat( errStat2, ErrMsg2 , errStat, ErrMsg , RoutineName )
   call ReadVar ( unIn, fileName, DvrData%OutFileData%Root, 'OutFileRoot', 'Root name for any output files', errStat2, errMsg2, UnEc )
      call setErrStat( errStat2, ErrMsg2 , errStat, ErrMsg , RoutineName )
   if (len_trim(DvrData%OutFileData%Root) == 0) then
      call getroot(fileName,DvrData%OutFileData%Root)
   end if
   
   call ReadVar ( unIn, fileName, TabDel,   'TabDel',   'Make output tab-delimited (fixed-width otherwise)?', errStat2, errMsg2, UnEc )
      call setErrStat( errStat2, ErrMsg2 , errStat, ErrMsg , RoutineName )
      if (TabDel) then
         DvrData%OutFileData%delim = TAB
      else
         DvrData%OutFileData%delim = " "
      end if
               
      ! OutFmt - Format used for text tabular output (except time).  Resulting field should be 10 characters. (-):
   call ReadVar( UnIn, fileName, DvrData%OutFileData%OutFmt, "OutFmt", "Format used for text tabular output (except time).  Resulting field should be 10 characters. (-)", ErrStat2, ErrMsg2, UnEc)
      call setErrStat( errStat2, ErrMsg2 , errStat, ErrMsg , RoutineName ) !bjj: this is a global variable in NWTC_Library            
   call ReadVar ( unIn, fileName, Beep,  'Beep',     'Beep on exit?', errStat2, errMsg2, UnEc )
      call setErrStat( errStat2, ErrMsg2 , errStat, ErrMsg , RoutineName ) !bjj: this is a global variable in NWTC_Library
      if ( errStat >= AbortErrLev ) then
         call cleanup()
         return
      end if


      ! Read the combined-case section.

   call ReadCom  ( unIn, fileName, 'the combined-case subtitle', errStat2, errMsg2, UnEc )
      call setErrStat( errStat2, ErrMsg2 , errStat, ErrMsg , RoutineName )
   call ReadVar  ( unIn, fileName, DvrData%NumCases, 'NumCases', 'Number of cases to run', errStat2, errMsg2, UnEc )
      call setErrStat( errStat2, ErrMsg2 , errStat, ErrMsg , RoutineName )
   call ReadCom  ( unIn, fileName, 'the combined-case-block header (names)', errStat2, errMsg2, UnEc )
      call setErrStat( errStat2, ErrMsg2 , errStat, ErrMsg , RoutineName )
   call ReadCom  ( unIn, fileName, 'the combined-case-block header (units)', errStat2, errMsg2, UnEc )
      call setErrStat( errStat2, ErrMsg2 , errStat, ErrMsg , RoutineName )

      if ( errStat >= AbortErrLev ) then
         call cleanup()
         return
      end if
      
   if ( DvrData%NumCases < 1 )  then
      call setErrStat( ErrID_Fatal,'Variable "NumCases" must be > 0.' ,errstat,errmsg,routinename)
      call cleanup()
      return
   end if
   
   allocate ( DvrData%Cases(DvrData%NumCases) , STAT=Sttus )
   if ( Sttus /= 0 )  then
      call setErrStat( ErrID_Fatal,'Error allocating memory for the Cases array.',errstat,errmsg,routinename)
      call cleanup()
      return
   end if

   do ICase=1,DvrData%NumCases

      call ReadAry ( unIn, fileName, InpCase,  NumCols, 'InpCase',  'parameters for Case #' &
                     //trim( Int2LStr( ICase ) )//'.', errStat2, errMsg2, UnEc )
         call setErrStat( errStat2, ErrMsg2 , errStat, ErrMsg , RoutineName )
            
      DvrData%Cases(iCase)%WndSpeed        = InpCase( 1)
      DvrData%Cases(ICase)%ShearExp        = InpCase( 2)
      DvrData%Cases(ICase)%RotSpeed        = InpCase( 3)*RPM2RPS
      DvrData%Cases(ICase)%Pitch           = InpCase( 4)*D2R
      DvrData%Cases(ICase)%Yaw             = InpCase( 5)*D2R
      DvrData%Cases(iCase)%dT              = InpCase( 6)
      DvrData%Cases(iCase)%Tmax            = InpCase( 7)
               
   end do ! ICase




  CALL GetNewUnit( UnIn, ErrStat2, ErrMsg2 )
      CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)


      ! Open the input file for blade 1 and that is the only one.

   CALL OpenFInpFile ( UnIn, DvrBlFile(1), ErrStat2, ErrMsg2 )
      CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      IF ( ErrStat >= AbortErrLev ) RETURN


   !  -------------- HEADER -------------------------------------------------------

      ! Skip the header.

   CALL ReadCom ( UnIn, DvrBlFile(1), 'unused blade file header line 1', ErrStat2, ErrMsg2, UnEc )
      CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)

   CALL ReadCom ( UnIn, DvrBlFile(1), 'unused blade file header line 2', ErrStat2, ErrMsg2, UnEc )
      CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      
   !  -------------- Blade properties table ------------------------------------------                                    
   CALL ReadCom ( UnIn, DvrBlFile(1), 'Section header: Blade Properties', ErrStat2, ErrMsg2, UnEc )
      CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)

      ! NumBlNds - Number of blade nodes used in the analysis (-):
   CALL ReadVar( UnIn, DvrBlFile(1), DvrData%NumBlNds, "NumBlNds", "Number of blade nodes used in the analysis (-)", ErrStat2, ErrMsg2, UnEc)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      IF ( ErrStat >= AbortErrLev ) RETURN

   CALL ReadCom ( UnIn, DvrBlFile(1), 'Table header: names', ErrStat2, ErrMsg2, UnEc )
      CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)

   CALL ReadCom ( UnIn, DvrBlFile(1), 'Table header: units', ErrStat2, ErrMsg2, UnEc )
      CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      
   IF ( ErrStat>= AbortErrLev ) THEN 
      CALL Cleanup()
      RETURN
   END IF
   
      
      ! allocate space for blade inputs:
   CALL AllocAry( DvrData%BlSpn,   DvrData%NumBlNds, 'BlSpn',   ErrStat2, ErrMsg2)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
   CALL AllocAry( DvrData%BlCrvAC, DvrData%NumBlNds, 'BlCrvAC', ErrStat2, ErrMsg2)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
   CALL AllocAry( DvrData%BlSwpAC, DvrData%NumBlNds, 'BlSwpAC', ErrStat2, ErrMsg2)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
   CALL AllocAry( DvrData%BlCrvAng,DvrData%NumBlNds, 'BlCrvAng',ErrStat2, ErrMsg2)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
   CALL AllocAry( DvrData%BlTwist, DvrData%NumBlNds, 'BlTwist', ErrStat2, ErrMsg2)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
   CALL AllocAry( DvrData%BlChord, DvrData%NumBlNds, 'BlChord', ErrStat2, ErrMsg2)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
   CALL AllocAry( DvrData%BlAFID,  DvrData%NumBlNds, 'BlAFID',  ErrStat2, ErrMsg2)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
  CALL AllocAry( DvrData%BlTECo_in,  DvrData%NumBlNds,3, 'BlTECo',  ErrStat2, ErrMsg2)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
  CALL AllocAry( DvrData%BlLECo_in,  DvrData%NumBlNds,3, 'BlLECo',  ErrStat2, ErrMsg2)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
 CALL AllocAry( DvrData%BlStaMul,  DvrData%NumBlNds, 'BlStaMul',  ErrStat2, ErrMsg2)
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )



      ! Return on error if we didn't allocate space for the next inputs
   IF ( ErrStat >= AbortErrLev ) THEN
      CALL Cleanup()
      RETURN
   END IF
            
   DO I=1,DvrData%NumBlNds
      READ( UnIn, *, IOStat=IOS ) DvrData%BlSpn(I), DvrData%BlCrvAC(I), DvrData%BlSwpAC(I), &
                                  DvrData%BlCrvAng(I), DvrData%BlTwist(I), DvrData%BlChord(I), &
                                  DvrData%BlAFID(I),DvrData%BlTECo_in(I,1),DvrData%BlTECo_in(I,2), &
				  DvrData%BlTECo_in(I,3),DvrData%BlLECo_in(I,1),DvrData%BlLECo_in(I,2), &
				  DvrData%BlLECo_in(I,3), DvrData%BlStaMul(I)
         CALL CheckIOS( IOS, DvrBlFile(1), 'Blade properties row '//TRIM(Num2LStr(I)), NumType, ErrStat2, ErrMsg2 )
         CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
               ! Return on error if we couldn't read this line
            IF ( ErrStat >= AbortErrLev ) THEN
               CALL Cleanup()
               RETURN
            END IF
         
         IF (UnEc > 0) THEN
            WRITE( UnEc, "(6(F9.4,1x),I9)", IOStat=IOS) DvrData%BlSpn(I), DvrData%BlCrvAC(I), DvrData%BlSwpAC(I), &
                                  DvrData%BlCrvAng(I), DvrData%BlTwist(I), DvrData%BlChord(I), &
                                  DvrData%BlAFID(I),DvrData%BlTECo_in(I,1),DvrData%BlTECo_in(I,2), &
				  DvrData%BlTECo_in(I,3),DvrData%BlLECo_in(I,1),DvrData%BlLECo_in(I,2), &
				  DvrData%BlLECo_in(I,3), DvrData%BlStaMul(I)
         END IF         
   END DO
   DvrData%BlCrvAng = DvrData%BlCrvAng*D2R
   DvrData%BlTwist  = DvrData%BlTwist*D2R
                  
   !  -------------- END OF FILE --------------------------------------------




   
   call cleanup ( )






   RETURN
contains
   subroutine cleanup()
      if (UnIn>0) close(UnIn)
      if (UnEc>0) close(UnEc)
   end subroutine cleanup
end subroutine Dvr_ReadInputFile
!----------------------------------------------------------------------------------------------------------------------------------
subroutine ValidateInputs(DvrData, errStat, errMsg)

   type(Dvr_SimData),             intent(in)    :: DvrData
   integer,                       intent(  out) :: errStat           ! returns a non-zero value when an error occurs  
   character(*),                  intent(  out) :: errMsg            ! Error message if errStat /= ErrID_None

   ! local variables:
   integer(intKi)                               :: i
   integer(intKi)                               :: FmtWidth          ! number of characters in string produced by DvrData%OutFmt
   integer(intKi)                               :: ErrStat2          ! temporary Error status
   character(ErrMsgLen)                         :: ErrMsg2           ! temporary Error message
   character(*), parameter                      :: RoutineName = 'ValidateInputs'
   
   
   
   ErrStat = ErrID_None
   ErrMsg  = ""
   
   
      ! Turbine Data:
   if ( DvrData%numBlades < 1 ) call SetErrStat( ErrID_Fatal, "There must be at least 1 blade (numBlades).", ErrStat, ErrMsg, RoutineName)
   if ( DvrData%numBlades > 3 ) call SetErrStat( ErrID_Fatal, "There can be no more than 3 blades (numBlades).", ErrStat, ErrMsg, RoutineName)
   if ( DvrData%HubRad < 0.0_ReKi .or. EqualRealNos(DvrData%HubRad, 0.0_ReKi) ) call SetErrStat( ErrID_Fatal, "HubRad must be a positive number.", ErrStat, ErrMsg, RoutineName)
   if ( DvrData%HubHt < DvrData%HubRad ) call SetErrStat( ErrID_Fatal, "HubHt must be at least HubRad.", ErrStat, ErrMsg, RoutineName)
   
      
      ! I-O Settings:
      ! Check that DvrData%OutFileData%OutFmt is a valid format specifier and will fit over the column headings
   call ChkRealFmtStr( DvrData%OutFileData%OutFmt, 'OutFmt', FmtWidth, ErrStat2, ErrMsg2 )
      call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )

   if ( FmtWidth /= ChanLen ) call SetErrStat( ErrID_Warn, 'OutFmt produces a column width of '// &
      TRIM(Num2LStr(FmtWidth))//' instead of '//TRIM(Num2LStr(ChanLen))//' characters.', ErrStat, ErrMsg, RoutineName )

      ! Combined-Case Analysis:
   do i=1,DvrData%NumCases
   
      if (DvrData%Cases(i)%DT < epsilon(0.0_ReKi) ) call SetErrStat(ErrID_Fatal,'dT must be larger than 0 in case '//trim(num2lstr(i))//'.',ErrStat, ErrMsg,RoutineName)
      if (DvrData%Cases(i)%TMax < DvrData%Cases(i)%DT ) call SetErrStat(ErrID_Fatal,'TMax must be larger than dT in case '//trim(num2lstr(i))//'.',ErrStat, ErrMsg,RoutineName)
      
   end do
   
   
   
end subroutine ValidateInputs
!----------------------------------------------------------------------------------------------------------------------------------
!----------------------------------------------------------------------------------------------------------------------------------
subroutine Init_Noise(iCase, DvrData, NN, dt, errStat, errMsg)

   integer(IntKi),               intent(in   ) :: iCase         ! driver case
   type(Dvr_SimData),            intent(inout) :: DvrData       ! Input data for initialization
   type(Noise_Data),             intent(inout) :: NN       	! Noise data 
   real(DbKi),                   intent(inout) :: dt            ! interval
      
   integer(IntKi)              , intent(  out) :: errStat       ! Status of error message
   character(*)                , intent(  out) :: errMsg        ! Error message if ErrStat /= ErrID_None

      ! locals
   real(reKi)                                  :: theta(3)
   integer(IntKi)                              :: j, k   
   integer(IntKi)                              :: errStat2      ! local status of error message
   character(ErrMsgLen)                        :: errMsg2       ! local error message if ErrStat /= ErrID_None
   character(*), parameter                     :: RoutineName = 'Init_Noise'
                                                  
   ! local data                                
   type(NN_InitInputType)                      :: InitInData     ! Input data for initialization
   type(NN_InitOutputType)                     :: InitOutData    ! Output data from initialization  
      
   errStat = ErrID_None
   errMsg  = ''
   
   InitInData%InputFile      = DvrData%NN_InputFile
   InitInData%NumBlades      = DvrData%numBlades
   InitInData%NumBlNds      = DvrData%NumBlNds
   InitInData%RootName       = DvrData%outFileData%Root

   call AllocAry( InitInData%BlChord, DvrData%NumBlNds, 'BlChord', errStat2, ErrMsg2 )
      call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
   InitInData%BlChord=DvrData%BlChord

   call AllocAry( InitInData%BlSpn,   DvrData%NumBlNds, 'BlSpn', errStat2, ErrMsg2 )
      call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
   InitInData%BlSpn=DvrData%BlSpn     
      
   call NN_Init(InitInData, NN%u(1), NN%p, NN%x, NN%xd, NN%z, NN%OtherState, NN%y, NN%m, dt, InitOutData, ErrStat2, ErrMsg2 )
      call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
   if (ErrStat >= AbortErrLev) then
      call Cleanup()
      return
   end if
   
   ! we know exact values, so we're going to initialize inputs this way (instead of using the input guesses from AD_Init)
   NN%InputTime = -999
   DO j = 1-numInp, 0
      call Set_NN_Inputs(iCase,j,DvrData,NN,errStat2,errMsg2)   
         call SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
   END DO              
  
      
      ! move NN initOut data to NN Driver
   call move_alloc( InitOutData%WriteOutputHdr, DvrData%OutFileData%WriteOutputHdr )
   call move_alloc( InitOutData%WriteOutputUnt, DvrData%OutFileData%WriteOutputUnt )   
     
   DvrData%OutFileData%NN_ver = InitOutData%ver
   
contains
   subroutine cleanup()
      call NN_DestroyInitInput( InitInData, ErrStat2, ErrMsg2 )   
      call NN_DestroyInitOutput( InitOutData, ErrStat2, ErrMsg2 )      
   end subroutine cleanup
   
end subroutine Init_Noise
!----------------------------------------------------------------------------------------------------------------------------------
!----------------------------------------------------------------------------------------------------------------------------------
!> this routine returns time=(nt-1) * DvrData%Cases(iCase)%dT, and cycles values in the input array NN%InputTime and NN%u.
!! it then sets the inputs for nt * DvrData%Cases(iCase)%dT, which are index values 1 in the arrays.
subroutine Set_NN_Inputs(iCase,nt,DvrData,NN,errStat,errMsg)

   integer(IntKi)              , intent(in   ) :: iCase         ! case number 
   integer(IntKi)              , intent(in   ) :: nt            ! time step number
   
   type(Dvr_SimData),            intent(inout) :: DvrData       ! Driver data 
   type(Noise_Data),             intent(inout) :: NN            ! Noise data 
   integer(IntKi)              , intent(  out) :: errStat       ! Status of error message
   character(*)                , intent(  out) :: errMsg        ! Error message if ErrStat /= ErrID_None

      ! local variables
   integer(IntKi)                              :: errStat2      ! local status of error message
   character(ErrMsgLen)                        :: errMsg2       ! local error message if ErrStat /= ErrID_None
   character(*), parameter                     :: RoutineName = 'Set_NN_Inputs'

   integer(intKi)                              :: j             ! loop counter for nodes
   integer(intKi)                              :: k             ! loop counter for blades

   real(ReKi)                                  :: z             ! height (m)
   !real(ReKi)                                  :: angle
   real(ReKi)                                  :: theta(3)
   real(ReKi)                                  :: position(3)
   real(ReKi)                                  :: orientation(3,3)
   real(ReKi)                                  :: rotateMat(3,3)
   real(ReKi)                                  :: degrot

   
   errStat = ErrID_None
   errMsg  = ""

      ! note that this initialization is a little different than the general algorithm in FAST because here
      ! we can get exact values, so we are going to ignore initial guesses and not extrapolate
   NN%m%RotSpeedAoA =  DvrData%Cases(ICase)%RotSpeed 
   !................
   ! shift previous calculations:
   !................
   do j = numInp-1,1,-1
      call NN_CopyInput (NN%u(j),  NN%u(j+1),  MESH_UPDATECOPY, ErrStat2, ErrMsg2)
         call SetErrStat(ErrStat2,ErrMsg2,ErrStat,ErrMsg,RoutineName)
            
      NN%InputTime(j+1) = NN%InputTime(j)
   end do
   NN%inputTime(1) = nt * DvrData%Cases(iCase)%dT
         
   !................
   ! calculate new values
   !................
  

      ! Hub motions:
      theta(1) = 0.0_ReKi
      theta(2) = 0.0_ReKi
      theta(3) = DvrData%Cases(iCase)%Yaw
      orientation = EulerConstruct(theta)
            
 DvrData%HubMotion%TranslationDisp(:,1) = matmul( DvrData%HubMotion%Position(:,1), orientation ) - DvrData%HubMotion%Position(:,1) 
      
      theta(1) = NN%inputTime(1) * DvrData%Cases(iCase)%RotSpeed
      theta(2) = 0.0_ReKi
      theta(3) = 0.0_ReKi
      DvrData%HubMotion%Orientation(  :,:,1) = matmul( DvrData%HubMotion%RefOrientation(:,:,1), orientation )
      orientation = EulerConstruct( theta )      
      DvrData%HubMotion%Orientation(  :,:,1) = matmul( orientation, DvrData%HubMotion%Orientation(  :,:,1) )
      
      DvrData%HubMotion%RotationVel(    :,1) = DvrData%HubMotion%Orientation(1,:,1) * DvrData%Cases(iCase)%RotSpeed
                  
      ! Blade root motions:
      do k=1,DvrData%numBlades         
         theta(1) = (k-1)*TwoPi/real(DvrData%numBlades,ReKi)
         theta(2) =  DvrData%precone
         theta(3) = -DvrData%Cases(iCase)%pitch
         orientation = EulerConstruct(theta)
         
         DvrData%BladeRootMotion(k)%Orientation(  :,:,1) = matmul( orientation, DvrData%HubMotion%Orientation(  :,:,1) )
         
      end do !k=numBlades
            
!      ! Blade motions:
      do k=1,DvrData%numBlades
         rotateMat = transpose( DvrData%BladeRootMotion(k)%Orientation(  :,:,1) )
         rotateMat = matmul( rotateMat,DvrData%BladeRootMotion(k)%RefOrientation(  :,:,1) ) 
         orientation = transpose(rotateMat)
         
         rotateMat(1,1) = rotateMat(1,1) - 1.0_ReKi
         rotateMat(2,2) = rotateMat(2,2) - 1.0_ReKi
         rotateMat(3,3) = rotateMat(3,3) - 1.0_ReKi
                  
         do j=1,DvrData%BladeMotion(k)%nnodes        
            position = DvrData%BladeMotion(k)%Position(:,j) - DvrData%HubMotion%Position(:,1) 
            DvrData%BladeMotion(k)%TranslationDisp(:,j) = DvrData%HubMotion%TranslationDisp(:,1) + matmul( rotateMat, position )
            
            DvrData%BladeMotion(k)%Orientation(  :,:,j) = matmul( DvrData%BladeMotion(k)%RefOrientation(:,:,j), orientation )
            
            
            position =  DvrData%BladeMotion(k)%Position(:,j) + DvrData%BladeMotion(k)%TranslationDisp(:,j) &
                      - DvrData%HubMotion%Position(:,1) - DvrData%HubMotion%TranslationDisp(:,1)
            DvrData%BladeMotion(k)%TranslationVel( :,j) = cross_product( DvrData%HubMotion%RotationVel(:,1), position )

         end do !j=nnodes
                                    
      end do !k=numBlades       
      
      ! Inflow wind velocities:
      ! InflowOnBlade
      do k=1,DvrData%numBlades
         do j=1,DvrData%BladeMotion(k)%nnodes
            z = DvrData%BladeMotion(k)%Position(3,j) + DvrData%BladeMotion(k)%TranslationDisp(3,j)
            DvrData%InflowOnBlade(1,j,k) = GetU(  DvrData%Cases(iCase)%WndSpeed, DvrData%HubHt, DvrData%Cases(iCase)%ShearExp, z )
            DvrData%InflowOnBlade(2,j,k) = 0.0_ReKi !V
            DvrData%InflowOnBlade(3,j,k) = 0.0_ReKi !W   
!       ! Assume a = 0.3 induction, a'=0.5
!         NN%u(1)%Vrel(j,k) =  (( DvrData%BlSpn(j) * DvrData%Cases(iCase)%RotSpeed ) * (1+1.33) )**2 +    (DvrData%InflowOnBlade(1,j,k)*(1-0.28))**2 
!	 NN%u(1)%Vrel(j,k) =  sqrt(NN%u(1)%Vrel(j,k))
!       ! Assume no pitch AoA =  inflow angle - twist
!         NN%u(1)%AoANoise(j,k)  =  (DvrData%InflowOnBlade(1,j,k)*(1-0.28)) /  ((DvrData%BlSpn(j) * DvrData%Cases(iCase)%RotSpeed)*(1+1.33))  
!	 NN%u(1)%AoANoise(j,k)  =  ATAN(NN%u(1)%AoANoise(j,k)) - DvrData%BlTwist(j)
!	 NN%u(1)%AoANoise(j,k)  =  NN%u(1)%AoANoise(j,k) * R2D


       ! Assume no induction, Vrel = SQRT( (omegar*r)^2 + u^2)   
         NN%u(1)%Vrel(j,k) =  (( DvrData%BlSpn(j) * DvrData%Cases(iCase)%RotSpeed )  )**2 +    (DvrData%InflowOnBlade(1,j,k))**2 
	 NN%u(1)%Vrel(j,k) =  sqrt(NN%u(1)%Vrel(j,k))
       ! Assume no pitch AoA =  inflow angle - twist
         NN%u(1)%AoANoise(j,k)  =  (DvrData%InflowOnBlade(1,j,k)) /  ((DvrData%BlSpn(j) * DvrData%Cases(iCase)%RotSpeed))  
	 NN%u(1)%AoANoise(j,k)  =  ATAN(NN%u(1)%AoANoise(j,k)) - DvrData%BlTwist(j)
	 NN%u(1)%AoANoise(j,k)  =  NN%u(1)%AoANoise(j,k)  ! keep it as rad 

!	print*, DvrData%InflowOnBlade(1,j,k),z,NN%u(1)%AoANoise(j,k),DvrData%BlTwist(j)*R2D,NN%u(1)%Vrel(j,k)
         end do !j=nnodes
      end do !k=numBlades



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Added later on for rotation of trailing edge (TE) and leading edge (LE) coordinates
! possibly blademotion matrices can be used but not very well understood by EB_DTU 
! so the rotation of TE and LE is done with the loop below for each time step
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! ROTATING BLADES MANUALLY

DEGROT = 3*nt ! dont use this change this with respect to delta t automatically will be set  (EB_DTU)
        do j=2,DvrData%BladeMotion(1)%nnodes


! Streamwise direction (x - horizontal) no change Blade Leading Edge Coordinates no deflection
		DvrData%BlLECo(1,j,1)=DvrData%BlLECo_in(j,1)  
		DvrData%BlLECo(2,j,1)=DvrData%BlLECo_in(j,1)
		DvrData%BlLECo(3,j,1)=DvrData%BlLECo_in(j,1)
! Crosswind direction (y - lateral/sideways) changing  Blade Leading Edge Coordinates
		DvrData%BlLECo(1,j,2)=DvrData%BlLECo_in(1,2)  +  ((DvrData%BlSpn(j)-DvrData%BlSpn(j-1))*j*COS(D2R*DEGROT))
		DvrData%BlLECo(2,j,2)=DvrData%BlLECo_in(1,2)  +  ((DvrData%BlSpn(j)-DvrData%BlSpn(j-1))*j*COS(D2R*(DEGROT+120)))
		DvrData%BlLECo(3,j,2)=DvrData%BlLECo_in(1,2)  +  ((DvrData%BlSpn(j)-DvrData%BlSpn(j-1))*j*COS(D2R*(DEGROT+240)))
! Vertical direction (z - vertical) changing  Blade Leading Edge Coordinates
		DvrData%BlLECo(1,j,3)=DvrData%BlLECo_in(j,3)  +  ((DvrData%BlSpn(j)-DvrData%BlSpn(j-1))*j*SIN(D2R*DEGROT))
		DvrData%BlLECo(2,j,3)=DvrData%BlLECo_in(j,3)  +  ((DvrData%BlSpn(j)-DvrData%BlSpn(j-1))*j*SIN(D2R*(DEGROT+120)))
		DvrData%BlLECo(3,j,3)=DvrData%BlLECo_in(j,3)  +  ((DvrData%BlSpn(j)-DvrData%BlSpn(j-1))*j*SIN(D2R*(DEGROT+240)))

! Streamwise direction (x - horizontal) no change Blade Trailing Edge Coordinates no deflection
		DvrData%BlTECo(1,j,1)=DvrData%BlTECo_in(j,1)  
		DvrData%BlTECo(2,j,1)=DvrData%BlTECo_in(j,1)
		DvrData%BlTECo(3,j,1)=DvrData%BlTECo_in(j,1)
! Crosswind direction (y - lateral/sideways) changing  Blade Trailing Edge Coordinates
		DvrData%BlTECo(1,j,2)=DvrData%BlTECo_in(1,2)  +  ((DvrData%BlSpn(j)-DvrData%BlSpn(j-1))*j*COS(D2R*DEGROT))
		DvrData%BlTECo(2,j,2)=DvrData%BlTECo_in(1,2)  +  ((DvrData%BlSpn(j)-DvrData%BlSpn(j-1))*j*COS(D2R*(DEGROT+120)))
		DvrData%BlTECo(3,j,2)=DvrData%BlTECo_in(1,2)  +  ((DvrData%BlSpn(j)-DvrData%BlSpn(j-1))*j*COS(D2R*(DEGROT+240)))
! Vertical direction (z - vertical) changing  Blade Trailing Edge Coordinates
		DvrData%BlTECo(1,j,3)=DvrData%BlTECo_in(j,3)  +  ((DvrData%BlSpn(j)-DvrData%BlSpn(j-1))*j*SIN(D2R*DEGROT))
		DvrData%BlTECo(2,j,3)=DvrData%BlTECo_in(j,3)  +  ((DvrData%BlSpn(j)-DvrData%BlSpn(j-1))*j*SIN(D2R*(DEGROT+120)))
		DvrData%BlTECo(3,j,3)=DvrData%BlTECo_in(j,3)  +  ((DvrData%BlSpn(j)-DvrData%BlSpn(j-1))*j*SIN(D2R*(DEGROT+240)))
	enddo

 
! print*, size(DvrData%BlTECo,1)
! print*, size(DvrData%BlTECo,2)
! print*, size(DvrData%BlTECo,3)

! open(10,file='A'//trim(num2lstr(nt))//'.bin',access='stream',form='unformatted',status='REPLACE') !open a binary file
! write(10) DvrData%BlTECo
! close(10)

! open(10,file='B'//trim(num2lstr(nt))//'.bin',access='stream',form='unformatted',status='REPLACE') !open a binary file
! write(10) DvrData%BlLECo
! close(10)
	NN%u(1)%BlTECo=DvrData%BlTECo
	NN%u(1)%BlLECo=DvrData%BlLECo

                     
end subroutine Set_NN_Inputs

!----------------------------------------------------------------------------------------------------------------------------------
function GetU( URef, ZRef, PLExp, z ) result (U)
   real(ReKi), intent(in) :: URef
   real(ReKi), intent(in) :: ZRef
   real(ReKi), intent(in) :: PLExp
   real(ReKi), intent(in) :: z
   real(ReKi)             :: U
   
   U = URef*(z/ZRef)**PLExp

end function GetU
!----------------------------------------------------------------------------------------------------------------------------------
subroutine Dvr_InitializeOutputFile( iCase, CaseData, OutFileData, errStat, errMsg)
      type(Dvr_OutputFile),     intent(inout)   :: OutFileData 
      
      integer(IntKi)         ,  intent(in   )   :: iCase                ! case number (to write in file description line and use for file name)
      type(Dvr_Case),           intent(in   )   :: CaseData
      
      integer(IntKi)         ,  intent(inout)   :: errStat              ! Status of error message
      character(*)           ,  intent(inout)   :: errMsg               ! Error message if ErrStat /= ErrID_None

         ! locals
      integer(IntKi)                            ::  i      
      integer(IntKi)                            :: numOuts
      
      
      
      call GetNewUnit( OutFileData%unOutFile, ErrStat, ErrMsg )
         if ( ErrStat >= AbortErrLev ) then
            OutFileData%unOutFile = -1
            return
         end if
         

      call OpenFOutFile ( OutFileData%unOutFile, trim(outFileData%Root)//'.'//trim(num2lstr(iCase))//'.out', ErrStat, ErrMsg )
         if ( ErrStat >= AbortErrLev ) return
         
      write (OutFileData%unOutFile,'(/,A)')  'Predictions were generated on '//CurDate()//' at '//CurTime()//' using '//trim(GetNVD(version))
      write (OutFileData%unOutFile,'(1X,A)') trim(GetNVD(OutFileData%NN_ver))
      write (OutFileData%unOutFile,'()' )    !print a blank line
     ! write (OutFileData%unOutFile,'(A,11(1x,A,"=",ES11.4e2,1x,A))'   ) 'Case '//trim(num2lstr(iCase))//':' &
      write (OutFileData%unOutFile,'(A,11(1x,A,"=",A,1x,A))'   ) 'Case '//trim(num2lstr(iCase))//':' &
         , 'WndSpeed', trim(num2lstr(CaseData%WndSpeed)), 'm/s;' &
         , 'ShearExp', trim(num2lstr(CaseData%ShearExp)), ';' &
         , 'RotSpeed', trim(num2lstr(CaseData%RotSpeed*RPS2RPM)),'rpm;' &
         , 'Pitch',    trim(num2lstr(CaseData%Pitch*R2D)), 'deg;' &
         , 'Yaw',      trim(num2lstr(CaseData%Yaw*R2D)), 'deg;' &
         , 'dT',       trim(num2lstr(CaseData%dT)), 's;' &
         , 'Tmax',     trim(num2lstr(CaseData%Tmax)),'s'
      
      write (OutFileData%unOutFile,'()' )    !print a blank line
              

      numOuts = size(OutFileData%WriteOutputHdr)
         !......................................................
         ! Write the names of the output parameters on one line:
         !......................................................

      call WrFileNR ( OutFileData%unOutFile, '     Time           ' )

      do i=1,NumOuts
        call WrFileNR ( OutFileData%unOutFile, OutFileData%delim//OutFileData%WriteOutputHdr(i) )
      end do ! i


      write (OutFileData%unOutFile,'()')

         !......................................................
         ! Write the units of the output parameters on one line:
         !......................................................

      call WrFileNR ( OutFileData%unOutFile, '      (s)           ' )

      do i=1,NumOuts
         call WrFileNR ( OutFileData%unOutFile, OutFileData%delim//OutFileData%WriteOutputUnt(i) )
      end do ! i

      write (OutFileData%unOutFile,'()')      
      

      
end subroutine Dvr_InitializeOutputFile
!----------------------------------------------------------------------------------------------------------------------------------
subroutine Dvr_WriteOutputLine(OutFileData, t, output, errStat, errMsg)

   real(DbKi)             ,  intent(in   )   :: t                    ! simulation time (s)
   type(Dvr_OutputFile)   ,  intent(in   )   :: OutFileData
   real(ReKi)             ,  intent(in   )   :: output(:)            ! Rootname for the output file
   integer(IntKi)         ,  intent(inout)   :: errStat              ! Status of error message
   character(*)           ,  intent(inout)   :: errMsg               ! Error message if ErrStat /= ErrID_None
      
   ! Local variables.

   character(200)                   :: frmt                                      ! A string to hold a format specifier
   character(15)                    :: tmpStr                                    ! temporary string to print the time output as text
   integer :: numOuts
   
   errStat = ErrID_None
   errMsg  = ''
   numOuts = size(output,1)
   frmt = '"'//OutFileData%delim//'"'//trim(OutFileData%outFmt)      ! format for array elements from individual modules
   
      ! time
   write( tmpStr, '(F15.4)' ) t
   call WrFileNR( OutFileData%unOutFile, tmpStr )
   call WrNumAryFileNR ( OutFileData%unOutFile, output,  frmt, errStat, errMsg )
   if ( errStat >= AbortErrLev ) return
   
     ! write a new line (advance to the next line)
   write (OutFileData%unOutFile,'()')
      
end subroutine Dvr_WriteOutputLine
!----------------------------------------------------------------------------------------------------------------------------------
!----------------------------------------------------------------------------------------------------------------------------------
end module Noise_Driver_Subs

