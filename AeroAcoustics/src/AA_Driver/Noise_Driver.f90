!**********************************************************************************************************************************
! LICENSING
! Copyright (C) 2015-2016  National Renewable Energy Laboratory
!
!    This file is part of Noise Module.
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
program Noise_Driver
  
     use Noise_Driver_Subs   
  
   implicit none   
   
   ! Program variables
  
	real(DbKi)                                     :: time                 !< Variable for storing time, in seconds 
	real(DbKi)                                     :: dT_Dvr               !< copy of DT, to make sure NN didn't change it
													
	type(Dvr_SimData)                              :: DvrData              ! The data required for running the NN driver
	type(Noise_Data)                               :: NN                    ! Noise Module data 
												
	integer(IntKi)                                 :: iCase                ! loop counter (for driver case)
	integer(IntKi)                                 :: nt                   ! loop counter (for time step)
	integer(IntKi)                                 :: j                    ! loop counter (for array of inputs)
	integer(IntKi)                                 :: numSteps             ! number of time steps in the simulation
	integer(IntKi)                                 :: errStat              ! Status of error message
	character(ErrMsgLen)                           :: errMsg               ! Error message if ErrStat /= ErrID_None
	
	logical                                        :: NN_Initialized

	errStat     = ErrID_None
	errMsg      = ''
	NN_Initialized = .false.
	
	time        = 0.0 ! seconds
!**********************************************************************************************************************************!
! Initialize the driver: writes some messages on the screen, reads in the '.dvr' and '.blade1.dat' and  validates the input (not fully) ! 
!**********************************************************************************************************************************!
    call Dvr_Init( DvrData, ErrStat, ErrMsg)  
    call CheckError()

!**********************************************************************************************************************************!
! Loop over each case that is input in .dvr file ! 
!**********************************************************************************************************************************!    
    do iCase = 1, DvrData%NumCases
     call WrScr( NewLine//'Running case '//trim(num2lstr(iCase))//' of '//trim(num2lstr(DvrData%NumCases))//'.' )
!dT = TwoPi/DvrData%Cases(iCase)%RotSpeed / DvrData%NumSect ! sec ! EB_DTU: this is commented but i think it can be used 
      numSteps = ceiling( DvrData%Cases(iCase)%TMax / DvrData%Cases(iCase)%dT)      
      dT_Dvr   = DvrData%Cases(iCase)%dT
	call WrScr ('   WndSpeed='//trim(num2lstr(DvrData%Cases(iCase)%WndSpeed))//&
	  	    ' m/s; ShearExp='//trim(num2lstr(DvrData%Cases(iCase)%ShearExp))//&
  	  	    '; RotSpeed='//trim(num2lstr(DvrData%Cases(iCase)%RotSpeed*RPS2RPM))//&
	  	    ' rpm; Pitch='//trim(num2lstr(DvrData%Cases(iCase)%Pitch*R2D))//&
	  	    ' deg; Yaw='//trim(num2lstr(DvrData%Cases(iCase)%Yaw*R2D))//&
	  	    ' deg; dT='//trim(num2lstr(DvrData%Cases(iCase)%dT))//&
 		    ' s; Tmax='//trim(num2lstr(DvrData%Cases(iCase)%Tmax))//&
	     	    ' s; numSteps='//trim(num2lstr(numSteps)) )
!**********************************************************************************************************************************!
! Set the Initialization input data for Noise based on the Driver input file data, and initialize NN
! (this also initializes inputs to NN for first time step)
!**********************************************************************************************************************************!   
     call Init_Noise(iCase, DvrData, NN, dT_Dvr, errStat, errMsg)
         call CheckError()
         NN_Initialized = .true.

        if (.not. EqualRealNos( dT_Dvr, DvrData%Cases(iCase)%dT ) ) then
            ErrStat = ErrID_Fatal
            ErrMsg = 'Noise changed the time step for case '//trim(num2lstr(iCase))//'. Change DTAero to "default".'
            call CheckError()
         end if
!**********************************************************************************************************************************!
! Opens and initializes an output file for the results (out)
!**********************************************************************************************************************************!
      call Dvr_InitializeOutputFile( iCase, DvrData%Cases(iCase), DvrData%OutFileData, errStat, errMsg)
         call CheckError()
      do nt = 1, numSteps ! Start Time Stepping
!**********************************************************************************************************************************!
! Rotates the blade and updates the velocities (only horizontal velocity) 
!**********************************************************************************************************************************!      
        call Set_NN_Inputs(iCase,nt,DvrData,NN,errStat,errMsg)
          call CheckError()
!**********************************************************************************************************************************!
! Takes the updated flow and the blade positions and calculates the noise, prepares the output
!**********************************************************************************************************************************! 
  	  !print*, time
	  time = NN%InputTime(2)

        call NN_CalcOutput( time, NN%u(1), NN%p, NN%x, NN%xd, NN%z, NN%OtherState, NN%y, NN%m, errStat, errMsg,nt )
            call CheckError()
!**********************************************************************************************************************************!
! Writes out the selected channels
!**********************************************************************************************************************************!  
         call Dvr_WriteOutputLine(DvrData%OutFileData, time, NN%y%WriteOutput, errStat, errMsg)
            call CheckError()         
!**********************************************************************************************************************************!
! Update States Vrel2,MeanVrel,TI
!**********************************************************************************************************************************!  
         call NN_UpdateStates( time, nt-1, NN%u(1), NN%InputTime,NN%p, NN%xd, errStat, errMsg )
            call CheckError()
                
      end do !nt=1,numSteps
!**********************************************************************************************************************************!
! Ends and destroys for each case
!**********************************************************************************************************************************!     
      call NN_End( NN%u(1), NN%p, NN%x, NN%xd, NN%z, NN%OtherState, NN%y, NN%m, errStat, errMsg )
         NN_Initialized = .false.         
         call CheckError()
         close( DvrData%OutFileData%unOutFile )
               
      do j = 2, numInp
         call NN_DestroyInput (NN%u(j),  errStat, errMsg)
           call CheckError()
      end do

  end do !iCase = 1, DvrData%NumCases
!!!   
!!!   
   call Dvr_End()
   
contains
!**********************************************************************************************************************************!     
!**********************************************************************************************************************************!     
   subroutine CheckError()
   
      if (ErrStat /= ErrID_None) then
         call WrScr(TRIM(ErrMsg))
         
         if (ErrStat >= AbortErrLev) then
            call Dvr_End()
         end if
      end if
         
   end subroutine CheckError
!**********************************************************************************************************************************!     
!**********************************************************************************************************************************!     
   subroutine Dvr_End()
   
         ! Local variables
      character(ErrMsgLen)                          :: errMsg2                 ! temporary Error message if ErrStat /= ErrID_None
      integer(IntKi)                                :: errStat2                ! temporary Error status of the operation
      
      character(*), parameter                       :: RoutineName = 'Dvr_End'
         ! Close the output file
      if (DvrData%OutFileData%unOutFile > 0) close(DvrData%OutFileData%unOutFile)
            
      if ( NN_Initialized ) then
         call NN_End( NN%u(1), NN%p, NN%x, NN%xd, NN%z, NN%OtherState, NN%y, NN%m, errStat2, errMsg2 )
         call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
      end if
           
      call NN_Dvr_DestroyDvr_SimData( DvrData, ErrStat2, ErrMsg2 )
         call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )

      call NN_Dvr_DestroyNoise_Data( NN, ErrStat2, ErrMsg2 )
         call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
               
      if (ErrStat >= AbortErrLev) then      
         CALL ProgAbort( 'Noise Driver encountered simulation error level: '&
             //TRIM(GetErrStr(ErrStat)), TrapErrors=.FALSE., TimeWait=3._ReKi )  ! wait 3 seconds (in case they double-clicked and got an error)
      else
         call NormStop()
      end if
      
      
   end subroutine Dvr_End
!**********************************************************************************************************************************!     
!**********************************************************************************************************************************!     
end program Noise_Driver		 
		
