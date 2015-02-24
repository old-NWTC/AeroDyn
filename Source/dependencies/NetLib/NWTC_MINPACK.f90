!**********************************************************************************************************************************
! Copyright (C) 2013-2014  National Renewable Energy Laboratory
!
! This code provides a wrapper for the MinPack routines currently used at the NWTC (mainly codes in the FAST framework).
!
!**********************************************************************************************************************************
! File last committed: $Date: 2014-09-18 15:01:50 -0600 (Thu, 18 Sep 2014) $
! (File) Revision #: $Rev: 257 $
! URL: $HeadURL: https://windsvn.nrel.gov/NWTC_Library/branches/NetLib/NWTC_source/NWTC_MinPack.f90 $
!**********************************************************************************************************************************
module BEMT_MinPack

   use NWTC_Base        ! we only need the precision and error level constants
!   USE, INTRINSIC               :: ISO_C_Binding, only: C_FLOAT, C_DOUBLE          ! this is included in NWTC_Library

      ! Notes:

         ! Your project must include the following files:
         !     NWTC_MinPack.f90
         !     dhybrd1.f
         !     shybrd1.f

   implicit  none

   interface BEMT_Hybrd1  ! sort
      !module procedure MinPack_dHybrd1
      module procedure BEMT_sHybrd1
   end interface   
   
   

contains

!=======================================================================
!INTERFACE MinPack_Hybrd1:
!  NEED DESCRIPTION HERE.
!  See documentation in  d/shybrd1 source code.
   subroutine BEMT_dHybrd1( ID, N, D, KEY, ErrStat, ErrMsg )
          
      ! passed parameters
 
      CHARACTER(1),    intent(in   ) :: ID                ! = 'I': sort D in increasing order; = 'D': sort D in decreasing order.
      INTEGER,         intent(in   ) :: N                 ! The length of the array D.
      INTEGER(IntKi),  intent(  out) :: ErrStat           ! Error level 
      CHARACTER(*),    intent(  out) :: ErrMsg            ! Message describing error

      !     .. Array Arguments ..
      INTEGER,         intent(inout) :: KEY( : )
      DOUBLE PRECISION,intent(inout) :: D( : )            ! On entry, the array to be sorted. On exit, D has been sorted into increasing/decreasing order, depending on ID

         ! Local variable
      INTEGER                        :: INFO              ! = 0:  successful exit; < 0:  if INFO = -i, the i-th argument had an illegal value 
      
      
      ErrStat = ErrID_None
      ErrMsg  = ""
      
      !CALL DLASRT2( ID, N, D, KEY, INFO )
                
      IF (INFO /= 0) THEN
         ErrStat = ErrID_FATAL
         WRITE( ErrMsg, * ) INFO
         IF (INFO < 0) THEN
            ErrMsg  = "MinPack_DLSRT2: illegal value in argument "//TRIM(ErrMsg)//"."
         ELSE
            ErrMsg = 'MinPack_DLSRT2: Unknown error '//TRIM(ErrMsg)//'.'
         END IF
      END IF      
 
      
   return
   end subroutine BEMT_dHybrd1
   
   
!=======================================================================
   subroutine BEMT_sHybrd1( x, fvec, tol, fcnArgs, errStat, errMsg )
      
      ! passed parameters
 
      
      !integer,               intent(in   ) :: lwa
      real,                   intent(in   ) :: tol
      real,                   intent(inout) :: x(2)     ! n long
      real,                   intent(  out) :: fvec(2)  ! n long
      external fcnArgs
      !real,                  intent(in   ) :: wa(:)    ! lwa long
      
      
      integer(IntKi), intent(  out) :: ErrStat           ! Error level 
      character(*),   intent(  out) :: ErrMsg            ! Message describing error

      
         ! Local variable
      integer                       :: info              ! = 0:  successful exit; < 0:  if INFO = -i, the i-th argument had an illegal value 
      real, allocatable             :: wa(:)
      integer                       :: lwa
      integer, parameter            :: n = 2      ! hardcoded to 2 for BEMT_Coupled solution
!      wa is a work array of length lwa.
!
!       lwa is a positive integer input variable not less than
!         (n*(3*n+13))/2.
      
      errStat = ErrID_None
      errMsg  = ""
      
      lwa = ceiling((n*(3*n+13))/2.)
      allocate( wa(lwa), STAT = errStat )
      if (errStat /= 0 ) then
         errStat = errID_FATAL
         errMsg  = "MinPack_sHybrd1: could not allocate array wa"
         return
      end if
      
      call sHybrd1( n, x, fvec, tol, info, wa, lwa, fcnArgs)
                
      if (info /= 1) then
         errStat = errID_FATAL
         write( errMsg, * ) info
         if (info < 0) then
            errMsg  = "MinPack_sHybrd1: illegal value in argument "//trim(errMsg)//"."
         else
            errMsg = 'MinPack_sHybrd1: Unknown error '//trim(errMsg)//'.'
         end if
      end if      
 
      deallocate(wa)
      
      return
   end subroutine BEMT_sHybrd1
!=======================================================================
end module BEMT_MinPack
