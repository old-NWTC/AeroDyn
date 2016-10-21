!  DBEMT_Test.f90 
!
!  FUNCTIONS:
!  DBEMT_Test - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: DBEMT_Test
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

program DBEMT_Test
   use DBEMT
   use NWTC_Library
   
   implicit none

   ! Variables
   integer(IntKi)                  :: errStat       !< Error status of the operation
   character(ErrMsgLen)            :: errMsg        !< Error message if ErrStat /= ErrID_None
   logical                         :: result
      ! Initialize the NWTC Subroutine Library

   call NWTC_Init( EchoLibVer=.FALSE. )
   
   ! Body of DBEMT_Test
   print *, 'Beginning DBEMT Testing'
   errStat = ErrID_None
   errMsg  = ""
   
   result =  DBEMT_TEST01(errStat, errMsg)
   print *, 'Test01: ', result
#ifdef PRINT_ERRMSG
   print *, trim(errMsg)
   print *
#endif

   result =  DBEMT_TEST02(errStat, errMsg)
   print *, 'Test02: ', result
#ifdef PRINT_ERRMSG
   print *, trim(errMsg)
   print *
#endif

result =  DBEMT_TEST03(errStat, errMsg)
   print *, 'Test03: ', result
#ifdef PRINT_ERRMSG
   print *, trim(errMsg)
   print *
#endif

   
   result =  DBEMT_TEST04(errStat, errMsg)
   print *, 'Test04: ', result
#ifdef PRINT_ERRMSG
   print *, trim(errMsg)
   print *
#endif

   
   result =  DBEMT_TEST05(errStat, errMsg)
   print *, 'Test05: ', result
#ifdef PRINT_ERRMSG
   print *, trim(errMsg)
   print *
#endif

   
   result =  DBEMT_TEST06(errStat, errMsg)
   print *, 'Test06: ', result
#ifdef PRINT_ERRMSG
   print *, trim(errMsg)
   print *
#endif

   
   result =  DBEMT_TEST07(errStat, errMsg)
   print *, 'Test07: ', result
#ifdef PRINT_ERRMSG
   print *, trim(errMsg)
   print *
#endif
   
   result =  DBEMT_TEST08(errStat, errMsg)
   print *, 'Test08: ', result
#ifdef PRINT_ERRMSG
   print *, trim(errMsg)
   print *
#endif

result =  DBEMT_TEST09(errStat, errMsg)
   print *, 'Test09: ', result
#ifdef PRINT_ERRMSG
   print *, trim(errMsg)
   print *
#endif

result =  DBEMT_TEST10(errStat, errMsg)
   print *, 'Test10: ', result
#ifdef PRINT_ERRMSG
   print *, trim(errMsg)
   print *
#endif

result =  DBEMT_TEST11(errStat, errMsg)
   print *, 'Test11: ', result
#ifdef PRINT_ERRMSG
   print *, trim(errMsg)
   print *
#endif


   end program DBEMT_Test

