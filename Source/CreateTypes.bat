@ECHO OFF

:: ----------------------------------------------------------------------------
:: ------------------------- LOCAL PATHS --------------------------------------
:: ----------------------------------------------------------------------------
:: -- USERS MAY EDIT THESE PATHS TO POINT TO FOLDERS ON THEIR LOCAL MACHINES. -
:: -- NOTE: do not use quotation marks around the path names!!!! --------------
:: ----------------------------------------------------------------------------
:: ----------------------------------------------------------------------------


IF "%COMPUTERNAME%"=="APLATT-21846S" GOTO APLATT-21846S
IF "%COMPUTERNAME%"=="BJONKMAN-23080S" GOTO BJONKMAN-23080S
IF "%COMPUTERNAME%"=="GHAYMAN-17919S" GOTO GHAYMAN-17919S
IF "%COMPUTERNAME%"=="MBUHL-20665S" GOTO MBUHL-20665S

@SET Registry=..\..\bin\Registry_win32.exe
@SET NWTC_Lib_Loc=.\include
GOTO DoIt


:APLATT-21846S
:: Andy: Put your paths here and delete this comment.
@SET Registry=C:\Users\aplatt\Documents\software_development\windsvn\FAST\branches\FAST_Registry\Registry.exe
@SET NWTC_Lib_Loc=C:\Users\aplatt\Documents\software_development\windsvn\NWTC_Library\trunk\source
GOTO DoIt

:BJONKMAN-23080S
:: Bonnie: Put your paths here and delete this comment.
SET Registry=CALL Registry
SET NWTC_Lib_Loc=C:\Users\bjonkman\Documents\DATA\DesignCodes\miscellaneous\nwtc_subs\SVNdirectory\trunk\source
GOTO DoIt

:GHAYMAN-17919S
@SET Registry=C:\Dev\NREL_SVN\FAST\branches\FAST_Registry\Registry.exe
@SET NWTC_Lib_Loc=C:\Dev\NREL_SVN\NWTC_Library\trunk\source
GOTO DoIt

:MBUHL-20665S
@SET Registry=C:\Users\mbuhl\UserData\CAEtools\Miscellaneous\Registry\Source\Registry.exe
@SET NWTC_Lib_Loc=C:\Users\mbuhl\UserData\CAEtools\Miscellaneous\NWTC_Library\trunk\source
GOTO DoIt


:DoIt
@%REGISTRY% Current.txt -I %NWTC_Lib_Loc%
@COPY Current_Types.f90 .. /Y
@%REGISTRY% Waves.txt -I %NWTC_Lib_Loc%
@COPY Waves_Types.f90 .. /Y
@%REGISTRY% Waves2.txt -I %NWTC_Lib_Loc%
@COPY Waves2_Types.f90 .. /Y
@%REGISTRY% SS_Radiation.txt -I %NWTC_Lib_Loc%
@COPY SS_Radiation_Types.f90 .. /Y
@%REGISTRY% Conv_Radiation.txt -I %NWTC_Lib_Loc%
@COPY Conv_Radiation_Types.f90 .. /Y
@%REGISTRY% WAMIT.txt -I %NWTC_Lib_Loc%
@COPY WAMIT_Types.f90 .. /Y
@%REGISTRY% WAMIT2.txt -I %NWTC_Lib_Loc%
@COPY WAMIT2_Types.f90 .. /Y
@%REGISTRY% Morison.txt -I %NWTC_Lib_Loc%
@COPY Morison_Types.f90 .. /Y
@%REGISTRY% HydroDyn.txt -I %NWTC_Lib_Loc%
@COPY HydroDyn_Types.f90 .. /Y

@SET Registry=
@SET NWTC_Lib_Loc=
