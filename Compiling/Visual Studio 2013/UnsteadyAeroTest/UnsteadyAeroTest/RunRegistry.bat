@ECHO OFF

set lines=======================================================================
echo %lines%
IF "%1"=="" (
ECHO.
ECHO   The calling syntax for this script is
ECHO             RunRegistry ModuleName
ECHO.
GOTO Done
)


REM ----------------------------------------------------------------------------
REM ------------------------- LOCAL PATHS --------------------------------------
REM ----------------------------------------------------------------------------
REM -- USERS MAY EDIT THESE PATHS TO POINT TO FOLDERS ON THEIR LOCAL MACHINES. -
REM -- NOTE: do not use quotation marks around the path names!!!! --------------
REM ----------------------------------------------------------------------------
REM ----------------------------------------------------------------------------

SET Registry=..\..\..\..\bin\Registry_win32.exe
SET Source_Loc=..\..\..\..\Source

SET NWTC_Lib_Loc=%Source_Loc%\dependencies\NWTC_Library
SET AirfoilInfo_Loc=%Source_Loc%\dependencies\Airfoil_Info
SET UnsteadyAeroInfo_Loc=%Source_Loc%\dependencies\UnsteadyAero



IF /I "%2"=="bjonkman" CALL ..\Set_FAST_paths.bat

SET ModuleName=%1

IF "%ModuleName%"=="UnsteadyAero" GOTO UnsteadyAero
IF "%ModuleName%"=="AirfoilInfo" GOTO AFI

REM ----------------------------------------------------------------------------
REM ---------------- RUN THE REGISTRY TO AUTO-GENERATE FILES -------------------
REM ----------------------------------------------------------------------------

:UnsteadyAero
SET CURR_LOC=%Source_Loc%\dependencies\UnsteadyAero
%REGISTRY% "%CURR_LOC%\UnsteadyAero_Registry.txt" -I %NWTC_Lib_Loc% -I %AirfoilInfo_Loc%
GOTO checkError

:AFI
SET CURR_LOC=%Source_Loc%\dependencies\Airfoil_Info
%REGISTRY% "%CURR_LOC%\AirfoilInfo_Registry.txt" -I %NWTC_Lib_Loc% 
GOTO checkError

:checkError
ECHO.
IF %ERRORLEVEL% NEQ 0 (
ECHO Error running  Registry for %ModuleName%.
) ELSE (
ECHO %ModuleName%_Types.f90 was created.
COPY /Y "%ModuleName%_Types.f90" "%CURR_LOC%"
)




:end
REM ----------------------------------------------------------------------------
REM ------------------------- CLEAR MEMORY -------------------------------------
REM ----------------------------------------------------------------------------
ECHO. 


SET REGISTRY=

SET NWTC_Lib_Loc=
SET Source_Loc=

SET CURR_LOC=
:Done
echo %lines%
set lines=