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

SET Registry=..\..\..\bin\Registry_win32.exe
SET Source_Loc=..\..\..\Source

SET NWTC_Lib_Loc=%Source_Loc%\dependencies\NWTC_Library
SET AirfoilInfo_Loc=%Source_Loc%\dependencies\Airfoil_Info
SET BEMT_Loc=%Source_Loc%\dependencies\BEMT
SET UnsteadyAero_Loc=%Source_Loc%\dependencies\UnsteadyAero
SET AeroDyn_Loc=%Source_Loc%\dependencies\AeroDyn
SET Driver_Loc=%Source_Loc%


IF /I "%2"=="dev" CALL ..\Set_FAST_paths.bat

SET ModuleName=%1

GOTO %ModuleName%

REM ----------------------------------------------------------------------------
REM ---------------- RUN THE REGISTRY TO AUTO-GENERATE FILES -------------------
REM ----------------------------------------------------------------------------

:UnsteadyAero
SET CURR_LOC=%UnsteadyAero_Loc%
%REGISTRY% "%CURR_LOC%\UnsteadyAero_Registry.txt" -I %NWTC_Lib_Loc% -I %AirfoilInfo_Loc% -O %CURR_LOC%
GOTO checkError

:BEMT
SET CURR_LOC=%BEMT_Loc%
%REGISTRY% "%CURR_LOC%\BEMT_Registry.txt" -I %NWTC_Lib_Loc% -I %AirfoilInfo_Loc% -I %UnsteadyAero_Loc% -O %CURR_LOC%
GOTO checkError

:AeroDyn
SET CURR_LOC=%AeroDyn_Loc%
%REGISTRY% "%CURR_LOC%\AeroDyn_Registry.txt" -I %NWTC_Lib_Loc% -I %AirfoilInfo_Loc% -I %BEMT_Loc% -I %UnsteadyAero_Loc% -O %CURR_LOC%
GOTO checkError

:AirfoilInfo
SET CURR_LOC=%AirfoilInfo_Loc%
%REGISTRY% "%CURR_LOC%\AirfoilInfo_Registry.txt" -I %NWTC_Lib_Loc% -O %CURR_LOC% -noextrap
GOTO checkError

:AeroDyn_Driver
SET CURR_LOC=%Driver_Loc%
%REGISTRY% "%CURR_LOC%\AeroDyn_Driver_Registry.txt" -I %NWTC_Lib_Loc% -I %AeroDyn_Loc% -I %AirfoilInfo_Loc% -I %BEMT_Loc% -I %UnsteadyAero_Loc% -O %CURR_LOC% -noextrap
GOTO checkError



:checkError
ECHO.
IF %ERRORLEVEL% NEQ 0 (
ECHO Error running  Registry for %ModuleName%.
) ELSE (
ECHO %ModuleName%_Types.f90 was created.
)




:end
REM ----------------------------------------------------------------------------
REM ------------------------- CLEAR MEMORY -------------------------------------
REM ----------------------------------------------------------------------------
ECHO. 


SET Registry=
SET Source_Loc=
SET NWTC_Lib_Loc=
SET AirfoilInfo_Loc=
SET BEMT_Loc=
SET UnsteadyAero_Loc=
SET AeroDyn_Loc=
SET Driver_Loc=

SET ModuleName=
SET CURR_LOC=
:Done
echo %lines%
set lines=