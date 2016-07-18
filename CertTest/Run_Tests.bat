set DriverProg=..\bin\AeroDyn_Driver_Win32.exe

%DriverProg% AOC_Test01.dvr
%DriverProg% AOC_Test02.dvr
%DriverProg% AOC_Test03.dvr
:: move AOC_Test*.out .\Results


%DriverProg% NRELOffshrBsline5MW_Onshore_Test01.dvr
%DriverProg% NRELOffshrBsline5MW_Onshore_Test02.dvr
%DriverProg% NRELOffshrBsline5MW_Onshore_Test03.dvr
:: move NRELOffshrBsline5MW_Onshore_Test*.out .\Results\

set DriverProg=