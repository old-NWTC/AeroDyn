cd "test cases"
..\bin\WT_Perf.exe skewWakeCorr.wtp skewWakeCorr
move skewWakeCorr.WTP.out ..\..\..\data\WT_Perf\skewWakeCorr.out

..\bin\WT_Perf.exe noSkewWakeCorr.wtp noSkewWakeCorr
move noSkewWakeCorr.WTP.out ..\..\..\data\WT_Perf\noSkewWakeCorr.out

..\bin\WT_Perf.exe coupledCorr.wtp coupledCorr
move coupledCorr.WTP.out ..\..\..\data\WT_Perf\coupledCorr.out

cd ..