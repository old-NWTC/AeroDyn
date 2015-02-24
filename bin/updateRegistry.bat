svn export --force --depth files "https://windsvn.nrel.gov/FAST/branches/FAST_Registry" "./temp"
cd temp
move registry.exe ..
del /q *.*
cd ..
rmdir temp
@IF EXIST Registry_win32.exe DEL Registry_win32.exe
rename registry.exe Registry_win32.exe
