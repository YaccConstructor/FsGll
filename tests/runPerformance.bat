::@ECHO OFF
SET fsgll="..\src\FsGll\bin\Debug\FsGll.exe"
SET logfile="perf.log"
rm %logfile%

::%fsgll% -test=extc-fslex -n=20 -log=%logfile% & goto END

FOR /L %%N IN (20, 4, 28) DO (%fsgll% -test=extc-fslex -n=%%N -log=%logfile%)
FOR /L %%N IN (10, 2, 18) DO (%fsgll% -test=nnn -n=%%N -log=%logfile%)

:END
  type %logfile%
  echo "Finished performace."
