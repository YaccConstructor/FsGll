::@ECHO OFF
SET fsgll="..\src\FsGll\bin\Debug\FsGll.exe"
SET logfile="perf.log"
rm %logfile%
%fsgll% -test=extc-fslex -n=5 -log=%logfile%
goto END

FOR /L %%N IN (10, 2, 14) DO (
  %fsgll% -test=nnn -n=%%N -log=%logfile%
  %fsgll% -test=extc-fslex -n=%%N -log=%logfile%
)

:END
  type %logfile%
  echo "Finished performace."
