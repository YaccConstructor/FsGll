@ECHO OFF
::SET fsgll="..\src\FsGll\bin\Debug\FsGll.exe"
SET fsgll="..\src\FsGll\bin\Release\FsGll.exe"
SET logfile="perf.log"

IF NOT "%1"=="chart" GOTO RUN

%fsgll% -test=chart-simple -n=nnn,nnn-pure -log=%logfile% & goto END
::%fsgll% -test=extc-fslex -n=20 -log=%logfile% & goto END

:RUN

del %logfile%

::FOR /L %%N IN (20, 4, 24) DO (%fsgll% -test=extc-fslex -n=%%N -log=%logfile%)
FOR /L %%N IN (80, 5, 200) DO (%fsgll% -test=nnn -n=%%N -log=%logfile%)
FOR /L %%N IN (80, 5, 200) DO (%fsgll% -test=nnn-pure -n=%%N -log=%logfile%)

:END
  echo %logfile% contents:
  type %logfile%
  echo "Finished performace."
