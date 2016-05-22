::@ECHO OFF
::SET fsgll="..\src\FsGll.Examples\bin\Debug\FsGll.Examples.exe"
SET fsgll="..\src\FsGll.Examples\bin\Release\FsGll.Examples.exe"
SET scalagll="C:\Users\melenk\IdeaProjects\GLL1\out\artifacts\GLL1_jar\GLL1.jar"
SET scalagll=java -jar %scalagll%
SET logfile="perf.log"
SET ecpath="%~dp0ECTests"

IF NOT "%1"=="genec" GOTO NOTGENEC
  %fsgll% -test=generate-ec -n=200 -log=%logfile% -ecpath=%ecpath% & goto END

:NOTGENEC
IF NOT "%1"=="chart" GOTO RUN

::type perf.log.r2 > perf.log
::type perf.log.r3 >> perf.log
::type perf.log.r4 >> perf.log

%fsgll% -test=chart-simple -n=nnn,nkx -log=%logfile% & goto END
::%fsgll% -test=chart-simple -n=extc-fslex,extc-fparsec -log=%logfile% & goto END

::%fsgll% -test=chart-simple -n=scala-extc,extc,extc-fparsec -log=%logfile% & goto END
::%fsgll% -test=chart-simple -n=scala-nnn -log=%logfile% & goto END
::%fsgll% -test=extc-fslex -n=20 -log=%logfile% & goto END
goto END

:RUN

FOR /L %%N IN (10, 2, 70) DO (%fsgll% -test=nnn -n=%%N -log=%logfile%  &  %fsgll% -test=nnn -n=%%N -log=%logfile%  &  %fsgll% -test=nnn -n=%%N -log=%logfile%)

::del %logfile%

::FOR /L %%N IN (22, 2, 24) DO (%fsgll% -test=nnn-incr-pure -n=%%N -log=%logfile% -ecpath=%ecpath%  &  %fsgll% -test=nnn-incr-pure -n=%%N -log=%logfile% -ecpath=%ecpath%)

::FOR /L %%N IN (40, 2, 70) DO (%fsgll% -test=extc-fslex -n=%%N -log=%logfile% -ecpath=%ecpath%  &  %fsgll% -test=extc-fslex -n=%%N -log=%logfile% -ecpath=%ecpath%)
::FOR /L %%N IN (40, 2, 70) DO (%fsgll% -test=extc-fparsec -n=%%N -log=%logfile% -ecpath=%ecpath%  &  %fsgll% -test=extc-fparsec -n=%%N -log=%logfile% -ecpath=%ecpath%)


::%fsgll% -test=extc-fparsec -n=60 -log=%logfile% -ecpath=%ecpath%
::%fsgll% -test=extc-fslex -n=60 -log=%logfile% -ecpath=%ecpath%

::FOR /L %%N IN (20, 2, 38) DO (%scalagll% -test=scala-nnn -n=%%N -log=%logfile%  &  %scalagll% -test=scala-nnn -n=%%N -log=%logfile%)
::FOR /L %%N IN (8, 2, 18) DO (%scalagll% -test=scala-nnn -n=%%N -log=%logfile%  &  %scalagll% -test=scala-nnn -n=%%N -log=%logfile%)
::FOR /L %%N IN (8, 2, 18) DO (%fsgll% -test=nnn -n=%%N -log=%logfile%  &  %fsgll% -test=nnn -n=%%N -log=%logfile%)


::FOR /L %%N IN (1, 1, 20) DO (%scalagll% -test=scala-extc -n=%%N -log=%logfile% -ecpath=%ecpath%  &  %scalagll% -test=scala-extc -n=%%N -log=%logfile% -ecpath=%ecpath%)
::FOR /L %%N IN (1, 1, 20) DO (%fsgll% -test=extc-fparsec -n=%%N -log=%logfile% -ecpath=%ecpath%  &  %fsgll% -test=extc-fparsec -n=%%N -log=%logfile% -ecpath=%ecpath%)
::FOR /L %%N IN (1, 1, 20) DO (%fsgll% -test=extc -n=%%N -log=%logfile% -ecpath=%ecpath%  &  %fsgll% -test=extc -n=%%N -log=%logfile% -ecpath=%ecpath%)



::FOR /L %%N IN (10, 4, 110) DO (%scalagll% -test=scala-nnn -n=%%N -log=%logfile%  &  %scalagll% -test=scala-nnn -n=%%N -log=%logfile%)

::FOR /L %%N IN (10, 4, 110) DO (%fsgll% -test=nnn-pure -n=%%N -log=%logfile%  &  %fsgll% -test=nnn-pure -n=%%N -log=%logfile%)
::FOR /L %%N IN (110, 4, 150) DO (%scalagll% -test=scala-nnn -n=%%N -log=%logfile%  &  %scalagll% -test=scala-nnn -n=%%N -log=%logfile%)
::FOR /L %%N IN (110, 4, 150) DO (%fsgll% -test=nnn -n=%%N -log=%logfile%  &  %fsgll% -test=nnn -n=%%N -log=%logfile%)
::FOR /L %%N IN (110, 4, 150) DO (%fsgll% -test=nnn-pure -n=%%N -log=%logfile%  &  %fsgll% -test=nnn-pure -n=%%N -log=%logfile%)

::FOR /L %%N IN (80, 5, 200) DO (%fsgll% -test=nnn-pure -n=%%N -log=%logfile%)
::FOR /L %%N IN (20, 4, 24) DO (%fsgll% -test=extc-fslex -ecpath=%ecpath% -n=%%N -log=%logfile%)
goto END
  
:GENRAND
  setlocal ENABLEEXTENSIONS ENABLEDELAYEDEXPANSION
  set alfanum=ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789

  set pwd=
  FOR /L %%b IN (0, 1, 16) DO (
  SET /A rnd_num=!RANDOM! * 62 / 32768 + 1
  for /F %%c in ('echo %%alfanum:~!rnd_num!^,1%%') do set pwd=!pwd!%%c
  )

echo pwd=%pwd%

:END
  echo %logfile% contents:
  type %logfile%
  echo "Finished performace."