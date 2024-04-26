rem  echo plot results of case SwapWofost

R  --no-restore --no-save < PlotStress.R > PlotStressR.log
R  --no-restore --no-save < PlotResult.R > PlotResultR.log
R  --no-restore --no-save < contourResultVap.R  > contourResultVap.R.log

pause
exit
