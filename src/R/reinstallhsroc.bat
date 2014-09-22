REM reinstall custom hsroc script to run on windows

rm HSROC_2.0.5.tar.gz
R CMD build HSROC
R CMD INSTALL HSROC_2.0.5.tar.gz

