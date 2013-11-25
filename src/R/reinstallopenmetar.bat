REM reinstall openmetar script to run on windows

rm openmetar_1.0.tar.gz
R CMD build openmetar
R CMD INSTALL openmetar_1.0.tar.gz
