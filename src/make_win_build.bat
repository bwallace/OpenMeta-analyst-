call clean_build_win.bat
python gsetup_win.py py2exe --includes sip
cd dist


REM Copy R in to executable
mkdir R_dist
cp -rv "C:\Program Files\R\R-3.0.1" R_dist

REM Copy over sample data
cp -rv ../../sample_data .


REM copy over imageformats folder
cp -rv C:\Python27\Lib\site-packages\PyQt4\plugins\imageformats .


REM copy over launch file
cp ../building/building_in_windows/LaunchOpenMetaAnalyst.bat .