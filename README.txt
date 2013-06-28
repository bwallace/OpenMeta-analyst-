+++++++++++++++++++++++++++++
+                           +
+       OpenMetaAnalyst     +
+                           +
+++++++++++++++++++++++++++++

To run OpenMetaAnalyst from source, you'll need to install the corresponding dependencies. If you're interested in simply running the program (i.e., not from source) consider grabbing a binary build (for Windows and Mac OS X) from: 

    http://www.cebm.brown.edu/open_meta

Otherwise, you'll need to install the necessary R packages. 
First install the dependencies:
From within a sudo-ed R session type:

	> install.packages(c("metafor","lme4","MCMCpack","igraph"))


	
Next, you'll need to build and install the openmetar packages and altered HSROC (NOT THE ONE FROM CRAN) package and install them. These package are distributed with the source under the "src/R" directory. 

    > R CMD build HSROC
    > R CMD build openmetar
    > sudo R CMD INSTALL HSROC_2.0.5.tar.gz
    > sudo R CMD INSTALL openmetar_1.0.tar.gz

On this branch, we have moved to R 3.0.1!

Once R is setup for OpenMeta, you'll need to install Python (we use 2.7) and the necessary libraries. You'll need PyQT (and QT: see http://www.riverbankcomputing.co.uk/software/pyqt/intro) installed -- we use PyQt 4.9; your mileage may vary with other versions. 

Next, install rpy2 (rpy.sourceforge.net/rpy2.html) in Python. Verify that all is well by executing:

    > import rpy2
    > from rpy2 import robjects 

At the Python console.

That should be all you need. Once everything is setup, you can launch the program by typing:

    > python src/launch.py

At the console. This should fire up the GUI.

For running nosetests:
these should be run from within the src folder:
nosetests -v test_meta_analysis.py

important dependency versions:
R      : 3.0.1 (2013-05-16) -- "Good Sport"
metafor: 1.6.0
pyqt4  : 4.10.1
