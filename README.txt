+++++++++++++++++++++++++++++
+                           +
+       OpenMetaAnalyst     +
+                           +
+++++++++++++++++++++++++++++

To run OpenMetaAnalyst from source, you'll need to install the corresponding dependencies. If you're interested in simply running the program (i.e., not from source) consider grabbing a binary build (for Windows and Mac OS X) from: 

    http://tuftscaes.org/open_meta/download.html

Otherwise, you'll need to install the necessary R packages. To this end, you'll need to build and install the openmetar library and its dependencies. This package is distributed with the source under the "R" directory. 

    > R CMD BUILD openmetar
    > R CMD INSTALL openmetar

For compatibility reasons, we use the slightly dated R 2.10. 

Once R is setup for OpenMeta, you'll need to install Python (we use 2.7) and the necessary libraries. You'll need PyQT (and QT: see http://www.riverbankcomputing.co.uk/software/pyqt/intro) installed -- we use PyQt 4.9; your mileage may vary with other versions. 

Next, install rpy2 (rpy.sourceforge.net/rpy2.html) in Python. Verify that all is well by executing:

    > import rpy2
    > from rpy2 import robjects 

At the Python console.

That should be all you need. Once everything is setup, you can launch the program by typing:

    > python meta_form.py

At the console. This should fire up the GUI.