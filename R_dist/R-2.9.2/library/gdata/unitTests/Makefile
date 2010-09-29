TOP=../..
PKG=${shell cd ${TOP};pwd}
SUITE=runRUnitTests.R
R=R

test: # Run unit tests
	${R} --vanilla --slave < ${SUITE}

inst: # Install package
	cd ${TOP}/..;\
	${R} CMD INSTALL ${PKG}

all: inst test

echo: # Echo env. variables
	@echo "Package folder: ${PKG}"
	@echo "R binary: ${R}"

help: # Help
	@echo -e '\nTarget: Dependency # Description'; \
	echo '=================================================='; \
	egrep '^[[:alnum:].+_()%]*:' ./Makefile
