GEBBIN?=geb
BUILDDIR?=./bin
SHELL?=/bin/bash
ROS?=ros
INSTALLDIR?=${HOME}/.local/bin/
GEBPACKAGE?=geb.asd
ROSFLAGS?=--load ${GEBPACKAGE}

all: build

build: clean
	@${ROS} run ${ROSFLAGS} --eval \
		"(progn \
			(load \"geb.asd\") \
			(make-system))"

docs: clean
	@${ROS} run ${ROSFLAGS} --eval \
		"(progn \
			(load \"geb.asd\") \
			(make-docs))"

install: build
	@mkdir -p ${INSTALLDIR}
	@mv ${BUILDDIR}/${GEBBIN} ${INSTALLDIR}

uninstall:
	@rm -f ${INSTALLDIR}${GEBBIN}


format:
	find . -name '*.lisp' | xargs ${ROS} fmt

clean-build:
	@rm -f ${BUILDDIR}/${GEBBIN}

clean-docs:
	@cd docs && rm -f *.html *.css *.js

clean: clean-build clean-docs
	@find . -name "*.fasl" -type f -delete