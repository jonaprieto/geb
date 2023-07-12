GEBBIN?=geb
BUILDDIR?=./build
ROS?=ros
INSTALLDIR?=${HOME}/.local/bin/
GEBPACKAGE?=geb.asd

all: build

build: clean
	@${ROS} run --load ${GEBPACKAGE} --eval \
		"(progn \
			(load \"geb.asd\") \
			(make-system))"

docs: clean
	@${ROS} run --load ${GEBPACKAGE} --eval \
		"(progn \
			(load \"geb.asd\") \
			(make-docs) \
			)"

install: build
	@mkdir -p ${INSTALLDIR}
	@mv ${BUILDDIR}/${GEBBIN} ${INSTALLDIR}

uninstall:
	@rm -f ${INSTALLDIR}${GEBBIN}

clean-build:
	@rm -f ${BUILDDIR}/${GEBBIN}

clean-docs:
	@cd docs && rm -f *.html *.css *.js

clean: clean-build clean-docs