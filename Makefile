GEBBIN?=geb
BUILDDIR?=./build
ROS?=ros
INSTALLDIR?=${HOME}/.local/bin/

all: build

build: clean
	${ROS} run --load "geb.asd" --eval "(progn (load \"geb.asd\") (make-system))"

install: build
	@mkdir -p ${INSTALLDIR}
	@mv ${BUILDDIR}/${GEBBIN} ${INSTALLDIR}

uninstall:
	@rm -f ${INSTALLDIR}${GEBBIN}

clean:
	@rm -f ${BUILDDIR}/${GEBBIN}
