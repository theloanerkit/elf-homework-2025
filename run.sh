#!/bin/bash

if [[ $2 -eq 1]];
	# run with test inputs
	cd test_inputs
else
	# run with actual inputs
	cd inputs
fi

# run python parser for inputs
python3 ../parser.py "$1"


cd ..
cd fortran

# now compile the fortran
gfortran -c "$1.f90"
gfortran "$1".o -o "$1".out

# now run
./"$1".out
