# Histogram
A FORTRAN90 code to Caluculate Histogram (Distribution ) for a given data

Here I have caluculate a Data git from one of my repository above BOX MULLAR SAMPLING. 
DISTRIBUTION.dat 

U have to use follow these to run this histogram code

gfortran histogram.f90

./a.out

#N      xmin  xmax   width  input_filename      output_filename

10000  -4.0   4.0    0.5     DISTRIBUTION.dat   out.dat

then, it will genarate a file called out.dat, that contains distribution data.

plot that data by using any plotting package (gnuplot, xmgrace ..etc.)

then u will see a distribution like attached pic. out.jpg.


THANKS,
ANJI BABU,
IIT KANPUR,
INDIA.
