The commands to compile and run the programs are shown below. Note that to compile and run these programs, 
an ubuntu system with OPAM is required.

Required packages:
yojson
str

To compile the more complex problem (X^4 + X^3 + X^2 + X):

corebuild -I utilities/ -pkg yojson -pkg str test_complex.byte 

To run afterwards:
./test_complex.byte

To compile the more simple problem (x + cos(x)): 

corebuild -I utilities/ -pkg yojson -pkg str test_simple.byte

To run afterwards:
./test_simple.byte
