#
# I consider the odds of this script working as slim, but at a minimum it gives you a roadmap 
# for where you have to compile all the codes
# 
cd ./TJO_Fortran_Library/fortran95
make tjolib
make install
make clean
cd ../..
#
cd ./Seismic_Utilities
make all
make install
make clean
cd ..
#
cd ./Rcvr_Func_Codes
make iterdecon
make pwaveqn
make install
make clean
cd ./Ray3d
make ray3d
make install
make clean
cd ../..
#
