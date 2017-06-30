
# for Intel compilers on Linux Shimojo says do
# if (-e /opt/intel/Compiler/11.0/074/bin/iccvars.csh) then
#         source /opt/intel/Compiler/11.0/074/bin/iccvars.csh ia32;
# endif
# if (-e /opt/intel/Compiler/11.0/074/bin/ifortvars.csh) then
#         source /opt/intel/Compiler/11.0/074/bin/ifortvars.csh ia32;
# endif

SRC_COM = 
SRC17 = c2fits17.f
SRC34 = c2fits34.f

OPT_SX4  = -Wf"-pvctl nomsg" -dW
OPT_SX5  = -Wf"-pvctl nomsg" -dW
OPT_NEC = -Zstatic
OPT_SUN =
OPT_LINUX = -save -zero -static -convert big_endian -assume byterecl 
# -integer_size 16

LIB_SX8  = -L../../../lib -lnorhsx8  -L/share/norh_local/lib -lsslsx8 -lfitsiosx8 -L/xmu/lib -lasl
LIB_SX5  = -L../../../lib -lnorhsx5  -L/share/norh_local/lib -lsslsx5 -lfitsiosx5 -L/xmu/lib -lasl
LIB_SX4  = -L../../../lib -lnorhsx4  -L/share/norh_local/lib -lsslsx4 -lfitsiosx4 -lasl
LIB_NEC = -L../../../lib -lnorhnec -L/share/norh_local/lib -lsslnec -lfitsionec
LIB_SUN = -L../../../lib -lnorhsun -L/usr/local/lib  -lssl -lfits
LIB_LINUX = -L/share/norh/lib -lnorhlinux -L/share/norh_local/lib -lssllinux -lcfitsiolinux

linux : c2fits17_linux c2fits34_linux
c2fits17_linux : $(SRC_COM) $(SRC17)
	ifort $(OPT_LINUX) -o $@ $(SRC17) $(SRC_COM) $(LIB_LINUX)
c2fits34_linux : $(SRC_COM) $(SRC34) 
	ifort $(OPT_LINUX) -o $@ $(SRC34) $(SRC_COM) $(LIB_LINUX)

sx8 : c2fits17_sx8 c2fits34_sx8
c2fits17_sx8 : $(SRC_COM) $(SRC17)
	f90 $(OPT_SX5) -o $@ $(SRC17) $(SRC_COM) $(LIB_SX8)
c2fits34_sx8 : $(SRC_COM) $(SRC34)
	f90 $(OPT_SX5) -o $@ $(SRC34) $(SRC_COM) $(LIB_SX8)

sx5 : c2fits17_sx5 c2fits34_sx5
c2fits17_sx5 : $(SRC_COM) $(SRC17)
	f90 $(OPT_SX5) -o $@ $(SRC17) $(SRC_COM) $(LIB_SX5)
c2fits34_sx5 : $(SRC_COM) $(SRC34)
	f90 $(OPT_SX5) -o $@ $(SRC34) $(SRC_COM) $(LIB_SX5)

sx4 : c2fits17_sx4 c2fits34_sx4
c2fits17_sx4 : $(SRC_COM) $(SRC17)
	f90 $(OPT_SX4) -o $@ $(SRC17) $(SRC_COM) $(LIB_SX4)
c2fits34_sx4 : $(SRC_COM) $(SRC34)
	f90 $(OPT_SX4) -o $@ $(SRC34) $(SRC_COM) $(LIB_SX4)

nec : c2fits17_nec  c2fits34_nec 
c2fits17_nec : $(SRC_COM) $(SRC17)
	f77 $(OPT_NEC) -o $@ $(SRC17) $(SRC_COM) $(LIB_NEC)
c2fits34_nec : $(SRC_COM) $(SRC34)
	f77 $(OPT_NEC) -o $@ $(SRC34) $(SRC_COM) $(LIB_NEC)

sun : c2fits17_sun c2fits34_sun
c2fits17_sun : $(SRC_COM) $(SRC17) $(SRC_SUN)
	f77 $(OPT_SUN) -o $@ $(SRC17) $(SRC_COM) $(LIB_SUN)
c2fits34_sun : $(SRC_COM) $(SRC34) $(SRC_SUN)
	f77 $(OPT_SUN) -o $@ $(SRC34) $(SRC_COM) $(LIB_SUN)

tmp : c2fits17.f
	f77 $(OPT_SUN) -o c2fits17_sun c2fits17.f tmp.o $(LIB_SUN)

clean :
	rm *.o 

