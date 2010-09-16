# 20061208 rsz Created.

NETCDF_INCLUDE = -I/soft/netcdf/v3.6.0b6_g/include
NETCDF_LIB     = -L/soft/netcdf/v3.6.0b6_g/lib -lnetcdf
# NETCDF_INCLUDE = -I/soft/netcdf/v3.6.2_lf/include
# NETCDF_LIB     = -L/soft/netcdf/v3.6.2_lf/lib -lnetcdf

all:
	gcc ncclamp.c -std=c99 -O3 -Wall $(NETCDF_INCLUDE) $(NETCDF_LIB) -o ncclamp
	strip ncclamp

clean:
	rm -f ncclamp
