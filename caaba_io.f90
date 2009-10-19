!*******************************************************************************

MODULE caaba_io

#ifdef NETCDF
INCLUDE 'caaba_io_netcdf.inc'
#else
INCLUDE 'caaba_io_ascii.inc'
#endif

END MODULE caaba_io

!*******************************************************************************
