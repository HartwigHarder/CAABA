/*
	20061201 rsz Created.
	20061208 rsz Added more comparison support with Infinity and NEQ with NAN.
	20061213 rsz Added more input symbol support for comparison operators.
*/

#define VERSION "0.1"

#include <stdlib.h>
#include <stdio.h>
#include <netcdf.h>
#include <string.h>
#include <strings.h>
#include <ctype.h>
#include <math.h>

#define handle_error(status) {												\
		if (status != NC_NOERR)														\
			{																								\
				fprintf(stderr, "%s\n", nc_strerror(status)); \
				exit(-1);																			\
			}																								\
	}

#define LE 1
#define LT 2
#define GE 3
#define GT 4
#define EQ 5
#define NEQ 6

int usage();

int main(int argc, char *argv[])
{
	int		cdfid;
	char *	path;
	int		varid;
	char	oldcharval=9;
	short	oldshortval=9;
	int oldintval=9;
	float	oldfloatval=9;
	double	olddoubleval=9;
	char	newcharval=9;
	short	newshortval=9;
	int newintval=9;
	float	newfloatval=9;
	double	newdoubleval=9;
	char	name[MAX_NC_NAME];
	nc_type	datatype;
	int		ndims;
	int		dim[MAX_NC_DIMS]; 
	int		natts;
	size_t	i, j, size, dimlen;
	size_t start[MAX_NC_DIMS], count[MAX_NC_DIMS];
	int		status = 0;
	int   isoldvalnan = 0;
	int   isoldvalinf = 0;
	int   recid;
	size_t nrec;
	char* tp;
	short* hp;
	int* ip;
	float* fp;
	double* dp;
	char* oper;
	int op;

	if (argc < 6)	{
		usage();
		return 1;
	}
	
	path = argv[1];
	oper = argv[5];

	if ((strncmp(oper, "le", 2) == 0) || (strncmp(oper, "<=", 2) == 0))
		op = LE;
	else if ((strncmp(oper, "ge", 2) == 0) || (strncmp(oper, ">=", 2) == 0))
		op = GE;
	else if ((strncmp(oper, "gt", 2) == 0) || (strncmp(oper, ">", 1) == 0))
		op = GT;
	else if ((strncmp(oper, "eq", 2) == 0) || (strncmp(oper, "==", 2) == 0))
		op = EQ;
	else if ((strncmp(oper, "neq", 3) == 0) || (strncmp(oper, "<>", 2) == 0) || (strncmp(oper, "!=", 2) == 0))
		op = NEQ;
	else if ((strncmp(oper, "lt", 2) == 0) || (strncmp(oper, "<", 1) == 0))
		op = LT;
	else
		{
			printf("Error. Operator must be either eq, neq, le, lt, ge, gt.\n");
			usage();
			return 1;
		}

	status = nc_open(path, NC_WRITE, &cdfid);
	handle_error(status);

	status = nc_inq_unlimdim(cdfid, &recid);
	handle_error(status);

	/* Number of records. */
	if (recid != -1)
		{
			status = nc_inq_dimlen(cdfid, recid, &nrec);
			handle_error(status);			
		}

	if (!isdigit(*argv[2]))	
		{
			status = nc_inq_varid(cdfid, argv[2], &varid);
			handle_error(status);
		}	else
		{
			varid = atoi(argv[2]);
		}

	status = nc_inq_var(cdfid, varid, name, &datatype, &ndims, dim, &natts);
	handle_error(status);

	if (strncasecmp("nan", argv[3], 3) == 0)
		{
			isoldvalnan = 1;
			
			if ( (op != EQ) && (op != NEQ))
				{
					printf("Error. Only eq and neq comparisons allowed for nan old value.\n");
					return 1;
				}
		} else if (strncasecmp("inf", argv[3], 3) == 0)
		{
			isoldvalinf = 1;
		} else if (strncasecmp("-inf", argv[3], 4) == 0)
		{
			isoldvalinf = -1;
		} else 
		{
			switch(datatype) 
				{
				case NC_BYTE:
				case NC_CHAR:
					oldcharval = argv[3][0];
					break;
				case NC_SHORT:
					oldshortval = (short)atoi(argv[3]);
					break;
				case NC_INT:
					oldintval = (int)atoi(argv[3]);
					break;
				case NC_FLOAT:
					oldfloatval = (float)atof(argv[3]);
					break;
				case NC_DOUBLE:
					olddoubleval = (double)atof(argv[3]);
					break;
				default:
					fprintf(stderr, "Error. Unsupported NetCDF datatype for var=%s.\n", name);
					return -1;
				}
		}
	
	switch(datatype) 
		{
		case NC_BYTE:
		case NC_CHAR:
			newcharval = argv[4][0];
			break;
		case NC_SHORT:
			newshortval = (short)atoi(argv[4]);
			break;
		case NC_INT:
			newintval = (int)atoi(argv[4]);
			break;
		case NC_FLOAT:
			newfloatval = (float)atof(argv[4]);
			break;
		case NC_DOUBLE:
			newdoubleval = (double)atof(argv[4]);
			break;
		default:
			fprintf(stderr, "Error. Unsupported NetCDF datatype for var=%s.\n", name);
			return -1;
		}
	
#define REPLACEEQ(ptr, size, old, new)					\
	for(j = 0; j < size; ++j)																							\
		{																																		\
			if (*(ptr+j) == old)																							\
				{																																\
					*(ptr+j) = new;																								\
				}																																\
		}

#define REPLACENEQ(ptr, size, old, new)					\
	for(j = 0; j < size; ++j)											\
		{																						\
			if (*(ptr+j) != old)											\
				{																				\
					*(ptr+j) = new;												\
				}																				\
		}

#define REPLACELT(ptr, size, old, new)						\
	for(j = 0; j < size; ++j)												\
		{																							\
			if (*(ptr+j) < old)													\
				{																					\
					*(ptr+j) = new;													\
				}																					\
		}

#define REPLACELE(ptr, size, old, new)						\
	for(j = 0; j < size; ++j)												\
		{																							\
			if (*(ptr+j) <= old)												\
				{																					\
					*(ptr+j) = new;													\
				}																					\
		}

#define REPLACEGT(ptr, size, old, new)						\
	for(j = 0; j < size; ++j)												\
		{																							\
			if (*(ptr+j) > old)													\
				{																					\
					*(ptr+j) = new;													\
				}																					\
		}

#define REPLACEGE(ptr, size, old, new)						\
	for(j = 0; j < size; ++j)												\
		{																							\
			if (*(ptr+j) >= old)												\
				{																					\
					*(ptr+j) = new;													\
				}																					\
		}

#define REPLACENANEQ(ptr, size, new)						\
	for(j = 0; j < size; ++j)											\
		{																						\
			if (isnan(*(ptr+j)))											\
				{																				\
					*(ptr+j) = new;												\
				}																				\
		}

#define REPLACENANNEQ(ptr, size, new)						\
	for(j = 0; j < size; ++j)											\
		{																						\
			if (isnan(*(ptr+j)) == 0)									\
				{																				\
					*(ptr+j) = new;												\
				}																				\
		}

#define REPLACEINFEQ(ptr, size, sign, new)				\
	for(j = 0; j < size; ++j)											\
		{																										\
			if (isinf(*(ptr+j)) == sign)											\
				{																								\
					*(ptr+j) = new;																\
				}																								\
		}

#define REPLACEINFNEQ(ptr, size, sign, new)												\
	for(j = 0; j < size; ++j)																				\
		{																															\
			if (isinf(*(ptr+j)) != sign)																\
				{																													\
					*(ptr+j) = new;																					\
				}																													\
		}

	/* Uses record? */
	if (dim[0] == recid)
		{
			/* Find shape of variable per record. */
			count[0] = 1;
			size = 1;
			for(i=1; i < ndims; ++i)
				{
					status = nc_inq_dimlen(cdfid, dim[i], &dimlen);
					handle_error(status);	
					count[i] = dimlen;
					start[i] = 0;
					size *= dimlen;
				}

			switch(datatype) 
				{
				case NC_BYTE:
				case NC_CHAR:
					tp = (char*)malloc(sizeof(char)*size);
					
					for(i=0; i < nrec; ++i)
						{
							start[0] = i;
							status = nc_get_vara_text(cdfid, varid, start, count, tp);
							handle_error(status);	
							switch(op) 
								{
								case EQ:
									REPLACEEQ(tp, size, oldcharval, newcharval)
									break;
								case NEQ:
									REPLACENEQ(tp, size, oldcharval, newcharval)
									break;
								case LT:
									REPLACELT(tp, size, oldcharval, newcharval)
									break;
								case LE:
									REPLACELE(tp, size, oldcharval, newcharval)
									break;
								case GE:
									REPLACEGE(tp, size, oldcharval, newcharval)
									break;
								case GT:
									REPLACEGT(tp, size, oldcharval, newcharval)
									break;
								default:
									printf("Error. Unkown comparison operator used for text type.\n");
									return 1;
								}
							status = nc_put_vara_text(cdfid, varid, start, count, tp);
							handle_error(status);	
						}
					free(tp);
					break;
				case NC_SHORT:
					hp = (short*)malloc(sizeof(short)*size);

					for(i=0; i < nrec; ++i)
						{
							start[0] = i;
							status = nc_get_vara_short(cdfid, varid, start, count, hp);
							switch(op)
								{
								case EQ:
									REPLACEEQ(hp, size, oldshortval, newshortval)
									break;
								case NEQ:
									REPLACENEQ(hp, size, oldshortval, newshortval)
									break;
								case GE:
									REPLACEGE(hp, size, oldshortval, newshortval)
									break;
								case GT:
									REPLACEGT(hp, size, oldshortval, newshortval)
									break;
								case LE:
									REPLACELE(hp, size, oldshortval, newshortval)
									break;
								case LT:
									REPLACELT(hp, size, oldshortval, newshortval)
									break;
								default:
									printf("Error. Unknown comparison operator for short type.\n");
									return 0;
								}
							status = nc_put_vara_short(cdfid, varid, start, count, hp);
							handle_error(status);	
						}
					free(hp);
					break;
				case NC_INT:
					ip = (int*)malloc(sizeof(int)*size);

					for(i=0; i < nrec; ++i)
						{
							start[0] = i;
							status = nc_get_vara_int(cdfid, varid, start, count, ip);
							switch(op)
								{
								case EQ:
									REPLACEEQ(ip, size, oldintval, newintval)
									break;
								case NEQ:
									REPLACENEQ(ip, size, oldintval, newintval)
									break;
								case GE:
									REPLACEGE(ip, size, oldintval, newintval)
									break;
								case GT:
									REPLACEGT(ip, size, oldintval, newintval)
									break;
								case LE:
									REPLACELE(ip, size, oldintval, newintval)
									break;
								case LT:
									REPLACELT(ip, size, oldintval, newintval)
									break;
								default:
									printf("Error. Unknown comparison operator for int type.\n");
									return 0;
								}
							status = nc_put_vara_int(cdfid, varid, start, count, ip);
							handle_error(status);	
						}
					free(ip);
					break;
				case NC_FLOAT:
					fp = (float*)malloc(sizeof(float)*size);

					for(i=0; i < nrec; ++i)
						{
							start[0] = i;
							status = nc_get_vara_float(cdfid, varid, start, count, fp);

							if (isoldvalnan)
								{
									switch(op)
										{
										case EQ:
											REPLACENANEQ(fp, size, newfloatval)
												break;
										case NEQ:
											REPLACENANNEQ(fp, size, newfloatval)
												break;
										default:
											printf("Error. Unknown comparison operator used for float type when replacing NANs.\n");
											return 1;
											break;
										}
								}
							else if (isoldvalinf != 0)
								switch(op)
									{
									case EQ:
										REPLACEINFEQ(fp, size, isoldvalinf, newfloatval)
										break;
									case NEQ:
										REPLACEINFNEQ(fp, size, isoldvalinf, newfloatval)
										break;
									case LE:
										REPLACELE(fp, size, isoldvalinf*INFINITY, newfloatval)
										break;
									case LT:
										REPLACELT(fp, size, isoldvalinf*INFINITY, newfloatval)
										break;
									case GE:
										REPLACEGE(fp, size, isoldvalinf*INFINITY, newfloatval)
										break;
									case GT:
										REPLACEGT(fp, size, isoldvalinf*INFINITY, newfloatval)
										break;
									default:
										printf("Error. Unknown comparison operator used for float type.\n");
										return 1;
										break;
									}
							else
								switch(op)
									{
									case EQ:
										REPLACEEQ(fp, size, oldfloatval, newfloatval)
										break;
									case NEQ:
										REPLACENEQ(fp, size, oldfloatval, newfloatval)
										break;
									case LE:
										REPLACELE(fp, size, oldfloatval, newfloatval)
										break;
									case LT:
										REPLACELT(fp, size, oldfloatval, newfloatval)
										break;
									case GE:
										REPLACEGE(fp, size, oldfloatval, newfloatval)
										break;
									case GT:
										REPLACEGT(fp, size, oldfloatval, newfloatval)
										break;
									default:
										printf("Error. Unknown comparison operator used for float type.\n");
										return 1;
										break;
									}
							status = nc_put_vara_float(cdfid, varid, start, count, fp);
							handle_error(status);	
						}
					free(fp);
					break;
				case NC_DOUBLE:
					dp = (double*)malloc(sizeof(double)*size);
					
					for(i=0; i < nrec; ++i)
						{
							start[0] = i;
							status = nc_get_vara_double(cdfid, varid, start, count, dp);
							
							if (isoldvalnan)
								{
									switch(op)
										{
										case EQ:
											REPLACENANEQ(dp, size, newdoubleval)
												break;
										case NEQ:
											REPLACENANNEQ(dp, size, newdoubleval)
												break;
										default:
											printf("Error. Unknown comparison operator used for double type when replacing NANs.\n");
											return 1;
											break;
										}
								}
							else if (isoldvalinf != 0)
								switch(op)
									{
									case EQ:
										REPLACEINFEQ(dp, size, isoldvalinf, newdoubleval)
										break;
									case NEQ:
										REPLACEINFNEQ(dp, size, isoldvalinf, newdoubleval)
										break;
									case LE:
										REPLACELE(dp, size, isoldvalinf*INFINITY, newdoubleval)
										break;
									case LT:
										REPLACELT(dp, size, isoldvalinf*INFINITY, newdoubleval)
										break;
									case GE:
										REPLACEGE(dp, size, isoldvalinf*INFINITY, newdoubleval)
										break;
									case GT:
										REPLACEGT(dp, size, isoldvalinf*INFINITY, newdoubleval)
										break;
									default:
										printf("Error. Unknown comparison operator used for double type.\n");
										return 1;
										break;
									}										
							else
								switch(op)
									{
									case EQ:
										REPLACEEQ(dp, size, olddoubleval, newdoubleval)
										break;
									case NEQ:
										REPLACENEQ(dp, size, olddoubleval, newdoubleval)
										break;
									case GE:
										REPLACEGE(dp, size, olddoubleval, newdoubleval)
										break;
									case GT:
										REPLACEGT(dp, size, olddoubleval, newdoubleval)
										break;
									case LE:
										REPLACELE(dp, size, olddoubleval, newdoubleval)
										break;
									case LT:
										REPLACELT(dp, size, olddoubleval, newdoubleval)
										break;
									default:
										printf("Error. Unknown comparison operator used for double type.\n");
										return 1;
										break;
									}
							status = nc_put_vara_double(cdfid, varid, start, count, dp);
							handle_error(status);	
						}
					free(dp);
					break;
				default:
					fprintf(stderr, "Error. Unsupported NetCDF datatype for var=%s.\n", name);
					return -1;
				}
		} else
		{
			/* Static variables. */
			size = 1;
			for(i=0; i < ndims; ++i)
				{
					status = nc_inq_dimlen(cdfid, dim[i], &dimlen);
					handle_error(status);	
					size *= dimlen;
				}

			switch(datatype) 
				{
				case NC_BYTE:
				case NC_CHAR:
					tp = (char*)malloc(sizeof(char)*size);
					status = nc_get_var_text(cdfid, varid, tp);
					handle_error(status);	
					switch(op) 
						{
						case EQ:
							REPLACEEQ(tp, size, oldcharval, newcharval)
								break;
						case NEQ:
							REPLACENEQ(tp, size, oldcharval, newcharval)
								break;
						case LT:
							REPLACELT(tp, size, oldcharval, newcharval)
								break;
						case LE:
							REPLACELE(tp, size, oldcharval, newcharval)
								break;
						case GE:
							REPLACEGE(tp, size, oldcharval, newcharval)
								break;
						case GT:
							REPLACEGT(tp, size, oldcharval, newcharval)
								break;
						default:
							printf("Error. Unknown comparison operator used for text type.\n");
							return 1;
						}
					status = nc_put_var_text(cdfid, varid, tp);
					handle_error(status);	
					free(tp);
					break;
				case NC_SHORT:
					hp = (short*)malloc(sizeof(short)*size);
					status = nc_get_var_short(cdfid, varid, hp);
					switch(op)
						{
						case EQ:
							REPLACEEQ(hp, size, oldshortval, newshortval)
								break;
						case NEQ:
							REPLACENEQ(hp, size, oldshortval, newshortval)
								break;
						case GE:
							REPLACEGE(hp, size, oldshortval, newshortval)
								break;
						case GT:
							REPLACEGT(hp, size, oldshortval, newshortval)
								break;
						case LE:
							REPLACELE(hp, size, oldshortval, newshortval)
								break;
						case LT:
							REPLACELT(hp, size, oldshortval, newshortval)
								break;
						default:
							printf("Error. Unknown comparison operator used for short type.\n");
							return 0;
						}
					status = nc_put_var_short(cdfid, varid, hp);
					handle_error(status);	
					free(hp);
					break;
				case NC_INT:
					ip = (int*)malloc(sizeof(int)*size);
					status = nc_get_var_int(cdfid, varid, ip);
					switch(op)
						{
						case EQ:
							REPLACEEQ(ip, size, oldintval, newintval)
								break;
						case NEQ:
							REPLACENEQ(ip, size, oldintval, newintval)
								break;
						case GE:
							REPLACEGE(ip, size, oldintval, newintval)
								break;
						case GT:
							REPLACEGT(ip, size, oldintval, newintval)
								break;
						case LE:
							REPLACELE(ip, size, oldintval, newintval)
								break;
						case LT:
							REPLACELT(ip, size, oldintval, newintval)
								break;
						default:
							printf("Error. Unknown comparison operator used for int type.\n");
							return 0;
						}
					status = nc_put_var_int(cdfid, varid, ip);
					handle_error(status);	
					free(ip);
					break;
				case NC_FLOAT:
					fp = (float*)malloc(sizeof(float)*size);
					status = nc_get_var_float(cdfid, varid, fp);

					if (isoldvalnan)
						{
							switch(op)
								{
								case EQ:
									REPLACENANEQ(fp, size, newfloatval)
   								break;
								case NEQ:
									REPLACENANNEQ(fp, size, newfloatval)
   								break;
								default:
									printf("Error. Unknown comparison operator used for float type when replacing NANs.\n");
									return 1;
									break;
								}	
						}								
					else if (isoldvalinf != 0)
						{
							switch(op)
								{
								case EQ:
									REPLACEINFEQ(fp, size, isoldvalinf, newfloatval)
										break;
								case NEQ:
									REPLACEINFNEQ(fp, size, isoldvalinf, newfloatval)
										break;
								case LE:
									REPLACELE(fp, size, isoldvalinf*INFINITY, newfloatval)
										break;
								case LT:
									REPLACELT(fp, size, isoldvalinf*INFINITY, newfloatval)
										break;
								case GE:
									REPLACEGE(fp, size, isoldvalinf*INFINITY, newfloatval)
										break;
								case GT:
									REPLACEGT(fp, size, isoldvalinf*INFINITY, newfloatval)
										break;
								default:
									printf("Error. Unknown comparison operator used for float type when replacing Infinities.\n");
									return 1;
									break;
								}
						}
					else
						switch(op)
							{
							case EQ:
								REPLACEEQ(fp, size, oldfloatval, newfloatval)
									break;
							case NEQ:
								REPLACENEQ(fp, size, oldfloatval, newfloatval)
									break;
							case LE:
								REPLACELE(fp, size, oldfloatval, newfloatval)
									break;
							case LT:
								REPLACELT(fp, size, oldfloatval, newfloatval)
									break;
							case GE:
								REPLACEGE(fp, size, oldfloatval, newfloatval)
									break;
							case GT:
								REPLACEGT(fp, size, oldfloatval, newfloatval)
									break;
							default:
								printf("Error.\n");
								return 1;
								break;
							}
					
					status = nc_put_var_float(cdfid, varid, fp);
					handle_error(status);	
					free(fp);
					break;
				case NC_DOUBLE:
					dp = (double*)malloc(sizeof(double)*size);
					status = nc_get_var_double(cdfid, varid, dp);

					if (isoldvalnan)
						{	
							switch(op)
								{
								case EQ:
									REPLACENANEQ(dp, size, newdoubleval)
										break;
								case NEQ:
									REPLACENANNEQ(dp, size, newdoubleval)
   								break;
								default:
									printf("Error. Unknown comparison operator used for double type when replacing NANs.\n");
									return 1;
									break;
								}
						}
					else if (isoldvalinf != 0)
						switch(op)
							{
							case EQ:
								REPLACEINFEQ(dp, size, isoldvalinf, newdoubleval)
									break;
							case NEQ:
								REPLACEINFNEQ(dp, size, isoldvalinf, newdoubleval)
									break;
							case LE:
								REPLACELE(dp, size, isoldvalinf*INFINITY, newdoubleval)
									break;
							case LT:
								REPLACELT(dp, size, isoldvalinf*INFINITY, newdoubleval)
									break;
							case GE:
								REPLACEGE(dp, size, isoldvalinf*INFINITY, newdoubleval)
									break;
							case GT:
								REPLACEGT(dp, size, isoldvalinf*INFINITY, newdoubleval)
									break;
							default:
								printf("Error. Unknown comparison operator used for double type when replacing Infinities.\n");
								return 1;
								break;
							}										
					else
						switch(op)
							{
							case EQ:
								REPLACEEQ(dp, size, olddoubleval, newdoubleval)
									break;
							case NEQ:
								REPLACENEQ(dp, size, olddoubleval, newdoubleval)
									break;
							case GE:
								REPLACEGE(dp, size, olddoubleval, newdoubleval)
									break;
							case GT:
								REPLACEGT(dp, size, olddoubleval, newdoubleval)
									break;
							case LE:
								REPLACELE(dp, size, olddoubleval, newdoubleval)
									break;
							case LT:
								REPLACELT(dp, size, olddoubleval, newdoubleval)
									break;
							default:
								printf("Error. Unknown comparison operator used for float type.\n");
								return 1;
								break;
							}
					status = nc_put_var_double(cdfid, varid, dp);
					handle_error(status);	
					free(dp);
					break;
				default:
					fprintf(stderr, "Error. Unsupported NetCDF datatype for var=%s.\n", name);
					return -1;
				}
		}

	/* Ignore return status. */
	(void)nc_close(cdfid);

	return status;
}

int usage()
{
	printf("ncclamp %s:\n\tChange values with another value using a comparison operator.\n", VERSION);
	printf("Usage:\n");
	printf("\tncclamp path varid oldvalue newvalue operator\n");
	printf("\t\tpath:   name of the NetCDF file.\n");
	printf("\t\tvarid:  id (or name) of the variable.\n");
	printf("\t\toldvalue: old value such as 1e-10, 1.234, nan, -inf, or inf.\n");
	printf("\t\tnewvalue: new value such as 1e-10, 1.234, nan, -inf, or inf.\n");
	printf("\t\toperator: '==', '<>', '!=', '<', '<=', '>', '>=', eq, neq, lt, le, gt, ge.\n");
	printf("Examples:\n");
	printf("\tncclamp ncfoo.nc land nan -1e9 eq\n");
	printf("\tncclamp ncfoo.nc topo 0 0 le\n");
	
	return (0);
}
/*
    ncclamp - Replaces values in NetCDF files.
    Copyright (C) 2006  Remik Ziemlinski

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
