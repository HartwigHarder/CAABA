/* A Bison parser, made by GNU Bison 1.875.  */

/* Skeleton parser for Yacc-like parsing with Bison,
   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

/* As a special exception, when this file is copied by Bison into a
   Bison output file, you may use that output file without restriction.
   This special exception was added by the Free Software Foundation
   in version 1.24 of Bison.  */

/* Written by Richard Stallman by simplifying the original so called
   ``semantic'' parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Using locations.  */
#define YYLSP_NEEDED 0



/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     JACOBIAN = 258,
     DOUBLE = 259,
     FUNCTION = 260,
     DEFVAR = 261,
     DEFRAD = 262,
     DEFFIX = 263,
     SETVAR = 264,
     SETRAD = 265,
     SETFIX = 266,
     HESSIAN = 267,
     STOICMAT = 268,
     STOCHASTIC = 269,
     DECLARE = 270,
     INITVALUES = 271,
     EQUATIONS = 272,
     LUMP = 273,
     INIEQUAL = 274,
     EQNEQUAL = 275,
     EQNCOLON = 276,
     LMPCOLON = 277,
     LMPPLUS = 278,
     SPCPLUS = 279,
     SPCEQUAL = 280,
     ATOMDECL = 281,
     CHECK = 282,
     CHECKALL = 283,
     REORDER = 284,
     MEX = 285,
     DUMMYINDEX = 286,
     EQNTAGS = 287,
     LOOKAT = 288,
     LOOKATALL = 289,
     TRANSPORT = 290,
     TRANSPORTALL = 291,
     MONITOR = 292,
     USES = 293,
     SPARSEDATA = 294,
     WRITE_ATM = 295,
     WRITE_SPC = 296,
     WRITE_MAT = 297,
     WRITE_OPT = 298,
     INITIALIZE = 299,
     XGRID = 300,
     YGRID = 301,
     ZGRID = 302,
     USE = 303,
     LANGUAGE = 304,
     INTFILE = 305,
     DRIVER = 306,
     RUN = 307,
     INLINE = 308,
     ENDINLINE = 309,
     PARAMETER = 310,
     SPCSPC = 311,
     INISPC = 312,
     INIVALUE = 313,
     EQNSPC = 314,
     EQNSIGN = 315,
     EQNCOEF = 316,
     RATE = 317,
     LMPSPC = 318,
     SPCNR = 319,
     ATOMID = 320,
     LKTID = 321,
     MNIID = 322,
     INLCTX = 323,
     INCODE = 324,
     SSPID = 325,
     EQNLESS = 326,
     EQNTAG = 327,
     EQNGREATER = 328,
     TPTID = 329,
     USEID = 330
   };
#endif
#define JACOBIAN 258
#define DOUBLE 259
#define FUNCTION 260
#define DEFVAR 261
#define DEFRAD 262
#define DEFFIX 263
#define SETVAR 264
#define SETRAD 265
#define SETFIX 266
#define HESSIAN 267
#define STOICMAT 268
#define STOCHASTIC 269
#define DECLARE 270
#define INITVALUES 271
#define EQUATIONS 272
#define LUMP 273
#define INIEQUAL 274
#define EQNEQUAL 275
#define EQNCOLON 276
#define LMPCOLON 277
#define LMPPLUS 278
#define SPCPLUS 279
#define SPCEQUAL 280
#define ATOMDECL 281
#define CHECK 282
#define CHECKALL 283
#define REORDER 284
#define MEX 285
#define DUMMYINDEX 286
#define EQNTAGS 287
#define LOOKAT 288
#define LOOKATALL 289
#define TRANSPORT 290
#define TRANSPORTALL 291
#define MONITOR 292
#define USES 293
#define SPARSEDATA 294
#define WRITE_ATM 295
#define WRITE_SPC 296
#define WRITE_MAT 297
#define WRITE_OPT 298
#define INITIALIZE 299
#define XGRID 300
#define YGRID 301
#define ZGRID 302
#define USE 303
#define LANGUAGE 304
#define INTFILE 305
#define DRIVER 306
#define RUN 307
#define INLINE 308
#define ENDINLINE 309
#define PARAMETER 310
#define SPCSPC 311
#define INISPC 312
#define INIVALUE 313
#define EQNSPC 314
#define EQNSIGN 315
#define EQNCOEF 316
#define RATE 317
#define LMPSPC 318
#define SPCNR 319
#define ATOMID 320
#define LKTID 321
#define MNIID 322
#define INLCTX 323
#define INCODE 324
#define SSPID 325
#define EQNLESS 326
#define EQNTAG 327
#define EQNGREATER 328
#define TPTID 329
#define USEID 330




/* Copy the first part of user declarations.  */
#line 37 "scan.y"

  #include <stdio.h>
  #include <stdlib.h>
  #include <malloc.h>
  #include <string.h>
  #include <unistd.h>
  #include "scan.h"

  #define __YYSCLASS

  #define YYDEBUG 1
  extern char yytext[];
  extern FILE * yyin;
  
  int nError   = 0;
  int nWarning = 0;

  int crt_section;
  int eqState;
  int isPhoto = 0;

  char crt_term[ 30 ];
  char crt_coef[ 30 ];

  char * InlineBuf;
  int InlineLen;

  void SemicolonError();
  int yyerrflag=0;

  void ParserErrorMessage();
  void yyerror(char *);



/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

#if ! defined (YYSTYPE) && ! defined (YYSTYPE_IS_DECLARED)
#line 73 "scan.y"
typedef union YYSTYPE {
  char str[300];
} YYSTYPE;
/* Line 191 of yacc.c.  */
#line 264 "y.tab.c"
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 214 of yacc.c.  */
#line 276 "y.tab.c"

#if ! defined (yyoverflow) || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# if YYSTACK_USE_ALLOCA
#  define YYSTACK_ALLOC alloca
# else
#  ifndef YYSTACK_USE_ALLOCA
#   if defined (alloca) || defined (_ALLOCA_H)
#    define YYSTACK_ALLOC alloca
#   else
#    ifdef __GNUC__
#     define YYSTACK_ALLOC __builtin_alloca
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning. */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
# else
#  if defined (__STDC__) || defined (__cplusplus)
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   define YYSIZE_T size_t
#  endif
#  define YYSTACK_ALLOC malloc
#  define YYSTACK_FREE free
# endif
#endif /* ! defined (yyoverflow) || YYERROR_VERBOSE */


#if (! defined (yyoverflow) \
     && (! defined (__cplusplus) \
	 || (YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  short yyss;
  YYSTYPE yyvs;
  };

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (short) + sizeof (YYSTYPE))				\
      + YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  register YYSIZE_T yyi;		\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (0)
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack)					\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack, Stack, yysize);				\
	Stack = &yyptr->Stack;						\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (0)

#endif

#if defined (__STDC__) || defined (__cplusplus)
   typedef signed char yysigned_char;
#else
   typedef short yysigned_char;
#endif

/* YYFINAL -- State number of the termination state. */
#define YYFINAL  124
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   192

/* YYNTOKENS -- Number of terminals. */
#define YYNTOKENS  77
/* YYNNTS -- Number of nonterminals. */
#define YYNNTS  35
/* YYNRULES -- Number of rules. */
#define YYNRULES  111
/* YYNRULES -- Number of states. */
#define YYNSTATES  202

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   330

#define YYTRANSLATE(YYX) 						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const unsigned char yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,    76,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const unsigned short yyprhs[] =
{
       0,     0,     3,     5,     8,    11,    14,    17,    20,    23,
      26,    29,    32,    35,    38,    41,    44,    47,    50,    53,
      56,    59,    62,    65,    68,    71,    74,    77,    80,    83,
      85,    87,    89,    91,    93,    95,    97,   100,   103,   106,
     109,   112,   115,   120,   123,   126,   129,   132,   135,   138,
     141,   143,   147,   150,   153,   155,   159,   162,   165,   167,
     171,   174,   177,   179,   183,   186,   189,   191,   195,   198,
     201,   203,   207,   210,   213,   215,   219,   222,   225,   227,
     229,   233,   235,   239,   241,   244,   246,   250,   253,   256,
     260,   264,   267,   270,   275,   279,   282,   284,   288,   291,
     294,   298,   301,   303,   306,   308,   312,   315,   318,   322,
     326,   329
};

/* YYRHS -- A `-1'-separated list of the rules' RHS. */
static const yysigned_char yyrhs[] =
{
      78,     0,    -1,    79,    -1,    79,    78,    -1,     3,    55,
      -1,    12,    55,    -1,    15,    55,    -1,    13,    55,    -1,
       4,    55,    -1,    29,    55,    -1,    30,    55,    -1,    31,
      55,    -1,    32,    55,    -1,     5,    55,    -1,    14,    55,
      -1,    26,    81,    -1,    27,    81,    -1,     6,    93,    -1,
       7,    93,    -1,     8,    93,    -1,     9,    91,    -1,    10,
      91,    -1,    11,    91,    -1,    16,    99,    -1,    17,   101,
      -1,    18,   109,    -1,    33,    83,    -1,    37,    85,    -1,
      35,    87,    -1,    28,    -1,    34,    -1,    36,    -1,    40,
      -1,    41,    -1,    42,    -1,    43,    -1,    48,    55,    -1,
      49,    55,    -1,    44,    55,    -1,    45,    55,    -1,    46,
      55,    -1,    47,    55,    -1,    53,    68,   111,    54,    -1,
      53,     1,    -1,    50,    55,    -1,    51,    55,    -1,    52,
      55,    -1,    38,    89,    -1,    39,    55,    -1,    80,    76,
      -1,    76,    -1,    81,    82,    80,    -1,    82,    80,    -1,
       1,    80,    -1,    65,    -1,    83,    84,    80,    -1,    84,
      80,    -1,     1,    80,    -1,    66,    -1,    85,    86,    80,
      -1,    86,    80,    -1,     1,    80,    -1,    67,    -1,    87,
      88,    80,    -1,    88,    80,    -1,     1,    80,    -1,    74,
      -1,    89,    90,    80,    -1,    90,    80,    -1,     1,    80,
      -1,    75,    -1,    91,    92,    80,    -1,    92,    80,    -1,
       1,    80,    -1,    70,    -1,    93,    94,    80,    -1,    94,
      80,    -1,     1,    80,    -1,    95,    -1,    96,    -1,    56,
      25,    97,    -1,    56,    -1,    97,    24,    98,    -1,    98,
      -1,    64,    56,    -1,    56,    -1,    99,   100,    80,    -1,
     100,    80,    -1,     1,    80,    -1,    57,    19,    58,    -1,
     101,   102,    80,    -1,   102,    80,    -1,     1,    80,    -1,
     104,   105,   106,   103,    -1,   105,   106,   103,    -1,    62,
     103,    -1,    62,    -1,    71,    72,    73,    -1,   107,    20,
      -1,   107,    21,    -1,   107,    60,   108,    -1,    60,   108,
      -1,   108,    -1,    61,    59,    -1,    59,    -1,   109,   110,
      80,    -1,   110,    80,    -1,     1,    80,    -1,    63,    23,
     110,    -1,    63,    22,    63,    -1,   111,    69,    -1,    69,
      -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const unsigned short yyrline[] =
{
       0,    97,    97,    98,   100,   103,   106,   109,   112,   115,
     118,   121,   124,   127,   130,   133,   135,   137,   139,   141,
     143,   145,   147,   149,   151,   153,   155,   157,   159,   161,
     163,   165,   167,   169,   171,   173,   175,   177,   179,   181,
     183,   185,   187,   192,   194,   196,   198,   200,   202,   206,
     209,   211,   212,   213,   216,   223,   224,   225,   228,   232,
     233,   234,   237,   241,   242,   243,   246,   250,   251,   252,
     255,   259,   260,   261,   264,   272,   273,   274,   277,   278,
     280,   288,   296,   297,   299,   302,   306,   307,   308,   311,
     314,   315,   316,   321,   326,   331,   335,   339,   343,   346,
     349,   352,   355,   359,   363,   368,   369,   370,   373,   376,
     381,   385
};
#endif

#if YYDEBUG || YYERROR_VERBOSE
/* YYTNME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals. */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "JACOBIAN", "DOUBLE", "FUNCTION", "DEFVAR", 
  "DEFRAD", "DEFFIX", "SETVAR", "SETRAD", "SETFIX", "HESSIAN", "STOICMAT", 
  "STOCHASTIC", "DECLARE", "INITVALUES", "EQUATIONS", "LUMP", "INIEQUAL", 
  "EQNEQUAL", "EQNCOLON", "LMPCOLON", "LMPPLUS", "SPCPLUS", "SPCEQUAL", 
  "ATOMDECL", "CHECK", "CHECKALL", "REORDER", "MEX", "DUMMYINDEX", 
  "EQNTAGS", "LOOKAT", "LOOKATALL", "TRANSPORT", "TRANSPORTALL", 
  "MONITOR", "USES", "SPARSEDATA", "WRITE_ATM", "WRITE_SPC", "WRITE_MAT", 
  "WRITE_OPT", "INITIALIZE", "XGRID", "YGRID", "ZGRID", "USE", "LANGUAGE", 
  "INTFILE", "DRIVER", "RUN", "INLINE", "ENDINLINE", "PARAMETER", 
  "SPCSPC", "INISPC", "INIVALUE", "EQNSPC", "EQNSIGN", "EQNCOEF", "RATE", 
  "LMPSPC", "SPCNR", "ATOMID", "LKTID", "MNIID", "INLCTX", "INCODE", 
  "SSPID", "EQNLESS", "EQNTAG", "EQNGREATER", "TPTID", "USEID", "';'", 
  "$accept", "program", "section", "semicolon", "atomlist", "atomdef", 
  "lookatlist", "lookatspc", "monitorlist", "monitorspc", "translist", 
  "transspc", "uselist", "usefile", "setspclist", "setspcspc", "species", 
  "spc", "spcname", "spcdef", "atoms", "atom", "initvalues", "assignment", 
  "equations", "equation", "rate", "eqntag", "lefths", "righths", 
  "expresion", "term", "lumps", "lump", "inlinecode", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const unsigned short yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
     315,   316,   317,   318,   319,   320,   321,   322,   323,   324,
     325,   326,   327,   328,   329,   330,    59
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const unsigned char yyr1[] =
{
       0,    77,    78,    78,    79,    79,    79,    79,    79,    79,
      79,    79,    79,    79,    79,    79,    79,    79,    79,    79,
      79,    79,    79,    79,    79,    79,    79,    79,    79,    79,
      79,    79,    79,    79,    79,    79,    79,    79,    79,    79,
      79,    79,    79,    79,    79,    79,    79,    79,    79,    80,
      80,    81,    81,    81,    82,    83,    83,    83,    84,    85,
      85,    85,    86,    87,    87,    87,    88,    89,    89,    89,
      90,    91,    91,    91,    92,    93,    93,    93,    94,    94,
      95,    96,    97,    97,    98,    98,    99,    99,    99,   100,
     101,   101,   101,   102,   102,   103,   103,   104,   105,   106,
     107,   107,   107,   108,   108,   109,   109,   109,   110,   110,
     111,   111
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const unsigned char yyr2[] =
{
       0,     2,     1,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     1,
       1,     1,     1,     1,     1,     1,     2,     2,     2,     2,
       2,     2,     4,     2,     2,     2,     2,     2,     2,     2,
       1,     3,     2,     2,     1,     3,     2,     2,     1,     3,
       2,     2,     1,     3,     2,     2,     1,     3,     2,     2,
       1,     3,     2,     2,     1,     3,     2,     2,     1,     1,
       3,     1,     3,     1,     2,     1,     3,     2,     2,     3,
       3,     2,     2,     4,     3,     2,     1,     3,     2,     2,
       3,     2,     1,     2,     1,     3,     2,     2,     3,     3,
       2,     1
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const unsigned char yydefact[] =
{
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    29,
       0,     0,     0,     0,     0,    30,     0,    31,     0,     0,
       0,    32,    33,    34,    35,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     2,     4,     8,    13,
       0,    81,    17,     0,    78,    79,    18,    19,     0,    74,
      20,     0,    21,    22,     5,     7,    14,     6,     0,     0,
      23,     0,     0,   104,     0,     0,     0,    24,     0,     0,
       0,     0,   102,     0,     0,    25,     0,     0,    54,    15,
       0,    16,     9,    10,    11,    12,     0,    58,    26,     0,
       0,    66,    28,     0,     0,    62,    27,     0,     0,    70,
      47,     0,    48,    38,    39,    40,    41,    36,    37,    44,
      45,    46,    43,     0,     1,     3,    50,    77,     0,     0,
      76,    73,     0,    72,    88,     0,     0,    87,    92,   101,
     103,     0,     0,    91,     0,     0,     0,    98,     0,   107,
       0,     0,     0,   106,    53,     0,    52,    57,     0,    56,
      65,     0,    64,    61,     0,    60,    69,     0,    68,   111,
       0,    49,    85,     0,    80,    83,    75,    71,    89,    86,
      97,    90,     0,    96,    94,    99,   100,   109,   108,   105,
      51,    55,    63,    59,    67,    42,   110,    84,     0,    93,
      95,    82
};

/* YYDEFGOTO[NTERM-NUM]. */
static const short yydefgoto[] =
{
      -1,    45,    46,   127,    89,    90,    98,    99,   106,   107,
     102,   103,   110,   111,    60,    61,    52,    53,    54,    55,
     174,   175,    70,    71,    77,    78,   184,    79,    80,   145,
      81,    82,    85,    86,   170
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -91
static const short yypact[] =
{
     112,    -6,    -3,     4,    11,    11,    11,     3,     3,     3,
      30,    33,    39,    40,     8,     1,    21,     9,     9,   -91,
      41,    42,    43,    45,     5,   -91,     6,   -91,    15,     0,
      46,   -91,   -91,   -91,   -91,    48,    49,    51,    52,    54,
      55,    57,    58,    76,    10,    63,   112,   -91,   -91,   -91,
      56,   108,    14,    56,   -91,   -91,    14,    14,    56,   -91,
      64,    56,    64,    64,   -91,   -91,   -91,   -91,    56,   116,
      79,    56,    56,   -91,   -38,    78,    94,   -33,    56,   -20,
     -20,    -7,   -91,    56,    34,   104,    56,    56,   -91,   103,
      56,   103,   -91,   -91,   -91,   -91,    56,   -91,   105,    56,
      56,   -91,    95,    56,    56,   -91,   106,    56,    56,   -91,
      97,    56,   -91,   -91,   -91,   -91,   -91,   -91,   -91,   -91,
     -91,   -91,   -91,   101,   -91,   -91,   -91,    98,   -32,    56,
      98,    98,    56,    98,    98,   117,    56,    98,    98,   -91,
     -91,   107,    56,    98,   -20,   114,    27,   -91,   -38,    98,
     115,   104,    56,    98,    98,    56,    98,    98,    56,    98,
      98,    56,    98,    98,    56,    98,    98,    56,    98,   -91,
     -40,   -91,   -91,   121,   155,   -91,    98,    98,   -91,    98,
     -91,    98,   114,   114,   -91,   -91,   -91,   -91,   -91,    98,
      98,    98,    98,    98,    98,   -91,   -91,   -91,   -32,   -91,
     -91,   -91
};

/* YYPGOTO[NTERM-NUM].  */
static const short yypgoto[] =
{
     -91,   135,   -91,   -53,   164,   -25,   -91,    86,   -91,    77,
     -91,    83,   -91,    80,    60,   -18,    85,   -21,   -91,   -91,
     -91,   -12,   -91,   118,   -91,   110,   -90,   -91,   113,    47,
     -63,   -71,   -91,   -65,   -91
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -1
static const unsigned char yytable[] =
{
     130,   108,    72,   139,    58,   131,    96,   100,   133,    68,
      87,   122,    50,   147,   195,   134,   104,   146,   137,   138,
     152,    73,    83,    75,   172,   143,    73,    74,    75,   196,
     149,   129,   173,   153,   154,   129,   129,   156,    76,    73,
      74,    75,   132,   157,   132,   132,   159,   160,   185,    47,
     162,   163,    48,   148,   165,   166,   150,   151,   168,    49,
      73,    74,    75,   124,   155,    69,   155,    51,    62,    63,
      51,    97,    76,    59,    88,   109,   176,   186,   123,   177,
     101,   146,   105,   179,    84,    64,   188,   148,    65,   181,
      56,    57,   199,   200,    66,    67,    92,    93,    94,   189,
      95,   112,   190,   113,   114,   191,   115,   116,   192,   117,
     118,   193,   119,   120,   194,     1,     2,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,   121,   126,   128,    59,   135,    69,   140,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,   141,    84,    88,   101,
     169,    97,   109,   105,   171,   178,   183,   197,   187,   198,
     180,   125,    91,   164,   158,   161,   201,   142,   136,     0,
     167,   182,   144
};

static const short yycheck[] =
{
      53,     1,     1,    74,     1,    58,     1,     1,    61,     1,
       1,     1,     1,    20,    54,    68,     1,    80,    71,    72,
      85,    59,     1,    61,    56,    78,    59,    60,    61,    69,
      83,    52,    64,    86,    87,    56,    57,    90,    71,    59,
      60,    61,    60,    96,    62,    63,    99,   100,    21,    55,
     103,   104,    55,    60,   107,   108,    22,    23,   111,    55,
      59,    60,    61,     0,    89,    57,    91,    56,     8,     9,
      56,    66,    71,    70,    65,    75,   129,   148,    68,   132,
      74,   144,    67,   136,    63,    55,   151,    60,    55,   142,
       5,     6,   182,   183,    55,    55,    55,    55,    55,   152,
      55,    55,   155,    55,    55,   158,    55,    55,   161,    55,
      55,   164,    55,    55,   167,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    55,    76,    25,    70,    19,    57,    59,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    72,    63,    65,    74,
      69,    66,    75,    67,    76,    58,    62,    56,    63,    24,
      73,    46,    18,   106,    98,   102,   198,    77,    70,    -1,
     110,   144,    79
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const unsigned char yystos[] =
{
       0,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    78,    79,    55,    55,    55,
       1,    56,    93,    94,    95,    96,    93,    93,     1,    70,
      91,    92,    91,    91,    55,    55,    55,    55,     1,    57,
      99,   100,     1,    59,    60,    61,    71,   101,   102,   104,
     105,   107,   108,     1,    63,   109,   110,     1,    65,    81,
      82,    81,    55,    55,    55,    55,     1,    66,    83,    84,
       1,    74,    87,    88,     1,    67,    85,    86,     1,    75,
      89,    90,    55,    55,    55,    55,    55,    55,    55,    55,
      55,    55,     1,    68,     0,    78,    76,    80,    25,    94,
      80,    80,    92,    80,    80,    19,   100,    80,    80,   108,
      59,    72,   102,    80,   105,   106,   107,    20,    60,    80,
      22,    23,   110,    80,    80,    82,    80,    80,    84,    80,
      80,    88,    80,    80,    86,    80,    80,    90,    80,    69,
     111,    76,    56,    64,    97,    98,    80,    80,    58,    80,
      73,    80,   106,    62,   103,    21,   108,    63,   110,    80,
      80,    80,    80,    80,    80,    54,    69,    56,    24,   103,
     103,    98
};

#if ! defined (YYSIZE_T) && defined (__SIZE_TYPE__)
# define YYSIZE_T __SIZE_TYPE__
#endif
#if ! defined (YYSIZE_T) && defined (size_t)
# define YYSIZE_T size_t
#endif
#if ! defined (YYSIZE_T)
# if defined (__STDC__) || defined (__cplusplus)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# endif
#endif
#if ! defined (YYSIZE_T)
# define YYSIZE_T unsigned int
#endif

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrlab1

/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto yyerrlab

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yytoken = YYTRANSLATE (yychar);				\
      YYPOPSTACK;						\
      goto yybackup;						\
    }								\
  else								\
    { 								\
      yyerror ("syntax error: cannot back up");\
      YYERROR;							\
    }								\
while (0)

#define YYTERROR	1
#define YYERRCODE	256

/* YYLLOC_DEFAULT -- Compute the default location (before the actions
   are run).  */

#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)         \
  Current.first_line   = Rhs[1].first_line;      \
  Current.first_column = Rhs[1].first_column;    \
  Current.last_line    = Rhs[N].last_line;       \
  Current.last_column  = Rhs[N].last_column;
#endif

/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (YYLEX_PARAM)
#else
# define YYLEX yylex ()
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (0)

# define YYDSYMPRINT(Args)			\
do {						\
  if (yydebug)					\
    yysymprint Args;				\
} while (0)

# define YYDSYMPRINTF(Title, Token, Value, Location)		\
do {								\
  if (yydebug)							\
    {								\
      YYFPRINTF (stderr, "%s ", Title);				\
      yysymprint (stderr, 					\
                  Token, Value);	\
      YYFPRINTF (stderr, "\n");					\
    }								\
} while (0)

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (cinluded).                                                   |
`------------------------------------------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yy_stack_print (short *bottom, short *top)
#else
static void
yy_stack_print (bottom, top)
    short *bottom;
    short *top;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (/* Nothing. */; bottom <= top; ++bottom)
    YYFPRINTF (stderr, " %d", *bottom);
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yy_reduce_print (int yyrule)
#else
static void
yy_reduce_print (yyrule)
    int yyrule;
#endif
{
  int yyi;
  unsigned int yylineno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %u), ",
             yyrule - 1, yylineno);
  /* Print the symbols being reduced, and their result.  */
  for (yyi = yyprhs[yyrule]; 0 <= yyrhs[yyi]; yyi++)
    YYFPRINTF (stderr, "%s ", yytname [yyrhs[yyi]]);
  YYFPRINTF (stderr, "-> %s\n", yytname [yyr1[yyrule]]);
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (Rule);		\
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YYDSYMPRINT(Args)
# define YYDSYMPRINTF(Title, Token, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   SIZE_MAX < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#if YYMAXDEPTH == 0
# undef YYMAXDEPTH
#endif

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif



#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined (__GLIBC__) && defined (_STRING_H)
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
#   if defined (__STDC__) || defined (__cplusplus)
yystrlen (const char *yystr)
#   else
yystrlen (yystr)
     const char *yystr;
#   endif
{
  register const char *yys = yystr;

  while (*yys++ != '\0')
    continue;

  return yys - yystr - 1;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined (__GLIBC__) && defined (_STRING_H) && defined (_GNU_SOURCE)
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
#   if defined (__STDC__) || defined (__cplusplus)
yystpcpy (char *yydest, const char *yysrc)
#   else
yystpcpy (yydest, yysrc)
     char *yydest;
     const char *yysrc;
#   endif
{
  register char *yyd = yydest;
  register const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

#endif /* !YYERROR_VERBOSE */



#if YYDEBUG
/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yysymprint (FILE *yyoutput, int yytype, YYSTYPE *yyvaluep)
#else
static void
yysymprint (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  /* Pacify ``unused variable'' warnings.  */
  (void) yyvaluep;

  if (yytype < YYNTOKENS)
    {
      YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
# ifdef YYPRINT
      YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
    }
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  switch (yytype)
    {
      default:
        break;
    }
  YYFPRINTF (yyoutput, ")");
}

#endif /* ! YYDEBUG */
/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yydestruct (int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yytype, yyvaluep)
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  /* Pacify ``unused variable'' warnings.  */
  (void) yyvaluep;

  switch (yytype)
    {

      default:
        break;
    }
}


/* Prevent warnings from -Wmissing-prototypes.  */

#ifdef YYPARSE_PARAM
# if defined (__STDC__) || defined (__cplusplus)
int yyparse (void *YYPARSE_PARAM);
# else
int yyparse ();
# endif
#else /* ! YYPARSE_PARAM */
#if defined (__STDC__) || defined (__cplusplus)
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */



/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;



/*----------.
| yyparse.  |
`----------*/

#ifdef YYPARSE_PARAM
# if defined (__STDC__) || defined (__cplusplus)
int yyparse (void *YYPARSE_PARAM)
# else
int yyparse (YYPARSE_PARAM)
  void *YYPARSE_PARAM;
# endif
#else /* ! YYPARSE_PARAM */
#if defined (__STDC__) || defined (__cplusplus)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{
  
  register int yystate;
  register int yyn;
  int yyresult;
  /* Number of tokens to shift before error messages enabled.  */
  int yyerrstatus;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;

  /* Three stacks and their tools:
     `yyss': related to states,
     `yyvs': related to semantic values,
     `yyls': related to locations.

     Refer to the stacks thru separate pointers, to allow yyoverflow
     to reallocate them elsewhere.  */

  /* The state stack.  */
  short	yyssa[YYINITDEPTH];
  short *yyss = yyssa;
  register short *yyssp;

  /* The semantic value stack.  */
  YYSTYPE yyvsa[YYINITDEPTH];
  YYSTYPE *yyvs = yyvsa;
  register YYSTYPE *yyvsp;



#define YYPOPSTACK   (yyvsp--, yyssp--)

  YYSIZE_T yystacksize = YYINITDEPTH;

  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;


  /* When reducing, the number of symbols on the RHS of the reduced
     rule.  */
  int yylen;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss;
  yyvsp = yyvs;

  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed. so pushing a state here evens the stacks.
     */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack. Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	short *yyss1 = yyss;


	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow ("parser stack overflow",
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),

		    &yystacksize);

	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyoverflowlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyoverflowlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	short *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyoverflowlab;
	YYSTACK_RELOCATE (yyss);
	YYSTACK_RELOCATE (yyvs);

#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;


      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

/* Do appropriate processing given the current state.  */
/* Read a lookahead token if we need one and don't already have one.  */
/* yyresume: */

  /* First try to decide what to do without reference to lookahead token.  */

  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YYDSYMPRINTF ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yyn == 0 || yyn == YYTABLE_NINF)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Shift the lookahead token.  */
  YYDPRINTF ((stderr, "Shifting token %s, ", yytname[yytoken]));

  /* Discard the token being shifted unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  *++yyvsp = yylval;


  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  yystate = yyn;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 4:
#line 101 "scan.y"
    { CmdJacobian( yyvsp[0].str );
                  ;}
    break;

  case 5:
#line 104 "scan.y"
    { CmdHessian( yyvsp[0].str );
                  ;}
    break;

  case 6:
#line 107 "scan.y"
    { CmdDeclareValues( yyvsp[0].str );
                  ;}
    break;

  case 7:
#line 110 "scan.y"
    { CmdStoicmat( yyvsp[0].str );
                  ;}
    break;

  case 8:
#line 113 "scan.y"
    { CmdDouble( yyvsp[0].str );
                  ;}
    break;

  case 9:
#line 116 "scan.y"
    { CmdReorder( yyvsp[0].str );
                  ;}
    break;

  case 10:
#line 119 "scan.y"
    { CmdMex( yyvsp[0].str );
                  ;}
    break;

  case 11:
#line 122 "scan.y"
    { CmdDummyindex( yyvsp[0].str );
                  ;}
    break;

  case 12:
#line 125 "scan.y"
    { CmdEqntags( yyvsp[0].str );
                  ;}
    break;

  case 13:
#line 128 "scan.y"
    { CmdFunction( yyvsp[0].str );
                  ;}
    break;

  case 14:
#line 131 "scan.y"
    { CmdStochastic( yyvsp[0].str );
                  ;}
    break;

  case 15:
#line 134 "scan.y"
    {;}
    break;

  case 16:
#line 136 "scan.y"
    {;}
    break;

  case 17:
#line 138 "scan.y"
    {;}
    break;

  case 18:
#line 140 "scan.y"
    {;}
    break;

  case 19:
#line 142 "scan.y"
    {;}
    break;

  case 20:
#line 144 "scan.y"
    {;}
    break;

  case 21:
#line 146 "scan.y"
    {;}
    break;

  case 22:
#line 148 "scan.y"
    {;}
    break;

  case 23:
#line 150 "scan.y"
    {;}
    break;

  case 24:
#line 152 "scan.y"
    {;}
    break;

  case 25:
#line 154 "scan.y"
    {;}
    break;

  case 26:
#line 156 "scan.y"
    {;}
    break;

  case 27:
#line 158 "scan.y"
    {;}
    break;

  case 28:
#line 160 "scan.y"
    {;}
    break;

  case 29:
#line 162 "scan.y"
    { CheckAll(); ;}
    break;

  case 30:
#line 164 "scan.y"
    { LookAtAll(); ;}
    break;

  case 31:
#line 166 "scan.y"
    { TransportAll(); ;}
    break;

  case 32:
#line 168 "scan.y"
    { WriteAtoms(); ;}
    break;

  case 33:
#line 170 "scan.y"
    { WriteSpecies(); ;}
    break;

  case 34:
#line 172 "scan.y"
    { WriteMatrices(); ;}
    break;

  case 35:
#line 174 "scan.y"
    { WriteOptions(); ;}
    break;

  case 36:
#line 176 "scan.y"
    { CmdUse( yyvsp[0].str ); ;}
    break;

  case 37:
#line 178 "scan.y"
    { CmdLanguage( yyvsp[0].str ); ;}
    break;

  case 38:
#line 180 "scan.y"
    { DefineInitializeNbr( yyvsp[0].str ); ;}
    break;

  case 39:
#line 182 "scan.y"
    { DefineXGrid( yyvsp[0].str ); ;}
    break;

  case 40:
#line 184 "scan.y"
    { DefineYGrid( yyvsp[0].str ); ;}
    break;

  case 41:
#line 186 "scan.y"
    { DefineZGrid( yyvsp[0].str ); ;}
    break;

  case 42:
#line 188 "scan.y"
    { 
		    AddInlineCode( yyvsp[-2].str, InlineBuf );
                    free( InlineBuf );
		  ;}
    break;

  case 43:
#line 193 "scan.y"
    { ParserErrorMessage(); ;}
    break;

  case 44:
#line 195 "scan.y"
    { CmdIntegrator( yyvsp[0].str ); ;}
    break;

  case 45:
#line 197 "scan.y"
    { CmdDriver( yyvsp[0].str ); ;}
    break;

  case 46:
#line 199 "scan.y"
    { CmdRun( yyvsp[0].str ); ;}
    break;

  case 47:
#line 201 "scan.y"
    {;}
    break;

  case 48:
#line 203 "scan.y"
    { SparseData( yyvsp[0].str );
                  ;}
    break;

  case 49:
#line 207 "scan.y"
    { ScanWarning("Unnecessary ';'");
                  ;}
    break;

  case 53:
#line 214 "scan.y"
    { ParserErrorMessage(); ;}
    break;

  case 54:
#line 217 "scan.y"
    { switch( crt_section ) {
                      case ATOMDECL: DeclareAtom( yyvsp[0].str ); break;
                      case CHECK:    SetAtomType( yyvsp[0].str, DO_CHECK ); break;
                    }
                  ;}
    break;

  case 57:
#line 226 "scan.y"
    { ParserErrorMessage(); ;}
    break;

  case 58:
#line 229 "scan.y"
    { AddLookAt( yyvsp[0].str );
                  ;}
    break;

  case 61:
#line 235 "scan.y"
    { ParserErrorMessage(); ;}
    break;

  case 62:
#line 238 "scan.y"
    { AddMonitor( yyvsp[0].str );
                  ;}
    break;

  case 65:
#line 244 "scan.y"
    { ParserErrorMessage(); ;}
    break;

  case 66:
#line 247 "scan.y"
    { AddTransport( yyvsp[0].str );
                  ;}
    break;

  case 69:
#line 253 "scan.y"
    { ParserErrorMessage(); ;}
    break;

  case 70:
#line 256 "scan.y"
    { AddUseFile( yyvsp[0].str );
                  ;}
    break;

  case 73:
#line 262 "scan.y"
    { ParserErrorMessage(); ;}
    break;

  case 74:
#line 265 "scan.y"
    { switch( crt_section ) {
                      case SETVAR: SetSpcType( VAR_SPC, yyvsp[0].str ); break;
                      case SETRAD: SetSpcType( RAD_SPC, yyvsp[0].str ); break;
                      case SETFIX: SetSpcType( FIX_SPC, yyvsp[0].str ); break;
                    }
                  ;}
    break;

  case 77:
#line 275 "scan.y"
    { ParserErrorMessage(); ;}
    break;

  case 80:
#line 281 "scan.y"
    { switch( crt_section ) {
                      case DEFVAR: DeclareSpecies( VAR_SPC, yyvsp[-2].str ); break;
                      case DEFRAD: DeclareSpecies( RAD_SPC, yyvsp[-2].str ); break;
                      case DEFFIX: DeclareSpecies( FIX_SPC, yyvsp[-2].str ); break;
                    } 
                  ;}
    break;

  case 81:
#line 289 "scan.y"
    { switch( crt_section ) {
                      case DEFVAR: DeclareSpecies( VAR_SPC, yyvsp[0].str ); break;
                      case DEFRAD: DeclareSpecies( RAD_SPC, yyvsp[0].str ); break;
                      case DEFFIX: DeclareSpecies( FIX_SPC, yyvsp[0].str ); break;
                    } 
                  ;}
    break;

  case 84:
#line 300 "scan.y"
    { AddAtom( yyvsp[0].str, yyvsp[-1].str );
                  ;}
    break;

  case 85:
#line 303 "scan.y"
    { AddAtom( yyvsp[0].str, "1" );
                  ;}
    break;

  case 88:
#line 309 "scan.y"
    { ParserErrorMessage(); ;}
    break;

  case 89:
#line 312 "scan.y"
    { AssignInitialValue( yyvsp[-2].str, yyvsp[0].str ); ;}
    break;

  case 92:
#line 317 "scan.y"
    { ParserErrorMessage();
                    eqState = LHS; 
                  ;}
    break;

  case 93:
#line 322 "scan.y"
    { eqState = LHS;
                    StoreEquationRate( yyvsp[0].str, yyvsp[-3].str ); 
                    CheckEquation();
                  ;}
    break;

  case 94:
#line 327 "scan.y"
    { eqState = LHS;
                    StoreEquationRate( yyvsp[0].str, "          " ); 
                    CheckEquation();
                  ;}
    break;

  case 95:
#line 332 "scan.y"
    { strcpy( yyval.str, yyvsp[-1].str );
                    strcat( yyval.str, yyvsp[0].str ); 
                  ;}
    break;

  case 96:
#line 336 "scan.y"
    { strcpy( yyval.str, yyvsp[0].str );
                  ;}
    break;

  case 97:
#line 340 "scan.y"
    { strcpy( yyval.str, yyvsp[-1].str );
                  ;}
    break;

  case 98:
#line 344 "scan.y"
    { eqState = RHS; ;}
    break;

  case 99:
#line 347 "scan.y"
    { eqState = RAT; ;}
    break;

  case 100:
#line 350 "scan.y"
    { ProcessTerm( eqState, yyvsp[-1].str, crt_coef, crt_term ); 
                  ;}
    break;

  case 101:
#line 353 "scan.y"
    { ProcessTerm( eqState, yyvsp[-1].str, crt_coef, crt_term );
                  ;}
    break;

  case 102:
#line 356 "scan.y"
    { ProcessTerm( eqState, "+", crt_coef, crt_term );
                  ;}
    break;

  case 103:
#line 360 "scan.y"
    { strcpy( crt_term, yyvsp[0].str );
                    strcpy( crt_coef, yyvsp[-1].str );  
                  ;}
    break;

  case 104:
#line 364 "scan.y"
    { strcpy( crt_term, yyvsp[0].str );         
                    strcpy( crt_coef, "1" ); 
                  ;}
    break;

  case 107:
#line 371 "scan.y"
    { ParserErrorMessage(); ;}
    break;

  case 108:
#line 374 "scan.y"
    { AddLumpSpecies( yyvsp[-2].str );
                  ;}
    break;

  case 109:
#line 377 "scan.y"
    {
                    AddLumpSpecies( yyvsp[-2].str );
                    CheckLump( yyvsp[0].str );  
                  ;}
    break;

  case 110:
#line 382 "scan.y"
    {
		    InlineBuf = AppendString( InlineBuf, yyvsp[0].str, &InlineLen, MAX_INLINE );
		  ;}
    break;

  case 111:
#line 386 "scan.y"
    {
		    InlineBuf = malloc( MAX_INLINE ); 
                    InlineLen = MAX_INLINE;
		    strcpy( InlineBuf, yyvsp[0].str);
		  ;}
    break;


    }

/* Line 991 of yacc.c.  */
#line 1833 "y.tab.c"

  yyvsp -= yylen;
  yyssp -= yylen;


  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;


  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if YYERROR_VERBOSE
      yyn = yypact[yystate];

      if (YYPACT_NINF < yyn && yyn < YYLAST)
	{
	  YYSIZE_T yysize = 0;
	  int yytype = YYTRANSLATE (yychar);
	  char *yymsg;
	  int yyx, yycount;

	  yycount = 0;
	  /* Start YYX at -YYN if negative to avoid negative indexes in
	     YYCHECK.  */
	  for (yyx = yyn < 0 ? -yyn : 0;
	       yyx < (int) (sizeof (yytname) / sizeof (char *)); yyx++)
	    if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	      yysize += yystrlen (yytname[yyx]) + 15, yycount++;
	  yysize += yystrlen ("syntax error, unexpected ") + 1;
	  yysize += yystrlen (yytname[yytype]);
	  yymsg = (char *) YYSTACK_ALLOC (yysize);
	  if (yymsg != 0)
	    {
	      char *yyp = yystpcpy (yymsg, "syntax error, unexpected ");
	      yyp = yystpcpy (yyp, yytname[yytype]);

	      if (yycount < 5)
		{
		  yycount = 0;
		  for (yyx = yyn < 0 ? -yyn : 0;
		       yyx < (int) (sizeof (yytname) / sizeof (char *));
		       yyx++)
		    if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
		      {
			const char *yyq = ! yycount ? ", expecting " : " or ";
			yyp = yystpcpy (yyp, yyq);
			yyp = yystpcpy (yyp, yytname[yyx]);
			yycount++;
		      }
		}
	      yyerror (yymsg);
	      YYSTACK_FREE (yymsg);
	    }
	  else
	    yyerror ("syntax error; also virtual memory exhausted");
	}
      else
#endif /* YYERROR_VERBOSE */
	yyerror ("syntax error");
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
	 error, discard it.  */

      /* Return failure if at end of input.  */
      if (yychar == YYEOF)
        {
	  /* Pop the error token.  */
          YYPOPSTACK;
	  /* Pop the rest of the stack.  */
	  while (yyss < yyssp)
	    {
	      YYDSYMPRINTF ("Error: popping", yystos[*yyssp], yyvsp, yylsp);
	      yydestruct (yystos[*yyssp], yyvsp);
	      YYPOPSTACK;
	    }
	  YYABORT;
        }

      YYDSYMPRINTF ("Error: discarding", yytoken, &yylval, &yylloc);
      yydestruct (yytoken, &yylval);
      yychar = YYEMPTY;

    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab2;


/*----------------------------------------------------.
| yyerrlab1 -- error raised explicitly by an action.  |
`----------------------------------------------------*/
yyerrlab1:

  /* Suppress GCC warning that yyerrlab1 is unused when no action
     invokes YYERROR.  */
#if defined (__GNUC_MINOR__) && 2093 <= (__GNUC__ * 1000 + __GNUC_MINOR__) \
    && !defined __cplusplus
  __attribute__ ((__unused__))
#endif


  goto yyerrlab2;


/*---------------------------------------------------------------.
| yyerrlab2 -- pop states until the error token can be shifted.  |
`---------------------------------------------------------------*/
yyerrlab2:
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (yyn != YYPACT_NINF)
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
	    {
	      yyn = yytable[yyn];
	      if (0 < yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
	YYABORT;

      YYDSYMPRINTF ("Error: popping", yystos[*yyssp], yyvsp, yylsp);
      yydestruct (yystos[yystate], yyvsp);
      yyvsp--;
      yystate = *--yyssp;

      YY_STACK_PRINT (yyss, yyssp);
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  YYDPRINTF ((stderr, "Shifting error token, "));

  *++yyvsp = yylval;


  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#ifndef yyoverflow
/*----------------------------------------------.
| yyoverflowlab -- parser overflow comes here.  |
`----------------------------------------------*/
yyoverflowlab:
  yyerror ("parser stack overflow");
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
  return yyresult;
}


#line 392 "scan.y"


void yyerror( char * str )
{
}

void ParserErrorMessage()
{
  /* yyerrok; */
/*
  Message("[%d,%s] -> [%d,%s]", crtTokType, crtToken, nextTokType, nextToken );  
*/
  if( crtToken[0] == ';' ) {
    ParserError("Misplaced ';'");
    return;
  }
  switch( crtTokType ) {
    case ATOMID:
      ParserError("Missing ';' after '%s'", crtToken );
      break; 

    case SPCSPC: 
      ParserError("Missing ';' or '+' after '%s'", crtToken );
      break; 
    case SPCNR:
      ParserError("Missing species after '%s'", crtToken );
      break; 
    case SPCPLUS:
      ParserError("Missing atom after '%s'", crtToken );
      break; 
    case SPCEQUAL:
      ParserError("Invalid '=' after '%s'", crtToken );
      break; 

    case INISPC: 
      ParserError("Missing '=' after '%s'", crtToken );
      break; 
    case INIEQUAL: 
      ParserError("Missing value after '%s'", crtToken );
      break; 
    case INIVALUE: 
      ParserError("Missing ';' after '%s'", crtToken );
      break; 

    case EQNSPC: 
      ParserError("Missing '+' or '=' after '%s'", crtToken );
      break; 
    case EQNEQUAL: 
      ParserError("Invalid right hand side of equation");
      break; 
    case EQNCOLON: 
      ParserError("Missing rate after '%s'", crtToken );
      break; 
    case EQNSIGN: 
      ParserError("Missing coeficient after '%s'", crtToken );
      break; 
    case EQNCOEF: 
      ParserError("Missing species after '%s'", crtToken );
      break; 
    case RATE: 
      ParserError("Missing ';' after '%s'", crtToken );
      break; 

    case LMPSPC: 
      ParserError("Missing '+' or ':' or ';' after '%s'", crtToken );
      break; 
    case LMPPLUS: 
      ParserError("Missing species after '%s'", crtToken );
      break; 
    case LMPCOLON: 
      ParserError("Missing species after '%s'", crtToken );
      break; 
    case INLINE:
      ParserError("Missing inline option after '%s'", crtToken );
      break;

    default:
      ParserError("Syntax error after '%s'", crtToken ); 
  }
}


int Parser( char * filename )
{
extern int yydebug;
FILE *f;

  crt_filename = filename;

  f = fopen( crt_filename, "r" );
  if( f == 0 ) {
    FatalError(7,"%s: File not found", crt_filename);
  } 
  
  yyin = f;
  nError   = 0;
  nWarning = 0;
  yydebug = 0;

  yyparse();

  fclose( f );

  return nError;
}          


