/* A Bison parser, made by GNU Bison 3.0.4.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.0.4"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* Copy the first part of user declarations.  */
#line 1 "parser.ypp" /* yacc.c:339  */

#include <bits/stdc++.h>	
#include "ast.h"
#include "parser.tab.hpp"
  void yyerror(char* s);
  extern int yylex();
  extern int yyparse();
  extern FILE *yyin;
  extern "C" int lineno;
  extern union ASTNode yylval;
  class Program* start = NULL; 
  int error_count=0;

#line 80 "parser.tab.cpp" /* yacc.c:339  */

# ifndef YY_NULLPTR
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULLPTR nullptr
#  else
#   define YY_NULLPTR 0
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* In a future release of Bison, this section will be replaced
   by #include "parser.tab.hpp".  */
#ifndef YY_YY_PARSER_TAB_HPP_INCLUDED
# define YY_YY_PARSER_TAB_HPP_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    CALLOUT = 258,
    TYPE = 259,
    VOID = 260,
    CLASS = 261,
    IF = 262,
    ELSE = 263,
    FOR = 264,
    RETURN = 265,
    BREAK = 266,
    CONTINUE = 267,
    COMMA = 268,
    SEMICOLON = 269,
    BOOL_LIT = 270,
    CHAR_LIT = 271,
    STRING_LIT = 272,
    ID = 273,
    INT_LIT = 274,
    OPEN_CB = 275,
    CLOSE_CB = 276,
    OPEN_SB = 277,
    CLOSE_SB = 278,
    OPEN_P = 279,
    CLOSE_P = 280,
    AND = 281,
    OR = 282,
    NOT = 283,
    PLUS = 284,
    MINUS = 285,
    MULT = 286,
    DIV = 287,
    MOD = 288,
    LESS = 289,
    GREAT = 290,
    LESSE = 291,
    GREATE = 292,
    EQ = 293,
    NEQ = 294,
    ASSIGN = 295,
    PLUSEQ = 296,
    MINUSEQ = 297
  };
#endif

/* Value type.  */


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_PARSER_TAB_HPP_INCLUDED  */

/* Copy the second part of user declarations.  */

#line 169 "parser.tab.cpp" /* yacc.c:358  */

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif

#ifndef YY_ATTRIBUTE
# if (defined __GNUC__                                               \
      && (2 < __GNUC__ || (__GNUC__ == 2 && 96 <= __GNUC_MINOR__)))  \
     || defined __SUNPRO_C && 0x5110 <= __SUNPRO_C
#  define YY_ATTRIBUTE(Spec) __attribute__(Spec)
# else
#  define YY_ATTRIBUTE(Spec) /* empty */
# endif
#endif

#ifndef YY_ATTRIBUTE_PURE
# define YY_ATTRIBUTE_PURE   YY_ATTRIBUTE ((__pure__))
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# define YY_ATTRIBUTE_UNUSED YY_ATTRIBUTE ((__unused__))
#endif

#if !defined _Noreturn \
     && (!defined __STDC_VERSION__ || __STDC_VERSION__ < 201112)
# if defined _MSC_VER && 1200 <= _MSC_VER
#  define _Noreturn __declspec (noreturn)
# else
#  define _Noreturn YY_ATTRIBUTE ((__noreturn__))
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN \
    _Pragma ("GCC diagnostic push") \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")\
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif


#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYSIZE_T yynewbytes;                                            \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / sizeof (*yyptr);                          \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, (Count) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYSIZE_T yyi;                         \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  4
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   277

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  43
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  25
/* YYNRULES -- Number of rules.  */
#define YYNRULES  72
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  147

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   297

#define YYTRANSLATE(YYX)                                                \
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, without out-of-bounds checking.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
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
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint8 yyrline[] =
{
       0,    71,    71,    77,    78,    84,    87,    91,    97,    98,
     101,   102,   108,   109,   112,   113,   120,   121,   128,   131,
     132,   138,   141,   142,   148,   149,   155,   156,   157,   158,
     159,   160,   161,   162,   163,   164,   167,   168,   169,   172,
     173,   176,   177,   183,   184,   190,   191,   197,   198,   201,
     202,   205,   206,   207,   208,   209,   210,   211,   212,   213,
     214,   215,   216,   217,   218,   219,   220,   221,   222,   223,
     226,   227,   228
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 0
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "CALLOUT", "TYPE", "VOID", "CLASS", "IF",
  "ELSE", "FOR", "RETURN", "BREAK", "CONTINUE", "COMMA", "SEMICOLON",
  "BOOL_LIT", "CHAR_LIT", "STRING_LIT", "ID", "INT_LIT", "OPEN_CB",
  "CLOSE_CB", "OPEN_SB", "CLOSE_SB", "OPEN_P", "CLOSE_P", "AND", "OR",
  "NOT", "PLUS", "MINUS", "MULT", "DIV", "MOD", "LESS", "GREAT", "LESSE",
  "GREATE", "EQ", "NEQ", "ASSIGN", "PLUSEQ", "MINUSEQ", "$accept",
  "program", "field_decl_list", "field_decl", "field_decl_var_list",
  "field_decl_var", "method_decl_list", "method_decl", "method_arg_list",
  "method_arg", "block", "var_decl_list", "var_decl", "var_list",
  "statement_list", "statement", "assign_op", "method_call",
  "method_call_arg_list", "method_call_arg", "callout_arg_list",
  "callout_arg", "location", "expr", "literal", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297
};
# endif

#define YYPACT_NINF -64

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-64)))

#define YYTABLE_NINF -1

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
      -1,     7,    45,    34,   -64,   -64,    42,    40,    48,   -64,
      46,    57,     5,    58,    60,    51,   -64,    56,   -64,    59,
      76,   -64,    63,    76,    64,    71,    69,    75,    79,   -64,
      78,   -64,    83,    85,    85,   102,   -64,   -64,   -64,   -64,
      89,   106,    83,    93,   -64,     3,   -64,   -64,    50,   133,
     146,   108,    41,   157,   158,     6,   -64,   -64,   -64,   159,
     -38,   156,   -64,   160,    74,   150,   -64,   -64,   -64,   -64,
      74,    74,    74,   -64,   -64,   115,   -64,   -64,   -64,    74,
      74,   -64,   -64,   -64,   -64,    74,   -64,   178,   181,    74,
     196,   -64,   -13,   -64,    74,    74,    74,    74,    74,    74,
      74,    74,    74,    74,    74,    74,    74,   166,   169,    86,
     129,    67,   184,    85,   101,   -64,   233,   233,   -13,   -13,
     -64,   -64,   -64,    20,    20,    20,    20,   224,   224,   -64,
     -64,    74,   -64,   -64,   -64,   178,   210,   -64,   216,    74,
      86,   -64,    85,   149,   -64,   -64,   -64
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,     0,     0,     0,     1,     3,    10,     0,     0,     4,
       0,    10,     8,     0,     6,     0,     2,     0,    11,     0,
      14,     5,     0,    14,     0,     0,     0,     0,     8,     7,
       0,     9,    16,     0,     0,     0,    15,    19,    12,    13,
       0,    24,    16,     0,    20,     0,    17,    22,     0,     0,
       0,     0,     0,     0,     0,    49,    18,    35,    25,     0,
       0,     0,    21,     0,     0,     0,    31,    72,    71,    70,
       0,     0,     0,    52,    51,     0,    53,    33,    34,     0,
      41,    27,    36,    37,    38,     0,    23,    45,     0,     0,
       0,    68,    67,    32,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    43,
       0,     0,     0,     0,     0,    69,    66,    65,    54,    55,
      56,    57,    58,    59,    60,    61,    62,    63,    64,    50,
      39,     0,    42,    26,    48,    45,    47,    40,    28,     0,
      43,    46,     0,     0,    44,    29,    30
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
     -64,   -64,   -64,   -64,   230,   -64,   227,   -64,   248,   231,
     -34,   -64,   -64,   -64,   -64,   -64,   -64,   229,   -64,   132,
     140,   -64,   232,   -63,   -64
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     2,     6,     9,    13,    14,    10,    11,    27,    36,
      38,    41,    44,    48,    45,    58,    85,    73,   108,   132,
     112,   135,    74,    75,    76
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_uint8 yytable[] =
{
      39,    88,    82,    83,    84,     1,    49,    90,    91,    92,
      50,    57,    51,    52,    53,    54,   107,   109,    98,    99,
     100,    55,   110,    37,    56,     3,   114,    19,    79,    20,
      80,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,   128,    49,     4,     7,     8,   136,    96,
      97,    98,    99,   100,     5,    66,    67,    68,    12,    55,
      69,    17,     8,    61,    62,    70,    15,    16,   140,    71,
      49,    72,    21,    22,    24,    23,   143,    49,    25,   138,
      26,    28,    67,    68,   134,    55,    69,    32,    20,    67,
      68,    70,    55,    69,    31,    71,    35,    72,    70,   131,
      33,    19,    71,    34,    72,    37,    40,    42,   145,   146,
      43,    47,    94,    95,   139,    96,    97,    98,    99,   100,
     101,   102,   103,   104,   105,   106,    65,    94,    95,    93,
      96,    97,    98,    99,   100,   101,   102,   103,   104,   105,
     106,    94,    95,   133,    96,    97,    98,    99,   100,   101,
     102,   103,   104,   105,   106,    94,    95,    63,    96,    97,
      98,    99,   100,   101,   102,   103,   104,   105,   106,    37,
      64,    77,    78,    81,    86,    94,    95,    87,    96,    97,
      98,    99,   100,   101,   102,   103,   104,   105,   106,   129,
      89,   111,    94,    95,   130,    96,    97,    98,    99,   100,
     101,   102,   103,   104,   105,   106,   113,    94,    95,   137,
      96,    97,    98,    99,   100,   101,   102,   103,   104,   105,
     106,   115,    94,    95,   142,    96,    97,    98,    99,   100,
     101,   102,   103,   104,   105,   106,    94,    95,    18,    96,
      97,    98,    99,   100,   101,   102,   103,   104,   105,   106,
      94,    95,    29,    96,    97,    98,    99,   100,   101,   102,
     103,   104,    96,    97,    98,    99,   100,   101,   102,   103,
     104,    30,   144,    46,    59,   141,     0,    60
};

static const yytype_int16 yycheck[] =
{
      34,    64,    40,    41,    42,     6,     3,    70,    71,    72,
       7,    45,     9,    10,    11,    12,    79,    80,    31,    32,
      33,    18,    85,    20,    21,    18,    89,    22,    22,    24,
      24,    94,    95,    96,    97,    98,    99,   100,   101,   102,
     103,   104,   105,   106,     3,     0,     4,     5,   111,    29,
      30,    31,    32,    33,    20,    14,    15,    16,    18,    18,
      19,     4,     5,    13,    14,    24,    18,    21,   131,    28,
       3,    30,    14,    13,    18,    24,   139,     3,    19,   113,
       4,    18,    15,    16,    17,    18,    19,    18,    24,    15,
      16,    24,    18,    19,    23,    28,    13,    30,    24,    13,
      25,    22,    28,    25,    30,    20,     4,    18,   142,   143,
       4,    18,    26,    27,    13,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    18,    26,    27,    14,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    26,    27,    14,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    26,    27,    24,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    20,
      24,    14,    14,    14,    18,    26,    27,    17,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    23,
      40,    13,    26,    27,    25,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    25,    26,    27,    25,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    25,    26,    27,     8,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    26,    27,    11,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      26,    27,    22,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    23,   140,    42,    45,   135,    -1,    45
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     6,    44,    18,     0,    20,    45,     4,     5,    46,
      49,    50,    18,    47,    48,    18,    21,     4,    49,    22,
      24,    14,    13,    24,    18,    19,     4,    51,    18,    47,
      51,    23,    18,    25,    25,    13,    52,    20,    53,    53,
       4,    54,    18,     4,    55,    57,    52,    18,    56,     3,
       7,     9,    10,    11,    12,    18,    21,    53,    58,    60,
      65,    13,    14,    24,    24,    18,    14,    15,    16,    19,
      24,    28,    30,    60,    65,    66,    67,    14,    14,    22,
      24,    14,    40,    41,    42,    59,    18,    17,    66,    40,
      66,    66,    66,    14,    26,    27,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    66,    61,    66,
      66,    13,    63,    25,    66,    25,    66,    66,    66,    66,
      66,    66,    66,    66,    66,    66,    66,    66,    66,    23,
      25,    13,    62,    14,    17,    64,    66,    25,    53,    13,
      66,    63,     8,    66,    62,    53,    53
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    43,    44,    45,    45,    46,    47,    47,    48,    48,
      49,    49,    50,    50,    51,    51,    52,    52,    53,    54,
      54,    55,    56,    56,    57,    57,    58,    58,    58,    58,
      58,    58,    58,    58,    58,    58,    59,    59,    59,    60,
      60,    61,    61,    62,    62,    63,    63,    64,    64,    65,
      65,    66,    66,    66,    66,    66,    66,    66,    66,    66,
      66,    66,    66,    66,    66,    66,    66,    66,    66,    66,
      67,    67,    67
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     6,     0,     2,     3,     1,     3,     1,     4,
       0,     2,     6,     6,     0,     3,     0,     4,     4,     0,
       2,     3,     1,     3,     0,     2,     4,     2,     5,     7,
       7,     2,     3,     2,     2,     1,     1,     1,     1,     4,
       5,     0,     2,     0,     3,     0,     3,     1,     1,     1,
       4,     1,     1,     1,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     2,     2,     3,
       1,     1,     1
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                  \
do                                                              \
  if (yychar == YYEMPTY)                                        \
    {                                                           \
      yychar = (Token);                                         \
      yylval = (Value);                                         \
      YYPOPSTACK (yylen);                                       \
      yystate = *yyssp;                                         \
      goto yybackup;                                            \
    }                                                           \
  else                                                          \
    {                                                           \
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;                                                  \
    }                                                           \
while (0)

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256



/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)

/* This macro is provided for backward compatibility. */
#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*----------------------------------------.
| Print this symbol's value on YYOUTPUT.  |
`----------------------------------------*/

static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  FILE *yyo = yyoutput;
  YYUSE (yyo);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
  YYUSE (yytype);
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyoutput, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yytype_int16 *yyssp, YYSTYPE *yyvsp, int yyrule)
{
  unsigned long int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       yystos[yyssp[yyi + 1 - yynrhs]],
                       &(yyvsp[(yyi + 1) - (yynrhs)])
                                              );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
yystrlen (const char *yystr)
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            /* Fall through.  */
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYSIZE_T *yymsg_alloc, char **yymsg,
                yytype_int16 *yyssp, int yytoken)
{
  YYSIZE_T yysize0 = yytnamerr (YY_NULLPTR, yytname[yytoken]);
  YYSIZE_T yysize = yysize0;
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat. */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Number of reported tokens (one for the "unexpected", one per
     "expected"). */
  int yycount = 0;

  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[*yyssp];
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYSIZE_T yysize1 = yysize + yytnamerr (YY_NULLPTR, yytname[yyx]);
                  if (! (yysize <= yysize1
                         && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
                    return 2;
                  yysize = yysize1;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    YYSIZE_T yysize1 = yysize + yystrlen (yyformat);
    if (! (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
      return 2;
    yysize = yysize1;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          yyp++;
          yyformat++;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
{
  YYUSE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}




/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;


/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        YYSTYPE *yyvs1 = yyvs;
        yytype_int16 *yyss1 = yyss;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * sizeof (*yyssp),
                    &yyvs1, yysize * sizeof (*yyvsp),
                    &yystacksize);

        yyss = yyss1;
        yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yytype_int16 *yyss1 = yyss;
        union yyalloc *yyptr =
          (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
        if (! yyptr)
          goto yyexhaustedlab;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
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

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

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
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 71 "parser.ypp" /* yacc.c:1646  */
    { 
		(yyval.prog) = new Program(string((yyvsp[-4].value)), (yyvsp[-2].fields), (yyvsp[-1].methods));
		start = (yyval.prog); 
	}
#line 1376 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 3:
#line 77 "parser.ypp" /* yacc.c:1646  */
    { (yyval.fields) = new Field_decl_list(); }
#line 1382 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 4:
#line 78 "parser.ypp" /* yacc.c:1646  */
    {
		(yyvsp[-1].fields)->append((yyvsp[0].field_decl));
		(yyval.fields) = (yyvsp[-1].fields);
	}
#line 1391 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 5:
#line 84 "parser.ypp" /* yacc.c:1646  */
    { (yyval.field_decl) = new Field_decl(string((yyvsp[-2].value)), (yyvsp[-1].field_decl_var_list)); }
#line 1397 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 6:
#line 87 "parser.ypp" /* yacc.c:1646  */
    {
		(yyval.field_decl_var_list) = new Field_decl_var_list();
		(yyval.field_decl_var_list)->append((yyvsp[0].field_decl_var));
	}
#line 1406 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 7:
#line 91 "parser.ypp" /* yacc.c:1646  */
    {
		(yyvsp[0].field_decl_var_list)->append((yyvsp[-2].field_decl_var));
		(yyval.field_decl_var_list) = (yyvsp[0].field_decl_var_list);
	}
#line 1415 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 8:
#line 97 "parser.ypp" /* yacc.c:1646  */
    { (yyval.field_decl_var) = new Field_decl_var(string((yyvsp[0].value)), string("var")); }
#line 1421 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 9:
#line 98 "parser.ypp" /* yacc.c:1646  */
    { (yyval.field_decl_var) = new Field_decl_var(string((yyvsp[-3].value)), string("array"), (yyvsp[-1].number)); }
#line 1427 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 10:
#line 101 "parser.ypp" /* yacc.c:1646  */
    { (yyval.methods) = new Method_decl_list(); }
#line 1433 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 11:
#line 102 "parser.ypp" /* yacc.c:1646  */
    {
		(yyvsp[0].methods)->append((yyvsp[-1].method_decl));
		(yyval.methods) = (yyvsp[0].methods);
	}
#line 1442 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 12:
#line 108 "parser.ypp" /* yacc.c:1646  */
    { (yyval.method_decl) = new Method_decl(string((yyvsp[-5].value)), string((yyvsp[-4].value)), (yyvsp[-2].method_arg_list), (yyvsp[0].block)); }
#line 1448 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 13:
#line 109 "parser.ypp" /* yacc.c:1646  */
    { (yyval.method_decl) = new Method_decl(string((yyvsp[-5].value)), string((yyvsp[-4].value)), (yyvsp[-2].method_arg_list), (yyvsp[0].block)); }
#line 1454 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 14:
#line 112 "parser.ypp" /* yacc.c:1646  */
    { (yyval.method_arg_list) = new Method_arg_list(); }
#line 1460 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 15:
#line 113 "parser.ypp" /* yacc.c:1646  */
    {
		Method_arg *arg = new Method_arg(string((yyvsp[-2].value)), string((yyvsp[-1].value)));
		(yyvsp[0].method_arg_list)->append(arg);
		(yyval.method_arg_list) = (yyvsp[0].method_arg_list);
	}
#line 1470 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 16:
#line 120 "parser.ypp" /* yacc.c:1646  */
    { (yyval.method_arg_list) = new Method_arg_list(); }
#line 1476 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 17:
#line 121 "parser.ypp" /* yacc.c:1646  */
    {
		Method_arg *arg = new Method_arg(string((yyvsp[-2].value)), string((yyvsp[-1].value)));
		(yyvsp[0].method_arg_list)->append(arg);
		(yyval.method_arg_list) = (yyvsp[0].method_arg_list);
	}
#line 1486 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 18:
#line 128 "parser.ypp" /* yacc.c:1646  */
    { (yyval.block) = new Block((yyvsp[-2].var_decl_list), (yyvsp[-1].statement_list)); }
#line 1492 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 19:
#line 131 "parser.ypp" /* yacc.c:1646  */
    { (yyval.var_decl_list) = new Variable_decl_list(); }
#line 1498 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 20:
#line 132 "parser.ypp" /* yacc.c:1646  */
    {
		(yyvsp[-1].var_decl_list)->append((yyvsp[0].var_decl));
		(yyval.var_decl_list) = (yyvsp[-1].var_decl_list);
	}
#line 1507 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 21:
#line 138 "parser.ypp" /* yacc.c:1646  */
    { (yyval.var_decl) = new Variable_decl(string((yyvsp[-2].value)), (yyvsp[-1].var_list)); }
#line 1513 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 22:
#line 141 "parser.ypp" /* yacc.c:1646  */
    { (yyval.var_list) = new Variable_list(string((yyvsp[0].value))); }
#line 1519 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 23:
#line 142 "parser.ypp" /* yacc.c:1646  */
    {
		(yyvsp[-2].var_list)->append(string((yyvsp[0].value)));
		(yyval.var_list) = (yyvsp[-2].var_list);
	}
#line 1528 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 24:
#line 148 "parser.ypp" /* yacc.c:1646  */
    { (yyval.statement_list) = new Statement_list(); }
#line 1534 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 25:
#line 149 "parser.ypp" /* yacc.c:1646  */
    {
		(yyvsp[-1].statement_list)->append((yyvsp[0].statement));
		(yyval.statement_list) = (yyvsp[-1].statement_list);
	}
#line 1543 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 26:
#line 155 "parser.ypp" /* yacc.c:1646  */
    { (yyval.statement) = new Assign_statement((yyvsp[-3].loc), string((yyvsp[-2].value)), (yyvsp[-1].expr)); }
#line 1549 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 27:
#line 156 "parser.ypp" /* yacc.c:1646  */
    { (yyval.statement) = (yyvsp[-1].method_call); }
#line 1555 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 28:
#line 157 "parser.ypp" /* yacc.c:1646  */
    { (yyval.statement) = new If_statement((yyvsp[-2].expr), (yyvsp[0].block)); }
#line 1561 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 29:
#line 158 "parser.ypp" /* yacc.c:1646  */
    { (yyval.statement) = new If_else_statement((yyvsp[-4].expr), (yyvsp[-2].block), (yyvsp[0].block)); }
#line 1567 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 30:
#line 159 "parser.ypp" /* yacc.c:1646  */
    { (yyval.statement) = new For_statement(string((yyvsp[-5].value)), (yyvsp[-3].expr), (yyvsp[-1].expr), (yyvsp[0].block)); }
#line 1573 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 31:
#line 160 "parser.ypp" /* yacc.c:1646  */
    { (yyval.statement) = new Return_statement(); }
#line 1579 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 32:
#line 161 "parser.ypp" /* yacc.c:1646  */
    { (yyval.statement) = new Return_statement((yyvsp[-1].expr)); }
#line 1585 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 33:
#line 162 "parser.ypp" /* yacc.c:1646  */
    { (yyval.statement) = new Break_statement(); }
#line 1591 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 34:
#line 163 "parser.ypp" /* yacc.c:1646  */
    { (yyval.statement) = new Continue_statement(); }
#line 1597 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 35:
#line 164 "parser.ypp" /* yacc.c:1646  */
    { (yyval.statement) = (yyvsp[0].block); }
#line 1603 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 36:
#line 167 "parser.ypp" /* yacc.c:1646  */
    { (yyval.value) = (yyvsp[0].value); }
#line 1609 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 37:
#line 168 "parser.ypp" /* yacc.c:1646  */
    { (yyval.value) = (yyvsp[0].value); }
#line 1615 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 38:
#line 169 "parser.ypp" /* yacc.c:1646  */
    { (yyval.value) = (yyvsp[0].value); }
#line 1621 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 39:
#line 172 "parser.ypp" /* yacc.c:1646  */
    { (yyval.method_call) = new Normal_call(string((yyvsp[-3].value)), (yyvsp[-1].method_call_arg_list)); }
#line 1627 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 40:
#line 173 "parser.ypp" /* yacc.c:1646  */
    { (yyval.method_call) = new Callout_call(string((yyvsp[-2].value)), (yyvsp[-1].callout_arg_list)); }
#line 1633 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 41:
#line 176 "parser.ypp" /* yacc.c:1646  */
    { (yyval.method_call_arg_list) = new Method_call_arg_list(); }
#line 1639 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 42:
#line 177 "parser.ypp" /* yacc.c:1646  */
    {
		(yyvsp[0].method_call_arg_list)->append((yyvsp[-1].expr));
		(yyval.method_call_arg_list) = (yyvsp[0].method_call_arg_list);
	}
#line 1648 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 43:
#line 183 "parser.ypp" /* yacc.c:1646  */
    { (yyval.method_call_arg_list) = new Method_call_arg_list(); }
#line 1654 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 44:
#line 184 "parser.ypp" /* yacc.c:1646  */
    {
		(yyvsp[0].method_call_arg_list)->append((yyvsp[-1].expr));
		(yyval.method_call_arg_list) = (yyvsp[0].method_call_arg_list);
	}
#line 1663 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 45:
#line 190 "parser.ypp" /* yacc.c:1646  */
    { (yyval.callout_arg_list) = new Callout_arg_list();  }
#line 1669 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 46:
#line 191 "parser.ypp" /* yacc.c:1646  */
    {
		(yyvsp[0].callout_arg_list)->append((yyvsp[-1].callout_arg));
		(yyval.callout_arg_list) = (yyvsp[0].callout_arg_list);
	}
#line 1678 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 47:
#line 197 "parser.ypp" /* yacc.c:1646  */
    { (yyval.callout_arg) = new Callout_arg((yyvsp[0].expr)); }
#line 1684 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 48:
#line 198 "parser.ypp" /* yacc.c:1646  */
    { (yyval.callout_arg) = new Callout_arg(string((yyvsp[0].value))); }
#line 1690 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 49:
#line 201 "parser.ypp" /* yacc.c:1646  */
    { (yyval.loc) = new Location(string((yyvsp[0].value)), string("var")); }
#line 1696 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 50:
#line 202 "parser.ypp" /* yacc.c:1646  */
    { (yyval.loc) = new Location(string((yyvsp[-3].value)), string("array"), (yyvsp[-1].expr));  }
#line 1702 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 51:
#line 205 "parser.ypp" /* yacc.c:1646  */
    { (yyval.expr) = (yyvsp[0].loc); }
#line 1708 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 52:
#line 206 "parser.ypp" /* yacc.c:1646  */
    { (yyval.expr) = (yyvsp[0].method_call); }
#line 1714 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 53:
#line 207 "parser.ypp" /* yacc.c:1646  */
    { (yyval.expr) = (yyvsp[0].lit); }
#line 1720 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 54:
#line 208 "parser.ypp" /* yacc.c:1646  */
    { (yyval.expr) = new Binary_expr((yyvsp[-2].expr), string((yyvsp[-1].value)), (yyvsp[0].expr)); }
#line 1726 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 55:
#line 209 "parser.ypp" /* yacc.c:1646  */
    { (yyval.expr) = new Binary_expr((yyvsp[-2].expr), string((yyvsp[-1].value)), (yyvsp[0].expr)); }
#line 1732 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 56:
#line 210 "parser.ypp" /* yacc.c:1646  */
    { (yyval.expr) = new Binary_expr((yyvsp[-2].expr), string((yyvsp[-1].value)), (yyvsp[0].expr)); }
#line 1738 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 57:
#line 211 "parser.ypp" /* yacc.c:1646  */
    { (yyval.expr) = new Binary_expr((yyvsp[-2].expr), string((yyvsp[-1].value)), (yyvsp[0].expr)); }
#line 1744 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 58:
#line 212 "parser.ypp" /* yacc.c:1646  */
    { (yyval.expr) = new Binary_expr((yyvsp[-2].expr), string((yyvsp[-1].value)), (yyvsp[0].expr)); }
#line 1750 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 59:
#line 213 "parser.ypp" /* yacc.c:1646  */
    { (yyval.expr) = new Binary_expr((yyvsp[-2].expr), string((yyvsp[-1].value)), (yyvsp[0].expr)); }
#line 1756 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 60:
#line 214 "parser.ypp" /* yacc.c:1646  */
    { (yyval.expr) = new Binary_expr((yyvsp[-2].expr), string((yyvsp[-1].value)), (yyvsp[0].expr)); }
#line 1762 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 61:
#line 215 "parser.ypp" /* yacc.c:1646  */
    { (yyval.expr) = new Binary_expr((yyvsp[-2].expr), string((yyvsp[-1].value)), (yyvsp[0].expr)); }
#line 1768 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 62:
#line 216 "parser.ypp" /* yacc.c:1646  */
    { (yyval.expr) = new Binary_expr((yyvsp[-2].expr), string((yyvsp[-1].value)), (yyvsp[0].expr)); }
#line 1774 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 63:
#line 217 "parser.ypp" /* yacc.c:1646  */
    { (yyval.expr) = new Binary_expr((yyvsp[-2].expr), string((yyvsp[-1].value)), (yyvsp[0].expr)); }
#line 1780 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 64:
#line 218 "parser.ypp" /* yacc.c:1646  */
    { (yyval.expr) = new Binary_expr((yyvsp[-2].expr), string((yyvsp[-1].value)), (yyvsp[0].expr)); }
#line 1786 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 65:
#line 219 "parser.ypp" /* yacc.c:1646  */
    { (yyval.expr) = new Binary_expr((yyvsp[-2].expr), string((yyvsp[-1].value)), (yyvsp[0].expr)); }
#line 1792 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 66:
#line 220 "parser.ypp" /* yacc.c:1646  */
    { (yyval.expr) = new Binary_expr((yyvsp[-2].expr), string((yyvsp[-1].value)), (yyvsp[0].expr)); }
#line 1798 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 67:
#line 221 "parser.ypp" /* yacc.c:1646  */
    { (yyval.expr) = new Unary_expr(string((yyvsp[-1].value)), (yyvsp[0].expr)); }
#line 1804 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 68:
#line 222 "parser.ypp" /* yacc.c:1646  */
    { (yyval.expr) = new Unary_expr(string((yyvsp[-1].value)), (yyvsp[0].expr)); }
#line 1810 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 69:
#line 223 "parser.ypp" /* yacc.c:1646  */
    { (yyval.expr) = (yyvsp[-1].expr); }
#line 1816 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 70:
#line 226 "parser.ypp" /* yacc.c:1646  */
    {(yyval.lit) = new Int_literal((yyvsp[0].number));}
#line 1822 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 71:
#line 227 "parser.ypp" /* yacc.c:1646  */
    {(yyval.lit) = new Char_literal(string((yyvsp[0].value)));}
#line 1828 "parser.tab.cpp" /* yacc.c:1646  */
    break;

  case 72:
#line 228 "parser.ypp" /* yacc.c:1646  */
    {(yyval.lit) = new Bool_literal(string((yyvsp[0].value)));}
#line 1834 "parser.tab.cpp" /* yacc.c:1646  */
    break;


#line 1838 "parser.tab.cpp" /* yacc.c:1646  */
      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = (char *) YYSTACK_ALLOC (yymsg_alloc);
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
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


      yydestruct ("Error: popping",
                  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

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

#if !defined yyoverflow || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  return yyresult;
}
#line 230 "parser.ypp" /* yacc.c:1906  */


int main(int argc, char **argv)
{
	FILE *input = fopen(argv[1], "r");

	if (input == NULL){
		printf("Can't open file\n");
		exit(-1);
	}
	yyin = input;
	do {
		yyparse();
	} while (!feof(yyin));

	// printf("Parsing done");
	if(start) {
    /* code generation */
		// cout << "Generating code" << endl;
		start->generate_IR();
	}
	if (error_count == 0) {
		start->generate_code_dump();
	}
	// printf("Parsing Over\n");
}

void yyerror(char *s)
{
	fprintf(stderr, "error: %s\n", s);
}
