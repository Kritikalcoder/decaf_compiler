/* A Bison parser, made by GNU Bison 3.0.4.  */

/* Bison interface for Yacc-like parsers in C

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
