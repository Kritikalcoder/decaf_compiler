%{

#include <bits/stdc++.h>
#include "ast.h"
#include "parser.tab.hpp"
using namespace std;
int lineno = 0;
%}

hex_digit {digit}|[a-fA-F]
char   \'[ -~]\'
digit [0-9]
letter [a-zA-Z]
alpha [0-9a-zA-Z]
string \"(\\n|\\t|\\'|\\\\|\\\"|[^\\"'])*\"
delim [ \t]+

%%
"class"             {return CLASS;}
"if"                {return IF;}
"else"              {return ELSE;}
"for"               {return FOR;}
"int"               {yylval.value = strdup(yytext); return TYPE;}
"boolean"           {yylval.value = strdup(yytext); return TYPE;}
"void"              {yylval.value = strdup(yytext); return VOID;}
"break"             {return BREAK;}
"return"            {return RETURN;}
"continue"          {return CONTINUE;}
"callout"           {return CALLOUT;}
"true"              {yylval.value = strdup(yytext); return BOOL_LIT;}
"false"             {yylval.value = strdup(yytext); return BOOL_LIT;}
"0x"{hex_digit}+|{digit}+    {yylval.number = atoi(yytext); return INT_LIT;}
{letter}{alpha}*	{yylval.value = strdup(yytext); return ID;}
"("					{return OPEN_P;}
")"					{return CLOSE_P;}
"["					{return OPEN_SB;}
"]"					{return CLOSE_SB;}
"{"                 {return OPEN_CB;}
"}"                 {return CLOSE_CB;}
"+"					{yylval.value = strdup(yytext); return PLUS;}
"-"					{yylval.value = strdup(yytext); return MINUS;}
"*"					{yylval.value = strdup(yytext); return MULT;}
"/"					{yylval.value = strdup(yytext); return DIV;}
"!"                 {yylval.value = strdup(yytext); return NOT;}
"!="                {yylval.value = strdup(yytext); return NEQ;}
"="                 {yylval.value = strdup(yytext); return ASSIGN;}
"+="                {yylval.value = strdup(yytext); return PLUSEQ;}
"-="                {yylval.value = strdup(yytext); return MINUSEQ;}
"%"                 {yylval.value = strdup(yytext); return MOD;}
","                 {yylval.value = strdup(yytext); return COMMA;}
"<"                 {yylval.value = strdup(yytext); return LESS;}
"<="                {yylval.value = strdup(yytext); return LESSE;}
">"                 {yylval.value = strdup(yytext); return GREAT;}
">="                {yylval.value = strdup(yytext); return GREATE;}
"=="                {yylval.value = strdup(yytext); return EQ;}
"&&"                {yylval.value = strdup(yytext); return AND;}
"||"                {yylval.value = strdup(yytext); return OR;}
";"                 {yylval.value = strdup(yytext); return SEMICOLON;}
{char}              {yylval.value = strdup(yytext); return CHAR_LIT;}
{string}            {yylval.value = strdup(yytext); return STRING_LIT;}
{delim}				{ }
\n                  {lineno++;}
"//".*				{}		/* Inline comments So Ignore */
.					{printf("Line No %d: Unrecognized Character\n", lineno); }

%%
int yywrap (void) {return 1;}