/*
File name: table.h
Compiler: MS Visual Studio 2019
Author: Divine Omorogieva, ID# 040897820
Course: CST 8152 – Compilers, Lab Section: 013
Assignment: 1
Date: July 17th, 2020
Professor: Paulo Sousa
Purpose: Header file for the scanner program, include declaration of functions with constant definitions
Function list: aa_func02(), aa_func03(), aa_func05(), aa_func08(), aa_func11(), aa_func12()
*/


#ifndef  TABLE_H_
#define  TABLE_H_ 

#ifndef BUFFER_H_
#include "buffer.h"
#endif

#ifndef NULL
#include <_null.h> /* NULL pointer constant is defined there */
#endif

/*   Source end-of-file (SEOF) sentinel symbol
 *    '\0' or one of 255,0xFF,EOF
 *    This case it is \0
 */
#define SEOF '\0'

 /*If user decided to put EOF (255, -1, 0xFF) at the end of the buffer*/
 /*Use the value 255 since the character is unsigned*/
#define SEOF_255 255

/*  Special case tokens processed separately one by one
 *  in the token-driven part of the scanner
 *  '=' , ' ' , '(' , ')' , '{' , '}' , == , <> , '>' , '<' , ';',
 *  For '=' and '<' getc it, check the next character to see the next one, if not valid,
 *  white space
 *  !!comment , ',' , ';' , '-' , '+' , '*' , '/', << ,
 *  .AND., .OR. , SEOF,
 */


#define ES  11
#define ER  12 
#define IS -1

/* State transition table definition */
#define TABLE_COLUMNS 8
/*transition table - type of states defined in separate table */
int  st_table[][TABLE_COLUMNS] = {

	{1, 6, 4, ES, ES, 9, ES, ES},			
	{1, 1, 1, ES, 3, 2, 2, 2},
	{IS, IS, IS, IS, IS, IS, IS, IS},
	{IS, IS, IS, IS, IS, IS, IS, IS},
	{ES, 4, 4, 7, 5, 5, 5, 5},
	{IS, IS, IS, IS, IS, IS, IS, IS},
	{ES, 6, ES, 7, 5, 5, 5, 5},
	{8, 7, 7, 8, 8, 8, 8, 8},
	{IS, IS, IS, IS, IS, IS, IS, IS},
	{9, 9, 9, 9, 9, 10, ES, 9},
	{IS, IS, IS, IS, IS, IS, IS, IS},
	{IS, IS, IS, IS, IS, IS, IS, IS},
	{IS, IS, IS, IS, IS, IS, IS, IS},
	/* State 13 reserved for future use */
};

/* Accepting state table definition */
/*What value should be here ? - Not matter*/
#define ASWR 1    /* accepting state with retract */
#define ASNR 2    /* accepting state with no retract */
#define NOAS 3    /* not accepting state */

int as_table[] = {
	 NOAS, NOAS, ASWR, ASNR, NOAS, ASWR, NOAS, NOAS, ASWR, NOAS, ASNR, ASNR, ASWR
};

/* Accepting action function declarations */

/*FOR EACH OF YOUR ACCEPTING STATES YOU MUST PROVIDE
ONE FUNCTION PROTOTYPE. THEY ALL RETURN Token AND TAKE
ONE ARGUMENT: A string REPRESENTING A TOKEN LEXEME.

Token aa_funcXX(char *lexeme);

Replace XX with the number of the accepting state: 02, 03 and so on.

/* defining a new type: pointer to function (of one char * argument)
   returning Token
*/
typedef Token(*PTR_AAF)(char* lexeme);

Token aa_func02(char* lexeme);
Token aa_func03(char* lexeme);
Token aa_func05(char* lexeme);
Token aa_func08(char* lexeme);
Token aa_func10(char* lexeme);
Token aa_func11(char* lexeme);

/* Accepting function (action) callback table (array) definition */
/* If you do not want to use the typedef, the equvalent declaration is:
 * Token (*aa_table[])(char lexeme[]) = {
 */

PTR_AAF aa_table[] = {
	NULL, NULL, aa_func02, aa_func03, NULL, aa_func05, NULL, NULL, aa_func08, NULL, aa_func10, aa_func11, aa_func11
};

/* Keyword lookup table (.AND. and .OR. are not keywords) */

#define KWT_SIZE  10

char* kw_table[] =
{
"ELSE",
"FALSE",
"IF",
"PLATYPUS",
"READ",
"REPEAT",
"THEN",
"TRUE",
"WHILE",
"WRITE"
};

#endif
