/*
File name: parser.h
Compiler: MS Visual Studio 2019
Author: Minh Duc Pham, ID# 040905103
Course: CST 8152 – Compilers, Lab Section: 023
Assignment: 3
Date: December 5th, 2019
Professor: Sv. Ranev
Purpose: Header file for the buffer program, include declaration of functions with constant definitions
Function list:  parser(),  match(),  syn_eh(),  syn_printe(),  gen_incode()
                program(),  opt_statements(),  statements(),  statement(),
                statements_p(),  assignment_statement(),  assignment_exp(), 
			    selection_statement(),  iteration_statement(),  pre_condition(),
                input_statement(),  variable_list(),  variable_identifier(),
			    variable_list_p(),  output_statement(),  output_list(),
			    opt_variable_list(),  arithmetic_exp(),  unary_arithmetic_exp(),
			    additive_arithmetic_exp(),  additive_arithmetic_exp_p(), 
			    multiplicative_arithmetic_exp(),  multiplicative_arithmetic_exp_p(),
			    primary_arithmetic_exp(),  string_exp(),  primary_string_exp(),
			    string_exp_p(),  conditional_exp(),  logical_or_exp()
                logical_or_exp_p(),  logical_and_exp(),  logical_and_exp_p(),
                relational_exp(),  operator_list(),  primary_a_relational_exp(),
			    primary_s_relational_exp()
*/

#ifndef PARSER_H_
#define PARSER_H_

#include "buffer.h"
#include "token.h"

/*#pragma warning(1:4001) *//*to enforce C89 type comments  - to make //comments an warning */
/*#pragma warning(error:4001)*//* to enforce C89 comments - to make // comments an error */

/*Keyword constant*/
#define NO_ATTR -1

typedef enum Keywords {
	ELSE,
	IF,
	PLATYPUS,
	READ,
	REPEAT,
	THEN,
	WHILE,
	WRITE
} Key_W;


/*Global variables*/
static Token lookahead; /*look for the next Token*/
int synerrno; /*Syntax error counter*/

/*External variables*/
extern pBuffer str_LTBL; /*String literal table*/
extern int line; /*Current line number of source code in scanner.c*/
extern Token malar_next_token(void); /*Function to return next token in scanner.c*/
extern char* kw_table[]; /*Keyword lookup table in table.h*/

/*Function declaration*/
void parser(void);
void match(int pr_token_code, int pr_token_attribute);
void syn_eh(int sync_token_code);
void syn_printe();
void gen_incode(char*);
void program(void);
void opt_statements(void);
void statements(void);
void statement(void);
void statements_p(void);
void assignment_statement(void);
void assignment_exp(void);
void selection_statement(void);
void iteration_statement(void);
void pre_condition(void);
void input_statement(void);
void variable_list(void);
void variable_identifier(void);
void variable_list_p(void);
void output_statement(void);
void output_list(void);
void opt_variable_list(void);
void arithmetic_exp(void);
void unary_arithmetic_exp(void);
void additive_arithmetic_exp(void);
void additive_arithmetic_exp_p(void);
void multiplicative_arithmetic_exp(void);
void multiplicative_arithmetic_exp_p(void);
void primary_arithmetic_exp(void);
void string_exp(void);
void primary_string_exp(void);
void string_exp_p(void);
void conditional_exp(void);
void logical_or_exp(void);
void logical_or_exp_p(void);
void logical_and_exp(void);
void logical_and_exp_p(void);
void relational_exp(void);
void operator_list(void);
void primary_a_relational_exp(void);
void primary_s_relational_exp(void);
#endif