/*
File name: parser.c
Compiler: MS Visual Studio 2019
Author: Divine Omorogieva, ID# 040897820
Course: CST 8152 – Compilers, Lab Section: 013
Assignment: 3
Date: August 8th, 2020
Professor: Paulo Sousa
Purpose: A recursive descent predictive parser for the PLATYPUS language
Function list: parser(), match(), syn_eh(), syn_printe(), gen_incode(), program(), 
               opt_statements(),  statements(),  statement(), statements_p(),  
			   assignment_statement(),  assignment_exp(), selection_statement(),  
			   iteration_statement(),  pre_condition(), input_statement(),
			   variable_list(),  variable_identifier(),variable_list_p(),
			   output_statement(),  output_list(), opt_variable_list(),
			   arithmetic_exp(),  unary_arithmetic_exp(), additive_arithmetic_exp(),
			   additive_arithmetic_exp_p(), multiplicative_arithmetic_exp(),
			   multiplicative_arithmetic_exp_p(), primary_arithmetic_exp(),
			   string_exp(),  primary_string_exp(), string_exp_p(),
			   conditional_exp(),  logical_or_exp(), logical_or_exp_p(),
			   logical_and_exp(),  logical_and_exp_p(), relational_exp(),
			   operator_list(),  primary_a_relational_exp(), primary_s_relational_exp()
*/

#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>   /* standard input / output */
#include <stdlib.h>  /* standard library functions and constants */

/*#define NDEBUG        to suppress assert() call */
#include <assert.h>  /* assert() prototype */

/* project header files */
#include "parser.h"

void parser(void) {
	lookahead = malar_next_token();
	program(); match(SEOF_T, NO_ATTR);
	gen_incode("PLATY: Source file parsed");
}

void match(int pr_token_code, int pr_token_attribute) {

	/*Unsuccessful match*/
	if (lookahead.code != pr_token_code) {
		syn_eh(pr_token_code);
		return;
	}

	/*Token checked with attribute*/
	switch (pr_token_code) {
	case KW_T: case LOG_OP_T: case ART_OP_T: case REL_OP_T:
		if (lookahead.attribute.get_int != pr_token_attribute) {
			syn_eh(pr_token_code);
			return;
		}
	}

	/*SEOF_T match*/
	if (lookahead.code == SEOF_T)
		return;

	/*Advances to next token if match is successful and not SEOF_T*/
	lookahead = malar_next_token();

	/*ERR_T match*/
	if (lookahead.code == ERR_T) {
		syn_printe();
		lookahead = malar_next_token();
		++synerrno;
	}
}

void syn_eh(int sync_token_code) {
	syn_printe();
	++synerrno;

	/*Advancing for the right token*/
	while (lookahead.code != sync_token_code) {
		/*Check for SEOF_T*/
		if (lookahead.code == SEOF_T) 
				exit(synerrno);
		
		/*Get the next token*/
		lookahead = malar_next_token();
	} 

	/*Found the token and not SEOF_T, advance the code and return*/
	if (sync_token_code != SEOF_T)
		lookahead = malar_next_token();
	return;
}

/* error printing function for Assignment 3 (Parser), S20 */
void syn_printe() {
	Token t = lookahead;

	printf("PLATY: Syntax error:  Line:%3d\n", line);
	printf("*****  Token code:%3d Attribute: ", t.code);
	switch (t.code) {
	case  ERR_T: /* ERR_T     0   Error token */
		printf("%s\n", t.attribute.err_lex);
		break;
	case  SEOF_T: /*SEOF_T    1   Source end-of-file token */
		printf("SEOF_T\t\t%d\t\n", t.attribute.seof);
		break;
	case  AVID_T: /* AVID_T    2   Arithmetic Variable identifier token */
	case  SVID_T:/* SVID_T    3  String Variable identifier token */
		printf("%s\n", t.attribute.vid_lex);
		break;
	case  FPL_T: /* FPL_T     4  Floating point literal token */
		printf("%5.1f\n", t.attribute.flt_value);
		break;
	case INL_T: /* INL_T      5   Integer literal token */
		printf("%d\n", t.attribute.get_int);
		break;
	case STR_T:/* STR_T     6   String literal token */
		printf("%s\n", b_location(str_LTBL, t.attribute.str_offset));
		break;

	case SCC_OP_T: /* 7   String concatenation operator token */
		printf("NA\n");
		break;

	case  ASS_OP_T:/* ASS_OP_T  8   Assignment operator token */
		printf("NA\n");
		break;
	case  ART_OP_T:/* ART_OP_T  9   Arithmetic operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  REL_OP_T: /*REL_OP_T  10   Relational operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  LOG_OP_T:/*LOG_OP_T 11  Logical operator token */
		printf("%d\n", t.attribute.get_int);
		break;

	case  LPR_T: /*LPR_T    12  Left parenthesis token */
		printf("NA\n");
		break;
	case  RPR_T: /*RPR_T    13  Right parenthesis token */
		printf("NA\n");
		break;
	case LBR_T: /*    14   Left brace token */
		printf("NA\n");
		break;
	case RBR_T: /*    15  Right brace token */
		printf("NA\n");
		break;

	case KW_T: /*     16   Keyword token */
		printf("%s\n", kw_table[t.attribute.get_int]);
		break;

	case COM_T: /* 17   Comma token */
		printf("NA\n");
		break;
	case EOS_T: /*    18  End of statement *(semi - colon) */
		printf("NA\n");
		break;
	default:
		printf("PLATY: Scanner error: invalid token code: %d\n", t.code);
	}/*end switch*/
}/* end syn_printe()*/

void gen_incode(char* stringOutput) {
	printf("%s\n", stringOutput);
}

void program(void) {
	match(KW_T, PLATYPUS); match(LBR_T, NO_ATTR); opt_statements();
	match(RBR_T, NO_ATTR);
	gen_incode("PLATY: Program parsed");
}

void opt_statements(void) {
	switch (lookahead.code) {
	case AVID_T: case SVID_T:
		statements();
		break;
	case KW_T:
		/*check for IF, WHILE, READ, WRITE and in statements_p*/
		if (lookahead.attribute.get_int == IF
			|| lookahead.attribute.get_int == WHILE
			|| lookahead.attribute.get_int == READ
			|| lookahead.attribute.get_int == WRITE) {
			statements();
			break;
		}
	default: /*empty string - optional statements*/
		gen_incode("PLATY: Opt_statements parsed");
	}
}

void statements(void) {
	statement();
	statements_p();
}

void statement(void) {
	switch (lookahead.code) {
	case AVID_T: case SVID_T:
		assignment_statement();
		break;
	case KW_T:
		/*check for IF, WHILE, READ, WRITE*/
		if (lookahead.attribute.get_int == IF)
			selection_statement();
		if (lookahead.attribute.get_int == WHILE)
			iteration_statement();
		if (lookahead.attribute.get_int == READ)
			input_statement();
		if (lookahead.attribute.get_int == WRITE) 
			output_statement();
		break;
	default:
		/*Else it is an error*/
		syn_printe();
		break;
	}
}

void statements_p(void) {
	switch (lookahead.code) {
	case AVID_T: case SVID_T:
		statement();  
		statements_p();
		break;
	case KW_T:
		/*check for IF, WHILE, READ, WRITE*/
		if (lookahead.attribute.get_int == IF
			|| lookahead.attribute.get_int == WHILE
			|| lookahead.attribute.get_int == READ
			|| lookahead.attribute.get_int == WRITE) {
			statement(); 
			statements_p();
		}
		break;
	}
}

void assignment_statement(void) {
	assignment_exp();
	/*Match semi-colon character*/
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Assignment statement parsed");
}

void assignment_exp(void) {
	switch (lookahead.code) {
	case AVID_T:
		match(AVID_T, NO_ATTR);
		match(ASS_OP_T,NO_ATTR);
		arithmetic_exp();
		gen_incode("PLATY: Assignment expression (arithmetic) parsed");
		break;
	case SVID_T:
		match(SVID_T, NO_ATTR);
		match(ASS_OP_T, NO_ATTR);
		string_exp();
		gen_incode("PLATY: Assignment expression (string) parsed");
		break;
	default:
		syn_printe();
		break;
	}
}

void selection_statement(void) {
	match(KW_T, IF);
	pre_condition();
	match(LPR_T, NO_ATTR);
	conditional_exp();
	match(RPR_T, NO_ATTR);

	match(KW_T, THEN);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);

	match(KW_T, ELSE);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);

	gen_incode("PLATY: Selection statement parsed");
}

void iteration_statement(void) {
	match(KW_T, WHILE);
	pre_condition();
	match(LPR_T, NO_ATTR);
	conditional_exp();
	match(RPR_T, NO_ATTR);

	match(KW_T, REPEAT);
	match(LBR_T, NO_ATTR);
	statements();
	match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);

	gen_incode("PLATY: Iteration statement parsed");
}

void pre_condition(void) {
	switch (lookahead.code) {
	case KW_T:
		if (lookahead.attribute.get_int == TRUE) 
			match(KW_T, TRUE);
		else if (lookahead.attribute.get_int == FALSE) 
			match(KW_T, FALSE);
		else
			syn_printe();
		break;
	default:
		syn_printe();
		break;
	}
}

void input_statement(void) {
	match(KW_T, READ);
	match(LPR_T, NO_ATTR);
	variable_list();
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Input statement parsed");
}

void variable_list(void) {
	variable_identifier();
	variable_list_p();
	gen_incode("PLATY: Variable list parsed");
}

void variable_identifier(void) {
	switch (lookahead.code) {
	case AVID_T:
		match(AVID_T, NO_ATTR);
		break;
	case SVID_T:
		match(SVID_T, NO_ATTR);
		break;
	default:
		syn_printe();
		break;
	}
}

void variable_list_p(void) {
	switch (lookahead.code) {
	case COM_T:
		match(COM_T, NO_ATTR);
		variable_identifier();
		variable_list_p();
		break;
	}
}

void output_statement(void) {
	match(KW_T, WRITE);
	match(LPR_T, NO_ATTR);
	output_list();
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Output statement parsed");
}

void output_list(void) {
	switch (lookahead.code) {
	case STR_T:
		match(STR_T, NO_ATTR);
		gen_incode("PLATY: Output list (string literal) parsed");
		break;
	default:
		opt_variable_list();
		break;
	}
}

void opt_variable_list(void) {
	switch (lookahead.code) {
	case AVID_T: case SVID_T:
		variable_list();
		break;
	default:
		gen_incode("PLATY: Output list (empty) parsed");
		break;
	}
}

void arithmetic_exp(void) {
	switch (lookahead.code) {
	case ART_OP_T:
		if (lookahead.attribute.arr_op == PLUS 
			|| lookahead.attribute.arr_op == MINUS)
			unary_arithmetic_exp();
		else {
			syn_printe();
			break;
		}
		gen_incode("PLATY: Arithmetic expression parsed");
		break;
	case AVID_T: case FPL_T: case INL_T: case LPR_T:
		additive_arithmetic_exp();
		gen_incode("PLATY: Arithmetic expression parsed");
		break;
	default: 
		syn_printe();
		break;
	}
}

void unary_arithmetic_exp(void) {
	switch (lookahead.code) {
	case ART_OP_T:
		if (lookahead.attribute.arr_op == MINUS 
			|| lookahead.attribute.arr_op == PLUS) {
			match(ART_OP_T, lookahead.attribute.arr_op);
			primary_arithmetic_exp();
		}else
			syn_printe();
		break;
	}
	gen_incode("PLATY: Unary arithmetic expression parsed");
}

void additive_arithmetic_exp(void) {
	multiplicative_arithmetic_exp();
	additive_arithmetic_exp_p();
}

void additive_arithmetic_exp_p(void) {
	if (lookahead.code == ART_OP_T) {
		if (lookahead.attribute.arr_op == PLUS 
			|| lookahead.attribute.arr_op == MINUS) {
			match(ART_OP_T, lookahead.attribute.arr_op);
			multiplicative_arithmetic_exp();
			additive_arithmetic_exp_p();
			gen_incode("PLATY: Additive arithmetic expression parsed");
		}
	}
}

void multiplicative_arithmetic_exp(void) {
	primary_arithmetic_exp();
	multiplicative_arithmetic_exp_p();
}

void multiplicative_arithmetic_exp_p(void) {
	if (lookahead.code == ART_OP_T) {
		if (lookahead.attribute.arr_op == MULT
			|| lookahead.attribute.arr_op == DIV) {
			match(ART_OP_T, lookahead.attribute.arr_op);
			primary_arithmetic_exp();
			multiplicative_arithmetic_exp_p();
			gen_incode("PLATY: Multiplicative arithmetic expression parsed");
		}
	}
}

void primary_arithmetic_exp(void) {
	switch (lookahead.code) {
	case AVID_T: case FPL_T: case INL_T:
		match(lookahead.code, NO_ATTR);
		gen_incode("PLATY: Primary arithmetic expression parsed");
		break;
	case LPR_T:
		match(LPR_T, NO_ATTR);
		arithmetic_exp();
		match(RPR_T, NO_ATTR);
		gen_incode("PLATY: Primary arithmetic expression parsed");
		break;
	default:
		syn_printe();
		break;
	}
}

void string_exp(void) {
	primary_string_exp();
	string_exp_p();
	gen_incode("PLATY: String expression parsed");
}

void primary_string_exp(void) {
	switch (lookahead.code) {
	case SVID_T: case STR_T:
		match(lookahead.code, NO_ATTR);
		gen_incode("PLATY: Primary string expression parsed");
		break;
	default: 
		syn_printe();
		break;
	}
}

void string_exp_p(void) {
	if (lookahead.code == SCC_OP_T) {
		match(SCC_OP_T, NO_ATTR);
		primary_string_exp();
		string_exp_p();
	}
}

void conditional_exp(void) {
	logical_or_exp();
	gen_incode("PLATY: Conditional expression parsed");
}

void logical_or_exp(void) {
	logical_and_exp();
	logical_or_exp_p();
}

void logical_or_exp_p(void) {
	if (lookahead.code == LOG_OP_T) {
		if (lookahead.attribute.log_op == OR) {
			match(LOG_OP_T, OR);
			logical_and_exp();
			logical_or_exp_p();
			gen_incode("PLATY: Logical OR expression parsed");
		}
	}
}


void logical_and_exp(void) {
	relational_exp();
	logical_and_exp_p();
}

void logical_and_exp_p(void) {
	if (lookahead.code == LOG_OP_T) {
		if (lookahead.attribute.log_op == AND) {
			match(LOG_OP_T, AND);
			relational_exp();
			logical_and_exp_p();
			gen_incode("PLATY: Logical AND expression parsed");
		}
	}
}

void relational_exp(void) {
	switch (lookahead.code) {
	case AVID_T: case FPL_T: case INL_T:
		primary_a_relational_exp();
		operator_list();
		primary_a_relational_exp();
		break;
	case SVID_T: case STR_T:
		primary_s_relational_exp();
		operator_list();
		primary_s_relational_exp();
		break;
	default:
		syn_printe();
		break;
	}
	gen_incode("PLATY: Relational expression parsed");
}

void operator_list(void) {
	if (lookahead.code == REL_OP_T)
		match(REL_OP_T, lookahead.attribute.log_op);
	else
		syn_printe();
}


void primary_a_relational_exp(void) {
	switch (lookahead.code) {
	case AVID_T: case FPL_T: case INL_T:
		match(lookahead.code, NO_ATTR);
		break;
	default: 
		syn_printe();
		break;
	}
	gen_incode("PLATY: Primary a_relational expression parsed");
}

void primary_s_relational_exp(void) {
	primary_string_exp();
	gen_incode("PLATY: Primary s_relational expression parsed");
}
