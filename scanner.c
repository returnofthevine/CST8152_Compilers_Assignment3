/*
File name: scanner.c
Compiler: MS Visual Studio 2019
Author: Divine Omorogieva, ID# 040897820
Course: CST 8152 – Compilers, Lab Section: 013
Assignment: 3
Date: July 17th, 2020
Professor: Paulo Sousa
Purpose: Implement the Lexical Analyzer part of the Buffer
Function list: scanner_init(), malar_next_token(), char_class(), get_next_state(), iskeyword(), aa_func02(),
			   aa_func03(), aa_func05(), aa_func08(), aa_func11()
*/

/* The #define _CRT_SECURE_NO_WARNINGS should be used in MS Visual Studio projects
 * to suppress the warnings about using "unsafe" functions like fopen()
 * and standard sting library functions defined in string.h.
 * The define does not have any effect in Borland compiler projects.
 */
#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>   /* standard input / output */
#include <ctype.h>   /* conversion functions */
#include <stdlib.h>  /* standard library functions and constants */
#include <string.h>  /* string functions */
#include <limits.h>  /* integer types constants */
#include <float.h>   /* floating-point types constants */

 /*#define NDEBUG        to suppress assert() call */
#include <assert.h>  /* assert() prototype */

/* project header files */
#include "buffer.h"
#include "token.h"
#include "table.h"

#define DEBUG  /* for conditional processing */
#undef  DEBUG

/* Global objects - variables */
/* This buffer is used as a repository for string literals.
   It is defined in platy_st.c */
extern pBuffer str_LTBL; /*String literal table */
int line; /* current line number of the source code */
extern int scerrnum;     /* defined in platy_st.c - run-time error number */

/* Local(file) global objects - variables */
static pBuffer lex_buf;/*pointer to temporary lexeme buffer*/
static pBuffer sc_buf; /*pointer to input source buffer*/
/* No other global variable declarations/definitiond are allowed */

/* scanner.c static(local) function  prototypes */
static int char_class(char c); /* character class function */
static int get_next_state(int, char); /* state machine function */
static int iskeyword(char* kw_lexeme); /*keywords lookup functuion */


/*Initializes scanner */
int scanner_init(pBuffer psc_buf) {
	if (b_isempty(psc_buf)) return EXIT_FAILURE;/*1*/
	/* in case the buffer has been read previously  */
	b_rewind(psc_buf);
	b_clear(str_LTBL);
	line = 1;
	sc_buf = psc_buf;
	return EXIT_SUCCESS;/*0*/
/*   scerrnum = 0;  *//*no need - global ANSI C */
}

/*
Purpose: Generate token from the input buffer and called required functions
Author: Divine Omorogieva
History/Versions: 1.0
Called functions: b_getc(), b_retract(), b_allocate(), b_getcoffset(), b_markc(), b_free(),
				  b_compact(), b_reset(), strcpy(), get_next_stage()
Parameters: None
Return value: Token t
Algorithm: - In a while loop, take in character by character from the input buffer
		   - Skip the whitespace and break the loop if SEOF happened
		   - If an error encountered, return a error token or runtime error token
		   - Filter the special characters and return token with proper attributes and code
		   - Implement the DFA based on the transition table by calling proper accepting functions
		   - Return the token
*/
Token malar_next_token(void)
{
	Token t = { 0 }; /* token to return after pattern recognition. Set all structure members to 0 */
	unsigned char c; /* input symbol */
	int state = 0; /* initial state of the FSM */
	short lexstart;  /*start offset of a lexeme in the input char buffer (array) */
	short lexend;    /*end   offset of a lexeme in the input char buffer (array)*/

	/*DECLARE YOUR LOCAL VARIABLES HERE IF NEEDED*/
	char getChar; /*Used to avoid calling b_getc twice*/
	char getChar1; /*Used to avoid calling b_getc multiple times*/
	char getChar2; /*Used to avoid calling b_getc multiple times*/
	char getChar3; /*Used to avoid calling b_getc multiple times*/
	int i; /*iterator for the loop*/

	while (1) { /* endless loop broken by token returns it will generate a warning */

		/*GET THE NEXT SYMBOL FROM THE INPUT BUFFER*/
		c = b_getc(sc_buf);

		/* Part 1: Implementation of token driven scanner */
		/* every token is possessed by its own dedicated code */

		switch (c) {
			/*White-space characters*/
		case ' ': case '\t': case '\v': case '\f': case '\r':
			continue;

			/*New line*/
		case '\n':
			line++;
			continue;

			/*Arithmethic Operator characters*/
		case '+':
			t.code = ART_OP_T;
			t.attribute.arr_op = PLUS;
			return t;

		case '-':
			t.code = ART_OP_T;
			t.attribute.arr_op = MINUS;
			return t;

		case '*':
			t.code = ART_OP_T;
			t.attribute.arr_op = MULT;
			return t;

		case '/':
			t.code = ART_OP_T;
			t.attribute.arr_op = DIV;
			return t;


			/*Relational Operator / String Concatenation / Assignment characters*/
		case '<':
			getChar = b_getc(sc_buf);

			if (getChar == '>') {
				t.code = REL_OP_T;
				t.attribute.rel_op = NE;
				return t;
			}
			if (getChar == '<') {
				t.code = SCC_OP_T;
				return t;
			}

			t.code = REL_OP_T;
			b_retract(sc_buf);
			t.attribute.rel_op = LT;
			return t;

		case '>':
			t.code = REL_OP_T;
			t.attribute.rel_op = GT;
			return t;

		case '=':
			if (b_getc(sc_buf) == '=') {
				t.code = REL_OP_T;
				t.attribute.rel_op = EQ;
				return t;
			}
			b_retract(sc_buf);
			t.code = ASS_OP_T;
			return t;

			/*Logical operator*/
		case '.':
			getChar = b_getc(sc_buf); //should be A or O
			getChar1 = b_getc(sc_buf); //should be N or R
			getChar2 = b_getc(sc_buf); //should be D or .
			getChar3 = b_getc(sc_buf); //Should be .

			/*.AND. logical operator*/
			if (getChar == 'A' && getChar1 == 'N' && getChar2 == 'D' && getChar3 == '.') {
				t.code = LOG_OP_T;
				t.attribute.log_op = AND;
				return t;
			}

			/*OR just need 3 b_getc call, call one retract*/
			b_retract(sc_buf);

			/*.OR. logical operator*/
			if (getChar == 'O' && getChar1 == 'R' && getChar2 == '.') {
				t.code = LOG_OP_T;
				t.attribute.log_op = OR;
				return t;
			}

			/*If not, retract three times and return an error*/
			for (i = 0; i < 3; i++) {
				b_retract(sc_buf);
			}
			t.code = ERR_T;
			strcpy(t.attribute.err_lex, ".");
			return t;

			/*Seperators*/
		case '(':
			t.code = LPR_T;
			return t;

		case ')':
			t.code = RPR_T;
			return t;

		case '{':
			t.code = LBR_T;
			return t;

		case '}':
			t.code = RBR_T;
			return t;

		case ',':
			t.code = COM_T;
			return t;

		case ';':
			t.code = EOS_T;
			return t;

			/*SEOF*/
		case SEOF:
			t.code = SEOF_T;
			t.attribute.seof = SEOF_0;
			return t;

			/*EOF*/
		case SEOF_255:
			t.attribute.seof = SEOF_EOF;
			t.code = SEOF_T;
			return t;

			/*Comments*/
		case '!':
			getChar = b_getc(sc_buf);

			/*If it is a commenet*/
			if (getChar == '!') {
				/*Ignore all characters until \n or SEOF*/
				while (getChar != '\n') {

					/*Get the next characters in the line*/
					getChar = b_getc(sc_buf);

					/*Check if it is SEOF/EOF*/
					if (getChar == SEOF || getChar == SEOF_255) {
						/*Retract the SEOF/EOF*/
						b_retract(sc_buf);
						/*Break the while loop*/
						break;
					}
				}
				/*Count the line*/
				if (getChar == '\n')
					line++;
				/*Move to the next line*/
				continue;
			}

			/*If it is not a comment*/
			if (getChar != '!') {
				t.code = ERR_T;
				t.attribute.err_lex[0] = '!';
				getChar2 = getChar; /*Carry the character right after the illegal comment*/

				/*Loop through all the symbols of the wrong comment*/
				while (getChar != '\n') {

					/*If an EOF/SEOF is after the illegal comment*/
					if (getChar == SEOF || getChar == SEOF_255) {
						/*Retract the SEOF/EOF character*/
						/*Return an error token*/
						b_retract(sc_buf);
						break;
					}

					/*Else keep ignoring other characters in the same line*/
					getChar = b_getc(sc_buf);
				}

				/*Add a new line*/
				if (getChar == '\n')
					line++;

				/*If the character right next to the illegal comment is not a SEOF/EOF,
				add that character and SEOF to the err_lex array*/
				if (getChar2 != SEOF || getChar2 != SEOF_255) {
					t.attribute.err_lex[1] = getChar2;
					t.attribute.err_lex[2] = SEOF;
				}
				else
					/*Else just add the SEOF to the err_lex array to close it*/
					t.attribute.err_lex[1] = SEOF;

				return t;
			}


			/*Error cases*/
		case RT_FAIL_2:
			scerrnum = TRUE;
			t.code = RTE_T;
			strcpy(t.attribute.err_lex, "RUN TIME ERROR: ");
			return t;

			/*Other cases, move to next implementation*/\
		default:
			break;

		}/*end of switch*/

 /* Part 2: Implementation of Finite State Machine (DFA)
			or Transition Table driven Scanner
			Note: Part 2 must follow Part 1 to catch the illegal symbols
 */

 /*SET THE MARK AT THE BEGINING OF THE LEXEME AND SAVE IT IN lexstart*/
		lexstart = b_markc(sc_buf, b_getcoffset(sc_buf) - 1);

		/*Check if run time error happened*/
		if (lexstart == RT_FAIL_1) {
			scerrnum = TRUE;
			strcpy(t.attribute.err_lex, "RUN TIME ERROR: ");
			t.code = RTE_T;
			return t;
		}

		/*FSM0. Start with state = 0 and input c. Can be Letter, Digit or String opening double quote*/
		/*FSM1. Get the next sate from the transition table*/
		/*FSM2. Use as_table to get the type of state (NOAS, ASWR, ASNR)
		If it is NOAS, get the next character and repeat FSM1*/
		while (as_table[state] == NOAS) {
			state = get_next_state(state, c);
			c = b_getc(sc_buf);
		}
		/*Since it reached an accepting state after getting an extra character,
		retract the buffer*/
		b_retract(sc_buf);

		/*FSM3. If the state is an accepting state, token is found, leave the machine and call a function*/
		/*For ASWR state, retract function is called first*/
		if (as_table[state] == ASWR)
			b_retract(sc_buf);

		/*Set lexend to getc_offset*/
		lexend = b_getcoffset(sc_buf);

		/*Create a temporary buffer for the lexeme*/
		lex_buf = b_allocate(DEFAULT_INIT_CAPACITY, INC_FACTOR_F, 'f');

		/*Check for run time error*/
		if (lex_buf == NULL) {
			scerrnum = TRUE;
			strcpy(t.attribute.err_lex, "RUN TIME ERROR: ");
			t.code = RTE_T;
			return t;
		}

		/*Reset getc_offset to markc_offset*/
		b_reset(sc_buf);

		/*Copy the lexeme betwween lexstart and lexend from the input buffer to the lexeme buffer
		using b_getc() and b_addc()*/
		for (i = lexstart; i < lexend; i++) {
			c = b_getc(sc_buf);
			if (b_addc(lex_buf, c) == NULL) {
				t.code = RTE_T;
				strcpy(t.attribute.err_lex, "RUN TIME ERROR: ");
				scerrnum = TRUE;
				return t;
			}
		}
		/*Add SEOF to the lexeme buffer end*/
		//b_compact(lex_buf, SEOF);
		b_addc(lex_buf, SEOF);

		/*Call out accepting function using the aa_table[]*/
		t = aa_table[state](b_location(lex_buf, lex_buf->markc_offset));

		b_free(lex_buf);
		return t;
	}//end while(1)
}


/*
Purpose: Get the next state inside as_table[]
Author: Svillen Ranev
History/Versions: 1.0
Called functions: char_class(), assert(), printf(), exit()
Parameters: int state, char c
Return value: int next
Algorithm: - Call the char_class function to get the column of the character
		   - Return the next state
*/
int get_next_state(int state, char c)
{
	int col;
	int next;
	col = char_class(c);
	next = st_table[state][col];
#ifdef DEBUG
	printf("Input symbol: %c Row: %d Column: %d Next: %d \n", c, state, col, next);
#endif

	assert(next != IS);

#ifdef DEBUG
	if (next == IS) {
		printf("Scanner Error: Illegal state:\n");
		printf("Input symbol: %c Row: %d Column: %d\n", c, state, col);
		exit(1);
	}
#endif
	return next;
}

/*
Purpose: Return the column number in the transition table for the input character c
Author: Divine Omorogieva
History/Versions: 1.0
Called functions: isalpha(), isdigit()
Parameters: char c
Return value: int val
Algorithm: - Check c with the transition characters (column) on the transition table
		   - Return the value of the matched column's index
*/
int char_class(char c)
{
	int val; /*Return column number value*/

	/*Column index 0: [a-zA-Z]*/
	if (isalpha(c))
		val = 0;
	/*Column index 1: 0*/
	else if (c == '0')
		val = 1;
	/*Column index 2: [1-9]*/
	else if (isdigit(c) && c != '0')
		val = 2;
	/*Column index 3: .*/
	else if (c == '.')
		val = 3;
	/*Column index 4: */
	else if (c == '#')
		val = 4;
	/*Column index 5: " */
	else if (c == '"')
		val = 5;
	/*Column index 6: SEOF*/
	else if (c == SEOF || c == SEOF_255)
		val = 6;
	/*Other cases*/
	else
		val = 7;

	return val;
}



/*HERE YOU WRITE THE DEFINITIONS FOR YOUR ACCEPTING FUNCTIONS.
*************************************************************/

/*
Purpose: Accepting function for AVID and KW
Author: Divine Omorogieva
History/Versions: 1.0
Called functions: iskeyword(), strlen(), strcpy()
Parameters: char* lexeme
Return value: Token
Algorithm: - Check if the lexeme is a keyword or not
		   - If it is a keyword, return a keyword token with attribute of the keyword index
			 inside kw_table[]
		   - If it is not, return a AVID token with the attribute of the lexeme or the lexeme up
			 up until the VID_LEN character
*/
Token aa_func02(char* lexeme) {
	Token t = { 0 }; /* token to return after pattern recognition. Set all structure members to 0 */
	int i; /*iterator for the loop*/

	/*Check if lexeme is a keyword*/
	if (iskeyword(lexeme) != RT_FAIL_2) {
		t.code = KW_T;
		t.attribute.kwt_idx = iskeyword(lexeme);
		return t;
	}

	/*If the lexeme is not a keyword, check for AVID*/
	/*Set a AVID TOKEN*/
	t.code = AVID_T;

	/*If lexeme is longer than VID_LEN, store the lexeme up until the VID_LEN character*/
	if (strlen(lexeme) > VID_LEN) {
		for (i = 0; i < VID_LEN; i++)
			t.attribute.vid_lex[i] = lexeme[i];
		/*Add the '\0' at the end of the buffer*/
		t.attribute.vid_lex[VID_LEN] = SEOF;
		return t;
	}

	/*If not, store the lexeme to vid_lex attribute*/
	strcpy(t.attribute.vid_lex, lexeme);

	return t;
}

/*
Purpose: Accepting function for SVID
Author: Divine Omorogieva
History/Versions: 1.0
Called functions: strlen(), strcpy()
Parameters: char* lexeme
Return value: Token
Algorithm: - Check the size of the lexeme and store into the attribute token properly
		   - Return the token
*/
Token aa_func03(char* lexeme) {
	Token t = { 0 }; /* token to return after pattern recognition. Set all structure members to 0 */
	int i; /*iterator for the forr loop*/

	/*Set the token SVID*/
	t.code = SVID_T;

	/*If the lexeme is longer than VID_LEN characters, store the first VID_LEN - 1 character*/
	if (strlen(lexeme) > VID_LEN) {
		for (i = 0; i < VID_LEN - 1; i++)
			t.attribute.vid_lex[i] = lexeme[i];
		/*Add @ and \0 to the end of vid_lex*/
		t.attribute.vid_lex[VID_LEN - 1] = '@';
		t.attribute.vid_lex[VID_LEN] = SEOF;
		return t;
	}

	/*If not, store the lexeme to vid_lex as a whole*/
	strcpy(t.attribute.vid_lex, lexeme);

	return t;
}

/*
Purpose: Accepting integer literal - decimal constant
Author: Divine Omorogieva
History/Versions: 1.0
Called functions: atol(), strcat(), strlen(), strcpy()
Parameters: char* lexeme
Return value: Token
Algorithm: - Check if the lexeme can be converted to a 4-byte float, if not return a error token
		   - The error token should be reducted to ERR_LEN-3 characters
		   - Else return a floatin point literal token
*/
Token aa_func05(char* lexeme) {
	Token t = { 0 }; /* token to return after pattern recognition. Set all structure members to 0 */
	int i; /*iterator for the loop*/
	long il; /*Variable to hold the converted decimal integer*/

	/*Convert the token to a decimal integer value*/
	il = atol(lexeme);

	/*Check if the value is in the same range as 2-byte integer in C*/
	if (il > SHRT_MAX || il < SHRT_MIN) {
		/*Set token to error*/
		t.code = ERR_T;
		if (strlen(lexeme) > ERR_LEN) {
			for (i = 0; i < ERR_LEN - 3; i++)
				t.attribute.err_lex[i] = lexeme[i];
			/*Add 3 dots and SEOF to the end*/
			strcat(t.attribute.err_lex, "...");
		}
		/*If the error is shorter*/
		else
			strcpy(t.attribute.err_lex, lexeme);

		return t;
	}

	/*Set the token*/
	t.code = INL_T;
	t.attribute.int_value = (short)il;

	return t;
}

/*
Purpose: Accepting floating point literal
Author: Divine Omorogieva
History/Versions: 1.0
Called functions: atof(), strcat(), strlen(), strcpy()
Parameters: char* lexeme
Return value: Token
Algorithm: - Check if the lexeme can be converted to a 4-byte float, if not return a error token
		   - The error token should be reducted to ERR_LEN-3 characters
		   - Else return a floatin point literal token
*/
Token aa_func08(char* lexeme) {
	Token t = { 0 }; /* token to return after pattern recognition. Set all structure members to 0 */
	int i; /*iterator for the for loop*/
	double fpl; /*Variable to hold the floating point literal*/

	/*Convert the token to float value*/
	fpl = atof(lexeme);

	/*Check if the value is same range with 4-byte float in c*/
	if (fpl > FLT_MAX || (fpl < FLT_MIN && fpl != 0)) {
		/*Set token to error*/
		t.code = ERR_T;
		/*If ther error lexeme islonger*/
		if (strlen(lexeme) > ERR_LEN) {
			for (i = 0; i < ERR_LEN - 3; i++)
				t.attribute.err_lex[i] = lexeme[i];
			/*Add 3 dots and SEOF to the end*/
			strcat(t.attribute.err_lex, "...");
		}
		/*If the error is shorter*/
		else
			strcpy(t.attribute.err_lex, lexeme);

		return t;
	}

	/*Set the token FPL*/
	t.code = FPL_T;
	t.attribute.flt_value = (float)fpl;

	return t;
}


/*
Purpose: Accepting string literal
Author: Divine Omorogieva
History/Versions: 1.0
Called functions: b_addcoffset(), b_addc(), strlen()
Parameters: char* lexeme
Return value: Token
Algorithm: - Set the token code and attribute properly to where the first lexeme char will be added
		   - Copy the lexeme to the string buffer, add a one to line counter if contain a line terminator
		   - Add \0 at the end of the string buffer
		   - Return the token
*/
Token aa_func10(char* lexeme) {
	Token t = { 0 }; /* token to return after pattern recognition. Set all structure members to 0 */
	unsigned int i; /*Iterator for the for loop*/

	/*Set the token code*/
	t.code = STR_T;
	/*Set the token attribute get_int to where the first lexeme char will be added*/
	t.attribute.str_offset = b_addcoffset(str_LTBL);

	/*Copy the lexeme content to str_LTBL*/
	for (i = 0; i < strlen(lexeme); i++) {
		/*Skip the " opening and closing*/
		if (lexeme[i] != '"')
			b_addc(str_LTBL, lexeme[i]);
		/*Add the line counter if the lexeme has an line terminator*/
		if (lexeme[i] == '\n')
			line++;
	}
	/*Add '\0' at the end of the string buffer*/
	b_addc(str_LTBL, SEOF);

	return t;
}

/*
Purpose: Accepting the error token
Author: Divine Omorogieva
History/Versions: 1.0
Called functions: strcat() strlen(), strcpy()
Parameters: char* lexeme
Return value: Token
Algorithm: - Check if the lexeme can be converted to a 4-byte float, if not return a error token
		   - The error token should be reducted to ERR_LEN-3 characters
		   - Else return a floatin point literal token
*/
Token aa_func11(char* lexeme) {
	Token t = { 0 }; /* token to return after pattern recognition. Set all structure members to 0 */
	unsigned int i; /*Iterator for the for loop*/

	/*Set the token code*/
	t.code = ERR_T;

	/*If ther error lexeme islonger*/
	if (strlen(lexeme) > ERR_LEN) {
		for (i = 0; i < ERR_LEN - 3; i++)
			t.attribute.err_lex[i] = lexeme[i];
		/*Add 3 dots and SEOF to the end*/
		strcat(t.attribute.err_lex, "...");
	}
	/*If the error is shorter*/
	else
		strcpy(t.attribute.err_lex, lexeme);

	/*Check if the lexeme have a line terminator or not. If yes, add one to the line counter*/
	for (i = 0; i < strlen(lexeme); i++) {
		if (lexeme[i] == '\n')
			line++;
	}

	return t;
}

/*
Purpose: Check the lexeme if it is a keyword or not
Author: Divine Omorogieva
History/Versions: 1.0
Called functions: strcmp()
Parameters: char* kw_lexeme
Return value: int i
Algorithm: - Loop through the kw_table[] and compare the lexeme. If true, return the index of the
		keyword inside kw_table[], else return RT_FAIL_2
*/
int iskeyword(char* kw_lexeme) {
	int i; /*The iterator for the loop*/

	/*Loop through the kw_table[]*/
	for (i = 0; i < KWT_SIZE; i++) {
		if (strcmp(kw_lexeme, kw_table[i]) == 0)
			return i;
	}

	/*Return RT_FAIL_2 if not founded*/
	return RT_FAIL_2;
}