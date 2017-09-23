/* File Name: scanner.c
Compiler: MS Visual Studio 2013
Author: Arjun Sivakumar, 040836029
Course: CST8152 - Compilers, Lab Section: 11
Assignment: 2
Date: March 3rd, 2017
Professor: Sv. Ranev
Purpose: A lexical scanner that reads input from a file and based on the instructions from the source file the scanner will determine the
type of token. The scanner will create the token and initalizes the appropriate attributes for each token. If an error is possible
then the token will return an error.
Function List: scanner_init(), malar_next_token(), get_next_state(), char_class(), aa_func02(), aa_func03(), aa_func08(), aa_func05(),
aa_func11(), aa_func12(), aa_func13(), atool(), iskeyword()

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
#include "stable.h"

#define DEBUG  /* for conditional processing */
#undef  DEBUG

#define INIT_CAPACITY 200 /* initial buffer capacity */
#define INC_FACTOR 15       /* increment factor */ 

/* Global objects - variables */
/* This buffer is used as a repository for string literals.
It is defined in platy_st.c */
extern Buffer * str_LTBL; /*String literal table */
int line; /* current line number of the source code */
extern int scerrnum;     /* defined in platy_st.c - run-time error number */
extern STD sym_table;

/* Local(file) global objects - variables */
static Buffer *lex_buf;/*pointer to temporary lexeme buffer*/

/* No other global variable declarations/definitiond are allowed */

/* scanner.c static(local) function  prototypes */
static int char_class(char c); /* character class function */
static int get_next_state(int, char, int *); /* state machine function */
static int iskeyword(char * kw_lexeme); /*keywords lookup functuion */
static long atool(char * lexeme); /* converts octal string to decimal value */

/* Purpose: Create and allocate buffer for the scanner
Author:  Sv. Ranev
History/Versions: v1.0
Called Functions: b_isempty(), b_setmark(), b_retract_to_mark(), b_reset()
Parameters: Buffer * sc_buf
Return value: int
Algorithm:

*/

int scanner_init(Buffer * sc_buf) {
	if (b_isempty(sc_buf)) return EXIT_FAILURE;/*1*/
	/* in case the buffer has been read previously  */
	b_setmark(sc_buf, 0);
	b_retract_to_mark(sc_buf);
	b_reset(str_LTBL);
	line = 1;
	return EXIT_SUCCESS;/*0*/
	/*   scerrnum = 0;  *//*no need - global ANSI C */
}

/*  Purpose: matches a lexeme and returns next token based on the character input
Author:  Arjun Sivakumar
History/Versions: v1.0
Called Functions: b_getc(), b_retract(), b_setmark(), b_getcoffset(), b_retract_to_mark(), b_addc(), b_mark(), b_size(), b_free(), get_next_state()
Parameters: Buffer * sc_buf
Return value: Token
Algorithm: The functions loops and reads a character one by one from the sc_buf, then the function determines what operation to do based on the character
and the switch case will then set the token code and attributes correctly. If a digit or alphabet character is found then the finite state machine
starts. The state machine will then determine what to do based on the character it reads from the buffer.

*/

Token malar_next_token(Buffer * sc_buf)
{
	Token t; /* token to return after recognition */
	unsigned char c; /* input symbol */
	int state = 0; /* initial state of the FSM */
	short lexstart = 0;  /*start offset of a lexeme in the input buffer */
	short lexend = 0;    /*end   offset of a lexeme in the input buffer */
	int accept = NOAS; /* type of state - initially not accepting */
	/*
	lexstart is the offset from the beginning of the char buffer of the
	input buffer (sc_buf) to the first character of the current lexeme,
	which is being processed by the scanner.
	lexend is the offset from the beginning of the char buffer of the
	input buffer (sc_buf) to the last character of the current lexeme,
	which is being processed by the scanner.

	*/

	int strOffset = 0; /*Used to add to str_offset in the Token */
	int lexemeLength = 0; /*Used for caluclating lexeme length*/

	while (1) { /* endless loop broken by token returns it will generate a warning */

		/*printf("Line: %d\n", line); */

		c = b_getc(sc_buf); /*Get next character from sc_buf for switch*/

		switch (c) {

		case '\t': /*Cases for white spaces*/
		case ' ':
		case '\f':
		case '\v':
			continue;

		case '\r': /*carridge return*/
			c = b_getc(sc_buf);

			if (c != '\n')
				b_retract(sc_buf);
			++line;
			continue;
		case '\n': /*New line*/
			++line;
			continue;
		case '{': /*Left brace token*/
			t.code = LBR_T;
			return t;

		case '}': /*Right brace token*/
			t.code = RBR_T;
			return t;

		case '(': /*Left paraenthesis token*/
			t.code = LPR_T;
			return t;

		case ')': /*Right paranthesis token*/
			t.code = RPR_T;
			return t;

		case ';': /*End of statement token*/
			t.code = EOS_T;
			return t;
		case ',': /*comma token*/
			t.code = COM_T;
			return t;
		case '+': /*Plus arithmetic token*/
			t.code = ART_OP_T;
			t.attribute.arr_op = PLUS;
			return t;

		case '-': /*Minus arithmetic token*/
			t.code = ART_OP_T;
			t.attribute.arr_op = MINUS;
			return t;

		case '*': /*Multiplication arithmetic token*/
			t.code = ART_OP_T;
			t.attribute.arr_op = MULT;
			return t;

		case '/': /*Divison arithmetic token*/
			t.code = ART_OP_T;
			t.attribute.arr_op = DIV;
			return t;

		case '=': /*Checks to see if token is either relational equal to token or assignment operator token*/
			c = b_getc(sc_buf);

			if (c == '=') {
				t.code = REL_OP_T;
				t.attribute.rel_op = EQ;
				return t;
			}

			b_retract(sc_buf);
			t.code = ASS_OP_T;
			return t;
		case '<': /* Checks to see if token is either string concatenation token, not equal to relational operator or less than relational operator*/
			c = b_getc(sc_buf);

			if (c == '<') {
				t.code = SCC_OP_T;
				return t;
			}

			else if (c == '>') {
				t.code = REL_OP_T;
				t.attribute.rel_op = NE;
				return t;
			}
			b_retract(sc_buf);
			t.code = REL_OP_T;
			t.attribute.rel_op = LT;
			return t;
		case '>': /*Greater than relational token*/
			t.code = REL_OP_T;
			t.attribute.rel_op = GT;
			return t;
		case '.': /*Check to see if .AND. or .OR. are the next characters after the . character*/
			b_setmark(sc_buf, b_getcoffset(sc_buf)); /*If logical token is not found then retract to '.' char*/

			c = b_getc(sc_buf);

			if (c == 'A') { /*If first char is 'A' continue to see if the 'N' 'D' and '.' are in the lexeme*/
				c = b_getc(sc_buf);
				if (c == 'N') {
					c = b_getc(sc_buf);
					if (c == 'D') {
						c = b_getc(sc_buf);
						if (c == '.') {
							t.code = LOG_OP_T;
							t.attribute.log_op = AND;
							return t;
						}
					}
				}
			}/*End AND nested if*/

			if (c == 'O') { /*If the first char is 'O' it will continue to see if 'R' and '.' are in the lexeme*/
				c = b_getc(sc_buf);
				if (c == 'R') {
					c = b_getc(sc_buf);
					if (c == '.') {
						t.code = LOG_OP_T;
						t.attribute.log_op = OR;
						return t;
					}
				}
			} /*End OR nested if*/

			/*If lexeme doesn't match logical tokens then set it to error*/
			t.code = ERR_T;
			b_retract_to_mark(sc_buf);
			t.attribute.err_lex[0] = '.';
			t.attribute.err_lex[1] = '\0';
			return t;

		case '!': /*Comment literal token*/
			b_setmark(sc_buf, b_getcoffset(sc_buf)); /*Set mark at where the exclamation point was found*/

			c = b_getc(sc_buf);

			if (c == '<') { /*If < char is found then it is a valid comment token*/

				while (c != '\n' && c != '\r' && c != SEOF && c != SEOF_1) {
					c = b_getc(sc_buf);
				}

				b_retract(sc_buf);
				continue;

			}

			else { /*If < is not found then return an error token*/
				b_retract(sc_buf);
				c = b_getc(sc_buf);
				t.code = ERR_T;
				t.attribute.err_lex[0] = '!';
				t.attribute.err_lex[1] = c;
				t.attribute.err_lex[2] = '\0';

				/*Skip rest of the comment up up until, a return of some sorts*/

				while (c != '\n' && c != '\r' && c != SEOF_1 && c != SEOF) {
					c = b_getc(sc_buf);

					if (c == '\n') {
						++line;
						return t;
					}
				}


			}

		case '"': /*String literal token*/
			b_setmark(sc_buf, b_getcoffset(sc_buf)); /*Set mark to where quote was found*/
			lexstart = b_mark(sc_buf);
			strOffset = b_size(str_LTBL); /*Offset is size of the buffer since the buffer size is increasing*/

			c = b_getc(sc_buf);

			if (c == '"') { /*If it an empty string*/

				b_addc(str_LTBL, '\0');
				t.code = STR_T;
				t.attribute.str_offset = (short)strOffset;
				return t;

			}

			else {

				while (1) { /* endless loop broken by token returns it will generate a warning */

					c = b_getc(sc_buf);

					if (c == '\n') { /*Skip line if \n is found*/
						++line;
					}

					if (c == SEOF || c == SEOF_1) { /*String reached end of file before the other " could be found*/
						t.code = ERR_T;
						lexend = b_getcoffset(sc_buf) - 1;
						b_setmark(sc_buf, lexstart);
						b_retract_to_mark(sc_buf);
						b_retract(sc_buf);
						lexemeLength = lexend - lexstart;

						if (lexemeLength > ERR_LEN) { /*If the lexeme is greater than the maximum length of ERR_LEN*/
							lexemeLength = ERR_LEN - 3; /*Only get first 17 characters*/
							int i;
							for (i = 0; i < lexemeLength; i++) {
								t.attribute.err_lex[i] = b_getc(sc_buf);
							}
							t.attribute.err_lex[i] = '.';
							t.attribute.err_lex[++i] = '.';
							t.attribute.err_lex[++i] = '.';
							t.attribute.err_lex[++i] = '\0';

							/*After reading in error, skip string until end_of_file*/
							while (b_getcoffset(sc_buf) <= lexend)
							{
								c = b_getc(sc_buf);
							}

							b_retract(sc_buf);
							return t;

						}

						else { /*If lexeme is less than the ERR_LEN*/
							int i;
							for (i = 0; i < lexemeLength; i++) {
								t.attribute.err_lex[i] = b_getc(sc_buf);
							}
							t.attribute.err_lex[i] = '\0';
							return t;
						}
					}

					if (c == '"') { /*If matching quote is found then begin to process literal string*/
						b_retract_to_mark(sc_buf);

						c = b_getc(sc_buf);
						while (c != '"') { /*Loop to find the matching quote*/

							if (b_addc(str_LTBL, c) != NULL) 
							{
								c = b_getc(sc_buf);
							}

							else
							{
								break;
							}
						}

						b_addc(str_LTBL, '\0');

						t.code = STR_T;
						t.attribute.str_offset = (short)strOffset;
						return t;
					}
				}
			}
		case SEOF: /*End of file token*/
		case SEOF_1:
			t.code = SEOF_T;
			return t;

		default:
			break;
		} /*End switch*/

		if (isdigit(c) || isalpha(c)) { /*If digit or alphabet is found start the finite state machine*/
			b_setmark(sc_buf, b_getcoffset(sc_buf) - 1); /*Set mark to where the digit or letter was found*/
			state = get_next_state(state, c, &accept);
			while (accept == NOAS) { /*Loop to find accepting state*/
				c = b_getc(sc_buf);
				state = get_next_state(state, c, &accept);
			}
			lexstart = b_mark(sc_buf); /*Set lexstart to when the charact*/

			if (accept == ASWR)
				b_retract(sc_buf);

			lex_buf = b_create(INIT_CAPACITY, INC_FACTOR, 'm'); /*Create temporary buffer for lexeme*/

			lexend = b_getcoffset(sc_buf);
			b_retract_to_mark(sc_buf);

			while (b_getcoffset(sc_buf) < lexend) { /*Read until lexend*/

				b_addc(lex_buf, b_getc(sc_buf));

			}
			b_addc(lex_buf, '\0');

			/*find corresponding state in aa_table[]*/
			t = aa_table[state](b_setmark(lex_buf, 0));

			/*Release the buffer, no longer needed*/
			b_free(lex_buf);
			return t;

		}

		else { /*report error token if next character is not digit or alphabet*/
			t.code = ERR_T;
			t.attribute.err_lex[0] = c;
			t.attribute.err_lex[1] = '\0';
			return t;

		}



	} /*End while loop*/

}

/*  Purpose: Get the next state from the as_table[]
Author:  Sv. Ranev
History/Versions: v1.0
Called Functions: char_class()
Parameters: int state, char c, int* accept
Return value: int
*/

int get_next_state(int state, char c, int *accept)
{
	int col;
	int next;
	col = char_class(c);
	next = st_table[state][col];
#ifdef DEBUG
	printf("Input symbol: %c Row: %d Column: %d Next: %d \n", c, state, col, next);
#endif
	/*
	The assert(int test) macro can be used to add run-time diagnostic to programs
	and to "defend" from producing unexpected results.
	assert() is a macro that expands to an if statement;
	if test evaluates to false (zero) , assert aborts the program
	(by calling abort()) and sends the following message on stderr:

	Assertion failed: test, file filename, line linenum

	The filename and linenum listed in the message are the source file name
	and line number where the assert macro appears.
	If you place the #define NDEBUG directive ("no debugging")
	in the source code before the #include <assert.h> directive,
	the effect is to comment out the assert statement.
	*/
	assert(next != IS);

	/*
	The other way to include diagnostics in a program is to use
	conditional preprocessing as shown bellow. It allows the programmer
	to send more details describing the run-time problem.
	Once the program is tested thoroughly #define DEBUG is commented out
	or #undef DEBUF is used - see the top of the file.
	*/
#ifdef DEBUG
	if (next == IS){
		printf("Scanner Error: Illegal state:\n");
		printf("Input symbol: %c Row: %d Column: %d\n", c, state, col);
		exit(1);
	}
#endif
	*accept = as_table[next];
	return next;
}
/*
Purpose: Based on the character return the column from the transition table
Author:  Arjun Sivakumar
History/Versions: v1.0
Called Functions: isalpha()
Parameters: int state, char c, int* accept
Return value: int
*/
int char_class(char c)
{
	int val;

	if (isalpha(c)) {
		val = 0;
	}
	else if (c == '0') {
		val = 1;
	}
	else if (c >= '1' && c <= '7') {
		val = 2;
	}
	else if (c >= '8' && c <= '9') {
		val = 3;
	}
	else if (c == '.') {
		val = 4;
	}
	else if (c == '#') {
		val = 5;
	}
	else {
		val = 6;
	}

	return val;
}
/*
	Purpose: Creates and returns Arithmetic Variable Identifier (AVID)  or Keyword (KW)
	Author:  Arjun Sivakumar
	History/Versions: v1.0
	Called Functions: iskeyword(), strlen(),
	Parameters: char[] lexeme
	Return value: Token
	Algorithim: Check to see if lexeme matches keyword table if it does, then create keyword token and return if not
	then create AVID token and return the token. If all is correct then the lexeme will attempt to install into the symbol table
*/
Token aa_func02(char lexeme[]){ /*AVID*/

	Token t;
	int keywordIndex; /*Used for return value of iskeyword()*/
	int lexemeLength;
	int offset; /* Used when st_install returns the location of the lexeme in the symbol table*/
	char type; /*Determining what type to install into symbol table */

	keywordIndex = iskeyword(lexeme);

	if (keywordIndex != R_FAIL1) { /*The lexeme is a keyword*/
		t.code = KW_T;
		t.attribute.kwt_idx = keywordIndex;
		return t;
	}


	t.code = AVID_T; /*If lexeme is not keyword set it to AVID*/

	/*If the first character in the lexeme is either 'i', 'o', 'd' or 'n' set the type to integer */
	if (lexeme[0] == 'i' || lexeme[0] == 'o' || lexeme[0] == 'd' || lexeme[0] == 'n') {
		type = 'I';
	}
	else {
		/*If not set it to float*/
		type = 'F';
	}

	lexemeLength = strlen(lexeme);

	if (lexemeLength > VID_LEN) { /*If lexeme is greater than 8 char, set the last character to null */
		lexeme[VID_LEN] = '\0';
	}

	/*Attempt to add VID to symbol table*/
	offset = st_install(sym_table, lexeme, type, line);

	/*If there was an error with installing to the symbol table then print an error and store symbol table in file*/
	if (offset == -1) {
		printf("\nError: Install failed - Symbol Table is full.\n");
		st_store(sym_table); 
		b_free(lex_buf);
		exit(0);
	}

	/*Set token attribute to the location of lexeme in symbol table*/
	t.attribute.vid_offset = offset;
	return t;
}
/*
	Purpose: Creates and returns SVID token
	Author:  Arjun Sivakumar
	History/Versions: v1.0
	Called Functions: strlen(),
	Parameters: char[] lexeme
	Return value: Token
	Algorithim: Checks to see if the lexeme is a valid in terms of length if not then the token will be set to an error token and it will return. 
	If all is correct then the lexeme will attempt to install into the symbol table
*/
Token aa_func03(char lexeme[]){ /*SVID*/

	Token t;
	int lexemeLength;
	int offset;

	t.code = SVID_T;

	lexemeLength = strlen(lexeme);

	if (lexemeLength > VID_LEN) {  /*If length is greater than VID_LEN then make lexemeLength smaller to add # character*/
		lexeme[VID_LEN - 1] = '#';
		lexeme[VID_LEN] = '\0';

		/*Attempt to install SVID in the symbol table*/
		offset = st_install(sym_table, lexeme, 'S', line);
	}

	else {
		/*Attempt to install SVID in the symbol table*/
		offset = st_install(sym_table, lexeme, 'S', line);
	}
	/*If there was an error with installing to the symbol table then print an error and store symbol table in file*/
	if (offset == -1) {
		printf("\nError: Install failed - Symbol Table is full.\n");
		st_store(sym_table);
		b_free(lex_buf);
		exit(0);
	}
	/*Set token attribute to the location of lexeme in symbol table*/
	t.attribute.vid_offset = offset;

	return t;
}
/*
Purpose: Creates and returns Floating-point literal token
Author:  Arjun Sivakumar
History/Versions: v1.0
Called Functions: strlen(), atof()
Parameters: char[] lexeme
Return value: Token
Algorithim: Checks to see if the lexeme is in range of floating point, if not return error token. If it is in range set the value and return FPL token
*/
Token aa_func08(char lexeme[]){ /*FPL*/

	Token t;
	double floatLexeme;
	int lexemeLength;
	int i;

	lexemeLength = strlen(lexeme);
	floatLexeme = atof(lexeme); /*Convert string to float*/

	if ((floatLexeme > FLT_MAX) || (floatLexeme < FLT_MIN) && floatLexeme != 0.0) { /*If float is not in range*/
		t.code = ERR_T;

		if (lexemeLength > ERR_LEN) { /*If the lexeme is larger than the error token length*/
			lexemeLength = ERR_LEN - 3;
			for (i = 0; i < lexemeLength; i++) {
				t.attribute.err_lex[i] = lexeme[i];
			}
			t.attribute.err_lex[i] = '.';
			t.attribute.err_lex[++i] = '.';
			t.attribute.err_lex[++i] = '.';
			t.attribute.err_lex[++i] = '\0';

			return t;

		}
		else { /*If the lexeme is not larger than the error token length*/
			for (i = 0; i < lexemeLength; i++) {
				t.attribute.err_lex[i] = lexeme[i];
			}

			t.attribute.err_lex[i] = '\0'; /*add null terminating character*/

			return t;

		}

	}

	else {
		/*Set token as floating point */
		t.code = FPL_T;
		t.attribute.flt_value = (float)floatLexeme;
		return t;

	}

}
/*
Purpose: Creates and returns decimal integer literal token
Author:  Arjun Sivakumar
History/Versions: v1.0
Called Functions: strlen(), atol(), aa_table()
Parameters: char[] lexeme
Return value: Token
Algorithim: Checks to see if the lexeme is in range of 2-byte integer, if not return error token. If it is in range set the value and return DIL token
*/
Token aa_func05(char lexeme[]){ /*DIL*/

	Token t;
	long intLexeme;
	int lexemeLength;

	lexemeLength = strlen(lexeme);

	if (lexemeLength > INL_LEN) { /*If lexeme is larger than INL_LEN*/
		return aa_table[ES](lexeme);
	}

	intLexeme = atol(lexeme); /*Convert lexeme from string to long*/

	if ((intLexeme > SHRT_MAX) || (intLexeme < 0)) {
		return aa_table[ES](lexeme);
	}

	t.code = INL_T;
	t.attribute.int_value = (int)intLexeme;
	return t;

}
/*
Purpose: Creates and returns octal integer literal token
Author:  Arjun Sivakumar
History/Versions: v1.0
Called Functions: atool(), strlen()
Parameters: char[] lexeme
Return value: Token
Algorithim: Checks to see if the lexeme is in range of 2-byte integer, if not return error token. If it is in range set the value and return DIL token
*/
Token aa_func11(char lexeme[]){ /*OIL*/

	Token t;
	short intLexeme;
	int lexemeLength;
	int i;

	intLexeme = (short)atool(lexeme);
	lexemeLength = strlen(lexeme);


	if ((intLexeme > SHRT_MAX) || (intLexeme < 0)) { /*If int is larger or smaller than 2-byte int*/

		t.code = ERR_T;
		if (lexemeLength > ERR_LEN) {
			lexemeLength = ERR_LEN - 3;
			for (i = 0; i < lexemeLength; i++) {
				t.attribute.err_lex[i] = lexeme[i];
			}
			t.attribute.err_lex[i] = '.';
			t.attribute.err_lex[i + 1] = '.';
			t.attribute.err_lex[i + 2] = '.';
			t.attribute.err_lex[i + 3] = '\0';

			return t;

		}
		else {
			for (i = 0; i < lexemeLength; i++) {
				t.attribute.err_lex[i] = lexeme[i];
			}
			t.attribute.err_lex[i] = '\0';
			return t;
		}

	}

	else {
		/*Set the integer literal token*/
		t.code = INL_T;
		t.attribute.int_value = intLexeme;
		return t;
	}

}
/*
Purpose: Creates and returns error token
Author:  Arjun Sivakumar
History/Versions: v1.0
Called Functions: strlen()
Parameters: char[] lexeme
Return value: Token
Algorithim: Copies the lexeme into an error token, if the lexeme is greater than the size of the error token then the lexeme will be truncated to
the first 17 characters and append ... at the end. Then the function will return
*/
Token aa_func12(char lexeme[]){

	Token t;
	int lexemeLength;
	int i;

	lexemeLength = strlen(lexeme);
	t.code = ERR_T;

	if (lexemeLength > ERR_LEN) { /*If lexeme length is larger than ERR_LEN then only take the first 17 characters then append ...*/
		lexemeLength = ERR_LEN - 3;
		for (i = 0; i < lexemeLength; i++) {
			t.attribute.err_lex[i] = lexeme[i];
		}
		t.attribute.err_lex[i] = '.';
		t.attribute.err_lex[i + 1] = '.';
		t.attribute.err_lex[i + 2] = '.';
		t.attribute.err_lex[i + 3] = '\0';

		return t;

	}
	else { /*If lexeme length is less than ERR_LEN*/
		for (i = 0; i < lexemeLength; i++) {
			t.attribute.err_lex[i] = lexeme[i];
		}
		t.attribute.err_lex[i] = '\0';
		return t;
	}

}
/*
Purpose: Creates and returns error token
Author:  Arjun Sivakumar
History/Versions: v1.0
Called Functions: strlen()
Parameters: char[] lexeme
Return value: Token
Algorithim: Copies the lexeme into an error token, if the lexeme is greater than the size of the error token then the lexeme will be truncated to
the first 17 characters and append ... at the end. Then the function will return
*/
Token aa_func13(char lexeme[]) {

	Token t;
	int lexemeLength;
	int i;

	lexemeLength = strlen(lexeme);
	t.code = ERR_T;

	if (lexemeLength > ERR_LEN) { /*If lexeme length is larger than ERR_LEN then only take the first 17 characters then append ...*/
		lexemeLength = ERR_LEN - 3;
		for (i = 0; i < lexemeLength; i++) {
			t.attribute.err_lex[i] = lexeme[i];
		}
		t.attribute.err_lex[i] = '.';
		t.attribute.err_lex[++i] = '.';
		t.attribute.err_lex[++i] = '.';
		t.attribute.err_lex[++i] = '\0';

		return t;

	}
	else {
		for (i = 0; i < lexemeLength; i++) { /*If lexeme length is less than ERR_LEN*/
			t.attribute.err_lex[i] = lexeme[i];
		}
		t.attribute.err_lex[i] = '\0';
		return t;
	}

}
/*
Purpose: Converts string octal literal to decimal literal
Author:  Arjun Sivakumar
History/Versions: v1.0
Called Functions: strtol()
Parameters: char * lexeme
Return value: long
Algorithim:
*/
long atool(char * lexeme){

	return strtol(lexeme, NULL, 8);
}
/*
Purpose: Checks if lexme matches in kw_table[]
Author:  Arjun Sivakumar
History/Versions: v1.0
Called Functions: strcmp()
Parameters: char * kw_lexeme
Return value: int
Algorithim: Loops through table and checks to see if the lexeme matches anything in the kw_table
*/
int iskeyword(char * kw_lexeme){

	int i = 0;

	for (i = 0; i < KWT_SIZE; i++) {
		if (strcmp(kw_lexeme, kw_table[i]) == 0) {
			return i; /*If match is found return the index of the keyword from the table*/
		}
	}
	return R_FAIL1; /*If the lexeme does not any keyword in the table*/
}