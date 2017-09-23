/* File Name: table.h
Compiler: MS Visual Studio 2013
Author: Arjun Sivakumar, 040836029
Course: CST8152 - Compilers, Lab Section: 11
Assignment: 2
Date: March 3rd, 2017
Professor: Sv. Ranev
Purpose: Defining transition table and accepting states
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
*    '\0' or only one of the folowing constants: 255, 0xFF , EOF
*/
#define SEOF 255
#define SEOF_1 '\0'
/*  Single-lexeme tokens processed separately one by one
*  in the token-driven part of the scanner
*  '=' , ' ' , '(' , ')' , '{' , '}' , == , <> , '>' , '<' ,
*       space
*  !<comment , ',' , '"' , ';' , '-' , '+' , '*' , '/', << ,
*  .AND., .OR. , SEOF, 'wrong symbol',
*/


#define ES  12 /* Error state */
#define IS -1    /* Inavalid state */

/* State transition table definition */



#define TABLE_COLUMNS 7
/*transition table - type of states defined in separate table */
int  st_table[][TABLE_COLUMNS] = {
	/* State 0 */{ 1, 6, 4, 4, IS, IS, IS },            /* Not Accepting State */
	/* State 1 */{ 1, 1, 1, 1, 2, 3, 2 },               /* Not Accepting State */
	/* State 2 */{ IS, IS, IS, IS, IS, IS, IS },  /* Accepting State with retract */
	/* State 3 */{ IS, IS, IS, IS, IS, IS, IS },  /* Accepting State with no retract */
	/* State 4 */{ ES, 4, 4, 4, 7, 5, 5 },         /* Not Accepting State */
	/* State 5 */{ IS, IS, IS, IS, IS, IS, IS },  /* Accepting State with retract */
	/* State 6 */{ ES, 10, 9, ES, 7, ES, 5 },                            /* Not Accepting State */
	/* State 7 */{ 8, 7, 7, 7, 8, 8, 8 },                            /* Not Accepting State */
	/* State 8 */{ IS, IS, IS, IS, IS, IS, IS },  /* Accepting State with retract*/
	/* State 9 */{ ES, 9, 9, ES, ES, ES, 11 },                            /* Not Accepting State */
	/* State 10 */{ ES, ES, ES, ES, ES, ES, 11 },                           /* Not Accepting State */
	/* State 11 */{ IS, IS, IS, IS, IS, IS, IS }, /* Accepting State with retract */
	/* State 12 */{ IS, IS, IS, IS, IS, IS, IS }, /* Accepting State with no retract*/
	/* State 13 */{ IS, IS, IS, IS, IS, IS, IS }  /* Accepting State with retract */

};


/* Accepting state table definition */
#define ASWR   1    /* accepting state with retract */
#define ASNR   2    /* accepting state with no retract */
#define NOAS   3    /* not accepting state */

int as_table[] = { NOAS, NOAS, ASWR, ASNR, NOAS, ASWR, NOAS, NOAS, ASWR, NOAS, NOAS, ASWR, ASNR, ASWR };

/* Accepting action function declarations */


Token aa_func02(char *lexeme); /* AVID (Retract) */
Token aa_func03(char *lexeme); /* SVID (No Retract) */
Token aa_func05(char *lexeme); /* DIL (Retract) */
Token aa_func08(char *lexeme); /* FPL (Retract) */
Token aa_func11(char *lexeme); /* OIL (Retract) */
Token aa_func12(char *lexeme); /* ES (No Retract) */
Token aa_func13(char *lexeme); /* ESWR (Retract) */

/* defining a new type: pointer to function (of one char * argument)
returning Token
*/

typedef Token(*PTR_AAF)(char *lexeme);


/* Accepting function (action) callback table (array) definition */
/* If you do not want to use the typedef, the equvalent declaration is:
* Token (*aa_table[])(char lexeme[]) = {
*/

PTR_AAF aa_table[] = {
	NULL, /* State 0 */
	NULL, /* State 1 */
	aa_func02, /* State 2 */
	aa_func03, /* State 3 */
	NULL,      /* State 4 */
	aa_func05, /* State 5 */
	NULL,     /* State 6 */
	NULL,     /* State 7 */
	aa_func08, /* State 8 */
	NULL, /* State 9 */
	NULL, /* State 10 */
	aa_func11, /* State 11 */
	aa_func12, /* State 12 */
	aa_func13 /* State 13 */
};

/* Keyword lookup table (.AND. and .OR. are not keywords) */

#define KWT_SIZE  8

char * kw_table[] = {
	"ELSE",
	"IF",
	"INPUT",
	"OUTPUT",
	"PLATYPUS",
	"REPEAT",
	"THEN",
	"USING"
};

#endif
