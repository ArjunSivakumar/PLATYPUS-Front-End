/* File Name: stable.h
Compiler: MS Visual Studio 2013
Author: Arjun Sivakumar, 040836029, Jamal Rahman, 040819635
Course: CST 8152 - Compilers, Lab Section: 11
Assignment: 3
Date: March 28th, 2017
Professor: Sv. Ranev
Purpose: Declares constants, unions and function prototypes needed for symbol table implementation
Function List: st_create(), st_install(), st_lookup(), st_change_type(), st_change_value(),
st_get_type(), st_get_value(), st_destroy(), st_print(), st_store(), st_sort()
*/

#ifndef STABLE_H_
#define STABLE_H_

#ifndef BUFFER_H_
#include "buffer.h"
#endif


/* Macro Definitions */
#define BUFFER_CAPACITY 100/*Initial buffer capacity*/
#define INCR_FACTOR 15/*Buffer increment*/
#define ST_ERR -1/*Invalid symbol table*/
#define BUF_ADDC_ERR -1/*Add_c error*/
#define ST_FULL -1/*Symbol table full*/
#define LEX_NF -1/*Lexeme not found*/
#define TYPE_ERR -1/*Invalid type*/
#define OVERWRITE "wt"/*Default file mode*/
#define FILENAME "$stable.ste"/*Default output file*/
#define FOPEN_FAIL -1/*File opening fail*/
#define TYPE_UPDATE_FAIL -1/*Type update fail*/
#define DEF_STR_VAL -1/*Default string i_value*/
#define DEF_INT_VAL 0/*Default int i_value*/
#define DEF_FLT_VAL 0/*Default flt i_value*/

/* Mask Definitions */
#define INIT_MASK 0xFFF8/*Initial status mask*/
#define INT_MASK 0x0004/*Int status mask*/
#define FLT_MASK  0x0002/*Float status mask*/
#define STR_MASK 0x0006/*String status mask*/
#define UPDATE_MASK 0x0001/*Update LSB*/

/* Union Declarations */

typedef union InitialValue {
	int int_val; /* Integer variable initial value */
	float fpl_val; /* floating-point variable inital value */
	int str_offset; /* string literal initial value (offset) */
} Value;

typedef struct SymbolTableVidRecord {
	unsigned short status_field; /* Variable record status field */
	char * plex; /* Pointer to lexeme (VID name) in CA */
	int o_line; /* line of first occurance */
	Value i_value; /* variable initial value */
	void * reserved; /* Reserved for future use */
} STVR;

typedef struct SymbolTableDescriptor {
	STVR *pstvr; /* pointer to array STVR */
	int st_size; /* size in number of STVR elements */
	int st_offset; /* offset in number of STVR elements */
	Buffer *plsBD; /* pointer to lexeme storage buffer descriptor */
} STD;


/* Function Prototypes */
STD st_create(int st_size);
int st_install(STD sym_table, char *lexeme, char type, int line);
int st_lookup(STD sym_table, char *lexeme);
int st_change_type(STD sym_table, int vid_offset, char v_type);
int st_change_value(STD sym_table, int vid_offset, Value value);
char st_get_type(STD sym_table, int vid_offset);
Value st_get_value(STD sym_table, int vid_offset);
void st_destroy(STD sym_table);
int st_print(STD sym_table);
int st_store(STD sym_table);
int st_sort(STD sym_table, char s_order);


#endif
