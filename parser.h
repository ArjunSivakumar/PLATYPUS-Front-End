/*	File Name: parser.h
	Compiler: MS Visual Studio 2013
	Author: Arjun Sivakumar, 040836029, Jamal Rahman, 040819635
	Course: CST 8152 - Compilers, Lab Section: 11
	Assignment: 4
	Date: April 18th, 2017
	Professor: Sv. Ranev
	Purpose: Declaring all function prototypes and necessary constants needed for parser implementation
*/


#ifndef PARSER_H_
#define PARSER_H_

#ifndef BUFFER_H_
#include "buffer.h"
#endif

#ifndef TOKEN_H_
#include "token.h"
#endif

#ifndef STABLE_H_
#include "stable.h"
#endif

/*Constants used for the index position in kw_table array*/

#define ELSE 0
#define IF 1
#define INPUT 2
#define OUTPUT 3
#define PLATYPUS 4
#define REPEAT 5
#define THEN 6
#define USING 7
#define NO_ATTR 8

/*Constant Definitions*/
#define TABLE_SIZE 8

/*Static Global Variables*/
static Token lookahead;
static Buffer * sc_buf;

/*Global Variables*/
int synerrno;

/*Extern variables (defined in other files)*/
extern int line;
extern STD sym_table;
extern Buffer * str_LTBL;
extern char * kw_table[TABLE_SIZE];
extern Token malar_next_token(Buffer * sc_buf);

/* Function prototypes (parser related) */
void parser(Buffer *);
void match(int, int);
void syn_eh(int);
void syn_printe(void);
void gen_incode(char *);

/* Function prototypes (production related) */
void program(void);
void opt_statements(void);
void statements(void);
void statements_p(void);
void statement(void);
void assignment_statement(void);
void assignment_expression(void);
void selection_statement(void);
void iteration_statement(void);
void input_statement(void);
void variable_list(void);
void variable_list_p(void);
void variable_identifier(void);
void output_statement(void);
void output_list(void);
void arithmetic_expression(void);
void unary_arithmetic_expression(void);
void additive_arithmetic_expression(void);
void additive_arithmetic_expression_p(void);
void multiplicative_arithmetic_expression(void);
void multiplicative_arithmetic_expression_p(void);
void primary_arithmetic_expression(void);
void string_expression(void);
void string_expression_p(void);
void primary_string_expression(void);
void conditional_expression(void);
void logical_OR_expression(void);
void logical_OR_expression_p(void);
void logical_AND_expression(void);
void logical_AND_expression_p(void);
void relational_expression(void);
void relational_operator(void);
void primary_a_relational_expression(void);
void primary_s_relational_expression(void);

#endif
