/* File Name: stable.c
Compiler: MS Visual Studio 2013
Author: Arjun Sivakumar, 040836029, Jamal Rahman, 040819635
Course: CST 8152 - Compilers, Lab Section: 11
Assignment: 3
Date: March 28th, 2017
Professor: Sv. Ranev
Purpose: Functions for symbol table creation and management
Function List: st_create(), st_install(), st_lookup(), st_change_type(), st_change_value(),
st_get_type(), st_get_value(), st_destroy(), st_print(), st_store(), st_sort()
*/

#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "buffer.h"
#include "stable.h"

/* static function definitions */
static void st_setsize(void);
static void st_incoffset(void);

/* global variable declaration */
extern STD sym_table;

/*
	Purpose: To create a symbol table
	Author: Arjun Sivakumar
	History/Versions: v1.0 (March 23, 2017)
	Called functions: malloc(), b_create(), free()
	Parameters: int st_size (cannot be a negative number or zero)
	Return value: STD with size 0 on error, or STD with non-negative and non-zero value on success
	Algorithm: Check if the parameters are within range, if so then continue to create the symbol table.
	Attempt to dynamically allocate the pointer to STVR and assign it to the pstvr in the STD struct.
	If the memory is not allocated then the function will return a symbol table with the size of 0 (which is invalid).
	The function will create a buffer and assign it to the plsBD* in the STD struct. If the buffer creation fails, the
	function will free any dynamically allocated memory and return a symbol table with a size of 0 (invalid symbol table).
	If all is sucessfull the symbol table will be assgined the correct st_size, set the st_offset to 0 and return the symbol table.


	*/

STD st_create(int st_size) {

	STD std;/*Temporary STD*/

	std.st_size = 0;

	/*The size of the table cannot be 0 or a negative number*/
	if (st_size <= 0) {
		return std;
	}

	/*Dynamically allocate pointer to STVR and set value of psvtr to newly allocated pointer*/

	std.pstvr = (STVR*)malloc(st_size * sizeof(STVR));

	/*If the memory cannot be correctly allocated*/
	if (std.pstvr == NULL) {
		return std;
	}

	/*Create buffer and assign pointer to buffer*/
	std.plsBD = b_create(BUFFER_CAPACITY, INCR_FACTOR, 'a');

	/*If the buffer could not be created then the symbol table cannot exist*/
	if (std.plsBD == NULL) {
		free(std.pstvr);
		return std;
	}

	/*If all else is successful*/
	std.st_offset = 0;
	std.st_size = st_size;
	return std;

}

/*
	Purpose: Installs VID entry into symbol table
	Author: Arjun Sivakumar and Jamal Rahman
	History/Versions: v1.0 (March 28th, 2017)
	Called functions:
	Parameters: STD sym_table, char *lexeme, char type, int line
	Return value: offset if function is successful, -1 if not
	Algorithm: Checks to see if the lexeme is already in the symbol table if it is, then return the position of the
	where the lexeme is located in the symbol table. If not, then read the lexeme into buffer and plex is now
	pointing to the lexeme in the symbol table, the appropriate mask is invoked, the value of is stored and the
	offset is incremented and then returned
	*/
int st_install(STD sym_table, char *lexeme, char type, int line) {

	int offset;/*The offset that will be returned*/
	int i;/*Counter*/
	int lexemeLen;/*length of the lexeme parameter*/
	char bufferMoved;/*buffer moved flag*/

	/*If a table could not be created in the st_create() function*/
	if (sym_table.st_size == 0) {
		return ST_ERR;
	}

	/*Check if lexeme is already in table*/
	offset = st_lookup(sym_table, lexeme);

	/*If the offset is the same as the symbol table size */
	if (sym_table.st_offset == sym_table.st_size && offset == LEX_NF) {/*Ask Svillen aboot this*/
		return ST_FULL;
	}

	/*Return the current offset if the lexeme was found in the symbol table*/
	if (offset != LEX_NF) {
		return offset;
	}

	/*If lexeme was not found set the offset to be the next position in the symbol table*/
	offset = sym_table.st_offset;

	/*The buffer hasn't been moved*/
	bufferMoved = 0;

	/*Set the plex to beginning of the lexeme (i think) */
	sym_table.pstvr[offset].plex = b_setmark(sym_table.plsBD, b_size(sym_table.plsBD));

	/*Set the first line of occurance to the line where the lexeme was found*/
	sym_table.pstvr[sym_table.st_offset].o_line = line;

	lexemeLen = strlen(lexeme);

	/*Copy lexeme into lexeme storage*/
	for (i = 0; i < lexemeLen; i++) {
		if (b_addc(sym_table.plsBD, lexeme[i]) == NULL) {
			return BUF_ADDC_ERR;
		}
		if (b_rflag(sym_table.plsBD))
		{
			bufferMoved = 1;
		}
	}
	/*Null terminated char added for c-style strings*/
	b_addc(sym_table.plsBD, '\0');

	if (!bufferMoved)
	{
		bufferMoved = b_rflag(sym_table.plsBD);
	}

	/*Depending on what variable needs to be installed set the corresponding status_field and initalize the value*/
	if (type == 'F') { /*For float type*/
		sym_table.pstvr[offset].status_field = INIT_MASK;
		sym_table.pstvr[offset].status_field |= FLT_MASK;
		sym_table.pstvr[offset].i_value.fpl_val = 0.0F;
	}
	else if (type == 'I') { /*For int type*/
		sym_table.pstvr[offset].status_field = INIT_MASK;
		sym_table.pstvr[offset].status_field |= INT_MASK;
		sym_table.pstvr[offset].i_value.int_val = DEF_INT_VAL;
	}
	else if (type == 'S') {/*For string type*/
		sym_table.pstvr[offset].status_field = INIT_MASK;
		sym_table.pstvr[offset].status_field |= UPDATE_MASK;
		sym_table.pstvr[offset].status_field |= STR_MASK;
		sym_table.pstvr[offset].i_value.str_offset = DEF_STR_VAL;
	}

	/*Set pointer to updated locations*/
	if (bufferMoved)
	{
		for (i = sym_table.st_offset; i >= 0; i--)
		{
			sym_table.pstvr[i].plex = b_setmark(sym_table.plsBD, ((short)(&sym_table.pstvr[i].plex - &sym_table.pstvr[0].plex)));
		}
	}

	/*Increment offset before returning*/
	st_incoffset();

	return offset;
}


/*
	Purpose: Checks symbol table for lexeme
	Author: Jamal Rahman
	History/Versions: v1.0 (March 25, 2017)
	Called functions: strcmp()
	Parameters: STD sym_table, char *lexeme
	Return value: location of where lexeme is located, -1 if lexeme could not be found
	Algorithm: Loops and checks to see if parameter lexeme already exists in the symbol table
	*/
int st_lookup(STD sym_table, char * lexeme)
{
	int i;/*Counter*/
	
	/*invalid symbol table*/
	if (sym_table.st_size == 0)
	{
		return ST_ERR;
	}

	/*check for matching lexeme*/
	for (i = sym_table.st_offset - 1; i >= 0; i--)
	{
		if (strcmp(lexeme, sym_table.pstvr[i].plex) == 0)
		{
			return i;
		}
	}

	return LEX_NF; /*If the lexeme is not found in the table*/
}

/*
	Purpose: Changes the varaible type to the type indicated by v_type
	Author: Arjun Sivakumar
	History/Versions: v1.0 (March 25, 2017)
	Parameters: STD sym_table, int vid_offset, char v_type
	Return value: -1 on error, or vid_offset
	Algorithm:
	*/

int st_change_type(STD sym_table, int vid_offset, char v_type)
{
	/*invalid symbol table*/
	if (sym_table.st_size == 0 || vid_offset >= sym_table.st_offset || vid_offset < 0)
	{
		return ST_ERR;
	}

	/*Type already updated*/
	if ((sym_table.pstvr[vid_offset].status_field & UPDATE_MASK) == UPDATE_MASK) {
		return TYPE_UPDATE_FAIL;
	}

	/*Check the v_type, change it accordingly*/
	if (v_type == 'F') {
		sym_table.pstvr[vid_offset].status_field |= FLT_MASK;
		sym_table.pstvr[vid_offset].status_field |= UPDATE_MASK;
	}

	if (v_type == 'I') {
		sym_table.pstvr[vid_offset].status_field |= INT_MASK;
		sym_table.pstvr[vid_offset].status_field |= UPDATE_MASK;
	}

	return vid_offset;

}
/*
Purpose: Change the value of i_value to the value parameter
Author: Jamal Rahman
History/Versions: v1.0 (March 25, 2017)
Parameters: STD sym_table, int vid_offset, Value value
Return value: the offset of what value was changed
*/
int st_change_value(STD sym_table, int vid_offset, Value value) {

	if (sym_table.st_size == 0 || vid_offset >= sym_table.st_offset || vid_offset < 0) 
	{
		return ST_ERR;
	}

	sym_table.pstvr[vid_offset].i_value = value;

	return vid_offset;
}
/*
Purpose: Determines the type of the variable at vid_offset
Author: Arjun Sivakumar and Jamal Rahman
History/Versions: v1.0 (March 25, 2017)
Parameters: STD sym_table, int vid_offset
Return value: S, F, or I depending on the determined type, or -1 on error
*/
char st_get_type(STD sym_table, int vid_offset) {

	unsigned short status;/*Retrieved status from pstvr at vid_offset*/

	if (sym_table.st_size == 0 || vid_offset >= sym_table.st_offset || vid_offset < 0)
	{
		return ST_ERR;
	}

	/*Get the status from the position of where vid_offset is*/
	status = sym_table.pstvr[vid_offset].status_field;

	/*Determine and return the corresponding letter of the type*/
	if ((status & STR_MASK) == STR_MASK) {
		return 'S';
	}

	if ((status & INT_MASK) == INT_MASK) {
		return 'I';
	}
	
	if ((status & FLT_MASK) == FLT_MASK) {
		return 'F';
	}

	return TYPE_ERR;

}

/*
Purpose: Returns the value at the given vid_offset. If called with invalid parameters, this function is unpredictable. 
Valid vid_offset between 0 and sym_table.st_offset
Author: Jamal Rahman
History/Versions: v1.0 (March 25, 2017)
Parameters: STD sym_table, int vid_offset
Return value: the value variable at vid_offset
*/
Value st_get_value(STD sym_table, int vid_offset)
{
	return sym_table.pstvr[vid_offset].i_value;
}
/*
Purpose: Free all memory associated with the parameter
Author: Arjun Sivakumar
History/Versions: v1.0 (March 25, 2017)
Called functions: b_free(), free()
Parameters: STD sym_table
Algorithm: Check if sym_table elements are valid, if so, free memory associated with it
*/
void st_destroy(STD sym_table) {

	if (sym_table.st_size == 0)
	{
		return;
	}

	if (sym_table.plsBD != NULL) {
		b_free(sym_table.plsBD);
	}

	if (sym_table.pstvr != NULL) {
		free(sym_table.pstvr);
	}

	st_setsize();
}
/*
Purpose: Output the symbol table
Author: Jamal Rahman
History/Versions: v1.0 (March 28th, 2017)
Called functions: printf()
Parameters: STD sym_table
Return value: -1 if invalid symbol table, else, the number of Variable Identifiers printed
*/
int st_print(STD sym_table) {

	int i;/*Counter*/

	if (sym_table.st_size == 0) {
		return ST_ERR;
	}

	printf("\nSymbol Table\n");
	printf("____________\n\n");
	printf("Line Number Variable Identifier\n");

	for (i = 0; i < sym_table.st_offset; i++)
	{
		printf("%2d          %s\n", sym_table.pstvr[i].o_line, sym_table.pstvr[i].plex);
	}

	return i;

}
/*
Purpose: Set global sym_table's size to 0
Author: Arjun Sivakumar
History/Versions: v1.0 (March 25, 2017)
*/
static void st_setsize(void) { /*Arjun*/

	sym_table.st_size = 0;
}
/*
Purpose: Increments the global sym_table's st_offset
Author: Arjun Sivakumar
History/Versions: v1.0 (March 25, 2017)
*/
static void st_incoffset(void) {

	++sym_table.st_offset;

}
/*
Purpose: Write the symbol table to a file
Author: Jamal Rahman
History/Versions: v1.0 (March 25, 2017)
Called functions: fopen(), fprintf(), printf(), fclose(), st_get_type()
Parameters: STD sym_table
Return value: -1 on error, else, number of lines outputted
*/
int st_store(STD sym_table)
{
	int i;/*Counter*/
	FILE * fi;/*File pointer to write to*/

	if (sym_table.st_size == 0)
	{
		return ST_ERR;
	}

	if (((fi = fopen(FILENAME, OVERWRITE))) == NULL)
	{
		return FOPEN_FAIL;
	}

	fprintf(fi, "%d", sym_table.st_size);

	/*Loop to output the variable information to the file*/
	for (i = 0; i < sym_table.st_offset; i++)
	{
		switch (st_get_type(sym_table, i))
		{
		case 'I': fprintf(fi, " %hX %d %s %d %d", sym_table.pstvr[i].status_field, strlen(sym_table.pstvr[i].plex), sym_table.pstvr[i].plex,
			sym_table.pstvr[i].o_line, sym_table.pstvr[i].i_value.int_val);
			break;

		case 'F': fprintf(fi, " %hX %d %s %d %.2f", sym_table.pstvr[i].status_field, strlen(sym_table.pstvr[i].plex), sym_table.pstvr[i].plex,
			sym_table.pstvr[i].o_line, sym_table.pstvr[i].i_value.fpl_val);
			break;

		case 'S': fprintf(fi, " %hX %d %s %d %d", sym_table.pstvr[i].status_field, strlen(sym_table.pstvr[i].plex), sym_table.pstvr[i].plex,
			sym_table.pstvr[i].o_line, sym_table.pstvr[i].i_value.str_offset);
			break;
		}
	}

	fclose(fi);

	printf("\nSymbol Table stored.\n");

	return i;
}
/*
Purpose: Sort the symbol table in ascending or descending order (not implemented)
Author: Arjun Sivakumar
History/Versions: v1.0 (March 25, 2017)
Parameters: STD sym_table, char s_order
Return value: 0
*/
int st_sort(STD sym_table, char s_order) {

	/*This function will generate warnings because of the unused parameteres in the parameter list*/
	return 0;
}

