/*
File Name: buffer.c
Compiler: MS Visual Studio 2013
Author: Arjun Sivakumar, 040836029
Course: CST8152 - Compilers, Lab Section: 11
Assignment: 1
Date: Feburary 2nd, 2017
Professor: Sv. Ranev
Purpose: The program allocates a buffer that can either have a fixed capacity, additive capacity or a
multiplicitve capacity depending on the buffer settings. The buffer will read text from a file
into the buffer and then display the contents from the buffer, character by character.
Function list: b_create(), b_addc(), b_reset(), b_free(), b_isfull(), b_size(), b_capacity(), b_setmark(),
b_mark(), b_mode(), b_incfactor(), b_load(), b_isempty(), b_isempty(), b_eob(), b_getc(),
b_print(), b_pack(), b_rflag(), b_retract(), b_retract_to_mark(), b_getcoffset()
*/

#include "buffer.h" /*Struct definition, macro definition and constant definition*/

/*
Purpose: Create and dynmaically allocate Buffer and character Buffer onto the heap
with appropicate capacity, increment factor and operation mode (fixed, additive or multiplicative)
Author: Arjun Sivakumar
History/Versions: v1.0 (Feburary 2, 2017)
Called functions: calloc(), malloc(), free()
Parameters: short init_capacity, char inc_factor (>= 1 and <=255 for o_mode 'a') or (>= 1 and <= 100 for o_mode 'm'), char o_mode ('f','a' or 'm')
Algorithm: Function attempts to create and allocate a buffer and a character buffer assuming that all parameters are true and there is no bad values. After the buffer is allocated
the function will attempt to set the capacity, the increment factor and the mode of the buffer. If bad values exists the function will free up all the dynamically allocated memory and the function
will return NULL as a result of it failing.

*/


Buffer* b_create(short init_capacity, char inc_factor, char o_mode) {

	Buffer *pBuffer;

	if (init_capacity <= 0 || inc_factor < 0) /*Init_capacity cannot have a negative value*/
		return NULL;



	/*allocates memory for a buffer pointer */
	pBuffer = (Buffer*)calloc(1, sizeof(Buffer));

	if (pBuffer == NULL) {
		return NULL;
	}

	/*allocates memory for dynamic char buffer*/
	pBuffer->cb_head = (char *)malloc(init_capacity);

	/*If the buffer pointer and character pointer are not initalized correctly*/
	if (pBuffer->cb_head == NULL) {

		/*free the dynamically allocated memory*/
		free(pBuffer);
		return NULL;
	}

	if (o_mode == 'f' || inc_factor == 0){

		pBuffer->inc_factor = 0;
		pBuffer->mode = 0;
	}

	else if (o_mode == 'a' && ((unsigned char)inc_factor >= 1 && (unsigned char)inc_factor <= MAX_INC_FACTOR_A)) {

		pBuffer->mode = 1;
		pBuffer->inc_factor = (unsigned char)inc_factor;

	}

	else if (o_mode == 'm' && (inc_factor >= 1 && inc_factor <= MAX_INC_FACTOR_M)) {

		pBuffer->mode = -1;
		pBuffer->inc_factor = (unsigned char)inc_factor;

	}

	else  { /*If o_mode is invalid or if inc_factor is not in range*/

		free(pBuffer->cb_head);
		free(pBuffer);
		return NULL;

	}


	pBuffer->capacity = init_capacity;

	/*Return the pointer if all is sucessfull*/
	return pBuffer;


}

/*
Purpose: Attempts to add character symbol to the buffer, if the adding of the symbol fails the function will attempt to expand the capacity and reallocate new memory for the buffer
Author: Arjun Sivakumar
History/Versions: v1.0 (Feburary 2, 2016)
Called functions: relloc(), free()
Parameters: pBuffer const pBD, char symbol
Algorithm:  The function will check if there is enough capacity in the buffer to add a new character. If such space is available then the buffer will add the symbol.
If the buffer capacity is full, the function will check to see if the buffer's operational mode allows for the buffer to expand it's capacity.
If the buffer's operational mode is in fixed-mode the function will not expand the capacity of the buffer and the function will return NULL.
However, if the buffer's operational mode is either additive mode or multiplicative mode then buffer will attempt to resize the capacity. If the
calculation of the new capacity is sucessfull then the function will reallocate the buffer with the newly calculated capacity.

*/


pBuffer b_addc(pBuffer  const pBD, char symbol) {

	/*Used to calculate and make logical evaluations before overwriting capacity in buffer*/
	short tempCapacity = 0, checkCapacity = 0, newIncrement = 0;
	int bufferFull;
	char *newCharBuffer;

	/*Checking if an initalized pointer to buffer is passed */
	if (pBD == NULL) {

		return NULL;

	}
	/*resets reallocation flag*/
	pBD->r_flag = 0;

#ifdef B_ISFULL
	bufferFull = B_ISFULL(pBD);
#else
	bufferFull = b_isfull(pBD);
#endif // B_ISFULL



	/*Since addc_offset is measured in bytes, it can be compared with buffer capacity*/
	if (bufferFull == BUFFER_NOT_FULL) { /*If offset has not reached the buffer capacity*/
		pBD->cb_head[pBD->addc_offset] = symbol;
		pBD->addc_offset += 1;
		return pBD;
	}

	/*If buffer is in fixed mode*/
	if (pBD->mode == 0) {
		return NULL;
	}

	/*If buffer is in additive self-incrementing mode*/
	else if (pBD->mode == 1) {

		tempCapacity = pBD->capacity + (pBD->inc_factor * sizeof(char)); /*New capacity is calculated using the additive incrementing formula*/

		/*If the new capacity overflows*/
		if (tempCapacity < 0 || tempCapacity > SHRT_MAX) {

			return NULL;

		}


	}

	/*If buffer is in  multiplicative self-incrementing mode*/
	else if (pBD->mode == -1) {

		if (pBD->capacity == SHRT_MAX)
		{
			return NULL;
		}

		checkCapacity = SHRT_MAX - pBD->capacity;
		newIncrement = (short)(checkCapacity * ((float)pBD->inc_factor / 100)); /*WARNING: The inc factor needs to be in float in order for operation to be sucessfull*/
		tempCapacity = (short)(pBD->capacity + newIncrement);

		if (tempCapacity > SHRT_MAX) { /*If newly calculated capacity overflows into negative number*/

			tempCapacity = SHRT_MAX; /*Set the capacity to maximum size of buffer*/
		}

	}

	/*Attempt to expand buffer with new capacity*/
	newCharBuffer = (char *)realloc(pBD->cb_head, tempCapacity);

	/*If the memory for the new buffer cannot be allocated*/
	if (newCharBuffer == NULL) {
		return NULL;
	}

	if (newCharBuffer != pBD->cb_head) { /*If the capacity has been sucessfully changed*/
		pBD->r_flag = SET_R_FLAG;
		pBD->cb_head = newCharBuffer;
	}
	/*continue to add character symbol to buffer*/
	pBD->cb_head[pBD->addc_offset] = symbol;
	++pBD->addc_offset;
	pBD->capacity = tempCapacity;

	return pBD;

}

/*
Purpose: Reset all the members in the buffer so that it looks like it's pointer to the beginning of the character array
Author: Arjun Sivakumar
History/Versions: v1.0 (Feburary 2, 2016)
Called functions: n/A
Parameters: PBuffer * const pBD
Algorithm:

*/

int b_reset(Buffer* const pBD) {

	if (pBD == NULL) {
		return  R_FAIL1;
	}
	pBD->addc_offset = 0;
	pBD->getc_offset = 0;
	pBD->markc_offset = 0;
	pBD->eob = 0;
	pBD->r_flag = 0;
	return 0;
}

/*
Purpose: Frees the dynamically allocated memory for the buffer and the character buffer
Author: Arjun Sivakumar
History/Versions: v1.0 (Feburary 2, 2016)
Called functions: free()
Parameters: Buffer * const pBD
Algorithm:

*/

void b_free(Buffer* const pBD) {

	if (pBD != NULL) {

		if (pBD->cb_head != NULL) {
			free(pBD->cb_head);
		}
		free(pBD);
	}

}

/*
Purpose: Checks to see if the buffer has reached the full capacity
Author: Arjun Sivakumar
History/Versions: v1.0 (Feburary 2, 2016)
Called functions:
Parameters: Buffer * const pBD
Algorithm: If the buffer's addc_offset is equal to the capacity then the buffer is full, if not the
buffer can continue to add characters to the buffer
*/

int b_isfull(Buffer * const pBD) { /*Finished*/

	if (pBD == NULL)
		return R_FAIL1;

	if ((unsigned short)(pBD->addc_offset * sizeof(char)) == (unsigned short)pBD->capacity) /*No more space in buffer*/
		return BUFFER_FULL;

	return 0;
}

/*
Purpose: Returns the current size of the buffer
Author: Arjun Sivakumar
History/Versions: v1.0 (Feburary 2, 2016)
Called functions:
Parameters: Buffer * const pBD
Algorithm:
*/

short b_size(Buffer * const pBD) {

	if (pBD == NULL)
		return R_FAIL1;


	return pBD->addc_offset;

}

/*
Purpose: Returns the maximum capacity of the buffer
Author: Arjun Sivakumar
History/Versions: v1.0 (Feburary 2, 2016)
Called functions:
Parameters: Buffer * const pBD
Algorithm:
*/

short b_capacity(Buffer * const pBD) {

	if (pBD == NULL)
		return R_FAIL1;

	return pBD->capacity;

}

/*
Purpose: Sets the buffer's markc_offset
Author: Arjun Sivakumar
History/Versions: v1.0 (Feburary 2, 2016)
Called functions:
Parameters: Buffer * const pBD, short mark
Algorithm:
*/

char* b_setmark(Buffer * const pBD, short mark){

	if (pBD == NULL) /*WARNING: The instructions of the assignment say to return null if a run time error can occur*/
		return NULL;

	if (mark >= 0 || mark <= pBD->addc_offset) /*mark parameter cannot be negative or greater than the addc_offset*/
		pBD->markc_offset = mark;
	else
		return NULL;

	return &pBD->cb_head[pBD->markc_offset]; /*return buffer with location of markc_offset*/

}

/*
Purpose: Returns the buffer's markc_offset
Author: Arjun Sivakumar
History/Versions: v1.0 (Feburary 2, 2016)
Called functions:
Parameters: Buffer * const pBD2
Algorithm:
*/

short b_mark(Buffer * const pBD){

	if (pBD == NULL)
		return R_FAIL1;

	return pBD->markc_offset;

}


/*
Purpose: Returns the buffer's op_mode
Author: Arjun Sivakumar
History/Versions: v1.0 (Feburary 2, 2016)
Called functions:
Parameters: Buffer * const pBD
Algorithm:
*/
int b_mode(Buffer * const pBD){

	if (pBD == NULL)
		return R_FAIL1;

	return pBD->mode;

}

/*
Purpose: Returns the buffer's inc_factor
Author: Arjun Sivakumar
History/Versions: v1.0 (Feburary 2, 2016)
Called functions:
Parameters: Buffer * const pBD
Algorithm:
*/

size_t b_incfactor(Buffer * const pBD) {

	if (pBD == NULL)
		return R_INC_FACTOR;

	return (unsigned char)pBD->inc_factor;

}


/*
Purpose: Reads a single character at a time and adds the character to the character buffer
Author: Arjun Sivakumar
History/Versions: v1.0 (Feburary 2, 2016)
Called functions: feof(), fgetc()
Parameters: FILE * const fi, Buffer * const pBD
Algorithm: Checks to see if the file or the buffer is NULL if it is, return an -1 (error code). However, if the
buffer is not NULL and file contains characters then the function will continue. The file is read in
a single character at a time and at the same time. If the character being added to the character buffer
was an unsucessful operation then the function will return an error (LOAD_FAIL), also if the file pointer
has reached the end of file character then the function will return the amount of characters added to the
character buffer.
*/


int b_load(FILE * const fi, Buffer * const pBD){

	int numOfChars = 0;
	char bufferChar;

	if (fi == NULL || pBD == NULL)
		return R_FAIL1;

	while (1) { /*Warning: using constant in while loop*/

		bufferChar = (char)fgetc(fi); /*Retrieve single char from file*/

		if (feof(fi)) /*Reached end of file exit loop*/
			break;

		if (b_addc(pBD, bufferChar) == NULL) { /*Attempt to add char to the buffer*/
			return LOAD_FAIL;
		}

		++numOfChars;
	}

	return numOfChars;


}

/*
Purpose: Checks to see if the buffer is empty
Author: Arjun Sivakumar
History/Versions: v1.0 (Feburary 2, 2016)
Called functions:
Parameters: Buffer * const pBD
Algorithm:
*/

int b_isempty(Buffer * const pBD){

	if (pBD == NULL) {

		return R_FAIL1;
	}

	if (pBD->addc_offset == 0)
		return 1;

	return 0;


}

/*
Purpose: Returns the buffer's eob status
Author: Arjun Sivakumar
History/Versions: v1.0 (Feburary 2, 2016)
Called functions:
Parameters: Buffer * const pBD
Algorithm:
*/

int b_eob(Buffer * const pBD){

	if (pBD == NULL)
		return R_FAIL1;

	return pBD->eob;

}

/*
Purpose: Retrives every character from the character buffer
Author: Arjun Sivakumar
History/Versions: v1.0 (Feburary 2, 2016)
Called functions:
Parameters: Buffer * const pBD
Algorithm: Checks to see if the buffer's getc_offset is equal to addc_offset, if it is then the buffer will signal that the program that the buffer is at the end
and will set the eob flag to 1 and return error code R_FAIL_1. If not then the function will store the location of the character retrived into a char and
then return the character to the appropriate function. The function will then increment the getc_offset so that when the function is called again the next
character will be retrived.
*/

char b_getc(Buffer * const pBD){

	char c = 0;

	if (pBD == NULL)
		return R_FAIL2;

	if (pBD->getc_offset == pBD->addc_offset) { /*getc_offset cannot go past addc_offset*/

		pBD->eob = 1;
		return R_FAIL1;
	}

	else {
		pBD->eob = 0;
		c = pBD->cb_head[pBD->getc_offset];
		++pBD->getc_offset;
	}


	return c; /*Return char retrived from the buffer*/

}

/*
Purpose: Prints the contents of the buffer onto the screen
Author: Arjun Sivakumar
History/Versions: v1.0 (Feburary 2, 2016)
Called functions: b_isempty(), b_eob() ,b_getc()
Parameters: Buffer * const pBD
Algorithm: The function checks to see if the buffer is first empty, if it is return R_FAIL1 and exits. If not the function will retrieve every character from the character buffer
using the b_getc() function, and as the characters are being retrived the function will begin to print the contents of the buffer.
*/

int b_print(Buffer * const pBD){
	char symbol;
	int numOfChars = 0;
	short tempOffset = 0;

	if (pBD == NULL) {

		return R_FAIL1;
	}

	if (b_isempty(pBD) == 1) { /*If buffer has no contents to print*/
		printf("The buffer is empty.\n");
		return 0;
	}
	tempOffset = pBD->getc_offset;
	pBD->getc_offset = 0;

	while (1) {/*Warning: using constant in while loop*/

		symbol = b_getc(pBD);

		if (b_eob(pBD) == 1) {
			break;
		}

		else {
			printf("%c", symbol);  /*Retrieve and print each individual char from the buffer*/
			numOfChars++;
		}

	}
	printf("\n");

	pBD->getc_offset = tempOffset;
	return numOfChars;

}

/*
Purpose: Attempts to add capacity for one extra character in the buffer. If sucessful the function will rellocate the buffer with space for one extra character
Author: Arjun Sivakumar
History/Versions: v1.0 (Feburary 2, 2016)
Called functions: realloc()
Parameters: Buffer * const pBD
Algorithm: The function will try to add on more capacity for one extra character. If the capacity overflows then the function returns.
If not, then the function will reallocate memeory for the newly calculated capacity and assigned the new capacity to the
buffer's capacity data memeber.
*/

Buffer * b_pack(Buffer * const pBD){

	short newCapacity;
	char * tempBuffer;

	if (pBD == NULL) {
		return NULL;
	}
	pBD->r_flag = 0;

	newCapacity = (pBD->addc_offset + 1) * sizeof(char); /*Adding additional capacity for one extra character*/

	if (newCapacity < 0) {
		return NULL;
	}


	tempBuffer = (char *)realloc(pBD->cb_head, newCapacity); /*Reallocate buffer with capacity for one extra character*/

	if (tempBuffer == NULL) {
		return NULL;
	}

	if (tempBuffer != pBD->cb_head) {
		pBD->r_flag = SET_R_FLAG;
		pBD->cb_head = tempBuffer;
	}

	pBD->capacity = newCapacity;
	return pBD;

}

/*
Purpose: Returns buffer's r_flag
Author: Arjun Sivakumar
History/Versions: v1.0 (Feburary 2, 2016)
Called functions:
Parameters: Buffer * const pBD
Algorithm:
*/


char b_rflag(Buffer * const pBD){

	if (pBD == NULL) {

		return R_FAIL1;

	}

	return pBD->r_flag;

}

/*
Purpose: Retracts the getc_offset by one
Author: Arjun Sivakumar
History/Versions: v1.0 (Feburary 2, 2016)
Called functions:
Parameters: Buffer * const pBD
Algorithm:
*/

short b_retract(Buffer * const pBD){

	if (pBD == NULL) {

		return R_FAIL1;
	}

	--pBD->getc_offset; /*Move getc_offset back by one */

	return pBD->getc_offset;

}

/*
Purpose: Retracts the getc_offset to the location of markc_offset
Author: Arjun Sivakumar
History/Versions: v1.0 (Feburary 2, 2016)
Called functions:
Parameters: Buffer * const pBD
Algorithm:
*/

short b_retract_to_mark(Buffer * const pBD){

	if (pBD == NULL) {

		return R_FAIL1;
	}

	pBD->getc_offset = pBD->markc_offset; /*Move getc_offset to whereever markc_offset is located*/

	return pBD->getc_offset;


}

/*
Purpose: Returns the buffer's getc_offset
Author: Arjun Sivakumar
History/Versions: v1.0 (Feburary 2, 2016)
Called functions:
Parameters: Buffer * const pBD
Algorithm:
*/


short b_getcoffset(Buffer * const pBD){

	if (pBD == NULL) {

		return R_FAIL1;

	}

	return pBD->getc_offset;

}
