/*	File Name: parser.c
	Compiler: MS Visual Studio 2013
	Author: Arjun Sivakumar, 040836029, Jamal Rahman, 040819635
	Course: CST 8152 - Compilers, Lab Section: 11
	Assignment: 4
	Date: April 18th, 2017
	Professor: Sv. Ranev
	Purpose: A RDPP used to parse statements in the PLATYPUS language

*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "parser.h"

/*Parser functions*/

/*Provided by: Sv. Ranev*/
void parser(Buffer * in_buf) {
	
	sc_buf = in_buf;
	lookahead = malar_next_token(sc_buf); /*Ask Sv. about this*/
	program(); match(SEOF_T, NO_ATTR);
	gen_incode("PLATY: Source file parsed");

}
/*
Purpose: matches the lookahead token to an expected token
Author:  Jamal Rahman
History/Versions: v1.0
Called Functions: syn_printe(), syn_eh(), malar_next_token()
Parameters: int pr_token_code, int pr_token_attribute
Return value: void
Algorithm: matches two tokens: the current input token (lookahead) and the
token required by the parser. The token required by the parser is represented by two
integers - the token code (pr_token_code), and the token attribute
(pr_token_attribute). The attribute code is used only when the token code is one of
the following codes: KW_T, LOG_OP_T, ART_OP_T, REL_OP_T. In all other cases
the token code is matched only.
If the match is successful and the lookahead is SEOF_T, the function returns.
If the match is successful and the lookahead is not SEOF_T, the function advances to
the next input token by executing the statement:
lookahead = malar_next_token (sc_buf);
If the new lookahead token is ERR_T, the function calls the error printing function
syn_printe(), advances to the next input token by calling malar_next_token () again,
increments the error counter synerrno, and returns.
If the match is unsuccessful, the function calls the error handler
syn_eh(pr_token_code) and returns.
*/
void match(int pr_token_code, int pr_token_attribute)
{
	if (lookahead.code == pr_token_code)/*Have expected token*/
	{
		switch (pr_token_code)
		{
		case SEOF_T: return;/*End of file, return*/

			/*Cases that need to match token attribute too*/
		case KW_T:
		case LOG_OP_T:
		case ART_OP_T:
		case REL_OP_T:
			if (lookahead.attribute.get_int == pr_token_attribute)
			{
				lookahead = malar_next_token(sc_buf);
			}

			else
			{
				syn_eh(pr_token_code);
			}
			break;

		default:
			lookahead = malar_next_token(sc_buf);
			break;
		}

		if (lookahead.code == ERR_T)
		{
			syn_printe();
			lookahead = malar_next_token(sc_buf);
			++synerrno;
			return;
		}
	}

	else
	{
		syn_eh(pr_token_code);
	}
}
/*
Purpose: keep getting token until a safe point is reached, or SEOF
Author:  Jamal Rahman
History/Versions: v1.0
Called Functions: exit(), malar_next_token()
Parameters: int sync_token_code
Return value: void
Algorithm: First, the function calls syn_printe() and increments the error counter. Then the
function implements a panic mode error recovery: the function advances the input token
(lookahead) until it finds a token code matching the one required by the parser
(pr_token_code passed to the function as sync_token_code ).
It is possible, when advancing, that the function can reach the end of the source file
without finding the matching token. To prevent from overrunning the input buffer, before
every move the function checks if the end of the file is reached. If the function looks for
sync_token_code different from SEOF_T and reaches the end of the source file, the
function calls exit(synerrno).
If a matching token is found and the matching token is not SEOF_T, the function
advances the input token one more time and returns. If a matching token is found and
the matching token is SEOF_T, the function returns.
*/
void syn_eh(int sync_token_code) 
{
	syn_printe();
	++synerrno;
	
	if (sync_token_code != SEOF_T)
	{
		while (lookahead.code != sync_token_code && lookahead.code != SEOF_T)
		{
			lookahead = malar_next_token(sc_buf);
		}

		if (lookahead.code == SEOF_T)
		{
			exit(synerrno);
		}

		else
		{
			lookahead = malar_next_token(sc_buf);
			return;
		}
	}

	else
	{
		while (lookahead.code != SEOF_T)
		{
			lookahead = malar_next_token(sc_buf);
		}
		return;
	}

	
}

/*Provided by: Sv. Ranev*/
void syn_printe(void) {
	Token t = lookahead;

	printf("PLATY: Syntax error:  Line:%3d\n", line);
	printf("*****  Token code:%3d Attribute: ", t.code);
	switch (t.code){
	case  ERR_T: /* ERR_T     0   Error token */
		printf("%s\n", t.attribute.err_lex);
		break;
	case  SEOF_T: /*SEOF_T    1   Source end-of-file token */
		printf("NA\n");
		break;
	case  AVID_T: /* AVID_T    2   Arithmetic Variable identifier token */
	case  SVID_T:/* SVID_T    3  String Variable identifier token */
		printf("%s\n", sym_table.pstvr[t.attribute.get_int].plex);
		break;
	case  FPL_T: /* FPL_T     4  Floating point literal token */
		printf("%5.1f\n", t.attribute.flt_value);
		break;
	case INL_T: /* INL_T      5   Integer literal token */
		printf("%d\n", t.attribute.get_int);
		break;
	case STR_T:/* STR_T     6   String literal token */
		printf("%s\n", b_setmark(str_LTBL, t.attribute.str_offset));
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
	} /*end switch */
}
/*
Purpose: print the message
Author:  Arjun Sivakumar
History/Versions: v1.0
Called Functions: printf()
Parameters: char * message
Return value: void
*/
void gen_incode(char * message) 
{	
	printf("%s\n", message);
}

/*Production functions*/

/*
	<program> -> PLATYPUS{<opt_statements>}
	FIRST(<program>) = {KW_T(PLATYPUS)} 
	Author: Sv. Ranev
*/
void program (void) {
	match(KW_T, PLATYPUS); match(LBR_T, NO_ATTR); opt_statements();
	match(RBR_T, NO_ATTR);
	gen_incode("PLATY: Program parsed");
}

/*	
	<opt_statements> -> <statements> | e 
	FIRST(<opt_statements>) = {AVID_T, SVID_T KW_T( not PLATYPUS, ELSE, THEN, REPEAT) and e}
	Author: Sv. Ranev
*/
void opt_statements(void) {
		switch (lookahead.code){
		case AVID_T:
		case SVID_T: statements(); break;
		case KW_T:
		
			if (lookahead.attribute.get_int != PLATYPUS
				&& lookahead.attribute.get_int != ELSE
				&& lookahead.attribute.get_int != THEN
				&& lookahead.attribute.get_int != REPEAT){
				statements();
				break;
			}
		default: /*empty string ñ optional statements*/;
			gen_incode("PLATY: Opt_statements parsed");
		}
	}

/*
	<statements> -> <statement><statements'>
	FIRST(<statements>) = {AVID_T, SVID_T, KW_T(IF, USING, INPUT, OUTPUT)}
	Author: Arjun Sivakumar
*/
void statements(void) {
	switch (lookahead.code)
	{
	case AVID_T:
	case SVID_T: statement(); statements_p(); break;
	case KW_T:
		if (lookahead.attribute.get_int == IF
			|| lookahead.attribute.get_int == USING
			|| lookahead.attribute.get_int == INPUT
			|| lookahead.attribute.get_int == OUTPUT) {
			statement();
			statements_p(); 
			break;
		}
	default: /*There can be no empty in statement production*/
		syn_printe();
		break;
	}
}


/*
    <statements’> -> <statement> <statements’> | ϵ
    FIRST{<statements’>} = {AVID, SVID, IF, USING, INPUT, OUTPUT, ϵ}
    Author: Arjun Sivakumar

 */

void statements_p(void) {
    switch(lookahead.code)
    {
        case AVID_T:
        case SVID_T: statement(); statements_p(); break;
        case KW_T:
            if (lookahead.attribute.get_int == IF
                || lookahead.attribute.get_int == USING
                || lookahead.attribute.get_int == INPUT
                || lookahead.attribute.get_int == OUTPUT) {
				statement();
                statements_p(); 
                break;
            }
    }
}
/*
    <statement> ->
    <assignment statement>
    |<selection statement>
    |<iteration statement>
    |<input statement>
    | <output statement>
 
    FIRST(<statement>) = {AVID, SVID, IF, USING, INPUT, OUTPUT}
    Author: Arjun Sivakumar

 */
void statement(void) {
    switch(lookahead.code) {
        case AVID_T:
        case SVID_T: assignment_statement(); break;
        case KW_T:
            if(lookahead.attribute.get_int == IF) {
                selection_statement();
                break;
            }
            if(lookahead.attribute.get_int == USING) {
                iteration_statement();
                break;
            }
            if(lookahead.attribute.get_int == INPUT) {
                input_statement();
                break;
            }
            if(lookahead.attribute.get_int == OUTPUT) {
                output_statement();
                break;
            }
            else {
                syn_printe();
                break;
            }
        default:
            syn_printe();
            break;
    } /*End switch*/

}
/*
	<assignment statement> ->
	<assignment expression>

	FIRST(<assignment statement>) = {AVID, SVID}
	Author: Arjun Sivakumar
*/
void assignment_statement(void) {
    switch(lookahead.code) {
        case AVID_T:
        case SVID_T:
            assignment_expression();
            match(EOS_T, NO_ATTR);
			gen_incode("PLATY: Assignment statement parsed");
            break;
    }
}

/*
	<assignment expression> ->
	AVID  = <arithmetic expression>
	|SVID = <string expression>

	FIRST(<assignment expression>) = {AVID, SVID}
	Author: Jamal Rahman and Arjun Sivakumar
*/
void assignment_expression(void) {
    switch(lookahead.code) {
        case AVID_T:
            match(AVID_T, NO_ATTR);
            match(ASS_OP_T, NO_ATTR);
			arithmetic_expression();
            gen_incode("PLATY: Assignment expression (arithmetic) parsed");
            break;
        case SVID_T:
        	match(SVID_T, NO_ATTR);
        	match(ASS_OP_T, NO_ATTR);
        	string_expression();
        	gen_incode("PLATY: Assignment expression (string) parsed");
        	break;
        default:
            syn_printe();
            break;    
    }
}
/*
	<selection statement> ->
	IF (<conditional expression>) THEN <opt_statements>
	ELSE {<opt_statements>};

	FIRST(<selection statement>) = {IF}
	Author: Arjun Sivakumar
*/
void selection_statement(void) {
		match(KW_T, IF);
		match(LPR_T, NO_ATTR);
		conditional_expression();
		match(RPR_T, NO_ATTR);
		match(KW_T, THEN);
		opt_statements();
		match(KW_T,ELSE);
		match(LBR_T, NO_ATTR);
		opt_statements();
		match(RBR_T, NO_ATTR);
		match(EOS_T, NO_ATTR);
		gen_incode("PLATY: IF statement parsed");
}
/*
	<iteration statement>
	USING (<assignment expression>, <conditional expression>, <assignment expression>)
	REPEAT {
	<opt_statements>
	};

	FIRST(<iteration statement>) = {USING}

	Author: Arjun Sivakumar
*/
void iteration_statement(void) {
	match(KW_T, USING);
	match(LPR_T, NO_ATTR);
	assignment_expression();
	match(COM_T, NO_ATTR);
	conditional_expression();
	match(COM_T, NO_ATTR);
	assignment_expression();
	match(RPR_T, NO_ATTR);
	match(KW_T, REPEAT);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: USING statement parsed");
}
/*
	<input statement>
	INPUT (<variable list>);

	FIRST(<input statement>) = {INPUT}
	Author: Jamal Rahman and Arjun Sivakumar
*/
void input_statement(void) {
   match(KW_T,INPUT);
   match(LPR_T,NO_ATTR);
   variable_list();
   match(RPR_T,NO_ATTR); 
   match(EOS_T,NO_ATTR);
   gen_incode("PLATY: INPUT statement parsed");
}
/*
	<variable list> -> <variable identifier> , <variable list’>
	FIRST(<variable list>) = {AVID_T, SVID_T}
*/
void variable_list(void) {
	variable_identifier();
	variable_list_p();
	gen_incode("PLATY: Variable list parsed");
}
/*
	<variable list’> -> , <variable identifier> <variable list’> | ε

	FIRST(<variable list’>) = {,  ϵ}
	Author: Arjun Sivakumar
*/
void variable_list_p(void) {
	switch (lookahead.code) {
	case COM_T:
		match(COM_T, NO_ATTR);
		variable_identifier();
		variable_list_p();
		break;

	default:
		break;
	}
		

}
/*
	<variable identifier> -> AVID_T | SVID_T

	FIRST(<variable identifier>) = {AVID_T, SVID_T}
	Author: Arjun Sivakumar
*/
void variable_identifier(void) {
	switch(lookahead.code) 
	{
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
/*
	<output statement> -> OUTPUT (<output list>);

	FIRST(<output statement>) = {OUTPUT}
	Author: Arjun Sivakumar
*/
void output_statement(void) {
	match(KW_T, OUTPUT);
	match(LPR_T, NO_ATTR);
	output_list();
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: OUTPUT statement parsed");
}
/*
	<output list> -> <variable list> | STR_T| ε

	FIRST(<output list>) = {AVID_T, SVID_T STR_T, ϵ}
	Author: Arjun Sivakumar
*/
void output_list(void) {

	switch(lookahead.code) {
		case AVID_T:
		case SVID_T: variable_list(); break;
		
		case STR_T:
		match(STR_T, NO_ATTR);
		gen_incode("PLATY: Output list (string literal) parsed");
		break;
		
		default:
			gen_incode("PLATY: Output list (empty) parsed");
			break;
	}
}
/*
	<arithmetic expression> ->
	<unary arithmetic expression>
	| <additive arithmetic expression>

	FIRST{<arithmetic expression>) = {-, +, AVID_T, FPL_T, INL_T, (}
	Author: Jamal Rahman and Arjun Sivakumar

*/
void arithmetic_expression(void) {
	switch(lookahead.code) 
	{
		case ART_OP_T:
			switch(lookahead.attribute.arr_op) {

				case MINUS:
				match(ART_OP_T, MINUS);
				unary_arithmetic_expression();
				gen_incode("PLATY: Arithmetic expression parsed");
				break;

				case PLUS:
				match(ART_OP_T, PLUS);
				unary_arithmetic_expression();
				gen_incode("PLATY: Arithmetic expression parsed");
				break;

				default: 
					syn_printe();
					break;
			} /*End arithmetic switch */
			break;		
		case AVID_T:
		case FPL_T:
		case INL_T:
		case LPR_T:
			additive_arithmetic_expression();
			gen_incode("PLATY: Arithmetic expression parsed");
			break;
		default:
			syn_printe();
			break;	 		
	}
}
/*
	<unary arithmetic expression> ->
	- <primary arithmetic expression>
	| + <primary arithmetic expression>

	FIRST(<unary arithmetic expression>) = {-, +, AVID_T, FPL_T, INL_T, (}

	Author: Jamal Rahman and Arjun Sivakumar
*/
void unary_arithmetic_expression(void) {
	switch (lookahead.code) {
	case ART_OP_T:
		switch (lookahead.attribute.arr_op)
		{
		case MINUS:
		case PLUS:
			primary_arithmetic_expression();
			gen_incode("PLATY: Unary arithmetic expression parsed");
			break;

		default:
			syn_printe();
			break;
		}
		break;

	case AVID_T:
	case FPL_T:
	case INL_T:
	case LPR_T:
		primary_arithmetic_expression();
		gen_incode("PLATY: Unary arithmetic expression parsed");
		break;

	default:
		syn_printe();
		break;
	}
		
}

/*
	<additive arithmetic expression> -> 
	<multiplicative arithmetic expression> <additive arithmetic expression’>

	FIRST(<additive arithmetic expression>) = { AVID_T, FPL_T, INL_T, ( }
	Author: Arjun Sivakumar
*/
void additive_arithmetic_expression(void) {
	multiplicative_arithmetic_expression();
	additive_arithmetic_expression_p();
}

/*
	<additive arithmetic expression’> ->
	+ <multiplicative arithmetic expression> <additive arithmetic expression’>
	| - <multiplicative arithmetic expression> <additive arithmetic expression’>
	| ε

	FIRST(<additive arithmetic expression’>) = {+, -, ϵ}
	Author: Jamal Rahman and Arjun Sivakumar

*/
void additive_arithmetic_expression_p(void) {
	switch(lookahead.code) {
		case ART_OP_T: 
		switch(lookahead.attribute.arr_op) {
			case PLUS:
			match(ART_OP_T, PLUS);
			multiplicative_arithmetic_expression();
			additive_arithmetic_expression_p();
			gen_incode("PLATY: Additive arithmetic expression parsed");
			break;
			case MINUS:
			match(ART_OP_T, MINUS);
			multiplicative_arithmetic_expression();
			additive_arithmetic_expression_p();
			gen_incode("PLATY: Additive arithmetic expression parsed");
			break;
		}
		break;
	}

}
/*
	<multiplicative arithmetic expression> ->
	<primary arithmetic expression> <multiplicative arithmetic expression’>

	FIRST(<multiplicative arithmetic expression> = { AVID_T, FPL_T, INL_T, (}
	Author: Arjun Sivakumar
*/
void multiplicative_arithmetic_expression(void) {
	primary_arithmetic_expression();
	multiplicative_arithmetic_expression_p();
}
/*
	<multiplicative arithmetic expression’> ->
	* <primary arithmetic expression> <multiplicative arithmetic expression’
	| /  <primary arithmetic expression> <multiplicative arithmetic expression’>
	| ε

	FIRST(<multiplicative arithmetic expression’> = {*, /, ϵ}
	Author: Arjun Sivakumar
*/
void multiplicative_arithmetic_expression_p(void) {
	switch(lookahead.code) {
		case ART_OP_T:
		switch(lookahead.attribute.arr_op) {
			case MULT:
			match(ART_OP_T, MULT);
			primary_arithmetic_expression();
			multiplicative_arithmetic_expression_p();
			gen_incode("PLATY: Multiplicative arithmetic expression parsed");
			break;
			case DIV:
			match(ART_OP_T, DIV);
			primary_arithmetic_expression();
			multiplicative_arithmetic_expression_p();
			gen_incode("PLATY: Multiplicative arithmetic expression parsed");
			break;
		}
		break;
	}

}


/*
	<primary arithmetic expression> ->
	AVID_T
	| FPL_T
	| INL_T
	| (<arithmetic expression>)

	FIRST(<primary arithmetic expression>) = {AVID_T, FPL_T, INL_T, (}
	Author: Jamal Rahman and Arjun Sivakumar

*/
void primary_arithmetic_expression(void) {
	switch(lookahead.code) {
		case AVID_T:
		match(AVID_T, NO_ATTR);
		gen_incode("PLATY: Primary arithmetic expression parsed");
		break;
		case FPL_T:
		match(FPL_T, NO_ATTR);
		gen_incode("PLATY: Primary arithmetic expression parsed");
		break;
		case INL_T:
		match(INL_T, NO_ATTR);
		gen_incode("PLATY: Primary arithmetic expression parsed");
		break;
		case LPR_T:
		match(LPR_T, NO_ATTR);
		arithmetic_expression();
		match(RPR_T, NO_ATTR);
		gen_incode("PLATY: Primary arithmetic expression parsed");
		break;
	}
}
/*
	<string expression> -> <primary string expression> <string expression’>

	FIRST(<string expression>) = {SVID_T, STR_T}
	Author: Arjun Sivakumar
*/
void string_expression(void) {
	primary_string_expression();
	string_expression_p();
	gen_incode("PLATY: String expression parsed");
}
/*
	<string expression’> -> << <primary string expression> <string expression’> | ε

	FIRST(<string expression’>) = { <<, ϵ}
	Author: Arjun Sivakumar
*/
void string_expression_p(void) {
	switch(lookahead.code) {
		case SCC_OP_T:
		match(SCC_OP_T, NO_ATTR);
		primary_string_expression();
		string_expression_p();
		break;
	}
}
/*
	<primary string expression> -> SVID_T | STR_T

	FIRST(<primary string expression>) = {SVID_T, STR_T}
	Author: Jamal Rahman and Arjun Sivakumar
*/
void primary_string_expression(void) {
	switch(lookahead.code) {
		case SVID_T:
		match(SVID_T, NO_ATTR);
		gen_incode("PLATY: Primary string expression parsed");
		break;

		case STR_T:
		match(STR_T, NO_ATTR);
		gen_incode("PLATY: Primary string expression parsed");
		break;

		default: 
		syn_printe();
		break;

	}
}
/*
	<conditional expression> -> <logical OR expression>

	FIRST(<conditional expression>} ={ AVID_T, FPL_T, INL_T, SVID_T, STR_T}
	Author: Arjun Sivakumar
*/
void conditional_expression(void) {
	logical_OR_expression();
	gen_incode("PLATY: Conditional expression parsed");
}
/*
	<logical OR expression> ->
	<logical AND expression> <logical OR expression’>

	FIRST(<logical .OR. expression> = {AVID_T, FPL_T, INL_T, SVID_T, STR_T}
	Author: Arjun Sivakumar
*/
void logical_OR_expression(void) {
	logical_AND_expression();
	logical_OR_expression_p();
}
/*
	<logical OR expression’> ->
	.OR. <logical AND expression> <logical OR expression’> | ε

	FIRST(<logical .OR. expression’> = {.OR., ϵ}
	Author: Jamal Rahman and Arjun Sivakumar
*/
void logical_OR_expression_p(void) {
	switch(lookahead.code) {
		case LOG_OP_T:
			switch(lookahead.attribute.log_op) {
				case OR:
				match(LOG_OP_T, OR);
				logical_AND_expression();
				logical_OR_expression_p();
				gen_incode("PLATY: Logical OR expression parsed");
			}
		break;
	}
}
/*
	<logical AND expression> -> <relational expression> <logical AND expression’>

	FIRST(<logical AND expression>) = { AVID_T, FPL_T, INL_T, SVID_T, STR_T}
	Author: Arjun Sivakumar
*/
void logical_AND_expression(void) {
	relational_expression();
	logical_AND_expression_p();

}
/*
	<logical AND expression’> -> .AND. <relational expression> <logical AND expression’> | ε

	FIRST(<logical AND expression’> = {.AND., ϵ}
	Author: Arjun Sivakumar
*/
void logical_AND_expression_p(void) {
	switch(lookahead.code) {
		case LOG_OP_T:
			switch(lookahead.attribute.log_op) {
				case AND:
				match(LOG_OP_T, AND);
				relational_expression();
				logical_AND_expression_p();
				gen_incode("PLATY: Logical AND expression parsed");
			}
		break;
		default:
		break;
	}
}

/*
		<relational expression> -> 
	<primary a_relational expression> <relational operator> <primary a_relational expression>
	| <primary s_relational expression> <relational operator> <primary s_relational expression>

	FIRST(<relational expression>) {AVID_T, FPL_T, INL_T, SVID_T, STR_T}
	Author: Arjun Sivakumar

*/
void relational_expression(void) {
	switch(lookahead.code) {
		case AVID_T:
		case FPL_T:
		case INL_T:
		primary_a_relational_expression();
		relational_operator();
		primary_a_relational_expression();
		gen_incode("PLATY: Relational expression parsed");
		break;

		case SVID_T:
		case STR_T:
		primary_s_relational_expression();
		relational_operator();
		primary_s_relational_expression();
		gen_incode("PLATY: Relational expression parsed");
		break;

		default:
		syn_printe();
		break;
	}
}
/*
	<relational operator> ->
	== | <> | < | >

	FIRST(<relational operators>) = {==, <>, <, >}
	Author: Arjun Sivakumar
*/
void relational_operator(void) {
	switch(lookahead.code) {
		case REL_OP_T:
		switch(lookahead.attribute.rel_op) {
			case EQ:
			match(REL_OP_T, EQ);
			break;

			case NE:
			match(REL_OP_T, NE);
			break;

			case GT:
			match(REL_OP_T, GT);
			break;

			case LT:
			match(REL_OP_T, LT);
			break;

			default:
			syn_printe();
			break;
		}
		break;

		default:
			syn_printe();
			break;
	}
}
/*
		<primary a_relational expression> -> 
			AVID_T
			| FPL_T 
			| INL_T

	FIRST(<primary a_relational expression>) = {AVID_T, FPL_T, INL_T}
	Author: Arjun Sivakumar
*/
void primary_a_relational_expression(void) {
	switch(lookahead.code) {
		case AVID_T:
		match(AVID_T, NO_ATTR);
		gen_incode("PLATY: Primary a_relational expression parsed");
		break;

		case FPL_T:
		match(FPL_T, NO_ATTR);
		gen_incode("PLATY: Primary a_relational expression parsed");
		break;

		case INL_T:
		match(INL_T, NO_ATTR);
		gen_incode("PLATY: Primary a_relational expression parsed");
		break;

		default:
		syn_printe();
		break;
	}
}
/*
	<primary s_relational expression> ->
	<primary string expression>

	FIRST(<primary s_relational expression>) = {SVID_T, STR_T}
	Author: Jamal Rahman and Arjun Sivakumar
*/
void primary_s_relational_expression(void) {
	switch(lookahead.code) {
		case SVID_T:
		case STR_T:
		primary_string_expression();
		gen_incode("PLATY: Primary s_relational expression parsed");
		break;

		default:
		syn_printe();
		break;
	}
}