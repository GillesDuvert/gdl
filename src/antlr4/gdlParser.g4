parser grammar gdlParser;
options {
        tokenVocab = gdlLexer;
}
@parser::context {
extern bool amIRelaxed;
static bool IsRelaxed() {return amIRelaxed;}
static void setRelaxed(bool b){amIRelaxed=b;}
extern int inloop;
static bool inside(){return ( inloop > 0 );}
static std::string StrUpCase(const std::string& s)
{
  unsigned len=s.length();
  char const *sCStr=s.c_str();
  char* r = new char[len+1];
//  ArrayGuard<char> guard( r);
  r[len]=0;
  for(unsigned i=0;i<len;i++)
  r[i]=std::toupper(sCStr[i]);
  return std::string(r);
}
}
// Actual grammar start.
tokens {
    ALL,        // arrayindex (*, e.g. [1:*])
//    ASSIGN, //already defined
    ASSIGN_INPLACE,
    ASSIGN_REPLACE,
    ASSIGN_ARRAYEXPR_MFCALL,
    ARRAYDEF,
    ARRAYDEF_CONST,
    ARRAYDEF_GENERALIZED_INDGEN,
    ARRAYIX,
    ARRAYIX_ALL,
    ARRAYIX_ORANGE,
    ARRAYIX_RANGE,
    ARRAYIX_ORANGE_S, // with stride
    ARRAYIX_RANGE_S,
    ARRAYEXPR,
    ARRAYEXPR_FCALL,
    ARRAYEXPR_MFCALL,
    BLOCK,
//    BREAK, //already defined
    CSBLOCK,
//    CONTINUE, //already defined
    COMMONDECL,
    COMMONDEF,
    CONSTANT,
    DEREF,
    ELSEBLK,
    EXPR,
//    FOR, //already defined
    FOR_STEP, // for with step
//    FOREACH, //already defined
    FOREACH_INDEX, // foreach with index (hash) variable
    FOR_LOOP,
    FOR_STEP_LOOP, // for with step
    FOREACH_LOOP,
    FOREACH_INDEX_LOOP,
    FCALL,
    FCALL_LIB, // library function call
     FCALL_LIB_DIRECT, // direct call
     FCALL_LIB_N_ELEMENTS, // N_ELEMENTS
    FCALL_LIB_RETNEW, // library function call always return newly allocated data
    GDLNULL,
    IF_ELSE,
    KEYDECL,
    KEYDEF,
    KEYDEF_REF, // keyword passed by reference
    KEYDEF_REF_CHECK, // keyword maybe passed by reference
    KEYDEF_REF_EXPR,  // keyword with assign/inc/dec passed by reference
      LABEL,
    MPCALL,
    MPCALL_PARENT, // explicit call to parent 
    MFCALL,
    MFCALL_LIB,
    MFCALL_LIB_RETNEW,
    MFCALL_PARENT, // explicit call to parent
    MFCALL_PARENT_LIB,
    MFCALL_PARENT_LIB_RETNEW,
      NOP,     // no operation
    NSTRUC,     // named struct
    NSTRUC_REF, // named struct reference
    ON_IOERROR_NULL,
    PCALL,
    PCALL_LIB, // libraray procedure call
    PARADECL,
    PARAEXPR,  // parameter
    PARAEXPR_VN, // _VN Variable Number of parameters version
    DEC_REF_CHECK, // called from EvalRefCheck() (no temporary needed then)
    INC_REF_CHECK, // called from EvalRefCheck() (no temporary needed then)
    POSTDEC,  //post-decrement : i--
    POSTINC, // Post-increment : i++
    DECSTATEMENT, // as a statement
    INCSTATEMENT, // as a statement
    REF,        // expr pass by reference
    REF_VN,        // expr pass by reference
    REF_CHECK,  // expr maybe be passed by reference
    REF_CHECK_VN,  // expr maybe be passed by reference
    REF_EXPR,   // assign/dec/inc expr passed by reference
    REF_EXPR_VN,   // assign/dec/inc expr passed by reference
//    REPEAT, //already defined
    REPEAT_LOOP,
//    RETURN,  //already defined
      RETF,    // return from function (return argument)
      RETP,    // return from procedure (no return argument)
    STRUC,  // struct
    SYSVAR,
//    UPLUS,
    UMINUS,
    VAR,     // variable, referenced through index
    VARPTR  //,  // variable, referenced through pointer
//    WHILE // unspecified return (replaced by tree parser with RETF/RETP)
}

 // 'reverse' identifier
 // allows reserved words as identifiers
 // needed for keyword abbreviations
 // if you change some keywords here you probably need to change
 // the reserved word list above
 reservedWords
     : AND_OP 
     | BEGIN  
     | CASE 
     | COMMON 
     | COMPILE_OPT
//     | CONTINUE
     | DO 
     | ELSE 
     | END 
     | ENDCASE 
     | ENDELSE 
     | ENDFOR 
     | ENDFOREACH 
     | ENDIF 
     | ENDREP 
     | ENDSWITCH 
     | ENDWHILE 
     | EQ_OP 
     | FOR 
     | FOREACH 
     | FORWARD_FUNCTION
     | FUNCTION 
     | GE_OP 
     | GOTO 
     | GT_OP 
     | IF 
     | INHERITS 
     | LE_OP 
     | LT_OP 
     | MOD_OP 
     | NE_OP 
     | NOT_OP 
     | OF 
     | ON_IOERROR
     | OR_OP 
     | PRO 
     | REPEAT 
     | SWITCH 
     | THEN 
     | UNTIL 
     | WHILE 
     | XOR_OP 
     ;

// whereever one END_U is there might be more
// end_unit is syntactical necessary, but not for the AST
endUnit:   (END_OF_LINE)+
	 ;

translation_unit
   :    ( forwardFunction endUnit
        | commonBlock
        | procedureDefinition
        | functionDefinition
        | endUnit
        )* // optional - only main program is also ok
        (  statementList END)? // $MAIN$ program
//	(endUnit)*
//        EOF
{ bailOut:;} // bailout jump label
    ;
//    catch[...] { /* catching translation_unit errors here */ }

// interactive compilation is not allowed
warnInteractiveCompile
    : (FUNCTION | PRO)       IDENTIFIER        (METHOD IDENTIFIER)?      (COMMA keywordDeclaration)?    endUnit
    ;

// interactive usage: analyze one line = statement or statementList with '&'. Must catch any PRO or FUNCTION definition as this is not yet programmed.
interactive
    :   ( warnInteractiveCompile
        | interactiveStatement
	| endUnit (anyEndMark)? 
        )+
    ;
//    catch[...] { /* catching interactive errors here */ }


identifierList : listOfIdentifiers+=IDENTIFIER (COMMA listOfIdentifiers+=IDENTIFIER)*    ; //create std::vector<antlr4::Token *> listOfIdentifiers to hold the tokens.

forwardFunction : FORWARD_FUNCTION identifierList;

compileOpt : COMPILE_OPT identifierList {  setRelaxed(false);};

commonBlock : COMMON IDENTIFIER  (COMMA identifierList)?  //should use a semantic predicate to say "% Common block ZZZZ must contain variables." if ZZZ is not defined
    ;



statement:                
      conditionalStatement
    | loopStatement       
    | jumpStatement       
    | forwardFunction     
    | commonBlock         
    | compileOpt          
    | procedureCall       
    | assignmentStatement 
    | specialHandlingOfOutOfLoopsJumps 
    ;

conditionalStatement
    : ifStatement
    | caseStatement
    | switchStatement
    ;
loopStatement
    : forStatement
    | foreachStatement
    | repeatStatement
    | whileStatement
    ;
 
anyEndMark
    : END
    | ENDIF
    | ENDELSE
    | ENDCASE
    | ENDSWITCH
    | ENDFOR
    | ENDFOREACH
    | ENDWHILE
    | ENDREP
    ;

// idl allows more than one ELSE: first is executed, *all*
// (including expression) later branches are ignored (case) or 
// executed (switch)

switchSelector: expression;   
switchStatement: SWITCH switchSelector OF (endUnit)? (switchBody )*  ( ENDSWITCH | END); 

switchClause: expression;
switchBody
    : switchClause COLON 
        ( labelledStatement
	| statement
	| BEGIN statementList  ( ENDSWITCH | END) 
	)? endUnit
    | ELSE COLON 
        ( labelledStatement
	| statement
	| BEGIN statementList ( ENDSWITCH | ENDELSE | END)
	)? endUnit
    ;    

caseSelector: expression ;
caseStatement    : CASE caseSelector OF (endUnit)?  (caseBody)*  (ENDCASE | END)    ;

caseClause: expression;
caseBody
    : caseClause COLON 
        ( labelledStatement
	| statement
	| BEGIN statementList (ENDCASE | END))? endUnit
    | ELSE COLON 
        ( labelledStatement
	| statement
	| BEGIN statementList (ENDCASE | ENDELSE | END))? endUnit
	;

// compound statements don't care about the specific anyEndMark
compoundStatement
    : BEGIN statementList anyEndMark
    | statement
    ;
    
label:  IDENTIFIER COLON;

labelledStatement: (label)+  (compoundStatement)?;

labellableStatement: (label)? statement;

statementList
    :  ( labelledStatement
       | compoundStatement
       | endUnit
       )+
    ;



baseclass_method
@init  { /*  here we translate IDL_OBJECT to GDL_OBJECT for source code compatibility */}
    : s=IDENTIFIER METHOD
    ;

compoundAssignment:
      AND_OP_EQ 
    | ASTERIX_EQ 
    | EQ_OP_EQ 
    | GE_OP_EQ
    | GTMARK_EQ
    | GT_OP_EQ
    | LE_OP_EQ
    | LTMARK_EQ
    | LT_OP_EQ
    | MATRIX_OP1_EQ
    | MATRIX_OP2_EQ
    | MINUS_EQ
    | MOD_OP_EQ
    | NE_OP_EQ
    | OR_OP_EQ
    | XOR_OP_EQ
    | PLUS_EQ
    | POW_EQ
    | SLASH_EQ
    ;

assignmentOperator:
     ASSIGN
    | compoundAssignment
    ;
    
repeatExpression: expression;
repeatStatement: REPEAT {inloop++;} repeatBlock UNTIL repeatExpression  {inloop--;} ;


repeatBlock
    : BEGIN statementList (ENDREP | END)
    | statement
    ;

whileExpression: expression;
whileStatement
    : WHILE  {inloop++;} whileExpression DO whileBlock  {inloop--;};


whileBlock
    : BEGIN statementList (ENDWHILE | END) 
    | statement
    ;

forVariable: variableName;
forInit: expression;
forLimit: expression;
forStep: expression;
forStatement : FOR forVariable ASSIGN forInit COMMA forLimit (COMMA forStep)? DO forBlock;

forBlock
@init{ inloop ++;}
@after{ inloop--;}
    : BEGIN statementList ( ENDFOR | END )
    | statement
    ;    

foreachElement: IDENTIFIER;
foreachVariable: expression;
foreachIndex: IDENTIFIER;
foreachStatement: FOREACH  {inloop++;} foreachElement COMMA foreachVariable (COMMA foreachIndex)? DO foreachBlock {inloop--;} ;

foreachBlock
    : BEGIN statementList (END | ENDFOREACH )
    | statement
    ;    

jumpStatement
   :
   (gotoStatement
   | onIoErrorStatement
   | RETURN (COMMA expression)?
//   | BREAK // only valid in loops and switchs
//   | CONTINUE // only valid in loops
   )
   ;

gotoStatement:  GOTO COMMA IDENTIFIER;
onIoErrorStatement: ON_IOERROR COMMA IDENTIFIER;

specialHandlingOfOutOfLoopsJumps:
      (
//      CONTINUE 
//      |
      BREAK
      ) endUnit
     ;

ifExpression: expression;
ifStatement: IF ifExpression THEN  ifBlock ( ELSE elseBlock )? ;


ifBlock
    : BEGIN statementList (ENDIF | END) 
    | statement
    ;


elseBlock
    : BEGIN statementList ( ENDELSE | END)
    | statement
    ;

valuedParameter    :<assoc=right> (IDENTIFIER | reservedWords) ASSIGN expression;
setParameter: SLASH (IDENTIFIER | reservedWords);
simpleParameter: (IDENTIFIER | reservedWords | expression);

callParameter:
      valuedParameter
    | setParameter
    | simpleParameter
    ;


parameterList : callParameter ( COMMA callParameter)*  ;

continueCall: CONTINUE; 

formalProcedureCall :
{ inside() }? continueCall
| IDENTIFIER (COMMA parameterList)? 
;

memberProcedureCall: variableAccessByValueOrReference (MEMBER|DOT) (IDENTIFIER METHOD)? formalProcedureCall;

procedureCall:
    memberProcedureCall
  | formalProcedureCall
  ;

formalFunctionCall : IDENTIFIER LBRACE (parameterList)? RBRACE  ;
memberFunctionCall : variableAccessByValueOrReference (MEMBER|DOT) (IDENTIFIER METHOD)? formalFunctionCall;
functionCall:
       memberFunctionCall
     | formalFunctionCall
     ;

valuedKeyword:<assoc=right> (IDENTIFIER | reservedWords) ASSIGN  (IDENTIFIER | reservedWords);
simpleKeyword: (IDENTIFIER | reservedWords);
keywordDeclaration:
         valuedKeyword
	|simpleKeyword
        ;
	
keywordDeclarationList : keywordDeclaration ( COMMA keywordDeclaration )*    ;
    

procedureDefinition :
        PRO ( objectName | IDENTIFIER ) (COMMA keywordDeclarationList)? endUnit {setRelaxed(true);} (statementList)*  END
  ;
 

functionDefinition:
        FUNCTION ( objectName | IDENTIFIER ) (COMMA keywordDeclarationList)? endUnit {setRelaxed(true);} (statementList)* END
    ;

objectName : IDENTIFIER METHOD IDENTIFIER ;    


expressionList: expression  ( COMMA expression )* ;
arrayListDefinition:          LSQUARE expressionList RSQUARE    ;
arrayAutoDefinition:          LSQUARE expression COLON expression RSQUARE    ;
arrayAutoIncrementDefinition: LSQUARE expression COLON expression COLON expression RSQUARE    ;

arrayDefinition:
 | arrayListDefinition        
 | arrayAutoDefinition        
 | arrayAutoIncrementDefinition
;


namedStructure:
	  LCURLY IDENTIFIER ( (COMMA inheritsOrTagDef)*
	                    | COMMA expressionList 
	                    | ) RCURLY
	;
anonymousStructure: LCURLY inheritsOrTagDef (COMMA inheritsOrTagDef)* RCURLY;

inheritsOrTagDef
    : inheritsStructure
    | normalTag
    ;

inheritsStructure: INHERITS IDENTIFIER;
normalTag:  IDENTIFIER COLON expression ;

structureDefinition
    : namedStructure
    | anonymousStructure
    ;


constant_hex_byte    	:  CONSTANT_HEX_BYTE    ;
constant_hex_long 	:  CONSTANT_HEX_LONG ;
constant_hex_long64 	:  CONSTANT_HEX_LONG64 ;
constant_hex_int 	:  CONSTANT_HEX_INT ;
constant_hex_i 		:  CONSTANT_HEX_I ;  // DEFINT32
constant_hex_ulong 	:  CONSTANT_HEX_ULONG ;
constant_hex_ulong64	:  CONSTANT_HEX_ULONG64;
constant_hex_ui		:  CONSTANT_HEX_UI;        // DEFINT32
constant_hex_uint	:  CONSTANT_HEX_UINT;
constant_byte  		:  CONSTANT_BYTE  ;
constant_long 		:  CONSTANT_LONG ;
constant_long64 	:  CONSTANT_LONG64 ;
constant_int		:  CONSTANT_INT;
constant_i		:  CONSTANT_I;        // DEFINT32
constant_ulong 		:  CONSTANT_ULONG ;
constant_ulong64 	:  CONSTANT_ULONG64 ;
constant_uint		:  CONSTANT_UINT;
constant_oct_byte  	:  CONSTANT_OCT_BYTE  ;
constant_oct_long 	:  CONSTANT_OCT_LONG ;
constant_oct_long64 	:  CONSTANT_OCT_LONG64 ;
constant_oct_int	:  CONSTANT_OCT_INT;
constant_oct_i		:  CONSTANT_OCT_I;        // DEFINT32
constant_oct_ulong 	:  CONSTANT_OCT_ULONG ;
constant_oct_ulong64 	:  CONSTANT_OCT_ULONG64 ;
constant_oct_ui		:  CONSTANT_OCT_UI;
constant_oct_uint	:  CONSTANT_OCT_UINT;
constant_float     	:  CONSTANT_FLOAT     ;
constant_double		:  CONSTANT_DOUBLE;
constant_bin_byte  	:  CONSTANT_BIN_BYTE  ;
constant_bin_long 	:  CONSTANT_BIN_LONG ;
constant_bin_long64 	:  CONSTANT_BIN_LONG64 ;
constant_bin_int	:  CONSTANT_BIN_INT;
constant_bin_i		:  CONSTANT_BIN_I;        // DEFINT32
constant_bin_ulong 	:  CONSTANT_BIN_ULONG ;
constant_bin_ulong64 	:  CONSTANT_BIN_ULONG64 ;
constant_bin_ui		:  CONSTANT_BIN_UI;        // DEFINT32
constant_bin_uint	:  CONSTANT_BIN_UINT;

numeric_constant:
 (
   constant_hex_byte    
 | constant_hex_long    
 | constant_hex_long64  
 | constant_hex_int     
 | constant_hex_i 	     
 | constant_hex_ulong   
 | constant_hex_ulong64 
 | constant_hex_ui	     
 | constant_hex_uint    
 | constant_byte  	     
 | constant_long 	     
 | constant_long64      
 | constant_int	     
 | constant_i	     
 | constant_ulong 	     
 | constant_ulong64     
 | constant_uint	     
 | constant_oct_byte    
 | constant_oct_long    
 | constant_oct_long64  
 | constant_oct_int     
 | constant_oct_i	     
 | constant_oct_ulong   
 | constant_oct_ulong64 
 | constant_oct_ui	     
 | constant_oct_uint    
 | constant_float       
 | constant_double	     
 | constant_bin_byte    
 | constant_bin_long    
 | constant_bin_long64  
 | constant_bin_int     
 | constant_bin_i	     
 | constant_bin_ulong   
 | constant_bin_ulong64 
 | constant_bin_ui	     
 | constant_bin_uint
 )
 ;

// used in
listOfArrayIndexes:
    LSQUARE arrayIndex (COMMA arrayIndex)* RSQUARE; // C++  LSQUARE arrayIndex ({++rank <= MAXRANK}? COMMA arrayIndex)* RSQUARE

relaxedListOfArrayIndexes: LBRACE arrayIndex ( COMMA arrayIndex)* RBRACE; 

//    | LBRACE arrayIndex ( COMMA arrayIndex)* RBRACE
    

allElements : ASTERIX ;
index : expression;
range:   expression COLON (allElements | expression );
stepRange: expression COLON (allElements | expression ) COLON expression;
arrayIndex: ( allElements  | stepRange | range | index) ; 

// the expressions *************************************

// system variable name
systemVariableName  : SYSVARNAME  ;

variableName : IDENTIFIER; //not SYSVARNAME

// this is SYNTATICALLY ok as an lvalue, but if one try to assign
// something to an non-var an error is raised
// bracedExpression :  LBRACE expression RBRACE ;

operatedVariable : LBRACE variableAccessByValueOrReference assignmentOperator expression RBRACE ;
// only used in variableAccessByValueOrReference
// sysvar or expression (first in struct access - therefore the name)
// a variable MUST be already defined here
varDesignator
    :  systemVariableName
    |  operatedVariable
    |  variableName
    |  LBRACE expression RBRACE //bracedExpression
    ;

varSubset:   varDesignator ( listOfArrayIndexes  |  { IsRelaxed()}? relaxedListOfArrayIndexes );
existingVariable
	: varSubset
	| varDesignator
	;

implicitArray:  listOfArrayIndexes;

tagNumberIndicator: LBRACE expression RBRACE;

tagIdentifier
    : tagNumberIndicator
    | SYSVARNAME  
    | IDENTIFIER
    | EXCLAMATION // IDL allows '!' as tag despite the documentation.
    ;

taggedEntryTarget
    : taggedEntry
    | varSubset
    | varDesignator
    ;

variableAccessByValueOrReference
    : pointedVariable
    | taggedEntry
    | varSubset
    | varDesignator
    ;
    
taggedEntry    :
      varSubset  DOT taggedEntryTarget //reflect struct-in-struct hierarchy.
    | varDesignator DOT taggedEntryTarget
    ;
pointedVariable : ASTERIX variableAccessByValueOrReference ;

string: STRING ;

// only here a function call is ok also (all other places must be an array)
nullVarMarker
    : LSQUARE RSQUARE
    | LCURLY RCURLY 
    ;
    

primaryLevelExpression:
      functionCall
    | implicitArray //always between '[]'
    | structureDefinition
    | variableAccessByValueOrReference //variable is Named
    | numeric_constant 	   
    | string
    | nullVarMarker
    ;
    
decincExpression:  primaryLevelExpression ( INC | DEC )?
        | INC primaryLevelExpression
	| DEC primaryLevelExpression
        ;
	
thirdLevelExpression:<assoc=right>
     decincExpression
     (
       ( //pointer dereference
         POW
       ) decincExpression
     )*
     ;
     
multiplicativeExpression: // '*' | '#' | '##' | '/' | 'mod' // level 4
      thirdLevelExpression
      (
        ( ASTERIX
	| MATRIX_OP1
	| MATRIX_OP2
	| SLASH
	| MOD_OP
	) thirdLevelExpression
      )*
      ;

signedMultiplicativeExpression:
        PLUS multiplicativeExpression
      | MINUS multiplicativeExpression
      | multiplicativeExpression
      ;
      
additiveExpression: // '+' | '-' | '<' | '>' | 'not' | '~' // level 5
       ( signedMultiplicativeExpression | negativeExpression )
         (
	    ( PLUS
            | MINUS
            | LTMARK
            | GTMARK
	    ) (  multiplicativeExpression | negativeExpression )
	 )*
	 ;
	    
negativeExpression:
      NOT_OP multiplicativeExpression
    | LOG_NEG multiplicativeExpression // true precedence of ~ operator
    ;

relationalExpression: // 'eq' | 'ne' | 'le' | 'lt' | 'ge' | 'gt' // level 6
        additiveExpression
        (
            ( EQ_OP
            | NE_OP
            | LE_OP
            | LT_OP
            | GE_OP
            | GT_OP
            ) additiveExpression
        )*
    ;

bitwiseExpression: // 'and' | 'or' | 'xor' // level 7
        relationalExpression
        ( 
            ( AND_OP 
            | OR_OP 
            | XOR_OP 
            ) relationalExpression
        )*
    ;
    
logicalExpression : // '&&' | '||'  //level 8
        left = bitwiseExpression
        ( 
           local_operator= ( LOG_AND 
            | LOG_OR 
            ) operand = bitwiseExpression
        )*
    ;

trinaryLogicalExpression:<assoc=right> logicalExpression QUESTION expression COLON expression ;  //' ?:' // level 9

expression :
     LBRACE expression RBRACE                         #groupingExpression
   | trinaryLogicalExpression                         #trinaryOperationExpression
   | logicalExpression                                #binaryOperationExpression
   ;

//expression
//    : LBRACE expression RBRACE                                                              #groupingExpression
//    | local_operator=compoundAssignment operand=expression                                           #unaryOperationExpression
//    | left=expression local_operator=(ASTERIX|SLASH) right=expression                                #binaryOperationExpression
//    | left=expression local_operator=(PLUS|MINUS) right=expression                                   #binaryOperationExpression
//    | left=expression local_operator=(LT_OP|GT_OP|GE_OP|LE_OP) right=expression                      #binaryOperationExpression
//    | left=expression local_operator=(EQ_OP|NE_OP) right=expression                                  #binaryOperationExpression
//    | variableName                                                                             #namedVariable
//    | target=NAME LPAREN (arguments+=expression (COMMA arguments+=expression)*)? RPAREN     #invocationExpression
//    | target=NAME                                                                           #referenceExpression
//    | value=NUMBER                                                                          #literalExpression                        
//    ;

assignmentStatement
    : variableAccessByValueOrReference assignmentOperator expression
    | decincExpression
    ;

autoPrintStatement: expressionList;
interactiveStatement
    : (IDENTIFIER COLON)*
        ( assignmentStatement
	| compoundStatement
        | ifStatement
	| switchStatement
	| caseStatement
	| forStatement
	| foreachStatement
	| repeatStatement
	| whileStatement
	| autoPrintStatement
        ) endUnit
    ;

