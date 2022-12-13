//##################################################################

//
// Parser for simple C programs.  This component checks 
// the input program to see if it meets the syntax rules
// of simple C.  The parser returns a string denoting
// success or failure. 
//
// Returns: the string "success" if the input program is
// legal, otherwise the string "syntax_error: ..." is
// returned denoting an invalid simple C program.
//
// New author:
//  Zaid Awaidah
//  UIC, CS341, Fall 2022
//
// Original author:
//   Prof. Joe Hummel
//   U. of Illinois, Chicago
//   CS 341, Spring 2022
//

//##################################################################

namespace compiler

//##################################################################

module parser =
  //##########################

  // NOTE: all functions in the module must be indented.

  //##########################
  
  // matchToken
  let private matchToken expected_token (tokens: string List) =
    // if the next token matches the expected token,  
    // keep parsing by returning the rest of the tokens.
    // Otherwise throw an exception because there's a 
    // syntax error, effectively stopping compilation
    // at the first error.
    //
    let next_token = List.head tokens

    if (next_token.StartsWith(expected_token:string)) then
      List.tail tokens
    else
      failwith ("expecting " + expected_token + ", but found " + next_token)

  //##########################

  // stmts
  //
  // evalutes all statements
  let rec private stmts tokens = 
    if (List.head tokens = "}") then
      failwith ("expecting statement, but found " + (List.head tokens))
    else
      let T1 = stmt tokens
      let T2 = morestmts T1
      T2

  //##########################

  // stmt
  //
  // evalutes individual statements
  and private stmt (tokens: string List) = 
    let head = List.head tokens
    if (tokens.Head.StartsWith(";")) then
      let T1 = empty tokens
      T1
    elif (head.StartsWith("int")) then
      let T1 = var_decl tokens
      T1
    elif (head.StartsWith("cin")) then
      let T1 = input tokens
      T1
    elif (head.StartsWith("cout")) then
      let T1 = output tokens
      T1
    elif (head.Contains("identifier")) then
      let T1 = assignment tokens
      T1
    elif (head.StartsWith("if")) then
      let T1 = if_stmt tokens
      T1
    elif (head = "}") then
      failwith ("expecting statement, but found " + head)
    else
      tokens
  
  //##########################

  // more_stmts
  //
  // checks for more statements
  and private morestmts tokens = 
    let head = List.head tokens
    if (head.StartsWith(";") || head.StartsWith("int") || head.StartsWith("cin") ||
        head.StartsWith("cout") || head.StartsWith("if") || head.Contains("identifier")) then
      let T1 = stmt tokens
      let T2 = morestmts T1
      T2
    else
      tokens

  //##########################

  // empty
  //
  // evalutes empty statement
  and private empty tokens = 
    let T1 = matchToken ";" tokens
    T1
  
  //##########################

  // var_decl
  //
  // evalutes variable declaration
  and private var_decl tokens = 
    let T1 = matchToken "int" tokens
    let var = T1.Head.Substring(T1.Head.IndexOf(":"))
    let T2 = matchToken ("identifier" + var) T1
    let T3 = matchToken ";" T2
    T3
  
  //##########################

  // input
  //
  // evalutes input statements
  and private input tokens = 
    let T1 = matchToken "cin" tokens
    let T2 = matchToken ">>" T1
    let T3 = matchToken ("identifier") T2
    let T4 = matchToken ";" T3
    T4

  //##########################

  // output
  //
  // evalutes output statements
  and private output tokens = 
    let T1 = matchToken "cout" tokens
    let T2 = matchToken "<<" T1 
    let T3 = output_value T2
    let T4 = matchToken ";" T3
    T4
  
  //##########################

  // assignment
  //
  // evalutes assignment statements
  and private assignment tokens =
    let var = tokens.Head.Substring(tokens.Head.IndexOf(":"))
    let T1 = matchToken ("identifier" + var) tokens
    let T2 = matchToken "=" T1
    let T3 = expr T2
    let T4 = matchToken ";" T3
    T4
  
  //##########################

  // if_stmt
  //
  // evalutes if statements
  and private if_stmt tokens = 
    let T1 = matchToken "if" tokens
    let T2 = matchToken "(" T1
    let T3 = condition T2
    let T4 = matchToken ")" T3
    let T5 = then_part T4
    let T6 = else_part T5
    T6

  //##########################

  // output_value
  //
  // evalutes output values
  and private output_value tokens =
    let head = List.head tokens
    if (head <> "endl") then
      let T1 = expr_value tokens
      T1
    else
      let T1 = matchToken "endl" tokens
      T1
  
  //##########################

  // expr
  //
  // evalutes expressions
  and private expr tokens =
    let head = List.head tokens
    if (head.Contains("identifier") || head.Contains("str_literal") || head.Contains("int_literal")
        || head.Contains("false")|| head.Contains("true")) then

      let T1 = expr_value tokens
      if (T1.Head.StartsWith("+") || T1.Head.StartsWith("-") || T1.Head.StartsWith("*")
            || T1.Head.StartsWith("/") || T1.Head.StartsWith("^") || T1.Head.StartsWith("<")
            || T1.Head.StartsWith("<=") || T1.Head.StartsWith(">") || T1.Head.StartsWith(">=")
            || T1.Head.StartsWith("==") || T1.Head.StartsWith("!=")) then

        let T2 = expr_op T1
        let (head_2:string) = List.head T2

        if (head_2.Contains("identifier") || head_2.Contains("str_literal") || head_2.Contains("int_literal")
        || head_2.Contains("false")|| head_2.Contains("true")) then
          let T3 = expr_value T2
          T3
        else
          failwith ("expecting identifier or literal, but found " + head_2)
      else
        T1
    else
      failwith ("expecting identifier or literal, but found " + tokens.Head)

  //##########################

  // expr_op
  //
  // evalutes the expression operator
  and private expr_op tokens =
    let head = List.head tokens
    if (head.StartsWith("+")) then
      let T1 = matchToken "+" tokens
      T1
    elif (head.StartsWith("-")) then
      let T1 = matchToken "-" tokens
      T1
    elif (head.StartsWith("*")) then
      let T1 = matchToken "*" tokens
      T1
    elif (head.StartsWith("/")) then
      let T1 = matchToken "/" tokens
      T1
    elif (head.StartsWith("^")) then
      let T1 = matchToken "^" tokens
      T1
    elif (head.StartsWith("<")) then
      let T1 = matchToken "<" tokens
      T1
    elif (head.StartsWith("<=")) then
      let T1 = matchToken "<=" tokens
      T1
    elif (head.StartsWith(">")) then
      let T1 = matchToken "" tokens
      T1
    elif (head.StartsWith(">=")) then
      let T1 = matchToken "" tokens
      T1
    elif (head.StartsWith("==")) then
      let T1 = matchToken "==" tokens
      T1
    elif (head.StartsWith("!=")) then
      let T1 = matchToken "!=" tokens
      T1
    else 
      failwith ("expecting statement, but found " + head)

  //##########################

  // expr_value
  //
  // evalutes expression values
  // based of type
  and private expr_value tokens = 
    let head = List.head tokens
    if (head.StartsWith("identifier")) then
      let T1 = matchToken ("identifier") tokens
      T1
    elif (head.StartsWith("int_literal")) then
      let T1 = matchToken ("int_literal") tokens
      T1
    elif (head.StartsWith("str_literal")) then
      let T1 = matchToken "str_literal" tokens
      T1
    elif (head.StartsWith("true")) then
      let T1 = matchToken "true" tokens
      T1
    elif (head.StartsWith("false")) then
      let T1 = matchToken "false" tokens
      T1
    else
      failwith ("expecting identifier or literal, but found " + head)
  
  //##########################

  // condition
  //
  // evalutes condition of if statement
  and private condition tokens = 
    let T1 = expr tokens
    T1
  
  //##########################

  // then_part
  //
  // evalutes then part of if statement
  and private then_part tokens = 
    let T1 = stmt tokens
    T1
  
  //##########################

  // else_part
  //
  // evalutes else part of if statement
  and private else_part tokens =
    let next_token = List.head tokens
    if (next_token = "else") then
      let T2 = matchToken "else" tokens
      stmt T2
    else
      tokens
  
  //##########################

  // simpleC
  //
  // evalutes program start
  let private simpleC tokens = 
    let var_1 = "unknown:#"
    let var_2 = "identifier:include"
    let var_3 = "identifier:iostream"
    let var_4 = "identifier:using"
    let var_5 = "identifier:namespace"
    let var_6 = "identifier:std"

    let T1 = matchToken var_1 tokens
    let T2 = matchToken var_2 T1
    let T3 = matchToken "<" T2
    let T4 = matchToken var_3 T3
    let T5 = matchToken ">" T4
    let T6 = matchToken var_4 T5
    let T7 = matchToken var_5 T6
    let T8 = matchToken var_6 T7
    let T9 = matchToken ";" T8
    

    let T10 = matchToken "void" T9
    let T11 = matchToken "main" T10
    let T12 = matchToken "(" T11
    let T13 = matchToken ")" T12
    let T14 = matchToken "{" T13
    let T15 = stmts T14
    let T16 = matchToken "}" T15
    let T17 = matchToken "$" T16
    T17

  //##########################

  //
  // parse tokens
  //
  // Given a list of tokens, parses the list and determines
  // if the list represents a valid simple C program.  Returns
  // the string "success" if valid, otherwise returns a 
  // string of the form "syntax_error:...".
  //
  let parse tokens = 
    try
      let result = simpleC tokens
      "success"
    with 
      | ex -> "syntax_error: " + ex.Message
  
  //##########################

//##################################################################
