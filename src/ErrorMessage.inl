/// @name Parser Messages
/// @{
DEF(MissTopLvlLetExpr, "Missing top-level let expression.")
DEF(NotAnIntLiteral, "Cannot convert '{0}' to an integer value.")
DEF(MissingCloser, "Missing closing '{0}' token after {1}.")
DEF(EarlyCloser, "Expected {0} instead of closing '{1}' token.")
DEF(MissingId, "Missing {0} identifier in {1}.")
DEF(MissingIdAfter, "Missing {0} identifier after {1} in {2}.")
DEF(MissingSeperator, "Missing seperator '{0}' token.")
DEF(MissingKeywordBefore, "Missing keyword '{0}' before {1} in {2}.")
DEF(MissingKeywordAfter, "Missing keyword '{0}' after {1} in {2}.")
DEF(MissingKeyword, "Missing keyword '{0}' in {1}.")
DEF(MissingRecFieldId, "Expected field of record {0} to start with an identifier.")
DEF(MissingToken, "Missing token '{0}' in {1}.")
DEF(MissingTokenAfter, "Missing token '{0}' after {1} in {2}.")
DEF(MissingTokenBetween, "Missing token '{0}' between {1} and {2}.")
DEF(InvalidTokenAfter, "Invalid token '{0}' after {1}.")
DEF(InvalidTokenBecause, "Invalid token '{0}': {1}")
DEF(CantCreateVarFrom, "Can't create {0} variable from non lvalue expression {1}.")
DEF(ExpIdInsteadOf, "Expected identifier instead of '{0}'.")
DEF(SymbUnknown, "Symbol '{0}' was not declared.")
DEF(SymbAlreadyKnown, "Symbol '{0}' was already declared in this scope.")
DEF(SymbNoType, "Symbol '{0}' does not refer to an existing type.")
DEF(EmptyToken, "Lexer error.")
/// @}
/// @name Type Checker Messages
/// @{
DEF(TyChInitial, "Type checker is in intial state.")
DEF(TyMismatchBinOp, "Type mismatch in binary operator: left '{0}', right '{1}'")
DEF(NotTruthy, "Type instances of '{0}' cannot be converted into truth values.")
DEF(NotIntegral, "Type '{0}' cannot be resolved to an integral type.")
DEF(NotString, "Type '{0}' cannot be resolved to a string type.")
/// @}
