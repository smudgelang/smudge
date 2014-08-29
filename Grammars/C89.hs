module Grammars.C89 (
    fromList,

    Choose(..),
    These(..),
    Pair(..),
    Trio(..),
    Quad(..),
    SimpleList(..),
    CommaList(..),

    Identifier,
    mangleIdentifier,

    Constant,
    EnumerationConstant(..),

    StringLiteral,

    LEFTSQUARE(..),
    RIGHTSQUARE(..),
    LEFTPAREN(..),
    RIGHTPAREN(..),
    LEFTCURLY(..),
    RIGHTCURLY(..),
    COMMA(..),
    ELLIPSIS(..),
    COLON(..),
    ELSE(..),
    EQUAL(..),
    SEMICOLON(..),
    BITWISE_AND(..),
    BITWISE_OR(..),
    BITWISE_XOR(..),
    LOGICAL_AND(..),
    LOGICAL_OR(..),
    QUESTION(..),
    DOWHILE(..),

    PrimaryExpression(..),
    PostfixExpression(..),
    MemberOp(..),
    ArgumentExpressionList,
    UnaryExpression(..),
    UnaryCrement(..),
    UnaryOperator(..),
    CastExpression(..),
    MultiplicativeExpression(..),
    MultiplicativeOp(..),
    AdditiveExpression(..),
    AdditiveOp(..),
    ShiftExpression(..),
    ShiftOp(..),
    RelationalExpression(..),
    ComparisonOp(..),
    EqualityExpression(..),
    EqualityOp(..),
    ANDExpression(..),
    ExclusiveORExpression(..),
    InclusiveORExpression(..),
    LogicalANDExpression(..),
    LogicalORExpression(..),
    ConditionalExpression(..),
    AssignmentExpression(..),
    AssignmentOperator(..),
    Expression(..),
    ConstantExpression(..),

    Declaration(..),
    DeclarationSpecifiers,
    InitDeclaratorList,
    InitDeclarator(..),
    StorageClassSpecifier(..),
    TypeSpecifier(..),
    StructDeclarationList,
    StructDeclaration(..),
    SpecifierQualifierList,
    StructDeclaratorList,
    StructDeclarator(..),
    EnumeratorList,
    Enumerator(..),
    TypeQualifier(..),
    Declarator(..),
    DirectDeclarator(..),
    Pointer(..),
    TypeQualifierList,
    ParameterTypeList(..),
    ParameterList,
    ParameterDeclaration(..),
    IdentifierList,
    TypeName(..),
    AbstractDeclarator(..),
    DirectAbstractDeclarator(..),
    TypedefName(..),
    Initializer(..),
    InitializerList,

    Statement(..),
    LabeledStatement(..),
    CompoundStatement(..),
    DeclarationList,
    StatementList,
    ExpressionStatement(..),
    SelectionStatement(..),
    IterationStatement(..),
    JumpStatement(..),

    TranslationUnit,
    ExternalDeclaration(..),
    FunctionDefinition(..),
) where

import Data.Char (isDigit, isAsciiLower, isAsciiUpper, ord)

-------------------
-- Helpful Class --
-------------------

class Listish l where
    (<:) :: x -> l x -> l x
    fromList :: [x] -> l x
    singleton :: x -> l x

    infixr 5 <:

    fromList []     = error "Listish values cannot be empty."
    fromList [x]    = singleton x
    fromList (x:xs) = x <: fromList xs

-------------------
-- Helpful Types --
-------------------

data Choose a b c = A a | B b | C c
data These a b = This a | That b | These a b
data Pair a b = Pair a b
data Trio a b c = Trio a b c
data Quad a b c d = Quad a b c d

data SimpleList x = SimpleList x (Maybe (SimpleList x))
instance Listish SimpleList where
    x <: sl = SimpleList x (Just sl)
    singleton = flip SimpleList Nothing

data CommaList x = CommaList x (Maybe (Pair COMMA (CommaList x)))
instance Listish CommaList where
    x <: sl = CommaList x (Just (Pair COMMA sl))
    singleton = flip CommaList Nothing

-------------------
-- The C Grammar --
-------------------

-- A.1.1.1 Tokens

-- A.1.1.3 Identifiers

type Identifier = String

isAsciiAlphaNum :: Char -> Bool
isAsciiAlphaNum c = isDigit c || isAsciiLower c || isAsciiUpper c

toAsciiCode :: Char -> String
toAsciiCode = show . ord

mangleChar :: Char -> String
mangleChar c | isAsciiAlphaNum c = [c]
mangleChar c                     = '_' : (toAsciiCode c) ++ "_"

mangleIdentifier :: String -> Identifier
mangleIdentifier []     = "__"
mangleIdentifier [c]    = mangleChar c
mangleIdentifier (c:cs) = (mangleChar c) ++ (mangleIdentifier cs)

-- A.1.1.4 Constants

type Constant = String

type EnumerationConstant = Identifier

-- A.1.1.5 String literals

type StringLiteral = String

-- A.1.2 Phrase structure grammar

-- Some necessary terminals; missing terminals are constructors below.

data LEFTSQUARE = LEFTSQUARE
data RIGHTSQUARE = RIGHTSQUARE
data LEFTPAREN = LEFTPAREN
data RIGHTPAREN = RIGHTPAREN
data LEFTCURLY = LEFTCURLY
data RIGHTCURLY = RIGHTCURLY
data COMMA = COMMA
data ELLIPSIS = ELLIPSIS
data COLON = COLON
data ELSE = ELSE
data EQUAL = EQUAL
data SEMICOLON = SEMICOLON
data BITWISE_AND = BITWISE_AND
data BITWISE_OR = BITWISE_OR
data BITWISE_XOR = BITWISE_XOR
data LOGICAL_AND = LOGICAL_AND
data LOGICAL_OR = LOGICAL_OR
data QUESTION = QUESTION
data DOWHILE = DOWHILE

-- A.1.2.1 Expressions
data PrimaryExpression = IPrimaryExpression Identifier
                       | CPrimaryExpression Constant
                       | SPrimaryExpression StringLiteral
                       | EPrimaryExpression LEFTPAREN Expression RIGHTPAREN

data PostfixExpression = PPostfixExpression PrimaryExpression
                       | EPostfixExpression PostfixExpression LEFTSQUARE Expression RIGHTSQUARE
                       | APostfixExpression PostfixExpression LEFTPAREN (Maybe ArgumentExpressionList) RIGHTPAREN
                       | MPostfixExpression PostfixExpression MemberOp Identifier
                       | UPostfixExpression PostfixExpression UnaryCrement

data MemberOp = DOT | ARROW

type ArgumentExpressionList = CommaList AssignmentExpression

data UnaryExpression = PUnaryExpression PostfixExpression
                     | UUnaryExpression UnaryCrement UnaryExpression
                     | CUnaryExpression UnaryOperator CastExpression
                     | SIZEOF (Either UnaryExpression (Trio LEFTPAREN TypeName RIGHTPAREN))

data UnaryCrement =  INCREMENT | DECREMENT

data UnaryOperator = ADDRESS_OF | VALUE_OF | POS | NEG | INVERT | LOGICAL_NOT

data CastExpression = UCastExpression UnaryExpression
                    | TCastExpression LEFTPAREN TypeName RIGHTPAREN CastExpression

data MultiplicativeExpression = MultiplicativeExpression (Maybe (Pair MultiplicativeExpression MultiplicativeOp)) CastExpression

data MultiplicativeOp = TIMES | DIV | MOD

data AdditiveExpression = AdditiveExpression (Maybe (Pair AdditiveExpression AdditiveOp)) MultiplicativeExpression

data AdditiveOp = PLUS | MINUS

data ShiftExpression = ShiftExpression (Maybe (Pair ShiftExpression ShiftOp)) AdditiveExpression

data ShiftOp = LSHIFT | RSHIFT

data RelationalExpression = RelationalExpression (Maybe (Pair RelationalExpression ComparisonOp)) ShiftExpression

data ComparisonOp = LESS_THAN | GREATER_THAN | LESS_EQUAL | GREATER_EQUAL

data EqualityExpression = EqualityExpression (Maybe (Pair EqualityExpression EqualityOp)) RelationalExpression

data EqualityOp = EQUALEQUAL | NOTEQUAL

data ANDExpression = ANDExpression (Maybe (Pair ANDExpression BITWISE_AND)) EqualityExpression

data ExclusiveORExpression = ExclusiveORExpression (Maybe (Pair ExclusiveORExpression BITWISE_XOR)) ANDExpression

data InclusiveORExpression = InclusiveORExpression (Maybe (Pair InclusiveORExpression BITWISE_OR)) ExclusiveORExpression

data LogicalANDExpression = LogicalANDExpression (Maybe (Pair LogicalANDExpression LOGICAL_AND)) InclusiveORExpression

data LogicalORExpression = LogicalORExpression (Maybe (Pair LogicalORExpression LOGICAL_OR)) LogicalANDExpression

data ConditionalExpression = ConditionalExpression LogicalORExpression (Maybe (Quad QUESTION Expression COLON ConditionalExpression))

data AssignmentExpression = AssignmentExpression (Either ConditionalExpression (Trio UnaryExpression AssignmentOperator AssignmentExpression))

data AssignmentOperator = ASSIGN | TIMES_EQUAL | DIV_EQUAL | MOD_EQUAL | PLUS_EQUAL | MINUS_EQUAL
                        | LSHIFT_EQUAL | RSHIFT_EQUAL | AND_EQUAL | XOR_EQUAL | OR_EQUAL

type Expression = CommaList AssignmentExpression

type ConstantExpression = ConditionalExpression


-- A.1.2.2 Declarations

data Declaration = Declaration DeclarationSpecifiers (Maybe InitDeclaratorList) SEMICOLON

type DeclarationSpecifiers = SimpleList (Choose StorageClassSpecifier TypeSpecifier TypeQualifier)

type InitDeclaratorList = CommaList InitDeclarator 

data InitDeclarator = InitDeclarator Declarator (Maybe (Pair EQUAL Initializer))

data StorageClassSpecifier = TYPEDEF | EXTERN | STATIC | AUTO | REGISTER

data TypeSpecifier = VOID | CHAR | SHORT | INT | LONG | FLOAT | DOUBLE | SIGNED | UNSIGNED
                   | STRUCT (Either Identifier (Quad (Maybe Identifier) LEFTCURLY StructDeclarationList RIGHTCURLY))
                   | UNION (Either Identifier (Quad (Maybe Identifier) LEFTCURLY StructDeclarationList RIGHTCURLY))
                   | ENUM (Either Identifier (Quad (Maybe Identifier) LEFTCURLY EnumeratorList RIGHTCURLY))
                   | TypeSpecifier TypedefName

type StructDeclarationList = SimpleList StructDeclaration

data StructDeclaration = StructDeclaration SpecifierQualifierList StructDeclaratorList SEMICOLON

type SpecifierQualifierList = SimpleList (Either TypeSpecifier TypeQualifier)

type StructDeclaratorList = CommaList StructDeclarator

data StructDeclarator = StructDeclarator (These Declarator (Pair COLON ConstantExpression))

type EnumeratorList = CommaList Enumerator

data Enumerator = Enumerator EnumerationConstant (Maybe (Pair EQUAL ConstantExpression))

data TypeQualifier = CONST | VOLATILE

data Declarator = Declarator (Maybe Pointer) DirectDeclarator

data DirectDeclarator = IDirectDeclarator Identifier
                      | DDirectDeclarator LEFTPAREN Declarator RIGHTPAREN
                      | CDirectDeclarator DirectDeclarator LEFTSQUARE (Maybe ConstantExpression) RIGHTSQUARE
                      | PDirectDeclarator DirectDeclarator LEFTPAREN (Maybe (Either ParameterTypeList IdentifierList)) RIGHTPAREN

data Pointer = POINTER (Maybe TypeQualifierList) (Maybe Pointer)

type TypeQualifierList = SimpleList TypeQualifier

data ParameterTypeList = ParameterTypeList ParameterList (Maybe (Pair COMMA ELLIPSIS))

type ParameterList = CommaList ParameterDeclaration

data ParameterDeclaration = ParameterDeclaration DeclarationSpecifiers (Maybe (Either Declarator AbstractDeclarator))

type IdentifierList = CommaList Identifier

data TypeName = TypeName SpecifierQualifierList (Maybe AbstractDeclarator)

data AbstractDeclarator = AbstractDeclarator (These Pointer DirectAbstractDeclarator)

data DirectAbstractDeclarator = ADirectAbstractDeclarator LEFTPAREN AbstractDeclarator RIGHTPAREN
                              | CDirectAbstractDeclarator (Maybe DirectAbstractDeclarator) LEFTSQUARE (Maybe ConstantExpression) RIGHTSQUARE
                              | PDirectAbstractDeclarator (Maybe DirectAbstractDeclarator) LEFTPAREN (Maybe ParameterTypeList) RIGHTPAREN

type TypedefName = Identifier

data Initializer = AInitializer AssignmentExpression
                 | LInitializer LEFTCURLY InitializerList (Maybe COMMA) RIGHTCURLY

type InitializerList = CommaList Initializer


-- A.1.2.3 Statements

data Statement = LStatement LabeledStatement
               | CStatement CompoundStatement
               | EStatement ExpressionStatement
               | SStatement SelectionStatement
               | IStatement IterationStatement
               | JStatement JumpStatement

data LabeledStatement = Label Identifier COLON Statement
                      | CASE  ConstantExpression COLON Statement
                      | DEFAULT COLON Statement

data CompoundStatement = CompoundStatement LEFTCURLY (Maybe DeclarationList) (Maybe StatementList) RIGHTCURLY

type DeclarationList = SimpleList Declaration

type StatementList = SimpleList Statement

data ExpressionStatement = ExpressionStatement (Maybe Expression) SEMICOLON

data SelectionStatement = IF LEFTPAREN Expression RIGHTPAREN Statement (Maybe (Pair ELSE Statement))
                        | SWITCH LEFTPAREN Expression RIGHTPAREN Statement

data IterationStatement = WHILE LEFTPAREN Expression RIGHTPAREN Statement
                        | DO Statement DOWHILE LEFTPAREN Expression RIGHTPAREN SEMICOLON
                        | FOR LEFTPAREN (Maybe Expression) SEMICOLON (Maybe Expression) SEMICOLON
                                (Maybe Expression) RIGHTPAREN Statement

data JumpStatement = GOTO Identifier SEMICOLON
                   | CONTINUE SEMICOLON
                   | BREAK SEMICOLON
                   | RETURN (Maybe Expression) SEMICOLON

-- A.1.2.4 External definitions

type TranslationUnit = SimpleList ExternalDeclaration

data ExternalDeclaration = ExternalDeclaration (Either FunctionDefinition Declaration)

data FunctionDefinition = Function (Maybe DeclarationSpecifiers) Declarator (Maybe DeclarationList) CompoundStatement
