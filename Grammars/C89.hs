{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Grammars.C89 (
    (#:),
    EndBrace(..),
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
    DOWHILE(..),

    PrimaryExpression(..),
    PostfixExpression(..),
    ArgumentExpressionList,
    UnaryExpression(..),
    UnaryCrement(..),
    UnaryOperator(..),
    CastExpression(..),
    MultiplicativeExpression(..),
    AdditiveExpression(..),
    ShiftExpression(..),
    RelationalExpression(..),
    EqualityExpression(..),
    ANDExpression(..),
    ExclusiveORExpression(..),
    InclusiveORExpression(..),
    LogicalANDExpression(..),
    LogicalORExpression(..),
    ConditionalExpression(..),
    AssignmentExpression(..),
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

class Expr a b where
    (#:) :: b -> EndBrace -> a
    def :: b -> a
    (#:) a b = def a

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

data EndBrace = (:#)
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
data DOWHILE = DOWHILE

-- A.1.2.1 Expressions
data PrimaryExpression = IPrimaryExpression Identifier
                       | CPrimaryExpression Constant
                       | SPrimaryExpression StringLiteral
                       | EPrimaryExpression LEFTPAREN Expression RIGHTPAREN
instance Expr PrimaryExpression Identifier where
    def = IPrimaryExpression

data PostfixExpression = PPostfixExpression PrimaryExpression
                       | EPostfixExpression PostfixExpression LEFTSQUARE Expression RIGHTSQUARE
                       | APostfixExpression PostfixExpression LEFTPAREN (Maybe ArgumentExpressionList) RIGHTPAREN
                       | PostfixExpression `DOT` Identifier
                       | PostfixExpression `ARROW` Identifier
                       | UPostfixExpression PostfixExpression UnaryCrement
instance Expr PostfixExpression PrimaryExpression where
    def = PPostfixExpression
instance (Expr PrimaryExpression x) => Expr PostfixExpression x where
    def = PPostfixExpression . def

type ArgumentExpressionList = CommaList AssignmentExpression

data UnaryExpression = PUnaryExpression PostfixExpression
                     | UUnaryExpression UnaryCrement UnaryExpression
                     | CUnaryExpression UnaryOperator CastExpression
                     | SIZEOF (Either UnaryExpression (Trio LEFTPAREN TypeName RIGHTPAREN))
instance Expr UnaryExpression PostfixExpression where
    def = PUnaryExpression
instance (Expr PostfixExpression x) => Expr UnaryExpression x where
    def = PUnaryExpression . def

data UnaryCrement =  INCREMENT | DECREMENT

data UnaryOperator = ADDRESS_OF | VALUE_OF | POS | NEG | INVERT | LOGICAL_NOT

data CastExpression = UCastExpression UnaryExpression
                    | TCastExpression LEFTPAREN TypeName RIGHTPAREN CastExpression
instance Expr CastExpression UnaryExpression where
    def = UCastExpression
instance (Expr UnaryExpression x) => Expr CastExpression x where
    def = UCastExpression . def

data MultiplicativeExpression = MultiplicativeExpression CastExpression
                              | MultiplicativeExpression `TIMES` CastExpression
                              | MultiplicativeExpression `DIV` CastExpression
                              | MultiplicativeExpression `MOD` CastExpression
instance Expr MultiplicativeExpression CastExpression where
    def = MultiplicativeExpression
instance (Expr CastExpression x) => Expr MultiplicativeExpression x where
    def = MultiplicativeExpression . def

data AdditiveExpression = AdditiveExpression MultiplicativeExpression
                        | AdditiveExpression `PLUS` MultiplicativeExpression
                        | AdditiveExpression `MINUS` MultiplicativeExpression
instance Expr AdditiveExpression MultiplicativeExpression where
    def = AdditiveExpression
instance (Expr MultiplicativeExpression x) => Expr AdditiveExpression x where
    def = AdditiveExpression . def

data ShiftExpression = ShiftExpression AdditiveExpression
                     | ShiftExpression `LSHIFT` AdditiveExpression
                     | ShiftExpression `RSHIFT` AdditiveExpression
instance Expr ShiftExpression AdditiveExpression where
    def = ShiftExpression
instance (Expr AdditiveExpression x) => Expr ShiftExpression x where
    def = ShiftExpression . def

data RelationalExpression = RelationalExpression ShiftExpression
                          | RelationalExpression `LESS_THAN` ShiftExpression
                          | RelationalExpression `GREATER_THAN` ShiftExpression
                          | RelationalExpression `LESS_EQUAL` ShiftExpression
                          | RelationalExpression `GREATER_EQUAL` ShiftExpression
instance Expr RelationalExpression ShiftExpression where
    def = RelationalExpression
instance (Expr ShiftExpression x) => Expr RelationalExpression x where
    def = RelationalExpression . def

data EqualityExpression = EqualityExpression RelationalExpression
                        | EqualityExpression `EQUALEQUAL` RelationalExpression
                        | EqualityExpression `NOTEQUAL` RelationalExpression
instance Expr EqualityExpression RelationalExpression where
    def = EqualityExpression
instance (Expr RelationalExpression x) => Expr EqualityExpression x where
    def = EqualityExpression . def


data ANDExpression = ANDExpression EqualityExpression
                   | ANDExpression `BITWISE_AND` EqualityExpression
instance Expr ANDExpression EqualityExpression where
    def = ANDExpression
instance (Expr EqualityExpression x) => Expr ANDExpression x where
    def = ANDExpression . def

data ExclusiveORExpression = ExclusiveORExpression ANDExpression
                           | ExclusiveORExpression `BITWISE_XOR` ANDExpression
instance Expr ExclusiveORExpression ANDExpression where
    def = ExclusiveORExpression
instance (Expr ANDExpression x) => Expr ExclusiveORExpression x where
    def = ExclusiveORExpression . def

data InclusiveORExpression = InclusiveORExpression ExclusiveORExpression
                           | InclusiveORExpression `BITWISE_OR` ExclusiveORExpression
instance Expr InclusiveORExpression ExclusiveORExpression where
    def = InclusiveORExpression
instance (Expr ExclusiveORExpression x) => Expr InclusiveORExpression x where
    def = InclusiveORExpression . def

data LogicalANDExpression = LogicalANDExpression InclusiveORExpression
                          | LogicalANDExpression `LOGICAL_AND` InclusiveORExpression
instance Expr LogicalANDExpression InclusiveORExpression where
    def = LogicalANDExpression
instance (Expr InclusiveORExpression x) => Expr LogicalANDExpression x where
    def = LogicalANDExpression . def

data LogicalORExpression = LogicalORExpression LogicalANDExpression
                         | LogicalORExpression `LOGICAL_OR` LogicalANDExpression
instance Expr LogicalORExpression LogicalANDExpression where
    def = LogicalORExpression
instance (Expr LogicalANDExpression x) => Expr LogicalORExpression x where
    def = LogicalORExpression . def

data ConditionalExpression = ConditionalExpression LogicalORExpression
                           | LogicalORExpression `QUESTION` (Trio Expression COLON ConditionalExpression)
instance Expr ConditionalExpression LogicalORExpression where
    def = ConditionalExpression
instance (Expr LogicalORExpression x) => Expr ConditionalExpression x where
    def = ConditionalExpression . def

data AssignmentExpression = AssignmentExpression ConditionalExpression
                          | UnaryExpression `ASSIGN` AssignmentExpression
                          | UnaryExpression `TIMES_EQUAL` AssignmentExpression
                          | UnaryExpression `DIV_EQUAL` AssignmentExpression
                          | UnaryExpression `MOD_EQUAL` AssignmentExpression
                          | UnaryExpression `PLUS_EQUAL` AssignmentExpression
                          | UnaryExpression `MINUS_EQUAL` AssignmentExpression
                          | UnaryExpression `LSHIFT_EQUAL` AssignmentExpression
                          | UnaryExpression `RSHIFT_EQUAL` AssignmentExpression
                          | UnaryExpression `AND_EQUAL` AssignmentExpression
                          | UnaryExpression `XOR_EQUAL` AssignmentExpression
                          | UnaryExpression `OR_EQUAL` AssignmentExpression
instance Expr AssignmentExpression ConditionalExpression where
    def = AssignmentExpression
instance (Expr ConditionalExpression x) => Expr AssignmentExpression x where
    def = AssignmentExpression . def

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
