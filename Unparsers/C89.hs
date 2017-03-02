{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Unparsers.C89 (
    renderPretty
) where

import Grammars.C89 (
    (#:),
    EndBrace(..),
    fromList,

    Choose(..),
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
    EnumeratorList,
    Enumerator(..),
    TypeQualifier(..),
    Declarator(..),
    DirectDeclarator(..),
    Pointer(..),
    PointerList(..),
    TypeQualifierList,
    ParameterTypeList(..),
    ParameterList,
    ParameterDeclaration(..),
    IdentifierList,
    TypeName(..),
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
    )

import Trashcan.These (These(..))

import Text.PrettyPrint (
    Doc,
    zeroWidthText,
    isEmpty,
    render,
    empty,
    nest,
    (<>),
    ($+$)
    )

renderPretty :: TranslationUnit -> String
renderPretty = render . pretty

indent = 4 :: Int

-------------------
-- PP Overrides  --
-------------------

char :: Char -> Doc
char = zeroWidthText . (:[])

text :: String -> Doc
text = zeroWidthText

(<+>) :: Doc -> Doc -> Doc
a <+> b | isEmpty b = a
a <+> b | isEmpty a = b
a <+> b             = a <> zeroWidthText " " <> b

hsep :: [Doc] -> Doc
hsep = foldl (<+>) empty

($++$) :: Doc -> Doc -> Doc
a $++$ b | isEmpty b = a
a $++$ b | isEmpty a = b
a $++$ b             = a $+$ text "" $+$ b

-------------------
-- Helpful Class --
-------------------

class Prettyable a where
    pretty :: a -> Doc

-------------------
-- Helpful Insts --
-------------------

instance Prettyable a => Prettyable (Maybe a) where
    pretty Nothing  = empty
    pretty (Just a) = pretty a

instance (Prettyable a, Prettyable b) => Prettyable (Either a b) where
    pretty (Left a)  = pretty a
    pretty (Right b) = pretty b

instance (Prettyable a, Prettyable b, Prettyable c) => Prettyable (Choose a b c) where
    pretty (A a) = pretty a
    pretty (B b) = pretty b
    pretty (C c) = pretty c

instance (Prettyable a, Prettyable b) => Prettyable (These a b) where
    pretty (This a)    = pretty a
    pretty (That b)    = pretty b
    pretty (These a b) = pretty a <+> pretty b

instance (Prettyable a, Prettyable b) => Prettyable (Pair a b) where
    pretty (Pair a b)  = pretty a <+> pretty b

-------------------
-- The C Grammar --
-------------------

-- A.1.1.1 Tokens

-- A.1.1.3 Identifiers

--instance Prettyable Identifier where
--    pretty = text

instance Prettyable String where
    pretty = text

-- A.1.1.4 Constants

--instance Prettyable Constant where
--    pretty = text
--
--type EnumerationConstant = Identifier
--
---- A.1.1.5 String literals
--
--instance Prettyable StringLiteral where
--    pretty = text

-- A.1.2 Phrase structure grammar

-- Some necessary terminals; missing terminals are constructors below.

instance Prettyable LEFTSQUARE where
    pretty _ = char '['

instance Prettyable RIGHTSQUARE where
    pretty _ = char ']'

instance Prettyable LEFTPAREN where
    pretty _ = char '('

instance Prettyable RIGHTPAREN where
    pretty _ = char ')'

instance Prettyable LEFTCURLY where
    pretty _ = char '{'

instance Prettyable RIGHTCURLY where
    pretty _ = char '}'

instance Prettyable COMMA where
    pretty _ = char ','

instance Prettyable ELLIPSIS where
    pretty _ = text "..."

instance Prettyable COLON where
    pretty _ = char ':'

instance Prettyable ELSE where
    pretty _ = text "else"

instance Prettyable EQUAL where
    pretty _ = char '='

instance Prettyable SEMICOLON where
    pretty _ = char ';'

instance Prettyable DOWHILE where
    pretty _ = text "while"

-- A.1.2.1 Expressions
instance Prettyable PrimaryExpression where
    pretty (IPrimaryExpression id) = pretty id
    pretty (CPrimaryExpression c) = pretty c
    pretty (SPrimaryExpression sl) = pretty sl
    pretty (EPrimaryExpression l e r) = pretty l <> pretty e <> pretty r

instance Prettyable PostfixExpression where
    pretty (PPostfixExpression pe) = pretty pe
    pretty (EPostfixExpression pe l e r) = pretty pe <> pretty l <> pretty e <> pretty r
    pretty (APostfixExpression pe l mael r) = pretty pe <> pretty l <> pretty mael <> pretty r
    pretty (pe `DOT` id) = pretty pe <> char '.' <> pretty id
    pretty (pe `ARROW` id) = pretty pe <> text "->" <> pretty id
    pretty (UPostfixExpression pe uc) = pretty pe <> pretty uc

--instance Prettyable ArgumentExpressionList where
--    pretty (CommaList x cl) = pretty x <> pretty cl

instance Prettyable UnaryExpression where
    pretty (PUnaryExpression pe) = pretty pe
    pretty (UUnaryExpression uc ue) = pretty uc <> pretty ue
    pretty (CUnaryExpression uo ce) = pretty uo <> pretty ce
    pretty (SIZEOF (Left ue)) = text "sizeof" <+> pretty ue
    pretty (SIZEOF (Right (Trio l tn r))) = text "sizeof" <> pretty l <> pretty tn <> pretty r

instance Prettyable UnaryCrement where
    pretty INCREMENT = text "++"
    pretty DECREMENT = text "--"

instance Prettyable UnaryOperator where
    pretty ADDRESS_OF = char '&'
    pretty VALUE_OF = char '*'
    pretty POS = char '+'
    pretty NEG = char '-'
    pretty INVERT = char '~'
    pretty LOGICAL_NOT = char '!'

instance Prettyable CastExpression where
    pretty (UCastExpression ue) = pretty ue
    pretty (TCastExpression l tn r ce) = pretty l <> pretty tn <> pretty r <> pretty ce

instance Prettyable MultiplicativeExpression where
    pretty (MultiplicativeExpression e) = pretty e
    pretty (me `TIMES` e) = pretty me <+> char '*' <+> pretty e
    pretty (me `DIV` e) = pretty me <+> char '/' <+> pretty e
    pretty (me `MOD` e) = pretty me <+> char '%' <+> pretty e

instance Prettyable AdditiveExpression where
    pretty (AdditiveExpression e) = pretty e
    pretty (ae `PLUS` e) = pretty ae <+> char '+' <+> pretty e
    pretty (ae `MINUS` e) = pretty ae <+> char '-' <+> pretty e

instance Prettyable ShiftExpression where
    pretty (ShiftExpression e) = pretty e
    pretty (se `LSHIFT` e) = pretty se <+> text "<<" <+> pretty e
    pretty (se `RSHIFT` e) = pretty se <+> text ">>" <+> pretty e

instance Prettyable RelationalExpression where
    pretty (RelationalExpression e) = pretty e
    pretty (re `LESS_THAN` e) = pretty re <+> char '<' <+> pretty e
    pretty (re `GREATER_THAN` e) = pretty re <+> char '>' <+> pretty e
    pretty (re `LESS_EQUAL` e) = pretty re <+> text "<=" <+> pretty e
    pretty (re `GREATER_EQUAL` e) = pretty re <+> text ">=" <+> pretty e

instance Prettyable EqualityExpression where
    pretty (EqualityExpression e) = pretty e
    pretty (ee `EQUALEQUAL` e) = pretty ee <+> text "==" <+> pretty e
    pretty (ee `NOTEQUAL` e) = pretty ee <+> text "!=" <+> pretty e

instance Prettyable ANDExpression where
    pretty (ANDExpression e) = pretty e
    pretty (ae `BITWISE_AND` e) = pretty ae <+> char '&' <+> pretty e

instance Prettyable ExclusiveORExpression where
    pretty (ExclusiveORExpression e) = pretty e
    pretty (oe `BITWISE_XOR` e) = pretty oe <+> char '^' <+> pretty e

instance Prettyable InclusiveORExpression where
    pretty (InclusiveORExpression e) = pretty e
    pretty (oe `BITWISE_OR` e) = pretty oe <+> char '|' <+> pretty e

instance Prettyable LogicalANDExpression where
    pretty (LogicalANDExpression e) = pretty e
    pretty (ae `LOGICAL_AND` e) = pretty ae <+> text "&&" <+> pretty e

instance Prettyable LogicalORExpression where
    pretty (LogicalORExpression e) = pretty e
    pretty (oe `LOGICAL_OR` e) = pretty oe <+> text "||" <+> pretty e

instance Prettyable ConditionalExpression where
    pretty (ConditionalExpression le) = pretty le
    pretty (le `QUESTION` (Trio e c ce)) = hsep [pretty le, char '?', pretty e, pretty c, pretty ce]

instance Prettyable AssignmentExpression where
    pretty (AssignmentExpression ce) = pretty ce
    pretty (ue `ASSIGN` e) = pretty ue <+> char '=' <+> pretty e
    pretty (ue `TIMES_EQUAL` e) = pretty ue <+> text "*=" <+> pretty e
    pretty (ue `DIV_EQUAL` e) = pretty ue <+> text "/=" <+> pretty e
    pretty (ue `MOD_EQUAL` e) = pretty ue <+> text "%=" <+> pretty e
    pretty (ue `PLUS_EQUAL` e) = pretty ue <+> text "+=" <+> pretty e
    pretty (ue `MINUS_EQUAL` e) = pretty ue <+> text "-=" <+> pretty e
    pretty (ue `LSHIFT_EQUAL` e) = pretty ue <+> text "<<=" <+> pretty e
    pretty (ue `RSHIFT_EQUAL` e) = pretty ue <+> text ">>=" <+> pretty e
    pretty (ue `AND_EQUAL` e) = pretty ue <+> text "&=" <+> pretty e
    pretty (ue `XOR_EQUAL` e) = pretty ue <+> text "^=" <+> pretty e
    pretty (ue `OR_EQUAL` e) = pretty ue <+> text "|=" <+> pretty e

instance Prettyable Expression where
    pretty (CommaList x cl) = pretty x <> pretty cl

--type ConstantExpression = ConditionalExpression


-- A.1.2.2 Declarations

instance Prettyable Declaration where
    pretty (Declaration dss midl s) = pretty dss <+> pretty midl <> pretty s

instance Prettyable DeclarationSpecifiers where
    pretty (SimpleList x ds) = pretty x <+> pretty ds

instance Prettyable InitDeclaratorList where
    pretty (CommaList x cl) = pretty x <> pretty cl

instance Prettyable InitDeclarator where
    pretty (InitDeclarator d mi) = pretty d <+> pretty mi

instance Prettyable StorageClassSpecifier where
    pretty TYPEDEF = zeroWidthText "typedef"
    pretty EXTERN = zeroWidthText "extern"
    pretty STATIC = zeroWidthText "static"
    pretty AUTO = zeroWidthText "auto"
    pretty REGISTER = zeroWidthText "register"

instance Prettyable TypeSpecifier where
    pretty VOID = zeroWidthText "void"
    pretty CHAR = zeroWidthText "char"
    pretty SHORT = zeroWidthText "short"
    pretty INT = zeroWidthText "int"
    pretty LONG = zeroWidthText "long"
    pretty FLOAT = zeroWidthText "float"
    pretty DOUBLE = zeroWidthText "double"
    pretty SIGNED = zeroWidthText "single"
    pretty UNSIGNED = zeroWidthText "unsigned"
    pretty (STRUCT (Left id)) = text "struct" <+> pretty id
    pretty (STRUCT (Right (Quad mi l sdl r))) = text "struct" <+> pretty mi $+$ pretty l $+$ nest indent (pretty sdl) $+$ pretty r
    pretty (UNION (Left id)) = text "union" <+> pretty id
    pretty (UNION (Right (Quad mi l sdl r))) = text "union" <+> pretty mi $+$ pretty l $+$ nest indent (pretty sdl) $+$ pretty r
    pretty (ENUM (Left id)) = text "enum" <+> pretty id
    pretty (ENUM (Right (Quad mi l el r))) = text "enum" <+> pretty mi $+$ pretty l $+$ nest indent (pretty el) $+$ pretty r
    pretty (TypeSpecifier tn) = pretty tn

instance Prettyable StructDeclarationList where
    pretty (SimpleList x sdl) = pretty x $+$ pretty sdl

instance Prettyable StructDeclaration where
    pretty (StructDeclaration sql sdl s) = pretty sql <+> pretty sdl <> pretty s

instance Prettyable SpecifierQualifierList where
    pretty (SimpleList x sl) = pretty x <+> pretty sl

instance Prettyable StructDeclaratorList where
    pretty (CommaList x (Just (Pair c cl))) = pretty x <> pretty c $+$ pretty cl
    pretty (CommaList x cl)                 = pretty x <> pretty cl

instance Prettyable EnumeratorList where
    pretty (CommaList x (Just (Pair c cl))) = pretty x <> pretty c $+$ pretty cl
    pretty (CommaList x cl)                 = pretty x <> pretty cl

instance Prettyable Enumerator where
    pretty (Enumerator ec me) = pretty ec <+> pretty me

instance Prettyable TypeQualifier where
    pretty CONST = zeroWidthText "const"
    pretty VOLATILE = zeroWidthText "volatile"

instance Prettyable Declarator where
    pretty (Declarator (Just p) dd) | last p = pretty p <+> pretty dd
        where
            last (SimpleList (POINTER (Just _)) Nothing) = True
            last (SimpleList _ Nothing) = False
            last (SimpleList x (Just sl)) = last sl
    pretty (Declarator mp dd) = pretty mp <> pretty dd

instance Prettyable DirectDeclarator where
    pretty (IDirectDeclarator id) = pretty id
    pretty (DDirectDeclarator l d r) = pretty l <> pretty d <> pretty r
    pretty (CDirectDeclarator dd l me r) = pretty dd <> pretty l <> pretty me <> pretty r
    pretty (PDirectDeclarator dd l mptori r) = pretty dd <> pretty l <> pretty mptori <> pretty r

instance Prettyable Pointer where
    pretty (POINTER mtql) = char '*' <> pretty mtql

instance Prettyable PointerList where
    pretty (SimpleList x sl) = pretty x <+> pretty sl

instance Prettyable TypeQualifierList where
    pretty (SimpleList x tql) = pretty x <+> pretty tql

instance Prettyable ParameterTypeList where
    pretty (ParameterTypeList pl mce) = pretty pl <> pretty mce

instance Prettyable ParameterList where
    pretty (CommaList x cl) = pretty x <> pretty cl

instance Prettyable ParameterDeclaration where
    pretty (ParameterDeclaration ds md) = pretty ds <+> pretty md

instance Prettyable IdentifierList where
    pretty (CommaList x (Just (Pair c cl))) = pretty x <> pretty c $+$ pretty cl
    pretty (CommaList x cl)                 = pretty x <> pretty cl

instance Prettyable TypeName where
    pretty (TypeName sql mad) = pretty sql <+> pretty mad

instance Prettyable DirectAbstractDeclarator where
    pretty (ADirectAbstractDeclarator l ad r) = pretty l <> pretty ad <> pretty r
    pretty (CDirectAbstractDeclarator mdad l me r) = pretty mdad <> pretty l <> pretty me <> pretty r
    pretty (PDirectAbstractDeclarator mdad l mp r) = pretty mdad <> pretty l <> pretty mp <> pretty r

--type TypedefName = Identifier

instance Prettyable Initializer where
    pretty (AInitializer e) = pretty e
    pretty (LInitializer l il c r) = pretty l $+$ nest indent (pretty il <> pretty c) $+$ pretty r

instance Prettyable InitializerList where
    pretty (CommaList x (Just (Pair c cl))) = pretty x <> pretty c $+$ pretty cl
    pretty (CommaList x cl)                 = pretty x <> pretty cl


-- A.1.2.3 Statements

instance Prettyable Statement where
    pretty (LStatement s) = pretty s
    pretty (CStatement s) = pretty s
    pretty (EStatement s) = pretty s
    pretty (SStatement s) = pretty s
    pretty (IStatement s) = pretty s
    pretty (JStatement s) = pretty s

instance Prettyable LabeledStatement where
    pretty (Label i c s) = pretty i <> pretty c $+$ pretty s
    pretty (CASE ce c s) = nest (-indent) (text "case") <+> pretty ce <> pretty c $+$ pretty s
    pretty (DEFAULT c s) = nest (-indent) (text "default") <> pretty c $+$ pretty s

instance Prettyable CompoundStatement where
    pretty (CompoundStatement l mdl msl r) = pretty l $+$
                                                 nest indent (pretty mdl) $++$
                                                 nest indent (pretty msl) $+$
                                             pretty r

instance Prettyable DeclarationList where
    pretty (SimpleList x dl) = pretty x $+$ pretty dl

instance Prettyable StatementList where
    pretty (SimpleList l@(CStatement _) sl) = pretty l $++$ pretty sl
    pretty (SimpleList l@(SStatement _) sl) = pretty l $++$ pretty sl
    pretty (SimpleList l@(IStatement _) sl) = pretty l $++$ pretty sl
    pretty (SimpleList x (Just (SimpleList l@(LStatement _) sl))) = pretty x $++$ pretty l $+$ pretty sl
    pretty (SimpleList x (Just (SimpleList l@(CStatement _) sl))) = pretty x $++$ pretty l $++$ pretty sl
    pretty (SimpleList x (Just (SimpleList l@(SStatement _) sl))) = pretty x $++$ pretty l $++$ pretty sl
    pretty (SimpleList x (Just (SimpleList l@(IStatement _) sl))) = pretty x $++$ pretty l $++$ pretty sl
    pretty (SimpleList x sl) = pretty x $+$ pretty sl

instance Prettyable ExpressionStatement where
    pretty (ExpressionStatement me s) = pretty me <> pretty s

instance Prettyable SelectionStatement where
    pretty (IF l e r s melse) = text "if" <> pretty l <> pretty e <> pretty r <+> pretty s <+> pretty melse
    pretty (SWITCH l e r s) = text "switch" <> pretty l <> pretty e <> pretty r <+> pretty s

instance Prettyable IterationStatement where
    pretty (WHILE l e r s) = text "while" <> pretty l <> pretty e <> pretty r <+> pretty s
    pretty (DO s w l e r s') = text "do" <+> pretty s <+> pretty w <> pretty l <> pretty e <> pretty r <> pretty s'
    pretty (FOR l me s me' s' me'' r st) = text "for" <> pretty l <> pretty me <> pretty s <+> pretty me' <> pretty s' <+> pretty me'' <> pretty r <+> pretty st

instance Prettyable JumpStatement where
    pretty (GOTO id s) = text "goto" <+> pretty id <> pretty s
    pretty (CONTINUE s) = text "continue" <> pretty s
    pretty (BREAK s) = text "break" <> pretty s
    pretty (RETURN me s) = text "return" <+> pretty me <> pretty s

-- A.1.2.4 External definitions

instance Prettyable TranslationUnit where
    pretty (SimpleList x tu) = pretty x $+$ text "" $+$ pretty tu

instance Prettyable ExternalDeclaration where
    pretty (ExternalDeclaration efdord) = pretty efdord

instance Prettyable FunctionDefinition where
    pretty (Function mds d mdl cs) = pretty mds <+> pretty d <+> pretty mdl $+$ pretty cs
