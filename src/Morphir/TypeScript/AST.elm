module Morphir.TypeScript.AST exposing (..)

{-| This module contains the TypeScript AST (Abstract Syntax Tree). The purpose of this AST is to make it easier to
generate valid TypeScript source code and to separate the language syntax from low-level formatting concerns. We use
this AST as the output of the TypeScript backend and also as the input of the pretty-printer that turns it into the
final text representation.

The AST is maintained manually and it does not have to cover the whole language. We focus on the parts of the language
that we use in the backend.

-}

import Morphir.IR.FQName exposing (FQName)
import Morphir.TypeScript.NamespacePath exposing (NamespacePath, namespaceNameFromPackageAndModule)


{-- Expressions --}

type Expression
    = ArrayLiteralExpression (List Expression)
    | CallExpression CallExpressionDetails
    | Identifier String
    | MemberExpression
        { object : Expression
        , member : Expression
        }
    | NewExpression
        { constructor : String
        , arguments : List Expression
        }
    | NullLiteral
    | ObjectLiteralExpression { properties : List ( String, Expression ) }
    | StringLiteralExpression String

type alias CallExpressionDetails =
    { function : Expression
    , arguments : List Expression
    }


{-- Statements --}

type Statement
    = AssignmentStatement Expression (Maybe TypeExp) Expression
    | ExpressionStatement Expression
    | FunctionDeclaration FunctionDetails
    | LetStatement Expression (Maybe TypeExp) Expression
    | ReturnStatement Expression


{-- Functions --}

type Privacy
    = Public
    | Private

type FunctionScope
    = ModuleFunction
    | ClassMemberFunction
    | ClassStaticFunction

type alias Parameter =
    { modifiers : List String
    , name : String
    , typeAnnotation : Maybe TypeExp
    }

type alias FunctionDetails =
    { name : String
    , scope : FunctionScope
    , parameters : List Parameter
    , body : List Statement
    , privacy : Privacy
    }

parameter : List String -> String -> Maybe TypeExp -> Parameter
parameter modifiers name typeAnnotation =
    { modifiers = modifiers
    , name = name
    , typeAnnotation = typeAnnotation
    }


{-- Types --}

{-| A type expression represents the right-hand side of a type annotation or a type alias.

The structure follows the documentation here:
<https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#the-primitives-string-number-and-boolean>

-}
type TypeExp
    = Any
    | Boolean
    | List TypeExp {- Represents a Morphir 'List' type, as a Typescript 'Array' type -}
    | LiteralString String
    | Map TypeExp TypeExp
    | Number
    | Object ObjectExp
    | String
    | Tuple (List TypeExp)
    | TypeRef FQName (List TypeExp)
    | Union (List TypeExp)
    | Variable String
    | UnhandledType String

{-| Represents an object expression (or interface definition) as a list of name-and-type pairs.
-}
type alias ObjectExp =
    List ( String, TypeExp )

emptyObject : Expression
emptyObject =
    ObjectLiteralExpression { properties = [] }


{-- Top level entities --}

type TypeDef
    = ImportAlias
        { name : String
        , privacy : Privacy
        , namespacePath : NamespacePath
        }
    | Namespace
        { name : String
        , privacy : Privacy
        , content : List TypeDef
        }
    | TypeAlias
        { name : String
        , doc : String
        , privacy : Privacy
        , variables : List TypeExp
        , typeExpression : TypeExp
        , decoder : Maybe Statement
        , encoder : Maybe Statement
        }
    | VariantClass
        { name : String
        , privacy : Privacy
        , variables : List TypeExp
        , body : List Statement
        , constructor : Maybe Statement
        , decoder : Maybe Statement
        , encoder : Maybe Statement
        , typeExpressions : List TypeExp -- for collecting import refs
        }

type alias ImportDeclaration =
    { importClause : String
    , moduleSpecifier : String
    }

type alias CompilationUnit =
    { dirPath : List String
    , fileName : String
    , imports : List ImportDeclaration
    , typeDefs : List TypeDef
    }
