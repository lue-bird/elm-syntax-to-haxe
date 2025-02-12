module ElmSyntaxToHaxe exposing
    ( modules, haxeDeclarationsToModuleString
    , HaxeLetDeclaration(..), HaxeExpression(..), HaxePattern(..), HaxeType(..)
    )

{-| Transpiling [`elm-syntax`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/)
declarations to haxe.

@docs modules, haxeDeclarationsToModuleString
@docs HaxeLetDeclaration, HaxeExpression, HaxePattern, HaxeType

If you need more fine-grained helpers,
[open an issue](https://github.com/lue-bird/elm-syntax-format/issues/new)

-}

import Data.Graph
import Elm.Syntax.Declaration
import Elm.Syntax.Exposing
import Elm.Syntax.Expression
import Elm.Syntax.File
import Elm.Syntax.Import
import Elm.Syntax.Module
import Elm.Syntax.ModuleName
import Elm.Syntax.Node
import Elm.Syntax.Pattern
import Elm.Syntax.Type
import Elm.Syntax.TypeAlias
import Elm.Syntax.TypeAnnotation
import FastDict
import FastSet
import Print exposing (Print)
import Unicode


{-| The sub-set of haxe type syntax used in generated code
-}
type HaxeType
    = HaxeTypeConstruct
        { moduleOrigin : Maybe String
        , name : String
        , arguments : List HaxeType
        }
    | HaxeTypeRecord (FastDict.Dict String HaxeType)
    | HaxeTypeVariable String
    | HaxeTypeFunction
        { input : List HaxeType
        , output : HaxeType
        }


{-| The sub-set of haxe pattern syntax used in generated code
-}
type HaxePattern
    = HaxePatternIgnore
    | HaxePatternFloat Float
    | HaxePatternString String
    | HaxePatternVariable String
    | HaxePatternVariableCapture
        { variable : String
        , pattern : HaxePattern
        }
    | HaxePatternVariant
        { moduleOrigin : Maybe String
        , name : String
        , values : List HaxePattern
        }
    | HaxePatternRecord (FastDict.Dict String HaxePattern)


{-| The sub-set of haxe expression syntax used in generated code
-}
type HaxeExpression
    = HaxeExpressionFloat Float
    | HaxeExpressionString String
    | HaxeExpressionReference
        { moduleOrigin : Maybe String
        , name : String
        }
    | HaxeExpressionRecordAccess
        { record : HaxeExpression
        , field : String
        }
    | HaxeExpressionList (List HaxeExpression)
    | HaxeExpressionRecord (FastDict.Dict String HaxeExpression)
    | HaxeExpressionCall
        { called : HaxeExpression
        , arguments : List HaxeExpression
        }
    | HaxeExpressionLambda
        { parameter0 : Maybe String
        , parameter1Up : List (Maybe String)
        , result : HaxeExpression
        }
    | HaxeExpressionSwitch
        { matched : HaxeExpression
        , case0 :
            { pattern : HaxePattern
            , result : HaxeExpression
            }
        , case1Up :
            List
                { pattern : HaxePattern
                , result : HaxeExpression
                }
        }
    | HaxeExpressionWithLocalDeclaration
        { declaration :
            { name : String
            , parameters : List (Maybe String)
            , result : HaxeExpression
            , type_ : Maybe HaxeType
            }
        , result : HaxeExpression
        }


{-| The sub-set of haxe local declaration syntax used in generated haxe code
-}
type HaxeLetDeclaration
    = HaxeLetDestructuring
        { pattern : HaxePattern
        , expression : HaxeExpression
        }
    | HaxeLetDeclarationValueOrFunction
        { name : String
        , parameters : List (Maybe String)
        , result : HaxeExpression
        , type_ : Maybe HaxeType
        }


{-| How do references used in a module map to their origin module?

Contains variants, variant function and value declaration names.

-}
type alias ModuleContext =
    { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            Elm.Syntax.ModuleName.ModuleName
    , variantLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
            , valueCount : Int
            }
    }


{-| Calculate valid mappings of qualifications + name
to origin module based on a module's imports.

Requires all exposed names
so we can resolve `exposing (..)` and `EnumType(..)`.

-}
importsToModuleContext :
    FastDict.Dict
        Elm.Syntax.ModuleName.ModuleName
        { valueOrFunctionOrTypeAliasNames : FastSet.Set String
        , enumTypesExposingVariants :
            FastDict.Dict String (FastDict.Dict String { valueCount : Int })
        }
    -> List (Elm.Syntax.Node.Node Elm.Syntax.Import.Import)
    ->
        { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup :
            FastDict.Dict
                ( Elm.Syntax.ModuleName.ModuleName, String )
                Elm.Syntax.ModuleName.ModuleName
        , variantLookup :
            FastDict.Dict
                ( Elm.Syntax.ModuleName.ModuleName, String )
                { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
                , valueCount : Int
                }
        }
importsToModuleContext moduleExposes imports =
    let
        importsNormal :
            List
                { moduleName : Elm.Syntax.ModuleName.ModuleName
                , alias : Maybe String
                , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes :
                    FastSet.Set String
                , exposedVariants :
                    FastDict.Dict String { valueCount : Int }
                }
        importsNormal =
            implicitImports
                ++ (imports
                        |> List.map
                            (\(Elm.Syntax.Node.Node _ syntaxImport) ->
                                let
                                    importModuleName : Elm.Syntax.ModuleName.ModuleName
                                    importModuleName =
                                        syntaxImport.moduleName |> Elm.Syntax.Node.value

                                    exposes :
                                        { valuesAndFunctionsAndTypeAliasesAndEnumTypes :
                                            FastSet.Set String
                                        , variants :
                                            FastDict.Dict String { valueCount : Int }
                                        }
                                    exposes =
                                        case syntaxImport.exposingList of
                                            Nothing ->
                                                { valuesAndFunctionsAndTypeAliasesAndEnumTypes =
                                                    FastSet.empty
                                                , variants = FastDict.empty
                                                }

                                            Just (Elm.Syntax.Node.Node _ syntaxExposing) ->
                                                case moduleExposes |> FastDict.get importModuleName of
                                                    Nothing ->
                                                        { valuesAndFunctionsAndTypeAliasesAndEnumTypes =
                                                            FastSet.empty
                                                        , variants = FastDict.empty
                                                        }

                                                    Just moduleExposedNames ->
                                                        case syntaxExposing of
                                                            Elm.Syntax.Exposing.All _ ->
                                                                { valuesAndFunctionsAndTypeAliasesAndEnumTypes =
                                                                    moduleExposedNames.enumTypesExposingVariants
                                                                        |> FastDict.foldl
                                                                            (\enumTypeName _ soFar ->
                                                                                soFar |> FastSet.insert enumTypeName
                                                                            )
                                                                            moduleExposedNames.valueOrFunctionOrTypeAliasNames
                                                                , variants =
                                                                    moduleExposedNames.enumTypesExposingVariants
                                                                        |> FastDict.foldl
                                                                            (\_ variantNames soFar -> FastDict.union variantNames soFar)
                                                                            FastDict.empty
                                                                }

                                                            Elm.Syntax.Exposing.Explicit explicitEposes ->
                                                                explicitEposes
                                                                    |> List.foldl
                                                                        (\(Elm.Syntax.Node.Node _ expose) soFar ->
                                                                            case expose of
                                                                                Elm.Syntax.Exposing.InfixExpose _ ->
                                                                                    soFar

                                                                                Elm.Syntax.Exposing.TypeOrAliasExpose name ->
                                                                                    { valuesAndFunctionsAndTypeAliasesAndEnumTypes =
                                                                                        soFar.valuesAndFunctionsAndTypeAliasesAndEnumTypes
                                                                                            |> FastSet.insert name
                                                                                    , variants = soFar.variants
                                                                                    }

                                                                                Elm.Syntax.Exposing.FunctionExpose name ->
                                                                                    { valuesAndFunctionsAndTypeAliasesAndEnumTypes =
                                                                                        soFar.valuesAndFunctionsAndTypeAliasesAndEnumTypes
                                                                                            |> FastSet.insert name
                                                                                    , variants = soFar.variants
                                                                                    }

                                                                                Elm.Syntax.Exposing.TypeExpose enumTypeExpose ->
                                                                                    { valuesAndFunctionsAndTypeAliasesAndEnumTypes =
                                                                                        soFar.valuesAndFunctionsAndTypeAliasesAndEnumTypes
                                                                                            |> FastSet.insert enumTypeExpose.name
                                                                                    , variants =
                                                                                        case enumTypeExpose.open of
                                                                                            Nothing ->
                                                                                                soFar.variants

                                                                                            Just _ ->
                                                                                                case
                                                                                                    moduleExposedNames.enumTypesExposingVariants
                                                                                                        |> FastDict.get enumTypeExpose.name
                                                                                                of
                                                                                                    Nothing ->
                                                                                                        soFar.variants

                                                                                                    Just enumTypeDeclared ->
                                                                                                        FastDict.union
                                                                                                            soFar.variants
                                                                                                            enumTypeDeclared
                                                                                    }
                                                                        )
                                                                        { valuesAndFunctionsAndTypeAliasesAndEnumTypes =
                                                                            FastSet.empty
                                                                        , variants = FastDict.empty
                                                                        }
                                in
                                { moduleName = importModuleName
                                , alias =
                                    syntaxImport.moduleAlias
                                        |> Maybe.map
                                            (\(Elm.Syntax.Node.Node _ syntaxAlias) ->
                                                syntaxAlias |> String.join "."
                                            )
                                , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes =
                                    exposes.valuesAndFunctionsAndTypeAliasesAndEnumTypes
                                , exposedVariants = exposes.variants
                                }
                            )
                   )
                |> importsCombine
    in
    importsNormal
        |> List.foldl
            (\syntaxImport soFar ->
                let
                    importedModuleMembers :
                        { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup :
                            FastSet.Set String
                        , variants : FastDict.Dict String { valueCount : Int }
                        }
                    importedModuleMembers =
                        case moduleExposes |> FastDict.get syntaxImport.moduleName of
                            Nothing ->
                                { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                                    FastSet.empty
                                , variants = FastDict.empty
                                }

                            Just moduleExposedNames ->
                                { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                                    moduleExposedNames.enumTypesExposingVariants
                                        |> FastDict.foldl
                                            (\enumTypeName _ namesSoFar ->
                                                namesSoFar
                                                    |> FastSet.insert enumTypeName
                                            )
                                            moduleExposedNames.valueOrFunctionOrTypeAliasNames
                                , variants =
                                    moduleExposedNames.enumTypesExposingVariants
                                        |> FastDict.foldl
                                            (\_ variantNames variantsSoFar ->
                                                FastDict.union variantNames variantsSoFar
                                            )
                                            FastDict.empty
                                }
                in
                moduleImportsContextMerge
                    (moduleImportsContextMerge
                        { variantLookup =
                            syntaxImport.exposedVariants
                                |> FastDict.foldl
                                    (\variantName variantInfo dictSoFar ->
                                        dictSoFar
                                            |> FastDict.insert ( [], variantName )
                                                { moduleOrigin = syntaxImport.moduleName
                                                , valueCount = variantInfo.valueCount
                                                }
                                    )
                                    FastDict.empty
                        , valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                            syntaxImport.exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes
                                |> FastSet.foldl
                                    (\expose dictSoFar ->
                                        dictSoFar
                                            |> FastDict.insert ( [], expose )
                                                syntaxImport.moduleName
                                    )
                                    FastDict.empty
                        }
                        (case syntaxImport.alias of
                            Nothing ->
                                { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                                    importedModuleMembers.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup
                                        |> FastSet.foldl
                                            (\exposeFromImportedModule dictSoFar ->
                                                dictSoFar
                                                    |> FastDict.insert
                                                        ( syntaxImport.moduleName, exposeFromImportedModule )
                                                        syntaxImport.moduleName
                                            )
                                            FastDict.empty
                                , variantLookup =
                                    importedModuleMembers.variants
                                        |> FastDict.foldl
                                            (\exposeFromImportedModule variantInfo dictSoFar ->
                                                dictSoFar
                                                    |> FastDict.insert
                                                        ( syntaxImport.moduleName, exposeFromImportedModule )
                                                        { moduleOrigin = syntaxImport.moduleName
                                                        , valueCount = variantInfo.valueCount
                                                        }
                                            )
                                            FastDict.empty
                                }

                            Just importAlias ->
                                { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                                    importedModuleMembers.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup
                                        |> FastSet.foldl
                                            (\exposeFromImportedModule dictSoFar ->
                                                dictSoFar
                                                    |> FastDict.insert
                                                        ( [ importAlias ], exposeFromImportedModule )
                                                        syntaxImport.moduleName
                                            )
                                            FastDict.empty
                                , variantLookup =
                                    importedModuleMembers.variants
                                        |> FastDict.foldl
                                            (\exposeFromImportedModule variantInfo dictSoFar ->
                                                dictSoFar
                                                    |> FastDict.insert
                                                        ( [ importAlias ], exposeFromImportedModule )
                                                        { moduleOrigin = syntaxImport.moduleName
                                                        , valueCount = variantInfo.valueCount
                                                        }
                                            )
                                            FastDict.empty
                                }
                        )
                    )
                    soFar
            )
            { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                FastDict.empty
            , variantLookup = FastDict.empty
            }


moduleImportsContextMerge :
    { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            Elm.Syntax.ModuleName.ModuleName
    , variantLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
            , valueCount : Int
            }
    }
    ->
        { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup :
            FastDict.Dict
                ( Elm.Syntax.ModuleName.ModuleName, String )
                Elm.Syntax.ModuleName.ModuleName
        , variantLookup :
            FastDict.Dict
                ( Elm.Syntax.ModuleName.ModuleName, String )
                { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
                , valueCount : Int
                }
        }
    ->
        { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup :
            FastDict.Dict
                ( Elm.Syntax.ModuleName.ModuleName, String )
                Elm.Syntax.ModuleName.ModuleName
        , variantLookup :
            FastDict.Dict
                ( Elm.Syntax.ModuleName.ModuleName, String )
                { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
                , valueCount : Int
                }
        }
moduleImportsContextMerge a b =
    { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
        FastDict.union
            a.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup
            b.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup
    , variantLookup =
        FastDict.union
            a.variantLookup
            b.variantLookup
    }


moduleContextMerge : ModuleContext -> ModuleContext -> ModuleContext
moduleContextMerge a b =
    { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
        FastDict.union
            a.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup
            b.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup
    , variantLookup =
        FastDict.union
            a.variantLookup
            b.variantLookup
    }


implicitImports :
    List
        { moduleName : Elm.Syntax.ModuleName.ModuleName
        , alias : Maybe String
        , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes :
            FastSet.Set String
        , exposedVariants :
            FastDict.Dict String { valueCount : Int }
        }
implicitImports =
    [ { moduleName = [ "Basics" ]
      , alias = Nothing
      , exposedVariants =
            FastDict.fromList
                [ ( "EQ", { valueCount = 0 } )
                , ( "LT", { valueCount = 0 } )
                , ( "GT", { valueCount = 0 } )
                , ( "True", { valueCount = 0 } )
                , ( "False", { valueCount = 0 } )
                ]
      , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes =
            FastSet.fromList
                [ "Int"
                , "Float"
                , "toFloat"
                , "round"
                , "floor"
                , "ceiling"
                , "truncate"
                , "max"
                , "min"
                , "compare"
                , "Order"
                , "Bool"
                , "not"
                , "xor"
                , "modBy"
                , "remainderBy"
                , "negate"
                , "abs"
                , "clamp"
                , "sqrt"
                , "logBase"
                , "e"
                , "pi"
                , "cos"
                , "sin"
                , "tan"
                , "acos"
                , "asin"
                , "atan"
                , "atan2"
                , "degrees"
                , "radians"
                , "turns"
                , "toPolar"
                , "fromPolar"
                , "isNaN"
                , "isInfinite"
                , "identity"
                , "always"
                , "Never"
                , "never"
                ]
      }
    , { moduleName = [ "List" ]
      , alias = Nothing
      , exposedVariants = FastDict.empty
      , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes =
            FastSet.fromList [ "List" ]
      }
    , { moduleName = [ "Maybe" ]
      , alias = Nothing
      , exposedVariants =
            FastDict.fromList
                [ ( "Just", { valueCount = 1 } )
                , ( "Nothing", { valueCount = 0 } )
                ]
      , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes =
            FastSet.fromList [ "Maybe" ]
      }
    , { moduleName = [ "Result" ]
      , alias = Nothing
      , exposedVariants =
            FastDict.fromList
                [ ( "Ok", { valueCount = 1 } )
                , ( "Err", { valueCount = 1 } )
                ]
      , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes =
            FastSet.fromList [ "Result" ]
      }
    , { moduleName = [ "String" ]
      , alias = Nothing
      , exposedVariants = FastDict.empty
      , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes =
            FastSet.fromList [ "String" ]
      }
    , { moduleName = [ "Char" ]
      , alias = Nothing
      , exposedVariants = FastDict.empty
      , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes =
            FastSet.fromList [ "Char" ]
      }
    , { moduleName = [ "Tuple" ]
      , alias = Nothing
      , exposedVariants = FastDict.empty
      , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes =
            FastSet.empty
      }
    , { moduleName = [ "Debug" ]
      , alias = Nothing
      , exposedVariants = FastDict.empty
      , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes =
            FastSet.empty
      }
    , { moduleName = [ "Platform" ]
      , alias = Nothing
      , exposedVariants = FastDict.empty
      , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes =
            FastSet.fromList [ "Program" ]
      }
    , { moduleName = [ "Platform", "Cmd" ]
      , alias = Just "Cmd"
      , exposedVariants = FastDict.empty
      , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes =
            FastSet.fromList [ "Cmd" ]
      }
    , { moduleName = [ "Platform", "Sub" ]
      , alias = Just "Sub"
      , exposedVariants = FastDict.empty
      , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes =
            FastSet.fromList [ "Sub" ]
      }
    ]


importsCombine :
    List
        { moduleName : Elm.Syntax.ModuleName.ModuleName
        , alias : Maybe String
        , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes :
            FastSet.Set String
        , exposedVariants :
            FastDict.Dict String { valueCount : Int }
        }
    ->
        List
            { moduleName : Elm.Syntax.ModuleName.ModuleName
            , alias : Maybe String
            , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes :
                FastSet.Set String
            , exposedVariants :
                FastDict.Dict String { valueCount : Int }
            }
importsCombine syntaxImports =
    importsCombineFrom [] syntaxImports


importsCombineFrom :
    List
        { moduleName : Elm.Syntax.ModuleName.ModuleName
        , alias : Maybe String
        , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes :
            FastSet.Set String
        , exposedVariants :
            FastDict.Dict String { valueCount : Int }
        }
    ->
        List
            { moduleName : Elm.Syntax.ModuleName.ModuleName
            , alias : Maybe String
            , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes :
                FastSet.Set String
            , exposedVariants :
                FastDict.Dict String { valueCount : Int }
            }
    ->
        List
            { moduleName : Elm.Syntax.ModuleName.ModuleName
            , alias : Maybe String
            , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes :
                FastSet.Set String
            , exposedVariants :
                FastDict.Dict String { valueCount : Int }
            }
importsCombineFrom soFar syntaxImports =
    case syntaxImports of
        [] ->
            soFar

        [ onlyImport ] ->
            onlyImport :: soFar

        import0 :: import1 :: import2Up ->
            if import0.moduleName == import1.moduleName then
                importsCombineFrom soFar
                    (importsMerge import0 import1
                        :: import2Up
                    )

            else
                importsCombineFrom
                    (import0 :: soFar)
                    (import1 :: import2Up)


importsMerge :
    { moduleName : Elm.Syntax.ModuleName.ModuleName
    , alias : Maybe String
    , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes :
        FastSet.Set String
    , exposedVariants :
        FastDict.Dict String { valueCount : Int }
    }
    ->
        { moduleName : Elm.Syntax.ModuleName.ModuleName
        , alias : Maybe String
        , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes :
            FastSet.Set String
        , exposedVariants :
            FastDict.Dict String { valueCount : Int }
        }
    ->
        { moduleName : Elm.Syntax.ModuleName.ModuleName
        , alias : Maybe String
        , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes :
            FastSet.Set String
        , exposedVariants :
            FastDict.Dict String { valueCount : Int }
        }
importsMerge earlier later =
    { moduleName = earlier.moduleName
    , alias =
        case earlier.alias of
            Just alias ->
                alias |> Just

            Nothing ->
                later.alias
    , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes =
        FastSet.union
            earlier.exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes
            later.exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes
    , exposedVariants =
        FastDict.union
            earlier.exposedVariants
            later.exposedVariants
    }


enumTypeDeclaration :
    ModuleContext
    -> Elm.Syntax.Type.Type
    ->
        Result
            String
            { name : String
            , parameters : List String
            , variants : FastDict.Dict String (List HaxeType)
            }
enumTypeDeclaration moduleOriginLookup syntaxEnumType =
    Result.map
        (\variants ->
            { name =
                syntaxEnumType.name
                    |> Elm.Syntax.Node.value
            , parameters =
                syntaxEnumType.generics
                    |> List.map
                        (\(Elm.Syntax.Node.Node _ parameter) ->
                            parameter |> uppercaseNameSanitizeForHaxe
                        )
            , variants = variants |> FastDict.fromList
            }
        )
        (syntaxEnumType.constructors
            |> listMapAndCombineOk
                (\(Elm.Syntax.Node.Node _ syntaxVariant) ->
                    Result.map
                        (\values ->
                            ( syntaxVariant.name |> Elm.Syntax.Node.value
                            , values
                            )
                        )
                        (syntaxVariant.arguments
                            |> listMapAndCombineOk
                                (\value ->
                                    value |> type_ moduleOriginLookup
                                )
                        )
                )
        )


haxeTypeParametersToString : List String -> String
haxeTypeParametersToString haxeTypeParameters =
    case haxeTypeParameters of
        [] ->
            ""

        parameter0 :: parameter1Up ->
            "<"
                ++ ((parameter0 :: parameter1Up)
                        |> String.join ", "
                   )
                ++ ">"


printHaxeChoiceTypeDeclaration :
    { name : String
    , parameters : List String
    , variants : FastDict.Dict String (List HaxeType)
    }
    -> Print
printHaxeChoiceTypeDeclaration haxeEnumType =
    Print.exactly
        ("enum "
            ++ haxeEnumType.name
            ++ (haxeEnumType.parameters
                    |> haxeTypeParametersToString
               )
            ++ " {"
        )
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy
                        (haxeEnumType.variants
                            |> FastDict.toList
                            |> Print.listMapAndIntersperseAndFlatten
                                (\( name, values ) ->
                                    printHaxeVariant
                                        { name = name
                                        , values = values
                                        }
                                )
                                Print.linebreakIndented
                        )
                )
            )
        |> Print.followedBy Print.linebreakIndented
        |> Print.followedBy (Print.exactly "}")


printHaxeVariant : { name : String, values : List HaxeType } -> Print
printHaxeVariant haxeVariant =
    Print.exactly haxeVariant.name
        |> Print.followedBy
            (case haxeVariant.values of
                [] ->
                    Print.empty

                value0 :: value1Up ->
                    let
                        valuePrints : List Print
                        valuePrints =
                            (value0 :: value1Up)
                                |> List.indexedMap
                                    (\index value ->
                                        let
                                            valuePrint : Print
                                            valuePrint =
                                                value |> printHaxeTypeNotParenthesized
                                        in
                                        Print.exactly
                                            ("value" ++ String.fromInt index ++ ":")
                                            |> Print.followedBy
                                                (Print.spaceOrLinebreakIndented
                                                    (valuePrint |> Print.lineSpread)
                                                )
                                            |> Print.followedBy
                                                valuePrint
                                    )

                        fullLineSpread : Print.LineSpread
                        fullLineSpread =
                            valuePrints
                                |> Print.lineSpreadListMapAndCombine Print.lineSpread
                    in
                    Print.exactly "("
                        |> Print.followedBy
                            (Print.withIndentAtNextMultipleOf4
                                (Print.emptyOrLinebreakIndented fullLineSpread
                                    |> Print.followedBy
                                        (valuePrints
                                            |> Print.listMapAndIntersperseAndFlatten
                                                (\valuePrint ->
                                                    Print.withIndentAtNextMultipleOf4
                                                        valuePrint
                                                )
                                                (Print.exactly ","
                                                    |> Print.followedBy
                                                        (Print.spaceOrLinebreakIndented fullLineSpread)
                                                )
                                        )
                                )
                            )
                        |> Print.followedBy
                            (Print.emptyOrLinebreakIndented fullLineSpread)
                        |> Print.followedBy (Print.exactly ")")
            )
        |> Print.followedBy (Print.exactly ";")


typeAliasDeclaration :
    ModuleContext
    -> Elm.Syntax.TypeAlias.TypeAlias
    ->
        Result
            String
            { name : String
            , parameters : List String
            , type_ : HaxeType
            }
typeAliasDeclaration moduleOriginLookup syntaxTypeAlias =
    Result.map
        (\aliasedType ->
            { name =
                syntaxTypeAlias.name
                    |> Elm.Syntax.Node.value
            , parameters =
                syntaxTypeAlias.generics
                    |> List.map
                        (\(Elm.Syntax.Node.Node _ parameter) ->
                            parameter |> uppercaseNameSanitizeForHaxe
                        )
            , type_ = aliasedType
            }
        )
        (syntaxTypeAlias.typeAnnotation
            |> type_ moduleOriginLookup
        )


printHaxeTypedefDeclaration :
    { name : String
    , parameters : List String
    , type_ : HaxeType
    }
    -> Print
printHaxeTypedefDeclaration haxeTypeAliasDeclaration =
    Print.exactly
        ("typedef "
            ++ haxeTypeAliasDeclaration.name
            ++ (haxeTypeAliasDeclaration.parameters
                    |> haxeTypeParametersToString
               )
            ++ " ="
        )
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy
                        (haxeTypeAliasDeclaration.type_
                            |> printHaxeTypeNotParenthesized
                        )
                )
            )
        |> Print.followedBy
            (Print.exactly ";")


type_ :
    ModuleContext
    -> Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
    -> Result String HaxeType
type_ moduleOriginLookup (Elm.Syntax.Node.Node _ syntaxType) =
    -- IGNORE TCO
    case syntaxType of
        Elm.Syntax.TypeAnnotation.Unit ->
            Ok haxeTypeUnit

        Elm.Syntax.TypeAnnotation.GenericType variable ->
            if variable |> String.startsWith "number" then
                Ok (HaxeTypeConstruct { moduleOrigin = Nothing, name = "Float", arguments = [] })

            else
                Ok (HaxeTypeVariable (variable |> uppercaseNameSanitizeForHaxe))

        Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node _ reference) typedArguments ->
            let
                ( qualification, name ) =
                    reference
            in
            case moduleOriginLookup.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup |> FastDict.get reference of
                Nothing ->
                    Err
                        ("could not find module origin of the type reference "
                            ++ qualifiedToString
                                { qualification = qualification
                                , name = name
                                }
                        )

                Just moduleOrigin ->
                    Result.map
                        (\arguments ->
                            let
                                haxeReference : { moduleOrigin : Maybe String, name : String }
                                haxeReference =
                                    case
                                        { moduleOrigin = moduleOrigin
                                        , name = name
                                        }
                                            |> referenceToCoreHaxe
                                    of
                                        Just coreHaxe ->
                                            coreHaxe

                                        Nothing ->
                                            { moduleOrigin = Nothing
                                            , name =
                                                { moduleOrigin = moduleOrigin
                                                , name = name
                                                }
                                                    |> uppercaseReferenceToHaxeName
                                            }
                            in
                            HaxeTypeConstruct
                                { moduleOrigin = haxeReference.moduleOrigin
                                , name = haxeReference.name
                                , arguments = arguments
                                }
                        )
                        (typedArguments
                            |> listMapAndCombineOk
                                (\argument -> argument |> type_ moduleOriginLookup)
                        )

        Elm.Syntax.TypeAnnotation.Tupled parts ->
            case parts of
                [] ->
                    -- should be handled by Unit
                    Ok haxeTypeUnit

                [ inParens ] ->
                    type_ moduleOriginLookup inParens

                [ tuplePart0, tuplePart1 ] ->
                    Result.map2
                        (\part0 part1 ->
                            HaxeTypeRecord
                                (FastDict.fromList
                                    [ ( "first", part0 )
                                    , ( "second", part1 )
                                    ]
                                )
                        )
                        (tuplePart0 |> type_ moduleOriginLookup)
                        (tuplePart1 |> type_ moduleOriginLookup)

                [ tuplePart0, tuplePart1, tuplePart2 ] ->
                    Result.map3
                        (\part0 part1 part2 ->
                            HaxeTypeRecord
                                (FastDict.fromList
                                    [ ( "first", part0 )
                                    , ( "second", part1 )
                                    , ( "third", part2 )
                                    ]
                                )
                        )
                        (tuplePart0 |> type_ moduleOriginLookup)
                        (tuplePart1 |> type_ moduleOriginLookup)
                        (tuplePart2 |> type_ moduleOriginLookup)

                _ :: _ :: _ :: _ :: _ ->
                    Err "too many tuple parts"

        Elm.Syntax.TypeAnnotation.Record recordFields ->
            Result.map
                (\fields ->
                    HaxeTypeRecord (FastDict.fromList fields)
                )
                (recordFields
                    |> listMapAndCombineOk
                        (\(Elm.Syntax.Node.Node _ ( Elm.Syntax.Node.Node _ name, valueNode )) ->
                            Result.map
                                (\value ->
                                    ( name |> lowercaseNameSanitizeForHaxe
                                    , value
                                    )
                                )
                                (valueNode |> type_ moduleOriginLookup)
                        )
                )

        Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation inputNode outputNode ->
            Result.map2
                (\input0 outputExpandedReverse ->
                    case outputExpandedReverse of
                        output :: inputLastTo1 ->
                            HaxeTypeFunction
                                { input = input0 :: (inputLastTo1 |> List.reverse)
                                , output = output
                                }

                        -- too lazy to make it non-empty
                        [] ->
                            input0
                )
                (inputNode |> type_ moduleOriginLookup)
                (outputNode
                    |> typeExpandFunctionOutputReverse
                    |> listMapAndCombineOk
                        (\partOfOutput ->
                            type_ moduleOriginLookup partOfOutput
                        )
                )

        Elm.Syntax.TypeAnnotation.GenericRecord _ (Elm.Syntax.Node.Node _ fields) ->
            Err
                ("extensible record types are not supported: { _ | "
                    ++ (fields
                            |> List.map
                                (\(Elm.Syntax.Node.Node _ ( Elm.Syntax.Node.Node _ name, _ )) ->
                                    name
                                )
                            |> String.join ", "
                       )
                    ++ " }"
                )


haxeTypeContainedVariables : HaxeType -> FastSet.Set String
haxeTypeContainedVariables haxeType =
    --IGNORE TCO
    case haxeType of
        HaxeTypeVariable name ->
            name |> FastSet.singleton

        HaxeTypeConstruct haxeTypeConstruct ->
            haxeTypeConstruct.arguments
                |> listMapToFastSetsAndUnify haxeTypeContainedVariables

        HaxeTypeRecord fields ->
            fields
                |> FastDict.values
                |> listMapToFastSetsAndUnify haxeTypeContainedVariables

        HaxeTypeFunction haxeTypeFunction ->
            FastSet.union
                (haxeTypeFunction.input
                    |> listMapToFastSetsAndUnify haxeTypeContainedVariables
                )
                (haxeTypeFunction.output |> haxeTypeContainedVariables)


typeExpandFunctionOutputReverse :
    Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
    -> List (Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation)
typeExpandFunctionOutputReverse typeNode =
    typeExpandFunctionOutputIntoReverse [] typeNode


typeExpandFunctionOutputIntoReverse :
    List (Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation)
    -> Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
    -> List (Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation)
typeExpandFunctionOutputIntoReverse soFarReverse (Elm.Syntax.Node.Node fullRange syntaxType) =
    case syntaxType of
        Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation inputNode outputNode ->
            typeExpandFunctionOutputIntoReverse
                (inputNode :: soFarReverse)
                outputNode

        otherType ->
            Elm.Syntax.Node.Node fullRange otherType :: soFarReverse


haxeTypeUnit : HaxeType
haxeTypeUnit =
    HaxeTypeConstruct
        { moduleOrigin = Nothing
        , name = "Unit"
        , arguments = []
        }


printHaxeTypeNotParenthesized : HaxeType -> Print
printHaxeTypeNotParenthesized haxeType =
    -- IGNORE TCO
    case haxeType of
        HaxeTypeVariable variable ->
            Print.exactly variable

        HaxeTypeConstruct typeConstruct ->
            printHaxeTypeConstruct typeConstruct

        HaxeTypeRecord fields ->
            printHaxeTypeRecord fields

        HaxeTypeFunction typeFunction ->
            printHaxeTypeFunction typeFunction


printHaxeTypeFunction :
    { input : List HaxeType, output : HaxeType }
    -> Print
printHaxeTypeFunction typeFunction =
    let
        inputPrints : List Print
        inputPrints =
            typeFunction.input
                |> List.map printHaxeTypeNotParenthesized

        outputPrint : Print
        outputPrint =
            printHaxeTypeNotParenthesized
                typeFunction.output

        inputLineSpread : Print.LineSpread
        inputLineSpread =
            inputPrints
                |> Print.lineSpreadListMapAndCombine
                    Print.lineSpread

        fullLineSpread : Print.LineSpread
        fullLineSpread =
            inputLineSpread
                |> Print.lineSpreadMergeWith
                    (\() ->
                        outputPrint |> Print.lineSpread
                    )
    in
    Print.exactly "("
        |> Print.followedBy
            (inputPrints
                |> Print.listMapAndIntersperseAndFlatten
                    (\inputPrint -> Print.withIndentIncreasedBy 3 inputPrint)
                    (Print.exactly ","
                        |> Print.followedBy
                            (Print.spaceOrLinebreakIndented inputLineSpread)
                    )
            )
        |> Print.followedBy
            (Print.emptyOrLinebreakIndented inputLineSpread)
        |> Print.followedBy (Print.exactly ") ->")
        |> Print.followedBy
            (Print.spaceOrLinebreakIndented fullLineSpread)
        |> Print.followedBy outputPrint


printHaxeTypeRecord : FastDict.Dict String HaxeType -> Print
printHaxeTypeRecord fields =
    Print.exactly "{"
        |> Print.followedBy
            (Print.withIndentIncreasedBy 2
                (fields
                    |> FastDict.toList
                    |> Print.listMapAndIntersperseAndFlatten
                        (\( fieldName, fieldValue ) ->
                            let
                                fieldValuePrint : Print
                                fieldValuePrint =
                                    fieldValue |> printHaxeTypeNotParenthesized
                            in
                            Print.exactly (fieldName ++ ":")
                                |> Print.followedBy
                                    (Print.spaceOrLinebreakIndented
                                        (fieldValuePrint |> Print.lineSpread)
                                    )
                                |> Print.followedBy
                                    (Print.withIndentAtNextMultipleOf4
                                        fieldValuePrint
                                    )
                        )
                        (Print.exactly ","
                            |> Print.followedBy Print.linebreakIndented
                        )
                )
            )
        |> Print.followedBy Print.linebreakIndented
        |> Print.followedBy (Print.exactly "}")


printHaxeTypeConstruct :
    { moduleOrigin : Maybe String
    , name : String
    , arguments : List HaxeType
    }
    -> Print
printHaxeTypeConstruct typeConstruct =
    let
        referencePrint : Print
        referencePrint =
            Print.exactly
                (haxeReferenceToString
                    { moduleOrigin = typeConstruct.moduleOrigin
                    , name = typeConstruct.name
                    }
                )
    in
    case typeConstruct.arguments of
        [] ->
            referencePrint

        argument0 :: argument1Up ->
            let
                argumentPrints : List Print
                argumentPrints =
                    (argument0 :: argument1Up)
                        |> List.map printHaxeTypeNotParenthesized

                fullLineSpread : Print.LineSpread
                fullLineSpread =
                    argumentPrints
                        |> Print.lineSpreadListMapAndCombine Print.lineSpread
            in
            referencePrint
                |> Print.followedBy (Print.exactly "<")
                |> Print.followedBy
                    (Print.withIndentAtNextMultipleOf4
                        (Print.emptyOrLinebreakIndented fullLineSpread
                            |> Print.followedBy
                                (argumentPrints
                                    |> Print.listIntersperseAndFlatten
                                        (Print.exactly ","
                                            |> Print.followedBy
                                                (Print.spaceOrLinebreakIndented fullLineSpread)
                                        )
                                )
                        )
                    )
                |> Print.followedBy
                    (Print.emptyOrLinebreakIndented fullLineSpread)
                |> Print.followedBy
                    (Print.exactly ">")


haxeReferenceToString :
    { moduleOrigin : Maybe String
    , name : String
    }
    -> String
haxeReferenceToString reference =
    case reference.moduleOrigin of
        Nothing ->
            reference.name

        Just moduleName ->
            moduleName
                ++ "."
                ++ reference.name


qualifiedToString :
    { qualification : Elm.Syntax.ModuleName.ModuleName
    , name : String
    }
    -> String
qualifiedToString reference =
    case reference.qualification of
        [] ->
            reference.name

        qualificationPart0 :: qualificationPart1Up ->
            ((qualificationPart0 :: qualificationPart1Up)
                |> String.join "."
            )
                ++ "."
                ++ reference.name


stringFirstCharToLower : String -> String
stringFirstCharToLower string =
    case string |> String.uncons of
        Nothing ->
            ""

        Just ( headChar, tailString ) ->
            String.cons (Char.toLower headChar) tailString


stringFirstCharToUpper : String -> String
stringFirstCharToUpper string =
    case string |> String.uncons of
        Nothing ->
            ""

        Just ( headChar, tailString ) ->
            String.cons (Char.toUpper headChar) tailString


stringFirstCharIsUpper : String -> Bool
stringFirstCharIsUpper string =
    case string |> String.uncons of
        Nothing ->
            False

        Just ( headChar, _ ) ->
            headChar |> Char.isUpper


printHaxeString : String -> Print
printHaxeString stringContent =
    let
        singleDoubleQuotedStringContentEscaped : String
        singleDoubleQuotedStringContentEscaped =
            stringContent
                |> String.foldl
                    (\contentChar soFar ->
                        soFar ++ singleDoubleQuotedStringCharToEscaped contentChar ++ ""
                    )
                    ""
    in
    Print.exactly ("\"" ++ singleDoubleQuotedStringContentEscaped ++ "\"")


singleDoubleQuotedStringCharToEscaped : Char -> String
singleDoubleQuotedStringCharToEscaped character =
    case character of
        '"' ->
            "\\\""

        '\\' ->
            "\\\\"

        '\t' ->
            "\\t"

        '\n' ->
            "\\n"

        '$' ->
            "\\$"

        '\u{000D}' ->
            "\\r"

        otherCharacter ->
            if characterIsNotPrint otherCharacter then
                "\\u{" ++ characterHex otherCharacter ++ "}"

            else
                String.fromChar otherCharacter


intToHexString : Int -> String
intToHexString int =
    -- IGNORE TCO
    if int < 16 then
        unsafeHexDigitIntToString int

    else
        intToHexString (int // 16)
            ++ unsafeHexDigitIntToString (int |> Basics.remainderBy 16)
            ++ ""


unsafeHexDigitIntToString : Int -> String
unsafeHexDigitIntToString int =
    case int of
        0 ->
            "0"

        1 ->
            "1"

        2 ->
            "2"

        3 ->
            "3"

        4 ->
            "4"

        5 ->
            "5"

        6 ->
            "6"

        7 ->
            "7"

        8 ->
            "8"

        9 ->
            "9"

        10 ->
            "A"

        11 ->
            "B"

        12 ->
            "C"

        13 ->
            "D"

        14 ->
            "E"

        -- 15
        _ ->
            "F"


characterHex : Char -> String
characterHex character =
    String.toUpper
        (intToHexString (Char.toCode character)
            |> String.padLeft 8 '0'
        )


characterIsNotPrint : Char -> Bool
characterIsNotPrint character =
    if
        -- Unicode.getCategory is very expensive so we shortcut if at all possible
        charIsLatinAlphaNumOrUnderscoreFast character
            || (character == ' ')
            || (character == '.')
            || (character == '!')
            || (character == '?')
            || (character == '-')
            || (character == ':')
    then
        False

    else
        case Unicode.getCategory character of
            Nothing ->
                True

            Just category ->
                case category of
                    Unicode.SeparatorLine ->
                        True

                    Unicode.SeparatorParagraph ->
                        True

                    Unicode.OtherControl ->
                        True

                    Unicode.OtherFormat ->
                        True

                    Unicode.OtherSurrogate ->
                        True

                    Unicode.OtherPrivateUse ->
                        True

                    Unicode.OtherNotAssigned ->
                        True

                    Unicode.LetterUppercase ->
                        False

                    Unicode.LetterLowercase ->
                        False

                    Unicode.LetterTitlecase ->
                        False

                    Unicode.MarkNonSpacing ->
                        False

                    Unicode.MarkSpacingCombining ->
                        False

                    Unicode.MarkEnclosing ->
                        False

                    Unicode.NumberDecimalDigit ->
                        False

                    Unicode.NumberLetter ->
                        False

                    Unicode.NumberOther ->
                        False

                    Unicode.SeparatorSpace ->
                        True

                    Unicode.LetterModifier ->
                        False

                    Unicode.LetterOther ->
                        False

                    Unicode.PunctuationConnector ->
                        False

                    Unicode.PunctuationDash ->
                        False

                    Unicode.PunctuationOpen ->
                        False

                    Unicode.PunctuationClose ->
                        False

                    Unicode.PunctuationInitialQuote ->
                        False

                    Unicode.PunctuationFinalQuote ->
                        False

                    Unicode.PunctuationOther ->
                        False

                    Unicode.SymbolMath ->
                        False

                    Unicode.SymbolCurrency ->
                        False

                    Unicode.SymbolModifier ->
                        False

                    Unicode.SymbolOther ->
                        False


charCodeIsLower : Int -> Bool
charCodeIsLower code =
    0x61 <= code && code <= 0x7A


charCodeIsUpper : Int -> Bool
charCodeIsUpper code =
    code <= 0x5A && 0x41 <= code


charCodeIsDigit : Int -> Bool
charCodeIsDigit code =
    code <= 0x39 && 0x30 <= code


charIsLatinAlphaNumOrUnderscoreFast : Char -> Bool
charIsLatinAlphaNumOrUnderscoreFast c =
    let
        code : Int
        code =
            Char.toCode c
    in
    charCodeIsLower code
        || charCodeIsUpper code
        || charCodeIsDigit code
        || -- (c == '_')
           (code == 95)


pattern :
    ModuleContext
    -> Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
    ->
        Result
            String
            { pattern : HaxePattern
            , introducedVariables : FastSet.Set String
            }
pattern moduleOriginLookup (Elm.Syntax.Node.Node _ syntaxPattern) =
    -- IGNORE TCO
    case syntaxPattern of
        Elm.Syntax.Pattern.AllPattern ->
            Ok
                { pattern = HaxePatternIgnore
                , introducedVariables = FastSet.empty
                }

        Elm.Syntax.Pattern.UnitPattern ->
            Ok
                { pattern = HaxePatternIgnore
                , introducedVariables = FastSet.empty
                }

        Elm.Syntax.Pattern.CharPattern charValue ->
            Ok
                { pattern = HaxePatternString (charValue |> String.fromChar)
                , introducedVariables = FastSet.empty
                }

        Elm.Syntax.Pattern.StringPattern stringValue ->
            Ok
                { pattern = HaxePatternString stringValue
                , introducedVariables = FastSet.empty
                }

        Elm.Syntax.Pattern.IntPattern intValue ->
            Ok
                { pattern = HaxePatternFloat (intValue |> Basics.toFloat)
                , introducedVariables = FastSet.empty
                }

        Elm.Syntax.Pattern.HexPattern intValue ->
            Ok
                { pattern = HaxePatternFloat (intValue |> Basics.toFloat)
                , introducedVariables = FastSet.empty
                }

        Elm.Syntax.Pattern.FloatPattern _ ->
            Err "float pattern is invalid syntax"

        Elm.Syntax.Pattern.VarPattern variableName ->
            let
                sanitizedVariableName : String
                sanitizedVariableName =
                    variableName |> lowercaseNameSanitizeForHaxe
            in
            Ok
                { pattern =
                    HaxePatternVariable sanitizedVariableName
                , introducedVariables =
                    FastSet.singleton sanitizedVariableName
                }

        Elm.Syntax.Pattern.ParenthesizedPattern inParens ->
            pattern moduleOriginLookup inParens

        Elm.Syntax.Pattern.TuplePattern parts ->
            case parts of
                [] ->
                    -- should be covered by UnitPattern
                    Ok
                        { pattern = HaxePatternIgnore
                        , introducedVariables = FastSet.empty
                        }

                [ inParens ] ->
                    -- should be covered by ParenthesizedPattern
                    pattern moduleOriginLookup inParens

                [ part0Node, part1Node ] ->
                    Result.map2
                        (\part0 part1 ->
                            { pattern =
                                HaxePatternRecord
                                    (FastDict.fromList
                                        [ ( "first", part0.pattern )
                                        , ( "second", part1.pattern )
                                        ]
                                    )
                            , introducedVariables =
                                FastSet.union
                                    part0.introducedVariables
                                    part1.introducedVariables
                            }
                        )
                        (part0Node |> pattern moduleOriginLookup)
                        (part1Node |> pattern moduleOriginLookup)

                [ part0Node, part1Node, part2Node ] ->
                    Result.map3
                        (\part0 part1 part2 ->
                            { pattern =
                                HaxePatternRecord
                                    (FastDict.fromList
                                        [ ( "first", part0.pattern )
                                        , ( "second", part1.pattern )
                                        , ( "third", part2.pattern )
                                        ]
                                    )
                            , introducedVariables =
                                FastSet.union
                                    part0.introducedVariables
                                    (FastSet.union
                                        part1.introducedVariables
                                        part2.introducedVariables
                                    )
                            }
                        )
                        (part0Node |> pattern moduleOriginLookup)
                        (part1Node |> pattern moduleOriginLookup)
                        (part2Node |> pattern moduleOriginLookup)

                _ :: _ :: _ :: _ :: _ ->
                    -- invalid syntax
                    Err "too many tuple parts"

        Elm.Syntax.Pattern.RecordPattern fields ->
            let
                fieldNames : List String
                fieldNames =
                    fields
                        |> List.map
                            (\(Elm.Syntax.Node.Node _ fieldName) ->
                                fieldName |> lowercaseNameSanitizeForHaxe
                            )
            in
            Ok
                { pattern =
                    HaxePatternRecord
                        (fieldNames
                            |> listMapToFastDict
                                (\fieldName -> ( fieldName, HaxePatternVariable fieldName ))
                        )
                , introducedVariables =
                    fieldNames |> FastSet.fromList
                }

        Elm.Syntax.Pattern.UnConsPattern headPatternNode tailPatternNode ->
            resultAndThen2
                (\head tail ->
                    let
                        introducedVariables : FastSet.Set String
                        introducedVariables =
                            FastSet.union
                                head.introducedVariables
                                tail.introducedVariables
                    in
                    Ok
                        { pattern =
                            HaxePatternVariant
                                { moduleOrigin = Nothing
                                , name = "List_Cons"
                                , values = [ head.pattern, tail.pattern ]
                                }
                        , introducedVariables = introducedVariables
                        }
                )
                (headPatternNode |> pattern moduleOriginLookup)
                (tailPatternNode |> pattern moduleOriginLookup)

        Elm.Syntax.Pattern.ListPattern elementPatterns ->
            Result.map
                (\elements ->
                    { pattern =
                        elements
                            |> List.foldr
                                (\headElement tail ->
                                    HaxePatternVariant
                                        { moduleOrigin = Nothing
                                        , name = "List_Cons"
                                        , values = [ headElement.pattern, tail ]
                                        }
                                )
                                (HaxePatternVariant
                                    { moduleOrigin = Nothing
                                    , name = "List_Empty"
                                    , values = []
                                    }
                                )
                    , introducedVariables =
                        elements
                            |> listMapToFastSetsAndUnify .introducedVariables
                    }
                )
                (elementPatterns
                    |> listMapAndCombineOk
                        (\element -> element |> pattern moduleOriginLookup)
                )

        Elm.Syntax.Pattern.NamedPattern syntaxQualifiedNameRef argumentPatterns ->
            Result.map2
                (\values reference ->
                    { pattern =
                        HaxePatternVariant
                            { moduleOrigin = reference.moduleOrigin
                            , name = reference.name
                            , values = values |> List.map .pattern
                            }
                    , introducedVariables =
                        values
                            |> listMapToFastSetsAndUnify .introducedVariables
                    }
                )
                (argumentPatterns
                    |> listMapAndCombineOk
                        (\argument -> argument |> pattern moduleOriginLookup)
                )
                (case moduleOriginLookup.variantLookup |> FastDict.get ( syntaxQualifiedNameRef.moduleName, syntaxQualifiedNameRef.name ) of
                    Nothing ->
                        Err
                            ("could not find origin choice type for the variant "
                                ++ qualifiedToString
                                    { qualification = syntaxQualifiedNameRef.moduleName
                                    , name = syntaxQualifiedNameRef.name
                                    }
                            )

                    Just variantInfo ->
                        Ok
                            (case { moduleOrigin = variantInfo.moduleOrigin, name = syntaxQualifiedNameRef.name } |> referenceToCoreHaxe of
                                Just haxeReference ->
                                    haxeReference

                                Nothing ->
                                    { moduleOrigin = Nothing
                                    , name =
                                        uppercaseReferenceToHaxeName
                                            { moduleOrigin = variantInfo.moduleOrigin
                                            , name = syntaxQualifiedNameRef.name
                                            }
                                    }
                            )
                )

        Elm.Syntax.Pattern.AsPattern aliasedPatternNode (Elm.Syntax.Node.Node _ variable) ->
            Result.map
                (\aliasedPattern ->
                    let
                        variableDisambiguated : String
                        variableDisambiguated =
                            variable |> lowercaseNameSanitizeForHaxe
                    in
                    { pattern =
                        HaxePatternVariableCapture
                            { pattern = aliasedPattern.pattern
                            , variable = variableDisambiguated
                            }
                    , introducedVariables =
                        aliasedPattern.introducedVariables
                            |> FastSet.insert variableDisambiguated
                    }
                )
                (aliasedPatternNode |> pattern moduleOriginLookup)


referenceToCoreHaxe :
    { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
    , name : String
    }
    ->
        Maybe
            { moduleOrigin : Maybe String
            , name : String
            }
referenceToCoreHaxe reference =
    case reference.moduleOrigin of
        [ "Debug" ] ->
            case reference.name of
                "toString" ->
                    Just { moduleOrigin = Just "Std", name = "string" }

                "log" ->
                    Just { moduleOrigin = Nothing, name = "debug_log" }

                "todo" ->
                    Just { moduleOrigin = Nothing, name = "debug_todo" }

                _ ->
                    Nothing

        [ "Basics" ] ->
            case reference.name of
                "identity" ->
                    Just { moduleOrigin = Nothing, name = "basics_identity" }

                "always" ->
                    Just { moduleOrigin = Nothing, name = "basics_always" }

                "compare" ->
                    Just { moduleOrigin = Nothing, name = "basics_compare" }

                "max" ->
                    Just { moduleOrigin = Just "Math", name = "max" }

                "min" ->
                    Just { moduleOrigin = Just "Math", name = "min" }

                "Order" ->
                    Just { moduleOrigin = Nothing, name = "Basics_Order" }

                "LT" ->
                    Just { moduleOrigin = Nothing, name = "Basics_LT" }

                "EQ" ->
                    Just { moduleOrigin = Nothing, name = "Basics_EQ" }

                "GT" ->
                    Just { moduleOrigin = Nothing, name = "Basics_GT" }

                "Bool" ->
                    Just { moduleOrigin = Nothing, name = "Bool" }

                "True" ->
                    Just { moduleOrigin = Nothing, name = "true" }

                "False" ->
                    Just { moduleOrigin = Nothing, name = "false" }

                "not" ->
                    Just { moduleOrigin = Nothing, name = "basics_not" }

                "xor" ->
                    Just { moduleOrigin = Nothing, name = "basics_neq " }

                "Int" ->
                    Just { moduleOrigin = Nothing, name = "Float" }

                "Float" ->
                    Just { moduleOrigin = Nothing, name = "Float" }

                "ceiling" ->
                    Just { moduleOrigin = Just "Math", name = "fceil" }

                "floor" ->
                    Just { moduleOrigin = Just "Math", name = "ffloor" }

                "round" ->
                    Just { moduleOrigin = Just "Math", name = "fround" }

                "truncate" ->
                    Just { moduleOrigin = Nothing, name = "basics_truncate" }

                "negate" ->
                    Just { moduleOrigin = Nothing, name = "basics_negate" }

                "abs" ->
                    Just { moduleOrigin = Just "Math", name = "abs" }

                "toFloat" ->
                    Just { moduleOrigin = Nothing, name = "basics_identity" }

                "isNaN" ->
                    Just { moduleOrigin = Just "Math", name = "isNaN" }

                "isInfinite" ->
                    Just { moduleOrigin = Just "Math", name = "basics_isInfinite" }

                "remainderBy" ->
                    Just { moduleOrigin = Nothing, name = "basics_remainderBy" }

                "modBy" ->
                    Just { moduleOrigin = Nothing, name = "basics_modBy" }

                "sqrt" ->
                    Just { moduleOrigin = Just "Math", name = "sqrt" }

                _ ->
                    Nothing

        [ "String" ] ->
            case reference.name of
                "String" ->
                    Just { moduleOrigin = Nothing, name = "String" }

                "isEmpty" ->
                    Just { moduleOrigin = Nothing, name = "string_isEmpty" }

                "length" ->
                    Just { moduleOrigin = Nothing, name = "string_length" }

                "append" ->
                    Just { moduleOrigin = Nothing, name = "string_append" }

                "trim" ->
                    Just { moduleOrigin = Just "StringTools", name = "trim" }

                "trimLeft" ->
                    Just { moduleOrigin = Just "StringTools", name = "ltrim" }

                "trimRight" ->
                    Just { moduleOrigin = Just "StringTools", name = "rtrim" }

                "left" ->
                    Just { moduleOrigin = Nothing, name = "string_left" }

                "right" ->
                    Just { moduleOrigin = Nothing, name = "string_right" }

                "dropLeft" ->
                    Just { moduleOrigin = Nothing, name = "string_dropLeft" }

                "dropRight" ->
                    Just { moduleOrigin = Nothing, name = "string_dropRight" }

                "padLeft" ->
                    Just { moduleOrigin = Nothing, name = "string_padLeft" }

                "padRight" ->
                    Just { moduleOrigin = Nothing, name = "string_padRight" }

                "toList" ->
                    Just { moduleOrigin = Nothing, name = "string_toList" }

                "fromList" ->
                    Just { moduleOrigin = Nothing, name = "string_fromList" }

                "concat" ->
                    Just { moduleOrigin = Nothing, name = "string_concat" }

                "join" ->
                    Just { moduleOrigin = Nothing, name = "string_join" }

                "filter" ->
                    Just { moduleOrigin = Nothing, name = "string_filter" }

                "any" ->
                    Just { moduleOrigin = Nothing, name = "string_any" }

                "all" ->
                    Just { moduleOrigin = Nothing, name = "string_all" }

                "map" ->
                    Just { moduleOrigin = Nothing, name = "string_map" }

                "repeat" ->
                    Just { moduleOrigin = Nothing, name = "string_repeat" }

                "replace" ->
                    Just { moduleOrigin = Nothing, name = "string_replace" }

                "lines" ->
                    Just { moduleOrigin = Nothing, name = "string_lines" }

                "startsWith" ->
                    Just { moduleOrigin = Nothing, name = "string_startsWith" }

                "endsWith" ->
                    Just { moduleOrigin = Nothing, name = "string_endsWith" }

                "toInt" ->
                    Just { moduleOrigin = Nothing, name = "string_toInt" }

                "toFloat" ->
                    Just { moduleOrigin = Nothing, name = "string_toFloat" }

                "fromInt" ->
                    Just { moduleOrigin = Just "Std", name = "string" }

                "fromFloat" ->
                    Just { moduleOrigin = Just "Std", name = "string" }

                "fromChar" ->
                    Just { moduleOrigin = Nothing, name = "basics_identity" }

                "cons" ->
                    Just { moduleOrigin = Nothing, name = "string_append" }

                "slice" ->
                    Just { moduleOrigin = Nothing, name = "string_slice" }

                "split" ->
                    Just { moduleOrigin = Nothing, name = "string_split" }

                "contains" ->
                    Just { moduleOrigin = Nothing, name = "string_contains" }

                "reverse" ->
                    Just { moduleOrigin = Nothing, name = "string_reverse" }

                "toLower" ->
                    Just { moduleOrigin = Nothing, name = "string_toLower" }

                "toUpper" ->
                    Just { moduleOrigin = Nothing, name = "string_toUpper" }

                _ ->
                    Nothing

        [ "Char" ] ->
            -- represented as String in haxe
            case reference.name of
                "Char" ->
                    Just { moduleOrigin = Nothing, name = "String" }

                "toCode" ->
                    Just { moduleOrigin = Nothing, name = "char_toCode" }

                "fromCode" ->
                    Just { moduleOrigin = Just "String", name = "fromCharCode" }

                "toLower" ->
                    Just { moduleOrigin = Nothing, name = "string_toLower" }

                "toUpper" ->
                    Just { moduleOrigin = Nothing, name = "string_toUpper" }

                _ ->
                    Nothing

        [ "List" ] ->
            case reference.name of
                "List" ->
                    Just { moduleOrigin = Nothing, name = "List_List" }

                "singleton" ->
                    Just { moduleOrigin = Nothing, name = "list_singleton" }

                "isEmpty" ->
                    Just { moduleOrigin = Nothing, name = "list_isEmpty" }

                "length" ->
                    Just { moduleOrigin = Nothing, name = "list_length" }

                "member" ->
                    Just { moduleOrigin = Nothing, name = "list_member" }

                "minimum" ->
                    Just { moduleOrigin = Nothing, name = "list_minimum" }

                "maximum" ->
                    Just { moduleOrigin = Nothing, name = "list_maximum" }

                "sum" ->
                    Just { moduleOrigin = Nothing, name = "list_sum" }

                "product" ->
                    Just { moduleOrigin = Nothing, name = "list_product" }

                "append" ->
                    Just { moduleOrigin = Nothing, name = "list_append" }

                "concat" ->
                    Just { moduleOrigin = Nothing, name = "list_concat" }

                "reverse" ->
                    Just { moduleOrigin = Nothing, name = "list_reverse" }

                "repeat" ->
                    Just { moduleOrigin = Nothing, name = "list_repeat" }

                "all" ->
                    Just { moduleOrigin = Nothing, name = "list_all" }

                "any" ->
                    Just { moduleOrigin = Nothing, name = "list_any" }

                "filter" ->
                    Just { moduleOrigin = Nothing, name = "list_filter" }

                "filterMap" ->
                    Just { moduleOrigin = Nothing, name = "list_filterMap" }

                "map" ->
                    Just { moduleOrigin = Nothing, name = "list_map" }

                "map2" ->
                    Just { moduleOrigin = Nothing, name = "list_map2" }

                "zip" ->
                    Just { moduleOrigin = Nothing, name = "list_zip" }

                "unzip" ->
                    Just { moduleOrigin = Nothing, name = "list_unzip" }

                "concatMap" ->
                    Just { moduleOrigin = Just "list", name = "list_concatMap" }

                "sort" ->
                    Just { moduleOrigin = Nothing, name = "list_sort" }

                "sortWith" ->
                    Just { moduleOrigin = Nothing, name = "list_sortWith" }

                "range" ->
                    Just { moduleOrigin = Nothing, name = "list_range" }

                "take" ->
                    Just { moduleOrigin = Nothing, name = "list_take" }

                "drop" ->
                    Just { moduleOrigin = Nothing, name = "list_drop" }

                "intersperse" ->
                    Just { moduleOrigin = Nothing, name = "list_intersperse" }

                "foldl" ->
                    Just { moduleOrigin = Nothing, name = "list_foldl" }

                "foldr" ->
                    Just { moduleOrigin = Nothing, name = "list_foldr" }

                _ ->
                    Nothing

        [ "Maybe" ] ->
            case reference.name of
                "Maybe" ->
                    Just { moduleOrigin = Nothing, name = "Option" }

                "Nothing" ->
                    Just { moduleOrigin = Nothing, name = "None" }

                "Just" ->
                    Just { moduleOrigin = Nothing, name = "Some" }

                _ ->
                    Nothing

        [ "Parser" ] ->
            case reference.name of
                -- refers to either a type or variant
                "Problem" ->
                    Just { moduleOrigin = Nothing, name = "Parser_Problem" }

                "Expecting" ->
                    Just { moduleOrigin = Nothing, name = "Parser_Expecting" }

                "ExpectingInt" ->
                    Just { moduleOrigin = Nothing, name = "Parser_ExpectingInt" }

                "ExpectingHex" ->
                    Just { moduleOrigin = Nothing, name = "Parser_ExpectingHex" }

                "ExpectingOctal" ->
                    Just { moduleOrigin = Nothing, name = "Parser_ExpectingOctal" }

                "ExpectingBinary" ->
                    Just { moduleOrigin = Nothing, name = "Parser_ExpectingBinary" }

                "ExpectingFloat" ->
                    Just { moduleOrigin = Nothing, name = "Parser_ExpectingFloat" }

                "ExpectingNumber" ->
                    Just { moduleOrigin = Nothing, name = "Parser_ExpectingNumber" }

                "ExpectingVariable" ->
                    Just { moduleOrigin = Nothing, name = "Parser_ExpectingVariable" }

                "ExpectingSymbol" ->
                    Just { moduleOrigin = Nothing, name = "Parser_ExpectingSymbol" }

                "ExpectingKeyword" ->
                    Just { moduleOrigin = Nothing, name = "Parser_ExpectingKeyword" }

                "ExpectingEnd" ->
                    Just { moduleOrigin = Nothing, name = "Parser_ExpectingEnd" }

                "UnexpectedChar" ->
                    Just { moduleOrigin = Nothing, name = "Parser_UnexpectedChar" }

                "BadRepeat" ->
                    Just { moduleOrigin = Nothing, name = "Parser_BadRepeat" }

                _ ->
                    Nothing

        _ ->
            Nothing


lowercaseReferenceToHaxeName :
    { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
    , name : String
    }
    -> String
lowercaseReferenceToHaxeName reference =
    (reference.moduleOrigin
        |> String.concat
        |> stringFirstCharToLower
    )
        ++ "_"
        ++ reference.name
        |> lowercaseNameSanitizeForHaxe


uppercaseReferenceToHaxeName :
    { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
    , name : String
    }
    -> String
uppercaseReferenceToHaxeName reference =
    (reference.moduleOrigin
        |> String.concat
    )
        ++ (reference.name |> stringFirstCharToUpper)
        |> uppercaseNameSanitizeForHaxe


printHaxePatternNotParenthesized : HaxePattern -> Print
printHaxePatternNotParenthesized haxePattern =
    -- IGNORE TCO
    case haxePattern of
        HaxePatternIgnore ->
            Print.exactly "_"

        HaxePatternFloat floatValue ->
            Print.exactly (haxeNumberLiteralToString floatValue)

        HaxePatternString string ->
            printHaxeString string

        HaxePatternVariable name ->
            Print.exactly name

        HaxePatternVariant patternVariant ->
            Print.exactly
                (haxeReferenceToString
                    { moduleOrigin = patternVariant.moduleOrigin
                    , name = patternVariant.name
                    }
                )
                |> Print.followedBy
                    (case patternVariant.values of
                        [] ->
                            Print.empty

                        variantValue0 :: variantValue1Up ->
                            Print.exactly "("
                                |> Print.followedBy
                                    ((variantValue0 :: variantValue1Up)
                                        |> Print.listMapAndIntersperseAndFlatten
                                            printHaxePatternNotParenthesized
                                            (Print.exactly ", ")
                                    )
                                |> Print.followedBy (Print.exactly ")")
                    )

        HaxePatternVariableCapture patternAs ->
            printHaxePatternAs patternAs

        HaxePatternRecord fields ->
            Print.exactly "{"
                |> Print.followedBy
                    (fields
                        |> FastDict.toList
                        |> Print.listMapAndIntersperseAndFlatten
                            (\( fieldName, fieldValue ) ->
                                Print.exactly (fieldName ++ ": ")
                                    |> Print.followedBy
                                        (printHaxePatternNotParenthesized fieldValue)
                            )
                            (Print.exactly ", ")
                    )
                |> Print.followedBy (Print.exactly "}")


printHaxePatternAs :
    { variable : String
    , pattern : HaxePattern
    }
    -> Print
printHaxePatternAs syntaxAs =
    Print.exactly (syntaxAs.variable ++ " = ")
        |> Print.followedBy
            (printHaxePatternParenthesizedIfSpaceSeparated
                syntaxAs.pattern
            )


printHaxeExpressionRecord : FastDict.Dict String HaxeExpression -> Print
printHaxeExpressionRecord syntaxRecordFields =
    let
        fieldsPrints : List Print
        fieldsPrints =
            syntaxRecordFields
                |> FastDict.toList
                |> List.map
                    (\( fieldName, fieldValue ) ->
                        let
                            fieldValuePrint : Print
                            fieldValuePrint =
                                printHaxeExpressionNotParenthesized fieldValue
                        in
                        Print.exactly (fieldName ++ ":")
                            |> Print.followedBy
                                (Print.withIndentAtNextMultipleOf4
                                    (Print.spaceOrLinebreakIndented
                                        (fieldValuePrint |> Print.lineSpread)
                                        |> Print.followedBy fieldValuePrint
                                    )
                                )
                    )

        fullLineSpread : Print.LineSpread
        fullLineSpread =
            fieldsPrints
                |> Print.lineSpreadListMapAndCombine Print.lineSpread
    in
    Print.exactly "{"
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.emptyOrLinebreakIndented fullLineSpread
                    |> Print.followedBy
                        (fieldsPrints
                            |> Print.listIntersperseAndFlatten
                                (Print.exactly ","
                                    |> Print.followedBy
                                        (Print.spaceOrLinebreakIndented fullLineSpread)
                                )
                        )
                )
            )
        |> Print.followedBy
            (Print.emptyOrLinebreakIndented fullLineSpread)
        |> Print.followedBy (Print.exactly "}")


printParenthesized : { opening : String, closing : String, inner : Print } -> Print
printParenthesized config =
    Print.exactly config.opening
        |> Print.followedBy
            (Print.withIndentIncreasedBy 1
                config.inner
            )
        |> Print.followedBy
            (Print.emptyOrLinebreakIndented
                (config.inner |> Print.lineSpread)
            )
        |> Print.followedBy (Print.exactly config.closing)


{-| Transpile a list of [`Elm.Syntax.Declaration.Declaration`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Declaration#Declaration)s
across multiple modules to value, function and type declarations.
Declarations that use unsupported stuff like parser kernel code (directly or indirectly)
will not be present in the final declarations.
Their errors can be found alongside the valid transpiled declarations.

The given list of files must also include files from used dependencies
including `elm/core`.

-}
modules :
    List Elm.Syntax.File.File
    ->
        { errors : List String
        , declarations :
            { valuesAndFunctions :
                FastDict.Dict
                    String
                    { parameters : List (Maybe String)
                    , result : HaxeExpression
                    , type_ : Maybe HaxeType
                    }
            , typeAliases :
                FastDict.Dict
                    String
                    { parameters : List String
                    , type_ : HaxeType
                    }
            , choiceTypes :
                FastDict.Dict
                    String
                    { parameters : List String
                    , variants : FastDict.Dict String (List HaxeType)
                    }
            }
        }
modules syntaxDeclarationsIncludingOverwrittenOnes =
    let
        syntaxModules : List Elm.Syntax.File.File
        syntaxModules =
            syntaxDeclarationsIncludingOverwrittenOnes
                |> List.filter
                    (\syntaxModule ->
                        case
                            syntaxModule.moduleDefinition
                                |> Elm.Syntax.Node.value
                                |> moduleHeaderName
                        of
                            [ "Basics" ] ->
                                False

                            [ "Array" ] ->
                                False

                            -- https://github.com/elm/core/blob/1.0.5/src/Elm/JsArray.elm
                            [ "Elm", "JsArray" ] ->
                                False

                            [ "Bitwise" ] ->
                                False

                            [ "Debug" ] ->
                                False

                            [ "Char" ] ->
                                False

                            [ "String" ] ->
                                False

                            [ "List" ] ->
                                False

                            [ "Dict" ] ->
                                False

                            [ "Set" ] ->
                                False

                            [ "Platform" ] ->
                                False

                            [ "Platform", "Cmd" ] ->
                                False

                            [ "Platform", "Sub" ] ->
                                False

                            [ "Process" ] ->
                                False

                            [ "Task" ] ->
                                False

                            [ "Json", "Decode" ] ->
                                False

                            [ "Json", "Encode" ] ->
                                False

                            [ "Parser" ] ->
                                False

                            [ "Parser", "Advanced" ] ->
                                False

                            [ "Regex" ] ->
                                False

                            [ "File" ] ->
                                False

                            [ "Bytes" ] ->
                                False

                            [ "Bytes", "Encode" ] ->
                                False

                            [ "Bytes", "Decode" ] ->
                                False

                            [ "Http" ] ->
                                False

                            [ "VirtualDom" ] ->
                                False

                            [ "Browser" ] ->
                                False

                            [ "Browser", "Events" ] ->
                                False

                            [ "Browser", "Navigation" ] ->
                                False

                            [ "Browser", "Dom" ] ->
                                False

                            -- https://github.com/elm/browser/blob/master/src/Browser/AnimationManager.elm
                            [ "Browser", "AnimationManager" ] ->
                                False

                            -- https://github.com/elm/browser/tree/master/src/Debugger
                            [ "Debugger", "Expando" ] ->
                                False

                            [ "Debugger", "History" ] ->
                                False

                            [ "Debugger", "Main" ] ->
                                False

                            [ "Debugger", "Metadata" ] ->
                                False

                            [ "Debugger", "Overlay" ] ->
                                False

                            [ "Debugger", "Report" ] ->
                                False

                            [ "Html" ] ->
                                False

                            [ "Html", "Attributes" ] ->
                                False

                            [ "Html", "Events" ] ->
                                False

                            [ "Html", "Keyed" ] ->
                                False

                            [ "Html", "Lazy" ] ->
                                False

                            [ "Svg" ] ->
                                False

                            [ "Svg", "Attributes" ] ->
                                False

                            [ "Svg", "Events" ] ->
                                False

                            [ "Svg", "Keyed" ] ->
                                False

                            [ "Svg", "Lazy" ] ->
                                False

                            [ "Time" ] ->
                                False

                            [ "Random" ] ->
                                False

                            [ "Url" ] ->
                                False

                            [ "Url", "Builder" ] ->
                                False

                            [ "Url", "Parser" ] ->
                                False

                            [ "Url", "Parser", "Query" ] ->
                                False

                            [ "Markdown" ] ->
                                False

                            [ "Benchmark" ] ->
                                False

                            [ "WebGL" ] ->
                                False

                            [ "WebGL", "Settings" ] ->
                                False

                            [ "WebGL", "Settings", "Blend" ] ->
                                False

                            [ "WebGL", "Settings", "DepthTest" ] ->
                                False

                            [ "WebGL", "Settings", "StencilTest" ] ->
                                False

                            [ "WebGL", "Texture" ] ->
                                False

                            [ "Math", "Matrix4" ] ->
                                False

                            [ "Math", "Vector2" ] ->
                                False

                            [ "Math", "Vector3" ] ->
                                False

                            [ "Math", "Vector4" ] ->
                                False

                            _ ->
                                True
                    )

        moduleMembers :
            FastDict.Dict
                Elm.Syntax.ModuleName.ModuleName
                { valueOrFunctionOrTypeAliasNames : FastSet.Set String
                , enumTypesExposingVariants :
                    FastDict.Dict String (FastDict.Dict String { valueCount : Int })
                }
        moduleMembers =
            syntaxDeclarationsIncludingOverwrittenOnes
                |> List.filter
                    (\syntaxModule ->
                        -- remove those modules we don't have a replacement for, yet
                        case
                            syntaxModule.moduleDefinition
                                |> Elm.Syntax.Node.value
                                |> moduleHeaderName
                        of
                            [ "Array" ] ->
                                False

                            -- https://github.com/elm/core/blob/1.0.5/src/Elm/JsArray.elm
                            [ "Elm", "JsArray" ] ->
                                False

                            [ "Bitwise" ] ->
                                False

                            [ "Debug" ] ->
                                False

                            [ "Set" ] ->
                                False

                            [ "Platform" ] ->
                                False

                            [ "Platform", "Cmd" ] ->
                                False

                            [ "Platform", "Sub" ] ->
                                False

                            [ "Process" ] ->
                                False

                            [ "Task" ] ->
                                False

                            [ "Json", "Decode" ] ->
                                False

                            [ "Json", "Encode" ] ->
                                False

                            [ "Parser" ] ->
                                False

                            [ "Parser", "Advanced" ] ->
                                False

                            [ "Regex" ] ->
                                False

                            [ "File" ] ->
                                False

                            [ "Bytes" ] ->
                                False

                            [ "Bytes", "Encode" ] ->
                                False

                            [ "Bytes", "Decode" ] ->
                                False

                            [ "Http" ] ->
                                False

                            [ "VirtualDom" ] ->
                                False

                            [ "Browser" ] ->
                                False

                            [ "Browser", "Events" ] ->
                                False

                            [ "Browser", "Navigation" ] ->
                                False

                            [ "Browser", "Dom" ] ->
                                False

                            -- https://github.com/elm/browser/blob/master/src/Browser/AnimationManager.elm
                            [ "Browser", "AnimationManager" ] ->
                                False

                            -- https://github.com/elm/browser/tree/master/src/Debugger
                            [ "Debugger", "Expando" ] ->
                                False

                            [ "Debugger", "History" ] ->
                                False

                            [ "Debugger", "Main" ] ->
                                False

                            [ "Debugger", "Metadata" ] ->
                                False

                            [ "Debugger", "Overlay" ] ->
                                False

                            [ "Debugger", "Report" ] ->
                                False

                            [ "Html" ] ->
                                False

                            [ "Html", "Attributes" ] ->
                                False

                            [ "Html", "Events" ] ->
                                False

                            [ "Html", "Keyed" ] ->
                                False

                            [ "Html", "Lazy" ] ->
                                False

                            [ "Svg" ] ->
                                False

                            [ "Svg", "Attributes" ] ->
                                False

                            [ "Svg", "Events" ] ->
                                False

                            [ "Svg", "Keyed" ] ->
                                False

                            [ "Svg", "Lazy" ] ->
                                False

                            [ "Time" ] ->
                                False

                            [ "Random" ] ->
                                False

                            [ "Url" ] ->
                                False

                            [ "Url", "Builder" ] ->
                                False

                            [ "Url", "Parser" ] ->
                                False

                            [ "Url", "Parser", "Query" ] ->
                                False

                            [ "Markdown" ] ->
                                False

                            [ "Benchmark" ] ->
                                False

                            [ "WebGL" ] ->
                                False

                            [ "WebGL", "Settings" ] ->
                                False

                            [ "WebGL", "Settings", "Blend" ] ->
                                False

                            [ "WebGL", "Settings", "DepthTest" ] ->
                                False

                            [ "WebGL", "Settings", "StencilTest" ] ->
                                False

                            [ "WebGL", "Texture" ] ->
                                False

                            [ "Math", "Matrix4" ] ->
                                False

                            [ "Math", "Vector2" ] ->
                                False

                            [ "Math", "Vector3" ] ->
                                False

                            [ "Math", "Vector4" ] ->
                                False

                            _ ->
                                True
                    )
                |> List.foldl
                    (\syntaxModule acrossModulesSoFar ->
                        acrossModulesSoFar
                            |> FastDict.insert
                                (syntaxModule.moduleDefinition
                                    |> Elm.Syntax.Node.value
                                    |> moduleHeaderName
                                )
                                (syntaxModule.declarations
                                    |> List.foldl
                                        (\(Elm.Syntax.Node.Node _ declaration) membersSoFar ->
                                            case declaration of
                                                Elm.Syntax.Declaration.FunctionDeclaration syntaxValueOrFunctionDeclaration ->
                                                    { valueOrFunctionOrTypeAliasNames =
                                                        membersSoFar.valueOrFunctionOrTypeAliasNames
                                                            |> FastSet.insert
                                                                (syntaxValueOrFunctionDeclaration.declaration
                                                                    |> Elm.Syntax.Node.value
                                                                    |> .name
                                                                    |> Elm.Syntax.Node.value
                                                                )
                                                    , enumTypesExposingVariants =
                                                        membersSoFar.enumTypesExposingVariants
                                                    }

                                                Elm.Syntax.Declaration.CustomTypeDeclaration syntaxEnumTypeDeclaration ->
                                                    { valueOrFunctionOrTypeAliasNames =
                                                        membersSoFar.valueOrFunctionOrTypeAliasNames
                                                    , enumTypesExposingVariants =
                                                        membersSoFar.enumTypesExposingVariants
                                                            |> FastDict.insert
                                                                (syntaxEnumTypeDeclaration.name |> Elm.Syntax.Node.value)
                                                                (syntaxEnumTypeDeclaration.constructors
                                                                    |> List.foldl
                                                                        (\(Elm.Syntax.Node.Node _ variant) variantNamesSoFar ->
                                                                            variantNamesSoFar
                                                                                |> FastDict.insert
                                                                                    (variant.name
                                                                                        |> Elm.Syntax.Node.value
                                                                                    )
                                                                                    { valueCount =
                                                                                        variant.arguments |> List.length
                                                                                    }
                                                                        )
                                                                        FastDict.empty
                                                                )
                                                    }

                                                Elm.Syntax.Declaration.AliasDeclaration typeAlias ->
                                                    { valueOrFunctionOrTypeAliasNames =
                                                        membersSoFar.valueOrFunctionOrTypeAliasNames
                                                            |> FastSet.insert
                                                                (typeAlias.name
                                                                    |> Elm.Syntax.Node.value
                                                                )
                                                    , enumTypesExposingVariants =
                                                        membersSoFar.enumTypesExposingVariants
                                                    }

                                                Elm.Syntax.Declaration.PortDeclaration _ ->
                                                    -- not supported
                                                    membersSoFar

                                                Elm.Syntax.Declaration.InfixDeclaration _ ->
                                                    membersSoFar

                                                Elm.Syntax.Declaration.Destructuring _ _ ->
                                                    -- invalid syntax
                                                    membersSoFar
                                        )
                                        { valueOrFunctionOrTypeAliasNames = FastSet.empty
                                        , enumTypesExposingVariants = FastDict.empty
                                        }
                                )
                    )
                    FastDict.empty

        valuesThatNeedToBeLazilyConstructed : FastSet.Set ( Elm.Syntax.ModuleName.ModuleName, String )
        valuesThatNeedToBeLazilyConstructed =
            syntaxModules
                |> listMapToFastSetsAndUnify
                    (\syntaxModule ->
                        let
                            moduleName : Elm.Syntax.ModuleName.ModuleName
                            moduleName =
                                syntaxModule.moduleDefinition
                                    |> Elm.Syntax.Node.value
                                    |> moduleHeaderName
                        in
                        syntaxModule.declarations
                            |> List.filterMap
                                (\(Elm.Syntax.Node.Node _ declaration) ->
                                    case declaration of
                                        Elm.Syntax.Declaration.FunctionDeclaration syntaxValueOrFunctionDeclaration ->
                                            let
                                                implementation =
                                                    syntaxValueOrFunctionDeclaration.declaration |> Elm.Syntax.Node.value
                                            in
                                            case implementation.arguments of
                                                _ :: _ ->
                                                    Nothing

                                                [] ->
                                                    case syntaxValueOrFunctionDeclaration.signature of
                                                        Nothing ->
                                                            Nothing

                                                        Just (Elm.Syntax.Node.Node _ syntaxType) ->
                                                            if
                                                                syntaxType.typeAnnotation
                                                                    |> typeContainedVariables
                                                                    |> FastSet.isEmpty
                                                            then
                                                                Nothing

                                                            else
                                                                Just
                                                                    ( moduleName
                                                                    , implementation.name
                                                                        |> Elm.Syntax.Node.value
                                                                    )

                                        _ ->
                                            Nothing
                                )
                            |> FastSet.fromList
                    )

        haxeDeclarationsWithoutExtraRecordTypeAliases :
            { errors : List String
            , declarations :
                { valuesAndFunctions :
                    FastDict.Dict
                        String
                        { parameters : List (Maybe String)
                        , result : HaxeExpression
                        , type_ : Maybe HaxeType
                        }
                , typeAliases :
                    FastDict.Dict
                        String
                        { parameters : List String
                        , type_ : HaxeType
                        }
                , choiceTypes :
                    FastDict.Dict
                        String
                        { parameters : List String
                        , variants : FastDict.Dict String (List HaxeType)
                        }
                }
            }
        haxeDeclarationsWithoutExtraRecordTypeAliases =
            syntaxModules
                |> List.foldr
                    (\syntaxModule soFarAcrossModules ->
                        let
                            moduleName : Elm.Syntax.ModuleName.ModuleName
                            moduleName =
                                syntaxModule.moduleDefinition
                                    |> Elm.Syntax.Node.value
                                    |> moduleHeaderName

                            importContext :
                                { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup :
                                    FastDict.Dict
                                        ( Elm.Syntax.ModuleName.ModuleName, String )
                                        Elm.Syntax.ModuleName.ModuleName
                                , variantLookup :
                                    FastDict.Dict
                                        ( Elm.Syntax.ModuleName.ModuleName, String )
                                        { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
                                        , valueCount : Int
                                        }
                                }
                            importContext =
                                syntaxModule.imports
                                    |> importsToModuleContext moduleMembers

                            createdModuleContext : ModuleContext
                            createdModuleContext =
                                moduleContextMerge
                                    importContext
                                    (case moduleMembers |> FastDict.get moduleName of
                                        Nothing ->
                                            { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                                                FastDict.empty
                                            , variantLookup = FastDict.empty
                                            }

                                        Just moduleLocalNames ->
                                            { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                                                FastSet.union
                                                    moduleLocalNames.valueOrFunctionOrTypeAliasNames
                                                    (moduleLocalNames.enumTypesExposingVariants
                                                        |> FastDict.foldl
                                                            (\enumTypeName _ soFar ->
                                                                soFar |> FastSet.insert enumTypeName
                                                            )
                                                            FastSet.empty
                                                    )
                                                    |> FastSet.foldl
                                                        (\name soFar ->
                                                            soFar
                                                                |> FastDict.insert ( [], name )
                                                                    moduleName
                                                        )
                                                        FastDict.empty
                                            , variantLookup =
                                                moduleLocalNames.enumTypesExposingVariants
                                                    |> FastDict.foldl
                                                        (\_ variantNames soFarAcrossEnumTypes ->
                                                            variantNames
                                                                |> FastDict.foldl
                                                                    (\name info soFar ->
                                                                        soFar
                                                                            |> FastDict.insert ( [], name )
                                                                                { moduleOrigin = moduleName
                                                                                , valueCount = info.valueCount
                                                                                }
                                                                    )
                                                                    soFarAcrossEnumTypes
                                                        )
                                                        FastDict.empty
                                            }
                                    )
                        in
                        syntaxModule.declarations
                            |> List.foldr
                                (\(Elm.Syntax.Node.Node _ declaration) soFar ->
                                    case declaration of
                                        Elm.Syntax.Declaration.FunctionDeclaration syntaxValueOrFunctionDeclaration ->
                                            case
                                                syntaxValueOrFunctionDeclaration
                                                    |> valueOrFunctionDeclaration
                                                        { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                                                            createdModuleContext.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup
                                                        , variantLookup = createdModuleContext.variantLookup
                                                        , valuesThatNeedToBeLazilyConstructed = valuesThatNeedToBeLazilyConstructed
                                                        }
                                            of
                                                Ok haxeValueOrFunctionDeclaration ->
                                                    { errors = soFar.errors
                                                    , declarations =
                                                        { typeAliases = soFar.declarations.typeAliases
                                                        , choiceTypes = soFar.declarations.choiceTypes
                                                        , valuesAndFunctions =
                                                            soFar.declarations.valuesAndFunctions
                                                                |> FastDict.insert
                                                                    ({ moduleOrigin = moduleName
                                                                     , name = haxeValueOrFunctionDeclaration.name
                                                                     }
                                                                        |> lowercaseReferenceToHaxeName
                                                                    )
                                                                    { parameters = haxeValueOrFunctionDeclaration.parameters
                                                                    , result = haxeValueOrFunctionDeclaration.result
                                                                    , type_ = haxeValueOrFunctionDeclaration.type_
                                                                    }
                                                        }
                                                    }

                                                Err error ->
                                                    { declarations = soFar.declarations
                                                    , errors = error :: soFar.errors
                                                    }

                                        Elm.Syntax.Declaration.AliasDeclaration syntaxTypeAliasDeclaration ->
                                            case syntaxTypeAliasDeclaration |> typeAliasDeclaration createdModuleContext of
                                                Ok haxeTypeAliasDeclaration ->
                                                    { errors = soFar.errors
                                                    , declarations =
                                                        { valuesAndFunctions = soFar.declarations.valuesAndFunctions
                                                        , choiceTypes = soFar.declarations.choiceTypes
                                                        , typeAliases =
                                                            soFar.declarations.typeAliases
                                                                |> FastDict.insert
                                                                    ({ moduleOrigin = moduleName
                                                                     , name = haxeTypeAliasDeclaration.name
                                                                     }
                                                                        |> uppercaseReferenceToHaxeName
                                                                    )
                                                                    { parameters = haxeTypeAliasDeclaration.parameters
                                                                    , type_ = haxeTypeAliasDeclaration.type_
                                                                    }
                                                        }
                                                    }

                                                Err error ->
                                                    { declarations = soFar.declarations
                                                    , errors = error :: soFar.errors
                                                    }

                                        Elm.Syntax.Declaration.CustomTypeDeclaration syntaxEnumTypeDeclaration ->
                                            case syntaxEnumTypeDeclaration.name |> Elm.Syntax.Node.value of
                                                "Maybe" ->
                                                    soFar

                                                _ ->
                                                    case syntaxEnumTypeDeclaration |> enumTypeDeclaration createdModuleContext of
                                                        Ok haxeTypeAliasDeclaration ->
                                                            { errors = soFar.errors
                                                            , declarations =
                                                                { valuesAndFunctions = soFar.declarations.valuesAndFunctions
                                                                , typeAliases = soFar.declarations.typeAliases
                                                                , choiceTypes =
                                                                    soFar.declarations.choiceTypes
                                                                        |> FastDict.insert
                                                                            ({ moduleOrigin = moduleName
                                                                             , name = haxeTypeAliasDeclaration.name
                                                                             }
                                                                                |> uppercaseReferenceToHaxeName
                                                                            )
                                                                            { parameters = haxeTypeAliasDeclaration.parameters
                                                                            , variants =
                                                                                haxeTypeAliasDeclaration.variants
                                                                                    |> FastDict.foldl
                                                                                        (\variantName maybeValue variantsSoFar ->
                                                                                            variantsSoFar
                                                                                                |> FastDict.insert
                                                                                                    ({ moduleOrigin = moduleName
                                                                                                     , name = variantName
                                                                                                     }
                                                                                                        |> uppercaseReferenceToHaxeName
                                                                                                    )
                                                                                                    maybeValue
                                                                                        )
                                                                                        FastDict.empty
                                                                            }
                                                                }
                                                            }

                                                        Err error ->
                                                            { declarations = soFar.declarations
                                                            , errors = error :: soFar.errors
                                                            }

                                        Elm.Syntax.Declaration.PortDeclaration _ ->
                                            soFar

                                        Elm.Syntax.Declaration.InfixDeclaration _ ->
                                            soFar

                                        Elm.Syntax.Declaration.Destructuring _ _ ->
                                            soFar
                                )
                                soFarAcrossModules
                    )
                    { errors = []
                    , declarations =
                        { valuesAndFunctions = FastDict.empty
                        , typeAliases = FastDict.empty
                        , choiceTypes = FastDict.empty
                        }
                    }
    in
    { declarations =
        { valuesAndFunctions =
            haxeDeclarationsWithoutExtraRecordTypeAliases.declarations.valuesAndFunctions
                |> FastDict.map
                    (\_ valueOrFunctionInfo ->
                        { type_ = valueOrFunctionInfo.type_
                        , parameters = valueOrFunctionInfo.parameters
                        , result = valueOrFunctionInfo.result
                        }
                    )
        , choiceTypes =
            haxeDeclarationsWithoutExtraRecordTypeAliases.declarations.choiceTypes
                |> FastDict.map
                    (\_ typeAliasInfo ->
                        { parameters = typeAliasInfo.parameters
                        , variants = typeAliasInfo.variants
                        }
                    )
        , typeAliases =
            haxeDeclarationsWithoutExtraRecordTypeAliases.declarations.typeAliases
                |> FastDict.map
                    (\_ typeAliasInfo ->
                        { parameters = typeAliasInfo.parameters
                        , type_ = typeAliasInfo.type_
                        }
                    )
        }
    , errors = haxeDeclarationsWithoutExtraRecordTypeAliases.errors
    }


typeContainedVariables :
    Elm.Syntax.Node.Node
        Elm.Syntax.TypeAnnotation.TypeAnnotation
    -> FastSet.Set String
typeContainedVariables (Elm.Syntax.Node.Node _ syntaxType) =
    -- IGNORE TCO
    case syntaxType of
        Elm.Syntax.TypeAnnotation.Unit ->
            FastSet.empty

        Elm.Syntax.TypeAnnotation.GenericType variableName ->
            variableName |> FastSet.singleton

        Elm.Syntax.TypeAnnotation.Typed _ argumentNodes ->
            argumentNodes
                |> listMapToFastSetsAndUnify typeContainedVariables

        Elm.Syntax.TypeAnnotation.Tupled parts ->
            parts
                |> listMapToFastSetsAndUnify typeContainedVariables

        Elm.Syntax.TypeAnnotation.Record fields ->
            fields
                |> listMapToFastSetsAndUnify
                    (\(Elm.Syntax.Node.Node _ ( _, value )) ->
                        value |> typeContainedVariables
                    )

        Elm.Syntax.TypeAnnotation.GenericRecord (Elm.Syntax.Node.Node _ recordVariableName) (Elm.Syntax.Node.Node _ fields) ->
            FastSet.insert recordVariableName
                (fields
                    |> listMapToFastSetsAndUnify
                        (\(Elm.Syntax.Node.Node _ ( _, value )) ->
                            value |> typeContainedVariables
                        )
                )

        Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation input output ->
            FastSet.union
                (input |> typeContainedVariables)
                (output |> typeContainedVariables)


moduleHeaderName : Elm.Syntax.Module.Module -> Elm.Syntax.ModuleName.ModuleName
moduleHeaderName moduleHeader =
    case moduleHeader of
        Elm.Syntax.Module.NormalModule header ->
            header.moduleName |> Elm.Syntax.Node.value

        Elm.Syntax.Module.PortModule header ->
            header.moduleName |> Elm.Syntax.Node.value

        Elm.Syntax.Module.EffectModule header ->
            header.moduleName |> Elm.Syntax.Node.value


valueOrFunctionDeclaration :
    { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            Elm.Syntax.ModuleName.ModuleName
    , valuesThatNeedToBeLazilyConstructed :
        FastSet.Set ( Elm.Syntax.ModuleName.ModuleName, String )
    , variantLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
            , valueCount : Int
            }
    }
    -> Elm.Syntax.Expression.Function
    ->
        Result
            String
            { name : String
            , parameters : List (Maybe String)
            , result : HaxeExpression
            , type_ : Maybe HaxeType
            }
valueOrFunctionDeclaration context syntaxDeclarationValueOrFunction =
    let
        implementation : Elm.Syntax.Expression.FunctionImplementation
        implementation =
            syntaxDeclarationValueOrFunction.declaration |> Elm.Syntax.Node.value
    in
    resultAndThen2
        (\parameters maybeType ->
            Result.map
                (\result ->
                    let
                        parametersAndDestructuring :
                            { parameters : List (Maybe String)
                            , destructuring :
                                Maybe
                                    { expression : HaxeExpression
                                    , pattern : HaxePattern
                                    }
                            }
                        parametersAndDestructuring =
                            parametersToHaxeAndDestructuring
                                (parameters |> List.map .pattern)
                    in
                    { name =
                        implementation.name
                            |> Elm.Syntax.Node.value
                            |> lowercaseNameSanitizeForHaxe
                    , type_ = maybeType
                    , parameters = parametersAndDestructuring.parameters
                    , result =
                        case parametersAndDestructuring.destructuring of
                            Nothing ->
                                result

                            Just destructuring ->
                                HaxeExpressionSwitch
                                    { matched = destructuring.expression
                                    , case0 =
                                        { pattern = destructuring.pattern
                                        , result = result
                                        }
                                    , case1Up = []
                                    }
                    }
                )
                (implementation.expression
                    |> expression
                        { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                            context.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup
                        , valuesThatNeedToBeLazilyConstructed =
                            context.valuesThatNeedToBeLazilyConstructed
                        , variantLookup = context.variantLookup
                        , variablesFromWithinDeclarationInScope =
                            parameters
                                |> listMapToFastSetsAndUnify .introducedVariables
                        }
                )
        )
        (implementation.arguments
            |> listMapAndCombineOk
                (\p ->
                    p
                        |> pattern
                            { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                                context.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup
                            , variantLookup = context.variantLookup
                            }
                )
        )
        (case syntaxDeclarationValueOrFunction.signature of
            Nothing ->
                Ok Nothing

            Just (Elm.Syntax.Node.Node _ signature) ->
                Result.map Just
                    (signature.typeAnnotation
                        |> type_
                            { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                                context.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup
                            , variantLookup = context.variantLookup
                            }
                    )
        )


lowercaseNameSanitizeForHaxe : String -> String
lowercaseNameSanitizeForHaxe lowercaseName =
    let
        lowercaseNameWithValidCharacters : String
        lowercaseNameWithValidCharacters =
            lowercaseName
                |> stringFirstCharToLower
    in
    if haxeReservedWords |> FastSet.member lowercaseNameWithValidCharacters then
        lowercaseNameWithValidCharacters ++ "_"

    else
        lowercaseNameWithValidCharacters


uppercaseNameSanitizeForHaxe : String -> String
uppercaseNameSanitizeForHaxe uppercaseName =
    let
        uppercaseNameWithValidCharacters : String
        uppercaseNameWithValidCharacters =
            uppercaseName
                |> stringFirstCharToUpper
    in
    if haxeReservedWords |> FastSet.member uppercaseNameWithValidCharacters then
        uppercaseNameWithValidCharacters ++ "_"

    else
        uppercaseNameWithValidCharacters


haxeReservedWords : FastSet.Set String
haxeReservedWords =
    -- https://haxe.org/manual/expression.html#keywords
    FastSet.fromList
        [ "abstract"
        , "break"
        , "case"
        , "cast"
        , "catch"
        , "class"
        , "continue"
        , "default"
        , "do"
        , "dynamic"
        , "else"
        , "enum"
        , "extends"
        , "extern"
        , "false"
        , "final"
        , "for"
        , "function"
        , "if"
        , "implements"
        , "import"
        , "in"
        , "inline"
        , "interface"
        , "macro"
        , "new"
        , "null"
        , "operator"
        , "overload"
        , "override"
        , "package"
        , "private"
        , "public"
        , "return"
        , "static"
        , "switch"
        , "this"
        , "throw"
        , "true"
        , "try"
        , "typedef"
        , "untyped"
        , "using"
        , "var"
        , "while"
        ]


expressionContextAddVariablesInScope :
    FastSet.Set String
    ->
        { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup :
            FastDict.Dict
                ( Elm.Syntax.ModuleName.ModuleName, String )
                Elm.Syntax.ModuleName.ModuleName
        , valuesThatNeedToBeLazilyConstructed :
            FastSet.Set ( Elm.Syntax.ModuleName.ModuleName, String )
        , variantLookup :
            FastDict.Dict
                ( Elm.Syntax.ModuleName.ModuleName, String )
                { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
                , valueCount : Int
                }
        , variablesFromWithinDeclarationInScope : FastSet.Set String
        }
    ->
        { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup :
            FastDict.Dict
                ( Elm.Syntax.ModuleName.ModuleName, String )
                Elm.Syntax.ModuleName.ModuleName
        , valuesThatNeedToBeLazilyConstructed :
            FastSet.Set ( Elm.Syntax.ModuleName.ModuleName, String )
        , variantLookup :
            FastDict.Dict
                ( Elm.Syntax.ModuleName.ModuleName, String )
                { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
                , valueCount : Int
                }
        , variablesFromWithinDeclarationInScope : FastSet.Set String
        }
expressionContextAddVariablesInScope additionalVariablesInScope context =
    { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
        context.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup
    , valuesThatNeedToBeLazilyConstructed =
        context.valuesThatNeedToBeLazilyConstructed
    , variantLookup =
        context.variantLookup
    , variablesFromWithinDeclarationInScope =
        FastSet.union
            additionalVariablesInScope
            context.variablesFromWithinDeclarationInScope
    }


expression :
    { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            Elm.Syntax.ModuleName.ModuleName
    , valuesThatNeedToBeLazilyConstructed :
        FastSet.Set ( Elm.Syntax.ModuleName.ModuleName, String )
    , variantLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
            , valueCount : Int
            }
    , variablesFromWithinDeclarationInScope : FastSet.Set String
    }
    -> Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
    -> Result String HaxeExpression
expression context (Elm.Syntax.Node.Node _ syntaxExpression) =
    -- IGNORE TCO
    case syntaxExpression of
        Elm.Syntax.Expression.UnitExpr ->
            Ok haxeExpressionUnit

        Elm.Syntax.Expression.Integer intValue ->
            Ok (HaxeExpressionFloat (intValue |> Basics.toFloat))

        Elm.Syntax.Expression.Hex intValue ->
            Ok (HaxeExpressionFloat (intValue |> Basics.toFloat))

        Elm.Syntax.Expression.Floatable floatValue ->
            Ok (HaxeExpressionFloat floatValue)

        Elm.Syntax.Expression.CharLiteral charValue ->
            Ok (HaxeExpressionString (charValue |> String.fromChar))

        Elm.Syntax.Expression.Literal stringValue ->
            Ok (HaxeExpressionString stringValue)

        Elm.Syntax.Expression.RecordAccessFunction fieldName ->
            let
                recordVariableName : String
                recordVariableName =
                    "generated_record"
            in
            Ok
                (HaxeExpressionLambda
                    { parameter0 = Just recordVariableName
                    , parameter1Up = []
                    , result =
                        HaxeExpressionRecordAccess
                            { record =
                                HaxeExpressionReference
                                    { moduleOrigin = Nothing
                                    , name = recordVariableName
                                    }
                            , field =
                                fieldName
                                    |> String.replace "." ""
                                    |> lowercaseNameSanitizeForHaxe
                            }
                    }
                )

        Elm.Syntax.Expression.Operator _ ->
            -- invalid syntax
            Err "operator is invalid syntax"

        Elm.Syntax.Expression.PrefixOperator operatorSymbol ->
            Result.map
                (\operationFunctionReference ->
                    HaxeExpressionReference operationFunctionReference
                )
                (expressionOperatorToHaxeFunctionReference operatorSymbol)

        Elm.Syntax.Expression.GLSLExpression _ ->
            Err "glsl not supported"

        Elm.Syntax.Expression.Application application ->
            case application of
                [] ->
                    Err "application without any parts is invalid"

                [ inParens ] ->
                    -- invalid syntax
                    expression context inParens

                calledNode :: argument0Node :: argument1UpNodes ->
                    Result.map3
                        (\called argument0 argument1Up ->
                            condenseExpressionCall
                                { called = called
                                , argument0 = argument0
                                , argument1Up = argument1Up
                                }
                        )
                        (calledNode |> expression context)
                        (argument0Node |> expression context)
                        (argument1UpNodes
                            |> listMapAndCombineOk
                                (\argument -> argument |> expression context)
                        )

        Elm.Syntax.Expression.OperatorApplication operatorSymbol _ leftNode rightNode ->
            case operatorSymbol of
                "|>" ->
                    Result.map2
                        (\argument called ->
                            condenseExpressionCall
                                { called = called
                                , argument0 = argument
                                , argument1Up = []
                                }
                        )
                        (leftNode |> expression context)
                        (rightNode |> expression context)

                "<|" ->
                    Result.map2
                        (\called argument ->
                            condenseExpressionCall
                                { called = called
                                , argument0 = argument
                                , argument1Up = []
                                }
                        )
                        (leftNode |> expression context)
                        (rightNode |> expression context)

                "++" ->
                    Result.map2
                        (\left right ->
                            if
                                (left |> haxeExpressionIsDefinitelyOfTypeString)
                                    || (right |> haxeExpressionIsDefinitelyOfTypeString)
                            then
                                HaxeExpressionCall
                                    { called =
                                        HaxeExpressionReference
                                            { moduleOrigin = Nothing
                                            , name = "string_append"
                                            }
                                    , arguments = [ left, right ]
                                    }

                            else
                                HaxeExpressionCall
                                    { called =
                                        HaxeExpressionReference
                                            { moduleOrigin = Nothing
                                            , name = "list_append"
                                            }
                                    , arguments = [ left, right ]
                                    }
                        )
                        (leftNode |> expression context)
                        (rightNode |> expression context)

                otherOperatorSymbol ->
                    Result.map3
                        (\operationFunctionReference left right ->
                            HaxeExpressionCall
                                { called =
                                    HaxeExpressionReference operationFunctionReference
                                , arguments = [ left, right ]
                                }
                        )
                        (expressionOperatorToHaxeFunctionReference otherOperatorSymbol)
                        (leftNode |> expression context)
                        (rightNode |> expression context)

        Elm.Syntax.Expression.FunctionOrValue qualification name ->
            let
                asVariableFromWithinDeclaration : Maybe String
                asVariableFromWithinDeclaration =
                    case qualification of
                        _ :: _ ->
                            Nothing

                        [] ->
                            let
                                haxeName : String
                                haxeName =
                                    name |> lowercaseNameSanitizeForHaxe
                            in
                            if
                                context.variablesFromWithinDeclarationInScope
                                    |> FastSet.member haxeName
                            then
                                Just haxeName

                            else
                                Nothing
            in
            case asVariableFromWithinDeclaration of
                Just variableFromWithinDeclaration ->
                    Ok
                        (HaxeExpressionReference
                            { moduleOrigin = Nothing
                            , name = variableFromWithinDeclaration
                            }
                        )

                Nothing ->
                    case context.variantLookup |> FastDict.get ( qualification, name ) of
                        Just variantInfo ->
                            let
                                reference : { moduleOrigin : Maybe String, name : String }
                                reference =
                                    case { moduleOrigin = variantInfo.moduleOrigin, name = name } |> referenceToCoreHaxe of
                                        Just haxeReference ->
                                            haxeReference

                                        Nothing ->
                                            { moduleOrigin = Nothing
                                            , name =
                                                uppercaseReferenceToHaxeName
                                                    { moduleOrigin = variantInfo.moduleOrigin
                                                    , name = name
                                                    }
                                            }
                            in
                            Ok
                                (case variantInfo.valueCount of
                                    0 ->
                                        HaxeExpressionReference reference

                                    1 ->
                                        HaxeExpressionReference reference

                                    valueCountAtLeast2 ->
                                        let
                                            generatedValueVariableReference : Int -> HaxeExpression
                                            generatedValueVariableReference valueIndex =
                                                HaxeExpressionReference
                                                    { moduleOrigin = Nothing
                                                    , name =
                                                        "generated_"
                                                            ++ (valueIndex |> String.fromInt)
                                                    }

                                            generatedValuePattern : Int -> Maybe String
                                            generatedValuePattern valueIndex =
                                                Just
                                                    ("generated_"
                                                        ++ (valueIndex |> String.fromInt)
                                                    )
                                        in
                                        HaxeExpressionLambda
                                            { parameter0 = generatedValuePattern 0
                                            , parameter1Up =
                                                generatedValuePattern 1
                                                    :: (List.range 2 (valueCountAtLeast2 - 1)
                                                            |> List.map generatedValuePattern
                                                       )
                                            , result =
                                                HaxeExpressionCall
                                                    { called = HaxeExpressionReference reference
                                                    , arguments =
                                                        generatedValueVariableReference 0
                                                            :: generatedValueVariableReference 1
                                                            :: (List.range 2 (valueCountAtLeast2 - 1)
                                                                    |> List.map generatedValueVariableReference
                                                               )
                                                    }
                                            }
                                )

                        -- not a variant
                        Nothing ->
                            case context.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup |> FastDict.get ( qualification, name ) of
                                Just moduleOrigin ->
                                    Ok
                                        (case { moduleOrigin = moduleOrigin, name = name } |> referenceToCoreHaxe of
                                            Just haxeReference ->
                                                HaxeExpressionReference haxeReference

                                            Nothing ->
                                                -- TODO should be redundant because variant check
                                                if name |> stringFirstCharIsUpper then
                                                    HaxeExpressionReference
                                                        { moduleOrigin = Nothing
                                                        , name =
                                                            uppercaseReferenceToHaxeName
                                                                { moduleOrigin = moduleOrigin
                                                                , name = name
                                                                }
                                                        }

                                                else if
                                                    context.valuesThatNeedToBeLazilyConstructed
                                                        |> FastSet.member
                                                            ( moduleOrigin, name )
                                                then
                                                    HaxeExpressionCall
                                                        { called =
                                                            HaxeExpressionReference
                                                                { moduleOrigin = Nothing
                                                                , name =
                                                                    lowercaseReferenceToHaxeName
                                                                        { moduleOrigin = moduleOrigin
                                                                        , name = name
                                                                        }
                                                                }
                                                        , arguments = []
                                                        }

                                                else
                                                    HaxeExpressionReference
                                                        { moduleOrigin = Nothing
                                                        , name =
                                                            lowercaseReferenceToHaxeName
                                                                { moduleOrigin = moduleOrigin
                                                                , name = name
                                                                }
                                                        }
                                        )

                                -- not a reference that was declared in elm
                                Nothing ->
                                    case qualification of
                                        qualificationPart0 :: qualificationPart1Up ->
                                            Err
                                                ("could not find module origin of the qualified reference "
                                                    ++ (((qualificationPart0 :: qualificationPart1Up) |> String.join ".")
                                                            ++ "."
                                                            ++ name
                                                       )
                                                )

                                        [] ->
                                            -- TODO convert to error
                                            Ok
                                                (HaxeExpressionReference
                                                    { moduleOrigin = Nothing
                                                    , name = name |> lowercaseNameSanitizeForHaxe
                                                    }
                                                )

        Elm.Syntax.Expression.IfBlock conditionNode onTrueNode onFalseNode ->
            Result.map3
                (\condition onTrue onFalse ->
                    HaxeExpressionSwitch
                        { matched = condition
                        , case0 = { pattern = haxePatternTrue, result = onTrue }
                        , case1Up = [ { pattern = haxePatternFalse, result = onFalse } ]
                        }
                )
                (conditionNode |> expression context)
                (onTrueNode |> expression context)
                (onFalseNode |> expression context)

        Elm.Syntax.Expression.ParenthesizedExpression inParens ->
            inParens |> expression context

        Elm.Syntax.Expression.Negation inNegationNode ->
            Result.map
                (\inNegation ->
                    HaxeExpressionCall
                        { called =
                            HaxeExpressionReference
                                { moduleOrigin = Nothing, name = "basics_negate" }
                        , arguments = [ inNegation ]
                        }
                )
                (inNegationNode |> expression context)

        Elm.Syntax.Expression.RecordAccess recordNode (Elm.Syntax.Node.Node _ fieldName) ->
            Result.map
                (\record ->
                    HaxeExpressionRecordAccess
                        { record = record
                        , field =
                            fieldName
                                |> String.replace "." ""
                                |> lowercaseNameSanitizeForHaxe
                        }
                )
                (recordNode |> expression context)

        Elm.Syntax.Expression.TupledExpression parts ->
            case parts of
                [] ->
                    -- invalid syntax
                    -- should be handled by Elm.Syntax.Expression.UnitExpr
                    Ok haxeExpressionUnit

                [ inParens ] ->
                    -- invalid syntax
                    -- should be handled by Elm.Syntax.Expression.ParenthesizedExpression
                    expression context inParens

                [ part0Node, part1Node ] ->
                    Result.map2
                        (\part0 part1 ->
                            HaxeExpressionRecord
                                (FastDict.fromList
                                    [ ( "first", part0 )
                                    , ( "second", part1 )
                                    ]
                                )
                        )
                        (part0Node |> expression context)
                        (part1Node |> expression context)

                [ part0Node, part1Node, part2Node ] ->
                    Result.map3
                        (\part0 part1 part2 ->
                            HaxeExpressionRecord
                                (FastDict.fromList
                                    [ ( "first", part0 )
                                    , ( "second", part1 )
                                    , ( "third", part2 )
                                    ]
                                )
                        )
                        (part0Node |> expression context)
                        (part1Node |> expression context)
                        (part2Node |> expression context)

                _ :: _ :: _ :: _ :: _ ->
                    Err "too many tuple parts"

        Elm.Syntax.Expression.ListExpr elementNodes ->
            Result.map (\elements -> HaxeExpressionList elements)
                (elementNodes
                    |> listMapAndCombineOk
                        (\element -> element |> expression context)
                )

        Elm.Syntax.Expression.RecordExpr fieldNodes ->
            Result.map (\fields -> HaxeExpressionRecord fields)
                (fieldNodes
                    |> listMapAndCombineOk
                        (\(Elm.Syntax.Node.Node _ ( Elm.Syntax.Node.Node _ fieldName, fieldValueNode )) ->
                            Result.map
                                (\fieldValue ->
                                    ( fieldName
                                        |> lowercaseNameSanitizeForHaxe
                                    , fieldValue
                                    )
                                )
                                (fieldValueNode |> expression context)
                        )
                    |> Result.map FastDict.fromList
                )

        Elm.Syntax.Expression.RecordUpdateExpression _ _ ->
            Err "record update not supported"

        Elm.Syntax.Expression.LambdaExpression lambda ->
            case lambda.args of
                [] ->
                    Err "lambda without parameters is invalid syntax"

                parameter0Node :: parameter1UpNodes ->
                    lambdaExpression context
                        { result = lambda.expression
                        , parameter0 = parameter0Node
                        , parameter1Up = parameter1UpNodes
                        }

        Elm.Syntax.Expression.CaseExpression caseOf ->
            case caseOf.cases of
                [] ->
                    Err "case-of without cases invalid syntax"

                case0Node :: case1Node ->
                    Result.map3
                        (\matched case0 case1Up ->
                            HaxeExpressionSwitch
                                { matched = matched
                                , case0 = case0
                                , case1Up = case1Up
                                }
                        )
                        (caseOf.expression |> expression context)
                        (case0Node |> case_ context)
                        (case1Node
                            |> listMapAndCombineOk
                                (\parameter ->
                                    parameter |> case_ context
                                )
                        )

        Elm.Syntax.Expression.LetExpression letIn ->
            case letIn.declarations of
                [] ->
                    Err "let-in without declarations is invalid syntax"

                declaration0Node :: declaration1UpNode ->
                    expressionWithLocalDeclarations context
                        { declaration0Node = declaration0Node
                        , declaration1UpNode = declaration1UpNode
                        , expression = letIn.expression
                        }


parametersToHaxeAndDestructuring :
    List HaxePattern
    ->
        { parameters : List (Maybe String)
        , destructuring :
            Maybe
                { expression : HaxeExpression
                , pattern : HaxePattern
                }
        }
parametersToHaxeAndDestructuring parameterPatterns =
    let
        parametersAndDestructurings :
            { parameters : List (Maybe String)
            , destructurings : FastDict.Dict String HaxePattern
            }
        parametersAndDestructurings =
            parameterPatterns
                |> List.foldr
                    (\parameterPattern soFar ->
                        case parameterPattern of
                            HaxePatternVariable variableName ->
                                { parameters =
                                    Just variableName :: soFar.parameters
                                , destructurings = soFar.destructurings
                                }

                            parameterPatternNotVariable ->
                                case parameterPatternNotVariable |> haxePatternIntroducedVariables of
                                    [] ->
                                        { parameters =
                                            Nothing :: soFar.parameters
                                        , destructurings = soFar.destructurings
                                        }

                                    parameterPatternIntroducedVariable0 :: parameterPatternIntroducedVariable1Up ->
                                        let
                                            generatedVariableName : String
                                            generatedVariableName =
                                                "generated_"
                                                    ++ ((parameterPatternIntroducedVariable0 :: parameterPatternIntroducedVariable1Up)
                                                            |> String.join "_"
                                                       )
                                        in
                                        { parameters =
                                            Just generatedVariableName
                                                :: soFar.parameters
                                        , destructurings =
                                            soFar.destructurings
                                                |> FastDict.insert generatedVariableName
                                                    parameterPatternNotVariable
                                        }
                    )
                    { parameters = []
                    , destructurings = FastDict.empty
                    }
    in
    { parameters = parametersAndDestructurings.parameters
    , destructuring =
        if parametersAndDestructurings.destructurings |> FastDict.isEmpty then
            Nothing

        else
            Just
                { expression =
                    HaxeExpressionRecord
                        (parametersAndDestructurings.destructurings
                            |> FastDict.keys
                            |> List.map
                                (\generatedVariableName ->
                                    ( generatedVariableName
                                    , HaxeExpressionReference
                                        { moduleOrigin = Nothing
                                        , name = generatedVariableName
                                        }
                                    )
                                )
                            |> FastDict.fromList
                        )
                , pattern =
                    HaxePatternRecord
                        parametersAndDestructurings.destructurings
                }
    }


lambdaExpression :
    { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            Elm.Syntax.ModuleName.ModuleName
    , valuesThatNeedToBeLazilyConstructed :
        FastSet.Set ( Elm.Syntax.ModuleName.ModuleName, String )
    , variantLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
            , valueCount : Int
            }
    , variablesFromWithinDeclarationInScope : FastSet.Set String
    }
    ->
        { result : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
        , parameter0 : Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
        , parameter1Up : List (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern)
        }
    -> Result String HaxeExpression
lambdaExpression context lambda =
    resultAndThen2
        (\parameterPattern0 parameterPattern1Up ->
            Result.map
                (\result ->
                    let
                        parametersAndDestructuring :
                            { parameters : List (Maybe String)
                            , destructuring :
                                Maybe
                                    { expression : HaxeExpression
                                    , pattern : HaxePattern
                                    }
                            }
                        parametersAndDestructuring =
                            parametersToHaxeAndDestructuring
                                (parameterPattern0.pattern
                                    :: (parameterPattern1Up |> List.map .pattern)
                                )
                    in
                    case parametersAndDestructuring.parameters of
                        [] ->
                            HaxeExpressionReference
                                { moduleOrigin = Nothing, name = "bugIn_parametersToHaxeAndDestructuring" }

                        parameter0 :: parameter1Up ->
                            HaxeExpressionLambda
                                { parameter0 = parameter0
                                , parameter1Up = parameter1Up
                                , result =
                                    case parametersAndDestructuring.destructuring of
                                        Nothing ->
                                            result

                                        Just destructuring ->
                                            HaxeExpressionSwitch
                                                { matched = destructuring.expression
                                                , case0 =
                                                    { pattern = destructuring.pattern
                                                    , result = result
                                                    }
                                                , case1Up = []
                                                }
                                }
                )
                (lambda.result
                    |> expression
                        (context
                            |> expressionContextAddVariablesInScope
                                (FastSet.union
                                    parameterPattern0.introducedVariables
                                    (parameterPattern1Up
                                        |> listMapToFastSetsAndUnify .introducedVariables
                                    )
                                )
                        )
                )
        )
        (lambda.parameter0
            |> pattern
                { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                    context.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup
                , variantLookup = context.variantLookup
                }
        )
        (lambda.parameter1Up
            |> listMapAndCombineOk
                (\parameter ->
                    parameter
                        |> pattern
                            { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                                context.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup
                            , variantLookup = context.variantLookup
                            }
                )
        )


expressionWithLocalDeclarations :
    { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            Elm.Syntax.ModuleName.ModuleName
    , valuesThatNeedToBeLazilyConstructed :
        FastSet.Set ( Elm.Syntax.ModuleName.ModuleName, String )
    , variantLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
            , valueCount : Int
            }
    , variablesFromWithinDeclarationInScope : FastSet.Set String
    }
    ->
        { declaration0Node : Elm.Syntax.Node.Node Elm.Syntax.Expression.LetDeclaration
        , declaration1UpNode : List (Elm.Syntax.Node.Node Elm.Syntax.Expression.LetDeclaration)
        , expression : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
        }
    -> Result String HaxeExpression
expressionWithLocalDeclarations context letIn =
    let
        variablesForWholeLetIn : FastSet.Set String
        variablesForWholeLetIn =
            (letIn.declaration0Node :: letIn.declaration1UpNode)
                |> listMapToFastSetsAndUnify
                    (\(Elm.Syntax.Node.Node _ syntaxLetDeclaration) ->
                        case syntaxLetDeclaration of
                            Elm.Syntax.Expression.LetFunction letFunction ->
                                FastSet.singleton
                                    (letFunction.declaration
                                        |> Elm.Syntax.Node.value
                                        |> .name
                                        |> Elm.Syntax.Node.value
                                        |> lowercaseNameSanitizeForHaxe
                                    )

                            Elm.Syntax.Expression.LetDestructuring patternNode _ ->
                                patternNode
                                    |> patternBindings
                                    |> listMapAndToFastSet
                                        lowercaseNameSanitizeForHaxe
                    )
    in
    Result.map3
        (\declaration0 declaration1Up result ->
            let
                valueOrFunctionDeclarations :
                    List
                        { name : String
                        , parameters : List (Maybe String)
                        , result : HaxeExpression
                        , type_ : Maybe HaxeType
                        }
                valueOrFunctionDeclarations =
                    (declaration0 :: declaration1Up)
                        |> List.filterMap
                            (\declaration ->
                                case declaration of
                                    HaxeLetDestructuring _ ->
                                        Nothing

                                    HaxeLetDeclarationValueOrFunction haxeLetValueOrFunction ->
                                        Just haxeLetValueOrFunction
                            )

                destructuringDeclarations :
                    List
                        { pattern : HaxePattern
                        , expression : HaxeExpression
                        }
                destructuringDeclarations =
                    (declaration0 :: declaration1Up)
                        |> List.filterMap
                            (\declaration ->
                                case declaration of
                                    HaxeLetDeclarationValueOrFunction _ ->
                                        Nothing

                                    HaxeLetDestructuring haxeLetDestructuring ->
                                        Just haxeLetDestructuring
                            )

                valueAndFunctionDeclarationsMostToLeastDependedUpon :
                    List
                        (Data.Graph.SCC
                            { parameters : List (Maybe String)
                            , type_ : Maybe HaxeType
                            , name : String
                            , result : HaxeExpression
                            }
                        )
                valueAndFunctionDeclarationsMostToLeastDependedUpon =
                    valueOrFunctionDeclarations
                        |> List.map
                            (\declaration ->
                                ( declaration
                                , declaration.name
                                , declaration.result
                                    |> haxeExpressionContainedLocalReferences
                                    |> FastSet.toList
                                )
                            )
                        |> Data.Graph.stronglyConnComp
            in
            includeDestructuringsIntoHaxeValueAndFunctionDeclarations
                { valueAndFunctionDeclarationsMostToLeastDependedUpon =
                    valueAndFunctionDeclarationsMostToLeastDependedUpon
                , destructuringDeclarationsMostToLeastDependedUpon =
                    destructuringDeclarations
                        |> destructuringDeclarationsSortMostToLeastDependedUpon
                , result = result
                }
        )
        (letIn.declaration0Node
            |> letDeclaration
                (context
                    |> expressionContextAddVariablesInScope
                        variablesForWholeLetIn
                )
        )
        (letIn.declaration1UpNode
            |> listMapAndCombineOk
                (\letDecl ->
                    letDecl
                        |> letDeclaration
                            (context
                                |> expressionContextAddVariablesInScope
                                    variablesForWholeLetIn
                            )
                )
        )
        (letIn.expression
            |> expression
                (context
                    |> expressionContextAddVariablesInScope
                        variablesForWholeLetIn
                )
        )


destructuringDeclarationsSortMostToLeastDependedUpon :
    List
        { pattern : HaxePattern
        , expression : HaxeExpression
        }
    ->
        List
            { pattern : HaxePattern
            , expression : HaxeExpression
            }
destructuringDeclarationsSortMostToLeastDependedUpon destructuringDeclarations =
    -- TODO
    destructuringDeclarations


includeDestructuringsIntoHaxeValueAndFunctionDeclarations :
    { destructuringDeclarationsMostToLeastDependedUpon :
        List
            { pattern : HaxePattern
            , expression : HaxeExpression
            }
    , valueAndFunctionDeclarationsMostToLeastDependedUpon :
        List
            (Data.Graph.SCC
                { name : String
                , parameters : List (Maybe String)
                , result : HaxeExpression
                , type_ : Maybe HaxeType
                }
            )
    , result : HaxeExpression
    }
    -> HaxeExpression
includeDestructuringsIntoHaxeValueAndFunctionDeclarations state =
    case state.destructuringDeclarationsMostToLeastDependedUpon of
        [] ->
            state.valueAndFunctionDeclarationsMostToLeastDependedUpon
                |> List.concatMap
                    (\group ->
                        case group of
                            Data.Graph.CyclicSCC cycleMembers ->
                                cycleMembers

                            Data.Graph.AcyclicSCC member ->
                                [ member ]
                    )
                |> List.foldr
                    (\declaration soFar ->
                        HaxeExpressionWithLocalDeclaration
                            { declaration = declaration
                            , result = soFar
                            }
                    )
                    state.result

        mostDependedUponDestructuringDeclaration :: destructuringDeclarationsWithoutMostDependedUpon ->
            case state.valueAndFunctionDeclarationsMostToLeastDependedUpon of
                [] ->
                    -- apply all destructurings as one switch
                    HaxeExpressionSwitch
                        { matched = mostDependedUponDestructuringDeclaration.expression
                        , case0 =
                            { pattern = mostDependedUponDestructuringDeclaration.pattern
                            , result = state.result
                            }
                        , case1Up = []
                        }

                mostDependedUponGroup :: remainingWithoutGroup ->
                    includeDestructuringsIntoHaxeValueAndFunctionDeclarations
                        (-- TODO avoid duplicate haxeExpressionContainedLocalReferences
                         let
                            destructuringIntroducedVariables : FastSet.Set String
                            destructuringIntroducedVariables =
                                mostDependedUponDestructuringDeclaration.pattern
                                    |> haxePatternIntroducedVariables
                                    |> FastSet.fromList

                            groupLocalReferences : FastSet.Set String
                            groupLocalReferences =
                                case mostDependedUponGroup of
                                    Data.Graph.CyclicSCC cycleMembers ->
                                        cycleMembers
                                            |> listMapToFastSetsAndUnify
                                                (\declaration ->
                                                    declaration.result
                                                        |> haxeExpressionContainedLocalReferences
                                                )

                                    Data.Graph.AcyclicSCC member ->
                                        member.result |> haxeExpressionContainedLocalReferences
                         in
                         if
                            fastSetsIntersect
                                destructuringIntroducedVariables
                                groupLocalReferences
                         then
                            { valueAndFunctionDeclarationsMostToLeastDependedUpon =
                                mostDependedUponGroup :: remainingWithoutGroup
                            , destructuringDeclarationsMostToLeastDependedUpon =
                                destructuringDeclarationsWithoutMostDependedUpon
                            , result =
                                HaxeExpressionSwitch
                                    { matched = mostDependedUponDestructuringDeclaration.expression
                                    , case0 =
                                        { pattern = mostDependedUponDestructuringDeclaration.pattern
                                        , result = state.result
                                        }
                                    , case1Up = []
                                    }
                            }

                         else
                            { valueAndFunctionDeclarationsMostToLeastDependedUpon =
                                remainingWithoutGroup
                            , destructuringDeclarationsMostToLeastDependedUpon =
                                mostDependedUponDestructuringDeclaration :: destructuringDeclarationsWithoutMostDependedUpon
                            , result =
                                case mostDependedUponGroup of
                                    Data.Graph.CyclicSCC cycleMembers ->
                                        cycleMembers
                                            |> List.foldl
                                                (\cycleMember soFarResult ->
                                                    HaxeExpressionWithLocalDeclaration
                                                        { declaration = cycleMember
                                                        , result = soFarResult
                                                        }
                                                )
                                                state.result

                                    Data.Graph.AcyclicSCC member ->
                                        HaxeExpressionWithLocalDeclaration
                                            { declaration = member
                                            , result = state.result
                                            }
                            }
                        )


fastSetsIntersect : FastSet.Set comparable -> FastSet.Set comparable -> Bool
fastSetsIntersect a b =
    -- TODO optimize
    Basics.not (FastSet.isEmpty (FastSet.intersect a b))


haxeExpressionUnit : HaxeExpression
haxeExpressionUnit =
    HaxeExpressionReference
        { moduleOrigin = Nothing
        , name = "Unit"
        }


haxePatternTrue : HaxePattern
haxePatternTrue =
    HaxePatternVariant
        { moduleOrigin = Nothing
        , name = "true"
        , values = []
        }


haxePatternFalse : HaxePattern
haxePatternFalse =
    HaxePatternVariant
        { moduleOrigin = Nothing
        , name = "false"
        , values = []
        }


haxeExpressionContainedLocalReferences : HaxeExpression -> FastSet.Set String
haxeExpressionContainedLocalReferences haxeExpression =
    --IGNORE TCO
    case haxeExpression of
        HaxeExpressionFloat _ ->
            FastSet.empty

        HaxeExpressionString _ ->
            FastSet.empty

        HaxeExpressionReference reference ->
            case reference.moduleOrigin of
                Nothing ->
                    reference.name |> FastSet.singleton

                Just _ ->
                    FastSet.empty

        HaxeExpressionRecordAccess haxeRecordAccess ->
            haxeExpressionContainedLocalReferences haxeRecordAccess.record

        HaxeExpressionList elements ->
            elements
                |> listMapToFastSetsAndUnify
                    haxeExpressionContainedLocalReferences

        HaxeExpressionRecord fields ->
            fields
                |> FastDict.values
                |> listMapToFastSetsAndUnify
                    haxeExpressionContainedLocalReferences

        HaxeExpressionCall call ->
            FastSet.union
                (call.called |> haxeExpressionContainedLocalReferences)
                (call.arguments
                    |> listMapToFastSetsAndUnify
                        haxeExpressionContainedLocalReferences
                )

        HaxeExpressionLambda lambda ->
            haxeExpressionContainedLocalReferences lambda.result

        HaxeExpressionSwitch switch ->
            FastSet.union
                (switch.matched |> haxeExpressionContainedLocalReferences)
                (FastSet.union
                    (switch.case0.result
                        |> haxeExpressionContainedLocalReferences
                    )
                    (switch.case1Up
                        |> listMapToFastSetsAndUnify
                            (\haxeCase ->
                                haxeCase.result
                                    |> haxeExpressionContainedLocalReferences
                            )
                    )
                )

        HaxeExpressionWithLocalDeclaration withLocalDeclarations ->
            FastSet.union
                (withLocalDeclarations.result
                    |> haxeExpressionContainedLocalReferences
                )
                (withLocalDeclarations.declaration.result
                    |> haxeExpressionContainedLocalReferences
                )


{-| Recursively find all introduced variables
in the [pattern](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Pattern)
(like `a` and `b` in `( Just a, { b } )`)
-}
patternBindings : Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern -> List String
patternBindings (Elm.Syntax.Node.Node _ syntaxPattern) =
    -- IGNORE TCO
    case syntaxPattern of
        Elm.Syntax.Pattern.VarPattern name ->
            [ name ]

        Elm.Syntax.Pattern.AsPattern afterAsPattern (Elm.Syntax.Node.Node _ name) ->
            name :: (afterAsPattern |> patternBindings)

        Elm.Syntax.Pattern.ParenthesizedPattern inParens ->
            inParens |> patternBindings

        Elm.Syntax.Pattern.ListPattern patterns ->
            patterns |> List.concatMap patternBindings

        Elm.Syntax.Pattern.TuplePattern patterns ->
            patterns |> List.concatMap patternBindings

        Elm.Syntax.Pattern.RecordPattern fields ->
            fields |> List.map Elm.Syntax.Node.value

        Elm.Syntax.Pattern.NamedPattern _ patterns ->
            patterns |> List.concatMap patternBindings

        Elm.Syntax.Pattern.UnConsPattern headPattern tailPattern ->
            (tailPattern |> patternBindings) ++ (headPattern |> patternBindings)

        Elm.Syntax.Pattern.AllPattern ->
            []

        Elm.Syntax.Pattern.UnitPattern ->
            []

        Elm.Syntax.Pattern.CharPattern _ ->
            []

        Elm.Syntax.Pattern.StringPattern _ ->
            []

        Elm.Syntax.Pattern.IntPattern _ ->
            []

        Elm.Syntax.Pattern.HexPattern _ ->
            []

        Elm.Syntax.Pattern.FloatPattern _ ->
            []


resultAndThen2 :
    (a -> b -> Result error c)
    -> Result error a
    -> Result error b
    -> Result error c
resultAndThen2 abToResult aResult bResult =
    case aResult of
        Err error ->
            Err error

        Ok a ->
            case bResult of
                Err error ->
                    Err error

                Ok b ->
                    abToResult a b


listMapToFastSetsAndUnify :
    (listElement -> FastSet.Set comparableFastSetElement)
    -> List listElement
    -> FastSet.Set comparableFastSetElement
listMapToFastSetsAndUnify elementToSet list =
    list
        |> List.foldl
            (\element soFar ->
                FastSet.union
                    (element |> elementToSet)
                    soFar
            )
            FastSet.empty


listMapToFastDictsAndUnify :
    (listElement -> FastDict.Dict comparableKey value)
    -> List listElement
    -> FastDict.Dict comparableKey value
listMapToFastDictsAndUnify elementToDict list =
    list
        |> List.foldl
            (\element soFar ->
                FastDict.union
                    (element |> elementToDict)
                    soFar
            )
            FastDict.empty


listMapToFastDict :
    (listElement -> ( comparableKey, value ))
    -> List listElement
    -> FastDict.Dict comparableKey value
listMapToFastDict elementToKeyValue list =
    list
        |> List.foldl
            (\element soFar ->
                let
                    ( key, value ) =
                        elementToKeyValue element
                in
                FastDict.insert key value soFar
            )
            FastDict.empty


listMapAndToFastSet :
    (a -> comparable)
    -> List a
    -> FastSet.Set comparable
listMapAndToFastSet elementToSetElement list =
    list
        |> List.foldl
            (\element soFar ->
                soFar
                    |> FastSet.insert
                        (element |> elementToSetElement)
            )
            FastSet.empty


condenseExpressionCall :
    { called : HaxeExpression
    , argument0 : HaxeExpression
    , argument1Up : List HaxeExpression
    }
    -> HaxeExpression
condenseExpressionCall call =
    case call.called of
        HaxeExpressionCall calledCall ->
            case calledCall.arguments of
                [] ->
                    HaxeExpressionCall calledCall

                calledCallArgument0 :: calledCallArgument1Up ->
                    condenseExpressionCall
                        { called = calledCall.called
                        , argument0 = calledCallArgument0
                        , argument1Up =
                            calledCallArgument1Up
                                ++ (call.argument0 :: call.argument1Up)
                        }

        HaxeExpressionLambda calledLambda ->
            case ( calledLambda.parameter0, calledLambda.result ) of
                ( Just "generated_record", HaxeExpressionRecordAccess recordAccess ) ->
                    case call.argument1Up of
                        [] ->
                            HaxeExpressionRecordAccess
                                { record = call.argument0
                                , field = recordAccess.field
                                }

                        argument1 :: argument2Up ->
                            HaxeExpressionCall
                                { called =
                                    HaxeExpressionRecordAccess
                                        { record = call.argument0
                                        , field = recordAccess.field
                                        }
                                , arguments = argument1 :: argument2Up
                                }

                ( Just "generated_0", HaxeExpressionCall variantCall ) ->
                    HaxeExpressionCall
                        { called = variantCall.called
                        , arguments = call.argument0 :: call.argument1Up
                        }

                _ ->
                    HaxeExpressionCall
                        { called = HaxeExpressionLambda calledLambda
                        , arguments = call.argument0 :: call.argument1Up
                        }

        calledNotCall ->
            HaxeExpressionCall
                { called = calledNotCall
                , arguments = call.argument0 :: call.argument1Up
                }


haxeExpressionIsDefinitelyOfTypeString : HaxeExpression -> Bool
haxeExpressionIsDefinitelyOfTypeString haxeExpression =
    case haxeExpression of
        HaxeExpressionString _ ->
            True

        HaxeExpressionCall call ->
            call.called
                == HaxeExpressionReference { moduleOrigin = Nothing, name = "string_append" }
                && ((call.arguments |> List.length) == 2)

        HaxeExpressionFloat _ ->
            False

        HaxeExpressionReference _ ->
            False

        HaxeExpressionRecordAccess _ ->
            False

        HaxeExpressionList _ ->
            False

        HaxeExpressionRecord _ ->
            False

        HaxeExpressionLambda _ ->
            False

        HaxeExpressionSwitch _ ->
            False

        HaxeExpressionWithLocalDeclaration _ ->
            False


case_ :
    { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            Elm.Syntax.ModuleName.ModuleName
    , valuesThatNeedToBeLazilyConstructed :
        FastSet.Set ( Elm.Syntax.ModuleName.ModuleName, String )
    , variantLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
            , valueCount : Int
            }
    , variablesFromWithinDeclarationInScope : FastSet.Set String
    }
    -> Elm.Syntax.Expression.Case
    ->
        Result
            String
            { pattern : HaxePattern, result : HaxeExpression }
case_ context ( patternNode, resultNode ) =
    Result.andThen
        (\casePattern ->
            Result.map
                (\result ->
                    { pattern = casePattern.pattern
                    , result = result
                    }
                )
                (resultNode
                    |> expression
                        (context
                            |> expressionContextAddVariablesInScope
                                casePattern.introducedVariables
                        )
                )
        )
        (patternNode
            |> pattern
                { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                    context.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup
                , variantLookup = context.variantLookup
                }
        )


letDeclaration :
    { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            Elm.Syntax.ModuleName.ModuleName
    , valuesThatNeedToBeLazilyConstructed :
        FastSet.Set ( Elm.Syntax.ModuleName.ModuleName, String )
    , variantLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
            , valueCount : Int
            }
    , variablesFromWithinDeclarationInScope : FastSet.Set String
    }
    -> Elm.Syntax.Node.Node Elm.Syntax.Expression.LetDeclaration
    -> Result String HaxeLetDeclaration
letDeclaration context (Elm.Syntax.Node.Node _ syntaxLetDeclaration) =
    case syntaxLetDeclaration of
        Elm.Syntax.Expression.LetDestructuring destructuringPatternNode destructuringExpressionNode ->
            Result.map2
                (\destructuringPattern destructuringExpression ->
                    HaxeLetDestructuring
                        { pattern = destructuringPattern.pattern
                        , expression = destructuringExpression
                        }
                )
                (destructuringPatternNode
                    |> pattern
                        { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                            context.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup
                        , variantLookup = context.variantLookup
                        }
                )
                (destructuringExpressionNode |> expression context)

        Elm.Syntax.Expression.LetFunction letValueOrFunction ->
            Result.map
                (\haxeLetDeclarationValueOrFunction ->
                    HaxeLetDeclarationValueOrFunction
                        haxeLetDeclarationValueOrFunction
                )
                (letValueOrFunction
                    |> letValueOrFunctionDeclaration context
                )


letValueOrFunctionDeclaration :
    { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            Elm.Syntax.ModuleName.ModuleName
    , valuesThatNeedToBeLazilyConstructed :
        FastSet.Set ( Elm.Syntax.ModuleName.ModuleName, String )
    , variantLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
            , valueCount : Int
            }
    , variablesFromWithinDeclarationInScope : FastSet.Set String
    }
    -> Elm.Syntax.Expression.Function
    ->
        Result
            String
            { name : String
            , parameters : List (Maybe String)
            , type_ : Maybe HaxeType
            , result : HaxeExpression
            }
letValueOrFunctionDeclaration context syntaxDeclarationValueOrFunction =
    let
        implementation : Elm.Syntax.Expression.FunctionImplementation
        implementation =
            syntaxDeclarationValueOrFunction.declaration |> Elm.Syntax.Node.value
    in
    resultAndThen2
        (\parameters maybeType ->
            Result.map
                (\result ->
                    let
                        parametersAndDestructuring :
                            { parameters : List (Maybe String)
                            , destructuring :
                                Maybe
                                    { expression : HaxeExpression
                                    , pattern : HaxePattern
                                    }
                            }
                        parametersAndDestructuring =
                            parametersToHaxeAndDestructuring
                                (parameters |> List.map .pattern)
                    in
                    { name =
                        implementation.name
                            |> Elm.Syntax.Node.value
                            |> lowercaseNameSanitizeForHaxe
                    , parameters = parametersAndDestructuring.parameters
                    , type_ = maybeType
                    , result =
                        case parametersAndDestructuring.destructuring of
                            Nothing ->
                                result

                            Just destructuring ->
                                HaxeExpressionSwitch
                                    { matched = destructuring.expression
                                    , case0 =
                                        { pattern = destructuring.pattern
                                        , result = result
                                        }
                                    , case1Up = []
                                    }
                    }
                )
                (implementation.expression
                    |> expression
                        { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                            context.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup
                        , valuesThatNeedToBeLazilyConstructed =
                            context.valuesThatNeedToBeLazilyConstructed
                        , variantLookup = context.variantLookup
                        , variablesFromWithinDeclarationInScope =
                            FastSet.union
                                (parameters
                                    |> listMapToFastSetsAndUnify .introducedVariables
                                )
                                context.variablesFromWithinDeclarationInScope
                        }
                )
        )
        (implementation.arguments
            |> listMapAndCombineOk
                (\p ->
                    p
                        |> pattern
                            { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                                context.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup
                            , variantLookup = context.variantLookup
                            }
                )
        )
        (case syntaxDeclarationValueOrFunction.signature of
            Nothing ->
                Ok Nothing

            Just (Elm.Syntax.Node.Node _ signature) ->
                Result.map Just
                    (signature.typeAnnotation
                        |> type_
                            { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                                context.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup
                            , variantLookup = context.variantLookup
                            }
                    )
        )


expressionOperatorToHaxeFunctionReference :
    String
    -> Result String { moduleOrigin : Maybe String, name : String }
expressionOperatorToHaxeFunctionReference operatorSymbol =
    case operatorSymbol of
        "+" ->
            Ok { moduleOrigin = Nothing, name = "basics_add" }

        "-" ->
            Ok { moduleOrigin = Nothing, name = "basics_sub" }

        "*" ->
            Ok { moduleOrigin = Nothing, name = "basics_mul" }

        "/" ->
            Ok { moduleOrigin = Nothing, name = "basics_fdiv" }

        "//" ->
            Ok { moduleOrigin = Nothing, name = "basics_idiv" }

        "^" ->
            Ok { moduleOrigin = Just "Math", name = "pow" }

        "==" ->
            Ok { moduleOrigin = Nothing, name = "basics_eq" }

        "/=" ->
            Ok { moduleOrigin = Nothing, name = "basics_neq" }

        "||" ->
            Ok { moduleOrigin = Nothing, name = "basics_or" }

        "&&" ->
            Ok { moduleOrigin = Nothing, name = "basics_and" }

        "<" ->
            Ok { moduleOrigin = Nothing, name = "basics_lt" }

        ">" ->
            Ok { moduleOrigin = Nothing, name = "basics_gt" }

        "<=" ->
            Ok { moduleOrigin = Nothing, name = "basics_le" }

        ">=" ->
            Ok { moduleOrigin = Nothing, name = "basics_ge" }

        "::" ->
            Ok { moduleOrigin = Nothing, name = "List_Cons" }

        "++" ->
            Ok { moduleOrigin = Nothing, name = "list_append" }

        unknownOrUnsupportedOperator ->
            Err ("unknown/unsupported operator " ++ unknownOrUnsupportedOperator)


qualifiedHaxeReferenceToString :
    { moduleOrigin : Maybe String
    , name : String
    }
    -> String
qualifiedHaxeReferenceToString reference =
    case reference.moduleOrigin of
        Nothing ->
            reference.name

        Just moduleOrigin ->
            moduleOrigin
                ++ "."
                ++ reference.name


printHaxeExpressionParenthesizedIfSpaceSeparated : HaxeExpression -> Print
printHaxeExpressionParenthesizedIfSpaceSeparated haxeExpression =
    if haxeExpression |> haxeExpressionIsSpaceSeparated then
        printParenthesized
            { opening = "("
            , closing = ")"
            , inner = printHaxeExpressionNotParenthesized haxeExpression
            }

    else
        printHaxeExpressionNotParenthesized haxeExpression


printHaxeExpressionParenthesizedIfWithLocalDeclarations : HaxeExpression -> Print
printHaxeExpressionParenthesizedIfWithLocalDeclarations haxeExpression =
    case haxeExpression of
        HaxeExpressionWithLocalDeclaration haxeExpressionWithLetDeclarations ->
            printParenthesized
                { opening = "{"
                , closing = "}"
                , inner =
                    printHaxeExpressionWithLocalDeclaration
                        haxeExpressionWithLetDeclarations
                        |> Print.followedBy (Print.exactly ";")
                }

        haxeExpressionNotWithLetDeclarations ->
            printHaxeExpressionNotParenthesized haxeExpressionNotWithLetDeclarations


haxeExpressionIsSpaceSeparated : HaxeExpression -> Bool
haxeExpressionIsSpaceSeparated haxeExpression =
    case haxeExpression of
        HaxeExpressionFloat _ ->
            False

        HaxeExpressionString _ ->
            False

        HaxeExpressionReference _ ->
            False

        HaxeExpressionRecordAccess _ ->
            False

        HaxeExpressionList _ ->
            False

        HaxeExpressionRecord _ ->
            False

        HaxeExpressionCall _ ->
            False

        HaxeExpressionLambda _ ->
            True

        HaxeExpressionSwitch _ ->
            True

        HaxeExpressionWithLocalDeclaration _ ->
            True


{-| Print a [`HaxeExpression`](#HaxeExpression)
-}
printHaxeExpressionNotParenthesized : HaxeExpression -> Print
printHaxeExpressionNotParenthesized haxeExpression =
    -- IGNORE TCO
    case haxeExpression of
        HaxeExpressionCall call ->
            printHaxeExpressionCall call

        HaxeExpressionReference reference ->
            Print.exactly
                (reference |> qualifiedHaxeReferenceToString)

        HaxeExpressionFloat float ->
            Print.exactly (haxeNumberLiteralToString float)

        HaxeExpressionString string ->
            printHaxeString string

        HaxeExpressionWithLocalDeclaration expressionWithLetDeclarations ->
            printHaxeExpressionWithLocalDeclaration expressionWithLetDeclarations

        HaxeExpressionSwitch syntaxWhenIs ->
            printHaxeExpressionCase syntaxWhenIs

        HaxeExpressionLambda syntaxLambda ->
            printHaxeExpressionLambda syntaxLambda

        HaxeExpressionRecord fields ->
            printHaxeExpressionRecord fields

        HaxeExpressionList elements ->
            printHaxeExpressionList elements

        HaxeExpressionRecordAccess syntaxRecordAccess ->
            printHaxeExpressionParenthesizedIfSpaceSeparated
                syntaxRecordAccess.record
                |> Print.followedBy
                    (Print.exactly
                        ("." ++ syntaxRecordAccess.field)
                    )


printHaxeExpressionCall :
    { called : HaxeExpression
    , arguments : List HaxeExpression
    }
    -> Print
printHaxeExpressionCall call =
    let
        calledPrint : Print
        calledPrint =
            printHaxeExpressionParenthesizedIfWithLocalDeclarations
                call.called

        argumentPrints : List Print
        argumentPrints =
            call.arguments
                |> List.map printHaxeExpressionParenthesizedIfWithLocalDeclarations

        fullLineSpread : Print.LineSpread
        fullLineSpread =
            argumentPrints
                |> Print.lineSpreadListMapAndCombine Print.lineSpread
                |> Print.lineSpreadMergeWith
                    (\() -> calledPrint |> Print.lineSpread)
    in
    calledPrint
        |> Print.followedBy (Print.exactly "(")
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.emptyOrLinebreakIndented fullLineSpread
                    |> Print.followedBy
                        (argumentPrints
                            |> Print.listIntersperseAndFlatten
                                (Print.exactly ","
                                    |> Print.followedBy
                                        (Print.spaceOrLinebreakIndented fullLineSpread)
                                )
                        )
                )
            )
        |> Print.followedBy (Print.emptyOrLinebreakIndented fullLineSpread)
        |> Print.followedBy (Print.exactly ")")


haxeNumberLiteralToString : Float -> String
haxeNumberLiteralToString float =
    let
        floatAsString : String
        floatAsString =
            float |> String.fromFloat
    in
    if floatAsString |> String.contains "." then
        floatAsString

    else
        floatAsString ++ ".0"


printHaxeExpressionList : List HaxeExpression -> Print
printHaxeExpressionList listElements =
    case listElements of
        [] ->
            Print.exactly "[]"

        element0 :: element1Up ->
            let
                elementsPrint : Print
                elementsPrint =
                    (element0 :: element1Up)
                        |> Print.listMapAndIntersperseAndFlatten
                            (\element ->
                                printHaxeExpressionNotParenthesized element
                            )
                            (Print.exactly ","
                                |> Print.followedBy Print.linebreakIndented
                            )
            in
            Print.exactly "[ "
                |> Print.followedBy
                    (Print.withIndentIncreasedBy 2
                        elementsPrint
                    )
                |> Print.followedBy
                    (Print.spaceOrLinebreakIndented
                        (elementsPrint |> Print.lineSpread)
                    )
                |> Print.followedBy
                    (Print.exactly "]")


patternIsSpaceSeparated : HaxePattern -> Bool
patternIsSpaceSeparated haxePattern =
    case haxePattern of
        HaxePatternIgnore ->
            False

        HaxePatternFloat _ ->
            False

        HaxePatternString _ ->
            False

        HaxePatternVariable _ ->
            False

        HaxePatternVariableCapture _ ->
            True

        HaxePatternVariant _ ->
            False

        HaxePatternRecord _ ->
            False


printHaxePatternParenthesizedIfSpaceSeparated : HaxePattern -> Print
printHaxePatternParenthesizedIfSpaceSeparated haxePattern =
    if haxePattern |> patternIsSpaceSeparated then
        printParenthesized
            { opening = "("
            , closing = ")"
            , inner = haxePattern |> printHaxePatternNotParenthesized
            }

    else
        haxePattern |> printHaxePatternNotParenthesized


haxePatternIntroducedVariables : HaxePattern -> List String
haxePatternIntroducedVariables haxePattern =
    -- IGNORE TCO
    case haxePattern of
        HaxePatternFloat _ ->
            []

        HaxePatternString _ ->
            []

        HaxePatternIgnore ->
            []

        HaxePatternVariable name ->
            [ name ]

        HaxePatternVariableCapture haxePatternAs ->
            haxePatternIntroducedVariables
                haxePatternAs.pattern

        HaxePatternVariant haxePatternVariant ->
            haxePatternVariant.values
                |> List.concatMap haxePatternIntroducedVariables

        HaxePatternRecord fields ->
            fields
                |> FastDict.values
                |> List.concatMap haxePatternIntroducedVariables


printHaxeExpressionLambda :
    { parameter0 : Maybe String
    , parameter1Up : List (Maybe String)
    , result : HaxeExpression
    }
    -> Print
printHaxeExpressionLambda syntaxLambda =
    Print.exactly "("
        |> Print.followedBy
            ((syntaxLambda.parameter0 :: syntaxLambda.parameter1Up)
                |> Print.listMapAndIntersperseAndFlatten
                    printHaxeParameterForExpression
                    (Print.exactly ", ")
            )
        |> Print.followedBy (Print.exactly ") ->")
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy
                        (printHaxeExpressionParenthesizedIfWithLocalDeclarations
                            syntaxLambda.result
                        )
                )
            )


printHaxeParameterForExpression : Maybe String -> Print
printHaxeParameterForExpression maybeVariable =
    case maybeVariable of
        Nothing ->
            Print.exactly "_"

        Just variableName ->
            Print.exactly variableName


printHaxeExpressionCase :
    { matched : HaxeExpression
    , case0 : { pattern : HaxePattern, result : HaxeExpression }
    , case1Up : List { pattern : HaxePattern, result : HaxeExpression }
    }
    -> Print
printHaxeExpressionCase haxeExpressionCase =
    let
        matchedPrint : Print
        matchedPrint =
            printHaxeExpressionNotParenthesized haxeExpressionCase.matched

        matchedPrintLineSpread : Print.LineSpread
        matchedPrintLineSpread =
            matchedPrint |> Print.lineSpread
    in
    Print.exactly "switch "
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.emptyOrLinebreakIndented matchedPrintLineSpread
                    |> Print.followedBy matchedPrint
                )
            )
        |> Print.followedBy
            (Print.emptyOrLinebreakIndented matchedPrintLineSpread)
        |> Print.followedBy (Print.exactly " {")
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy
                        ((haxeExpressionCase.case0 :: haxeExpressionCase.case1Up)
                            |> Print.listMapAndIntersperseAndFlatten
                                printHaxeExpressionSingleCase
                                (Print.linebreak
                                    |> Print.followedBy Print.linebreakIndented
                                )
                        )
                )
            )
        |> Print.followedBy Print.linebreakIndented
        |> Print.followedBy (Print.exactly "}")


printHaxeExpressionWithLocalDeclaration :
    { declaration :
        { name : String
        , parameters : List (Maybe String)
        , result : HaxeExpression
        , type_ : Maybe HaxeType
        }
    , result : HaxeExpression
    }
    -> Print
printHaxeExpressionWithLocalDeclaration haxeExpressionWithLocalDeclaration =
    (haxeExpressionWithLocalDeclaration.declaration |> printHaxeValueOrFunctionDeclaration)
        |> Print.followedBy Print.linebreak
        |> Print.followedBy Print.linebreakIndented
        |> Print.followedBy
            (printHaxeExpressionNotParenthesized haxeExpressionWithLocalDeclaration.result)


printHaxeExpressionSingleCase :
    { pattern : HaxePattern, result : HaxeExpression }
    -> Print
printHaxeExpressionSingleCase branch =
    let
        patternPrint : Print
        patternPrint =
            printHaxePatternNotParenthesized branch.pattern
    in
    Print.exactly "case "
        |> Print.followedBy
            (Print.withIndentIncreasedBy 2
                patternPrint
            )
        |> Print.followedBy
            (Print.emptyOrLinebreakIndented
                (patternPrint |> Print.lineSpread)
            )
        |> Print.followedBy (Print.exactly ":")
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy
                        (printHaxeExpressionParenthesizedIfWithLocalDeclarations
                            branch.result
                        )
                )
            )
        |> Print.followedBy (Print.exactly ";")


{-| Print value/function declarations into
an haxe module called `Elm` in the global namespace that exposes all members.
Will also add some internal wrapper declarations.
-}
haxeDeclarationsToModuleString :
    { valuesAndFunctions :
        FastDict.Dict
            String
            { parameters : List (Maybe String)
            , result : HaxeExpression
            , type_ : Maybe HaxeType
            }
    , typeAliases :
        FastDict.Dict
            String
            { parameters : List String
            , type_ : HaxeType
            }
    , choiceTypes :
        FastDict.Dict
            String
            { parameters : List String
            , variants : FastDict.Dict String (List HaxeType)
            }
    }
    -> String
haxeDeclarationsToModuleString haxeDeclarations =
    let
        valueAndFunctionDeclarationsOrdered :
            List
                { name : String
                , parameters : List (Maybe String)
                , result : HaxeExpression
                , type_ : Maybe HaxeType
                }
        valueAndFunctionDeclarationsOrdered =
            haxeDeclarations.valuesAndFunctions
                |> fastDictMapAndToList
                    (\name valueOrFunctionInfo ->
                        { name = name
                        , type_ = valueOrFunctionInfo.type_
                        , parameters = valueOrFunctionInfo.parameters
                        , result = valueOrFunctionInfo.result
                        }
                    )

        typeAliasDeclarations :
            List
                { name : String
                , parameters : List String
                , type_ : HaxeType
                }
        typeAliasDeclarations =
            haxeDeclarations.typeAliases
                |> fastDictMapAndToList
                    (\name info ->
                        { name = name
                        , parameters = info.parameters
                        , type_ = info.type_
                        }
                    )

        choiceTypeDeclarations :
            List
                { name : String
                , parameters : List String
                , variants : FastDict.Dict String (List HaxeType)
                }
        choiceTypeDeclarations =
            haxeDeclarations.choiceTypes
                |> fastDictMapAndToList
                    (\name info ->
                        { name = name
                        , parameters = info.parameters
                        , variants = info.variants
                        }
                    )
    in
    """import haxe.exceptions.NotImplementedException;
import haxe.ds.Option;

enum List_List<A> {
\tList_Empty;
\tList_Cons(head:A, tail:List_List<A>);
}

enum Basics_Order {
\tBasics_LT;
\tBasics_EQ;
\tBasics_GT;
}

enum Unit {
\tUnit;
}

"""
        ++ (typeAliasDeclarations
                |> Print.listMapAndIntersperseAndFlatten
                    (\haxeTypeAliasDeclaration ->
                        printHaxeTypedefDeclaration haxeTypeAliasDeclaration
                    )
                    (Print.linebreak
                        |> Print.followedBy Print.linebreak
                    )
                |> Print.toString
           )
        ++ """

"""
        ++ (choiceTypeDeclarations
                |> Print.listMapAndIntersperseAndFlatten
                    (\haxeChoiceTypeDeclaration ->
                        printHaxeChoiceTypeDeclaration haxeChoiceTypeDeclaration
                    )
                    (Print.linebreak
                        |> Print.followedBy Print.linebreak
                    )
                |> Print.toString
           )
        ++ """


class Elm {
"""
        ++ haxeDefaultDeclarations
        ++ """

"""
        ++ (Print.withIndentAtNextMultipleOf4
                (valueAndFunctionDeclarationsOrdered
                    |> Print.listMapAndIntersperseAndFlatten
                        (\haxeValueOrFunctionDeclaration ->
                            Print.exactly "static "
                                |> Print.followedBy
                                    (printHaxeValueOrFunctionDeclaration
                                        haxeValueOrFunctionDeclaration
                                    )
                        )
                        (Print.linebreak
                            |> Print.followedBy Print.linebreakIndented
                        )
                )
                |> Print.toString
           )
        ++ """
}
"""


printHaxeValueDeclaration :
    { name : String
    , type_ : Maybe HaxeType
    , result : HaxeExpression
    }
    -> Print
printHaxeValueDeclaration haxeValueOrFunctionDeclaration =
    Print.exactly
        ("final "
            ++ haxeValueOrFunctionDeclaration.name
        )
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                ((case haxeValueOrFunctionDeclaration.type_ of
                    Nothing ->
                        Print.empty

                    Just declaredType ->
                        let
                            typePrint : Print
                            typePrint =
                                printHaxeTypeNotParenthesized
                                    declaredType

                            typeLineSpread : Print.LineSpread
                            typeLineSpread =
                                typePrint |> Print.lineSpread
                        in
                        Print.exactly ":"
                            |> Print.followedBy
                                (Print.emptyOrLinebreakIndented typeLineSpread
                                    |> Print.followedBy
                                        (Print.withIndentAtNextMultipleOf4
                                            typePrint
                                        )
                                )
                 )
                    |> Print.followedBy
                        (Print.exactly " =")
                    |> Print.followedBy Print.linebreakIndented
                    |> Print.followedBy
                        (printHaxeExpressionNotParenthesized
                            haxeValueOrFunctionDeclaration.result
                        )
                    |> Print.followedBy (Print.exactly ";")
                )
            )


maybeHaxeTypeContainsVariables : Maybe HaxeType -> Bool
maybeHaxeTypeContainsVariables maybeHaxeType =
    case maybeHaxeType of
        Nothing ->
            False

        Just declaredType ->
            Basics.not
                (FastSet.isEmpty
                    (declaredType
                        |> haxeTypeContainedVariables
                    )
                )


printHaxeValueOrFunctionDeclaration :
    { parameters : List (Maybe String)
    , name : String
    , type_ : Maybe HaxeType
    , result : HaxeExpression
    }
    -> Print
printHaxeValueOrFunctionDeclaration haxeValueOrFunctionDeclaration =
    case haxeValueOrFunctionDeclaration.parameters of
        [] ->
            if haxeValueOrFunctionDeclaration.type_ |> maybeHaxeTypeContainsVariables then
                printHaxeFunctionDeclaration
                    { name = haxeValueOrFunctionDeclaration.name
                    , parameters = []
                    , type_ = haxeValueOrFunctionDeclaration.type_
                    , result = haxeValueOrFunctionDeclaration.result
                    }

            else
                printHaxeValueDeclaration
                    { name = haxeValueOrFunctionDeclaration.name
                    , type_ = haxeValueOrFunctionDeclaration.type_
                    , result = haxeValueOrFunctionDeclaration.result
                    }

        parameter0 :: parameter1Up ->
            printHaxeFunctionDeclaration
                { name = haxeValueOrFunctionDeclaration.name
                , parameters = parameter0 :: parameter1Up
                , type_ = haxeValueOrFunctionDeclaration.type_
                , result = haxeValueOrFunctionDeclaration.result
                }


printHaxeFunctionDeclaration :
    { parameters : List (Maybe String)
    , name : String
    , type_ : Maybe HaxeType
    , result : HaxeExpression
    }
    -> Print
printHaxeFunctionDeclaration haxeFunctionDeclaration =
    let
        resultPrint : Print
        resultPrint =
            printHaxeExpressionParenthesizedIfWithLocalDeclarations
                haxeFunctionDeclaration.result
    in
    (case haxeFunctionDeclaration.type_ of
        Nothing ->
            Print.exactly
                ("function "
                    ++ haxeFunctionDeclaration.name
                    ++ "("
                )
                |> Print.followedBy
                    (haxeFunctionDeclaration.parameters
                        |> Print.listMapAndIntersperseAndFlatten
                            printHaxeParameterForExpression
                            (Print.exactly ", ")
                    )
                |> Print.followedBy
                    (Print.exactly ")")

        Just declaredType ->
            let
                typedParametersAndResultType :
                    { parameters : List { name : Maybe String, type_ : HaxeType }
                    , result : HaxeType
                    }
                typedParametersAndResultType =
                    case declaredType of
                        HaxeTypeFunction haxeTypeFunction ->
                            { parameters =
                                List.map2
                                    (\parameterType parameterName ->
                                        { name = parameterName, type_ = parameterType }
                                    )
                                    haxeTypeFunction.input
                                    haxeFunctionDeclaration.parameters
                            , result = haxeTypeFunction.output
                            }

                        declaredTypeNotFunction ->
                            -- TODO fail or something
                            { parameters = []
                            , result = declaredTypeNotFunction
                            }

                resultTypePrint : Print
                resultTypePrint =
                    printHaxeTypeNotParenthesized
                        typedParametersAndResultType.result

                typeLineSpread : Print.LineSpread
                typeLineSpread =
                    resultTypePrint |> Print.lineSpread

                generics : List String
                generics =
                    declaredType
                        |> haxeTypeContainedVariables
                        |> FastSet.toList
            in
            Print.exactly
                ((case generics of
                    [] ->
                        "function "
                            ++ haxeFunctionDeclaration.name

                    generic0 :: generic1Up ->
                        -- @:generic
                        "function "
                            ++ haxeFunctionDeclaration.name
                            ++ "<"
                            ++ ((generic0 :: generic1Up)
                                    |> String.join ", "
                               )
                            ++ ">"
                 )
                    ++ "("
                )
                |> Print.followedBy
                    (typedParametersAndResultType.parameters
                        |> Print.listMapAndIntersperseAndFlatten
                            (\typedParameter ->
                                typedParameter.name
                                    |> printHaxeParameterForExpression
                                    |> Print.followedBy
                                        (Print.exactly ":")
                                    |> Print.followedBy
                                        (typedParameter.type_
                                            |> printHaxeTypeNotParenthesized
                                        )
                            )
                            (Print.exactly ", ")
                    )
                |> Print.followedBy
                    (Print.exactly ")")
                |> Print.followedBy
                    (Print.withIndentAtNextMultipleOf4
                        (Print.exactly ":"
                            |> Print.followedBy
                                (Print.emptyOrLinebreakIndented typeLineSpread
                                    |> Print.followedBy
                                        (Print.withIndentAtNextMultipleOf4
                                            resultTypePrint
                                        )
                                )
                        )
                    )
    )
        |> Print.followedBy
            (Print.exactly " {")
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy
                        (Print.exactly "return")
                    |> Print.followedBy
                        (Print.withIndentAtNextMultipleOf4
                            (Print.spaceOrLinebreakIndented
                                (resultPrint |> Print.lineSpread)
                                |> Print.followedBy
                                    resultPrint
                                |> Print.followedBy
                                    (Print.exactly ";")
                            )
                        )
                )
            )
        |> Print.followedBy Print.linebreakIndented
        |> Print.followedBy (Print.exactly "}")


haxeDefaultDeclarations : String
haxeDefaultDeclarations =
    """\tstatic function debug_log<A>(tag:String, data:A):A {
\t\ttrace(tag, data);
\t\treturn data;
\t}

\tstatic function debug_todo<A>(message:String):A {
\t\tthrow new NotImplementedException(message);
\t}

\tinline static function basics_identity<A>(a:A):A {
\t\treturn a;
\t}

\tinline static function basics_always<Ignored, Kept>(kept:Kept, _:Ignored):Kept {
\t\treturn kept;
\t}

\tinline static function basics_not(bool:Bool):Bool {
\t\treturn !bool;
\t}

\tinline static function basics_or(a:Bool, b:Bool):Bool {
\t\treturn a || b;
\t}

\tinline static function basics_and(a:Bool, b:Bool):Bool {
\t\treturn a && b;
\t}

\tinline static function basics_eq<A>(a:A, b:A):Bool {
\t\treturn a == b;
\t}

\tinline static function basics_neq<A>(a:A, b:A):Bool {
\t\treturn a != b;
\t}

\tinline static function basics_lt(a:Float, b:Float):Bool {
\t\treturn a < b;
\t}

\tinline static function basics_gt(a:Float, b:Float):Bool {
\t\treturn a > b;
\t}

\tinline static function basics_le(a:Float, b:Float):Bool {
\t\treturn a <= b;
\t}

\tinline static function basics_ge(a:Float, b:Float):Bool {
\t\treturn a >= b;
\t}

\tstatic function basics_compare<A>(a:A, b:A):Basics_Order {
\t\treturn intToBasics_Order(Reflect.compare(a, b));
\t}

\tstatic function intToBasics_Order(comparisonInt:Int):Basics_Order {
\t\treturn if (comparisonInt <= -1) Basics_LT else if (comparisonInt == 0) Basics_EQ else Basics_GT;
\t}

\tstatic function basics_OrderToInt(order:Basics_Order):Int {
\t\treturn switch (order) {
\t\t\tcase Basics_LT: -1;
\t\t\tcase Basics_EQ: 0;
\t\t\tcase Basics_GT: 1;
\t\t}
\t}

\tinline static function basics_negate(float:Float):Float {
\t\treturn -float;
\t}

\tinline static function basics_truncate(float:Float):Float {
\t\treturn Std.int(float);
\t}

\tstatic function basics_isInfinite(float:Float):Bool {
\t\treturn (float == Math.POSITIVE_INFINITY) || (float == Math.NEGATIVE_INFINITY);
\t}

\tinline static function basics_add(a:Float, b:Float):Float {
\t\treturn a + b;
\t}

\tinline static function basics_sub(base:Float, toSubtract:Float):Float {
\t\treturn base - toSubtract;
\t}

\tinline static function basics_mul(a:Float, b:Float):Float {
\t\treturn a * b;
\t}

\tinline static function basics_idiv(to_divide:Float, divisor:Float):Float {
\t\treturn Std.int(to_divide / divisor);
\t}

\tinline static function basics_fdiv(to_divide:Float, divisor:Float):Float {
\t\treturn to_divide / divisor;
\t}

\tinline static function basics_modBy(divisor:Float, to_divide:Float):Float {
\t\treturn to_divide % divisor;
\t}

\tstatic function basics_remainderBy(divisor:Float, to_divide:Float):Float {
\t\tfinal remainder = to_divide % divisor;
\t\treturn if (((remainder > 0.0) && (divisor < 0.0)) || ((remainder < 0.0) && (divisor > 0.0))) {
\t\t\tremainder - to_divide;
\t\t} else {
\t\t\tremainder;
\t\t};
\t}

\tstatic function string_toInt(str:String):Option<Float> {
\t\treturn switch Std.parseInt(str) {
\t\t\tcase null:
\t\t\t\tOption.None;
\t\t\tcase int:
\t\t\t\tOption.Some(int);
\t\t};
\t}

\tstatic function string_toFloat(str:String):Option<Float> {
\t\treturn switch Std.parseFloat(str) {
\t\t\tcase null:
\t\t\t\tOption.None;
\t\t\tcase float:
\t\t\t\tOption.Some(float);
\t\t};
\t}

\tinline static function string_length(str:String) {
\t\treturn str.length;
\t}

\tinline static function string_isEmpty(str:String) {
\t\treturn str == "";
\t}

\tinline static function string_append(earlier:String, later:String):String {
\t\treturn earlier + later;
\t}

\tinline static function string_contains(sub:String, str:String):Bool {
\t\treturn StringTools.contains(str, sub);
\t}

\tinline static function string_startsWith(start:String, str:String):Bool {
\t\treturn StringTools.startsWith(str, start);
\t}

\tinline static function string_endsWith(end:String, str:String):Bool {
\t\treturn StringTools.endsWith(str, end);
\t}

\tstatic function string_toList(str:String):List_List<String> {
\t\treturn if (str == "") List_Empty else arrayToList_List(str.split(""));
\t}

\tstatic function string_concat(segments:List_List<String>):String {
\t\tvar remainingSegments = segments;
\t\tvar stringBuffer = new StringBuf();
\t\twhile (true) {
\t\t\tswitch remainingSegments {
\t\t\t\tcase List_Empty:
\t\t\t\t\tbreak;
\t\t\t\tcase List_Cons(head, tail):
\t\t\t\t\tstringBuffer.add(head);
\t\t\t\t\tremainingSegments = tail;
\t\t\t}
\t\t}
\t\treturn stringBuffer.toString();
\t}

\tstatic function string_join(inBetween:String, segments:List_List<String>):String {
\t\tswitch segments {
\t\t\tcase List_Empty:
\t\t\t\treturn "";
\t\t\tcase List_Cons(headSegment, tailSegments):
\t\t\t\tvar remainingCharacters = tailSegments;
\t\t\t\tvar stringBuffer = new StringBuf();
\t\t\t\tstringBuffer.add(headSegment);
\t\t\t\twhile (true) {
\t\t\t\t\tswitch remainingCharacters {
\t\t\t\t\t\tcase List_Empty:
\t\t\t\t\t\t\tbreak;
\t\t\t\t\t\tcase List_Cons(head, tail):
\t\t\t\t\t\t\tstringBuffer.add(inBetween);
\t\t\t\t\t\t\tstringBuffer.add(head);
\t\t\t\t\t\t\tremainingCharacters = tail;
\t\t\t\t\t}
\t\t\t\t}
\t\t\t\treturn stringBuffer.toString();
\t\t}
\t}

\tstatic function string_reverse(str:String):String {
\t\tfinal charArray = str.split("");
\t\tcharArray.reverse(); // mutate
\t\treturn charArray.join("");
\t}

\tinline static function string_dropLeft(countToSkip:Float, str:String):String {
\t\treturn str.substr(Std.int(countToSkip));
\t}

\tstatic function string_dropRight(countToSkip:Float, str:String):String {
\t\treturn str.substr(0, str.length - Std.int(countToSkip));
\t}

\tinline static function string_left(countToTake:Float, str:String):String {
\t\treturn str.substr(0, Std.int(countToTake));
\t}

\tstatic function string_right(countToTake:Float, str:String):String {
\t\treturn str.substr(str.length - Std.int(countToTake));
\t}

\tinline static function string_padRight(desiredLength:Float, padChar:String, str:String,):String {
\t\treturn StringTools.rpad(str, padChar, Std.int(desiredLength));
\t}

\tinline static function string_padLeft(desiredLength:Float, padChar:String, str:String,):String {
\t\treturn StringTools.lpad(str, padChar, Std.int(desiredLength));
\t}

\tstatic function string_repeat(repetitionCount:Float, segment:String):String {
\t\tfinal buffer = new StringBuf();
\t\tfor (_ in 0...(Std.int(repetitionCount) - 1)) {
\t\t\tbuffer.add(segment);
\t\t}
\t\treturn buffer.toString();
\t}

\tinline static function string_replace(toReplace:String, replacement:String, str:String,):String {
\t\treturn StringTools.replace(str, toReplace, replacement);
\t}

\tinline static function string_toLower(string:String):String {
\t\treturn string.toLowerCase();
\t}

\tinline static function string_toUpper(string:String):String {
\t\treturn string.toUpperCase();
\t}

\tstatic function string_map(characterChange:String->String, str:String):String {
\t\treturn str.split("").map(characterChange).join("");
\t}

\tstatic final linebreakRegex = // doesn't cover old mac and other uncommon platforms
\t\t~/(\\t?\\n)/;

\tinline static function string_lines(str:String):List_List<String> {
\t\treturn arrayToList_List(linebreakRegex.split(str));
\t}

\tstatic function string_slice(start:Float, end:Float, str:String):String {
\t\tfinal realStartIndexInclusive = if (start >= 0) Std.int(start) else {
\t\t\tstr.length + Std.int(start);
\t\t};
\t\tfinal realEndIndexExclusive = if (end >= 0) Std.int(end) else {
\t\t\tstr.length + Std.int(end);
\t\t};
\t\treturn str.substring(realStartIndexInclusive, realEndIndexExclusive);
\t}

\tinline static function string_split(separator:String, str:String):List_List<String> {
\t\treturn arrayToList_List(str.split(separator));
\t}

\tprivate static function arrayToList_List<A>(array:Array<A>):List_List<A> {
\t\tvar soFar = List_Empty;
\t\tvar index = array.length - 1;
\t\twhile (index >= 0) {
\t\t\tsoFar = List_Cons(array[index], soFar);
\t\t\tindex--;
\t\t}
\t\treturn soFar;
\t}

\tprivate static function list_ListToArray<A>(fullList:List_List<A>):Array<A> {
\t\tvar indexToFillNext = 0;
\t\tvar soFar = [];
\t\tvar remainingList = fullList;
\t\twhile (true) {
\t\t\tswitch remainingList {
\t\t\t\tcase List_Empty:
\t\t\t\t\treturn soFar;
\t\t\t\tcase List_Cons(remainingHead, remainingTail):
\t\t\t\t\tsoFar[indexToFillNext] = remainingHead;
\t\t\t\t\tremainingList = remainingTail;
\t\t\t\t\tindexToFillNext++;
\t\t\t}
\t\t}
\t}

\tstatic function char_toCode(char:String):Float {
\t\treturn char.charCodeAt(0) ?? 0;
\t}

\tstatic function string_all(isExpected:String->Bool, str:String):Bool {
\t\treturn if (str == "") true else iterableAll(isExpected, str.split(""));
\t}

\tstatic function string_any(isOdd:String->Bool, str:String):Bool {
\t\treturn if (str == "") true else Lambda.exists(str.split(""), isOdd);
\t}

\tinline private static function iterableAll<A>(isExpected:A->Bool, array:Iterable<A>):Bool {
\t\treturn !Lambda.exists(array, element -> !isExpected(element));
\t}

\tstatic function string_filter(shouldKeep:String->Bool, str:String):String {
\t\treturn if (str == "") "" else str.split("").filter(shouldKeep).join("");
\t}

\tstatic function list_singleton<A>(onlyElement:A) {
\t\treturn List_Cons(onlyElement, List_Empty);
\t}

\tinline static function list_isEmpty<A>(list:List_List<A>):Bool {
\t\treturn list == List_Empty;
\t}

\tstatic function list_length<A>(list:List_List<A>):Float {
\t\treturn list_foldl((_, soFar) -> soFar + 1, 0, list);
\t}

\tstatic function list_all<A>(isExpected:A->Bool, list:List_List<A>):Bool {
\t\tvar remainingList = list;
\t\twhile (true) {
\t\t\tswitch (remainingList) {
\t\t\t\tcase List_Empty:
\t\t\t\t\treturn true;
\t\t\t\tcase List_Cons(head, tail):
\t\t\t\t\tif (!isExpected(head)) {
\t\t\t\t\t\treturn false;
\t\t\t\t\t} else {
\t\t\t\t\t\tremainingList = tail;
\t\t\t\t\t};
\t\t\t}
\t\t}
\t}

\tstatic function list_any<A>(isOdd:A->Bool, list:List_List<A>):Bool {
\t\tvar remainingList = list;
\t\twhile (true) {
\t\t\tswitch (remainingList) {
\t\t\t\tcase List_Empty:
\t\t\t\t\treturn false;
\t\t\t\tcase List_Cons(head, tail):
\t\t\t\t\tif (isOdd(head)) {
\t\t\t\t\t\treturn true;
\t\t\t\t\t} else {
\t\t\t\t\t\tremainingList = tail;
\t\t\t\t\t};
\t\t\t}
\t\t}
\t}

\tinline static function list_member<A>(needle:A, list:List_List<A>):Bool {
\t\treturn list_any(element -> element == needle, list);
\t}

\tstatic function list_drop<A>(countToSkip:Float, list:List_List<A>):List_List<A> {
\t\tvar remainingCountToSkip = countToSkip;
\t\tvar soFar = list;
\t\twhile (remainingCountToSkip >= 1) {
\t\t\tswitch (soFar) {
\t\t\t\tcase List_Empty:
\t\t\t\t\tbreak;
\t\t\t\tcase List_Cons(_, tail):
\t\t\t\t\tsoFar = tail;
\t\t\t\t\tremainingCountToSkip--;
\t\t\t}
\t\t}
\t\treturn soFar;
\t}

\tstatic function list_take<A>(countToTake:Float, list:List_List<A>):List_List<A> {
\t\tvar remainingCountToTake = countToTake;
\t\tvar remainingList = list;
\t\tvar takenElementsArraySoFar = [];
\t\twhile (remainingCountToTake >= 1) {
\t\t\tswitch (remainingList) {
\t\t\t\tcase List_Empty:
\t\t\t\t\tbreak;
\t\t\t\tcase List_Cons(head, tail):
\t\t\t\t\ttakenElementsArraySoFar.push(head);
\t\t\t\t\tremainingList = tail;
\t\t\t\t\tremainingCountToTake--;
\t\t\t}
\t\t}
\t\treturn arrayToList_List(takenElementsArraySoFar);
\t}

\tstatic function list_intersperse<A>(inBetween:A, list:List_List<A>):List_List<A> {
\t\treturn switch list {
\t\t\tcase List_Empty: List_Empty;
\t\t\tcase List_Cons(head, tail):
\t\t\t\tlist_foldr((element, soFar) -> List_Cons(element, List_Cons(inBetween, soFar)), list_singleton(head), tail);
\t\t}
\t}

\tstatic function list_map<A, B>(elementChange:A->B, list:List_List<A>):List_List<B> {
\t\treturn list_foldr((element, soFar) -> List_Cons(elementChange(element), soFar), List_Empty, list);
\t}

\tstatic function list_indexedMap<A, B>(indexedElementChange:Float->A->B, list:List_List<A>):List_List<B> {
\t\treturn list_foldr((element, soFar) -> {
\t\t\tindex: soFar.index + 1,
\t\t\tlist: List_Cons(indexedElementChange(soFar.index, element), soFar.list)
\t\t}, {index: 0, list: List_Empty}, list).list;
\t}

\tstatic function list_map2<A, B, C>(combine_ab:(A, B) -> C, aList:List_List<A>, bList:List_List<B>,):List_List<C> {
\t\tvar remainingAList = aList;
\t\tvar remainingBList = bList;
\t\tvar combinedArraySoFar = [];
\t\twhile (true) {
\t\t\tswitch {a: remainingAList, b: remainingBList} {
\t\t\t\tcase {a: List_Empty, b: List_Empty}:
\t\t\t\t\tbreak;
\t\t\t\tcase {a: List_Empty, b: List_Cons(_, _)}:
\t\t\t\t\tbreak;
\t\t\t\tcase {a: List_Cons(_, _), b: List_Empty}:
\t\t\t\t\tbreak;
\t\t\t\tcase {a: List_Cons(aHead, aTail), b: List_Cons(bHead, bTail)}:
\t\t\t\t\tremainingAList = aTail;
\t\t\t\t\tremainingBList = bTail;
\t\t\t\t\tcombinedArraySoFar.push(combine_ab(aHead, bHead));
\t\t\t}
\t\t}
\t\treturn arrayToList_List(combinedArraySoFar);
\t}

\tinline static function list_zip<A, B>(aList:List_List<A>, bList:List_List<B>):List_List<{first:A, second:B}> {
\t\treturn list_map2((a, b) -> {first: a, second: b}, aList, bList);
\t}

\tstatic function list_unzip<A, B>(abList:List_List<{first:A, second:B}>):{first:List_List<A>, second:List_List<B>} {
\t\treturn {first: list_map(ab -> ab.first, abList), second: list_map(ab -> ab.second, abList)};
\t}

\tstatic function list_filterMap<A, B>(element_toOption:A->Option<B>, list:List_List<A>,):List_List<B> {
\t\treturn list_foldr((element, soFar) -> switch element_toOption(element) {
\t\t\tcase None: soFar;
\t\t\tcase Some(value): List_Cons(value, soFar);
\t\t}, List_Empty, list);
\t}

\tstatic function list_append<A>(earlier:List_List<A>, later:List_List<A>):List_List<A> {
\t\treturn list_foldr((earlierElement, soFar) -> List_Cons(earlierElement, soFar), later, earlier);
\t}

\tstatic function list_concatMap<A, B>(elementToList:A->List_List<B>, list:List_List<A>):List_List<B> {
\t\treturn list_foldr((element, soFar) -> list_append(elementToList(element), soFar), List_Empty, list);
\t}

\tstatic function list_concat<A, B>(list:List_List<List_List<A>>):List_List<A> {
\t\treturn list_foldr((element, soFar) -> list_append(element, soFar), List_Empty, list);
\t}

\tstatic function list_filter<A>(keepElement:A->Bool, list:List_List<A>):List_List<A> {
\t\treturn list_foldr((element, soFar) -> if (keepElement(element)) soFar else List_Cons(element, soFar), List_Empty, list);
\t}

\tstatic function list_repeat<A>(final_length:Float, element_to_repeat:A):List_List<A> {
\t\tvar soFar = List_Empty;
\t\tfor (_ in 1...Std.int(final_length)) {
\t\t\tsoFar = List_Cons(element_to_repeat, soFar);
\t\t}
\t\treturn soFar;
\t}

\tstatic function list_foldl<A, Folded>(reduce:(A, Folded) -> Folded, initialFolded:Folded, list:List_List<A>):Folded {
\t\tvar foldedSoFar = initialFolded;
\t\tvar remainingList = list;
\t\twhile (true) {
\t\t\tswitch list {
\t\t\t\tcase List_Empty:
\t\t\t\t\tbreak;
\t\t\t\tcase List_Cons(head, tail):
\t\t\t\t\tfoldedSoFar = reduce(head, initialFolded);
\t\t\t\t\tremainingList = tail;
\t\t\t}
\t\t}
\t\treturn foldedSoFar;
\t}

\tstatic function list_foldr<A, Folded>(reduce:(A, Folded) -> Folded, initialFolded:Folded, list:List_List<A>,):Folded {
\t\treturn list_foldl(reduce, initialFolded, list_reverse(list));
\t}

\tstatic function list_reverse<A>(list:List_List<A>):List_List<A> {
\t\treturn list_foldr(List_Cons, List_Empty, list);
\t}

\tstatic function list_range(start:Float, end:Float):List_List<Float> {
\t\tvar soFar:List_List<Float> = List_Empty;
\t\tfor (i in Std.int(start)...Std.int(end)) {
\t\t\tsoFar = List_Cons(i, soFar);
\t\t}
\t\treturn soFar;
\t}

\tstatic function list_sortWith<A>(elementCompare:(A, A) -> Basics_Order, list:List_List<A>,):List_List<A> {
\t\tfinal asArray = list_ListToArray(list);
\t\tasArray.sort((a, b) -> basics_OrderToInt(elementCompare(a, b))); // mutate
\t\treturn arrayToList_List(asArray);
\t}

\tstatic function list_sortBy<A, Comparable>(elementToComparable:A->Comparable, list:List_List<A>,):List_List<A> {
\t\tfinal asArray = list_ListToArray(list);
\t\tasArray.sort((a, b) -> Reflect.compare(elementToComparable(a), elementToComparable(b))); // mutate
\t\treturn arrayToList_List(asArray);
\t}

\tstatic function list_sort<T>(list:List_List<T>):List_List<T> {
\t\tfinal asArray = list_ListToArray(list);
\t\tasArray.sort(Reflect.compare); // mutate
\t\treturn arrayToList_List(asArray);
\t}

\tstatic function list_sum(list:List_List<Float>):Float {
\t\treturn list_foldl(basics_add, 0, list);
\t}

\tstatic function list_product(list:List_List<Float>):Float {
\t\treturn list_foldl(basics_mul, 1, list);
\t}

\tstatic function list_maximum(list:List_List<Float>):Option<Float> {
\t\treturn switch list {
\t\t\tcase List_Empty: None;
\t\t\tcase List_Cons(head, tail): Some(list_foldl(Math.max, head, tail));
\t\t}
\t}

\tstatic function list_minimum(list:List_List<Float>):Option<Float> {
\t\treturn switch list {
\t\t\tcase List_Empty: None;
\t\t\tcase List_Cons(head, tail): Some(list_foldl(Math.min, head, tail));
\t\t}
\t}
"""


resultAndThen3 :
    (a -> b -> c -> Result error d)
    -> Result error a
    -> Result error b
    -> Result error c
    -> Result error d
resultAndThen3 abToResult aResult bResult cResult =
    case aResult of
        Err error ->
            Err error

        Ok a ->
            case bResult of
                Err error ->
                    Err error

                Ok b ->
                    case cResult of
                        Err error ->
                            Err error

                        Ok c ->
                            abToResult a b c


fastDictMapAndToList :
    (key -> value -> element)
    -> FastDict.Dict key value
    -> List element
fastDictMapAndToList keyValueToElement fastDict =
    fastDict
        |> FastDict.foldr
            (\key value soFar ->
                keyValueToElement key value
                    :: soFar
            )
            []


listMapAndCombineOk : (a -> Result err ok) -> List a -> Result err (List ok)
listMapAndCombineOk elementToResult list =
    listMapAndCombineOkFrom [] elementToResult list


listMapAndCombineOkFrom : List ok -> (a -> Result err ok) -> List a -> Result err (List ok)
listMapAndCombineOkFrom soFar elementToResult list =
    case list of
        [] ->
            Ok (soFar |> List.reverse)

        head :: tail ->
            case head |> elementToResult of
                Err headErr ->
                    Err headErr

                Ok headOk ->
                    listMapAndCombineOkFrom (headOk :: soFar)
                        elementToResult
                        tail
