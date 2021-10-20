# TypeScript API

The purpose of this document is describing the TypeScript API generated for Morphir
models by running `morphir-elm gen --target=TypeScript`.

## Generating TypeScript

Given a model represented in `morphir-ir.json`, you can generate a TypeScript
representation by running:

    morphir-elm gen --input morphir-ir.json --output ./generated --target=TypeScript

Note that at present only the types are converted. Data values and functions
are not.

You can generate a TypeScript representation of the Morphir IR itself by
running this in the morphir-elm repo:

    morphir-elm make ./morphir-make --types-only
    morphir-elm gen --input=morphir-ir.json --output=./generated --target=TypeScript

## Using the generated types

The TypeScript backend outputs a top-level module per package, which your own
code should import. The namespaces correspond with the package and module names
in the IR. Only namespaces and symbols marked as public will be exported in the
TypeScript API.

For example, you can use the `IR` types from the `Morphir` package like this:

    import { Morphir } from './generated/Morphir'
 
    const myName: Morphir.IR.Name.Name = ["this", "is", "a", "great", "name"]

Internally the types are mapped to JavaScript types. You will find this in
`generated/morphir/ir/Name.ts`:

    export type Name = Array<string>

You benefit from all the usual TypeScript type checking. For example, a Path
must be a list of Name instances, so this example will not compile:

    import { Morphir } from './generated/Morphir'
 
    const myName: Morphir.IR.Path.Path = "This is the wrong type."

You should see an error when compiling this:

    test.ts:3:7 - error TS2322: Type 'string' is not assignable to type 'Path'.

Most Morphir types correspond directly to JavaScript types. The
[JSON mapping](https://github.com/finos/morphir-elm/blob/main/docs/json-mapping.md)
gives a useful reference. There are some special cases, which are documented below.

### Type mapping details

#### Dict

A `Morphir.SDK.Dict.Dict K V` maps to a TypeScript `Array<K,V>`. We plan to
change to using
[Map](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Map).

#### Custom types

We follow the example
["Tagged Union Types in TypeScript"](https://mariusschulz.com/blog/tagged-union-types-in-typescript)
to implement custom types.

Each type variant is a TypeScript `interface`, with a `kind` and maybe some
fields: `arg1`, `arg2`, `arg3` and so on.

You need to construct these manually. Here's an example using the Morphir IR
`Literal` type:

    import { Morphir } from './generated/Morphir'

    let myBool: Morphir.IR.Literal.BoolLiteral = {
        kind: "BoolLiteral",
        arg1: true,
    }

    let myString: Morphir.IR.Literal.StringLiteral = {
        kind: "StringLiteral",
        arg1: "This is a wonderful string",
    }

    let myLiteral: Morphir.IR.Literal.Literal = myString;

Note that this example would not actually compile as the `Morphir.IR.Literal`
module is private by default. You could work around this by modifying
`generated/Morphir.ts` to export it.

#### Type variables

Morphir's custom types and type aliases can use type variables. These map to
TypeScript [generics](https://www.typescriptlang.org/docs/handbook/2/generics.html).

Here's an example using Morphir IR's `AccessControlled` type, which is a type
alias that maps to a Record.

    import { Morphir } from './generated/Morphir'

    const myAccess: Morphir.IR.AccessControlled.Access = {
        "kind": "Public"
    }

    let myAccessControlled: Morphir.IR.AccessControlled.AccessControlled<String> = {
        access: myAccess,
        value: "I'm a string",
    }

## JSON serialization and deserialization

The generated TypeScript API includes `decode` and `encode` functions for each
type, used to serialize and deserialize instances of the types according to the
[standard Morphir JSON mapping](https://github.com/finos/morphir-elm/blob/master/docs/json-mapping.md).

With the generated Morphir.IR API, this allows you to read entire `morphir-ir.json` files
into your TypeScript program and create instances of the appropriate types. Here's how you
might do that:

    import { Morphir } from './generated/Morphir'

    function loadMorphirIR(text) {
        let data = JSON.parse(text);

        if (data['format-version'] != 2) {
            throw "Unsupported morphir-ir.json format";
        }

        return Morphir.IR.Distribution.decodeDistribution(data['distribution']);
    }
