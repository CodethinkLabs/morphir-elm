/*
Copyright 2021 Morgan Stanley

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
 */

class DecodeError extends Error {}

Object.defineProperty(DecodeError.prototype, 'name', {
    value: 'DecodeError',
});

type GenericDecoder = (input: any) => any;
type DecoderList = Array<GenericDecoder>;
type DecoderMap = Map<string, GenericDecoder>;

export function decodeUnit(input: any): [] {
    return [];
}

export function decodeBoolean(input: any): boolean {
    if (typeof(input) != "boolean") {
        throw new DecodeError(`Expected bool, got ${typeof(input)}`);
    }
    return input;
}

export function decodeChar(input: any): string {
    if (typeof(input) != "string") {
        throw new DecodeError(`Expected char, got ${typeof(input)}`);
    }
    if (input.length != 1) {
        throw new DecodeError(`Expected char, got string`);
    }
    return input;
}

export function decodeString(input: any): string {
    if (typeof(input) != "string") {
        throw new DecodeError(`Expected string, got ${typeof(input)}`);
    }
    return input;
}

export function decodeInt(input: any): number {
    if (typeof(input) != "number") {
        throw new DecodeError(`Expected int, got ${typeof(input)}`);
    }
    return input;
}

export function decodeFloat(input: any): number {
    if (typeof(input) != "number") {
        throw new DecodeError(`Expected float, got ${typeof(input)}`);
    }
    return input;
}

export function decodeArray<T>(decodeElement: (any) => T,
                               input: any): Array<T> {
    if (!(input instanceof Array)) {
        throw new DecodeError(`Expected Array, got ${typeof(input)}`);
    }

    const inputArray: Array = input;
    return inputArray.map(decodeElement);
}

export function decodeCustomType(kind: string,
                                 mapDecoders: DecoderList,
                                 input: any): object {
    if (input[0].kind != kind) {
        throw new DecodeError(`Expected kind ${kind}, got ${input[0].kind}`);
    }

    const argCount = input.length - 1;
    if (argCount != argDecoders.length) {
        throw new DecodeError(`Expected ${argDecoders.length} args for custom type "${kind}", got ${argCount}`);
    }

    result = {
        kind: kind
    }
    for (i = 0. i < argDecoders.length; i++) {
        paramName = `arg${i + 1}`;
        result[paramName] = argDecoders[i](input[i + 1]);
    }
}

// FIXME: dict are represented as arrays right now
export function decodeDict<K,V>(decodeKey: (any) => K, decodeValue: (any) => V, input: any): Array<[K,V]> {
    if (!(input instanceof Array)) {
        throw new DecodeError(`Expected array, got ${typeof(input)}`);
    }
    const inputArray: Array<any> = input;
    return inputArray.map((item: any) => {
        if (!(item instanceof Array)) {
            throw new DecodeError(`Expected array, got ${typeof(item)}`);
        }

        const itemArray: Array<any> = item;
        return [decodeKey(itemArray[0]), decodeValue(itemArray[1])];
    });
}

type DecoderMap = Map<string, (any) => any>;

export function decodeRecord(fieldDecoders: DecoderMap, input: any): object {
    if (!(input instanceof Object)) {
        throw new DecodeError(`Expected Object, got ${typeof(input)}`);
    }

    const inputObject: object = input;

    const fieldNames: Array<string> = Array.from(fieldDecoders.keys());
    for (var field in fieldNames) {
        if (!(field in Object.keys(input))) {
            throw new DecodeError(`Expected field ${field} was not found`);
        }
    }
    if (Object.keys(inputObject).length > fieldNames.length) {
        throw new DecodeError(`Input object has extra fields, expected ${fieldNames.length}, got ${input.keys().length}`);
    }

    var result = new Object;
    fieldDecoders.forEach((decoder: GenericDecoder, name: string) => {
        result[name] = decoder(inputObject[name]);
    });

    return result;
}

export function decodeTuple(elementDecoders: DecoderList, input: any): Array<any> {
    if (!(input instanceof Array)) {
        throw new DecodeError(`Expected Array, got ${typeof(input)}`);
    }

    const inputArray: Array = input;
    let result = [];
    for (i = 0; i < inputArray.length; i++) {
        result.push(elementDecoders[i](inputArray[i]));
    }
    return result;
}
