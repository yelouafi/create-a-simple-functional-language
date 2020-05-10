# cerate-a-simple-functional-language

Companion code for https://bit.ly/2WCoR0f

# Running

The easiest way is to use [parcel](https://parceljs.org/)

```sh
parcel index.html
```

Then you can use `_eval` in the developper console. Example

```js
_eval("let inc = fn(x: int) => x + 1 in inc(100)");
```

# More features

I'll be adding occasionnally examples for supporting more features. The
examples will be in separate branches.

- boolean types && `if` construct, also has more operations && priority/associativity ([example](https://github.com/yelouafi/create-a-simple-functional-language/tree/mono-extended))

- record types ([example](https://github.com/yelouafi/create-a-simple-functional-language/tree/mono-records))
