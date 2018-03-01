# purescript-uri

[![Latest release](http://img.shields.io/github/release/slamdata/purescript-uri.svg)](https://github.com/slamdata/purescript-uri/releases)
[![Build status](https://travis-ci.org/slamdata/purescript-uri.svg?branch=master)](https://travis-ci.org/slamdata/purescript-uri)
[![Dependency status](https://img.shields.io/librariesio/github/slamdata/purescript-uri.svg)](https://libraries.io/github/slamdata/purescript-uri)

A type-safe parser, printer, and ADT for URLs and URIs based on [RFC 3986](http://tools.ietf.org/html/rfc3986).

## Installation

```
bower install purescript-uri
```

## Getting started

The types and names here are a fairly faithful representation of the components described in the spec.

- [`URI`][URI] is for absolutely specified URIs that can also have path, query, and fragment (hash) parts.
- [`AbsoluteURI`][AbsoluteURI] is a variation on `URI` that drops the ability for the URI to carry a fragment.
- [`RelativeRef`][RelativeRef] is for relatively specified URIs that can also have path, query, and fragment (hash) parts.
- [`URIRef`][URIRef] is combination of `URI` and `RelativeRef`, allowing the full range of representable URIs.

The absolute/relative terminology when applied to URIs does not relate to the paths that a URI may carry, it refers to whether the URI has a "scheme" or not. For example `http://example.com` and `file://../test.txt` are absolute URIs but `//example.com` and `/test.txt` are relative.

Assuming none of the `unsafe`-prefixed functions are used when constructing a URI, it should be impossible to construct a URI that is invalid using the types this library provides*. The slight downside of this is the data structures are relatively complex so as to only admit correct possibilities.

\* Actually, there is one exception to that - `IPv6Address` is far too forgiving in what it allows currently. Contributions welcome!

### URI component representations

Due to the differing needs of users of this library, the URI types are all parameterised to allow for custom representations to be used for parts of the URI. Take a look at the most heavily parametrised type, `URIRef`:

``` purescript
type URIRef userInfo hosts path hierPath relPath query fragment = ...
```

This allows us to provide hooks into the parsing and printing processes for a URI, so that types better suited to the intended use case can be used.

Taking `userInfo` as an example, according to the spec, the `user-info` part of an authority is just an arbitrary string of characters terminated by an `@` before a hostname. An extremely common usage for this is the `user:password` scheme, so by leaving the choice of representation as a type variable we can switch it out for a type specifically designed to handle that (this library includes one actually, under [`URI.Extra.UserPassInfo`][UserPassInfo]).

### App-specific URI type definitions

When using this library, you'll probably want to define type synonyms for the URIs that make sense for your use case. A URI type that uses the simple representations for each component will look something like this:

``` purescript
type MyURI = URIRef UserInfo (HostPortPair Host Port) Path HierPath RelPath Query Fragment
```

Along with these types, you'll want to define an options record that specifies how to parse and print URIs that look like this:

``` purescript
options ∷ Record (URIRefOptions UserInfo (HostPortPair Host Port) Path HierPath RelPath Query Fragment)
options =
  { parseUserInfo: pure
  , printUserInfo: id
  , parseHosts: HostPortPair.parser pure pure
  , printHosts: HostPortPair.print id id
  , parsePath: pure
  , printPath: id
  , parseHierPath: pure
  , printHierPath: id
  , parseRelPath: pure
  , printRelPath: id
  , parseQuery: pure
  , printQuery: id
  , parseFragment: pure
  , printFragment: id
  }
```

As you can see by all the `pure` and `id`, we're not doing a whole lot here. `parseHosts` is a bit of an exception, but that's just due to the way that case is handled (see [later in this README](#host-parsing) for more details about that).

These types ([`UserInfo`][UserInfo], [`HostPortPair`][HostPortPair], [`Host`][Host], etc.) are all provided by the library, and where necessary can only be constructed via smart constructor. This ensures that percent-encoding is applied to characters where necessary to ensure the constructed values will print as valid URIs, and so on.

If we decided that we wanted to support `user:password` style user-info, we'd modify this by changing our type to use [`UserPassInfo`][UserPassInfo]:

``` purescript
type MyURI = URIRef UserPassInfo (HostPortPair Host Port) Path HierPath RelPath Query Fragment
```

And update our options to use the appropriate parse/print functions accordingly:

``` purescript
options ∷ Record (URIRefOptions UserPassInfo (HostPortPair Host Port) Path HierPath RelPath Query Fragment)
options =
  { parseUserInfo: UserPassInfo.parse
  , printUserInfo: UserPassInfo.print
  , ...
```

### Writing custom component types

These `parse/print` functions all share much the same shape of signature. For the case in the previous example, they come out as:

``` purescript
parseUserInfo ∷ UserInfo → Either URIPartParseError UserPassInfo
printUserInfo ∷ UserPassInfo → UserInfo
```

So you can see that for each component, when the options hooks/custom representation stuff is used, we take one of these library-provided component types and parse it into our new representation, and also print it back to that simple type later.

Each of the library-provided component types have a `toString` function that extracts the inner value as a string after applying percent-decoding, and an `unsafeToString` that provides exactly the value that was parsed, preserving percent decoding. Similarly, there's a `fromString` that performs the minimal amount of required percent encoding for that part of the URI, and an `unsafeFromString` that performs no encoding at all.

You may ask why it's ever useful to have access to the encoded values, or to be able to print without encoding, so here's a motivating example:

For the [`UserPassInfo`][UserPassInfo] example, the typical way of encoding a username or password that contains a colon within it is to use `%3A` (`us:er` becomes `us%3Aer`). This allows colons-within-the-values to be recongised as independent from the colon-separating-username-and-password (`us%3Aer:password`).

According to the spec it is not a requirement to encode colons in this part of the URI scheme, so just using [`toString`][UserInfo.toString] on `us:er` will get us back a `us:er`, resulting in `us:er:password`, so we'd have no way of knowing where the user ends and where the password starts.

The solution when printing is to do some custom encoding that also replaces `:` with `%3A` for the user/password parts, and then joins them with the unencoded `:` afterwards. If we constructed the resulting [`UserInfo`][UserInfo] value with [`fromString`][UserInfo.fromString] it would re-encode our already encoded user/password parts (giving us `%253A` instead of `%3A`), so we use [`unsafeFromString`][UserInfo.unsafeFromString] since we've done the encoding ourselves.

Similarly, when parsing these values back, we want to split on `:` and then percent-decode the user/password parts individually, so we need to use [`unsafeToString`][UserInfo.unsafeToString] to ensure we get the encoded version.

Another example where this sort of thing might be useful is if you would like to encode/decode spaces in paths as `+` rather than `%20`. Having the ability to hook into the parse/print stage and choose to examine or print with or without percent encoding/decoding applied gives us the flexibility to produce and consume values exactly as we want, rather than the library attempting to know best in all cases.

### Host parsing

The host printing/parsing setup is a little different. This is to accommodate something that lies outside of the RFC 3986 spec: multiple host definitions within a URI. The motivating case for this is things like connection strings for MongoDB, where host/port pairs can be defined separated by commas within a single URI:

```
mongodb://db1.example.net:27017,db2.example.net:2500/?replicaSet=test
```

This doesn't jive with what is said in RFC 3986, as there a comma is allowed as part of a hostname, but the multiple ports don't fit into the schema. To get around this, when it comes to parsing hosts, the parsing is entirely handed over to the `parseHosts` parser in the options (in the cases for the other parameters, a normal function is run on a value that has been parsed according to the spec already).

For normal URIs the [`HostPortPair`][HostPortPair] parser/printer should serve well enough. This accepts functions to deal with the host/port parts allowing for those aspects to be dealt with much like all the other options.

For URIs that are like the MongoDB connection string, this library provides [`URI.Extra.MultiHostPortPair`][MultiHostPortPair]. Given that both of these allow for custom `Host` / `Port` types, hopefully nobody else will need to write anything for the general host-section-parsing part!

## Further documentation

[The tests](test/) contain many examples of URI constructions using the basic types this library provides.

Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-uri).

[AbsoluteURI]: https://pursuit.purescript.org/packages/purescript-uri/docs/URI.AbsoluteURI
[Host]: https://pursuit.purescript.org/packages/purescript-uri/docs/URI.Host
[HostPortPair]: https://pursuit.purescript.org/packages/purescript-uri/docs/URI.HostPortPair
[MultiHostPortPair]: https://pursuit.purescript.org/packages/purescript-uri/docs/URI.Extra.MultiHostPortPair
[RelativeRef]: https://pursuit.purescript.org/packages/purescript-uri/docs/URI.RelativeRef
[URI]: https://pursuit.purescript.org/packages/purescript-uri/docs/URI.URI
[URIRef]: https://pursuit.purescript.org/packages/purescript-uri/docs/URI.URIRef
[UserInfo.fromString]: https://pursuit.purescript.org/packages/purescript-uri/docs/URI.UserInfo#v:fromString
[UserInfo.toString]: https://pursuit.purescript.org/packages/purescript-uri/docs/URI.UserInfo#v:toString
[UserInfo.unsafeFromString]: https://pursuit.purescript.org/packages/purescript-uri/docs/URI.UserInfo#v:unsafeFromString
[UserInfo.unsafeToString]: https://pursuit.purescript.org/packages/purescript-uri/docs/URI.UserInfo#v:unsafeToString
[UserInfo]: https://pursuit.purescript.org/packages/purescript-uri/docs/URI.UserInfo
[UserPassInfo]: https://pursuit.purescript.org/packages/purescript-uri/docs/URI.Extra.UserPassInfo
