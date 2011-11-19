# XVL.hs

This is a(n attempt to write) package for parsing XVL (_eXtended Values Language_) in Haskell.

## XVL

_XVL_ is a simple data description language which bears some similarity to JSON. It is something
like a generalized, hierarchical INI format with C-like syntax (if that even makes sense ;>).

### Concepts

* document is a collection of items and/or sections
* sections may contain items and/or nested sections
* items are simple literals or key-value pairs
* values are simple literals, quoted strings or arrays (of values)

### Syntax

Syntax is intended to be very lax, allowing any kind of separators that make sense (including just whitespace)
and supporting strings without surrounding quotes. Comments start with a hash (<code>#</code>) and continue
until end of the line.  

### Example

This example has two main sections. The second one contains a nested subsection.

    application {
        name = SomeAwesomeApp
        version = 0.42
        systems = { Windows, Linux, "Mac OS X" } # we can separate values with comma
    }
    
    config {
        user {
            first-name = John
            last-name = Smith
            authenticated # item without value
        }
        
        permissions = { disk-read disk-write network-read }
    }


## The library

<code>Text.XVL</code> is an attempt to implement a parser for XVL in Haskell using Parsec module. Since I'm
still a Haskell newbie, it uses mostly generic <code>GenParser</code> combinators rather than the <code>TokenParser</code>
ones, which would likely be more appropriate.

### Status

Under development. It probably doesn't work yet :)