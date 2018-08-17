
# guile-jwt

guile-jwt is a JSON Web Token module for Guile. JSON Web Tokens are an open,
industry standard ([RFC 7519](https://tools.ietf.org/html/rfc7519)) method for
representing claims securely between two parties. guile-jwt allows you to
decode, verify and generate JWT.

- Supported algorithms: HS256, HS384, HS512.

# Installation

guile-jwt is freely available for download under the terms of the GNU General
Public License version 3 (GPLv3) as well as the GNU Lesser General Public
License version 3 (LGPLv3).

Download the latest tarball and untar it:

- [guile-jwt-0.1.0.tar.gz](http://download.savannah.gnu.org/releases/guile-jwt/guile-jwt-0.1.0.tar.gz)

If you are cloning the repository make sure you run this first:

    $ autoreconf -vif

Then, run the typical sequence:

    $ ./configure --prefix=<guile-prefix>
    $ make
    $ sudo make install

Where <guile-prefix> should preferably be the same as your system Guile
installation directory (e.g. /usr).

If everything installed successfully you should be up and running:

    $ guile
    scheme@(guile-user)> (use-modules (jwt))
    scheme@(guile-user)> (jwt-encode '((test . "1234567890")) "secret"))
    "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9..."

It might be that you installed guile-jwt somewhere differently than
your system's Guile. If so, you need to indicate Guile where to find
guile-jwt, for example:

    $ GUILE_LOAD_PATH=/usr/local/share/guile/site guile

# Usage

guile-jwt provides a couple procedures, one to generate a JWT and another to
verify it.

To start using guile-jwt you simply need to load the module:

    scheme@(guile-user)> (use-modules (jwt))

## Procedures

- (**jwt-encode** payload secret #:key algorithm header) : Creates a new JWT
  with the given *payload* and the specified *secret*. Throws
  *jwt-invalid-algorithm* if a not supported or invalid algorithm is provided.

  - *algorithm* : symbol specifying signing algorithm. Defaults to HS256.
  - *header* : association list with additional header fields.

  **Returns** : a JWT string.

- (**jwt-decode** jwt secret #:key verify) : Decodes the given JWT *jwt* with
  the specified *secret*. The algorithm to verify the signature will be
  extracted from the JWT header.

  - *verify* : verify JWT signature and fields. Defaults to true.

  **Returns** : a hash-table with the payload.

  **Throws**

  - *jwt-invalid-algorithm* : if a not supported or invalid algorithm is found
    in the header.
  - *jwt-invalid* : if the JWT is invalid.

## Examples

- Generate a JWT with the default HS256 algorithm and a given payload:

```
scheme@(guile-user)> (jwt-encode '((test . "1234567890")) "secret")
"...7HFTJOiPGdXmk0G1rqzhpZV_POhjRrTBlR_WOC8-_L0"
```

- Generate a JWT with the HS512 algorithm and a given payload:

```
scheme@(guile-user)> (jwt-encode '((test . "1234567890")) "secret" #:algorithm 'HS512)
"...Yv9ivHlSk-94djH4Qv_k1nkeyAQzN41iN9aEcdQIOgOkmxp0zb725ogOFm52snD_DfQk8u1-tayRXj6cMlagtA"
```

- Generate a JWT with a payload and additional headers:

```
scheme@(guile-user)> (jwt-encode '((test . "1234567890")) "secret" #:header '((foo . bar)))
"...MIgbpMmk9MkzbG0a4NYttv5FoJx72pz6wL0iu_pPSkE"
```

- Verify a JWT:

```
scheme@(guile-user)> (define jwt (jwt-encode '((test . "1234567890")) "secret"))
scheme@(guile-user)> (define payload (jwt-decode jwt "secret"))
scheme@(guile-user)> (hash-ref payload "test")
"1234567890"
```
