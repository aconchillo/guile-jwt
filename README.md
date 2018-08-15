
# guile-jwt

guile-jwt is a JSON Web Token module for Guile.

# Installation

guile-jwt is freely available for download under the terms of the GNU General
Public License version 3 (GPLv3) as well as the GNU Lesser General Public
License version 3 (LGPLv3).

Sorry, no release is available yet.

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
