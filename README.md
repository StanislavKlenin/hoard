# Hoard

Haskell board, work in progress

## Build

[libgd](http://libgd.github.io/) required.

[Haskell stack](https://www.haskellstack.org/) is recommened.
```
stack setup
stack build
stack exec hoard-run
```

Alternatively, Cabal sandbox might still work
```
cabal sandbox init
cabal install --only-dependencies --max-backjumps 16384
cabal run
```
## Configuration

Hoard loads configuration from an optional config file
passed as the first (and only) command line argument
```
stack exec hoard-run sample.conf
```
or
```
cabal run hoard.conf
```
or
```
/path/to/hoard hoard.conf
```
hoard.conf is a `name = value` text config with following fields:
 * `port`: port to listen at;
    default: `8000`
 * `domain`: hostname part of generated urls; should contain scheme, host and (optionally) port, such as `http://domain:8000`; note: port needs not be the same as listen port;
    default: `http://localhost:8000`
 * `storage`: AcidState storage directory;
    default: `/tmp`
 * `tmp`: temporary directory for file uploads;
    default: `/tmp`
 * `static`: directory for static content (images, maybe styles and scripts);
    default: `/tmp`
 * `prefix`: URL prefix (site home);
    default: empty

## Issues / TODO
 * ~~Home/root url prefix~~
 * ~~Image upload and resize (for previews)~~
 * ~~Removing posts~~
 * ~~CSS~~
 * ~~Display original post and several latest posts in thread view~~
 * Pagination
 * ~~Order thread list by recent post~~
 * Use current timezone
 * ~~Typesafe URLs~~
 * REST API
 * Markup parser
 * Admin console

## Routes

### HTML rendering

For a board /b/,
 * /b/ - list of threads (html);
   POST (multipart/form-data) creates new thread
 * /b/NNN - specific thread (html);
   POST (multipart/form-data) creates new message
 * /b/src/NNNN.ext - images

### REST API: later

 * /b/rest/ - list of threads (json)
 * /b/rest/NNN - specific thread (json)
 * /b/rest/NNN/MMM - specific post (json)

or
 * b/rest/thread/NNN
 * b/rest/post/NNN
