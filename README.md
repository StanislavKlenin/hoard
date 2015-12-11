# Hoard

Haskell board, work in progress

## Build

[libdg](http://libgd.github.io/) required.

Cabal sandbox is recommended
```
cabal sandbox init
cabal install --only-dependencies --max-backjumps 16384
cabal run
```
## Configuration

Hoard loads configuration from an optional config file
passed as the first (and only) command line argument
```
cabal run hoard.conf
```
or
```
/path/to/hoard hoard.conf
```
hoard.conf is a `name = value` text config fith following fields:
 * `host`: hostname (used to generate urls, mostly);
    default: `localhost`
 * `port`: port to listen at;
    default: `8000`
 * `storage`: AcidState storage directory;
    default: `/tmp`
 * `tmp`: temporary directory for file uploads;
    default: `/tmp`
 * `static`: directory for static content (images, maybe stles and scripts);
    default: `/tmp`

## Issues / TODO
 * ~~Image upload and resize (for previews)~~
 * Removing posts
 * ~~CSS~~
 * ~~Display original post and several latest posts in thread view~~
 * Pagination
 * ~~Order thread list by recent post~~
 * Use current timezone
 * ~~Typesafe URLs~~
 * REST API

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
