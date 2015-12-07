# Hoard

Haskell board, work in progress

## Build

Cabal sandbox is recommended
```
cabal sandbox init
cabal install --only-dependencies --max-backjumps 16384
cabal run
```
No configuration present currently,
hoard will use `./state/` for storage and run on `localhost:8000`

## Issues / TODO
 * Image upload and resize (for previews)
 * Removing posts
 * ~~CSS~~
 * ~~Display original post and several latest posts in thread view~~
 * Pagination
 * Order thread list by recent post
 * Use current timezone
 * ~~Typesafe URLs~~
 * REST API

## Routes

### HTML rendering

 * /b/ - list of threads (html);
   POST (multipart/form-data) creates new thread
 * /b/NNN - specific thread (html);
   POST (multipart/form-data) creates new message

### REST API: later

 * /b/rest/ - list of threads (json)
 * /b/rest/NNN - specific thread (json)
 * /b/rest/NNN/MMM - specific post (json)

or
 * b/rest/thread/NNN
 * b/rest/post/NNN
