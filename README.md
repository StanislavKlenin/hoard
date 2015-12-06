# Hoard

Haskell board, work in progress

## Issues / TODO
 * Image upload and resize (for previews)
 * Removing posts
 * ~~CSS~~
 * Display original post and several latest posts in thread view
 * Pagination
 * Order thread list by recent post
 * Use current timezone
 * ~~Typesafe URLs~~

## Routes

### HTML rendering

 * /b/ - list of threads (html)       POST (urlencoded) creates new thread
 * /b/NNN - specific thread (html)    POST (urlencoded) creates new message

### REST API: later

 * /b/rest/ - list of threads (json)
 * /b/rest/NNN - specific thread (json)
 * /b/rest/NNN/MMM - specific post (json)
or
 * b/rest/thread/NNN
 * b/rest/post/NNN
