# git-fast-export-filter

Just a dumb thing to munge the output of `git fast-export`

# Background

This was mostly a way to play around with Conduit and Megaparsec, but it solved
a specific task I was trying to accomplish.

# Operation

The filter changes the following three things:

- Sets the ref (in `reset` or `commit`) to the value of `$REF`
- Sets the email (in `author` or `committer`) to `$EMAIL`
- Moves files under `$BASE` (including trailing `/`) with extension `$EXT`
  (including the `.`) to the root path

# License

Copyright © 2017 Benjamin R. Haskell

Distributed under the MIT License (included in file: [LICENSE](LICENSE)).
