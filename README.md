# om-tops

A Om project designed to show optmistic render on the client
side while asynchronously calling the server for validation. This project
also has tests using [clojurescript.test][3] and [double-check][4].

The client pools the server each second for a random word and shows the last
10 words. The user can submit new words, which will appear on the list
immediately and submited to the server.

The server has an artificial delay of 2 seconds before returning a response.
If the word has more than 6 characteres the server will tell that is a
invalid word and the client will show in a different manner valid and invalid
words.

## Dependencies

- java 1.7+
- [leiningen][1]

## Usage

1. Start the auto-compiler. In a terminal:

    ```bash
    $ lein cljsbuild auto dev
    ```

2. Open a repl. In another terminal:

    ```bash
    $ lein repl
    ```

3. Require the core namespace. This will start the server. On the repl:

    ```
    user=> (require 'om-tops.core)
    ```

4. Go to [http://localhost:8001][2] in your browser.

## Testing

1. Start the auto-compiler. In a terminal:

    ```bash
    $ lein cljsbuild auto test
    ```

2. When you save a file, cljsbuild will recompile and then the tests will
auto run.

## License

Copyright © 2014, Marcelo Nomoto. All rights reserved.
```
The use and distribution terms for this software are covered by the Eclipse
Public License 1.0 (http://opensource.org/licenses/EPL-1.0) which can
be found in the file epl-v10.html at the root of this distribution. By using
this software in any fashion, you are agreeing to be bound by the terms of
this license. You must not remove this notice, or any other, from this software.
```
[1]: https://github.com/technomancy/leiningen
[2]: http://localhost:8001
[3]: https://github.com/cemerick/clojurescript.test
[4]: https://github.com/cemerick/double-check
