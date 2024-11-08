# Elm <-> JS Bytes Interop Benchmark


> This is a fork of https://github.com/anmolitor/elm-bytes-ports-benchmark
> which adds more intermediate representation candidates.

There are various usecases in which we want to pass Bytes directly from Javascript
to the Elm application and vice versa. A concrete usecase I struggle with is Websockets with binary data (protobuf encoding).
Unfortunately, Elm's primary interop mechanism, Ports, is based on JSON encoding and as such unable
to transfer Bytes (in the concrete form of Uint8Array, DataView or ArrayBuffer).

The goal of this benchmark is to compare the different available workarounds.

## TL;DR:

For transferring small to medium-sized amounts (< 3Mb) of data at a time, a JSON compatible encoding like [the ascii one](#ascii-1-char--1-byte) is fine.
For big amounts of data, use [the HTTP prototype hack](#http-taskport). The overhead of the HTTP method is remarkably low, so there is no benefit of implementing multiple methods and deciding which method to use depending on size.

### JavaScript -> Elm

<img src='./results/jsToElm.png' width=100% alt="Graph displaying the results for sending bytes from JavaScript to Elm">

### Elm -> JavaScript

<img src='./results/elmToJs.png' width=100% alt="Graph displaying the results for sending bytes from Elm to JavaScript">

Note: "identity" measures overhead due to the method used for benchmarking by just passing the Bytes back and forth as Json.Encode.Value.

## The contestants

### Int Array / Int List

While Bytes cannot be directly represented in JSON,
an array of numbers can. Thus we can copy each element of the `Uint8Array` to a standard JS array and revert the process on the Elm side via the `Bytes.Encode`/`Bytes.Decode` API.

This seems to perform reasonable well for small workloads but causes slowdown for large Arrays due to the O(n) copying.

### Base64

Similar approach but more traditional in a sense, since bytes are often encoded as Base64 in the browser, for example in URLs. Faster than the Int Array approach. I would be interested to compare the memory usages between the two, but I was not sure how to best accomplish that. Suggestions/PRs welcome!

### Hex (2 chars = 1 byte)

Like Base64 but significantly faster for js → elm and a bit slower for elm → js (TODO why?)

### "Ascii" (1 char = 1 byte)

Like Base64 and Hex but faster than both of them.


### File API

**Warning**: Calling `File.toBytes` apparently causes memory to be leaked: https://github.com/elm/file/issues/31

The package `elm/file` includes a `File.decoder` as well as a `File.toBytes` function.

Therefore we can call `new File(bytes, '')` on the JS side and pass that into Elm over a port without any trouble. The benchmark shows the overhead is constant/independent of the bytes size.

This seems to perform consistently worse that the below Http approach so I would not recommend it.

### Http "Taskport"

I call this one Taskport, since the idea is based on the
[elm-taskport](https://package.elm-lang.org/packages/lobanov/elm-taskport/2.0.1/) library.

Essentially, we send a HTTP request from Elm with a nonsense URL like `elm://post`.
On the JS side, we monkeypatch the XHRHttpRequest prototype by intercepting the `open` and `send` methods. We check for the nonsense URL or prefix and send the bytes from JS directly instead of sending a request.

This, for some reason, has remarkably low overhead according to the benchmark and is thus the best solution from Elm to JS for large bytes size. Even for small byte sizes and the impractical JS to Elm direction (JS tells the Elm runtime over a port that it should get some data via HTTP), the performance rivals the Json-based approaches, so you shouldn't need to implement multiple approaches.

## Method used for benchmarking

Each contestant has a JS file which

- implements a `send` method with the signature `Uint8Array -> Promise<number>`. The returned number is the array length and used as a sanity check that the bytes were passed successfully
- implements a `receive` method with the signature `() -> Promise<Uint8Array>`.

The benchmark in `dist/index.js` then executes each send method for each contestant sequentially and measures the time via
the Browsers performance API. Methods are executed `n = 100` times and the average time is used to hopefully even out garbage collector spikes a bit. Afterwards, each receive method is called sequentially with the same approach.

Uint8Arrays of different sizes are generated outside of the timing in Javascript.

Chart.js is used to visualize the results.
