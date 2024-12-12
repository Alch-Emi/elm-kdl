# KDL to Elm and Back

[KDL (Cuddly Document Language)](https://kdl.dev/) is a human-readable, human-writable  document language for configuring your apps and data interchange.  This package makes it easy to parse KDL and translate it into Elm values and back, with a particular emphasis on top-notch helpful error reporting. 

This library is broken into four modules:
- `Kdl.Parse` for turning strings into internal KDL documents (`String -> Document`)
- `Kdl.Decode` for building decoders that turn KDL documents into Elm values (`Document -> YourConfigType`)
- `Kdl.Encode` for building encoders that turn Elm values into KDL documents (`YourConfigType -> Document`)
- `Kdl.Serialize` for turning KDL documents into strings (`Document -> String`)

The module `Kdl` holds the `Document` type which all four other modules revolve around.
