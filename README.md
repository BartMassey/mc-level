# mc-level
Copyright © 2012 Bart Massey

*This is a work in progress--not at all ready for use yet.*

This Haskell code is intended to interact with Minecraft
<http://minecraft.net> "level"s, also known as worlds. It is
inspired by `acfoltzer`'s `minecraft-data`
<http://github.com/acfoltzer/minecraft-data> project, and
requires a slightly-updated version of the corresponding
`nbt` <http://github.com/acfoltzer/nbt.git> package,
available at <http://github.com/BartMassey/nbt>, to
compile. (A pull request is in, so hopefully this extra repo
can go away.)

The chief goal of `mc-level` is to provide human-friendly
XML serialization of Minecraft levels. The immediate need
that led to its creation was to find lost items. XML
serialization would enable using XPath instead of having to
invent some complicated infrastructure. There are other uses
for XML-serialized worlds: they compress nicely (better than
NBT), they can be subsetted easily, and they are easy to
generate or edit.

To build this, first install
<http://github.com/BartMassey/nbt>.  Then a simple "`cabal
configure; cabal build`" should get you there.

This work is available under a 3-clause BSD license. See the
file `COPYING` in this distribution for license terms.
