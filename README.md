# mc-level
Copyright © 2012 Bart Massey

*This is a work in progress--not at all ready for use yet.*

This Haskell code is intended to interact with Minecraft
<http://minecraft.net> "level"s, also known as worlds. It is
inspired by `acfoltzer`'s `minecraft-data`
<http://github.com/acfoltzer/minecraft-data> project, and
requires an updated version of the corresponding
`nbt` <http://github.com/acfoltzer/nbt.git> package,
available at <http://github.com/BartMassey/nbt>, to
compile. (Pull requests are in, so hopefully this extra repo
can go away.)

One goal of `mc-level` was to provide human-friendly XML
serialization of Minecraft levels. The immediate need that
led to its creation was to find lost items. XML
serialization would enable using XPath instead of having to
invent some complicated infrastructure. There are other uses
for XML-serialized worlds: they can be subsetted easily, and
they are easy to generate or edit.

Sadly, having done the XML thing, I was unable to find any
decent-performance XPath tool for Minecraft-level-sized XML
databases. So I gave up, and am working on a finder that
works directly with the NBT in the level structure. Examples
of the planned finder syntax are:

  * Find Diamond Pickaxes:

        item=278

  * Find Diamond Pickaxes with Productivity 2 or more:

        item=278[ench=32[level>=2]]

Yes, eventually I'll build in the symbolic names so that you
don't have to go look them up at
<http://www.minecraftwiki.net/wiki/Data_values>.

To build this, first install
<http://github.com/BartMassey/nbt>.  Then a simple "`cabal
configure; cabal build`" should get you there.

This work is available under a 3-clause BSD license. See the
file `COPYING` in this distribution for license terms.
