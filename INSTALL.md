Run these:

Install the needed libraries:
```
opam install graphics ounit
```

Check if the game runs on your system:
```
make check
```

If the above check doesn't work, you may need to ACTUALLY install the Graphics library.

It may not have been installed due to OCaml installation weirdness.

There are numerous posts on Campuswire on how to install the Graphics library (just search "graphics").

### For WSL:

Try to install XMing. After doing this, try `make check` again. If this fails, continue below. 

See post #1935 for an installation guide. 

If that doesn't work, maybe also see post #1904:

> I had the same problem. I fixed it by running ```opam switch create 4.08.1``` (this took a very long time ~30 mins) and then installing the graphics package with ```opam install graphics``` 

Note that you may actually need to upgrade to a later version (4.09.x) for this to work.

### For MacOS:

Try to install XQuartz. After doing this, try `make check` again. If this fails, continue below. 

See post #2130

If none of the above work, you can probably just install it straight from the [source](https://github.com/ocaml/graphics)


Play the game:
```
make play
```
Hit escape to exit the game.

Note that on harder difficulties against the computer, the game may slow down a bit,
a consequence of the depth our minmax algorithm searches. Just wait a second for the computer to finish thinking.