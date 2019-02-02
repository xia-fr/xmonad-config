# xmonad-config

Hi friends. And maybe random strangers that are interested in my xmonad config!

## Getting Started

First you will need to install some things!

### Installing xmonad and related things

This is the base of the things you'll need to get xmonad+gnome to work.

```
sudo apt-get install xmonad
sudo add-apt-repository ppa:gekkio/xmonad
sudo apt-get update
sudo apt-get install gnome-session-xmonad
```

### Additional things

These are some additional things that you'll need to get my xmonad config + xmobar config to work. I think I got all of them but there may be something I'm missing.

Start by getting cabal and suckless-tools. Suckless-tools gives you dmenu, cabal lets you install xmobar and yeganesh (useful wrapper thing for dmenu).
```
sudo apt-get install suckless-tools cabal-install
sudo cabal update
```
Install yeganesh.
```
sudo cabal install --global yeganesh
```
Install xmobar. Note that you can compile it with flags for various extensions. Because I use an xmobar thing to get wifi card info, I compile with iwlib.
```
sudo apt-get install xmobar
sudo apt-get install libiw-dev
sudo cabal install  xmobar --flags="with_iwlib"
```

### What to do with xmonad.hs and xmobar.hs?

Put them in ~/.xmonad which is a directory you'll have to create yourself.

### To get to xmonad+gnome

Log out, click on the gear, select the one that says xmonad+gnome. It should work for the most part, you might need to set up some other stuff.

Also, to get rid of the gnome-flashback panel (you only ever need to do this once):
```
dconf write /org/gnome/gnome-panel/layout/toplevel-id-list "['']"
```

