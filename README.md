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
## Setting up i3lock

I use i3lock-color for screen locking because I think the gnome-flashback screen locker is horrendous in appearance. I have the locking script hooked into a systemctl service file so it automatically runs whenever a suspend signal is sent (e.g. I close the lid of my Dell XPS 13).

### Installing i3lock-color

So... there are a lot of dependencies and they are poorly listed out. I think this is the complete list of them, but I might be missing one or two becauase I did this myself a while ago and did not write down what I installed (bad!).

```
sudo apt-get update
sudo apt-get install \
      autoconf \
      automake \
      checkinstall \
      libev-dev \
      libxcb-composite0 \
      libxcb-composite0-dev \
      libxcb-xinerama0 \
      libxcb-randr0 \
      libxcb-xinerama0-dev \
      libxcb-xkb-dev \
      libxcb-image0-dev \
      libxcb-util-dev \
      libxkbcommon-x11-dev \
      libjpeg-turbo8-dev \
      libpam0g-dev \
      pkg-config \
      xcb-proto \
      libxcb-xrm-dev \
      libxcb-randr0-dev \
      libxkbcommon-dev \
      libcairo2-dev \
      libxcb1-dev \
      libxcb-dpms0-dev \
      libxcb-image0-dev \
      libxcb-util0-dev \
      libev-dev \
      libxcb-xinerama0-dev \
      libxcb-xkb-dev
sudo add-apt-repository ppa:aguignard/ppa
sudo apt-get update
sudo apt-get install libxcb-keysyms1-dev libxcb-randr0-dev
```

Then you get the source code:
```
git clone https://github.com/PandorasFox/i3lock-color.git 
cd i3lock-color
```
Do the thing (I believe):
```
sudo autoreconf -i && ./configure && make
make -j
sudo make install
```

