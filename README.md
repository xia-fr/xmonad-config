# xmonad + gnome config (for Ubuntu 20.04 LTS)

## Getting Started

We begin here with a fresh install of Ubuntu 20.04 LTS. After doing some basic updating of packages, etc., we just need to install some things to get going with xmonad + gnome.

### Installing xmonad and related things

This is the base of the things you'll need to get xmonad+gnome to work.

```
sudo apt-get install xmonad
sudo add-apt-repository ppa:gekkio/xmonad
sudo apt-get update
sudo apt-get install gnome-session-xmonad
```

### Additional things

These are some additional things that you'll need to get the xmonad config + xmobar config to work.

Start by getting cabal and suckless-tools. Suckless-tools gives you dmenu, cabal lets you install xmobar and yeganesh (useful wrapper thing for dmenu).
```
sudo apt-get install suckless-tools cabal-install
sudo cabal update
```
Install yeganesh. Note that there are some warnings here about global installation and cabal because they're changing things. It still ended up working for me at the point in which I wrote this, but it might not in the future.
```
stack install yeganesh
```
Install the latest version of xmobar. Note this requires installing stack, and then setting it up.
```
sudo apt-get install stack
stack update
stack setup
sudo apt-get install libiw-dev
stack install --flag xmobar:all_all_extensions xmobar
```
Might also need:
```
sudo apt install libasound2-dev
```

### What to do with xmonad.hs and xmobar.hs?

Put them in ~/.xmonad which is a directory you'll have to create yourself. The 'lib' folder also needs to go into the ~/.xmonad folder.

### To get to xmonad + gnome

Log out, click on the gear, select the one that says xmonad + gnome. Log in. There are some things you will need to run (only once) to get various gnome 
panels out of the way of xmonad.

To get rid of the gnome-flashback panel:
```
dconf write /org/gnome/gnome-panel/layout/toplevel-id-list "['']"
```
To get rid of the desktop icons and the issue with the gnome desktop drawing over xmonad/xmobar (this seems to be a 20.04 related issue):
```
gsettings set org.gnome.gnome-flashback root-background true
gsettings set org.gnome.gnome-flashback desktop false
```
## Miscellaneous

### Icon pack and fonts

Install the papirus icon theme! Personally I am also particular to Inconsolata as a font.

### Terminal appearance

To import gnome terminal profile, run:
```
dconf load /org/gnome/terminal/legacy/profiles:/ < NAME_OF_FILE.dconf
```
To get rid of the enormous headerbar,
```
gsettings set org.gnome.Terminal.Legacy.Settings headerbar false
```
### Git
```
sudo apt install git
```
### Latex
```
sudo apt install texlive texlive-latex-extra cm-super
```
Also for pdf viewer Zathura for integration with vim:
```
sudo apt install zathura xdotool
```
### Vim

Put the .vimrc file in home directory.

```
sudo apt install vim
curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
```
Open vim and `:PlugInstall` to have vim-plug install plugins as listed in .vimrc.

Also make sure to install `vim-gtk3` so the copy/paste options interact with system clipboard.

### Firefox

In preferences, density is `Compact` and `Title Bar` is checked.

### Screenshots

Disable the default keyboard shortcuts for screenshots, custom `gnome-screenshot` command (ssarea.sh in ~/.local/bin):
```
#!/bin/bash
## Screenshot Area Script
DATE=$(date +%Y-%m-%d-%H:%M:%S)
gnome-screenshot -c -a -f /home/USER/Pictures/Screenshots/Screenshot-$DATE.png
```

In gnome control center, keyboard shortcut command: `sh -c '/home/<USER>/.local/bin/ssarea.sh'`

Make it executable with `sudo chmod a+x '/home/<USER>/.local/bin/ssarea.sh'`

### Titlebar
```
gsettings set org.gnome.desktop.wm.preferences button-layout :
```
### GTK Themes/Grub Themes

I'm quite fond of Equilux, Nordic Darker, or Kripton (with a few modifications).

Use `sudo apt-get install grub-customizer` to get the grub customizer to edit list entries. Install a nice
grub theme off the internet. :)

### Dual boot time issue
```
timedatectl set-local-rtc 1 --adjust-system-clock
```
