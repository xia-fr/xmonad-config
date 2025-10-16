# xmonad + gnome config (updated for Ubuntu 24.04 LTS)

## Getting Started
Start with a fresh install of Ubuntu 24.04 LTS.

### Installing xmonad + gnome

First we install xmonad.
```
apt-get install xmonad
```
As the package for xmonad-gnome is no longer being maintained, we must place the
session files ourselves.
```
apt-get install gnome-session-flashback
```
- place the file `gnome-xmonad` in `/usr/libexec/`
  - modified from `/usr/libexec/gnome-flashback-metacity`
- place `gnome-session@gnome-xmonad.target.d/session.conf` in `/usr/lib/systemd/user/`
  - modified from `/usr/lib/systemd/user/gnome-session@gnome-flashback-metacity.target.d/session.conf`
- place the file `gnome-xmonad.session` in `/usr/share/gnome-session/sessions/`
  - modified from `/usr/share/gnome-session/sessions/gnome-flashback-metacity.session`
- place the file `gnome-xmonad.desktop` in `/usr/share/xsessions`
  - modified from `/usr/share/xsessions/gnome-flashback-metacity.desktop`  

### xmobar, dmenu, yeganesh

Start by getting cabal and suckless-tools. Suckless-tools gives you dmenu, cabal lets you install xmobar.
```
apt-get install suckless-tools cabal-install
```

Install the latest version of xmobar (as referenced from xmobar documentation).
```
# required C libraries
apt-get install xorg-dev libxrandr-dev libpango1.0-dev
# optional C libraries for additional plugins
apt-get install libasound2-dev libxpm-dev libmpd-dev

cabal install xmobar -fall_extensions
```

Note: I had to manually create the symlink for `xmobar` in my `~/.local/bin` folder to get
xmonad to work nicely with it.

### To get to xmonad + gnome

Log out, click login, click on the gear in the bottom right of the screen, select the one that says xmonad + gnome. Log in. 

### Some one-time fixes

To get rid of the gnome-flashback panel:
```
dconf write /org/gnome/gnome-panel/layout/toplevel-id-list "['']"
```
To get rid of the desktop icons and the issue with the gnome desktop drawing over xmonad/xmobar (this issue popped up in 20.04LTS):
```
gsettings set org.gnome.gnome-flashback root-background true
gsettings set org.gnome.gnome-flashback desktop false
```
To remove the navigational titlebar buttons so other people have no idea how to navigate your computer:
```
gsettings set org.gnome.desktop.wm.preferences button-layout :
```
To fix the time issue if you dual boot:
```
timedatectl set-local-rtc 1 --adjust-system-clock
```

## Miscellaneous preferemces

### Themes in Gnome 42+

tbd

### Terminal appearance
To import gnome terminal profile, run:
```
dconf load /org/gnome/terminal/legacy/profiles:/ < NAME_OF_FILE.dconf
```
To get rid of the enormous terminal headerbar,
```
gsettings set org.gnome.Terminal.Legacy.Settings headerbar false
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


### Grub themeing/editing
```
sudo add-apt-repository ppa:danielrichter2007/grub-customizer
sudo apt install grub-customizer
```
