# xmonad + gnome config (for Ubuntu 24.04 LTS)

## Getting Started
Start with a fresh install of Ubuntu 24.04 LTS.

### Installing xmonad and related things

First we install xmonad.
```
apt-get install xmonad
```
As the package for xmonad-gnome is no longer being maintained, we must generate the
session files ourselves.
```
apt-get install gnome-session-flashback
```
- place the file `gnome-xmonad` in `/usr/libexec/`
  - modified from `/usr/libexec/gnome-flashback-metacity`
- place the file `session.conf` in `/usr/lib/systemd/user/gnome-session@gnome-xmonad.target.d/`
  - modified from `/usr/lib/systemd/user/gnome-session@gnome-flashback-metacity.target.d/session.conf`
  - you may have to create the folder `/usr/lib/systemd/user/gnome-session@gnome-xmonad.target.d/` yourself
- place the file `gnome-xmonad.session` in `/usr/share/gnome-session/sessions/`
  - modified from `/usr/share/gnome-session/sessions/gnome-flashback-metacity.session`
- place the file `gnome-xmonad.desktop` in `/usr/share/xsessions`
  - modified from `/usr/share/xsessions/gnome-flashback-metacity.desktop`  

### Additional things

These are some additional things that you'll need to get the xmonad config + xmobar config to work.

Start by getting stack and suckless-tools. Suckless-tools gives you dmenu, cabal lets you install xmobar and yeganesh (useful wrapper for dmenu).
```
sudo apt-get install suckless-tools cabal-install
cabal install yeganesh
```

Install the latest version of xmobar (sudo version comes with all extensions).
```
sudo apt-get install libiw-dev libasound2-dev libxpm-dev
sudo apt-get install xmobar
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
