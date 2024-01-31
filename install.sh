#!/bin/zsh

# Install xCode cli tools
echo "Installing commandline tools..."
xcode-select --install

# Homebrew
## Install
echo "Installing Brew..."
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
brew analytics off

## Taps
echo "Tapping Brew..."
brew tap homebrew/cask-fonts
brew tap FelixKratz/formulae
brew tap koekeishiya/formulae
brew tap d12frosted/emacs-plus
brew tap artginzburg/tap
brew tap sbfnk/formulae

## Formulae
echo "Installing Brew Formulae..."
### Essentials
brew install gsl
brew install llvm
brew install boost
brew install libomp
brew install python

### Tools
brew install wget
brew install tmux
brew install ripgrep
brew install mas
brew install gh
brew install ifstat
brew install switchaudio-osx
brew install sketchybar
brew install borders
brew install yabai
brew install nnn
brew install jq

## sudo
brew install --cask git-credential-manager
brew install sudo-touchid
brew services start sudo-touchid

### Science
brew install mactex

### Terminal
brew install neovim

### Nice to have
brew install btop
brew install svim

### Additional apps
brew install dropbox
brew install mailmate
brew install isync
brew install slack

### Emacs
brew install emacs-plus --with-native-comp
brew install --HEAD sbfnk/formulae/isync
brew install oauth2ms

## Casks
echo "Installing Brew Casks..."
### Terminals & Browsers
brew install --cask kitty
brew install --cask 1password
brew install --cask arc

### R
brew install --cask r

### Office
brew install --cask inkscape
brew install --cask zoom
brew install --cask skim
brew install --cask busycal

### Essential
brew install --cask karabiner-elements

### Nice to have
brew install --cask alfred
brew install --cask spotify
brew install --cask telegram
brew install --cask whatsapp

### Fonts
brew install --cask sf-symbols
brew install --cask font-hack-nerd-font
brew install --cask font-jetbrains-mono
brew install --cask font-fira-code

# Mac App Store Apps
echo "Installing Mac App Store Apps..."
mas install 360593530 #Notability

# macOS Settings
echo "Changing macOS defaults..."
defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true
defaults write com.apple.dock autohide -bool true
defaults write com.apple.dock "mru-spaces" -bool "false"
defaults write NSGlobalDomain NSAutomaticWindowAnimationsEnabled -bool false
defaults write com.apple.LaunchServices LSQuarantine -bool false
defaults write NSGlobalDomain com.apple.swipescrolldirection -bool false
defaults write NSGlobalDomain KeyRepeat -int 1
defaults write NSGlobalDomain NSAutomaticSpellingCorrectionEnabled -bool false
defaults write NSGlobalDomain AppleShowAllExtensions -bool true
defaults write NSGlobalDomain _HIHideMenuBar -bool true
defaults write NSGlobalDomain AppleHighlightColor -string "0.65098 0.85490 0.58431"
defaults write NSGlobalDomain AppleAccentColor -int 1
defaults write com.apple.screencapture location -string "$HOME/Desktop"
defaults write com.apple.screencapture disable-shadow -bool true
defaults write com.apple.screencapture type -string "png"
defaults write com.apple.finder DisableAllAnimations -bool true
defaults write com.apple.finder ShowExternalHardDrivesOnDesktop -bool false
defaults write com.apple.finder ShowHardDrivesOnDesktop -bool false
defaults write com.apple.finder ShowMountedServersOnDesktop -bool false
defaults write com.apple.finder ShowRemovableMediaOnDesktop -bool false
defaults write com.apple.Finder AppleShowAllFiles -bool true
defaults write com.apple.finder FXDefaultSearchScope -string "SCcf"
defaults write com.apple.finder FXEnableExtensionChangeWarning -bool false
defaults write com.apple.finder _FXShowPosixPathInTitle -bool true
defaults write com.apple.finder FXPreferredViewStyle -string "Nlsv"
defaults write com.apple.finder ShowStatusBar -bool false
defaults write com.apple.TimeMachine DoNotOfferNewDisksForBackup -bool YES
defaults write NSGlobalDomain WebKitDeveloperExtras -bool true
defaults write com.freron.MailMate MmMessagesOutlineMoveStrategy -string "unreadOrPrevious"

## install doom emacs
git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs
~/.config/emacs/bin/doom install

# Copying and checking out configuration files
echo "Planting Configuration Files..."
mkdir $HOME/code
git clone git@github.com:sbfnk/dotfiles.git $HOME/code/dotfiles
git clone git@github.com:sbfnk/dotfiles_private.git $HOME/code/dotfiles_private

# linking dot files
mkdir $HOME/.config
for dir in $HOME/code/dotfiles*; do
  if [ -d $dir/config ]; then
    for file in $dir/config/*; do
      ln -sf $file $HOME/.config
    done
  fi
  if [ -d $dir/root ]; then
    for file in $dir/root/*; do
      ln -sFh $file $HOME/.$(basename $file)
    done
  fi
  if [ -d $dir/Application\ Support ]; then
    for file in $dir/Application\ Support/*; do
      ln -sf $file $HOME/Library/Application\ Support
    done
  fi
done

ln -sFh $HOME/code/dotfiles/bin ~/bin

# Installing Fonts
git clone git@github.com:shaunsingh/SFMono-Nerd-Font-Ligaturized.git /tmp/SFMono_Nerd_Font
mv /tmp/SFMono_Nerd_Font/* $HOME/Library/Fonts
rm -rf /tmp/SFMono_Nerd_Font/

# Install oauth2ms
git clone git@github.com:harishkrupo/oauth2ms.git $HOME/code/oauth2ms
cd $HOME/code/oauth2ms
pip install -r requirements.txt
cp oauth2ms /usr/local/bin

curl -L https://github.com/kvndrsslr/sketchybar-app-font/releases/download/v1.0.23/sketchybar-app-font.ttf -o $HOME/Library/Fonts/sketchybar-app-font.ttf

# Start Services
echo "Starting Services (grant permissions)..."
yabai --start-service
skhd --start-service
brew services start sketchybar
brew services start borders
brew services start svim

csrutil status
echo "Add sudoer manually:\n '$(whoami) ALL = (root) NOPASSWD: sha256:$(shasum -a 256 $(which yabai) | awk "{print \$1;}") $(which yabai) --load-sa' to '/private/etc/sudoers.d/yabai'"
echo "Installation complete...\n"
