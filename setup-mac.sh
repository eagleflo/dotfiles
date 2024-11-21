#!/usr/bin/env sh

# Appearance: Auto
defaults write -globalDomain AppleInterfaceStyleSwitchesAutomatically -bool true

# Dock > Automatically hide and show the Dock
defaults write com.apple.dock autohide -bool true

# Key repeat rate
defaults write NSGlobalDomain KeyRepeat -int 1

# Delay until repeat
defaults write NSGlobalDomain InitialKeyRepeat -int 10

# Applications
# brew tap railwaycat/emacsmacport
# brew install emacs-mac

# brew tap homebrew/cask-fonts
# brew install --cask font-fira-code

# brew install fd fnm fzf git gpg htop mosh nvim ripgrep tig
# brew install bat dust eza hyperfine tokei vivid zoxide
# brew install go node python3 ruby rustup

# brew install --cask alacritty android-studio rectangle Spotify
