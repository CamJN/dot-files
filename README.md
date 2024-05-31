# CamJN's dotfiles

This repo represents most of the configuration needed to make a Mac usable for me, it's usually kept up to date with the latest version of macOS and the configured software.

Some exceptions:

- 1Password is left at v7 because in v8 they switched to being Electron crap.
- Firefox is the Developer Edition instead of Stable.
- Sketch is not updated because it is expensive.
- Almost all software is managed by homebrew, so I wait on them.

# To run on a new mac:
`bash -c "$(curl -fsSL https://raw.githubusercontent.com/CamJN/dot-files/master/setup.sh)"`

# To run on an already setup mac (eg after macOS updates and erases some configs):
`~/Developer/Bash/dot-files/setup.sh`


# Why not use chezmoi, or stow, or yadm, or ?

I have tried, but none of them manage the whole system very well, they're mostly interested with dotfiles that live in the home dir not configs for the whole system, or lack templating.

# What about Ansible or Chef or similar?

Too heavyweight of bootstrap environments.

# Have you heard of NixOS?

I have, and I've even tried it. It sucks worse than all the other options I've tried.
