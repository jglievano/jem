# JEM

JEM is an Emacs configuration designed to improve UX.

It introduces 3 elements:

- jem-buffer.
- SPC shortcuts with UI assistance.
- Intuitive structure.

# NOTES

- Remember to install gnutls. e.g: `brew install gnutls`.

# Features

## Emacs
- Open `*Messages*`. SPC e m
- Open `*scratch*`. SPC e k
- Quit. SPC e q
- Save. SPC e s
- Kill. SPC e k
- Command (M-x). SPC SPC

## Debug
- Compile. SPC d c
- Test. SPC d t
- Lint. SPC d l
- Run. SPC d r

## Windows
- Close window. SPC w w
- Window move.
  - Up. SPC w k
  - Down. SPC w j
  - Right. SPC w l
  - Left. SPC w h
- Window create.
  - Right. SPC w v
  - Down. SPC w s
- Window resize.
  - Enlarge. SPC w +
  - Shrink. SPC w -

## Buffer
- Goto line. SPC b g
- New buffer. SPC b n
- Open buffer. SPC b b
- Search in buffer. SPC b s
- iMenu in buffer. SPC b S
- Paste from clipboard. SPC b C-v
- Copy buffer to clipboard. CPC b C-c

## File management
- Open file. SPC f f
- Delete file. SPC f D
- Delete file associated to current buffer. SPC f d
- Rename file associated to current buffer. SPC f r
- Rename file. SPC f R

## Project
- Ignore file names. SPC p #
- Switch to project. SPC p p
- Find file in project. SPC p f
- Search files containing string in project. SPC p s

## Text
- Select block. SPC t t b
- Sort lines. SPC t s
- Unique - Remove duplicate lines. SPC t u
- Indent buffer or region. SPC t i
- Align. SPC t a

## Shell
- Run shell command. SPC ! !
- Open terminal.
  - In project root directory. SPC ! p
  - In file directory. SPC ! f
  - In user home directory. SPC ! h
  - In root directory ("/"). SPC ! r