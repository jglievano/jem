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
- Open buffer. SPC b o
- Search in buffer. SPC b s

## Project
- Switch to project. SPC p p
- Find file in project. SPC p f
- Search files containing string in project. SPC p s

## Formating
- Indent buffer or region. SPC f i

## Terminal
- Open terminal.
  - In project root directory. SPC t p
  - In file directory. SPC t f
  - In user home directory. SPC t h
  - In root directory ("/"). SPC t r