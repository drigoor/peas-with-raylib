# Bag of Peas
A game jam or nothing at all


## Screenshots

1. Initial status (basic shapes replicated and initial player orientation)

![screenrec001](/etc/screenrec001.gif)


## Current status

- [ ] shapes
  - [ ] player
  - [ ] asteroids
  - [ ] enemys
- [ ] movement
  - [ ] player
  - [ ] asteroids
  - [ ] enemys
- [ ] basic input
- [ ] basic state management
- [ ] simple menus
- [ ] collision detection
- [ ] particles
- [ ] high scores
- [ ] menu option to turn on/off the antialiasing (maybe with a shortcut?)


## Installation and loading

```bat
mklink /J c:\home\quicklisp\local-projects\peas-with-raylib c:\home\projects\lisp\peas-with-raylib
```

`Bag of Peas` raylib a simple and easy-to-use library to enjoy videogames programming.

```lisp
;; load the peas
(ql:quickload :peas-with-raylib)

;; run it
(peas:run)
```

Note: ensure that raylib.dll and libffi-7.dll (from msys2) are copied for sbcl root directory.


## Ideas

- [ ] starfield (parallax?)
- [ ] the player's spaceship is fixed in the center and everything around it moves
- [ ] mini map like a radar to see all around (or a given distance if the world is big)
