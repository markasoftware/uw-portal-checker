# UW Portal Checker

Check your UW admissions portal with ease.

## Usage outside of Emacs

TODO

## Usage inside of Emacs

1. Install `uw.el` by `load`ing it in your init.el or equivalent.
2. Setup your UW NetID credentials in init.el or equivalent:
```
(setq uw-netid-username "johndoe"
      uw-netid-password "hunter2")
```
3. Setup is done! M-x uw-check to see if your portal has updated yet!
