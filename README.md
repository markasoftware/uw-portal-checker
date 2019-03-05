# UW Portal Checker

Check your UW admissions portal with ease.

This is an Emacs package for University of Washington applicants to check their portal and see whether their admissions decision has been posted. It has only been tested for first-time freshman applications.

## Usage outside of Emacs

1. `git clone https://github.com/markasoftware/uw-portal-checker`
1. `cd uw-portal-checker`
1. `./uw.sh 'my-netid-username 'Pa5sw0rd'`

## Usage inside of Emacs

1. Install `uw.el` by `load`ing it in your init.el or equivalent.
2. Setup your UW NetID credentials in init.el or equivalent:
```
(setq uw-netid-username "johndoe"
      uw-netid-password "hunter2")
``
3. Setup is done! Do `C-c C-u C-w` or `M-x uw-check` and watch your status bar!
