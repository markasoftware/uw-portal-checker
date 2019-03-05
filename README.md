# UW Portal Checker

Check your UW admissions portal with ease.

This is an Emacs package for University of Washington applicants to check their portal and see whether their admissions decision has been posted. It has only been tested for first-time freshman applications.

## Usage outside of Emacs

UW Portal Checker is written in Emacs Lisp, a language designed specifically for customizing the Emacs editor. But if you aren't familiar with Emacs, you can still use the UW Portal Checker!

1. Make sure you are on Mac or Linux. You might be able to get it working in the Windows Subsystem for Linux, but don't bank on it.
1. [Install emacs](https://gnu.org/software/emacs)
1. Clone this github repository from the terminal: `git clone https://github.com/markasoftware/uw-portal-checker`
1. Enter the cloned project (still in terminal): `cd uw-portal-checker`
1. Check the portal by running this command, using your NetID username and password: `./uw.sh 'my-netid-username' 'Pa5sw0rd'`

## Usage inside of Emacs

1. Install `uw.el` by `load`ing it in your init.el or equivalent.
2. Setup your UW NetID credentials in init.el or equivalent:
```
(setq uw-netid-username "johndoe"
      uw-netid-password "hunter2")
``
3. Setup is done! Do `C-c C-u C-w` or `M-x uw-check` and watch your status bar!
