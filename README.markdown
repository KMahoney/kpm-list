# kpm-list

An emacs buffer list that tries to intelligently group together open buffers.
Useful when you have a lot of them. Named kpm-list to avoid name clashes, and for vanity's sake.

Tested with emacs 23 only.

## Screenshot

![Screenshot](https://github.com/KMahoney/kpm-list/raw/master/screenshot.png)

## Installation

Place kpm-list.el into an emacs load-path and require the feature `kpm-list`. For example:

    (push "~/emacs.d" load-path)
    (require 'kpm-list)

## Improvements to be made

* I would like to display the buffers grouped together by directory as a fancy-pants tree.
* Perhaps some kind of colour interpolation that gradients your buffers from most recently used to least recent.
* Intelligently split large groups of buffers when they pass a certain threshold.
