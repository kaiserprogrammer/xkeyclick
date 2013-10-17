# XKeyclick

Tired of leaving your probably mechanical keyboard to use your mouse
or trackpad or whatever. Just start XKeyclick and you have never ever
to leave your keyboard anymore.

## Usage

When XKeyclick is started you get 8 regions to select from with
your home row. 3 times for the width and 2 times for the height and
your mouseclick is precise enough to be executed.

## Build

```
buildapp --output xkeyclick --load "quicklisp/setup.lisp" --load-system clx --load-system lisp-unit --load "xkeyclick.lisp" --entry "xkeyclick:start"
```
