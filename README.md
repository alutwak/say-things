# say-things

### _Ayal Lutwak <ayal@audio-electric.com>_

A valuable utility for those who need their Macintosh computers to say mostly unintelligible, random sentences

## License

This project may not be used or copied without the express consent of Ayal Lutwak

## Building

### Prerequisites

1. [SBCL](http://www.sbcl.org/):
   You can download sbcl from source and build yourself, but if you'd rather not, you can get it from:
   
   * MacPorts:
     ```sh
     port install sbcl
     ```
   * Homebrew:
     ```sh
     brew install sbcl
     ```
2. [Quicklisp](https://www.quicklisp.org/beta/):
   You can install Quicklisp using the instructions [here](https://www.quicklisp.org/beta/#installation)

### Build the binary

Once the prerequisites are installed, build the `say-thing` binary with the following command:

```sh
sbcl --load setup.lisp
```

## Installing the `say-things` daemon

If you want your computer to randomly say things all the time (why wouldn't you?), without having to run the program by hand
(what a pain!), you can install it as a background daemon using the install script:

```sh
./install.sh
```

In order for this to work, you will also need to install the `reattach-to-user-namespace` tool, which is available from the
[tmux-MacOSX-pasteboard](https://github.com/ChrisJohnsen/tmux-MacOSX-pasteboard) project. It can be installed using MacPorts
or Homebrew:

```sh
port install tmux-pasteboard

brew install reattach-to-user-namespace
```
