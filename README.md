# TUI Template

This is a template implementation of a tool with a terminal user interface.

* Haskell code for a TUI with all best practices
* OptParse for the TUI
* A nix build
* An example cast for documentation
* CI
  * Stack-based CI
  * Nix-based CI
* Pre-commit hooks

## License

This template is **not** free to use.
See https://template.cs-syd.eu/template/NorfairKing/template-tui for more information.

Copyright (c) 2020 Tom Sydney Kerckhove.

All Rights Reserved.

## Instructions

To use this template in a new project, choose the name for your project, for example `homeless-shelter`.
Then use [template-filler](https://github.com/NorfairKing/template-filler) to use the template, like this:

```
template-filler --source /path/to/this/template-tui --destination /path/to/your/homeless-shelter --find FooBar --replace HomelessShelter
```

### Template overview

There is a single Haskell package in `foo-bar-tui`.
It contains the following structure:

- The entry point in `Foo.Bar.TUI`.
- The TUI state in `Foo.Bar.TUI.State`.
- The rendering in `Foo.Bar.TUI.Draw`.
- The Worker environment and types in `Foo.Bar.TUI.Env`
- The worker function in `Foo.Bar.TUI.Worker`
- The event handling in `Foo.Bar.TUI.Handle`.

### OptParse

The option parsing is based on [the option parsing template](https://github.com/NorfairKing/template-optparse).
It is included in this template so you will not need to also buy the option parsing template.

For more information about how to use the option parsing, follow the instructions in `template-tui/src/Foo/Bar/TUI/OptParse.hs`.

### Nix build

If you don't need a nix build, remove these files:

```
rm -rf *.nix nix .github/workflows/nix.yaml
```

In `nix/nixpkgs-version.nix`, we pin a `nixpkgs` commit.
In `nix/pkgs.nix` we define our own 'version' of the `nixpkgs` by adding our own overlays.
The project overlay is defined in `nix/overlay.nix`.

See the instructions in `nix/overlay.nix` for more details.

### CI

CI is set up for both a stack build and a nix build.
See `.github/workflows` for more details.

The stack build should "just work".

For the nix build to work, there is a manual step that you need to go through:
First, make a cachix cache at cachix.org.
Put its name in the right places within `.github/workflows/nix.yaml`.
Then put its signing key in the 'Secrets' part of your repository on github.

### Casts

An example [autorecorder](https://github.com/NorfairKing/autorecorder) cast is provided in the `casts` directory.
You can declare ascii casts for your TUI for documentation using autorecorder.
If you don't need this, you can delete the `casts` directory and the relevant part of `nix/overlay.nix` and `ci.nix`.

### Workflow examples

#### Adding functionality to the TUI

Brick, the TUI library that we use, uses the following architecture.

```
         Start
           |
           v
Event -> State -> Draw
  |                |
  \----- Brick-----/
```

To add functionality:

1. Change the `State` to have what you need for your functionality.
   Remember that this state needs to contain everything that you may want to draw on the screen.
2. If some long-running IO needs to happen, change the `Request` and `Response` type to support the messages that need to be sent.
   Also add the request handling to the `Worker`.
3. Change the `draw` function to draw the new state.
4. Add any event handling that's necessary to the `handle` function.
