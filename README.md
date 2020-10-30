# TUI Template

This is a template implementation of a tool with a terminal user interface.

* Haskell code for a TUI with all best practices
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

To use this template in a new project, choose the name for your project, for example `shelter`.
Then use [template-filler](https://github.com/NorfairKing/template-filler) to use the template, like this:

```
template-filler --source /path/to/this/template-cli --destination /path/to/your/shelter --find Foobar --replace Shelter
```

### Template overview

There is a single Haskell package in `foobar-tui`.
It contains the following structure:

- The entry point in `Foobar.TUI`.
- The TUI state in `Foobar.TUI.State`.
- The rendering in `Foobar.TUI.Draw`.
- The Worker environment and types in `Foobar.TUI.Env`
- The worker function in `Foobar.TUI.Worker`
- The event handling in `Foobar.TUI.Handle`.

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
