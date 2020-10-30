# TUI Template

This is a template implementation of a tool with a terminal user interface.

* Haskell code for a TUI

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

### Workflow examples

TODO
