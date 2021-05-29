{ useWarp ? false }:
(import ./release.nix { inherit useWarp; }).shells.ghcjs
