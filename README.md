# plumber_core

A Rust library for converting Source Engine games' VMF maps (and related files) into a generic 3D format.
Mainly designed for a Blender importer, but can be also used for other purposes.


## Current progress

Feature                   | Status
------------------------- | ----------
Installed games detection | Ready
Game asset opening        | Ready
VMF reading               | Ready
VMF brushes to geometry   | Ready
VMF overlays to geometry  | Ready
VMF decals to geometry    | Not started
VMF entity handling       | Ready
VMF skybox reading        | Not started
VMT (material) reading    | Ready
VTF (texture) reading     | Ready
MDL (model) mesh reading  | Ready
MDL skeleton reading      | Ready
MDL animation reading     | In progress
MDL skins                 | Not started
MDL other features        | Currently not planned


## Code structure

The library is currently split into three main crates.

`plumber_vdf` is a VDF (also known as Valve KeyValues) file format implementation for the Serde framework.

`plumber_vpk` is a VPK (packaged game content) file reader.

These could also be used independently of other crates, if needed.

`plumber_core` contains most of the logic and depends on the VDF and VPK crates.


## Credits

- VTF reading uses panzi's Linux-compatible version of Nemesis's VTFLib
- MDL reading is based on:
    - ZeqMacaw's Crowbar
    - REDxEYE's SourceIO
    - Artfunkel's Blender Source Tools
