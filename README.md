# plumber_core

A Rust library for converting Source Engine games' VMF maps (and related files) into a generic 3D format.
Mainly designed for a Blender importer, but can be also used for other purposes.


## Current progress

Feature                   | Status                 | Source
------------------------- | ---------------------- | ------
Installed games detection | Ready                  | [plumber_core/src/steam.rs](https://github.com/lasa01/plumber_core/blob/master/plumber_core/src/steam.rs)
Game asset opening        | Ready                  | [plumber_core/src/fs.rs](https://github.com/lasa01/plumber_core/blob/master/plumber_core/src/fs.rs)
VMF reading               | Ready                  | [plumber_core/src/vmf/mod.rs](https://github.com/lasa01/plumber_core/blob/master/plumber_core/src/vmf/mod.rs)
VMF brushes to geometry   | Ready                  | [plumber_core/src/vmf/solid_builder.rs](https://github.com/lasa01/plumber_core/blob/master/plumber_core/src/vmf/solid_builder.rs)
VMF overlays to geometry  | Ready                  | [plumber_core/src/vmf/overlay_builder.rs](https://github.com/lasa01/plumber_core/blob/master/plumber_core/src/vmf/overlay_builder.rs)
VMF decals to geometry    | Not started            |
VMF entity handling       | Ready                  | [plumber_core/src/vmf/entities.rs](https://github.com/lasa01/plumber_core/blob/master/plumber_core/src/vmf/entities.rs)
VMF skybox reading        | Not started            |
VMT (material) reading    | Ready                  | [plumber_core/src/vmt/mod.rs](https://github.com/lasa01/plumber_core/blob/master/plumber_core/src/vmt/mod.rs)
VTF (texture) reading     | Ready                  | [plumber_core/src/vmt/loader.rs](https://github.com/lasa01/plumber_core/blob/master/plumber_core/src/vmt/loader.rs)
MDL (model) mesh reading  | Ready                  | [plumber_core/src/model](https://github.com/lasa01/plumber_core/tree/master/plumber_core/src/model)
MDL skeleton reading      | Ready                  | [plumber_core/src/model](https://github.com/lasa01/plumber_core/tree/master/plumber_core/src/model)
MDL animation reading     | In progress            | [plumber_core/src/model](https://github.com/lasa01/plumber_core/tree/master/plumber_core/src/model)
MDL skins                 | Not started            |
MDL secondary UV map      | Not started            |
MDL flex animation        | Not started            |
MDL other features        | Currently not planned  |


## Code structure

The library is currently split into three main crates.

`plumber_vdf` is a VDF (also known as Valve KeyValues) file format implementation for the Serde framework.

`plumber_vpk` is a VPK (packaged game content) file reader.

These could also be used independently of other crates, if needed.

`plumber_core` contains most of the logic and depends on the VDF and VPK crates.


## Notes

This uses my case-insensitive fork of Serde.
Requires the following in Cargo.toml to use:
```toml
[patch.crates-io]
serde = { git = "https://github.com/lasa01/serde", branch = "case-insensitive-attr" }
serde_derive = { git = "https://github.com/lasa01/serde", branch = "case-insensitive-attr" }
```

Currently statically links into VTFLib, which is LGPL-licensed.


## Credits

- VTF reading uses panzi's Linux-compatible version of Nemesis's VTFLib
- MDL reading is based on:
    - ZeqMacaw's Crowbar
    - REDxEYE's SourceIO
    - Artfunkel's Blender Source Tools
