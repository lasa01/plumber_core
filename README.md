# plumber_core

A Rust library for converting Source Engine games' VMF maps (and related files) into a generic 3D format.
Mainly designed for a Blender importer, but can be also used for other purposes.


## Current progress

Feature                   | Status                 | Source
------------------------- | ---------------------- | ------
Installed games detection | Ready                  | [plumber_steam/src/lib.rs](https://github.com/lasa01/plumber_core/blob/master/plumber_steam/src/lib.rs)
Game asset opening        | Ready                  | [plumber_fs/src/lib.rs](https://github.com/lasa01/plumber_core/blob/master/plumber_fs/src/lib.rs)
VMF reading               | Ready                  | [plumber_vmf/src/vmf.rs](https://github.com/lasa01/plumber_core/blob/master/plumber_vmf/src/vmf.rs)
VMF brushes to geometry   | Ready                  | [plumber_vmf/src/solid_builder.rs](https://github.com/lasa01/plumber_core/blob/master/plumber_vmf/src/solid_builder.rs)
VMF overlays to geometry  | Ready                  | [plumber_vmf/src/overlay_builder.rs](https://github.com/lasa01/plumber_core/blob/master/plumber_vmf/src/overlay_builder.rs)
VMF decals to geometry    | Not started            |
VMF entity handling       | Ready                  | [plumber_vmf/src/entities.rs](https://github.com/lasa01/plumber_core/blob/master/plumber_vmf/src/vmf/entities.rs)
VMT (material) reading    | Ready                  | [plumber_vmt/src/lib.rs](https://github.com/lasa01/plumber_core/blob/master/plumber_vmt/src/lib.rs)
Skybox VMT reading        | Ready                  |
VTF (texture) reading     | Ready                  | [plumber_asset/src/vmt.rs](https://github.com/lasa01/plumber_core/blob/master/plumber_asset/src/vmt.rs)
MDL (model) mesh reading  | Ready                  | [plumber_mdl/src/lib.rs](https://github.com/lasa01/plumber_core/tree/master/plumber_mdl/src/lib.rs)
MDL skeleton reading      | Ready                  | [plumber_mdl/src/mdl.rs](https://github.com/lasa01/plumber_core/tree/master/plumber_mdl/src/mdl.rs)
MDL animation reading     | Ready                  | [plumber_mdl/src/mdl.rs](https://github.com/lasa01/plumber_core/tree/master/plumber_mdl/src/mdl.rs)
MDL external ANI animation| Not started            |
MDL animation movements   | Not started            |
MDL skins                 | Not started            |
MDL secondary UV map      | Not started            |
MDL flex animation        | Not started            |
MDL other features        | Currently not planned  |


## Code structure

The library is split into small crates.

`plumber_vdf` is a VDF (also known as Valve KeyValues) file format implementation for the Serde framework.

`plumber_vpk` is a VPK (packaged game content) file reader.

`plumber_uncased` is a case-insensitive string wrapper.

`plumber_steam` detects installed Steam games.

`plumber_fs` reads files in a Source game's file system.

`plumber_vmt` is a VMT (material) reader.

`plumber_vmf` is a VMF (uncompiled map file) reader and converter into 3D geometry.

`plumber_mdl` is a MDL (3d model) reader.

`plumber_asset` contains an interface for creating a multithreaded importer using the other crates.

`plumber_core` re-exports all the other crates for convenience.


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


## License

Licensed under either of

 * Apache License, Version 2.0
   ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license
   ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.


## Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.
