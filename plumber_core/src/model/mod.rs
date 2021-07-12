mod mdl;
mod vtx;
mod vvd;

use std::io;

use mdl::Mdl;
use vtx::Vtx;
use vvd::Vvd;
pub use vvd::{BoneWeight, Vertex};

use thiserror::Error;

use crate::fs::{GameFile, OpenFileSystem, Path, PathBuf};

#[derive(Debug, Clone, Error)]
pub enum Error {
    #[error("io error reading `{path}`: {kind:?}")]
    Io { path: String, kind: io::ErrorKind },
    #[error("not a {0} file: invalid signature `{1}`")]
    InvalidSignature(&'static str, String),
    #[error("unsupported {0} version {1}")]
    UnsupportedVersion(&'static str, i32),
    #[error("{0} checksum doesn't match mdl checksum")]
    ChecksumMismatch(&'static str),
    #[error("model corrupted: {0}")]
    Corrupted(&'static str),
}

impl Error {
    fn from_io(err: &io::Error, path: &Path) -> Self {
        Self::Io {
            path: path.as_str().to_string(),
            kind: err.kind(),
        }
    }
}

const VTX_EXTENSIONS: &[&str] = &["dx90.vtx", "dx80.vtx", "sw.vtx", "xbox.vtx", "vtx"];

fn find_vtx<'a>(
    mdl_path: &Path,
    file_system: &'a OpenFileSystem,
) -> Result<(PathBuf, GameFile<'a>), Error> {
    for &extension in VTX_EXTENSIONS {
        let path = mdl_path.with_extension(extension);
        match file_system.open_file(&path) {
            Ok(file) => return Ok((path, file)),
            Err(err) => {
                if err.kind() == io::ErrorKind::NotFound {
                    continue;
                }
                return Err(Error::from_io(&err, &path));
            }
        }
    }
    Err(Error::Io {
        path: mdl_path.with_extension("*.vtx").into_string(),
        kind: io::ErrorKind::NotFound,
    })
}

#[derive(Debug, Clone)]
pub struct Model {
    mdl: Mdl,
    vvd: Vvd,
    vtx: Vtx,
}

impl Model {
    /// # Errors
    ///
    /// Returns `Err` if reading the mdl file fails or if reading an associated vvd or vtx file fails.
    pub fn read(path: impl AsRef<Path>, file_system: &OpenFileSystem) -> Result<Self, Error> {
        let path = path.as_ref();
        let mdl_file = file_system
            .open_file(path)
            .map_err(|err| Error::from_io(&err, path))?;
        let mdl = Mdl::read(mdl_file).map_err(|err| Error::from_io(&err, path))?;

        let vvd_path = path.with_extension("vvd");
        let vvd_file = file_system
            .open_file(&vvd_path)
            .map_err(|err| Error::from_io(&err, &vvd_path))?;
        let vvd = Vvd::read(vvd_file).map_err(|err| Error::from_io(&err, &vvd_path))?;

        let (vtx_path, vtx_file) = find_vtx(path, file_system)?;
        let vtx = Vtx::read(vtx_file).map_err(|err| Error::from_io(&err, &vtx_path))?;

        Ok(Model { mdl, vvd, vtx })
    }

    /// # Errors
    ///
    /// Returns `Err` if a signature or header is invalid or a version is unsupported.
    pub fn verify(&self) -> Result<Verified, Error> {
        self.mdl.check_signature().map_err(|signature| {
            signature.map_or(
                Error::Corrupted("could not read mdl signature"),
                |signature| {
                    Error::InvalidSignature("mdl", String::from_utf8_lossy(signature).into_owned())
                },
            )
        })?;
        self.mdl.check_version().map_err(|v| {
            v.map_or(Error::Corrupted("could not read mdl version"), |v| {
                Error::UnsupportedVersion("mdl", v)
            })
        })?;

        self.vvd.check_signature().map_err(|signature| {
            signature.map_or(
                Error::Corrupted("could not read vvd signature"),
                |signature| {
                    Error::InvalidSignature("vvd", String::from_utf8_lossy(signature).into_owned())
                },
            )
        })?;
        self.vvd.check_version().map_err(|v| {
            v.map_or(Error::Corrupted("could not read vvd version"), |v| {
                Error::UnsupportedVersion("vvd", v)
            })
        })?;

        self.vtx.check_version().map_err(|v| {
            v.map_or(Error::Corrupted("could not read vtx version"), |v| {
                Error::UnsupportedVersion("vtx", v)
            })
        })?;

        let mdl_header = self
            .mdl
            .header()
            .ok_or(Error::Corrupted("could not read mdl header"))?;
        let vvd_header = self
            .vvd
            .header()
            .ok_or(Error::Corrupted("could not read vvd header"))?;
        let vtx_header = self
            .vtx
            .header()
            .ok_or(Error::Corrupted("could not read vtx header"))?;

        if vvd_header.checksum() != mdl_header.checksum() {
            return Err(Error::ChecksumMismatch("vvd"));
        }
        if vtx_header.checksum() != mdl_header.checksum() {
            return Err(Error::ChecksumMismatch("vtx"));
        }

        Ok(Verified {
            mdl_header,
            vvd_header,
            vtx_header,
        })
    }
}

#[derive(Debug, Clone)]
pub struct Verified<'a> {
    mdl_header: mdl::HeaderRef<'a>,
    vvd_header: vvd::HeaderRef<'a>,
    vtx_header: vtx::HeaderRef<'a>,
}

impl<'a> Verified<'a> {
    #[must_use]
    pub fn is_static_prop(&self) -> bool {
        self.mdl_header
            .flags()
            .contains(mdl::HeaderFlags::STATIC_PROP)
    }

    /// # Errors
    ///
    /// Returns `Err` if reading the vertices fails.
    pub fn vertices(&self) -> Result<&[Vertex], Error> {
        self.vvd_header
            .vertices()
            .ok_or(Error::Corrupted("could not read vvd vertices"))
    }

    pub fn models(&self) -> Result<(), Error> {
        Ok(())
    }
}
