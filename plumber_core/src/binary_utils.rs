use std::io::{self, Read};
use std::mem::align_of;

#[cfg(feature = "fs")]
use crate::fs::GameFile;

pub fn null_terminated_prefix(bytes: &[u8]) -> Option<&[u8]> {
    if bytes.is_empty() {
        return None;
    }
    bytes.splitn(2, |&b| b == 0).next()
}

#[cfg(all(feature = "maligned", feature = "fs"))]
pub fn read_file_aligned<A: maligned::Alignment>(mut file: GameFile) -> io::Result<Vec<u8>> {
    let size = file.size().unwrap_or_default();

    let mut bytes = maligned::align_first::<u8, A>(size);
    file.read_to_end(&mut bytes)?;

    if bytes.as_ptr() as usize % align_of::<A>() != 0 {
        // vector reallocated, no longer aligned
        let mut new_bytes = maligned::align_first::<u8, A>(bytes.len());
        new_bytes.append(&mut bytes);
        bytes = new_bytes;
    }

    assert!(bytes.as_ptr() as usize % align_of::<A>() == 0);

    Ok(bytes)
}
