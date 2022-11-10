use std::io::{self, Read};
use std::mem::align_of;

use zerocopy::{FromBytes, LayoutVerified};

use plumber_fs::GameFile;

pub fn null_terminated_prefix(bytes: &[u8]) -> Option<&[u8]> {
    if bytes.is_empty() {
        return None;
    }
    bytes.splitn(2, |&b| b == 0).next()
}

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

pub fn parse<T: FromBytes>(bytes: &[u8], offset: usize) -> Option<&T> {
    bytes
        .get(offset..)
        .and_then(LayoutVerified::<_, T>::new_from_prefix)
        .map(|(res, _)| res.into_ref())
}

pub fn parse_slice<T: FromBytes>(bytes: &[u8], offset: usize, count: usize) -> Option<&[T]> {
    if count == 0 {
        return Some(&[]);
    }

    bytes
        .get(offset..)
        .and_then(|bytes| LayoutVerified::new_slice_from_prefix(bytes, count))
        .map(|(res, _)| res.into_slice())
}

pub fn parse_mut<'a, T: FromBytes>(bytes: &mut &'a [u8]) -> Option<&'a T> {
    LayoutVerified::<_, T>::new_from_prefix(*bytes).map(|(res, remaining)| {
        *bytes = remaining;
        res.into_ref()
    })
}

pub fn parse_slice_mut<'a, T: FromBytes>(bytes: &mut &'a [u8], count: usize) -> Option<&'a [T]> {
    if count == 0 {
        return Some(&[]);
    }

    LayoutVerified::new_slice_from_prefix(*bytes, count).map(|(res, remaining)| {
        *bytes = remaining;
        res.into_slice()
    })
}
