use std::io::{self, Read};
use std::mem::align_of;

use zerocopy::{FromBytes, Immutable, KnownLayout, Ref};

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

    if !(bytes.as_ptr() as usize).is_multiple_of(align_of::<A>()) {
        // vector reallocated, no longer aligned
        let mut new_bytes = maligned::align_first::<u8, A>(bytes.len());
        new_bytes.append(&mut bytes);
        bytes = new_bytes;
    }

    assert!((bytes.as_ptr() as usize).is_multiple_of(align_of::<A>()));

    Ok(bytes)
}

pub fn parse<T: KnownLayout + Immutable + FromBytes>(bytes: &[u8], offset: usize) -> Option<&T> {
    bytes
        .get(offset..)
        .and_then(|bytes| Ref::<_, T>::from_prefix(bytes).ok())
        .map(|(res, _)| Ref::into_ref(res))
}

pub fn parse_slice<T: KnownLayout + Immutable + FromBytes>(bytes: &[u8], offset: usize, count: usize) -> Option<&[T]> {
    if count == 0 {
        return Some(&[]);
    }

    bytes
        .get(offset..)
        .and_then(|bytes| Ref::<_, [T]>::from_prefix_with_elems(bytes, count).ok())
        .map(|(res, _)| Ref::into_ref(res))
}

pub fn parse_mut<'a, T: KnownLayout + Immutable + FromBytes>(bytes: &mut &'a [u8]) -> Option<&'a T> {
    Ref::<_, T>::from_prefix(*bytes).ok().map(|(res, remaining)| {
        *bytes = remaining;
       Ref::into_ref(res)
    })
}

pub fn parse_slice_mut<'a, T: KnownLayout + Immutable + FromBytes>(bytes: &mut &'a [u8], count: usize) -> Option<&'a [T]> {
    if count == 0 {
        return Some(&[]);
    }

    Ref::<_, [T]>::from_prefix_with_elems(*bytes, count).ok().map(|(res, remaining)| {
        *bytes = remaining;
        Ref::into_ref(res)
    })
}
