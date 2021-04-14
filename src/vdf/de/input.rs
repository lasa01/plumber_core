pub(crate) trait Input<'a> = Clone
    + PartialEq
    + InputTake
    + InputTakeAtPosition
    + InputLength
    + InputIter
    + Offset
    + Slice<Range<usize>>
    + Slice<RangeFrom<usize>>
    + Slice<RangeTo<usize>>
    + Compare<&'a str>
where
    <Self as InputTakeAtPosition>::Item: AsChar + Clone + PartialEq<char>,
    <Self as InputIter>::Item: AsChar + Copy,
    &'a str: FindToken<<Self as InputTakeAtPosition>::Item> + FindToken<<Self as InputIter>::Item>;
