use plumber_vmf::Vmf;

#[test]
fn test_vmf_roundtrip() {
    let input = include_str!("test.vmf");
    let first_vmf = Vmf::from_bytes(input.as_bytes()).unwrap();
    let serialized_vmf = first_vmf.to_string().unwrap();
    let second_vmf = Vmf::from_bytes(serialized_vmf.as_bytes()).unwrap();
    assert_eq!(first_vmf, second_vmf)
}
