use valveflow::vmf::Vmf;

#[test]
fn test_vmf_roundtrip() {
    let input = include_str!("test.vmf");
    let first_vmf = Vmf::from_str(input).unwrap();
    let serialized_vmf = first_vmf.to_string().unwrap();
    let second_vmf = Vmf::from_str(&serialized_vmf).unwrap();
    assert_eq!(first_vmf, second_vmf)
}
