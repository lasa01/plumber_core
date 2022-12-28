use plumber_vmf::vmf::Vmf;

fn test_vmf_roundtrip(input: &str) {
    let first_vmf = Vmf::from_bytes(input.as_bytes()).unwrap();
    let serialized_vmf = first_vmf.to_string().unwrap();
    let second_vmf = Vmf::from_bytes(serialized_vmf.as_bytes()).unwrap();
    assert_eq!(first_vmf, second_vmf)
}

#[test]
fn vmf_roundtrip() {
    test_vmf_roundtrip(include_str!("test.vmf"));
}

#[test]
fn hidden_vmf_roundtrip() {
    test_vmf_roundtrip(include_str!("test_hidden.vmf"));
}
