use valveflow::vmf::Vmf;

#[test]
fn test() {
    let input = include_str!("test.vmf");
    let vmf = Vmf::from_str(input).unwrap();
    println!("{:?}", &vmf);
    let string = vmf.to_string().unwrap();
    print!("{}", string);
}
