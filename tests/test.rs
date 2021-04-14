use valveflow::vmf;

#[test]
fn test() {
    let input = include_str!("test.vmf");
    let vmf = vmf::from_str(input).unwrap();
    println!("{:?}", &vmf);
    let string = vmf::to_string(&vmf).unwrap();
    print!("{}", string);
}
