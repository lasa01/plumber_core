use std::collections::HashMap;

use serde_derive::Deserialize;
use maplit::hashmap;

use valveflow::vdf;

#[derive(Deserialize, PartialEq, Debug)]
struct Key7 {}

#[derive(Deserialize, PartialEq, Debug)]
struct Key8 {
    key1: String,
}

#[derive(Deserialize, PartialEq, Debug)]
struct Key12 {
    key1: Key12Key1,
}

#[derive(Deserialize, PartialEq, Debug)]
struct Key12Key1 {
    #[serde(rename = "#key1")]
    key1: String,
}

#[derive(Deserialize, PartialEq, Debug)]
struct Element {
    key1: f32,
}

#[derive(Deserialize, PartialEq, Debug)]
struct Test {
    key1: String,
    key2: String,
    key3: String,
    key4: String,
    key5: (),
    key6: HashMap<String, String>,
    key7: Key7,
    key8: Key8,
    seq1: Vec<String>,
    #[serde(rename = "#key9")]
    key9: String,
    #[serde(rename = "$key10")]
    key10: String,
    #[serde(rename = "%key#11%&!")]
    key11: String,
    key12: Key12,
    key13: i32,
    key14: f32,
    seq2: Vec<Element>,
}

#[test]
fn test_parse() {
    let input = include_str!("test.vdf");
    assert_eq!(
        vdf::from_str::<Test>(input).unwrap(),
        Test {
            key1: "value1".into(),
            key2: "value2".into(),
            key3: "value3".into(),
            key4: ("a\t\r\nb\r\n// not a comment\r\nc\r\n  d\r\ne\r\nf\t\r\n").into(),
            key5: (),
            key6: hashmap! {
                "key1".into() => "value1".into(),
                "key2".into() => "value2".into(),
            },
            key7: Key7 {},
            key8: Key8 {
                key1: "value1".into()
            },
            seq1: vec!["hello".into(), "this is kind of a hacky seq".into(), "but hey, whatever works!".into()],
            key9: "/value/9".into(),
            key10: "value 10".into(),
            key11: "value'11+-0$%!".into(),
            key12: Key12 {
                key1: Key12Key1 {
                    key1: "/%/%.".into(),
                }
            },
            key13: 31254,
            key14: 1.004,
            seq2: vec![
                Element {
                    key1: -1.02,
                },
                Element {
                    key1: 31.0,
                },
                Element {
                    key1: -32.0,
                }
            ],
        }
    );
}
