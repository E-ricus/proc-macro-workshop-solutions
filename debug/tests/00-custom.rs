// use derive_debug::CustomDebug;

// #[derive(CustomDebug)]
// pub struct Field {
//     name: &'static str,
//     #[debug = "0b{:08b}"]
//     bitmask: u8,
// }

fn main() {
    let bm: u8 = 0b00011100;
    let debug = format!("{}", format_args!("0b{:08b}", bm));
    let expected = r#"0b00011100"#;

    assert_eq!(debug, expected);
}
