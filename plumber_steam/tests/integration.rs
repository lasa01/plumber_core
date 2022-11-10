use plumber_steam::{App, Libraries};

/// Fails if steam is not installed
#[test]
#[ignore]
fn test_library_discovery() {
    let libraries = Libraries::discover().unwrap();
    eprintln!("discovered libraries: {:?}", libraries.paths);
    let apps: Vec<App> = libraries.apps().map(Result::unwrap).collect();
    eprintln!("discovered apps: {:?}", apps);
    let source_apps: Vec<App> = libraries.apps().source().map(Result::unwrap).collect();
    eprintln!("discovered source apps: {:?}", source_apps);
}
