extern crate bindgen;

use std::env;
use std::path::PathBuf;

fn main() {
    println!("cargo:rerun-if-changed=wrapper.h");

    let bindings = bindgen::Builder::default()
        .header("wrapper.h")
        .clang_arg("-I../../../_build/stage1/rts/build/include")
        .clang_arg("-I../../include")
        .clang_arg("-I../..")
        .clang_arg("-DTHREADED_RTS") // todo: handle non threaded runtime
        .allowlist_type("Capability")
        .allowlist_var("capabilities")
        .allowlist_var("n_capabilities")
        .allowlist_type("Task")
        .blocklist_type("StgTSO_")
        .blocklist_type("StgTSO")
        .parse_callbacks(Box::new(bindgen::CargoCallbacks))
        .generate()
        .expect("Unable to generate bindings");

    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());
    bindings
        .write_to_file(out_path.join("bindings.rs"))
        .expect("Couldn't write bindings!");
}


