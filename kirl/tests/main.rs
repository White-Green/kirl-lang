use assert_cmd::assert::OutputAssertExt;
use assert_cmd::prelude::CommandCargoExt;
use predicates::prelude::predicate;
use std::collections::{HashMap, VecDeque};
use std::fs;
use std::process::Command;
use tempfile::TempDir;

#[derive(Debug)]
enum FileMap {
    Directory(HashMap<&'static str, Self>),
    File(&'static [u8]),
}

impl From<HashMap<&'static str, FileMap>> for FileMap {
    fn from(map: HashMap<&'static str, FileMap>) -> Self {
        FileMap::Directory(map)
    }
}

impl From<&'static [u8]> for FileMap {
    fn from(value: &'static [u8]) -> Self {
        FileMap::File(value)
    }
}

impl<const N: usize> From<&'static [u8; N]> for FileMap {
    fn from(value: &'static [u8; N]) -> Self {
        FileMap::File(value)
    }
}

macro_rules! insert_map {
    ($base:expr, $var:ident, $name:tt : {$($c:tt)*}, $($t:tt)*) => {
        $var.insert(stringify!($name), FileMap::from(create_map!(concat!($base, stringify!($name), "/"), $($c)*)));
        insert_map!($base, $var, $($t)*)
    };
    ($base:expr, $var:ident, $name:tt, $($t:tt)*) => {
        $var.insert(concat!(stringify!($name), ".kirl"), FileMap::from(include_bytes!(concat!($base, stringify!($name), ".kirl"))));
        insert_map!($base, $var, $($t)*)
    };
    ($base:expr, $var:ident, $name:tt : {$($c:tt)*}) => {
        $var.insert(stringify!($name), FileMap::from(create_map!(concat!($base, stringify!($name), "/"), $($c)*)));
    };
    ($base:expr, $var:ident, $name:tt) => {
        $var.insert(concat!(stringify!($name), ".kirl"), FileMap::from(include_bytes!(concat!($base, stringify!($name), ".kirl"))));
    };
}

macro_rules! create_map {
    ($base:expr, $($t:tt)*) => {
        {
            let mut map = HashMap::new();
            insert_map!($base, map, $($t)*);
            map
        }
    };
}

macro_rules! create_test_inner {
    ($name:ident, {$($map:tt)*}, $entry:expr, $out:expr, $cmd:ident, $($assert:tt)*) => {
        #[test]
        fn $name() -> anyhow::Result<()> {
            let files = create_map!("../../examples/", $($map)*);
            let temp_dir = TempDir::new()?;
            let temp_path = temp_dir.path();
            let mut q = VecDeque::new();
            q.push_back((temp_path.to_path_buf(), &files));
            while let Some((path, files)) = q.pop_front() {
                for (name, files) in files {
                    let child_path = path.join(name);
                    match files {
                        FileMap::Directory(body) => {
                            fs::create_dir(&child_path)?;
                            q.push_back((child_path, body));
                        }
                        FileMap::File(body) => {
                            fs::write(child_path, body)?;
                        }
                    }
                }
            }
            let mut $cmd = Command::cargo_bin("kirl")?;
            $cmd.arg(temp_path.join($entry));
            $($assert)*
            Ok(())
        }
    };
}

macro_rules! create_test {
    (name: $name:ident; map: {$($map:tt)*}; entry: $entry:expr; out: $out:expr;) => {
        create_test_inner!($name, {$($map)*}, $entry, $out, cmd, cmd.assert()
                .success()
                .stdout(predicate::str::contains($out)););
    };
}

macro_rules! create_failue_test {
    (name: $name:ident; map: {$($map:tt)*}; entry: $entry:expr; out: $out:expr;) => {
        create_test_inner!($name, {$($map)*}, $entry, $out, cmd, cmd.assert()
                .failure()
                .stderr(predicate::str::contains($out)););
    };
}

create_test! {
    name: test_0;
    map: {
        0: {
            hello
        }
    };
    entry: "0/hello.kirl";
    out: "Hello, World!
";
}

create_test! {
    name: test_1_1;
    map: {
        1: {
            fib1
        }
    };
    entry: "1/fib1.kirl";
    out: "6765
";
}

create_test! {
    name: test_1_2;
    map: {
        1: {
            fib2
        }
    };
    entry: "1/fib2.kirl";
    out: "6765
";
}

create_test! {
    name: test_1_3;
    map: {
        1: {
            fib3
        }
    };
    entry: "1/fib3.kirl";
    out: "6765
";
}

create_test! {
    name: test_1_4;
    map: {
        1: {
            fib4
        }
    };
    entry: "1/fib4.kirl";
    out: "6765
";
}

create_test! {
    name: test_2;
    map: {
        2: {
            main,
            sub1,
            subdir: {
                sub2,
                sub3
            }
        }
    };
    entry: "2/main.kirl";
    out: "this is in main.kirl
this is in function \"f\" in sub.kirl
this is in function \"f\" in subdir/sub2.kirl
this is in function \"f\" in subdir/sub3.kirl
";
}

create_test! {
    name: test_3_valid;
    map: {
        3: {
            valid
        }
    };
    entry: "3/valid.kirl";
    out: "this is in function \"f\"
";
}

create_failue_test! {
    name: test_3_error;
    map: {
        3: {
            error
        }
    };
    entry: "3/error.kirl";
    out: "Error: \"A named reference \\\"f\\\" at Line:5,position:0 - Line:5,position:1 is not found.\"
";
}

create_test! {
    name: test_4;
    map: {
        4: {
            main
        }
    };
    entry: "4/main.kirl";
    out: "this is in function \"f\":(String)->()
string
this is in function \"f\":(Number)->()
256
";
}

create_test! {
    name: test_5;
    map: {
        5: {
            main
        }
    };
    entry: "5/main.kirl";
    out: "this is a string message
1024
てすと用めっせーじ
2048
this is a new string message
";
}

create_test! {
    name: test_6;
    map: {
        6: {
            main
        }
    };
    entry: "6/main.kirl";
    out: "128
test string
128
test string
";
}

create_test! {
    name: test_7;
    map: {
        7: {
            main
        }
    };
    entry: "7/main.kirl";
    out: "1
string_1
2
string_2
3
string_3
1
string_1
3
string_3
2
string_2
";
}
