use include_dir::{include_dir, Dir};

use crate::{
    machine::{LoadResult, Loader},
    parser::parse,
};

static STDLIB: Dir = include_dir!("src/stdlib/");

pub struct StdlibLoader;

impl Loader for StdlibLoader {
    fn name(&self) -> &'static str {
        "stdlib"
    }

    fn load(&self, path: &str) -> LoadResult {
        if path == "library" {
            return self.load("lib/library");
        }
        if let Some((_, path)) = path.split_once("lib/") {
            if let Some(file) = STDLIB.get_file(path.to_owned() + ".tl") {
                let contents = file
                    .contents_utf8()
                    .expect("failed to decode file contents");
                return LoadResult::Ok {
                    values: parse(contents)
                        .expect("failed to parse file")
                        .into_iter()
                        .map(|e| e.into())
                        .collect(),
                    cache: true,
                };
            }
        }
        return LoadResult::NotFound;
    }
}
