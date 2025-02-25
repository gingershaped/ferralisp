use std::{fs::read_to_string, io::ErrorKind, path::PathBuf};

use include_dir::{include_dir, Dir};

use crate::{
    machine::{LoadResult, Loader, ModuleLoad},
    parser::parse,
};

static STDLIB: Dir = include_dir!("src/stdlib/");

pub struct StdlibLoader;

impl Loader for StdlibLoader {
    fn name(&self) -> &'static str {
        "stdlib"
    }

    fn load(&self, path: &str, loads: &Vec<ModuleLoad>) -> LoadResult {
        if path == "library" {
            return self.load("lib/library", loads);
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

pub struct FileLoader {
    base_dir: PathBuf,
}

impl FileLoader {
    pub fn new(base_dir: PathBuf) -> Self {
        FileLoader { base_dir }
    }
}

impl Loader for FileLoader {
    fn name(&self) -> &'static str {
        "file"
    }

    fn load(&self, path: &str, loads: &Vec<ModuleLoad>) -> LoadResult {
        let mut target = self.base_dir.clone();
        for load in loads {
            if load.loader == self.name() {
                let mut path = PathBuf::from(&load.path);
                path.pop();
                target.push(path);
            }
        }
        target.push(path);
        match read_to_string(target) {
            Ok(contents) => match parse(&contents) {
                Ok(contents) => LoadResult::Ok {
                    values: contents.into_iter().map(|e| e.into()).collect(),
                    cache: true,
                },
                Err(error) => LoadResult::Err(Box::new(error)),
            },
            Err(error) => {
                if matches!(error.kind(), ErrorKind::NotFound) {
                    LoadResult::NotFound
                } else {
                    LoadResult::Err(Box::new(error))
                }
            }
        }
    }
}
