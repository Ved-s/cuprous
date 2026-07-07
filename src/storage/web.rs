use std::{
    collections::HashSet,
    io::{self, Read, Write},
    path::Path,
};

use crate::storage::{
    Filesystem,
    ascii_encoding::{ByteAsciiDecoder, ByteAsciiEncoder},
};

use super::ItemType;

#[derive(Clone)]
pub struct LocalStorageFilesystem {
    prefix_length: usize,
    key_builder: String,
}

impl LocalStorageFilesystem {
    pub fn new(prefix: String) -> Self {
        Self {
            prefix_length: prefix.len(),
            key_builder: prefix,
        }
    }

    fn local_storage() -> Result<web_sys::Storage, io::Error> {
        web_sys::window()
            .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "window is not readable"))?
            .local_storage()
            .map_err(|_| io::Error::other("error while reading window.local_storage"))?
            .ok_or_else(|| {
                io::Error::new(
                    io::ErrorKind::NotFound,
                    "window.local_storage is not readable",
                )
            })
    }

    fn set_path(&mut self, path: &Path) -> Result<(), io::Error> {
        self.key_builder.drain(self.prefix_length..);
        let mut first_component = true;

        for c in path.components() {
            match c {
                std::path::Component::Prefix(_) => {
                    return Err(io::Error::new(
                        io::ErrorKind::Unsupported,
                        "prefixes in path are unsupported",
                    ));
                }
                std::path::Component::RootDir => {
                    self.key_builder.drain(self.prefix_length..);
                    first_component = true;
                }
                std::path::Component::CurDir => {}
                std::path::Component::ParentDir => {
                    return Err(io::Error::new(
                        io::ErrorKind::Unsupported,
                        "path traversal is not supported",
                    ));
                }
                std::path::Component::Normal(s) => {
                    if !first_component {
                        self.key_builder.push('/');
                    }
                    first_component = false;

                    self.key_builder.push_str(&s.to_string_lossy());
                }
            }
        }
        Ok(())
    }
}

impl Filesystem for LocalStorageFilesystem {
    fn writefile(
        &mut self,
        path: &Path,
        writer: &mut dyn FnMut(&mut dyn Write) -> io::Result<()>,
    ) -> io::Result<()> {
        let ls = Self::local_storage()?;
        self.set_path(path)?;

        let mut string = String::new();
        string.push('A');
        let mut encoder = ByteAsciiEncoder::new(&mut string);
        writer(&mut encoder)?;
        encoder.finish();

        ls.set_item(&self.key_builder, &string).map_err(|_| {
            io::Error::other(format!(
                "error writing localStorage key \"{}\"",
                self.key_builder
            ))
        })
    }

    fn readfile(
        &mut self,
        path: &Path,
        reader: &mut dyn FnMut(&mut dyn Read) -> io::Result<()>,
    ) -> io::Result<()> {
        let ls = Self::local_storage()?;
        self.set_path(path)?;
        let item = ls.get_item(&self.key_builder).map_err(|_| {
            io::Error::other(format!(
                "error reading localStorage key \"{}\"",
                self.key_builder
            ))
        })?;

        let Some(str) = item else {
            return Err(io::Error::new(
                io::ErrorKind::NotFound,
                format!("localStorage key \"{}\" not found", self.key_builder),
            ));
        };

        match str.as_bytes().first() {
            None => return Ok(()),
            Some(b'A') => {
                let mut decoder = ByteAsciiDecoder::new(&str.as_bytes()[1..]);
                reader(&mut decoder)?;
            }
            Some(e) => {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!(
                        "unknown encoding 0x{:02x} of the localStorage key \"{}\"",
                        e,
                        self.key_builder
                    ),
                ));
            }
        }

        Ok(())
    }

    fn readdir(
        &mut self,
        path: &Path,
        reader: &mut dyn FnMut(&str, ItemType) -> io::Result<()>,
    ) -> io::Result<()> {
        let ls = Self::local_storage()?;
        self.set_path(path)?;

        let item = ls.get_item(&self.key_builder).map_err(|_| {
            io::Error::other(format!(
                "error reading localStorage key \"{}\"",
                self.key_builder
            ))
        })?;

        if item.is_some() {
            return Err(io::Error::new(
                io::ErrorKind::NotADirectory,
                format!(
                    "Cannot read directory \"{}\": not a directory",
                    path.display()
                ),
            ));
        }

        self.key_builder.push('/');

        let mut seen_directories = HashSet::new();

        let length = ls
            .length()
            .map_err(|_| io::Error::other("error reading localStorage.length"))?;

        for n in 0..length {
            let key = ls
                .key(n)
                .map_err(|_| io::Error::other(format!("error calling localStorage.key({n})")))?;
            let Some(key) = key else {
                continue;
            };

            let Some(suffix) = key.strip_prefix(&self.key_builder) else {
                continue;
            };

            if let Some((dir, _)) = suffix.split_once('/') {
                if !seen_directories.contains(dir) {
                    seen_directories.insert(dir.to_owned());
                    reader(dir, ItemType::Directory)?;
                }
            } else {
                reader(suffix, ItemType::File)?;
            }
        }

        Ok(())
    }

    fn stat(&mut self, path: &Path) -> io::Result<ItemType> {
        let ls = Self::local_storage()?;
        self.set_path(path)?;
        let item = ls.get_item(&self.key_builder).map_err(|_| {
            io::Error::other(format!(
                "error reading localStorage key \"{}\"",
                self.key_builder
            ))
        })?;

        Ok(match item {
            Some(_) => ItemType::File,
            None => ItemType::Directory,
        })
    }

    fn rmdir(&mut self, path: &Path) -> io::Result<()> {
        let ls = Self::local_storage()?;
        self.set_path(path)?;

        let item = ls.get_item(&self.key_builder).map_err(|_| {
            io::Error::other(format!(
                "error reading localStorage key \"{}\"",
                self.key_builder
            ))
        })?;

        if item.is_some() {
            return Err(io::Error::new(
                io::ErrorKind::NotADirectory,
                format!(
                    "Cannot remove directory \"{}\": not a directory",
                    path.display()
                ),
            ));
        }

        self.key_builder.push('/');

        let length = ls
            .length()
            .map_err(|_| io::Error::other("error reading localStorage.length"))?;

        for n in 0..length {
            let key = ls
                .key(n)
                .map_err(|_| io::Error::other(format!("error calling localStorage.key({n})")))?;
            let Some(key) = key else {
                continue;
            };

            if !key.starts_with(&self.key_builder) {
                continue;
            }

            ls.remove_item(&key).map_err(|_| {
                io::Error::other(format!("error calling localStorage.removeItem(\"{key}\")"))
            })?;
        }

        Ok(())
    }

    fn rmfile(&mut self, path: &Path) -> io::Result<()> {
        let ls = Self::local_storage()?;
        self.set_path(path)?;

        ls.remove_item(&self.key_builder).map_err(|_| {
            io::Error::other(format!(
                "error calling localStorage.removeItem(\"{}\")",
                self.key_builder
            ))
        })
    }

    fn mkdir(&mut self, _path: &Path) -> io::Result<()> {
        Ok(())
    }
}
