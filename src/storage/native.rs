use std::{
    borrow::Cow,
    fs::{File, OpenOptions},
    io,
    path::{Path, PathBuf},
};

use crate::storage::{Filesystem, ItemType, verify_contained_path};

#[derive(Clone)]
pub struct NativeFilesystem {
    root: PathBuf,
    tmp_path: PathBuf,
    tmp_str: String,
}

impl NativeFilesystem {
    pub fn new(root: PathBuf) -> Self {
        Self {
            root,
            tmp_path: Default::default(),
            tmp_str: Default::default(),
        }
    }

    fn setpath(&mut self, path: &Path) -> io::Result<&Path> {
        verify_contained_path(path)?;
        self.tmp_path.clone_from(&self.root);
        self.tmp_path.push(path);
        Ok(&self.tmp_path)
    }
}

impl Filesystem for NativeFilesystem {
    fn writefile(
        &mut self,
        path: &std::path::Path,
        writer: &mut dyn FnMut(&mut dyn std::io::Write) -> std::io::Result<()>,
    ) -> std::io::Result<()> {
        let path = self.setpath(path)?;
        let mut file = OpenOptions::new()
            .create(true)
            .write(true)
            .truncate(true)
            .open(path)?;
        writer(&mut file)?;
        Ok(())
    }

    fn readfile(
        &mut self,
        path: &std::path::Path,
        reader: &mut dyn FnMut(&mut dyn std::io::Read) -> std::io::Result<()>,
    ) -> std::io::Result<()> {
        let path = self.setpath(path)?;
        let mut file = File::open(path)?;
        reader(&mut file)?;
        Ok(())
    }

    fn readdir(
        &mut self,
        path: &std::path::Path,
        reader: &mut dyn FnMut(&str, super::ItemType) -> std::io::Result<()>,
    ) -> std::io::Result<()> {
        let path = self.setpath(path)?;

        for entry in std::fs::read_dir(path)? {
            let entry = entry?;

            let file_name = entry.file_name();
            let file_name = if file_name.len() > 256 {
                file_name.to_string_lossy()
            } else {
                self.tmp_str.clear();
                for chunk in file_name.as_encoded_bytes().utf8_chunks() {
                    self.tmp_str.push_str(chunk.valid());

                    if !chunk.invalid().is_empty() {
                        self.tmp_str.push(char::REPLACEMENT_CHARACTER);
                    }
                }

                Cow::Borrowed(self.tmp_str.as_str())
            };

            let meta = std::fs::metadata(entry.path())?;
            let ty = if meta.is_file() {
                ItemType::File
            } else {
                ItemType::Directory
            };

            reader(&file_name, ty)?;
        }

        Ok(())
    }

    fn stat(&mut self, path: &Path) -> io::Result<ItemType> {
        let path = self.setpath(path)?;
        Ok(match std::fs::metadata(path)?.is_file() {
            true => ItemType::File,
            false => ItemType::Directory,
        })
    }

    fn rmdir(&mut self, path: &std::path::Path) -> std::io::Result<()> {
        let path = self.setpath(path)?;
        std::fs::remove_dir_all(path)
    }

    fn rmfile(&mut self, path: &std::path::Path) -> std::io::Result<()> {
        let path = self.setpath(path)?;
        std::fs::remove_file(path)
    }

    fn mkdir(&mut self, path: &std::path::Path) -> std::io::Result<()> {
        let path = self.setpath(path)?;
        std::fs::create_dir_all(path)
    }
}
