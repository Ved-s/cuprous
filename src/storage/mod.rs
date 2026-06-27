use std::{
    io,
    path::{Path, PathBuf},
};

use parking_lot::Mutex;

pub mod native;

pub enum ItemType {
    File,
    Directory,
}

pub trait Filesystem {
    /// Creates file if it doesn't exist
    fn writefile(
        &mut self,
        path: &Path,
        writer: &mut dyn FnMut(&mut dyn io::Write) -> io::Result<()>,
    ) -> io::Result<()>;
    fn readfile(
        &mut self,
        path: &Path,
        reader: &mut dyn FnMut(&mut dyn io::Read) -> io::Result<()>,
    ) -> io::Result<()>;

    fn readdir(
        &mut self,
        path: &Path,
        reader: &mut dyn FnMut(&str, ItemType) -> io::Result<()>,
    ) -> io::Result<()>;

    fn stat(&mut self, path: &Path) -> io::Result<ItemType>;

    /// Removes directories with contents
    fn rmdir(&mut self, path: &Path) -> io::Result<()>;
    fn rmfile(&mut self, path: &Path) -> io::Result<()>;

    /// Makes all directories in the path
    fn mkdir(&mut self, path: &Path) -> io::Result<()>;
}

impl<F: Filesystem + ?Sized> Filesystem for &mut F {
    fn writefile(
        &mut self,
        path: &Path,
        writer: &mut dyn FnMut(&mut dyn io::Write) -> io::Result<()>,
    ) -> io::Result<()> {
        <F as Filesystem>::writefile(*self, path, writer)
    }

    fn readfile(
        &mut self,
        path: &Path,
        reader: &mut dyn FnMut(&mut dyn io::Read) -> io::Result<()>,
    ) -> io::Result<()> {
        <F as Filesystem>::readfile(*self, path, reader)
    }

    fn readdir(
        &mut self,
        path: &Path,
        reader: &mut dyn FnMut(&str, ItemType) -> io::Result<()>,
    ) -> io::Result<()> {
        <F as Filesystem>::readdir(*self, path, reader)
    }

    fn stat(&mut self, path: &Path) -> io::Result<ItemType> {
        <F as Filesystem>::stat(*self, path)
    }

    fn rmdir(&mut self, path: &Path) -> io::Result<()> {
        <F as Filesystem>::rmdir(*self, path)
    }

    fn rmfile(&mut self, path: &Path) -> io::Result<()> {
        <F as Filesystem>::rmfile(*self, path)
    }

    fn mkdir(&mut self, path: &Path) -> io::Result<()> {
        <F as Filesystem>::mkdir(*self, path)
    }
}

#[derive(Clone)]
pub struct FilesystemDirectory<F: Filesystem> {
    dir: PathBuf,
    tmp: PathBuf,
    inner: F,
}

impl<F: Filesystem> FilesystemDirectory<F> {
    pub fn new(inner: F, dir: PathBuf) -> io::Result<Self> {
        verify_contained_path(&dir)?;
        Ok(Self {
            dir,
            tmp: Default::default(),
            inner,
        })
    }
}

impl<F: Filesystem> Filesystem for FilesystemDirectory<F> {
    fn writefile(
        &mut self,
        path: &Path,
        writer: &mut dyn FnMut(&mut dyn io::Write) -> io::Result<()>,
    ) -> io::Result<()> {
        self.tmp.clone_from(&self.dir);
        self.tmp.push(path);
        self.inner.writefile(&self.tmp, writer)
    }

    fn readfile(
        &mut self,
        path: &Path,
        reader: &mut dyn FnMut(&mut dyn io::Read) -> io::Result<()>,
    ) -> io::Result<()> {
        self.tmp.clone_from(&self.dir);
        self.tmp.push(path);
        self.inner.readfile(&self.tmp, reader)
    }

    fn readdir(
        &mut self,
        path: &Path,
        reader: &mut dyn FnMut(&str, ItemType) -> io::Result<()>,
    ) -> io::Result<()> {
        self.tmp.clone_from(&self.dir);
        self.tmp.push(path);
        self.inner.readdir(&self.tmp, reader)
    }

    fn stat(&mut self, path: &Path) -> io::Result<ItemType> {
        self.tmp.clone_from(&self.dir);
        self.tmp.push(path);
        self.inner.stat(&self.tmp)
    }

    fn rmdir(&mut self, path: &Path) -> io::Result<()> {
        self.tmp.clone_from(&self.dir);
        self.tmp.push(path);
        self.inner.rmdir(&self.tmp)
    }

    fn rmfile(&mut self, path: &Path) -> io::Result<()> {
        self.tmp.clone_from(&self.dir);
        self.tmp.push(path);
        self.inner.rmfile(&self.tmp)
    }

    fn mkdir(&mut self, path: &Path) -> io::Result<()> {
        self.tmp.clone_from(&self.dir);
        self.tmp.push(path);
        self.inner.mkdir(&self.tmp)
    }
}

pub struct EpiStorageAdapter<F: Filesystem>(Mutex<F>);

impl<F: Filesystem> EpiStorageAdapter<F> {
    pub fn new(inner: F) -> Self {
        Self(Mutex::new(inner))
    }
}

impl<F: Filesystem> eframe::Storage for EpiStorageAdapter<F> {
    fn get_string(&self, key: &str) -> Option<String> {
        let mut lock = self.0.lock();
        let mut str = String::new();
        lock.readfile(key.as_ref(), &mut |r| {
            r.read_to_string(&mut str).map(|_| ())
        })
        .ok()?;

        Some(str)
    }

    fn set_string(&mut self, key: &str, value: String) {
        let mut lock = self.0.lock();

        let mut res = lock.writefile(key.as_ref(), &mut |w| w.write_all(value.as_bytes()));

        if res
            .as_ref()
            .is_err_and(|e| matches!(e.kind(), io::ErrorKind::NotFound))
        {
            res = lock.mkdir("".as_ref());

            if res.is_ok() {
                res = lock.writefile(key.as_ref(), &mut |w| w.write_all(value.as_bytes()));
            }
        }

        if let Err(e) = res {
            panic!("EpiStorageAdapter::get_string error: {e:?}")
        }
    }

    fn flush(&mut self) {}
}

pub fn verify_contained_path(path: &Path) -> io::Result<()> {
    let mut level = 0usize;
    for c in path.components() {
        match c {
            std::path::Component::Prefix(_) | std::path::Component::RootDir => {
                return Err(io::Error::new(
                    io::ErrorKind::Unsupported,
                    "Unsupported path",
                ));
            }
            std::path::Component::CurDir => {}
            std::path::Component::ParentDir => {
                if level == 0 {
                    return Err(io::Error::new(
                        io::ErrorKind::InvalidInput,
                        "Invalid path traversal",
                    ));
                }
                level -= 1;
            }
            std::path::Component::Normal(_) => {
                level += 1;
            }
        }
    }

    Ok(())
}

#[derive(Clone)]
pub struct DummyFilesystem;

impl Filesystem for DummyFilesystem {
    fn writefile(
        &mut self,
        _path: &Path,
        _writer: &mut dyn FnMut(&mut dyn io::Write) -> io::Result<()>,
    ) -> io::Result<()> {
        Err(io::Error::new(io::ErrorKind::Unsupported, "Dummy filesystem: Operation not implemented"))
    }

    fn readfile(
        &mut self,
        _path: &Path,
        _reader: &mut dyn FnMut(&mut dyn io::Read) -> io::Result<()>,
    ) -> io::Result<()> {
        Err(io::Error::new(io::ErrorKind::Unsupported, "Dummy filesystem: Operation not implemented"))
    }

    fn readdir(
        &mut self,
        _path: &Path,
        _reader: &mut dyn FnMut(&str, ItemType) -> io::Result<()>,
    ) -> io::Result<()> {
        Err(io::Error::new(io::ErrorKind::Unsupported, "Dummy filesystem: Operation not implemented"))
    }

    fn stat(&mut self, _path: &Path) -> io::Result<ItemType> {
        Err(io::Error::new(io::ErrorKind::Unsupported, "Dummy filesystem: Operation not implemented"))
    }

    fn rmdir(&mut self, _path: &Path) -> io::Result<()> {
        Err(io::Error::new(io::ErrorKind::Unsupported, "Dummy filesystem: Operation not implemented"))
    }

    fn rmfile(&mut self, _path: &Path) -> io::Result<()> {
        Err(io::Error::new(io::ErrorKind::Unsupported, "Dummy filesystem: Operation not implemented"))
    }

    fn mkdir(&mut self, _path: &Path) -> io::Result<()> {
        Err(io::Error::new(io::ErrorKind::Unsupported, "Dummy filesystem: Operation not implemented"))
    }
}