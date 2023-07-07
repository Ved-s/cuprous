use std::mem::MaybeUninit;

use crate::{vector::Vector, SizeCalc};


#[derive(Default, Debug)]
pub struct FixedVec<T> {
    vec: Vec<Option<T>>,
    first_free: Option<usize>,
}

pub struct VecSetResult<'a, T> {
    pub value_ref: &'a mut T,
    pub prev: Option<T>,
}

impl<T> FixedVec<T> {
    pub fn new(mut vec: Vec<T>) -> Self {
        Self {
            vec: vec.drain(..).map(|v| Some(v)).collect(),
            first_free: None,
        }
    }

    pub fn get(&self, pos: usize) -> Option<&T> {
        self.vec.get(pos)?.as_ref()
    }

    pub fn get_mut(&mut self, pos: usize) -> Option<&mut T> {
        self.vec.get_mut(pos)?.as_mut()
    }

    pub fn remove(&mut self, pos: usize) -> Option<T> {
        if pos >= self.vec.len() {
            return None;
        }

        let item = &mut self.vec[pos];

        if item.is_some() {
            match &mut self.first_free {
                Some(v) if *v < pos => {}
                fe => *fe = Some(pos),
            }
        }

        item.take()
    }

    pub fn exists(&self, pos: usize) -> bool {
        self.vec.get(pos).is_some_and(|v| v.is_some())
    }

    pub fn set<'a>(&'a mut self, value: T, into: usize) -> VecSetResult<'a, T> {
        if into >= self.vec.len() {
            if self.vec.len() != into && self.first_free.is_none() {
                self.first_free = Some(self.vec.len())
            }
            self.vec.reserve(into + 1 - self.vec.len());
            while self.vec.len() < into {
                self.vec.push(None)
            }
            self.vec.push(Some(value));
            return VecSetResult {
                value_ref: self.vec[into].as_mut().unwrap(),
                prev: None,
            };
        };

        if let Some(ff) = self.first_free {
            if ff == into {
                self.first_free = (ff + 1..self.vec.len()).find(|i| matches!(self.vec[*i], None));
            }
        }

        let prev = self.vec.get_mut(into).unwrap().replace(value);

        return VecSetResult {
            value_ref: self.vec[into].as_mut().unwrap(),
            prev: prev,
        };
    }

    pub fn first_free_pos(&self) -> usize {
        match self.first_free {
            Some(v) => v,
            None => self.vec.len(),
        }
    }

    pub fn iter<'a>(&'a self) -> FixedVecIterator<'a, T> {
        FixedVecIterator {
            vec: &self.vec,
            pos: 0,
        }
    }
}

impl<T> From<Vec<T>> for FixedVec<T> {
    fn from(value: Vec<T>) -> Self {
        Self::new(value)
    }
}

pub struct FixedVecIterator<'a, T> {
    vec: &'a Vec<Option<T>>,
    pos: usize,
}

impl<'a, T> Iterator for FixedVecIterator<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        while self.pos < self.vec.len() {
            self.pos += 1;
            if let Some(v) = &self.vec[self.pos - 1] {
                return Some(v);
            }
        }
        None
    }
}


type Chunk<const CHUNK_SIZE: usize, T> = [[T; CHUNK_SIZE]; CHUNK_SIZE];
type ChunksQuarter<const CHUNK_SIZE: usize, T> =
    Vec<Option<Vec<Option<Box<Chunk<CHUNK_SIZE, T>>>>>>;

#[derive(Default)]
pub struct Chunks2D<const CHUNK_SIZE: usize, T: Default> {
    quarters: [ChunksQuarter<CHUNK_SIZE, T>; 4],
}

pub struct ChunksLookaround<'a, const CHUNK_SIZE: usize, T: Default> {
    chunks: &'a Chunks2D<CHUNK_SIZE, T>,
    chunk: &'a Chunk<CHUNK_SIZE, T>,
    pos: Vector<2, isize>,
    in_chunk_pos: Vector<2, usize>,
}

impl<'a, const CHUNK_SIZE: usize, T: Default> ChunksLookaround<'a, CHUNK_SIZE, T> {
    pub fn new(chunks: &'a Chunks2D<CHUNK_SIZE, T>, chunk: &'a Chunk<CHUNK_SIZE, T>, pos: Vector<2, isize>, in_chunk_pos: Vector<2, usize>) -> Self { Self { chunks, chunk, pos, in_chunk_pos } }

    pub fn get_relative(&self, x: isize, y: isize) -> Option<&T> {
        let target_x = x + self.in_chunk_pos.x() as isize;
        let target_y = y + self.in_chunk_pos.y() as isize;

        if target_x < 0
            || target_x >= CHUNK_SIZE as isize
            || target_y < 0
            || target_y >= CHUNK_SIZE as isize
        {
            self.chunks.get(self.pos.x() + x, self.pos.y() + y)
        } else {
            Some(&self.chunk[target_x as usize][target_y as usize])
        }
    }
}

#[allow(dead_code)]
impl<const CHUNK_SIZE: usize, T: Default> Chunks2D<CHUNK_SIZE, T> {
    const QUARTER_TL: usize = 0;
    const QUARTER_TR: usize = 1;
    const QUARTER_BL: usize = 2;
    const QUARTER_BR: usize = 3;

    const QUARTERS_BOTTOM: usize = 0x2;
    const QUARTERS_RIGHT: usize = 0x1;

    fn to_chunk_pos(v: isize) -> (isize, usize) {
        let chunk = v.div_floor(CHUNK_SIZE as isize);
        let pos = (v - (chunk * CHUNK_SIZE as isize)) as usize;
        (chunk, pos)
    }

    fn to_quarter_id_pos(chunk_x: isize, chunk_y: isize) -> ((usize, usize), usize) {
        let (x, right) = if chunk_x >= 0 {
            (chunk_x as usize, true)
        } else {
            ((-chunk_x - 1) as usize, false)
        };

        let (y, bottom) = if chunk_y >= 0 {
            (chunk_y as usize, true)
        } else {
            ((-chunk_y - 1) as usize, false)
        };

        let quarter = match (right, bottom) {
            (false, false) => Self::QUARTER_TL,
            (false, true) => Self::QUARTER_BL,
            (true, false) => Self::QUARTER_TR,
            (true, true) => Self::QUARTER_BR,
        };

        ((x, y), quarter)
    }

    pub fn get_chunk<'a>(&'a self, x: isize, y: isize) -> Option<&'a Chunk<CHUNK_SIZE, T>> {
        let ((qx, qy), qid) = Self::to_quarter_id_pos(x, y);
        let quarter = &self.quarters[qid];
        let col = quarter.get(qx)?;
        col.as_ref()
            .and_then(|col| col.get(qy)?.as_ref().map(|b| b.as_ref()))
    }

    pub fn get_chunk_mut<'a>(
        &'a mut self,
        x: isize,
        y: isize,
    ) -> Option<&'a mut Chunk<CHUNK_SIZE, T>> {
        let ((qx, qy), qid) = Self::to_quarter_id_pos(x, y);
        let quarter = &mut self.quarters[qid];
        let col = quarter.get_mut(qx)?;
        col.as_mut()
            .and_then(|col| col.get_mut(qy)?.as_mut().map(|b| b.as_mut()))
    }

    pub fn get_or_create_chunk_mut(&mut self, x: isize, y: isize) -> &mut Chunk<CHUNK_SIZE, T> {
        let ((qx, qy), qid) = Self::to_quarter_id_pos(x, y);
        let quarter = &mut self.quarters[qid];

        if quarter.capacity() <= qx {
            quarter.reserve_exact(qx + 1 - quarter.len());
        }

        while quarter.len() <= qx {
            quarter.push(None);
        }
        let col = &mut quarter[qx];

        match col.as_mut() {
            None => {
                *col = Some(Vec::with_capacity(qy + 1));
            }
            Some(v) if v.capacity() <= qy => v.reserve_exact(qy + 1 - v.len()),
            _ => {}
        }

        let col = match col.as_mut() {
            Some(v) => v,
            None => unreachable!(),
        };

        while col.len() <= qy {
            col.push(None);
        }
        let chunk = &mut col[qy];

        if chunk.is_none() {
            let mut arr: Chunk<CHUNK_SIZE, T> = unsafe { MaybeUninit::uninit().assume_init() };
            for i in 0..CHUNK_SIZE {
                let col = &mut arr[i];
                for j in 0..CHUNK_SIZE {
                    col[j] = T::default();
                }
            }
            *chunk = Some(Box::new(arr));
        }

        match chunk.as_mut() {
            Some(v) => v,
            None => unreachable!(),
        }
    }

    pub fn get(&self, x: isize, y: isize) -> Option<&T> {
        let (chunk_x, x) = Self::to_chunk_pos(x);
        let (chunk_y, y) = Self::to_chunk_pos(y);

        let chunk = self.get_chunk(chunk_x, chunk_y)?;
        Some(&chunk[x][y])
    }

    pub fn get_mut(&mut self, x: isize, y: isize) -> Option<&mut T> {
        let (chunk_x, x) = Self::to_chunk_pos(x);
        let (chunk_y, y) = Self::to_chunk_pos(y);

        let chunk = self.get_chunk_mut(chunk_x, chunk_y)?;
        Some(&mut chunk[x][y])
    }

    pub fn get_or_create_mut(&mut self, x: isize, y: isize) -> &mut T {
        let (chunk_x, x) = Self::to_chunk_pos(x);
        let (chunk_y, y) = Self::to_chunk_pos(y);

        let chunk = self.get_or_create_chunk_mut(chunk_x, chunk_y);
        &mut chunk[x][y]
    }

    pub fn chunk_exists_at(&self, x: isize, y: isize) -> bool {
        let (chunk_x, _) = Self::to_chunk_pos(x);
        let (chunk_y, _) = Self::to_chunk_pos(y);

        self.get_chunk(chunk_x, chunk_y).is_some()
    }
}

impl<const CHUNK_SIZE: usize, T: SizeCalc + Default> SizeCalc for Chunks2D<CHUNK_SIZE, T> {
    fn calc_size_inner(&self) -> usize {
        self.quarters.calc_size_inner()
    }
}