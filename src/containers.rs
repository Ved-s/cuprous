use std::{
    collections::hash_map::RandomState,
    hash::{BuildHasher, Hasher},
    ops::{Range, Index}, mem::MaybeUninit,
};

use serde::{Deserialize, Serialize};

use crate::{
    unwrap_option_or_continue,
    vector::{Vec2isize, Vec2usize, Vector},
    Intersect,
    r#const::*
};

#[derive(Debug, Clone)]
pub struct FixedVec<T> {
    vec: Vec<Option<T>>,
    first_free: Option<usize>,
}

impl<T: Serialize> Serialize for FixedVec<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.vec.serialize(serializer)
    }
}
impl<'de, T: Deserialize<'de>> Deserialize<'de> for FixedVec<T> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let vec = Vec::<Option<T>>::deserialize(deserializer)?;
        let first_free = vec.iter().enumerate().find_map(|(i, o)| match o {
            Some(_) => None,
            None => Some(i),
        });
        Ok(Self { vec, first_free })
    }
}

impl<T: Default> Default for FixedVec<T> {
    fn default() -> Self {
        Self {
            vec: vec![],
            first_free: None,
        }
    }
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

    pub fn from_option_vec(vec: Vec<Option<T>>) -> Self {
        let first_free = vec.iter().enumerate().find(|(_, v)| v.is_none()).map(|(i, _)| i);
        Self {
            vec,
            first_free,
        }
    }

    pub fn get_nth_existing_index(&self, pos: usize) -> Option<usize> {
        if pos >= self.vec.len() {
            return None;
        }

        let mut n = 0;
        for i in 0..self.vec.len() {
            if self.vec[i].is_some() {
                if n == pos {
                    return Some(i);
                }
                n += 1;
            }
        }
        None
    }

    pub fn get_nth_existing_index_filtered(&self, pos: usize, f: impl Fn(&T) -> bool) -> Option<usize> {
        if pos >= self.vec.len() {
            return None;
        }

        let mut n = 0;
        for i in 0..self.vec.len() {
            if self.vec[i].as_ref().is_some_and(&f) {
                if n == pos {
                    return Some(i);
                }
                n += 1;
            }
        }
        None
    }

    pub fn get(&self, pos: usize) -> Option<&T> {
        self.vec.get(pos)?.as_ref()
    }

    pub fn get_clone(&self, pos: usize) -> Option<T>
    where
        T: Clone,
    {
        self.vec.get(pos)?.as_ref().cloned()
    }

    pub fn get_mut(&mut self, pos: usize) -> Option<&mut T> {
        self.vec.get_mut(pos)?.as_mut()
    }

    pub fn get_or_create_mut(&mut self, pos: usize, creator: impl FnOnce() -> T) -> &mut T {
        if pos >= self.vec.len() {
            return self.set(creator(), pos).value_ref;
        }
        self.update_first_free_set(pos);

        let option = &mut self.vec[pos];
        option.get_or_insert_with(creator)
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

        let value = item.take();

        if pos == self.vec.len() - 1 {
            self.vec.remove(pos);
            while !self.vec.is_empty() && self.vec[self.vec.len() - 1].is_none() {
                self.vec.remove(self.vec.len() - 1);
            }
            if let Some(v) = self.first_free {
                if v >= self.vec.len() {
                    self.first_free = None;
                }
            }
        }

        self.test_free_correct();

        value
    }

    pub fn exists(&self, pos: usize) -> bool {
        self.vec.get(pos).is_some_and(|v| v.is_some())
    }

    pub fn set(&mut self, value: T, into: usize) -> VecSetResult<'_, T> {
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

        self.update_first_free_set(into);

        let prev = self.vec.get_mut(into).unwrap().replace(value);

        self.test_free_correct();

        return VecSetResult {
            value_ref: self.vec[into].as_mut().unwrap(),
            prev,
        };
    }

    fn update_first_free_set(&mut self, into: usize) {
        if let Some(ff) = self.first_free {
            if ff == into {
                self.first_free = (ff + 1..self.vec.len()).find(|i| self.vec[*i].is_none());
            }
        }
    }

    pub fn first_free_pos(&self) -> usize {
        match self.first_free {
            Some(v) => v,
            None => self.vec.len(),
        }
    }

    pub fn iter(&self) -> FixedVecIterator<'_, T> {
        FixedVecIterator {
            vec: &self.vec,
            pos: 0,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.vec.is_empty()
    }

    pub fn inner(&self) -> &Vec<Option<T>> {
        &self.vec
    }

    fn test_free_correct(&self) {
        assert!(!self.first_free.is_some_and(|v| self.get(v).is_some()));
    }

    pub fn clear(&mut self) {
        self.vec.clear();
        self.first_free = None;
    }

    fn len(&self) -> usize {
        self.vec.iter().filter(|o| o.is_some()).count()
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

// Cols then rows, chunk[x][y]
type Chunk<const CHUNK_SIZE: usize, T> = [[T; CHUNK_SIZE]; CHUNK_SIZE];

// Vec of rows, vec of cols, quaerter[y][x]
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
    pub fn new(
        chunks: &'a Chunks2D<CHUNK_SIZE, T>,
        chunk: &'a Chunk<CHUNK_SIZE, T>,
        pos: Vector<2, isize>,
        in_chunk_pos: Vector<2, usize>,
    ) -> Self {
        Self {
            chunks,
            chunk,
            pos,
            in_chunk_pos,
        }
    }

    pub fn get_relative(&self, rel: impl Into<Vec2isize>) -> Option<&T> {
        let rel = rel.into();
        let target = rel + self.in_chunk_pos.convert(|v| v as isize);

        if target.x() < 0
            || target.x() >= CHUNK_SIZE as isize
            || target.y() < 0
            || target.y() >= CHUNK_SIZE as isize
        {
            self.chunks.get(self.pos + rel)
        } else {
            Some(&self.chunk[target.x() as usize][target.y() as usize])
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

    fn to_chunk_pos(pos: Vec2isize) -> (Vec2isize, Vec2usize) {
        let chunk_x = pos.x().div_floor(CHUNK_SIZE as isize);
        let chunk_y = pos.y().div_floor(CHUNK_SIZE as isize);
        let pos_x = (pos.x() - (chunk_x * CHUNK_SIZE as isize)) as usize;
        let pos_y = (pos.y() - (chunk_y * CHUNK_SIZE as isize)) as usize;
        ([chunk_x, chunk_y].into(), [pos_x, pos_y].into())
    }

    fn to_quarter_id_pos(chunk_pos: Vec2isize) -> ((usize, usize), usize) {
        let (x, right) = if chunk_pos.x() >= 0 {
            (chunk_pos.x() as usize, true)
        } else {
            ((-chunk_pos.x() - 1) as usize, false)
        };

        let (y, bottom) = if chunk_pos.y() >= 0 {
            (chunk_pos.y() as usize, true)
        } else {
            ((-chunk_pos.y() - 1) as usize, false)
        };

        let quarter = match (right, bottom) {
            (false, false) => Self::QUARTER_TL,
            (false, true) => Self::QUARTER_BL,
            (true, false) => Self::QUARTER_TR,
            (true, true) => Self::QUARTER_BR,
        };

        ((x, y), quarter)
    }

    pub fn get_chunk(&self, pos: impl Into<Vec2isize>) -> Option<&Chunk<CHUNK_SIZE, T>> {
        let ((qx, qy), qid) = Self::to_quarter_id_pos(pos.into());
        let quarter = &self.quarters[qid];
        let row = quarter.get(qy)?;
        row.as_ref()
            .and_then(|row| row.get(qx)?.as_ref().map(|b| b.as_ref()))
    }

    pub fn get_chunk_mut(
        &mut self,
        pos: impl Into<Vec2isize>,
    ) -> Option<&mut Chunk<CHUNK_SIZE, T>> {
        let ((qx, qy), qid) = Self::to_quarter_id_pos(pos.into());
        let quarter = &mut self.quarters[qid];
        let row = quarter.get_mut(qy)?;
        row.as_mut()
            .and_then(|row| row.get_mut(qx)?.as_mut().map(|b| b.as_mut()))
    }

    pub fn get_or_create_chunk_mut(
        &mut self,
        pos: impl Into<Vec2isize>,
    ) -> &mut Chunk<CHUNK_SIZE, T> {
        let ((qx, qy), qid) = Self::to_quarter_id_pos(pos.into());
        let quarter = &mut self.quarters[qid];

        if quarter.capacity() <= qy {
            quarter.reserve_exact(qy + 1 - quarter.len());
        }

        while quarter.len() <= qy {
            quarter.push(None);
        }
        let row = &mut quarter[qy];

        match row.as_mut() {
            None => {
                *row = Some(Vec::with_capacity(qx + 1));
            }
            Some(v) if v.capacity() <= qx => v.reserve_exact(qx + 1 - v.len()),
            _ => {}
        }

        let row = match row.as_mut() {
            Some(v) => v,
            None => unreachable!(),
        };

        while row.len() <= qx {
            row.push(None);
        }
        let chunk = &mut row[qx];

        if chunk.is_none() {
            let arr = std::array::from_fn(|_| std::array::from_fn(|_| T::default()));
            *chunk = Some(Box::new(arr));
        }

        match chunk.as_mut() {
            Some(v) => v,
            None => unreachable!(),
        }
    }

    pub fn get(&self, pos: impl Into<Vec2isize>) -> Option<&T> {
        let (chunk_pos, pos) = Self::to_chunk_pos(pos.into());

        let chunk = self.get_chunk(chunk_pos)?;
        Some(&chunk[pos.x()][pos.y()])
    }

    pub fn get_mut(&mut self, pos: impl Into<Vec2isize>) -> Option<&mut T> {
        let (chunk_pos, pos) = Self::to_chunk_pos(pos.into());
        let chunk = self.get_chunk_mut(chunk_pos)?;
        Some(&mut chunk[pos.x()][pos.y()])
    }

    pub fn get_or_create_mut(&mut self, pos: impl Into<Vec2isize>) -> &mut T {
        let (chunk_pos, pos) = Self::to_chunk_pos(pos.into());
        let chunk = self.get_or_create_chunk_mut(chunk_pos);
        &mut chunk[pos.x()][pos.y()]
    }

    pub fn chunk_exists_at(&self, pos: impl Into<Vec2isize>) -> bool {
        let (chunk_pos, _) = Self::to_chunk_pos(pos.into());

        self.get_chunk(chunk_pos).is_some()
    }

    pub fn get_chunk_row_range(&self, row: isize) -> Range<isize> {
        let (leftq, rightq, row) = if row < 0 {
            (
                &self.quarters[Self::QUARTER_TL],
                &self.quarters[Self::QUARTER_TR],
                (-row - 1) as usize,
            )
        } else {
            (
                &self.quarters[Self::QUARTER_BL],
                &self.quarters[Self::QUARTER_BR],
                row as usize,
            )
        };

        let leftcount = leftq
            .get(row)
            .map(|ro| ro.as_ref().map(|r| r.len()).unwrap_or(0))
            .unwrap_or(0) as isize;
        let rightcount = rightq
            .get(row)
            .map(|ro| ro.as_ref().map(|r| r.len()).unwrap_or(0))
            .unwrap_or(0) as isize;
        Range {
            start: -leftcount,
            end: rightcount,
        }
    }

    pub fn iter_area(
        &self,
        pos: Vector<2, isize>,
        size: Vector<2, usize>,
    ) -> ChunksAreaIterator<'_, CHUNK_SIZE, T> {
        let size = size.convert(|v| v as isize);

        let tiles_tl = pos;
        let tiles_br = pos + size;

        let chunks_tl = pos.convert(|v| v.div_floor(CHUNK_SIZE as isize));
        let chunks_br = (pos + size).convert(|v| v.div_ceil(CHUNK_SIZE as isize));

        let empty = size.x() == 0 || size.y() == 0;

        let in_chunk_start = tiles_tl - chunks_tl * CHUNK_SIZE as isize;
        let in_chunk_end = Vector::single_value(CHUNK_SIZE as isize)
            - ((chunks_br * CHUNK_SIZE as isize) - tiles_br);

        ChunksAreaIterator {
            chunks: self,
            chunk_bounds_tl: chunks_tl,
            chunk_bounds_br_excl: chunks_br,
            chunk: self.get_chunk(chunks_tl),
            chunk_pos: chunks_tl,
            in_chunk_pos: in_chunk_start.convert(|v| v as usize),
            in_chunk_start: in_chunk_start.convert(|v| v as usize),
            in_chunk_end_excl: in_chunk_end.convert(|v| v as usize),
            empty,
        }
    }

    pub fn for_each_item<P>(
        &self,
        tl: Vector<2, isize>,
        br: Vector<2, isize>,
        pass: &P,
        f: impl Fn(Vector<2, isize>, &T, &P),
    ) {
        let chunks_tl = tl.convert(|v| v.div_floor(CHUNK_SIZE as isize));
        let chunks_br = br.convert(|v| v.div_floor(CHUNK_SIZE as isize));

        for cy in chunks_tl.y()..=chunks_br.y() {
            let rowrange = self.get_chunk_row_range(cy);
            let rowrange = Range {
                start: rowrange.start,
                end: rowrange.end,
            };

            for cx in (chunks_tl.x()..chunks_br.x() + 1).intersect(&rowrange) {
                let chunk_coord: Vector<2, isize> = [cx, cy].into();
                let chunk_tl = chunk_coord * 16;
                let chunk = unwrap_option_or_continue!(self.get_chunk(chunk_coord));

                let chunk_start = tl - chunk_tl;
                let chunk_wnd = br - chunk_tl;

                for j in 0..16 {
                    if j < chunk_start.y() {
                        continue;
                    } else if j > chunk_wnd.y() {
                        break;
                    }

                    for i in 0..16 {
                        if i < chunk_start.x() {
                            continue;
                        } else if i > chunk_wnd.x() {
                            break;
                        }

                        let item = &chunk[i as usize][j as usize];

                        let pos = chunk_tl + [i, j];

                        f(pos, item, pass);
                    }
                }
            }
        }
    }
}

pub struct ChunksAreaIterator<'a, const CHUNK_SIZE: usize, T: Default> {
    chunks: &'a Chunks2D<CHUNK_SIZE, T>,

    chunk_bounds_tl: Vector<2, isize>,
    chunk_bounds_br_excl: Vector<2, isize>,

    chunk: Option<&'a Chunk<CHUNK_SIZE, T>>,
    chunk_pos: Vector<2, isize>,
    in_chunk_pos: Vector<2, usize>,

    in_chunk_start: Vector<2, usize>,
    in_chunk_end_excl: Vector<2, usize>,

    empty: bool,
}

impl<'a, const CHUNK_SIZE: usize, T: Default> ChunksAreaIterator<'a, CHUNK_SIZE, T> {
    fn next_chunk(&mut self) -> bool {
        if self.empty || self.chunk_pos.y() >= self.chunk_bounds_br_excl.y() {
            return false;
        }

        self.in_chunk_pos = [0, 0].into();

        loop {
            *self.chunk_pos.x_mut() += 1;
            if self.chunk_pos.x() >= self.chunk_bounds_br_excl.x() {
                *self.chunk_pos.x_mut() = self.chunk_bounds_tl.x();
                *self.chunk_pos.y_mut() += 1;
                if self.chunk_pos.y() >= self.chunk_bounds_br_excl.y() {
                    self.chunk = None;
                    self.empty = true;
                    return false;
                }
            }

            self.chunk = self.chunks.get_chunk(self.chunk_pos);

            match self.chunk {
                Some(_) => {
                    self.set_in_chunk_start_pos();
                    return true;
                }
                None => continue,
            }
        }
    }

    fn transform_coord(chunk: isize, in_chunk: usize) -> isize {
        chunk * CHUNK_SIZE as isize + in_chunk as isize
    }

    fn next_item(&mut self) {
        let mut pos = self.in_chunk_pos;
        *pos.x_mut() += 1;

        let x_end = if self.chunk_pos.x() == self.chunk_bounds_br_excl.x() - 1 {
            self.in_chunk_end_excl.x()
        } else {
            CHUNK_SIZE
        };

        if pos.x() >= x_end {
            let x_start = if self.chunk_pos.x() == self.chunk_bounds_tl.x() {
                self.in_chunk_start.x()
            } else {
                0
            };

            *pos.x_mut() = x_start;
            *pos.y_mut() += 1;

            let y_end = if self.chunk_pos.y() == self.chunk_bounds_br_excl.y() - 1 {
                self.in_chunk_end_excl.y()
            } else {
                CHUNK_SIZE
            };

            if pos.y() >= y_end {
                self.next_chunk();
                return;
            }
        }

        self.in_chunk_pos = pos;
    }

    fn set_in_chunk_start_pos(&mut self) {
        let x_start = if self.chunk_pos.x() == self.chunk_bounds_tl.x() {
            self.in_chunk_start.x()
        } else {
            0
        };

        let y_start = if self.chunk_pos.y() == self.chunk_bounds_tl.y() {
            self.in_chunk_start.y()
        } else {
            0
        };
        self.in_chunk_pos = [x_start, y_start].into();
    }
}

impl<'a, const CHUNK_SIZE: usize, T: Default> Iterator for ChunksAreaIterator<'a, CHUNK_SIZE, T> {
    type Item = (Vector<2, isize>, &'a T);

    fn next(&mut self) -> Option<Self::Item> {
        if self.empty {
            return None;
        }

        if self.chunk.is_none() && !self.next_chunk() {
            return None;
        }

        match self.chunk {
            None => None,
            Some(chunk) => {
                let item = &chunk[self.in_chunk_pos.x()][self.in_chunk_pos.y()];
                let tx = Self::transform_coord(self.chunk_pos.x(), self.in_chunk_pos.x());
                let ty = Self::transform_coord(self.chunk_pos.y(), self.in_chunk_pos.y());
                let res = ([tx, ty].into(), item);

                self.next_item();

                Some(res)
            }
        }
    }
}

pub struct RandomQueue<T, S: BuildHasher = RandomState> {
    vec: FixedVec<T>,
    hasher: <S as BuildHasher>::Hasher,
}

impl<T: Serialize, H: BuildHasher> Serialize for RandomQueue<T, H> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer {
        self.vec.inner().serialize(serializer)
    }
}

impl<T, S: Default + BuildHasher> Default for RandomQueue<T, S> {
    fn default() -> Self {
        Self::with_hasher(&Default::default())
    }
}

impl<T> RandomQueue<T> {
    pub fn new() -> RandomQueue<T, RandomState> {
        Default::default()
    }

    pub fn from_vec(vec: Vec<T>) -> RandomQueue<T, RandomState> {
        Self {
            vec: vec.into(),
            ..Default::default()
        }
    }
}

impl<T, S: BuildHasher> RandomQueue<T, S> {
    pub fn with_hasher(hash_builder: &S) -> Self {
        Self {
            vec: vec![].into(),
            hasher: hash_builder.build_hasher(),
        }
    }

    pub fn enqueue(&mut self, value: T) {
        let pos = self.vec.first_free_pos();
        self.vec.set(value, pos);
        self.hasher.write_usize(pos);
    }

    pub fn dequeue(&mut self) -> Option<T> {
        if self.vec.is_empty() {
            return None;
        }

        let len = self.vec.iter().count();
        assert_ne!(len, 0, "FixedVec::is_empty() is false with zero length");

        let pos = (self.hasher.finish() % len as u64) as usize;
        let real_pos = match self.vec.get_nth_existing_index(pos) {
            Some(v) => v,
            None => unreachable!(
                "Queue length ({}) does not match internal vector ({})!",
                len,
                self.vec.iter().count()
            ),
        };
        let item = match self.vec.remove(real_pos) {
            Some(v) => v,
            None => unreachable!(),
        };
        self.hasher.write_usize(pos);
        Some(item)
    }

    pub fn dequeue_filtered(&mut self, f: impl Fn(&T) -> bool) -> Option<T> {
        if self.vec.is_empty() {
            return None;
        }

        let len = self.vec.iter().filter(|v| f(*v)).count();
        if len == 0 {
            return None;
        }

        let pos = (self.hasher.finish() % len as u64) as usize;
        let real_pos = match self.vec.get_nth_existing_index_filtered(pos, f) {
            Some(v) => v,
            None => unreachable!(
                "Queue length ({}) does not match internal vector ({})!",
                len,
                self.vec.iter().count()
            ),
        };
        let item = match self.vec.remove(real_pos) {
            Some(v) => v,
            None => unreachable!(),
        };
        self.hasher.write_usize(pos);
        Some(item)
    }

    pub fn clear(&mut self) {
        self.vec.clear();
    }

    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.vec.iter()
    }

    pub fn len(&self) -> usize {
        self.vec.len()
    }
}

pub struct ConstRingBuffer<const SIZE: usize, T> {
    array: [MaybeUninit<T>; SIZE],

    /// Points to position after last valid item 
    pos: usize,
    len: usize

    //    /---len---\
    // ---###########------
    //               ^- pos 
}

#[allow(clippy::uninit_assumed_init)]
#[allow(unused)]
impl<T, const SIZE: usize> ConstRingBuffer<SIZE, T> {
    pub fn new() -> Self {
        Self {
            array: unsafe { MaybeUninit::uninit().assume_init() },
            pos: 0,
            len: 0,
        }
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn is_continious(&self) -> bool {
        self.pos >= self.len || self.pos == 0
    }

    pub fn as_slice(&self) -> Option<&[T]> {
        if self.pos >= self.len {
            let start = self.pos - self.len;
            Some(unsafe { std::mem::transmute(&self.array[start..start+self.len]) })
        } else if self.pos == 0 {
            Some(unsafe { std::mem::transmute(&self.array[SIZE-self.len..]) })
        } else {
            None
        }
    }

    pub fn as_mut_slice(&mut self) -> Option<&mut [T]> {
        if self.pos >= self.len {
            let start = self.pos - self.len;
            Some(unsafe { std::mem::transmute(&mut self.array[start..start+self.len]) })
        } else if self.pos == 0 {
            Some(unsafe { std::mem::transmute(&mut self.array[SIZE-self.len..]) })
        } else {
            None
        }
    }
}

#[allow(unused)]
impl<T, const SIZE: usize> ConstRingBuffer<SIZE, T> where Bool<{SIZE > 0}>: True {
    pub fn push_back(&mut self, value: T) {
        if self.len >= SIZE {
            unsafe { self.array[self.pos].assume_init_drop() }
        }
        self.array[self.pos].write(value);
        self.len = (self.len + 1).min(SIZE);
        self.pos = (self.pos + 1) % SIZE;
    }
    pub fn push_front(&mut self, value: T) {
        let pos = (self.pos + SIZE - self.len - 1) % SIZE;
        if self.len >= SIZE {
            unsafe { self.array[pos].assume_init_drop() }
            self.pos = (self.pos - 1 + SIZE) % SIZE;
        }
        self.array[pos].write(value);
        self.len = (self.len + 1).min(SIZE);
    }

    pub fn pop_back(&mut self) -> Option<T> {
        if self.len == 0 {
            None
        } else {
            let pos = (self.pos - 1 + SIZE) % SIZE;
            self.len -= 1;
            Some(unsafe { self.array[pos].assume_init_read() })
        }
    }

    pub fn pop_front(&mut self) -> Option<T> {
        if self.len == 0 {
            None
        } else {
            let pos = (self.pos + SIZE - self.len) % SIZE;
            self.len -= 1;
            Some(unsafe { self.array[pos].assume_init_read() })
        }
    }

    pub fn iter(&self) -> ConstRingBufferRefIterator<'_, T> {
        let start = (self.pos + SIZE - self.len) % SIZE;
        ConstRingBufferRefIterator { buf: unsafe { std::mem::transmute(self.array.as_ref()) }, start, len: self.len, pos: 0 }
    }
}

impl<const SIZE: usize, T: Default> Default for ConstRingBuffer<SIZE, T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T, const SIZE: usize> Index<usize> for ConstRingBuffer<SIZE, T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        assert!(index < self.len, "oob read");
        let back = self.len - index;
        let pos = (self.pos + SIZE - back) % SIZE;
        unsafe { self.array[pos].assume_init_ref() }
    }
}

pub struct ConstRingBufferRefIterator<'a, T> {
    buf: &'a [T],
    start: usize,
    len: usize,

    // max: len
    pos: usize,
}

impl<'a, T> Iterator for ConstRingBufferRefIterator<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        if self.pos >= self.len {
            None
        } else {
            self.pos += 1;
            Some(&self.buf[(self.start + self.pos - 1) % self.buf.len()])
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let size = self.len - self.pos;
        (size, Some(size))
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn fixed_vec_remove_shrink() {
        let mut fv = FixedVec::new(vec![]);

        fv.set(35, 0);
        fv.set(15, 1);
        fv.set(100, 4);

        assert_eq!(fv.vec.len(), 5);
        fv.remove(4);
        assert_eq!(fv.vec.len(), 2);
    }

    #[test]
    fn fixed_vec_correct_free() {
        let mut fv = FixedVec::new(vec![]);

        fv.set(35, 0);
        fv.set(15, 1);
        fv.set(100, 4);

        assert_eq!(fv.first_free_pos(), 2);

        fv.set(25, 2);
        fv.remove(4);
        assert_eq!(fv.first_free_pos(), 3);

        fv.remove(1);
        assert_eq!(fv.first_free_pos(), 1);
    }

    #[test]
    fn ring_buf_basic() {
        let mut buf = ConstRingBuffer::<4, i32>::new();

        buf.push_back(1);
        buf.push_back(2);
        buf.push_back(3);
        buf.push_back(4);
        buf.push_back(5);

        let vec: Vec<_> = buf.iter().cloned().collect();
        assert_eq!(vec, [2, 3, 4, 5]);

        assert_eq!(buf[0], 2);
        assert_eq!(buf[1], 3);
        assert_eq!(buf[2], 4);
        assert_eq!(buf[3], 5);
    }
}