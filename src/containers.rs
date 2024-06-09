use std::{
    mem::MaybeUninit,
    ops::{Bound, Index, Range, RangeBounds},
};

use serde::{Deserialize, Serialize};

use crate::{
    div_floor_isize,
    vector::{Vec2isize, Vec2usize},
};

#[derive(Debug, Clone)]
pub struct FixedVec<T> {
    pub inner: Vec<Option<T>>,
    first_free: Option<usize>,
}

impl<T: Serialize> Serialize for FixedVec<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.inner.serialize(serializer)
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
        Ok(Self {
            inner: vec,
            first_free,
        })
    }
}

impl<T: Default> Default for FixedVec<T> {
    fn default() -> Self {
        Self::new()
    }
}

pub struct VecSetResult<'a, T> {
    pub value_ref: &'a mut T,
    pub prev: Option<T>,
}

impl<T> FixedVec<T> {
    pub fn new() -> Self {
        Self {
            inner: vec![],
            first_free: None,
        }
    }

    pub fn from_vec(mut vec: Vec<T>) -> Self {
        Self {
            inner: vec.drain(..).map(|v| Some(v)).collect(),
            first_free: None,
        }
    }

    pub fn from_option_vec(vec: Vec<Option<T>>) -> Self {
        let first_free = vec
            .iter()
            .enumerate()
            .find(|(_, v)| v.is_none())
            .map(|(i, _)| i);
        Self {
            inner: vec,
            first_free,
        }
    }

    pub fn get_nth_existing_index(&self, pos: usize) -> Option<usize> {
        if pos >= self.inner.len() {
            return None;
        }

        let mut n = 0;
        for i in 0..self.inner.len() {
            if self.inner[i].is_some() {
                if n == pos {
                    return Some(i);
                }
                n += 1;
            }
        }
        None
    }

    pub fn get_nth_existing_index_filtered(
        &self,
        pos: usize,
        f: impl Fn(&T) -> bool,
    ) -> Option<usize> {
        if pos >= self.inner.len() {
            return None;
        }

        let mut n = 0;
        for i in 0..self.inner.len() {
            if self.inner[i].as_ref().is_some_and(&f) {
                if n == pos {
                    return Some(i);
                }
                n += 1;
            }
        }
        None
    }

    pub fn get(&self, pos: usize) -> Option<&T> {
        self.inner.get(pos)?.as_ref()
    }

    pub fn get_mut(&mut self, pos: usize) -> Option<&mut T> {
        self.inner.get_mut(pos)?.as_mut()
    }

    pub fn get_or_create_mut(&mut self, pos: usize, creator: impl FnOnce() -> T) -> &mut T {
        if pos >= self.inner.len() {
            return self.set(pos, creator()).value_ref;
        }
        self.update_first_free_set(pos);

        let option = &mut self.inner[pos];
        option.get_or_insert_with(creator)
    }

    pub fn remove(&mut self, pos: usize) -> Option<T> {
        if pos >= self.inner.len() {
            return None;
        }

        let item = &mut self.inner[pos];

        if item.is_some() {
            match &mut self.first_free {
                Some(v) if *v < pos => {}
                fe => *fe = Some(pos),
            }
        }

        let value = item.take();

        if pos == self.inner.len() - 1 {
            self.inner.remove(pos);
            self.strip_inner();
            if let Some(v) = self.first_free {
                if v >= self.inner.len() {
                    self.first_free = None;
                }
            }
        }

        self.test_free_correct();

        value
    }

    fn strip_inner(&mut self) {
        while !self.inner.is_empty() && self.inner[self.inner.len() - 1].is_none() {
            self.inner.remove(self.inner.len() - 1);
        }
    }

    pub fn exists(&self, pos: usize) -> bool {
        self.inner.get(pos).is_some_and(|v| v.is_some())
    }

    pub fn set(&mut self, pos: usize, value: T) -> VecSetResult<'_, T> {
        if pos >= self.inner.len() {
            if self.inner.len() != pos && self.first_free.is_none() {
                self.first_free = Some(self.inner.len())
            }
            self.inner.reserve(pos + 1 - self.inner.len());
            while self.inner.len() < pos {
                self.inner.push(None)
            }
            self.inner.push(Some(value));
            return VecSetResult {
                value_ref: self.inner[pos].as_mut().unwrap(),
                prev: None,
            };
        };

        self.update_first_free_set(pos);

        let prev = self.inner.get_mut(pos).unwrap().replace(value);

        self.test_free_correct();

        return VecSetResult {
            value_ref: self.inner[pos].as_mut().unwrap(),
            prev,
        };
    }

    fn update_first_free_set(&mut self, into: usize) {
        if let Some(ff) = self.first_free {
            if ff == into {
                self.first_free = (ff + 1..self.inner.len()).find(|i| self.inner[*i].is_none());
            }
        }
    }

    pub fn first_free_pos(&self) -> usize {
        match self.first_free {
            Some(v) => v,
            None => self.inner.len(),
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.inner.iter().filter_map(|v| v.as_ref())
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut T> {
        self.inner.iter_mut().filter_map(|v| v.as_mut())
    }

    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    fn test_free_correct(&self) {
        assert!(!self.first_free.is_some_and(|v| self.get(v).is_some()));
    }

    pub fn clear(&mut self) {
        self.inner.clear();
        self.first_free = None;
    }

    pub fn len(&self) -> usize {
        self.inner.iter().filter(|o| o.is_some()).count()
    }

    pub fn drain(&mut self, range: impl RangeBounds<usize>) -> FixedVecDrain<'_, T> {
        let start = match range.start_bound() {
            Bound::Included(i) => *i,
            Bound::Excluded(e) => *e + 1,
            Bound::Unbounded => 0,
        };
        let end = match range.end_bound() {
            Bound::Included(i) => *i + 1,
            Bound::Excluded(e) => *e,
            Bound::Unbounded => self.inner.len(),
        };
        if end > self.inner.len() {
            panic!("invalid: end {end} > length {}", self.inner.len());
        }
        if end < start {
            panic!("invalid: end {end} < start {start}");
        }
        let len = end - start;

        FixedVecDrain {
            vec: self,
            start,
            len,
            pos: 0,
        }
    }

    // pub fn read_or_create_locked<R>(
    //     lock: &RwLock<Self>,
    //     pos: usize,
    //     creator: impl FnOnce() -> T,
    //     reader: impl FnOnce(&T) -> R,
    // ) -> R {
    //     if let Some(value) = lock.read().get(pos) {
    //         return reader(value);
    //     }

    //     let mut lock = lock.write();
    //     if let Some(value) = lock.get(pos) {
    //         reader(value)
    //     } else {
    //         let r = lock.set(pos, creator()).value_ref;
    //         reader(r)
    //     }
    // }
}

impl<T> From<Vec<T>> for FixedVec<T> {
    fn from(value: Vec<T>) -> Self {
        Self::from_vec(value)
    }
}

pub struct FixedVecDrain<'a, T> {
    vec: &'a mut FixedVec<T>,
    start: usize,
    len: usize,
    pos: usize,
}

impl<T> Drop for FixedVecDrain<'_, T> {
    fn drop(&mut self) {
        if self.pos >= self.len {
            return;
        }

        for i in self.start + self.pos..self.start + self.len {
            self.vec.inner[i] = None;
        }
        self.vec.strip_inner();
        self.vec.first_free = match self.vec.first_free {
            None => None,
            Some(ff) => {
                let ff = ff.min(self.start);
                if ff >= self.vec.inner.len() {
                    None
                } else {
                    Some(ff)
                }
            }
        }
    }
}

impl<T> Iterator for FixedVecDrain<'_, T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        while self.pos < self.len {
            let item = self.vec.remove(self.pos);
            self.pos += 1;
            match item {
                Some(v) => return Some(v),
                None => continue,
            }
        }

        None
    }
}

// Cols then rows, chunk[x][y]
type Chunk<const CHUNK_SIZE: usize, T> = [[T; CHUNK_SIZE]; CHUNK_SIZE];

// Vec of rows, vec of cols, quarter[y][x]
type ChunksQuarter<const CHUNK_SIZE: usize, T> =
    Vec<Option<Vec<Option<Box<Chunk<CHUNK_SIZE, T>>>>>>;

#[derive(Default)]
pub struct Chunks2D<const CHUNK_SIZE: usize, T: Default> {
    quarters: [ChunksQuarter<CHUNK_SIZE, T>; 4],
}

pub struct ChunksLookaround<'a, const CHUNK_SIZE: usize, T: Default> {
    chunks: &'a Chunks2D<CHUNK_SIZE, T>,
    chunk: &'a Chunk<CHUNK_SIZE, T>,
    chunk_pos: Vec2isize,
    pos: Vec2usize,
}

impl<'a, const CHUNK_SIZE: usize, T: Default> ChunksLookaround<'a, CHUNK_SIZE, T> {
    pub fn new(
        chunks: &'a Chunks2D<CHUNK_SIZE, T>,
        chunk: &'a Chunk<CHUNK_SIZE, T>,
        chunk_pos: Vec2isize,
        pos: Vec2usize,
    ) -> Self {
        Self {
            chunks,
            chunk,
            chunk_pos,
            pos,
        }
    }

    pub fn get_relative(&self, rel: impl Into<Vec2isize>) -> Option<&T> {
        let rel = rel.into();
        let target = rel + self.pos.convert(|v| v as isize);

        if target.x < 0
            || target.x >= CHUNK_SIZE as isize
            || target.y < 0
            || target.y >= CHUNK_SIZE as isize
        {
            self.chunks
                .get(self.chunk_pos * CHUNK_SIZE as isize + target)
        } else {
            Some(&self.chunk[target.x as usize][target.y as usize])
        }
    }

    pub fn get(&self, pos: impl Into<Vec2isize>) -> Option<&T> {
        let pos = pos.into();
        let (chunk, ipos) = global_to_chunk_pos(pos, CHUNK_SIZE);
        if chunk != self.chunk_pos {
            return self.chunks.get(pos);
        }

        Some(&self.chunk[ipos.x][ipos.y])
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

    fn to_quarter_id_pos(chunk_pos: Vec2isize) -> ((usize, usize), usize) {
        let (x, right) = if chunk_pos.x >= 0 {
            (chunk_pos.x as usize, true)
        } else {
            ((-chunk_pos.x - 1) as usize, false)
        };

        let (y, bottom) = if chunk_pos.y >= 0 {
            (chunk_pos.y as usize, true)
        } else {
            ((-chunk_pos.y - 1) as usize, false)
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
        let (chunk_pos, pos) = global_to_chunk_pos(pos.into(), CHUNK_SIZE);

        let chunk = self.get_chunk(chunk_pos)?;
        Some(&chunk[pos.x][pos.y])
    }

    pub fn get_mut(&mut self, pos: impl Into<Vec2isize>) -> Option<&mut T> {
        let (chunk_pos, pos) = global_to_chunk_pos(pos.into(), CHUNK_SIZE);
        let chunk = self.get_chunk_mut(chunk_pos)?;
        Some(&mut chunk[pos.x][pos.y])
    }

    pub fn get_or_create_mut(&mut self, pos: impl Into<Vec2isize>) -> &mut T {
        let (chunk_pos, pos) = global_to_chunk_pos(pos.into(), CHUNK_SIZE);
        let chunk = self.get_or_create_chunk_mut(chunk_pos);
        &mut chunk[pos.x][pos.y]
    }

    pub fn chunk_exists_at(&self, pos: impl Into<Vec2isize>) -> bool {
        let (chunk_pos, _) = global_to_chunk_pos(pos.into(), CHUNK_SIZE);

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

    pub fn iter_chunks(
        &self,
        pos: Vec2isize,
        size: Vec2usize,
    ) -> ChunksIterator<'_, CHUNK_SIZE, T> {
        let size = size.convert(|v| v as isize);

        let tl = pos;
        let br = pos + size;

        let empty = size.x == 0 || size.y == 0;

        let (y, empty) = 'fy: {
            if !empty {
                for y in tl.y..br.y {
                    if self.get_chunk_row_range(y).is_empty() {
                        continue;
                    }
                    break 'fy (y, false);
                }
            }
            (tl.y, true)
        };

        let tl = Vec2isize::new(tl.x, y);

        let range = self.get_chunk_row_range(tl.y);

        ChunksIterator {
            chunks: self,
            chunk_bounds_tl: tl,
            chunk_bounds_br_excl: br,

            chunk_pos: [range.start.max(tl.x), tl.y].into(),
            chunk_row_end: range.end.min(br.x),
            empty,
        }
    }

    pub fn iter_area(
        &self,
        pos: Vec2isize,
        size: Vec2usize,
    ) -> impl Iterator<Item = (Vec2isize, &T)> {
        let size = size.convert(|v| v as isize);

        let tl = pos;
        let br = pos + size;

        let (chunk_tl, tl) = global_to_chunk_pos(tl, CHUNK_SIZE);
        let (chunk_br, br) = global_to_chunk_pos(br, CHUNK_SIZE);

        let size = if size.x == 0 && size.y == 0 {
            size
        } else {
            (chunk_br - chunk_tl) + 1
        };

        self.iter_chunks(chunk_tl, size.convert(|v| v as usize))
            .flat_map(move |(chunk_pos, chunk)| {
                let range_x_start = if chunk_pos.x == chunk_tl.x { tl.x } else { 0 };

                let range_y_start = if chunk_pos.y == chunk_tl.y { tl.y } else { 0 };

                let range_x_end = if chunk_pos.x == chunk_br.x { br.x } else { 16 };

                let range_y_end = if chunk_pos.y == chunk_br.y { br.y } else { 16 };

                let range_x = range_x_start..range_x_end;

                range_x
                    .flat_map(move |x| (range_y_start..range_y_end).map(move |y| (x, y)))
                    .map(move |(x, y)| {
                        let pos = Vec2usize::new(x, y);
                        let global_pos = chunk_to_global_pos(chunk_pos, pos, CHUNK_SIZE);
                        let item = &chunk[pos.x][pos.y];
                        (global_pos, item)
                    })
            })
    }

    pub fn iter_area_with_lookaround(
        &self,
        pos: Vec2isize,
        size: Vec2usize,
    ) -> impl Iterator<Item = (Vec2isize, ChunksLookaround<CHUNK_SIZE, T>, &T)> {
        let size = size.convert(|v| v as isize);

        let tl = pos;
        let br = pos + size;

        let (chunk_tl, tl) = global_to_chunk_pos(tl, CHUNK_SIZE);
        let (chunk_br, br) = global_to_chunk_pos(br, CHUNK_SIZE);

        let size = if size.x == 0 && size.y == 0 {
            size
        } else {
            (chunk_br - chunk_tl) + 1
        };

        self.iter_chunks(chunk_tl, size.convert(|v| v as usize))
            .flat_map(move |(chunk_pos, chunk)| {
                let range_x_start = if chunk_pos.x == chunk_tl.x { tl.x } else { 0 };

                let range_y_start = if chunk_pos.y == chunk_tl.y { tl.y } else { 0 };

                let range_x_end = if chunk_pos.x == chunk_br.x { br.x } else { 16 };

                let range_y_end = if chunk_pos.y == chunk_br.y { br.y } else { 16 };

                let range_x = range_x_start..range_x_end;

                range_x
                    .flat_map(move |x| (range_y_start..range_y_end).map(move |y| (x, y)))
                    .map(move |(x, y)| {
                        let pos = Vec2usize::new(x, y);
                        let global_pos = chunk_to_global_pos(chunk_pos, pos, CHUNK_SIZE);
                        let item = &chunk[pos.x][pos.y];
                        let lookaround = ChunksLookaround::new(self, chunk, chunk_pos, pos);
                        (global_pos, lookaround, item)
                    })
            })
    }
}

pub struct ChunksIterator<'a, const CHUNK_SIZE: usize, T: Default> {
    chunks: &'a Chunks2D<CHUNK_SIZE, T>,

    chunk_bounds_tl: Vec2isize,
    chunk_bounds_br_excl: Vec2isize,

    chunk_row_end: isize,

    chunk_pos: Vec2isize,

    empty: bool,
}

impl<'a, const CHUNK_SIZE: usize, T: Default> Iterator for ChunksIterator<'a, CHUNK_SIZE, T> {
    type Item = (Vec2isize, &'a Chunk<CHUNK_SIZE, T>);

    fn next(&mut self) -> Option<Self::Item> {
        if self.empty {
            return None;
        }

        if self.empty || self.chunk_pos.y >= self.chunk_bounds_br_excl.y {
            return None;
        }

        let chunk = loop {
            if self.chunk_pos.x >= self.chunk_bounds_br_excl.x {
                loop {
                    self.chunk_pos.y += 1;
                    if self.chunk_pos.y >= self.chunk_bounds_br_excl.y {
                        break;
                    }
                    let range = self.chunks.get_chunk_row_range(self.chunk_pos.y);
                    if range.is_empty() {
                        continue;
                    }
                    self.chunk_pos.x = self.chunk_bounds_tl.x.max(range.start);
                    self.chunk_row_end = self.chunk_bounds_br_excl.x.min(range.end);
                    break;
                }
                if self.chunk_pos.y >= self.chunk_bounds_br_excl.y {
                    self.empty = true;
                    return None;
                }
            }

            match self.chunks.get_chunk(self.chunk_pos) {
                Some(c) => break c,
                None => {
                    self.chunk_pos.x += 1;
                    continue;
                }
            }
        };

        let res = Some((self.chunk_pos, chunk));

        self.chunk_pos.x += 1;
        res
    }
}

pub struct ConstRingBuffer<const SIZE: usize, T> {
    array: [MaybeUninit<T>; SIZE],

    /// Points to position after last valid item
    pos: usize,
    len: usize, //    /---len---\
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

    pub fn is_empty(&self) -> bool {
        self.len == 0
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
            Some(unsafe { assume_init_slice_ref(&self.array[start..start + self.len]) })
        } else if self.pos == 0 {
            Some(unsafe { assume_init_slice_ref(&self.array[SIZE - self.len..]) })
        } else {
            None
        }
    }

    pub fn as_mut_slice(&mut self) -> Option<&mut [T]> {
        if self.pos >= self.len {
            let start = self.pos - self.len;
            Some(unsafe { assume_init_slice_mut(&mut self.array[start..start + self.len]) })
        } else if self.pos == 0 {
            Some(unsafe { assume_init_slice_mut(&mut self.array[SIZE - self.len..]) })
        } else {
            None
        }
    }

    pub fn push_back(&mut self, value: T) {
        if SIZE == 0 {
            return;
        }
        if self.len >= SIZE {
            unsafe { self.array[self.pos].assume_init_drop() }
        }
        self.array[self.pos].write(value);
        self.len = (self.len + 1).min(SIZE);
        self.pos = (self.pos + 1) % SIZE;
    }
    pub fn push_front(&mut self, value: T) {
        if SIZE == 0 {
            return;
        }
        let pos = (self.pos + SIZE - self.len - 1) % SIZE;
        if self.len >= SIZE {
            unsafe { self.array[pos].assume_init_drop() }
            self.pos = (self.pos - 1 + SIZE) % SIZE;
        }
        self.array[pos].write(value);
        self.len = (self.len + 1).min(SIZE);
    }

    pub fn pop_back(&mut self) -> Option<T> {
        if SIZE == 0 {
            return None;
        }
        if self.len == 0 {
            None
        } else {
            let pos = (self.pos - 1 + SIZE) % SIZE;
            self.len -= 1;
            Some(unsafe { self.array[pos].assume_init_read() })
        }
    }

    pub fn pop_front(&mut self) -> Option<T> {
        if SIZE == 0 {
            return None;
        }
        if self.len == 0 {
            None
        } else {
            let pos = (self.pos + SIZE - self.len) % SIZE;
            self.len -= 1;
            Some(unsafe { self.array[pos].assume_init_read() })
        }
    }

    pub fn iter(&self) -> ConstRingBufferRefIterator<'_, T> {
        let start = if SIZE == 0 {
            self.pos
        } else {
            (self.pos + SIZE - self.len) % SIZE
        };
        ConstRingBufferRefIterator {
            buf: unsafe { assume_init_slice_ref(self.array.as_ref()) },
            start,
            len: self.len,
            pos: 0,
        }
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

unsafe fn assume_init_slice_ref<T>(s: &[MaybeUninit<T>]) -> &[T] {
    unsafe { &*(s as *const [MaybeUninit<T>] as *const [T]) }
}

unsafe fn assume_init_slice_mut<T>(s: &mut [MaybeUninit<T>]) -> &mut [T] {
    unsafe { &mut *(s as *mut [MaybeUninit<T>] as *mut [T]) }
}

pub fn global_to_chunk_pos(
    pos: impl Into<Vec2isize>,
    chunk_size: impl Into<Vec2usize>,
) -> (Vec2isize, Vec2usize) {
    let pos = pos.into();
    let chunk_size = chunk_size.into();
    let chunk_x = div_floor_isize(pos.x, chunk_size.x as isize);
    let chunk_y = div_floor_isize(pos.y, chunk_size.y as isize);
    let pos_x = (pos.x - (chunk_x * chunk_size.x as isize)) as usize;
    let pos_y = (pos.y - (chunk_y * chunk_size.y as isize)) as usize;
    ([chunk_x, chunk_y].into(), [pos_x, pos_y].into())
}

pub fn chunk_to_global_pos(
    chunk: impl Into<Vec2isize>,
    item: impl Into<Vec2usize>,
    chunk_size: impl Into<Vec2usize>,
) -> Vec2isize {
    chunk.into() * chunk_size.into().convert(|v| v as isize) + item.into().convert(|v| v as isize)
}
