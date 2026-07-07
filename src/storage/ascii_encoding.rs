use std::io::{self, Read, Write};

pub struct ByteAsciiEncoder<'a> {
    str: &'a mut String,
    buffer: u8,
    buffer_bits: u32,
}

impl<'a> ByteAsciiEncoder<'a> {
    pub fn new(str: &'a mut String) -> Self {
        Self {
            str,
            buffer: 0,
            buffer_bits: 0,
        }
    }

    pub fn finish(self) {
        if self.buffer_bits > 0 {
            self.str
                .push(char::from_u32(self.buffer as u32).expect("valid ascii char"));
        }
    }

    pub fn write_byte(&mut self, byte: u8) {
        // buffer: 00000AAA
        // buffer_bits: 3
        // data: BBBBBBBB

        // BBBBBBBB
        //     \==/

        let take_bits = 7 - self.buffer_bits;

        // 00001111
        //     \==/
        let byte_bits_mask = ((1 << take_bits) - 1) as u8;

        // 0BBBBAAA
        let data_byte = ((byte & byte_bits_mask) << self.buffer_bits) | self.buffer;

        self.buffer = byte >> take_bits;
        self.buffer_bits = 8 - take_bits;

        self.str
            .push(char::from_u32(data_byte as u32).expect("valid ascii char"));

        if self.buffer_bits >= 7 {
            self.str
                .push(char::from_u32((self.buffer & 0x7f) as u32).expect("valid ascii char"));

            self.buffer >>= 7;
            self.buffer_bits -= 7;
        }
    }
}

impl<'a> Write for ByteAsciiEncoder<'a> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        for &b in buf {
            self.write_byte(b);
        }

        Ok(buf.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

pub struct ByteAsciiDecoder<'a> {
    remaining: &'a [u8],
    buffer: u8,
    buffer_bits: u32,
}

impl<'a> ByteAsciiDecoder<'a> {
    pub fn new(encoded: &'a [u8]) -> Self {
        Self {
            remaining: encoded,
            buffer: 0,
            buffer_bits: 0,
        }
    }

    pub fn read_byte(&mut self) -> Option<u8> {
        // buffer: 00000000
        // buffer_bits: 0

        // data_byte: -AAAAAAA
        let mut data_byte = *self.remaining.first()?;

        self.remaining = &self.remaining[1..];

        if self.buffer_bits == 0 {
            // read second byte

            self.buffer = data_byte & 0x7f;
            self.buffer_bits = 7;

            data_byte = *self.remaining.first()?;
            self.remaining = &self.remaining[1..];
        }

        // buffer: 0AAAAAAA
        // buffer_bits: 7

        // data_byte: -BBBBBBA

        // -BBBBBBA
        //        ^
        let take_bits = 8 - self.buffer_bits;

        // 00000001
        let byte_bits_mask = ((1 << take_bits) - 1) as u8;

        // AAAAAAAA
        let byte = ((data_byte & byte_bits_mask) << self.buffer_bits) | self.buffer;

        // 00BBBBBB
        self.buffer = (data_byte & 0x7f) >> take_bits;
        self.buffer_bits = 7 - take_bits;

        Some(byte)
    }
}

impl<'a> Read for ByteAsciiDecoder<'a> {

    #[allow(clippy::needless_range_loop)]
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        for i in 0..buf.len() {
            match self.read_byte() {
                Some(b) => buf[i] = b,
                None => return Ok(i),
            }
        }
        Ok(buf.len())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_ascii_encoding() {
        let data = [
            0xbf, 0x47, 0x8d, 0x15, 0xfb, 0xf4, 0x14, 0x1f, 0x15, 0xbc, 0xad, 0x3f, 0x30, 0x6c,
        ];

        let mut encoded = String::new();

        let mut encoder = ByteAsciiEncoder::new(&mut encoded);

        encoder.write_all(&data).unwrap();
        encoder.finish();

        let mut decoder = ByteAsciiDecoder::new(encoded.as_bytes());
        let mut vec = vec![];

        decoder.read_to_end(&mut vec).unwrap();

        assert_eq!(&data[..], &vec[..]);
    }
}
