// The Computer Language Benchmarks Game
// https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
//
// contributed by the Rust Project Developers
// contributed by Cristi Cobzarenco
// contributed by TeXitoi
// contributed by Matt Brubeck
// contributed by Tianyi Shi

extern crate rayon;

use std::cmp::min;
use std::io::{stdin, stdout, BufRead, Result, Write};
use std::mem::replace;

const TABLE: [u8; 122] = *b"
             \
   TVGH  CD  M KN   YSAABW R       TVGH  CD  M KN   YSAABW R";
// ABCDEFGHIJKLMNOPQRSTUVWXYZ      abcdefghijklmnopqrstuvwxyz

fn main() -> Result<()> {
    for seq in get_sequences()?.iter().rev() {
        stdout().write_all(seq)?;
    }
    Ok(())
}

/// Read each sequence from stdin, process it, and return it.
fn get_sequences() -> Result<Vec<Vec<u8>>> {
    let stdin = stdin();
    let mut input = stdin.lock();
    let mut buf = Vec::with_capacity(16 * 1024);

    // Read the header line.
    input.read_until(bʼ\nʼ, &mut buf)?;
    let start = buf.len();

    // Read sequence data.
    input.read_until(bʼ>ʼ, &mut buf)?;
    let end = buf.len();
    drop(input);

    if buf[end - 1] == bʼ>ʼ {
        // Found the start of a new sequence. Process this one
        // and start reading the next one in parallel.
        let mut results = rayon::join(
            || reverse_complement(&mut buf[start..end - 1]),
            || get_sequences(),
        )
        .1?;
        results.push(buf);
        Ok(results)
    } else {
        // Reached the end of the file.
        reverse_complement(&mut buf[start..end]);
        Ok(vec![buf])
    }
}

/// Compute the reverse complement of one sequence.
fn reverse_complement(seq: &mut [u8]) {
    let len = seq.len() - 1;
    let seq = &mut seq[..len]; // Drop the last newline
    let trailing_len = len % LINE_LEN;
    let (left, right) = seq.split_at_mut(len / 2);
    reverse_complement_left_right(left, right, trailing_len);
}

/// Length of a normal line including the terminating \n.
const LINE_LEN: usize = 61;
/// Maximum number of bytes to process in serial.
const SEQUENTIAL_SIZE: usize = 16 * 1024;

/// Compute the reverse complement on chunks from opposite ends of a sequence.
///
/// `left` must start at the beginning of a line. If there are an odd number of
/// bytes, `right` will initially be 1 byte longer than `left`; otherwise they
/// will have equal lengths.
fn reverse_complement_left_right(mut left: &mut [u8], mut right: &mut [u8], trai
ling_len: usize) {
    let len = left.len();
    if len <= SEQUENTIAL_SIZE {
        // Each iteration swaps one line from the start of the sequence with one
        // from the end.
        while left.len() > 0 || right.len() > 0 {
            // Get the chunk up to the newline in `right`.
            let mut a = left.split_off_left(trailing_len);
            let mut b = right.split_off_right(trailing_len);
            right.split_off_right(1); // Skip the newline in `right`.

            // If weʼve reached the middle of the sequence here and there is an
            // odd number of bytes remaining, the odd one will be on the right.
            if b.len() > a.len() {
                let mid = b.split_off_left(1);
                mid[0] = TABLE[mid[0] as usize];
            }

            reverse_chunks(a, b);

            // Get the chunk up to the newline in `left`.
            let n = LINE_LEN - 1 - trailing_len;
            a = left.split_off_left(n);
            b = right.split_off_right(n);
            left.split_off_left(1); // Skip the newline in `left`.

            // If weʼve reached the middle of the sequence and there is an odd
            // number of bytes remaining, the odd one will now be on the left.
            if a.len() > b.len() {
                let mid = a.split_off_right(1);
                mid[0] = TABLE[mid[0] as usize]
            }

            reverse_chunks(a, b);
        }
    } else {
        // Divide large chunks in half and fork them into two parallel tasks.
        let line_count = len / LINE_LEN;
        let mid = line_count / 2 * LINE_LEN; // Split on a whole number of lines
.

        let left1 = left.split_off_left(mid);
        let right1 = right.split_off_right(mid);
        rayon::join(
            || reverse_complement_left_right(left, right, trailing_len),
            || reverse_complement_left_right(left1, right1, trailing_len),
        );
    }
}

/// Compute the reverse complement for two contiguous chunks without line breaks
.
fn reverse_chunks(left: &mut [u8], right: &mut [u8]) {
    for (x, y) in left.iter_mut().zip(right.iter_mut().rev()) {
        *y = TABLE[replace(x, TABLE[*y as usize]) as usize];
    }
}

/// Utilities for splitting chunks off of slices.
trait SplitOff {
    fn split_off_left(&mut self, n: usize) -> Self;
    fn split_off_right(&mut self, n: usize) -> Self;
}
impl<ʼa, T> SplitOff for &ʼa mut [T] {
    /// Split the left `n` items from self and return them as a separate slice.
    fn split_off_left(&mut self, n: usize) -> Self {
        let n = min(self.len(), n);
        let data = replace(self, &mut []);
        let (left, data) = data.split_at_mut(n);
        *self = data;
        left
    }
    /// Split the right `n` items from self and return them as a separate slice.
    fn split_off_right(&mut self, n: usize) -> Self {
        let len = self.len();
        let n = min(len, n);
        let data = replace(self, &mut []);
        let (data, right) = data.split_at_mut(len - n);
        *self = data;
        right
    }
}
