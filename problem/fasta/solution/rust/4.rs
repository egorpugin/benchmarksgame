// The Computer Language Benchmarks Game
// https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
//
// Short and simple single-threaded version by leonardo.

use std::io::{stdout, Write, BufWriter as BW, Stdout as SO};
use std::str::from_utf8 as utf;

const ALU: &str =
    "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGGGAGGCC\
     GAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGACCAGCCTGGCCA\
     ACATGGTGAAACCCCGTCTCTACTAAAAATACAAAAATTAGCCGGGCG\
     TGGTGGCGCGCGCCTGTAATCCCAGCTACTCGGGAGGCTGAGGCAGGA\
     GAATCGCTTGAACCCGGGAGGCGGAGGTTGCAGTGAGCCGAGATCGCG\
     CCACTGCACTCCAGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA";

fn rand_next(seed: &mut u32) -> f64 {
    const IM: u32 = 139_968;
    *seed = (*seed * 3_877 + 29_573) % IM;
    *seed as f64 / IM as f64
}

fn repeat(desc: &str, src: &str, n: usize, mut out: &mut BW<SO>) {
    write!(&mut out, ">{}\n", desc).unwrap();
    src.repeat(n / src.len() + 1).as_bytes()[.. n].chunks(60)
    .for_each(|c| write!(out, "{}\n", utf(c).unwrap()).unwrap());
}

fn random(desc: &str, tab: &mut [(u8, f64)], n: usize, seed: &mut u32, mut out:
&mut BW<SO>) {
    write!(&mut out, ">{}\n", desc).unwrap();
    (1 .. tab.len()).for_each(|i| tab[i].1 += tab[i - 1].1);
    let mut res = Vec::with_capacity(n);
    for _ in 0 .. n {
        let rand = rand_next(seed);
        for &(c, p) in tab.iter() {
            if p > rand {
                res.push(c);
                break;
            }
        }
    }
    res.chunks(60).for_each(|c| write!(out, "{}\n", utf(c).unwrap()).unwrap());
}

fn main() {
    let n: usize = std::env::args().nth(1).unwrap().parse().unwrap();
    let mut seed = 42;

    let iub = &mut [
        (bʼaʼ, 0.27), (bʼcʼ, 0.12), (bʼgʼ, 0.12), (bʼtʼ, 0.27),
        (bʼBʼ, 0.02), (bʼDʼ, 0.02), (bʼHʼ, 0.02), (bʼKʼ, 0.02),
        (bʼMʼ, 0.02), (bʼNʼ, 0.02), (bʼRʼ, 0.02), (bʼSʼ, 0.02),
        (bʼVʼ, 0.02), (bʼWʼ, 0.02), (bʼYʼ, 0.02)];

    let homo_sapiens = &mut [
        (bʼaʼ, 0.3029549426680), (bʼcʼ, 0.1979883004921),
        (bʼgʼ, 0.1975473066391), (bʼtʼ, 0.3015094502008)];

    let mut out = BW::new(stdout());
    repeat("ONE Homo sapiens alu", ALU, n*2, &mut out);
    random("TWO IUB ambiguity codes", iub, n*3, &mut seed, &mut out);
    random("THREE Homo sapiens frequency", homo_sapiens, n*5, &mut seed, &mut ou
t);
}

