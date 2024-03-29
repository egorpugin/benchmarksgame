// The Computer Language Benchmarks Game
// https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
//
// Contributed by Kevin Miller
// Converted from C to Rust by Tung Duong

extern crate rayon;

use std::ops::{Add, Sub, Mul};
use std::io::Write;
use rayon::prelude::*;


#[allow(non_camel_case_types)]
#[derive(Clone, Copy)]
struct f64x2(f64,f64);

const ZEROS: [f64x2;4] = [f64x2(0.0,0.0); 4];

impl Add for f64x2 {
    type Output = Self;
    fn add(self, rhs: Self) -> Self {
        f64x2(self.0 + rhs.0, self.1 + rhs.1)
    }
}

impl Sub for f64x2 {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self {
        f64x2(self.0 - rhs.0, self.1 - rhs.1)
    }
}

impl Mul for f64x2 {
    type Output = Self;
    fn mul(self, rhs: Self) -> Self {
        f64x2(self.0 * rhs.0, self.1 * rhs.1)
    }
}

#[inline(always)]
fn vec_nle(v : &[f64x2;4], f : f64) -> bool {
    return if v[0].0 <= f ||
        v[0].1 <= f ||
        v[1].0 <= f ||
        v[1].1 <= f ||
        v[2].0 <= f ||
        v[2].1 <= f ||
        v[3].0 <= f ||
        v[3].1 <= f {false} else {true};
}

#[inline(always)]
fn clr_pixels_nle(v : &[f64x2;4], f: f64, pix8 : &mut u8){
    if !(v[0].0 <= f) {*pix8 &= 0x7f;}
    if !(v[0].1 <= f) {*pix8 &= 0xbf;}
    if !(v[1].0 <= f) {*pix8 &= 0xdf;}
    if !(v[1].1 <= f) {*pix8 &= 0xef;}
    if !(v[2].0 <= f) {*pix8 &= 0xf7;}
    if !(v[2].1 <= f) {*pix8 &= 0xfb;}
    if !(v[3].0 <= f) {*pix8 &= 0xfd;}
    if !(v[3].1 <= f) {*pix8 &= 0xfe;}
}

#[inline(always)]
fn calc_sum(r: &mut [f64x2;4], i : &mut[f64x2;4], sum : &mut[f64x2;4], init_r: &
[f64x2;4], init_i : f64x2){
        for j in 0..4{
                let r2 = r[j] * r[j];
                let i2 = i[j] * i[j];
                let ri = r[j] * i[j];
                sum[j] = r2 + i2;
                r[j] = r2 - i2 + init_r[j];
                i[j] = ri + ri + init_i;
        }
}

#[inline(always)]
fn mand8(init_r: &[f64x2;4], init_i : f64x2) -> u8 {
        let mut r = ZEROS;
        let mut i = ZEROS;
        let mut sum = ZEROS;

        for j in 0..4{
                r[j] = init_r[j];
                i[j] = init_i;
        }

    let mut pix8 : u8 = 0xff;

    for _ in 0..6 {
        for _ in 0..8{
            calc_sum(&mut r, &mut i, &mut sum, &init_r, init_i);
        }

        if vec_nle(&sum, 4.0) {
            pix8 = 0x00;
            break;
        }
    }
    if pix8 != 0 {
        calc_sum(&mut r, &mut i, &mut sum, &init_r, init_i);
        calc_sum(&mut r, &mut i, &mut sum, &init_r, init_i);
        clr_pixels_nle(&sum, 4.0, &mut pix8);
    }

    pix8
}

fn mand64(init_r: &[f64x2;32], init_i : f64x2, out : &mut [u8]) {
    let mut tmp_init_r = ZEROS;

    for i in 0..8 {
        tmp_init_r.copy_from_slice(&init_r[4*i..4*i+4]);
        out[i] = mand8(&tmp_init_r, init_i);
    }
}

fn main(){
        let mut width = std::env::args_os().nth(1)
        .and_then(|s| s.into_string().ok())
        .and_then(|n| n.parse().ok())
        .unwrap_or(16000);
    width = (width+7) & !7;

    println!("P4\n{} {}", width, width);
    let mut r0 = vec![f64x2(0.0,0.0); width/2];
    let mut i0 = vec![0.0; width];

    for i in 0..width/2 {
        let x1 = (2*i) as f64;
        let x2 = (2*i+1) as f64;
        let k = 2.0 / (width as f64);
        r0[i] = f64x2(k*x1, k*x2) - f64x2(1.5,1.5);
        i0[2*i]    = k*x1 - 1.0;
        i0[2*i+1]  = k*x2 - 1.0;
    }

    let rows : Vec<_>  = if width%64==0 {
        // process 64 pixels (8 bytes) at a time
        (0..width).into_par_iter().map(|y|{
            let mut tmp_r0 = [f64x2(0.0,0.0);32];
            let mut row = vec![0 as u8; (width/8) as usize];
            let init_i = f64x2(i0[y], i0[y]);

            for x in 0..width/64{
                tmp_r0.copy_from_slice(&r0[32*x..32*x+32]);
                mand64(&tmp_r0, init_i, &mut row[8*x..8*x + 8]);
            }
            row
        }).collect()
    }else {
                // process 8 pixels (1 byte) at a time
        (0..width).into_par_iter().map(|y|{
                let mut tmp_r0 = ZEROS;
                let mut row = vec![0 as u8; (width/8) as usize];
                let init_i = f64x2(i0[y], i0[y]);

                for x in 0..width/8{
                        tmp_r0.copy_from_slice(&r0[4*x..4*x+4]);
                        row[x] = mand8(&tmp_r0, init_i);
                }
                row
        }).collect()
    };

    let stdout_unlocked = std::io::stdout();
    let mut stdout = stdout_unlocked.lock();
    for row in rows{
        stdout.write_all(&row).unwrap();
    }
    stdout.flush().unwrap();
}

