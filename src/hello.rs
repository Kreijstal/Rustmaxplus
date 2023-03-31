use num_bigint::BigInt;
use std::ops::{Add, Mul};

enum IntOrBigInt {
    Int(i32),
    BigInt(BigInt),
}

impl Add for IntOrBigInt {
    type Output = Self;

    fn add(self, other: Self) -> Self::Output {
        match (self, other) {
            (IntOrBigInt::Int(a), IntOrBigInt::Int(b)) => IntOrBigInt::Int(a + b),
            (IntOrBigInt::Int(a), IntOrBigInt::BigInt(b)) => IntOrBigInt::BigInt(BigInt::from(a) + b),
            (IntOrBigInt::BigInt(a), IntOrBigInt::Int(b)) => IntOrBigInt::BigInt(a + BigInt::from(b)),
            (IntOrBigInt::BigInt(a), IntOrBigInt::BigInt(b)) => IntOrBigInt::BigInt(a + b),
        }
    }
}

impl Mul for IntOrBigInt {
    type Output = Self;

    fn mul(self, other: Self) -> Self::Output {
        match (self, other) {
            (IntOrBigInt::Int(a), IntOrBigInt::Int(b)) => IntOrBigInt::Int(a * b),
            (IntOrBigInt::Int(a), IntOrBigInt::BigInt(b)) => IntOrBigInt::BigInt(BigInt::from(a) * b),
            (IntOrBigInt::BigInt(a), IntOrBigInt::Int(b)) => IntOrBigInt::BigInt(a * BigInt::from(b)),
            (IntOrBigInt::BigInt(a), IntOrBigInt::BigInt(b)) => IntOrBigInt::BigInt(a * b),
        }
    }
}

fn main() {
    let a = IntOrBigInt::Int(42);
    let b = IntOrBigInt::BigInt(BigInt::from(1234567890123456789u64));

    let sum = a + b;
    let product = a * b;

    match sum {
        IntOrBigInt::Int(value) => println!("Sum (i32): {}", value),
        IntOrBigInt::BigInt(value) => println!("Sum (BigInt): {}", value),
    }

    match product {
        IntOrBigInt::Int(value) => println!("Product (i32): {}", value),
        IntOrBigInt::BigInt(value) => println!("Product (BigInt): {}", value),
    }
}
