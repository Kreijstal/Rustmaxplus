/*
[dependencies]

macro_rules_attribute = "*"
*/
//ndarray = "*"
#![allow(non_snake_case)]
#![allow(non_camel_case_types)]
#![allow(unused_macros)]
#![allow(unused_imports)]
#![allow(unused_variables)]
#![allow(dead_code)]
//#![no_std]

#[macro_use]
extern crate macro_rules_attribute;

/* The derive_alias! macro defines a set of traits and their corresponding implementations. These traits can then be used in struct and enum definitions to automatically derive certain traits for those types. In the code above, the derive_alias! macro defines the following aliases:

    #[derive(Eq!)] is equivalent to #[derive(Eq, PartialEq)]
    #[derive(Ord!)] is equivalent to #[derive(Ord, PartialOrd, Eq!)]]
    #[derive(Copy!)] is equivalent to #[derive(Copy, Clone)]
    #[derive(StdDerives!)] is equivalent to #[derive(Debug, Copy!, Default, Ord!, Hash)]

These aliases are then used in the DashNumber enum definition, which specifies that the DashNumber enum should automatically derive the Debug, Copy, Ord, and Eq traits. This means that the DashNumber enum will have the necessary implementations of these traits provided automatically.
  */
derive_alias! {
    #[derive(Eq!)] = #[derive(Eq, PartialEq)];
    #[derive(Ord!)] = #[derive(Ord, PartialOrd, Eq!)];
    #[derive(Copy!)] = #[derive(Copy, Clone)];
    #[derive(StdDerives!)] = #[derive(Debug, Copy!, Default, Ord!, Hash)];
}

macro_rules! my_macro {
    ($num:expr) => {
        let x = $num;
        println!("{}", x)
    };
}
use core::*;
use std::{ops::Neg, process::Output};
//use std::{fs::canonicalize, default::default};
//use ndarray;
#[derive(Debug,Ord!,Copy!)]
enum DashNumber<N> {
    NegInfinity,
    Number(N),
    Infinity,
}

//use crate::num;
// Implement the One trait for the types that we want to
// be able to multiply by 1.
/*impl<N: ops::Mul<Output = N> + Copy> ops::Mul for DashNumber<N> {
    type Output = Self;
    fn mul(self, other: Self) -> Self::Output {
        use DashNumber::*;
        match (self, other) {
            (NegInfinity, _) => NegInfinity,
            (_, NegInfinity) => NegInfinity,
            (Infinity, _) => Infinity,
            (_, Infinity) => Infinity,
            (Number(a), Number(b)) => Number(a + b),
        }
    }
}
impl<N> ::num::One for DashNumber<N> where
N: ::num::One{
    fn one() -> Self {
        DashNumber::Number(N::one())
    }
}*/
impl<T: ::num::Integer + Clone> DashNumber<::num::rational::Ratio<T>> {
    fn round(self) -> DashNumber<T> {
        use DashNumber::*;
        match self {
            DashNumber::Number(a) => DashNumber::Number(a.to_integer()),
            Infinity => Infinity,
            NegInfinity => NegInfinity,
        }
    }
}
impl<T: Default> Default for DashNumber<T> {
    fn default() -> Self {
        DashNumber::Number(T::default())
    }
}
impl<T: fmt::Display> fmt::Display for DashNumber<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use DashNumber::*;
        match self {
            NegInfinity => write!(f, "-∞"),
            Infinity => write!(f, "∞"),
            Number(a) => write!(f, "{}", a),
        }
    }
}
impl<T> DashNumber<T> {}
impl<T> From<T> for DashNumber<T> {
    fn from(item: T) -> Self {
        DashNumber::Number(item)
    }
}
macro_rules! get_number_opt {
    ($e:expr) => {
        match $e {
            DashNumber::Number(a) => Some(a),
            _ => None,
        }
    };
}
macro_rules! get_number {
    ($e:expr) => {
        get_number_opt!($e).expect("We expected the number to not be an infinity")
    };
}
fn lol<T: ::num::Integer>(a: T) -> usize
where
    usize: TryFrom<T>,
{
    usize::try_from(a)
        .ok()
        .expect("argument should be positive and not that big")
}
/*impl<T> TryFrom<DashNumber<T>> for usize where usize:TryFrom<T> {
    type Error=&'static str;
    fn try_from(item: DashNumber<T>) -> Result<Self,Self::Error> {
        get_number_opt!(item).ok_or("Infinities found").and_then(|a| usize::try_from(a))
    }
}*/

#[derive(Debug, PartialEq)]
enum TryFromDashNumberError {
    NegInfinity,
    Number(std::num::TryFromIntError),
    Infinity,
}

impl fmt::Display for TryFromDashNumberError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TryFromDashNumberError::NegInfinity => {
                write!(f, "Negative infinity cannot be converted to a usize")
            }
            TryFromDashNumberError::Number(e) => {
                write!(f, "Error converting number to usize: {}", e)
            }
            TryFromDashNumberError::Infinity => {
                write!(f, "Infinity cannot be converted to a usize")
            }
        }
    }
}

impl std::error::Error for TryFromDashNumberError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            TryFromDashNumberError::Number(e) => Some(e),
            _ => None,
        }
    }
}

impl<T> TryFrom<DashNumber<T>> for usize
where
    usize: TryFrom<T, Error = std::num::TryFromIntError>,
{
    type Error = TryFromDashNumberError;

    fn try_from(value: DashNumber<T>) -> Result<Self, Self::Error> {
        match value {
            DashNumber::NegInfinity => Err(TryFromDashNumberError::NegInfinity),
            DashNumber::Number(n) => {
                usize::try_from(n).map_err(|e| TryFromDashNumberError::Number(e))
            }
            DashNumber::Infinity => Err(TryFromDashNumberError::Infinity),
        }
    }
}

impl<T> PartialEq<T> for DashNumber<T>
where
    T: PartialEq,
{
    fn eq(&self, other: &T) -> bool {
        //Some(self.cmp(other))
        use DashNumber::*;
        match self {
            NegInfinity => false,
            Number(n) => n.eq(other),
            Infinity => false,
        }
    }
}
impl<T> PartialOrd<T> for DashNumber<T>
where
    T: PartialOrd,
{
    fn partial_cmp(&self, other: &T) -> Option<cmp::Ordering> {
        //Some(self.cmp(other))
        use DashNumber::*;
        match self {
            NegInfinity => Some(cmp::Ordering::Less),
            Number(n) => n.partial_cmp(other),
            Infinity => Some(cmp::Ordering::Greater),
        }
    }
}
impl<N: ::num::Zero + Copy> ::num::Zero for DashNumber<N> {
    fn zero() -> Self {
        DashNumber::Number(N::zero())
    }
    fn is_zero(&self) -> bool {
        match self {
            DashNumber::Number(a) => a.is_zero(),
            _ => false,
        }
    }
}
impl<N: ::num::Zero + Copy + ::num::Integer + PartialOrd> ops::Div for DashNumber<N>
//where Self: Output<N>
{
    type Output = DashNumber<::num::rational::Ratio<N>>;

    fn div(self, rhs: DashNumber<N>) -> Self::Output {
        use DashNumber::*;
        //use ::num::Zero;
        use core::panic;
        match (self, rhs) {
            (_, Number(a)) if a.is_zero() => panic!("no division by zero"),
            (Infinity | NegInfinity, Infinity | NegInfinity) => {
                panic!("Infinity/Infinity division")
            }
            (_, Infinity | NegInfinity) => {
                <DashNumber<::num::rational::Ratio<N>> as ::num::Zero>::zero()
            }
            (Infinity, _) => {
                if rhs > <DashNumber<N> as ::num::Zero>::zero() {
                    Infinity
                } else {
                    NegInfinity
                }
            }
            (NegInfinity, _) => {
                if rhs > <DashNumber<N> as ::num::Zero>::zero() {
                    NegInfinity
                } else {
                    Infinity
                }
            }
            (Number(c), Number(d)) => Number(::num::rational::Ratio::new(c.clone(), d.clone())),
        }
        //  todo!();
        // implementation goes here
    }
}

macro_rules! impl_mul {
    ($a:ident, $b:ident) => {
        if $a > Self::zero() && $b > Self::zero() || $a < Self::zero() && $b < Self::zero() {
            Infinity
        } else if $a > Self::zero() && $b < Self::zero() || $a < Self::zero() && $b > Self::zero() {
            NegInfinity
        } else {
            //Should I panic instead?
            Self::zero()
        }
    };
}

impl<N: ops::Mul<Output = N> + Copy + ::num::Zero + PartialOrd> ops::Mul for DashNumber<N> {
    type Output = Self;
    fn mul(self, other: Self) -> Self::Output {
        use ::num::Zero;
        use DashNumber::*;
        match (self, other) {
            (NegInfinity | Infinity, _) => impl_mul!(self, other),
            (_, NegInfinity | Infinity) => impl_mul!(self, other),
            (Number(a), Number(b)) => Number(a * b),
        }
    }
}
impl<N: ops::Mul<Output = N> + Copy + ::num::Zero + PartialOrd> ops::Mul<N> for DashNumber<N> {
    type Output = Self;
    fn mul(self, dother: N) -> Self::Output {
        use ::num::Zero;
        use DashNumber::*;
        let other = DashNumber::from(dother);
        match (self, dother) {
            (NegInfinity | Infinity, _) => impl_mul!(self, other),
            (Number(a), b) => Number(a * b),
        }
    }
}
impl<N: ops::Mul<Output = N> + Copy + ::num::Zero + PartialOrd + ::num::One> ::num::One
    for DashNumber<N>
{
    fn one() -> Self {
        DashNumber::Number(N::one())
    }
    fn is_one(&self) -> bool {
        match self {
            DashNumber::Number(a) => a.is_one(),
            _ => false,
        }
    }
}
/*impl<N: ops::Mul<Output = N> + Copy+ ::num::Zero + PartialOrd> ops::Mul for DashNumber<N> {
    type Output = Self;
    fn mul(self, other: Self) -> Self::Output {
        use DashNumber::*;
        use ::num::Zero;
        match (self, other) {
            (NegInfinity, _) => if other>Self::zero() {NegInfinity}else if other<Self::zero() {Infinity} else{ Self::zero()},
            (_, NegInfinity) => if self>Self::zero() {NegInfinity}else if self<Self::zero() {Infinity} else{ Self::zero()},
            (Infinity, _) => if other>Self::zero() {Infinity}else if other<Self::zero() {NegInfinity} else{ Self::zero()},
            (_, Infinity) =>  if self>Self::zero() {Infinity}else if self<Self::zero() {NegInfinity} else{ Self::zero()},
            (Number(a), Number(b)) => Number(a + b),
        }
    }
}
*/
impl<N: ops::Neg<Output = N> + Clone> ops::Neg for DashNumber<N> {
    type Output = Self;
    fn neg(self) -> Self::Output {
        use DashNumber::*;
        match self {
            NegInfinity => Infinity,
            Number(n) => Number(-n.clone()),
            Infinity => NegInfinity,
        }
    }
}

impl<N: ops::Add<Output = N> + Copy> ops::Add for DashNumber<N> {
    type Output = Self;
    fn add(self, other: Self) -> Self::Output {
        use DashNumber::*;
        match (self, other) {
            (NegInfinity, _) => NegInfinity,
            (_, NegInfinity) => NegInfinity,
            (Infinity, _) => Infinity,
            (_, Infinity) => Infinity,
            (Number(a), Number(b)) => Number(a + b),
        }
    }
}
macro_rules! impl_add_assign {
    ($e:ty, $t:ty$(:$lt:lifetime)?) => {
        impl<$($lt,)? N: ops::Add<Output = N> + Clone> ops::AddAssign<$t> for $e
            where
                $e: ops::Add<$t, Output = Self> + Clone
        {
            fn add_assign(&mut self, other: $t) {
                *self=self.clone() + other;
            }
        }
        impl<$($lt,)? N: ops::Add<Output = N> + Clone> ops::SubAssign<$t> for $e
            where
                $e: ops::AddAssign<$t> + Clone,
                $t: ops::Neg<Output=$t>
        {
            fn sub_assign(&mut self, other: $t) {
                *self+=-other;
            }
        }
    };
}

impl_add_assign!(DashNumber<N>, Self);
impl_add_assign!(DashNumber<N>, N);
impl_add_assign!(DashNumber<N>,&'b Self:'b);
/*impl<N: ops::Add<Output = N> + Copy> ops::AddAssign for DashNumber<N> {
    fn add_assign(&mut self, other: Self) {
        *self = *self + other;
    }
}*/
impl<N: ops::Add<Output = N> + Copy> ops::Add<N> for DashNumber<N> {
    type Output = Self;
    fn add(self, other: N) -> Self::Output {
        use DashNumber::*;
        match (self, other) {
            (NegInfinity, _) => NegInfinity,
            (Infinity, _) => Infinity,
            (Number(a), b) => Number(a + b),
        }
    }
}

impl<N: ::num::Integer>  DashNumber<N> {

    fn from_str_radix(str: &str, radix: u32) -> Result<Self, N::FromStrRadixErr> {
        use DashNumber::*;
        match str {
            "∞" => Ok(Infinity),
            "-∞" => Ok(NegInfinity),
            _ => N::from_str_radix(str, radix).map(Number),
        }
    }
fn gcd(&self, other: &Self) -> Self {
        use DashNumber::*;
        match (self, other) {
            (NegInfinity, _) | (Infinity, _) => Number(N::zero()),
            (_, NegInfinity) | (_, Infinity) => Number(N::zero()),
            (Number(a), Number(b)) => Number(a.gcd(&b)),
        }
    }

}

/*impl<N: ops::Add<Output = N> + Copy> ops::AddAssign<N> for DashNumber<N> {
    fn add_assign(&mut self, other: N) {
        *self = *self + other;
    }
}*/
impl<N: ops::Add<Output = N> + ops::Neg<Output = N> + Copy> ops::Sub<DashNumber<N>>
    for DashNumber<N>
{
    type Output = DashNumber<N>;
    fn sub(self, other: DashNumber<N>) -> Self::Output {
        self + (-other)
    }
}
/* The infprecedence struct is used as a wrapper type to represent a value that has an "infinite" precedence in mathematical operations.
 * This is used in the implementation of the ops::Add trait for the DashNumber enum, where the infprecedence struct is used to ensure that when using NegInfinity and Infinity values, when the operation is not well defined (for example inf-inf), it defaults to positive Infinity with operations involving DashNumber values. As opposed to negative infinity.
 * This allows the DashNumber enum to correctly represent and handle the mathematical concept of infinity in its operations.
 * It's just the Max-Plus multiplication as opposed to it's Dual: The Min-Plus multiplication */
//
struct infprecedence<N>(N);
trait unwrappable<N> {
    fn unwrap(&self) -> N;
}
impl<N: Copy> unwrappable<N> for infprecedence<N> {
    fn unwrap(&self) -> N {
        self.0
    }
}
impl<N: ops::Add<Output = N> + Copy> ops::Add<DashNumber<N>> for infprecedence<DashNumber<N>> {
    type Output = DashNumber<N>;
    fn add(self, other: DashNumber<N>) -> Self::Output {
        use DashNumber::*;
        match (self.unwrap(), other) {
            (Infinity, _) => Infinity,
            (_, Infinity) => Infinity,
            (NegInfinity, _) => NegInfinity,
            (_, NegInfinity) => NegInfinity,
            (Number(a), Number(b)) => Number(a + b),
        }
    }
}
impl<N: ops::Add<Output = N> + ops::Neg<Output = N> + Copy> ops::Sub<DashNumber<N>>
    for infprecedence<DashNumber<N>>
{
    type Output = DashNumber<N>;
    fn sub(self, other: DashNumber<N>) -> Self::Output {
        self + (-other)
    }
}

#[derive(Debug,Eq!,Ord,Copy!)]
struct gd<T> {
    g: T,
    d: T,
}
impl<T: Clone + fmt::Display> fmt::Display for gd<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "g{}.d{}", self.g, self.d)
    }
}
impl<T> PartialOrd for gd<T>
where
    T: PartialOrd,
{
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        //Some(self.cmp(other))
        if self.g == other.g {
            other.d.partial_cmp(&self.d)
        } else {
            self.g.partial_cmp(&other.g)
        }
    }
}

impl<T: Clone> From<(T, T)> for gd<DashNumber<T>> {
    fn from(item: (T, T)) -> Self {
        gd {
            g: item.0.into(),
            d: item.1.into(),
        }
    }
}
impl<T: Clone> From<(T, T)> for gd<T> {
    fn from(item: (T, T)) -> Self {
        gd {
            g: item.0,
            d: item.1,
        }
    }
}
#[derive(Debug,Ord!,Copy!)]
enum epsortop {
    eps,
    NoXtreme,
    top,
}
impl<T: Clone + Default> gd<DashNumber<T>> {
    fn isDegenerate(&self) -> bool {
        use DashNumber::*;
        match (&self.g, &self.d) {
            (Infinity | NegInfinity, _) => true,
            (_, Infinity | NegInfinity) => true,
            _ => false,
        }
        // self.g == NegInfinity || self.g == Infinity || self.d == NegInfinity || self.d == Infinity
    }
    fn is_epsilon_or_top(&self) -> epsortop {
        use epsortop::*;
        use DashNumber::*;
        match (&self.g, &self.d) {
            (Infinity, Infinity) | (_, NegInfinity) => eps,
            (NegInfinity, _) => top,
            (_, _) => NoXtreme,
        }
    }
    fn epsilon() -> Self {
        use DashNumber::*;
        (Infinity, NegInfinity).into()
    }
    fn top() -> Self {
        use DashNumber::*;
        (NegInfinity, Infinity).into()
    }
    fn zero() -> Self {
        (T::default(), T::default()).into()
    }
}
impl<T: Copy + Default + PartialOrd> gd<DashNumber<T>> {
    fn star(&self) -> series<T> {
        use DashNumber::*;
        //Degenerate Case
        if self.g == DashNumber::Infinity || self.d == DashNumber::default() {
            return series {
                p: gd::epsilon().into(),
                q: Poly::from(gd::zero()),
                r: gd::zero(),
                canonise: true,
            };
        } else if self.g == DashNumber::default() && self.d > DashNumber::default() {
            return series {
                p: gd::epsilon().into(),
                q: Poly::from(gd::zero()),
                r: gd {
                    g: DashNumber::default(),
                    d: Infinity,
                },
                canonise: true,
            };
        } else if self.d == Infinity {
            return series {
                p: gd::zero().into(),
                q: Poly::from(gd::from((self.g, Infinity))),
                r: (self.g, Infinity).into(),
                canonise: true,
            };
        }
        //Classical Case
        return series {
            p: gd::epsilon().into(),
            q: Poly::from(gd::zero()),
            r: self.clone(),
            canonise: true,
        };
    }
}
impl<T: Copy + Default + ops::Add<Output = T>> gd<DashNumber<T>> {
    fn otimes(&self, other: &Self) -> Self {
        use DashNumber::*;
        //product of 2 monomials, degenerate cases are treated
        //as input, 2 monomials by reference
        //the function returns
        //a monomial to allow for successive calls
        gd {
            g: self.g + other.g,
            d: self.d + other.d,
        }
    }
}
impl<T: Clone + Default> Default for gd<DashNumber<T>> {
    fn default() -> Self {
        Self::epsilon()
    }
}
impl<T: Ord + Clone> gd<DashNumber<T>> {
    fn inf(&self, other: &gd<DashNumber<T>>) -> gd<DashNumber<T>> {
        gd {
            g: (&self.g).max(&other.g).clone(),
            d: (&self.d).min(&other.d).clone(),
        }
    }
}
impl<T: Copy + ops::Neg<Output = T> + ops::Add<Output = T>> gd<DashNumber<T>> {
    fn frac(&self, other: &gd<DashNumber<T>>) -> gd<DashNumber<T>> {
        gd {
            g: self.g + (-other.g),
            d: infprecedence(self.d) - other.d,
        }
    }
    fn dualFrac(&self, other: &gd<DashNumber<T>>) -> gd<DashNumber<T>> {
        gd {
            g: infprecedence(self.g) - other.g,
            d: self.d + (-other.d),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
struct Poly<T: Clone> {
    epsNTop: epsortop,
    data: Vec<T>,
    // n: u32,
    //  nblock: u32,
    simple: bool,
}
#[derive(Debug,Ord!,Copy!)]
enum Polyinit {
    simplify,
    notsimple,
    onlysimplify,
    alreadysimple,
}
impl<T: Clone> Poly<gd<DashNumber<T>>> {
    fn new(someData: Vec<gd<DashNumber<T>>>) -> Self {
        Poly {
            epsNTop: epsortop::NoXtreme,
            data: someData,
            simple: false,
        }
    }
}

impl<T: Clone> Default for Poly<gd<DashNumber<T>>> {
    fn default() -> Self {
        Self::new(vec![])
    }
}

impl<T: Copy + fmt::Display> fmt::Display for Poly<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.epsNTop {
            epsortop::eps => write!(f, "eps"),
            epsortop::top => write!(f, "T"),
            _ => write!(
                f,
                "{}",
                self.data
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join("+")
            ),
        }
    }
}
impl<T: Copy + PartialEq + Default> From<gd<DashNumber<T>>> for Poly<gd<DashNumber<T>>> {
    fn from(item: gd<DashNumber<T>>) -> Self {
        let a = item.is_epsilon_or_top();
        use epsortop::*;
        Poly {
            epsNTop: a,
            data: vec![match a {
                eps => gd::epsilon(),
                top => gd::top(),
                _ => item,
            }],
            simple: true,
        }
    }
}
impl<T: Copy + PartialEq + Default> From<Vec<gd<DashNumber<T>>>> for Poly<gd<DashNumber<T>>> {
    fn from(item: Vec<gd<DashNumber<T>>>) -> Self {
        //let a = item.is_epsilon_or_top();
        use epsortop::*;
        Poly {
            epsNTop: NoXtreme,
            data: item,
            simple: false,
        }
    }
}
impl<T: Copy + Ord + Default + ops::Add<Output = T>> Poly<gd<DashNumber<T>>> {
    fn otimes(&self, rhs: &Self) -> Self {
        assert!(
            self.simple && rhs.simple,
            "otimes should only be called on simplified polynoms"
        );
        use itertools::iproduct;
        let product: Vec<gd<DashNumber<T>>> = iproduct!(&self.data, &rhs.data)
            .map(|(&a, &b)| a.otimes(&b))
            .collect();
        Self::init(product, Polyinit::simplify)
    }
}
impl<T: Copy + Ord + Default> Poly<gd<DashNumber<T>>> {
    //get causal part of Polynom
    fn prcaus(&self) -> Self {
        assert!(
            self.simple,
            "prcaus shouldn't be called on unsimplified form"
        );
        let mut local = Self::default();
        local.data = self
            .data
            .clone()
            .into_iter()
            .filter(|&g| g.g >= T::default() && g.d >= T::default())
            .collect();
        // If local.data.len()==self.data.len(), all the monomials of p were causal
        // Otherwise, there may be the causal part of the self.data.len()-i-1th to add
        if local.data.len() < self.data.len() {
            local
                .data
                .push(self.data[self.data.len() - 1 - local.data.len()].clone())
            //guaranteed to work
        }
        local.simple = false;
        local.simplify();
        local
    }

    fn simplify(&mut self) {
        if self.simple {
            return;
        };
        self.data.sort_unstable();
        self.onlysimply();
    }
    fn Top() -> Self {
        gd::top().into()
    }
    fn Epsilon() -> Self {
        gd::epsilon().into()
    }
    //fn pop(&mut self)
    fn popj(&mut self, j: usize) {
        if !self.data.is_empty() {
            //if j<self.n-1{
            /*for i in j..(self.n-1){
                    self.data[i]=self.data[i+1]
                }
            if(j<self.n)
                self.pop()
                */
            // self.data.drain(j..(j + 1));
            self.data.remove(j);
            if self.data.len() <= 1 {
                self.data = vec![gd {
                    g: DashNumber::Infinity,
                    d: DashNumber::NegInfinity,
                }]
            }
        }
    }
    fn assign(&mut self, monom: Vec<gd<DashNumber<T>>>, propre: bool) {
        self.data = monom;
        self.simple = propre;
    }
    fn init(monom: Vec<gd<DashNumber<T>>>, can: Polyinit) -> Self {
        let mut something = Self::new(vec![]);
        something.assign(monom, false);
        use Polyinit::*;
        match can {
            simplify => something.simplify(),
            notsimple => (),
            onlysimplify => something.onlysimply(),
            alreadysimple => something.simple = true,
        };
        something
    }
    fn onlysimply(&mut self) {
        use DashNumber::*;
        if self.simple {
            return;
        };
        //  gamma has precedence bigger gamma goes to the right, smaller delta goes to the right,
        //  deltas smaller than 0 are ignored
        let mut i: usize = 0;
        for j in 1..(self.data.len()) {
            //get rid of values where delta is not isotone in case it is isotone, it is counted
            //through i
            if self.data[j].d > self.data[i].d {
                i = i + 1;
                println!("replacing self.data[{}] with self.data[{}]", i, j);
                if i != j {
                    //prevent unnecessary copying
                    self.data[i] = self.data[j];
                }
            }
        }
        let mut n = (i) + 1;
        if self.data[0].g == NegInfinity {
            if self.data[0].d != NegInfinity {
                self.assign(vec![gd::top()], true);
            } else {
                while self.data[0].g == NegInfinity {
                    self.popj(0)
                }
            }
            n = self.data.len();
        }
        //if right most element has an infinity gamma, it gets removed, in case of it being the
        //last value it gets converted to eps
        if self.data[n - 1].g == Infinity && self.data[n - 1].d != NegInfinity {
            self.popj(n - 1);
            n = self.data.len();
        }
        //if left most element has -infinity delta, it gets removed, if it's the first value it
        //also gets converted to eps
        if self.data[0].d == NegInfinity && self.data[0].g != Infinity {
            self.popj(0);
            n = self.data.len();
        }
        //checking if my logic makes sense
        //assert!(n == self.data.len(), "n and self.data.len not equal");
        if n != self.data.len() {
            self.data.drain(n..);
        }
        self.simple = true;
    }

    fn oplus(&self, rhs: &Self) -> Self {
        //assume simplified
        assert!(
            self.simple && rhs.simple,
            "both Polynomes should be simplified"
        );
        match self.epsNTop {
            epsortop::top => Self::Top(),
            epsortop::eps => rhs.clone(),
            epsortop::NoXtreme => {
                let n = self.data.len() + rhs.data.len();
                let mut temp: Vec<gd<DashNumber<T>>> =
                    iter::repeat(Default::default()).take(n).collect();
                let nb = loop {
                    let mut first1 = 0;
                    let mut first2 = 0;
                    let mut tempi = 0;
                    let f = &self.data;
                    let s = &rhs.data;
                    if s[first2].g < f[first1].g {
                        if s[first2].d > temp[tempi].d {
                            if s[first2].g <= temp[tempi].g {
                                temp[tempi] = s[first2];
                            } else {
                                tempi += 1;
                                temp[tempi] = s[first2];
                            }
                        }

                        first2 += 1;
                    } else {
                        if f[first1].d > temp[tempi].d {
                            if f[first1].g <= temp[tempi].g {
                                temp[tempi] = f[first1];
                            } else {
                                tempi += 1;
                                temp[tempi] = f[first1];
                            }
                        }
                        first1 += 1;
                    }
                    if first1 == f.len() {
                        while !(first2 == s.len()) {
                            if s[first2].d > temp[tempi].d {
                                if s[first2].g <= temp[tempi].g {
                                    temp[tempi] = s[first2];
                                } else {
                                    tempi += 1;
                                    temp[tempi] = s[first2];
                                }
                            }
                            first2 += 1;
                        }
                        tempi += 1;
                        break tempi;
                    }
                    if first2 == s.len() {
                        while !(first1 == f.len()) {
                            if f[first1].d > temp[tempi].d {
                                if f[first1].g <= temp[tempi].g {
                                    temp[tempi] = f[first1];
                                } else {
                                    tempi += 1;
                                    temp[tempi] = f[first1];
                                }
                            }
                            first1 += 1;
                        }
                        tempi += 1;
                        break tempi;
                    }
                };
                let mut result = Self::init(temp, Polyinit::notsimple);
                result
            }
        }
    }
}

impl<T> Poly<gd<DashNumber<T>>>
where
    T: Copy + Default + ops::AddAssign + ::num::Integer, // + ops::Add<Output = T>,
{
    fn star(&self) -> Result<series<T>, &str> {
        use DashNumber::*;
        let mut numax = Infinity;
        if !self.simple {
            return Err("please simplify before using the star operator.");
        }
        let qtemp = self.prcaus();
        if !self.eq(&qtemp) {
            return Err("Star of non causal Polynomial is prohibited");
        }
        if self.data.len() == 0 {
            return Err("Polynom is empty");
        }
        //So now we know it must be at least length of 1
        if self.data[0].g == NegInfinity {
            return Ok(series::<T> {
                p: Poly::Epsilon(),
                q: Poly::Top(),
                r: gd::zero(),
                canonise: true,
            });
        }
        if self.data[0].g == Infinity {
            return Ok(series::<T> {
                r: gd::zero(),
                p: Poly::Epsilon(),
                q: gd::zero().into(),
                canonise: true,
            });
        }
        let mut poly1 = self.data.clone();
        for i in 0..self.data.len() {
            // We remove the elements whose star value is e. But a simplified polynom contains no
            // gamma infinities unless it's eps
            if self.data[i].g == Infinity || self.data[i].d == DashNumber::default() {
                // the result is the series is: epsilon+ e .(e)*
                // because there is only one element that is null in the polynomial
                return Ok(series::<T> {
                    r: gd::zero(),
                    p: Poly::Epsilon(),
                    q: gd::zero().into(),
                    canonise: true,
                });
                // we return (epsilon+ e .(e)*
            }
            // we check if the star of one of the elements is equal to (delta)*
            if self.data[i].g == DashNumber::default() && self.data[i].d > DashNumber::default() {
                return Ok(series::<T> {
                    r: ((DashNumber::default(), Infinity).into()),
                    p: Poly::from(gd::from((DashNumber::default(), Infinity))),
                    q: gd::epsilon().into(),
                    canonise: true,
                });
            }
            if self.data[i].d == Infinity {
                // we save the nui associated with a taui=Infinity
                if self.data[i].g < numax {
                    numax = self.data[i].g;
                }
            }
        }
        //self.data[0].g.some_mul(T::one());
        //We handle cases where at least one of the taui is infinite
        if numax != Infinity {
            let mut p: Self = Poly::from(gd::zero());
            for i in 0..self.data.len() {
                //For each element, we continue until numax if necessary.
                let mut j = T::one();
                while self.data[i].g * j < numax {
                    // let monom:gd=(self.data[i].g*j,self.data[i].d*j).into();
                    // monome.init(j*self.data[i].g,j*self.data[i].d);
                    p.data.push((self.data[i].g * j, self.data[i].d * j).into());
                    j += T::one();
                }
            }
            use ::num::Zero;
            return Ok(series {
                p,
                r: (DashNumber::zero(), DashNumber::Infinity).into(),
                q: Poly::from(gd::from((DashNumber::zero(), Infinity))),
                canonise: true,
            });
        }

        // The Non  degenereated case

        // Below is a trick to avoid computing the star of a polynomial that is already a star (08/04/2019 Angers)
        if self.data[0] == gd::zero() {
            let qtemp = self.otimes(self);
            if &qtemp == self {
                // It is already a star, it is enough
                return Ok(qtemp.into());
            }
        }
        // End of the added trick (08/04/2019 Angers)

        // Search for the smallest slope with the smallest nu
        let (nj, pente) = self
            .data
            .iter()
            .enumerate()
            .min_by_key(|(_, &item)| item.g / item.d)
            .unwrap();

        // New algorithm: we deal with all monomials with the slope equal to the smallest slope (30/12/2018), which means the slower production rate
        let mut stemp = series::default();
        let result = self.data[nj].star();
        for i in (nj + 1)..self.data.len() {
            //let pente1 = self.data[i].g as f64 / self.data[i].d as f64;
            if self.data[i].g * pente.d == pente.g * self.data[i].d {
                stemp.p = Poly::Epsilon();
                stemp.q = gd::zero().into();
                stemp.r = self.data[i].clone();
                //remove       result = result.otimes(stemp);
                // self.popj(i); // Remove the element from the polynomial, it has been processed
                /* if i < nj {
                    nj -= 1; // Shift the nj element if necessary
                }*/
                // i -= 1;
            }
        }

        // Process all stars whose slope is lower than the retained slope above
        for i in 0..self.data.len() {
            stemp.p = Poly::Epsilon();
            stemp.q = gd::zero().into();
            stemp.r = self.data[i].clone();
            //    stemp.init(epsilon, e, self.data[i]);
            //     result = otimes(stemp, result);
        }

        return Ok(result);

        /*{
            p: Poly<gd<DashNumber<T>>>,
            q: Poly<gd<DashNumber<T>>>,
            r: gd<DashNumber<T>>,
            canonise: bool,
        }
        */
        // todo!()
    }
}
#[derive(Clone)]
struct series<T: Clone> {
    p: Poly<gd<DashNumber<T>>>,
    q: Poly<gd<DashNumber<T>>>,
    r: gd<DashNumber<T>>,
    canonise: bool,
}
impl<T: ops::Neg<Output = T> + Copy + Default + ::num::Integer> series<T>
where
    usize: TryFrom<DashNumber<T>, Error = TryFromDashNumberError>,
    T: From<u8> + From<usize> + fmt::Debug, // <usize as TryFrom<T>>::Error: fmt::Debug,
                                            //  <usize as TryFrom<DashNumber<T>>>::Error: TryFromDashNumberError
{
    fn otimes(&self, s2: &Self) -> Self {
        use DashNumber::*;
        assert!(
            self.canonise && s2.canonise,
            "series should be canonized before"
        );
        let mut result: series<T> = Default::default();
        //** if one of the series is equal to epsilon
        if self.q.data[0].g == Infinity || s2.q.data[0].g == Infinity {
            return series {
                p: Poly::Epsilon(),
                q: Poly::Epsilon(),
                r: gd::zero(),
                canonise: true,
            };
        };
        //if one of the series is equal to top
        if self.q.data[0].g == NegInfinity || s2.q.data[0].g == NegInfinity {
            return series {
                p: Poly::Epsilon(),
                q: Poly::Top(),
                r: gd::zero(),
                canonise: true,
            };
        };
        // (p1 + q1r1*)(p2 + q2r2*)=p1p2 +p1 q2 r2* + p2 q1 r1* + q1 q2 r1* r2 *
        result.canonise = false;
        result.p = self.p.otimes(&s2.p); // p1 p2
        result.q = self.p.otimes(&s2.p); // p1 q2
        result.r = s2.r;
        result.canon();
        let mut temp1 = series {
            q: s2.p.otimes(&self.q),
            p: Poly::<_>::from(gd::epsilon()),
            r: self.r,
            canonise: false,
        };
        temp1.canon();
        result = result.oplus(&temp1);
        todo!();
    }
    fn oplus(&self, s2: &Self) -> Self {
        use DashNumber::*;
        assert!(
            self.canonise && s2.canonise,
            "series should be canonized before"
        );
        let mut result: series<T> = Default::default();
        //if one of the series is equal to top
        if self.q.data[0].g == NegInfinity || s2.q.data[0].g == NegInfinity {
            return series {
                p: Poly::Epsilon(),
                q: Poly::Top(),
                r: gd::zero(),
                canonise: true,
            };
        };
        //** if one of the series is equal to epsilon
        if self.q.data[0].g == Infinity {
            return s2.clone();
        }
        if s2.q.data[0].g == Infinity {
            return self.clone();
        }
        //test the degenerate case
        if <DashNumber<T> as ::num::Zero>::is_zero(&self.r.d)
            && <DashNumber<T> as ::num::Zero>::is_zero(&s2.r.d)
        {
            return Self::from(self.p.oplus(&self.q.oplus(&s2.p.oplus(&s2.q))));
        }
        let mut ads2 = &s2;
        let mut ads1 = &self;
        if <DashNumber<T> as ::num::Zero>::is_zero(&self.r.d)
            && !<DashNumber<T> as ::num::Zero>::is_zero(&s2.r.d)
        {
            // Swap s1 and s2
            ads2 = &self;
            ads1 = &s2;
        }
        if !<DashNumber<T> as ::num::Zero>::is_zero(&ads1.r.d)
            && <DashNumber<T> as ::num::Zero>::is_zero(&ads2.r.d)
        {
            let p2 = ads1.p.oplus(&ads2.p.oplus(&ads2.q));
            if <DashNumber<T> as ::num::Zero>::is_zero(&ads1.r.d) {
                // p = p1 + p2 + q2 + q1 * (0,infinite)
                // q = last point of the polynomial
                // r = 0,0
                return Self::from(
                    p2.oplus(&Poly::from(gd::from((ads1.q.data[0].g.clone(), Infinity)))),
                );
            } else {
                // p = p1 + p2 + q2
                // q = q1
                // r = r1
                result = series {
                    p: p2,
                    q: ads1.q.clone(),
                    r: ads1.r,
                    canonise: false,
                };
                result.canon();
                return result;
            }
        }

        if !<DashNumber<T> as ::num::Zero>::is_zero(&self.r.d)
            && !<DashNumber<T> as ::num::Zero>::is_zero(&s2.r.d)
            && <DashNumber<T> as ::num::Zero>::is_zero(&self.r.g)
            && <DashNumber<T> as ::num::Zero>::is_zero(&s2.r.g)
        {
            // p = p1 + p2 + q1 * (0, Infinity) + q2 * (o, Infinity)
            let monome: gd<DashNumber<T>> = (self.q.data[0].g, Infinity).into(); // normally not necessary
                                                                                 //  result.p = monome;
            result.p = Poly::from(result.p).oplus(&self.p.oplus(&s2.p));
            let monome: gd<_> = (s2.q.data[0].g, Infinity).into(); // normally not necessary
            result.p = Poly::from(monome).oplus(&result.p);
            result = series::from(result.p);
            result.canonise = true;

            return result;
        }
        let mut i: T;
        if !<DashNumber<T> as ::num::Zero>::is_zero(&self.r.d)
            && !<DashNumber<T> as ::num::Zero>::is_zero(&s2.r.d)
            && !<DashNumber<T> as ::num::Zero>::is_zero(&self.r.g)
            && <DashNumber<T> as ::num::Zero>::is_zero(&s2.r.g)
        {
            // p = p2 + q2 * (0, Infinity) + p1 + q1 * (r1) *

            // extend q1 beyond the first point of q2
            let mut result = series::default();

            result.p = self.q.clone();
            i = T::one();

            while result.p.data[result.p.data.len() - 1].g <= s2.q.data[0].g {
                let monome: gd<_> = (self.r.g * i, self.r.d * i).into();
                let temp = self.q.otimes(&monome.into());
                result.p = result.p.oplus(&temp);
                i = i + T::one();
            }

            let monome: gd<_> = (s2.q.data[0].g, Infinity).into(); // not necessary
            result.p =
                s2.p.oplus(&self.p.oplus(&result.p.oplus(&Poly::from(monome))));
            result = series::from(result.p);
            result.r = (T::default().into(), Infinity).into(); // not necessary
            result.canonise = true;

            return result;
        }
        if !<DashNumber<T> as ::num::Zero>::is_zero(&self.r.d)
            && !<DashNumber<T> as ::num::Zero>::is_zero(&s2.r.d)
            && <DashNumber<T> as ::num::Zero>::is_zero(&self.r.g)
            && !<DashNumber<T> as ::num::Zero>::is_zero(&s2.r.g)
        {
            // p = p1 + q1 * (0, Infinity) + p2 + q2 * (r2)

            // extend q2 beyond the first point of q1
            result.p = s2.q.clone();
            i = T::one();
            while result.p.data[result.p.data.len() - 1].g <= self.q.data[0].g {
                let monome: gd<_> = (s2.r.g * i, s2.r.d * i).into();
                result.p = result.p.oplus(&s2.q.oplus(&Poly::from(monome)));
                i = i + T::one();
            }
            let monome: gd<_> = (self.q.data[0].g, Infinity).into(); // normally not necessary
            result.p = self
                .p
                .oplus(&s2.p.oplus(&result.p.oplus(&Poly::from(monome))));
            result = series::from(result.p);
            result.r = (T::zero().into(), Infinity).into(); // normally not necessary
            result.canonise = true;
            return result;
        }

        todo!();
    }
    fn canon(&mut self) {
        use DashNumber::*;
        if self.canonise == true {
            return; // already in canonical form
        }
        self.p.simplify();
        self.q.simplify();
        let top = gd::top();
        let epsilon = gd::epsilon();
        let zero = gd::zero();

        // case where p=T or q=T
        if self.p.data[0] == top || self.q.data[0] == top
        // the series is equal to top
        {
            self.p = epsilon.into();
            self.q = top.into();
            self.r = zero;
            self.canonise = true;
            return;
        }

        // case where q = epsilon
        if self.q.data[0] == epsilon {
            // add a polynomial
            // set the last point of the polynomial
            self.q = self.p.data.pop().unwrap().into(); // and remove it from p
            self.r = zero;
            self.canonise = true;
            return;
        }
        // Case where p or q is a trajectory
        if self.q.data.last().unwrap().d == Infinity || self.p.data.last().unwrap().d == Infinity {
            let mut res = self.p.oplus(&self.q);
            self.q = res.data.pop().unwrap().into(); // Set the last point of p to q
            self.p = res;
            self.r = (T::default().into(), Infinity).into();
            self.canonise = true;
            return;
        }
        //The degenerate case
        // case where r.g is Infinity or r.d is 0
        if self.r.g == Infinity || self.r.d == DashNumber::default() {
            self.p = self.p.oplus(&self.q);
            self.q = self.p.data.pop().unwrap().into(); // and remove it from p
            self.r = (T::default().into(), Infinity).into();
            self.canonise = true;
            return;
        }

        // case where r.g is greater than 0 and r.d is Infinity
        if self.r.g > DashNumber::default() && self.r.d == Infinity {
            self.p = self.p.oplus(&self.q);
            self.q = self.q.data[0].otimes(&self.r).into();
            // if the degree of q is less than the degree of p
            if self.q.data[0].g <= self.p.data.last().unwrap().g {
                self.p = self.p.oplus(&self.q);
                self.p.data.pop(); // remove the last element of the polynomial if there are 2
            }
            self.r = (T::default().into(), Infinity).into();
            self.canonise = true;
            return;
        }

        // case where r.d is greater than 0 and r.g is 0
        if self.r.d > DashNumber::default() && self.r.g == DashNumber::default() {
            self.r = (T::default().into(), Infinity).into();
            self.q = self.q.data[0].otimes(&self.r).into();
            self.p = self.p.oplus(&self.q);
            self.p.data.pop();
            self.canonise = true;
            return;
        }
        //The non-degenerate case
        // Putting the periodic in proper form
        // that is, the pattern fits within the periodic
        //We remove the roots of the polynomial q that would be dominated by others after development
        for j in (1..self.q.data.len()).rev() {
            for i in 0..j {
                let k: DashNumber<T> = ((self.q.data[j].g - self.q.data[i].g) / self.r.g).round(); // round down
                if k >= <DashNumber<T> as ::num::One>::one()
                    && (self.q.data[i].d + k * self.r.d) >= self.q.data[j].d
                {
                    self.q.popj(j);
                    break;
                }
            }
        }
        let mut j = self.q.data.len() - 1; // index of last element
        let nb_g = self.q.data[j].g - self.q.data[0].g; // number of gammas in the periodic pattern -1

        // reduce the size of the pattern so that it is included in r
        if nb_g >= self.r.g || self.q.data[j].d - self.q.data[0].d >= self.r.d {
            let mut periodique = Vec::with_capacity(self.q.data.len());
            periodique.push(self.q.data[j]); // the first is the last
            let nb_max: usize =
                usize::try_from(((self.q.data[j].g - self.q.data[0].g) / self.r.g).round())
                    .unwrap()
                    + 1; // maximum number of points to add to the transient
            let mut transitoire = Vec::with_capacity(nb_max * self.q.data.len()); // nb_max * j corresponds to an upper bound on the number of elements that will be added to the transient
            let mut k = 0;
            for i in 0..j {
                // for all points in the pattern except the last
                transitoire.push(self.q.data[i]); // add point i
                let nbcoups: usize = (((self.q.data[j].g
                    - self.q.data[i].g
                    - <DashNumber<T> as ::num::One>::one())
                    / self.r.g)
                    .round())
                .try_into()
                .unwrap(); // how many points to add to offset the last one
                for n in 1..=nbcoups {
                    transitoire.push(transitoire[k + n - 1].otimes(&self.r)); // add nbcoups points
                }
                k += nbcoups; // reset index
                periodique.push(transitoire[k].otimes(&self.r)); // add point offsetting the last to the new pattern
                k += 1;
            }
            //transitoire
            let ptemp = Poly::init(transitoire, Polyinit::simplify); // instead of using add, create a poly ptemp already sorted, just simplify
            self.p = self.p.oplus(&ptemp); // then sum, a simple merge
            self.q = Poly::init(periodique, Polyinit::simplify); // reset with new periodic pattern
        }

        // Reduce r and therefore q
        let mut indice: DashNumber<T>;
        if self.r.g <= self.r.d {
            indice = self.r.g; // Search for the largest possible divisor
        } else {
            indice = self.r.d;
        }

        while indice >= T::from(2u8) && self.q.data.len() > 1 {
            // Does indice divide r.getg() and r.getd()
            if get_number!(self.r.g) % get_number!(indice) == T::zero()
                && get_number!(self.r.d) % get_number!(indice) == T::zero()
            {
                // Calculate the new nu and the new tau
                let nu = (self.r.g / indice).round();
                let tau = (self.r.d / indice).round();
                // Calculate the supposed pattern
                let mut nouveau: Poly<_> = self.q.clone();
                let mut i: usize = 1;

                while i < self.q.data.len() - 1
                    && self.q.data[i].g - self.q.data[0].g < nu
                    && self.q.data[i].d - self.q.data[0].d < tau
                {
                    nouveau.data.push(self.q.data[i].clone());
                    //               nouveau.add(&self.q.data[i]);
                    i += 1;
                }
                nouveau.simple = true; //No need to simplify.
                #[cfg(debug_assertions)]
                {
                    let mut a = nouveau.clone();
                    a.simple = false;
                    a.simplify();
                    assert_eq!(a, nouveau);
                }
                // We extend it for comparison with q
                let mut etendu = nouveau.clone();
                for i in 1..usize::try_from(indice).unwrap() {
                    let nutau: gd<DashNumber<T>> = (nu * T::from(i), tau * T::from(i)).into();
                    etendu = etendu.oplus(&(nouveau.otimes(&nutau.into())));
                }
                // We compare etendu and q if they are equal we have reduced the size of the periodic
                // Otherwise, it is necessary to start again with a smaller index
                let mut egaux: bool = false;
                if etendu.data.len() == self.q.data.len() {
                    // If they have the same size, the first n points are necessarily equal, no need to test them
                    i = nouveau.data.len();
                    egaux = true;
                    while egaux && i < self.q.data.len() {
                        if etendu.data[i] != self.q.data[i] {
                            egaux = false;
                        }
                        i += 1;
                    }
                    #[cfg(debug_assertions)]
                    {
                        assert_eq!(egaux, etendu == self.q);
                        println!("if this doesn't error you should simply use etendu==self.q")
                    }
                }

                if egaux {
                    self.q = nouveau.into(); // Reduce the periodic
                    self.r = (nu, tau).into();
                    if self.r.g <= self.r.d {
                        indice = self.r.g;
                    } else {
                        indice = self.r.d;
                    }
                } else {
                    indice -= T::one();
                } // index was common divisor but there is no equality
            } else {
                indice -= T::one(); // not common divisor
            }
        }
        // Reduce the transitory
        // 1st: are some points of the transitory beyond the periodic?
        // Are the last points of the transitory not dominated by the periodic?
        let mut i = self.p.data.len() - 1;

        loop {
            let mut domine = false;
            let mut j = 0;
            loop {
                // Test if the last element of the transitory is dominated by an element of the periodic
                if self.p.data[i] <= self.q.data[j] && self.p.data[i] != epsilon {
                    // If so, remove it
                    self.p.data.pop();
                    // Point to the new last element
                    i = self.p.data.len() - 1;
                    domine = true;
                } else {
                    j += 1;
                }
                if j >= self.q.data.len() || domine == true {
                    break;
                }
            }
            if domine == false {
                break;
            }
        }
        // Does the last point of the transitory not dominate the periodic?
        i = self.p.data.len() - 1;
        if self.p.data[i] != epsilon {
            let mut temp: Poly<_> = epsilon.into();
            while self.p.data[i].g >= self.q.data[0].g || self.p.data[i].d >= self.q.data[0].d {
                // It is necessary to shift the periodic as much as necessary to make the transitory part clean
                for j in 0..self.q.data.len() {
                    temp.data.push(self.q.data[j].clone());
                }
                self.q = self.q.otimes(&self.r.into());
            }
            self.p = temp.oplus(&self.p);
        }
        // Shift the periodic as much as possible onto the transitory
        while self.q.data.last().unwrap() == &self.p.data.last().unwrap().otimes(&self.r) {
            self.q.data.rotate_right(1);
            // Place the last element of p at the head
            self.q.data[0] = self.p.data.pop().unwrap();
            /*        for i in (1..self.q.data.len()).rev() {
                      self.q.data[i] = self.q.data[i - 1].clone();
                  }
                  self.q.data[0] = self.p.data.pop().unwrap().clone();
            */
        }
        // The series should now be in canonical form
        self.canonise = true;
    }
}
impl<T: Clone + Default> Default for series<T> {
    fn default() -> Self {
        series {
            p: Default::default(),
            q: Default::default(),
            r: (T::default(), T::default()).into(),
            canonise: false,
        }
    }
}
impl<T: Copy + Ord + Default> From<Poly<gd<DashNumber<T>>>> for series<T> {
    fn from(mut item: Poly<gd<DashNumber<T>>>) -> Self {
        //let a = item.is_epsilon_or_top();
        item.simplify();
        assert!(
            item.data.len() > 0,
            "Can't convert Poly to Series, if Poly is zero-sized"
        );
        use epsortop::*;
        let q = item.data.pop().unwrap_or_default();
        series {
            p: item,
            q: q.into(),
            r: gd::zero(),
            canonise: true,
        }
    }
}
fn division<T: ::num::Zero + Copy + PartialEq + ::num::Integer>(
    a: &DashNumber<T>,
    b: &DashNumber<T>,
) -> Option<DashNumber<T>> {
    use DashNumber::*;
    //use ::num::Zero;
    match (a, b) {
        (Infinity | NegInfinity, Infinity | NegInfinity) => None,
        (_, Infinity | NegInfinity) => Some(<DashNumber<T> as ::num::Zero>::zero()),
        (Infinity | NegInfinity, _) => Some(a.clone()),
        (Number(c), Number(d)) => {
            if d == &T::zero() {
                None
            } else {
                Some(Number(
                    ::num::rational::Ratio::new(c.clone(), d.clone()).to_integer(),
                ))
            }
        }
    }
    //todo!();
}
fn main() {
    {
        // my_macro!(3);
        #[cfg(debug_assertions)]
        {
            println!("debug assertions is enabled");
        }
        //let d:DashNumber<i32>= DashNumber::NegInfinity.into();
        let test: DashNumber<i32> = 3.into();
        let test2: DashNumber<i32> = 10.into();
        /*let a = match (test2, test) {
            (DashNumber::Number(a), DashNumber::Number(b)) => {
                Some(::num::rational::Ratio::new(a, b))
            }
            _ => None,
        };*/

        use ::num::ToPrimitive;
        println!("{:?}", test2 / test > (DashNumber::from(9) / 3.into()));
        /*
        let s: gd<DashNumber<i32>> = (DashNumber::Infinity, 5.into()).into();
        //let o:gd<DashNumber<_>> = (1,5).into();
        let mut o = Poly::from(vec![
            (4, 4).into(),
            (4, 3).into(),
            (5, 2).into(),
            (6, 5).into(),
        ]);
        o.data.sort();
        println!("sorted:{:?}", o);
        o.simple = false;
        o.simplify();
        println!("huh:{:?}", o);
        let mut a = Poly::from(s).oplus(&Poly::from(gd::from((5, 4))));
        a.simple = false;
        a.simplify();
        println!("{}", a);*/
    }
}

#[cfg(test)]
mod tests {
    use crate::*;
    #[test]
    fn addition_and_printing() {
        let s: gd<DashNumber<_>> = (1, 5).into();
        let a = Poly::from(s).oplus(&Poly::from(gd::from((0, 4))));
        assert_eq!(a.to_string(), "g0.d4+g1.d5");
    }

    #[test]
    fn test_simplify() {
        let mut p1 = Poly::new(vec![(0, 1).into(), (2, 3).into(), (3, 4).into()]);
        p1.simplify();
        p1.simple = false;
        assert_eq!(
            p1,
            Poly::new(vec![(0, 1).into(), (2, 3).into(), (3, 4).into()])
        );

        let mut p2 = Poly::new(vec![
            (0, 0).into(),
            (0, 0).into(),
            (0, 0).into(),
            (0, 0).into(),
        ]);
        p2.simplify();
        p2.simple = false;
        assert_eq!(p2, Poly::new(vec![(0, 0).into()]));

        //  let mut p3 = Poly::new(vec![1, 0, -1, 0, 1]);
        //  assert_eq!(p3.simplify(), Poly::new(vec![1, 0, -1, 0, 1]));
    }
    #[test]
    fn test_neg_infinity() {
        let neg_infinity: DashNumber<u32> = DashNumber::NegInfinity;
        let res: Result<usize, TryFromDashNumberError> = neg_infinity.try_into();
        assert_eq!(res, Err(TryFromDashNumberError::NegInfinity));
    }

    #[test]
    fn test_number() {
        let number: DashNumber<i32> = DashNumber::Number(10);
        let res: Result<usize, TryFromDashNumberError> = number.try_into();
        assert_eq!(res, Ok(10));
    }

    #[test]
    fn test_infinity() {
        let infinity: DashNumber<u64> = DashNumber::Infinity;
        let res: Result<usize, TryFromDashNumberError> = infinity.try_into();
        assert_eq!(res, Err(TryFromDashNumberError::Infinity));
    }
}
//:execute "$|y" | execute strpart(@",2)
//exe "ExecutorShowDetail"|sleep 1|exe "wincmd w"|set nonumber|exe "wincmd p"|exe "vertical resize 160"|exe "ExecutorSetCommand"|call feedkeys("cargo run\r")|autocmd BufWritePre *.rs :ExecutorRun
