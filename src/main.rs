#![allow(non_snake_case)]
#![allow(non_camel_case_types)]
#![allow(unused_macros)]
#![allow(unused_imports)]
#![allow(unused_variables)]
#![allow(dead_code)]
#![feature(const_try)]


#[derive(Debug,Ord!,Copy!)]
pub enum DashNumber<N> {
    NegInfinity,
    Number(N),
    Infinity,
}
impl<T> From<T> for DashNumber<T> {
    fn from(item: T) -> Self {
        DashNumber::Number(item)
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

use core::*;
use std::vec;
#[macro_use]
extern crate macro_rules_attribute;
macro_rules_attribute::derive_alias! {
    #[derive(Eq!)] = #[derive(Eq, PartialEq)];
    #[derive(Ord!)] = #[derive(Ord, PartialOrd, Eq!)];
    #[derive(Copy!)] = #[derive(Copy, Clone)];
}            

impl<T: ::num::Integer + Clone> DashNumber<::num::rational::Ratio<T>> {
    fn round(self) -> DashNumber<T> {
        use DashNumber::*;
        match self {
            DashNumber::Number(a) => DashNumber::Number(a.to_integer()),
            Infinity => Infinity,
            NegInfinity => NegInfinity,
        }
    }
    fn ceil(self) -> DashNumber<T> {
        use DashNumber::*;
        match self {
            DashNumber::Number(a) => DashNumber::Number(a.ceil().to_integer()),
            Infinity => Infinity,
            NegInfinity => NegInfinity,
        }
    }
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
pub enum TryFromDashNumberError {
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

impl<N: ::num::Integer> DashNumber<N> {
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

#[derive(Debug,Eq!,Copy!)]
struct gd<T> {
    g: T,
    d: T,
}
#[macro_export]
macro_rules! get_number_opt {
    ($e:expr) => {
        match $e {
            $crate::DashNumber::Number(a) => Some(a),
            _ => None,
        }
    };
}
#[macro_export]
macro_rules! get_number {
    ($e:expr) => {
        $crate::get_number_opt!($e).expect("We expected the number to not be an infinity")
    };
}

impl<T: Clone + fmt::Display> fmt::Display for gd<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "g{}.d{}", self.g, self.d)
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

impl<T> PartialOrd for gd<T>
where
    T: PartialOrd,
{
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        //Some(self.cmp(other))
        if self.g == other.g {
            self.d.partial_cmp(&other.d)
        } else {
            other.g.partial_cmp(&self.g)
        }
    }
}
impl<T> Ord for gd<T>
where
    T: Ord,
{
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        if self.g > other.g {
            cmp::Ordering::Less
        } else if self.g < other.g {
            cmp::Ordering::Greater
        } else if self.d < other.d {
            cmp::Ordering::Less
        } else if self.d > other.d {
            cmp::Ordering::Greater
        } else {
            cmp::Ordering::Equal
        }
    }
}

use gd_core::epsortop;
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

trait usefulNum:
    TryFrom<u8>
    + TryFrom<usize>
    + fmt::Debug
    + Clone
    + ::num::Integer
    + Default
    + ops::Neg
    + ops::AddAssign
{
}
impl<
        T: TryFrom<u8>
            + TryFrom<usize>
            + fmt::Debug
            + Clone
            + ::num::Integer
            + Default
            + ops::Neg
            + ops::AddAssign,
    > usefulNum for T
{
}
trait starable {
    type Output;
    fn star(&self) -> Result<Self::Output, String>;
}
impl<T: Copy + Default + PartialOrd> starable for gd<DashNumber<T>> {
    type Output = series<T>;
    fn star(&self) -> Result<Self::Output, String> {
        use DashNumber::*;
        //Degenerate Case
        if self.g == DashNumber::Infinity || self.d == DashNumber::default() {
            return Ok(series {
                p: gd::epsilon().into(),
                q: Poly::from(gd::zero()),
                r: gd::zero(),
                canonise: true,
            });
        } else if self.g == DashNumber::default() && self.d > DashNumber::default() {
            return Ok(series {
                p: gd::epsilon().into(),
                q: Poly::from(gd::zero()),
                r: gd {
                    g: DashNumber::default(),
                    d: Infinity,
                },
                canonise: true,
            });
        } else if self.d == Infinity {
            return Ok(series {
                p: gd::zero().into(),
                q: Poly::from(gd::from((self.g, Infinity))),
                r: (self.g, Infinity).into(),
                canonise: true,
            });
        }
        //Classical Case
        Ok(series {
            p: gd::epsilon().into(),
            q: Poly::from(gd::zero()),
            r: self.clone(),
            canonise: true,
        })
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

/*impl<N: ::num::Zero + Copy> ::num::Zero for series<N> {
    fn zero() -> Self {
        Self::from(Poly::from(gd::zero()))
    }
    fn is_zero(&self) -> bool {
        self == Self::zero()
    }
}*/

#[derive(Debug, Clone, PartialEq)]
struct Poly<T: Clone> {
    epsNTop: epsortop,
    data: Vec<T>,
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
        use DashNumber::*;
        let mut ans = Self::new(vec![gd::from((Infinity, NegInfinity))]);
        ans.epsNTop = epsortop::eps;
        ans.simple = true;
        ans
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
        //let mut p
        Poly {
            epsNTop: NoXtreme,
            data: item,
            simple: false,
        }
    }
}
impl<T: Clone +Copy+ PartialEq + Default> From<Vec<(T,T)>> for Poly<gd<DashNumber<T>>> {
    fn from(item: Vec<(T,T)>) -> Self {
        let vec: Vec<gd<DashNumber<T>>> = item.into_iter().map(|x| gd::from(x)).collect();
        vec.into()
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
        let a = Self::init(product, Polyinit::simplify);
        a
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
        local.data = {
            let pos: Option<usize> = self
                .data
                .iter()
                .rposition(|&g| !(g.g >= T::default() && g.d >= T::default()));
            match pos {
                Some(p) => self.data[(p + 1)..].to_vec(),
                None => self.data.clone(),
            }
        };
        // If local.data.len()==self.data.len(), all the monomials of p were causal
        // Otherwise, there may be the causal part of the self.data.len()-i-1th to add
        if local.data.len() < self.data.len() {
            local
                .data
                .push(self.data[self.data.len() - 1 - local.data.len()].clone())
            //guaranteed to work
        }
        local.simple = false;
        //println!("self.length is {:?}", self.data.len());
        assert!(
            local.data.len() != 0,
            "don't call simplify if local is empty"
        );
        local.simplify();
        local
    }

    fn simplify(&mut self) {
        if self.simple {
            return;
        };
        self.data.sort_unstable_by(|a, b| b.cmp(a));
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
            self.simple = false;
            if self.data.len() < 1 {
                self.epsNTop = epsortop::eps;
                self.simple = true;
                self.data = vec![gd {
                    g: DashNumber::Infinity,
                    d: DashNumber::NegInfinity,
                }]
            }
        }
    }
    fn pop(&mut self) -> gd<DashNumber<T>> {
        assert!(
            self.data.len() > 0,
            "length should be bigger than zero, otherwise don't pop!"
        );
        let last = self.data.len() - 1;
        let a = self.data[last];
        self.popj(last);
        a
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
        assert!(
            self.data.len() != 0,
            "never call simplify when you're empty"
        );
        //  gamma has precedence bigger gamma goes to the right, smaller delta goes to the right,
        //  deltas smaller than 0 are ignored
        let mut i: usize = 0;
        for j in 1..(self.data.len()) {
            //get rid of values where delta is not isotone in case it is isotone, it is counted
            //through i
            if self.data[j].d > self.data[i].d {
                i = i + 1;
                //println!("replacing self.data[{}] with self.data[{}]", i, j);
                if i != j {
                    //prevent unnecessary copying
                    self.data[i] = self.data[j];
                }
            }
        }
        //println!("out of the loop");
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
        self.epsNTop = epsortop::NoXtreme;
        if self.data.len() == 1 && self.data[0] == gd::epsilon() {
            self.epsNTop = epsortop::eps;
        }
        //      println!("end of simiplify");
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
                let mut first1 = 0;
                let mut first2 = 0;
                let mut tempi = 0;
                let f = &self.data;
                let s = &rhs.data;

                let nb = loop {
                    //                    println!("suspect");
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
                result.simplify();
                result
            }
        }
    }
}

impl<T> starable for Poly<gd<DashNumber<T>>>
where
    T: Copy
        + Default
        + ops::AddAssign
        + ::num::Integer
        + fmt::Debug
        + ops::Neg<Output = T>
        + TryFrom<u8>
        + TryFrom<usize>,
    usize: TryFrom<T> + TryFrom<DashNumber<T>, Error = TryFromDashNumberError>, // + ops::Add<Output = T>,
{
    type Output = series<T>;
    fn star(&self) -> Result<series<T>, String> {
        use DashNumber::*;
        let mut numax = Infinity;
        if !self.simple {
            return Err("please simplify before using the star operator.".to_string());
        }
        if self.data.len() == 0 {
            return Err("Polynom is empty".to_string());
        }
        let qtemp = self.prcaus();
        if !self.eq(&qtemp) {
            //println!("HI!{:?}={:?}", self, self.prcaus());
            return Err("Star of non causal Polynomial is prohibited".to_string());
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
        let mut poly1 = self.clone();
        let mut i: usize = 0;
        while i < poly1.data.len() {
            //for i in 0..poly1.data.len() {
            // We remove the elements whose star value is e. But a simplified polynom contains no
            // gamma infinities unless it's eps
            if poly1.data[i].g == Infinity || poly1.data[i].d == DashNumber::default() {
                if poly1.data.len() > 1 {
                    poly1.popj(i);
                } else {
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
            }
            // we check if the star of one of the elements is equal to (delta)*
            if poly1.data[i].g == DashNumber::default() && poly1.data[i].d > DashNumber::default() {
                return Ok(series::<T> {
                    r: ((DashNumber::default(), Infinity).into()),
                    p: Poly::from(gd::from((DashNumber::default(), Infinity))),
                    q: gd::epsilon().into(),
                    canonise: true,
                });
            }
            if poly1.data[i].d == Infinity {
                // we save the nui associated with a taui=Infinity
                if poly1.data[i].g < numax {
                    numax = poly1.data[i].g;
                }
            }
            i += 1;
        }
        //self.data[0].g.some_mul(T::one());
        //We handle cases where at least one of the taui is infinite
        if numax != Infinity {
            let mut p: Self = Poly::from(gd::zero());
            for i in 0..poly1.data.len() {
                //For each element, we continue until numax if necessary.
                let mut j = T::one();
                while poly1.data[i].g * j < numax {
                    // let monom:gd=(self.data[i].g*j,self.data[i].d*j).into();
                    // monome.init(j*self.data[i].g,j*self.data[i].d);
                    p.data
                        .push((poly1.data[i].g * j, poly1.data[i].d * j).into());
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
        if poly1.data[0] == gd::zero() {
            let qtemp = self.otimes(self);
            if &qtemp == &poly1 {
                // It is already a star, it is enough
                return Ok(qtemp.into());
            }
        }
        // End of the added trick (08/04/2019 Angers)

        // Search for the smallest slope with the smallest nu
        let (nj, pente) = poly1
            .data
            .iter()
            .enumerate()
            .min_by_key(|(_, &item)| item.g / item.d)
            .expect("Well, a minimum must exist doesn't it?");

        // New algorithm: we deal with all monomials with the slope equal to the smallest slope (30/12/2018), which means the slower production rate
        let mut stemp = series::default();
        let mut result = poly1.data[nj].star()?; //Works up to here at least
        let mut ignore = vec![];
        for i in (nj + 1)..poly1.data.len() {
            //let pente1 = self.data[i].g as f64 / self.data[i].d as f64;
            if poly1.data[i].g * pente.d == pente.g * poly1.data[i].d {
                stemp.p = Poly::Epsilon();
                stemp.q = gd::zero().into();
                stemp.r = poly1.data[i].clone();
                result = result.otimes(&stemp);
                ignore.push(i);
                // self.popj(i); // Remove the element from the polynomial, it has been processed
                /* if i < nj {
                    nj -= 1; // Shift the nj element if necessary
                }*/
                // i -= 1;
            }
        }

        // Process all stars whose slope is lower than the retained slope above
        for i in 0..poly1.data.len() {
            if !ignore.contains(&i) {
                stemp.p = Poly::Epsilon();
                stemp.q = gd::zero().into();
                stemp.r = poly1.data[i].clone();
                stemp.canonise = false;
                stemp.canon();
                result = stemp.otimes(&result);
            }
            //    stemp.init(epsilon, e, self.data[i]);
            //     result = otimes(stemp, result);
        }

        Ok(result)

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
#[derive(Clone, PartialEq, Debug)]
struct series<T: Clone> {
    p: Poly<gd<DashNumber<T>>>,
    q: Poly<gd<DashNumber<T>>>,
    r: gd<DashNumber<T>>,
    canonise: bool,
}
impl<T: usefulNum+Copy+ops::Neg<Output=T>> From<(Vec<(T, T)>, Vec<(T, T)>, (T, T))> for series<T> where
usize: TryFrom<T> + TryFrom<DashNumber<T>, Error = TryFromDashNumberError>
{
    fn from(item: (Vec<(T, T)>, Vec<(T, T)>, (T, T))) -> Self {
        let p: Poly<gd<DashNumber<T>>> = item.0.into();
        let q: Poly<gd<DashNumber<T>>> = item.1.into();
        let r: gd<DashNumber<T>> = gd::from(item.2);
        let mut s=series { p, q, r,canonise:false };
        s.canon();
        s
    }
}


impl<T: Copy + fmt::Display + cmp::PartialEq + Default> fmt::Display for series<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        //println!("you are in format {:?}",self.p.eps);
        if self.isEpsilon() {
            return write!(f, "eps");
        }
        if self.isTop() {
            return write!(f, "T");
        }
        write!(
            f,
            "{}({}){}",
            if self.p.epsNTop != epsortop::eps {
                format!("{}+", self.p)
            } else {
                "".to_owned()
            },
            self.q,
            if self.r != gd::zero() && !self.r.isDegenerate() {
                format!(".[{}]*", self.r)
            } else {
                "".to_owned()
            }
        )
    }
}
impl<T> starable for series<T>
where
    T: Copy
        + Default
        + ops::AddAssign
        + ::num::Integer
        + fmt::Debug
        + ops::Neg<Output = T>
        + TryFrom<u8>
        + TryFrom<usize>,
    usize: TryFrom<T> + TryFrom<DashNumber<T>, Error = TryFromDashNumberError>, // + ops::Add<Output = T>,
{
    type Output = Self;
    fn star(&self) -> Result<Self, String> {
        if !self.canonise {
            return Err(
                "series must be already canonized! before attempting the star operator!"
                    .to_string(),
            );
        }
        if self.isEpsilon() {
            return Ok(Self::from(Poly::from(gd::zero())));
        }
        let s1 = self.clone();
        // Below a trick added to avoid the computation of (s1)* if s1 is already a star Angers 08/04/2019
        let mut temp = s1.otimes(&s1);
        if temp == s1 {
            return Ok(s1);
        }
        let mut result: Self = Self::default();
        // End of the trick Angers 08/04/2019
        let monome: gd<DashNumber<T>> = gd::zero();

        result.q = s1.q.oplus(&Poly::from(self.r)); //(q+r)
                                                    //cout<<" before (q+r)*"<<endl;
        result.q.simplify();

        let mut result = result.q.star()?; //.expect("result.q couldn't be used to create star");    // (q+r)*

        //cout<<" after (q+r)*"<<endl;

        //println!("{:?}", s1.clone());
        result = result
            .otimes(&series::from(s1.q))
            .oplus(&series::from(Poly::from(monome))); // e + q .(q+r)*
                                                       //cout<<" after (e+q(q+r)*"<<endl;
                                                       //println!("HUH!?2");
        temp = s1.p.star()?; // p*
                             //println!("HUH!?3");
                             //cout<<" after p*"<<endl;

        result = temp.otimes(&result);
        //cout<<" after p*(e+q(q+r)*"<<endl;
        Ok(result)
    }
}

impl<T: Clone> series<T> {
    fn isTop(&self) -> bool {
        self.q.epsNTop == epsortop::top
    }
    fn isEpsilon(&self) -> bool {
        self.p.epsNTop == epsortop::eps && self.q.epsNTop == epsortop::eps
    }
}

impl<T: ops::Neg<Output = T> + Copy + Default + ::num::Integer + std::ops::AddAssign> series<T>
where
    usize: TryFrom<DashNumber<T>, Error = TryFromDashNumberError> + TryFrom<T>,
    T: TryFrom<u8>
        + TryFrom<usize>
        + fmt::Debug
        + // <usize as TryFrom<T>>::Error: fmt::Debug,
        ops::Mul<Output = T>
        + Copy
        + ::num::Zero
        + PartialOrd, //  <usize as TryFrom<DashNumber<T>>>::Error: TryFromDashNumberError
{
    fn otimes(&self, s2: &Self) -> Self {
        ////println!("series otimes");
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

        result.q = self.p.otimes(&s2.q); // p1 q2
        result.r = s2.r; // r2
        result.canon();
        let mut temp1 = series {
            q: s2.p.otimes(&self.q), // p2 q1
            p: Poly::<_>::from(gd::epsilon()),
            r: self.r, // r1
            canonise: false,
        };
        //let mut whatthefuck=temp1.clone();
        temp1.canon();
        /*if !temp1.p.simple {
            whatthefuck.canon();
        }*/
        assert!(temp1.p.simple, "temp1 must be simple");
        result = result.oplus(&temp1);

        let ads1: &Self;
        let ads2: &Self;

        //println!("THIS HAS BEEN RUN a");
        //***** Processing q1, q2, r1*, r2* *****//
        temp1.canonise = false;
        temp1.q = self.q.otimes(&s2.q); // q1 * q2
                                        //********* r1 * r2 * *******************//
                                        /**** Degenerated cases *******/
        if <DashNumber<T> as ::num::Zero>::is_zero(&self.r.d)
            && <DashNumber<T> as ::num::Zero>::is_zero(&s2.r.d)
        {
            //???
            temp1.canon();
            result = result.oplus(&temp1);
            return result;
        }

        //println!("THIS HAS BEEN RUN b");
        if (<DashNumber<T> as ::num::Zero>::is_zero(&self.r.g) && self.r.d == Infinity)
            || (<DashNumber<T> as ::num::Zero>::is_zero(&s2.r.g) && s2.r.d == Infinity)
        {
            temp1.p = Poly::from(gd::from((Infinity, NegInfinity)));
            temp1.r = (T::zero().into(), Infinity).into();
            temp1.canon();
            //println!("result.simple {} temp1 {}", result.canonise, temp1.canonise);
            result = result.oplus(&temp1);
            return result;
        }

        //println!("THIS HAS BEEN RUN c");
        let (ads1, ads2) = if <DashNumber<T> as ::num::Zero>::is_zero(&self.r.d)
            && <DashNumber<T> as ::num::Zero>::is_zero(&self.r.g)
            && !<DashNumber<T> as ::num::Zero>::is_zero(&s2.r.g)
            && !<DashNumber<T> as ::num::Zero>::is_zero(&s2.r.d)
            && s2.r.d != Infinity
        {
            // Symmetrical treatment after inversion
            (&s2, &self)
        } else {
            (&self, &s2)
        };
        //println!("THIS HAS BEEN RUN d");
        if <DashNumber<T> as ::num::Zero>::is_zero(&ads2.r.d)
            && <DashNumber<T> as ::num::Zero>::is_zero(&ads2.r.g)
            && !<DashNumber<T> as ::num::Zero>::is_zero(&ads1.r.g)
            && !<DashNumber<T> as ::num::Zero>::is_zero(&ads1.r.d)
            && ads1.r.d != Infinity
        {
            temp1.r = ads1.r;
            temp1.p = Poly::from(gd::from((Infinity, NegInfinity)));
            temp1.canon();
            result = result.oplus(&temp1);

            return result;
        }
        //the non-degenerate case
        //println!("THIS HAS BEEN RUN");

        let s1g = get_number!(self.r.g);
        let s1d = get_number!(self.r.d);
        let s2g = get_number!(s2.r.g);
        let s2d = get_number!(s2.r.d);
        let pente1 = ::num::rational::Ratio::new_raw(s1g, s1d);
        let pente2 = ::num::rational::Ratio::new_raw(s2g, s2d);
        let mut p1: Poly<gd<DashNumber<T>>> = Poly::default();
        if pente1 == pente2 {
            // pente identique
            let k1 = ::num::integer::gcd(s1g, s2g);
            let k2 = ::num::integer::gcd(s1d, s2d);
            temp1.r = (k1, k2).into();
            let tau = ::num::rational::Ratio::new_raw(k1, k2);
            let k1 = ::num::rational::Ratio::new_raw((s1g - k1) * (s2g - k1), k1).to_integer();
            let k2 = ::num::rational::Ratio::new_raw((s1d - k2) * (s2d - k2), k2).to_integer();
            let mut i = T::zero();
            let mut j = T::zero();
            let mut teta = T::zero();
            while teta < k2 {
                while teta < k2 {
                    let monome = ((tau * teta).to_integer(), teta).into();
                    p1.data.push(monome);
                    j += T::one();
                    teta = i * s1d + j * s2d;
                }
                i += T::one();
                j = T::zero();
                teta = i * s1d + j * s2d;
            }

            p1.simplify(); // transient of r1* . r*

            temp1.p = p1.otimes(&temp1.q); // q1*q2 * transient of r1*.r2*
            let monome: gd<DashNumber<T>> = (k1, k2).into();
            temp1.q = temp1.q.otimes(&Poly::from(monome)); // q1*q2* motif of r1*.r2*

            temp1.canon();
        } else {
            let (ads1, ads2) = if pente1 > pente2 {
                (&s2, &self)
            } else {
                (&self, &s2)
            };
            let s1g = get_number!(ads1.r.g);
            let s1d = get_number!(ads1.r.d);
            let s2g = get_number!(ads2.r.g);
            let s2d = get_number!(ads2.r.d);

            let k1 = s1g * s1d;
            let k2 = s1d * s2g - s1g * s2d;
            let mut k1 = core::cmp::max(
                ::num::rational::Ratio::new_raw(k1, k2).to_integer() + T::one(),
                T::zero(),
            );
            let mut a = ::num::rational::Ratio::new_raw(k1 * s2g, s1g).to_integer();
            let mut test1 = s1d * a;
            let mut test2 = s2d * k1;
            while test1 >= test2 && k1 > T::zero() {
                k1 += -T::one();
                a = ::num::rational::Ratio::new_raw(k1 * s2g, s1g).to_integer();
                test1 = s1d * a;
                test2 = s2d * k1;
            }

            k1 += T::one();
            let mut q1: Poly<gd<DashNumber<T>>> = gd::zero().into();
            for j in 1..usize::try_from(k1).ok().unwrap() {
                let monome: gd<DashNumber<T>> = (
                    s2g * T::try_from(j).ok().unwrap(),
                    s2d * T::try_from(j).ok().unwrap(),
                )
                    .into();
                q1.data.push(monome);
            }

            temp1.q = temp1.q.otimes(&q1);
            temp1.p = Poly::from(gd::from((Infinity, NegInfinity)));
            temp1.r = ads1.r;
            temp1.canon();
        }
        result = result.oplus(&temp1);
        result.canonise = false;
        result.canon();
        result
    }
    fn oplus(&self, s2: &Self) -> Self {
        use DashNumber::*;
        //println!("series oplus self is canonise {:?} {:?}",self.canonise,s2.canonise);
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
        //println!("SERIES OPLUS A");
        //** if one of the series is equal to epsilon
        if self.q.data[0].g == Infinity {
            return s2.clone();
        }
        if s2.q.data[0].g == Infinity {
            return self.clone();
        }
        //println!("SERIES OPLUS E");
        //test the degenerate case
        if <DashNumber<T> as ::num::Zero>::is_zero(&self.r.d)
            && <DashNumber<T> as ::num::Zero>::is_zero(&s2.r.d)
        {
            let temp1 = s2.p.oplus(&s2.q);
            let temp2 = self.q.oplus(&temp1);
            let temp3 = self.p.oplus(&temp2);
            let ayy = Self::from(temp3);
            return ayy;
        }
        let mut ads2 = &s2;
        let mut ads1 = &self;
        //println!("SERIES OPLUS c");
        if <DashNumber<T> as ::num::Zero>::is_zero(&self.r.d)
            && !<DashNumber<T> as ::num::Zero>::is_zero(&s2.r.d)
        {
            // Swap s1 and s2
            ads2 = &self;
            ads1 = &s2;
        }
        //println!("SERIES OPLUS D");
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
        //println!("SERIES OPLUS B");
        if !<DashNumber<T> as ::num::Zero>::is_zero(&self.r.d)
            && !<DashNumber<T> as ::num::Zero>::is_zero(&s2.r.d)
            && <DashNumber<T> as ::num::Zero>::is_zero(&self.r.g)
            && <DashNumber<T> as ::num::Zero>::is_zero(&s2.r.g)
        {
            // p = p1 + p2 + q1 * (0, Infinity) + q2 * (o, Infinity)
            let monome: gd<DashNumber<T>> = (self.q.data[0].g, Infinity).into(); // normally not necessary
                                                                                 //  result.p = monome;
            let inter = self.p.oplus(&s2.p);
            result.p = Poly::from(monome).oplus(&inter);
            result.p.simplify();
            let monome: gd<_> = (s2.q.data[0].g, Infinity).into(); // normally not necessary
            result.p = Poly::from(monome).oplus(&result.p);
            result.p.simplify();
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
        //the non degenerate case
        //(this means there shouldn't be infinities so it should be safe to extract numbers)
        //
        let s1g = get_number!(self.r.g);
        let s1d = get_number!(self.r.d);
        let s2g = get_number!(s2.r.g);
        let s2d = get_number!(s2.r.d);
        let pente1 = ::num::rational::Ratio::new_raw(s1g, s1d);
        let pente2 = ::num::rational::Ratio::new_raw(s2g, s2d);
        if pente1 == pente2 {
            let p = self.p.oplus(&s2.p);
            let g = ::num::integer::lcm(s1g, s2g);
            let r: gd<DashNumber<T>> = (g, ::num::integer::lcm(s1d, s2d)).into();
            let k1 = g / s1g;
            let k2 = g / s2g;
            let mut q = self.q.clone();
            //error[E0277]: cannot multiply `DashNumber<T>` by `{integer}`
            for i in 1..DashNumber::Number(k1).try_into().unwrap() {
                for j in 0..self.q.data.len() {
                    let mut monome: gd<DashNumber<T>> = (
                        self.r.g * T::try_from(i).ok().unwrap(),
                        self.r.d * T::try_from(i).ok().unwrap(),
                    )
                        .into();
                    monome = monome.otimes(&self.q.data[j]);
                    q.data.push(monome);
                }
            }

            for i in 0..DashNumber::Number(k2).try_into().unwrap() {
                for j in 0..s2.q.data.len() {
                    let monome: gd<DashNumber<T>> = (
                        s2.q.data[j].g + s2.r.g * T::try_from(i).ok().unwrap(),
                        s2.q.data[j].d + s2.r.d * T::try_from(i).ok().unwrap(),
                    )
                        .into();
                    q.data.push(monome);
                }
            }
            result.p = p;
            result.q = q;
            result.r = r;
            result.canonise = false;
            result.canon();
        } else {
            // series of different slopes
            // The slope of r2 must be lower than that of r1
            // if this is not the case, we swap the 2 series
            //
            let (ads1, ads2) = if pente1 > pente2 {
                (&s2, &self)
            } else {
                (&self, &s2)
            };

            // domination lemma
            let t2 = ads2.q.data[ads2.q.data.len() - 1].d;
            let k1 = ads1.r.g * (t2 - ads1.q.data[0].d + ads1.r.d)
                + ads1.r.d * (ads1.q.data[0].g - ads2.q.data[0].g);
            let k2 = ads1.r.d * ads2.r.g - ads1.r.g * ads2.r.d;
            let k = cmp::max(
                cmp::max(T::zero(), get_number!((k1 / k2).ceil())),
                get_number!(((ads1.q.data[0].g - ads2.q.data[0].g) / ads2.r.g).ceil()),
            );
            // initialize p = p1 + p2

            let mut p = ads1.p.oplus(&ads2.p);
            for i in 0..DashNumber::Number(k).try_into().unwrap() {
                for j in 0..ads2.q.data.len() {
                    let monome: gd<DashNumber<T>> = (
                        ads2.q.data[j].g + ads2.r.g * T::try_from(i).ok().unwrap(),
                        ads2.q.data[j].d + ads2.r.d * T::try_from(i).ok().unwrap(),
                    )
                        .into();
                    p.data.push(monome);
                }
            }
            // Calculate the polynomial q and the monome r
            //let q = ads1.q;
            //let r = ads1.r;
            result.q = ads1.q.clone();
            result.r = ads1.r.clone();
            result.p = p;
            result.canonise = false;
            result.canon();
        }
        result.canon();
        result
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
            self.q = self.p.pop().into(); // and remove it from p
            self.r = zero;
            self.canonise = true;
            return;
        }
        // Case where p or q is a trajectory
        if self.q.data.last().unwrap().d == Infinity || self.p.data.last().unwrap().d == Infinity {
            let mut res = self.p.oplus(&self.q);
            self.q = res.pop().into(); // Set the last point of p to q
            self.p = res;
            self.p.simplify();
            self.r = (T::default().into(), Infinity).into();
            self.canonise = true;
            return;
        }
        //The degenerate case
        // case where r.g is Infinity or r.d is 0
        if self.r.g == Infinity || self.r.d == DashNumber::default() {
            self.p = self.p.oplus(&self.q);
            self.q = self.p.pop().into(); // and remove it from p
            self.p.simplify();
            self.r = gd::zero();
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
                self.p.pop(); // remove the last element of the polynomial if there are 2
            }
            self.r = (T::default().into(), Infinity).into();
            self.p.simplify();
            self.canonise = true;
            return;
        }

        // case where r.d is greater than 0 and r.g is 0
        if self.r.d > DashNumber::default() && self.r.g == DashNumber::default() {
            self.r = (T::default().into(), Infinity).into();
            self.q = self.q.data[0].otimes(&self.r).into();
            self.p = self.p.oplus(&self.q);
            self.p.pop();
            self.p.simplify();
            self.canonise = true;
            return;
        }
        //The non-degenerate case
        // Putting the periodic in proper form
        // that is, the pattern fits within the periodic
        //We remove the roots of the polynomial q that would be dominated by others after development
        let mut filter = vec![];
        for j in (1..self.q.data.len()).rev() {
            for i in 0..j {
                let k: DashNumber<T> = ((self.q.data[j].g - self.q.data[i].g) / self.r.g).round(); // round down
                if k >= <DashNumber<T> as ::num::One>::one()
                    && (self.q.data[i].d + k * self.r.d) >= self.q.data[j].d
                {
                    filter.push(j);
                    //self.q.popj(j);
                    break;
                }
            }
        }
        self.q.data = self
            .q
            .data
            .clone()
            .into_iter()
            .enumerate()
            .filter(|(a, b)| !filter.contains(a))
            .map(|(_, e)| e)
            .collect();
        self.q.simple = false;
        self.q.simplify();
        let j = self.q.data.len() - 1; // index of last element
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

        while indice >= T::try_from(2u8).ok().unwrap() && self.q.data.len() > 1 {
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
                for i in 1..usize::try_from(indice).ok().unwrap() {
                    let nutau: gd<DashNumber<T>> = (
                        nu * T::try_from(i).ok().unwrap(),
                        tau * T::try_from(i).ok().unwrap(),
                    )
                        .into();
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
                //Problematic
                //FIXME: <= operator unknown in etvo..
                if self.p.data[i] <= self.q.data[j] && self.p.data[i] != epsilon {
                    // If so, remove it
                    self.p.pop();
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
            self.p.simplify();
            self.p = temp.oplus(&self.p);
        }
        // Shift the periodic as much as possible onto the transitory
        while self.q.data.last().unwrap() == &self.p.data.last().unwrap().otimes(&self.r) {
            self.q.data.rotate_right(1);
            // Place the last element of p at the head
            self.q.data[0] = self.p.pop();
            /*        for i in (1..self.q.data.len()).rev() {
                      self.q.data[i] = self.q.data[i - 1].clone();
                  }
                  self.q.data[0] = self.p.data.pop().unwrap().clone();
            */
        }
        // The series should now be in canonical form
        self.canonise = true;
        self.q.simple = false;
        self.q.simplify();
        self.p.simple = false;
        self.p.simplify()
    }
}
#[derive(Debug, Clone)]
struct Matrix<O, I, T>(std::collections::HashMap<(O, I), T>);

impl<O, I, T> cmp::PartialEq for Matrix<O, I, T>
where
    T: cmp::PartialEq,
    O: std::cmp::Eq+hash::Hash+Clone,
    I:std::cmp::Eq+hash::Hash+Clone
{
    fn eq(&self, other: &Self) -> bool {
        if self.0.len() != other.0.len() {
            return false;
        }
        for ((self_o, self_i), self_t) in &self.0 {
            if let Some(other_t) = other.0.get(&(self_o.clone(), self_i.clone())) {
                if self_t != other_t {
                    return false;
                }
            } else {
                return false;
            }
        }
        true
    }
}



#[cfg(not(debug_assertions))]
impl<A, B, T> fmt::Display for Matrix<A, B, T>
where
    T: fmt::Display + fmt::Debug,
    A: fmt::Display,
    B: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        /*    self.data
                            .iter()
                            .map(|x| x.to_string())
                            .collect::<Vec<_>>()
                            .join("+")
        */
        write!(
            f,
            "{}",
            self.0
                .iter()
                .map(|((x, y), v)| format!("[{},{}]={}", x, y, v))
                .collect::<Vec<_>>()
                .join("\n")
        )
    }
}

#[cfg(debug_assertions)]
impl fmt::Display for Matrix<i32, i32, series<i32>> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            self.0
                .iter()
                .map(|((x, y), v)| format!("[{},{}]={}", x, y, v))
                .collect::<Vec<_>>()
                .join("\n")
        )
    }
}
fn inner<O, Intermediate, I, T>(
    a: &Matrix<O, Intermediate, T>,
    b: &Matrix<Intermediate, I, T>,
    f: fn(T, T) -> T,
    g: fn(T, T) -> T,
    zero: T,
) -> Matrix<O, I, T>
where
    O: Eq + Clone + hash::Hash,
    Intermediate: Eq + Clone,
    I: Eq + Clone + hash::Hash,
    T: Clone,
{
    let mut out: Matrix<O, I, T> = Matrix(std::collections::HashMap::new());

    for (k1, v1) in a.0.iter() {
        let (i1, j1) = k1;
        for (k2, v2) in b.0.iter() {
            let (i2, j2) = k2;
            if j1 == i2 {
                //println!("Here stop?");
                let v = f(v1.clone(), v2.clone());
                let out_value = out
                    .0
                    .entry((i1.clone(), j2.clone()))
                    .or_insert(zero.clone());
                *out_value = g(v, out_value.clone());
                // println!("{?:}",out_value);
            }
        }
    }
    out
}

impl<O, I, T> Matrix<O, I, T>
where
    O: Eq + Clone + hash::Hash,
    I: Eq + Clone + hash::Hash,
    T: Clone,
{
    fn map<T2>(&self, f: fn(&T) -> T2) -> Matrix<O, I, T2> {
        let new_map = self
            .0
            .iter()
            .map(|((o, i), v)| ((o.clone(), i.clone()), f(v)))
            .collect();
        Matrix(new_map)
    }
    fn transpose(&self) -> Matrix<I, O, T> {
        let mut new_map = std::collections::HashMap::new();
        ////
        for ((o, i), v) in &self.0 {
            new_map.insert((i.clone(), o.clone()), v.clone());
        }

        Matrix(new_map)
    }
}
/* can't be used because it conflicts with other implementation.
 *
 * impl<O, I, T,T2,T3> ops::Add<Matrix<O,I,T2>> for Matrix<O, I, T>
where
    O: Eq + std::hash::Hash + Clone,
    I: Eq + std::hash::Hash + Clone,
    T: ops::Add<T2,Output = T3> + Clone+::num::Zero,
    T2: ::num::Zero+Clone
{
    type Output = Matrix<O, I, T3>;

    fn add(self, other: Matrix<O, I, T2>) -> Matrix<O, I, T3> {
        let keys: std::collections::HashSet<&(O, I)> = self.0.keys().chain(other.0.keys()).collect();

        let mut new_map:std::collections::HashMap<(O, I), T3> = std::collections::HashMap::new();

        for key in keys.iter() {
            let value = match (self.0.get(key), other.0.get(key)) {
                (Some(v1), Some(v2)) => v1.to_owned() + (v2.to_owned()),
                (Some(v1), None) => v1.clone()+T2::zero(),
                (None, Some(v2)) => T::zero()+v2.clone().to_owned(),
                (None, None) => unreachable!(),
            };
            new_map.insert(key.clone().to_owned(), value);
        }

        Matrix(new_map)
    }
}*/
impl<O, I, T> ops::Add for &Matrix<O, I, series<T>>
where
    O: Eq + std::hash::Hash + Clone,
    I: Eq + std::hash::Hash + Clone,
    usize: TryFrom<DashNumber<T>, Error = TryFromDashNumberError> + TryFrom<T>,
    T: TryFrom<u8>
        + TryFrom<usize>
        + fmt::Debug
        + Copy
        + ::num::Integer
        + Default
        + ops::Neg<Output = T>
        + ops::AddAssign<T>,
{
    type Output = Matrix<O, I, series<T>>;

    fn add(self, other: &Matrix<O, I, series<T>>) -> Matrix<O, I, series<T>> {
        let keys: std::collections::HashSet<&(O, I)> =
            self.0.keys().chain(other.0.keys()).collect();

        let mut new_map: std::collections::HashMap<(O, I), series<T>> =
            std::collections::HashMap::new();

        for key in keys.iter() {
            let value = match (self.0.get(key), other.0.get(key)) {
                (Some(v1), Some(v2)) => v1.oplus(&v2),
                (Some(v1), None) => v1.clone(),
                (None, Some(v2)) => v2.clone(),
                (None, None) => unreachable!(),
            };
            new_map.insert(key.clone().to_owned(), value);
        }

        Matrix(new_map)
    }
}

impl<O, It, I, T> ops::Mul<&Matrix<It, I, series<T>>> for &Matrix<O, It, series<T>>
where
    usize: TryFrom<DashNumber<T>, Error = TryFromDashNumberError> + TryFrom<T>,
    T: TryFrom<u8>
        + TryFrom<usize>
        + fmt::Debug
        + Copy
        + ::num::Integer
        + Default
        + ops::Neg<Output = T>
        + ops::AddAssign<T>,
    O: Eq + Clone + hash::Hash,
    I: hash::Hash + Eq + Clone,
    It: Clone + Eq + hash::Hash,
{
    type Output = Matrix<O, I, series<T>>;
    fn mul(self, other: &Matrix<It, I, series<T>>) -> Self::Output {
        inner(
            &self,
            &other,
            |a, b| a.otimes(&b),
            |a, b| a.oplus(&b),
            series::from(Poly::Epsilon()),
        )
    }
}

//#[cfg(not(debug_assertions))]
impl<V, T> starable for Matrix<V, V, series<T>>
where
    V: Eq + Clone + hash::Hash + fmt::Debug,
    series<T>: starable<Output = series<T>> + Clone,
    //+fmt::Display,
    //TEMPORARY
    //std::collections::hash_set::IntoIter<&V>: Clone,
    usize: TryFrom<DashNumber<T>, Error = TryFromDashNumberError> + TryFrom<T>,
    T: TryFrom<u8>
        + TryFrom<usize>
        + fmt::Debug
        // + fmt::Display//TEMPORARY
        + Copy
        + ::num::Integer
        + Default
        + ops::Neg<Output = T>
        + ops::AddAssign<T>,
{
    type Output = Self;
    fn star(&self) -> Result<Self, String> {
        let mut id = std::collections::HashMap::new();
        let default = series::from(Poly::from(gd::zero())); // do this once. btw what's up with the casing on these names?
        for ((x, y), v) in &self.0 {
            if x == y {
                // on the diagonal: use v
                let xy = (x.clone(), y.clone());
                id.insert(xy, default.clone());
            } else {
                // off the diagonal: "project" onto the diagonal and populate with a default value
                let xx = (x.clone(), x.clone());
                id.insert(xx, default.clone());
                let yy = (y.clone(), y.clone());
                id.insert(yy, default.clone());
            }
        }
        let k: std::collections::HashSet<&V> =
            self.0.keys().flat_map(|(x, y)| vec![x, y]).collect();
        //let scanon=self.map(|s| {let mut a=s.clone();a.canon();a});
        let mut multi: Matrix<V, V, series<T>> = self.clone();
        //println!("IT BEGINS!");
        // let mut o: Matrix<V, V, series<T>> = &Matrix(m) + &multi;
        let eps: series<T> = series::from(Poly::Epsilon());
        let k: Vec<&V> = k.clone().into_iter().collect();
        for key in k.clone().into_iter() {
            let mut newM = std::collections::HashMap::new();
            let akkstar = multi
                .0
                .get(&(key.clone(), key.clone()))
                .unwrap_or(&eps)
                .star()?;

            use itertools::iproduct;
            for (i, j) in iproduct!(k.clone(), k.clone()) {
                let aij = multi.0.get(&(i.clone(), j.clone())).unwrap_or(&eps);
                let aik = multi.0.get(&(i.clone(), key.clone())).unwrap_or(&eps);
                let akj = multi.0.get(&(key.clone(), j.clone())).unwrap_or(&eps);
                let ans = aij.oplus(&aik.otimes(&akkstar.otimes(akj)));
                if ans != eps {
                    newM.insert((i.clone(), j.clone()), ans);
                }
            }

            multi = Matrix(newM);
            //println!("ITERATION next multi: {}",multi);
        }
        Ok(&multi + &Matrix(id))
    }
}
/*#[cfg(debug_assertions)]
impl starable for Matrix<i32, i32, series<i32>> {
    type Output = Self;
    fn star(&self) -> Result<Self, String> {
        let mut id = std::collections::HashMap::new();
        let default = series::from(Poly::from(gd::zero())); // do this once. btw what's up with the casing on these names?
        for ((x, y), v) in &self.0 {
            if x == y {
                // on the diagonal: use v
                let xy = (x.clone(), y.clone());
                id.insert(xy, default.clone());
            } else {
                // off the diagonal: "project" onto the diagonal and populate with a default value
                let xx = (x.clone(), x.clone());
                id.insert(xx, default.clone());
                let yy = (y.clone(), y.clone());
                id.insert(yy, default.clone());
            }
        }
        let k: std::collections::HashSet<&i32> =
            self.0.keys().flat_map(|(x, y)| vec![x, y]).collect();
        //let scanon=self.map(|s| {let mut a=s.clone();a.canon();a});
        let mut multi: Matrix<i32, i32, series<i32>> = self.clone();
        //println!("IT BEGINS!");
        // let mut o: Matrix<V, V, series<T>> = &Matrix(m) + &multi;
        let k: Vec<&i32> = k.clone().into_iter().collect();
        let eps: series<i32> = series::from(Poly::Epsilon());
        for key in k.clone().into_iter() {
            let akkstar = multi
                .0
                .get(&(key.clone(), key.clone()))
                .unwrap_or(&eps)
                .star()?;
            let mut newM = std::collections::HashMap::new();
            use itertools::iproduct;
            for (i, j) in iproduct!(k.clone(), k.clone()) {
                let aij = multi.0.get(&(i.clone(), j.clone())).unwrap_or(&eps);
                let aik = multi.0.get(&(i.clone(), key.clone())).unwrap_or(&eps);
                let akj = multi.0.get(&(key.clone(), j.clone())).unwrap_or(&eps);
                let ans = aij.oplus(&aik.otimes(&akkstar.otimes(akj)));
                if ans != eps {
                    newM.insert((i.clone(), j.clone()), ans);
                }
            }

            multi = Matrix(newM);
            //println!("ITERATION next multi: {}",multi);
        }
        Ok(&multi + &Matrix(id))
    }
    
}*/

/*fn star(&self) -> Result<Self, String> {
        let mut m = std::collections::HashMap::new();
        let default = series::from(Poly::from(gd::zero())); // do this once. btw what's up with the casing on these names?
        for ((x, y), v) in &self.0 {
            if x == y {
                // on the diagonal: use v
                let xy = (x.clone(), y.clone());
                m.insert(xy, v.star()?);
            } else {
                // off the diagonal: "project" onto the diagonal and populate with a default value
                let xx = (x.clone(), x.clone());
                m.entry(xx).or_insert_with(|| default.clone());
                let yy = (y.clone(), y.clone());
                m.entry(yy).or_insert_with(|| default.clone());
            }
        }
        let k: std::collections::HashSet<&i32> =
            self.0.keys().flat_map(|(x, y)| vec![x, y]).collect();
        //let scanon=self.map(|s| {let mut a=s.clone();a.canon();a});
        let mut multi = self.clone();
        //println!("IT BEGINS!");
        let mut o = &Matrix(m) + &multi;

        for key in k.iter() {
            let old=multi.clone();
            multi = &multi * self;
            /*if multi.0.get(&(1,3)).is_some()&&multi.0.get(&(1,3)).unwrap().r.d==DashNumber::Infinity {
                println!("[1,3] d=infinity?\nmultiplication of these is fucked up {:?} \n\n next: {} \n new multi {} \n END",old,self,multi);
            }*/
            o = &o + &multi;

        }
        Ok(o)
    }*/
/*impl<O,I,T> matrix<O,I,T>{
fn map<T2>(f:fn(T)->T2)->matrix<O,I,T2>{
todo!();
}

}*/

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
        let q = item.pop();
        if item.data.len() == 0 {
            item.data.push(gd::epsilon());
        }
        assert!(item.data.len() != 0, "this should be prevented?");
        item.simple = false;
        item.simplify();
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
        let test: DashNumber<i32> = 3.into();
        let test2: DashNumber<i32> = 10.into();
        /*[0,3]=(g0.d2)
        [0,4]=(g1.d0)
        [1,0]=(g0.d3)
        [1,5]=(g1.d0)
        [2,6]=(g1.d0)
        [3,2]=(g0.d1)
        [3,5]=(g1.d0)
        [3,7]=(g1.d0)
        [4,0]=(g0.d0)
        [4,5]=(g1.d10)
        [4,6]=(g1.d0)
        [4,7]=(g0.d2)
        [5,1]=(g0.d0)
        [5,4]=(g0.d3)
        [6,1]=(g0.d10)
        [6,2]=(g0.d0)
        [7,3]=(g0.d0)
        [7,6]=(g0.d1)*/

        //       let mut k = collections::HashMap::new();
        /*println!("huh {} end",Matrix(collections::HashMap::from(vec![((1,3),(0,5))
        ,((0,2),(0,3))
        ,((3,6),(1,1))
        ,((1,4),(1,3))].iter().map(|(a,b)| (a,series::from(Poly::from(gd::from(b))))).collect()))*Matrix(collections::HashMap::from(vec![((3,2),(0,1))
        ,((0,3),(0,2))
        ,((2,6),(1,0))
        ,((1,0),(0,3))
        ,((1,5),(1,0))
        ,((0,4),(1,0))].iter().map(|(a,b)| (a,series::from(Poly::from(gd::from(b))))).collect())));*/
        //
        /*let wat=Matrix(vec![
            ((1,3),(0,5))
            ,((0,2),(0,3))
            ,((3,6),(1,1))
            ,((1,4),(1,3))].into_iter().map(|(a,b)| (a,series::from(Poly::from(gd::from(b))))).collect::<collections::HashMap<(i32,i32),series<i32>>>());
        let dwat=Matrix(vec![((3,2),(0,1))
        ,((0,3),(0,2))
        ,((2,6),(1,0))
        ,((1,0),(0,3))
        ,((1,5),(1,0))
        ,((0,4),(1,0))].into_iter().map(|(a,b)| (a,series::from(Poly::from(gd::from(b))))).collect::<collections::HashMap<(i32,i32),series<i32>>>());
        //println!("time to explore wat:{:?},\nhuh {} end",wat,&wat * &dwat );*/
        ::gdmacro::noideawhatimdoing!(g0.d3+g0.d2);
        let a: Matrix<i32, i32, series<i32>> = Matrix(
            vec![
                ((0, 3), (0, 2)),
                ((0, 4), (1, 0)),
                ((1, 0), (0, 3)),
                ((1, 5), (1, 0)),
                ((2, 6), (1, 0)),
                ((3, 2), (0, 1)),
                ((3, 5), (1, 0)),
                ((3, 7), (1, 0)),
                ((4, 0), (0, 0)),
                ((4, 5), (1, 10)),
                ((4, 6), (1, 0)),
                ((4, 7), (0, 2)),
                ((5, 1), (0, 0)), //Weird behaviour up to here
                ((5, 4), (0, 3)),
                ((6, 1), (0, 10)),
                ((6, 2), (0, 0)),
                ((7, 3), (0, 0)),
                ((7, 6), (0, 1)),
            ]
            .into_iter()
            .map(|(a, b)| (a, series::from(Poly::from(gd::from(b)))))
            .collect::<std::collections::HashMap<(i32, i32), series<i32>>>(),
        );
       
        println!("{}", a.star().ok().unwrap());
    }
}

#[cfg(test)]
mod tests {
    use crate::*;
    #[test]
    fn testStarOperatorMatrix() {
        let a: Matrix<i32, i32, series<i32>> = Matrix(
            vec![
                ((4, 5), (1, 10)),
                ((5, 4), (0, 3)),
            ]
            .into_iter()
            .map(|(a, b)| (a, series::from(Poly::from(gd::from(b)))))
            .collect::<std::collections::HashMap<(i32, i32), series<i32>>>(),
        );
        assert_eq!(Matrix::<i32, i32, series<i32>>(
            vec![
                ((4, 4), series{p:Poly::Epsilon(),q:Poly::from(gd::zero()),canonise:true,r:gd::from((1,13))}),
                ((4, 5), series{p:Poly::Epsilon(),q:Poly::from(gd::from((1,10))),canonise:true,r:gd::from((1,13))}),
                ((5, 4), series{p:Poly::Epsilon(),q:Poly::from(gd::from((0,3))),canonise:true,r:gd::from((1,13))}),
                ((5, 5), series{p:Poly::Epsilon(),q:Poly::from(gd::zero()),canonise:true,r:gd::from((1,13))}),
            ]
            .into_iter()
            .collect::<std::collections::HashMap<(i32, i32), series<i32>>>(),
        ),a.star().ok().unwrap())


    }
    #[test]
    fn ordforgd() {
        let z = gd::zero();
        let d13 = gd::from((1, 13));
        //println!("{:?}",gd::<DashNumber<i32>>::from((0, 4)) < gd::from((1, 5)));
        assert!((z > d13), "g0.d0 > g1.d13")
    }
    #[test]
    fn oplus_for_series() {
        let huh = series::<i32> {
            p: Poly::Epsilon(),
            q: Poly::from(gd::from((1, 13))),
            r: gd::from((1, 13)),
            canonise: true,
        };
        let hah = series::from(Poly::from(gd::from((0, 0))));
        //println!("{:?}",huh.star());
        assert_eq!((huh.oplus(&hah)).to_string(), "(g0.d0).[g1.d13]*");
    }
    #[test]
    fn starPolyCheck() {
        let mut huh = Poly::from(vec![gd::from((0, 0)), gd::from((1, 13))]);
        huh.simplify();

        //println!("{:?}",huh.star());
        assert_eq!((huh.star().ok().unwrap()).to_string(), "(g0.d0).[g1.d13]*");
    }
    #[test]
    fn starSeriesCheck() {
        let huh = series::from(Poly::from(gd::from((1, 13))));
        assert_eq!((huh.star().ok().unwrap()).to_string(), "(g0.d0).[g1.d13]*");
    }
    #[test]
    fn multiplySeriesCheck() {
        let s1 = series::<i32> {
            p: Poly::from(gd::from((1, 15))),
            q: Poly::from(gd::from((2, 23))),
            r: gd::zero(),
            canonise: true,
        };
        let s2 = series::<i32> {
            p: Poly::Epsilon(),
            q: Poly::from(gd::from((0, 0))),
            r: gd::from((1, 13)),
            canonise: true,
        };
        assert_eq!((s1.otimes(&s2)).to_string(), "(g1.d15).[g1.d13]*");
    }
    #[test]
    fn seriesOtimesWithStar() {
        let s = series {
            canonise: true,
            p: Poly::Epsilon(),
            q: Poly::from(gd::zero()),
            r: gd::from((5, 4)),
        };
        assert_eq!((s.otimes(&s)).to_string(), "(g0.d0).[g5.d4]*");
    }
    #[test]
    fn addition_and_printing() {
        let s: gd<DashNumber<_>> = (1, 5).into();
        let a = Poly::from(s).oplus(&Poly::from(gd::from((0, 4))));
        assert_eq!(a.to_string(), "g0.d4+g1.d5");
    }

    #[test]
    fn test_series() {
        let a = series::from(Poly::from(gd::from((0, 1))))
            .otimes(&series::from(Poly::from(gd::from((1, 0)))));
        assert_eq!(a.r, gd::zero());
        assert_eq!(a.p.epsNTop, epsortop::eps);
        assert_eq!(a.q.data[0], gd::from((1, 1)));
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
//try|exe "ExecutorShowDetail"|catch|echo "Executor show detail errored"|endtry|sleep 1|exe "wincmd w"|set nonumber|exe "wincmd p"|exe "vertical resize 160"|exe "ExecutorSetCommand"|call feedkeys("cargo run\r")|autocmd BufWritePre *.rs :ExecutorRun
