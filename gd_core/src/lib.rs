use core::*;
use nom::{
    character::complete::{char, digit1,one_of,multispace0},
    combinator::{map, map_res,map_parser, opt,recognize},
    sequence::{pair,preceded,terminated,tuple},
    multi::{separated_list1,many0, many1},
    branch::alt,
    bytes::complete::tag,
    IResult,
    error::{ParseError,VerboseError,context},
    Parser
};


#[macro_use]
extern crate macro_rules_attribute;
//#[allow(unused_macros)]
macro_rules_attribute::derive_alias! {
    #[derive(Eq!)] = #[derive(Eq, PartialEq)];
    #[derive(Ord!)] = #[derive(Ord, PartialOrd, Eq!)];
    #[derive(Copy!)] = #[derive(Copy, Clone)];
}

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


/*fn decimal(input: &str) -> IResult<&str, &str> {
    recognize(many1(terminated(
        one_of("0123456789"),
        many0(char('_')),
    )))(input)
}
*/
pub trait ParseAndArithmeticOps: ::num::Integer + ops::Neg<Output = Self> + str::FromStr+Clone
{
    // Add any additional functions if needed.
}

impl<T> ParseAndArithmeticOps for T where T: Clone + ::num::Integer + ops::Neg<Output = T> + str::FromStr, T::Err: fmt::Debug {}
fn parse_integer<T>(input: &str) -> IResult<&str, T>
where
        T: ParseAndArithmeticOps,
        <T as str::FromStr>::Err: fmt::Debug
{
    map_res(
        tuple((
            opt(char('-')),
            multispace0,
            digit1,
        )),
        |(sign, _space, num_str): (Option<char>, &str, &str)| -> Result<T, T::Err> {
            let parsed_num = T::from_str(num_str)?;
            Ok(if sign.is_some() { -parsed_num } else { parsed_num })
        },
    )(input)
}

fn parse_dash_number<T>(input: &str) -> IResult<&str, DashNumber<T>>
where
    T: ParseAndArithmeticOps,
    <T as str::FromStr>::Err: fmt::Debug
{
    alt((
        map(char('∞'), |_| DashNumber::Infinity),
        map(tuple((char('-'), multispace0, char('∞'))), |_| DashNumber::NegInfinity),
        map(parse_integer, DashNumber::Number),
    ))(input)
}
/*
fn parse_dash_number(input: &str) -> IResult<&str, DashNumber<i32>> {
    alt((
        map(char('∞'), |_| DashNumber::Infinity),
        map(tuple((char('-'), multispace0, char('∞'))), |_| DashNumber::NegInfinity),
        map(parse_i32, DashNumber::Number),
    ))(input)
}
*/


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

//pub(crate) use get_number; 
//pub(crate) use get_number_opt; 
#[derive(Debug,Eq!,Copy!)]
pub struct gd<T> {
    g: T,
    d: T,
}
impl<T: Clone + fmt::Display> fmt::Display for gd<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "g{}.d{}", self.g, self.d)
    }
}
fn parse_gd<T>(input: &str) -> IResult<&str, gd<DashNumber<T>>>
where
    T: ParseAndArithmeticOps,
    <T as str::FromStr>::Err: fmt::Debug
{
    let (input, _) = char('g')(input)?;
    let (input, g) = parse_dash_number(input)?;
    let (input,_)=   multispace0(input)?;
    let (input, _) = char('.')(input)?;
    let (input,_)=   multispace0(input)?;
    let (input, _) = char('d')(input)?;
    let (input, d) = parse_dash_number(input)?;
    Ok((input, gd { g, d }))
}
impl<T: str::FromStr + Clone> str::FromStr for gd<DashNumber<T>>
where
    T: ParseAndArithmeticOps,
    <T as str::FromStr>::Err: fmt::Debug,
{
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse_gd::<T>(s) {
            Ok((_, gd)) => Ok(gd),
            Err(e) => Err(format!("Failed to parse gd from '{}': {:?}", s, e)),
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
pub enum epsortop {
    eps,
    NoXtreme,
    top,
}
impl<T:Clone> gd<DashNumber<T>> {
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
   
    
}
impl<T: ::num::Zero+Clone> gd<DashNumber<T>> {
     //can't implement num::Zero because it has bounds on Addition
    fn zero() -> Self {
        (T::zero(), T::zero()).into()
    }
}



#[derive(Debug, Clone, PartialEq)]
pub struct Poly<T: Clone> {
    epsNTop: epsortop,
    data: Vec<T>,
    simple: bool,
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
impl<T: Copy + ParseAndArithmeticOps> From<gd<DashNumber<T>>> for Poly<gd<DashNumber<T>>> {
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
impl<T:ParseAndArithmeticOps+Copy> Poly<gd<DashNumber<T>>>{
    fn Top() -> Self {
        gd::top().into()
    }
    fn Epsilon() -> Self {
        gd::epsilon().into()
    }
}
impl<T:Copy+ ParseAndArithmeticOps> Poly<gd<DashNumber<T>>>{
    fn zero() -> Self {
        gd::zero().into()
    }
}
impl<T: Copy + ParseAndArithmeticOps> From<Vec<gd<DashNumber<T>>>> for Poly<gd<DashNumber<T>>> {
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
impl<T: ParseAndArithmeticOps +Copy> From<Vec<(T,T)>> for Poly<gd<DashNumber<T>>> {
    fn from(item: Vec<(T,T)>) -> Self {
        let vec: Vec<gd<DashNumber<T>>> = item.into_iter().map(|x| gd::from(x)).collect();
        vec.into()
    }
}
fn parse_plus(input: &str) -> IResult<&str, char> {
    preceded(multispace0, terminated(char('+'), multispace0))(input)
}

fn parse_poly_numbercase<T>(input: &str) -> IResult<&str, Poly<gd<DashNumber<T>>>>
where
T: ParseAndArithmeticOps+Copy,
<T as str::FromStr>::Err: fmt::Debug,
{
    let parse_term = parse_gd::<T>;
    let parse_separator =parse_plus;

    let (input, terms) = many1(preceded(opt(parse_separator), parse_term))(input)?;

    let poly = Poly::from(terms);
    Ok((input, poly))
}

fn parse_poly<T>(input: &str) -> IResult<&str, Poly<gd<DashNumber<T>>>>
where
    T: ParseAndArithmeticOps+Copy,
    <T as str::FromStr>::Err: fmt::Debug,
{
 
    let parse_zero = map(tag("e"), |_| Poly::zero());
    let parse_epsilon = map(tag("eps"), |_| Poly::Epsilon());
    let parse_top = map(tag("T"), |_| Poly::Top());

    let parse_special_cases = alt((parse_epsilon,parse_zero, parse_top));

    let (input, result) = alt((
        parse_poly_numbercase,
        parse_special_cases,
    ))(input)?;

    Ok((input, result))
}
#[derive(Debug)]
pub struct ParsePolyError;

impl fmt::Display for ParsePolyError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Failed to parse Poly")
    }
}

impl<T> str::FromStr for Poly<gd<DashNumber<T>>>
where
    T: ParseAndArithmeticOps + Copy,
    <T as str::FromStr>::Err: fmt::Debug,
{
    type Err = ParsePolyError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse_poly::<T>(s) {
            Ok((remaining_input, poly)) if remaining_input.trim().is_empty() => Ok(poly),
        _a => {
                Err(ParsePolyError)},
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct series<T: Clone> {
    p: Poly<gd<DashNumber<T>>>,
    q: Poly<gd<DashNumber<T>>>,
    r: gd<DashNumber<T>>,
    canonise: bool,
}

fn parse_series_normal<T>(input: &str) -> IResult<&str, series<T>>
where
T: ParseAndArithmeticOps + Copy,
<T as str::FromStr>::Err: fmt::Debug,
{
    let (input, p) = context("Parsing p", parse_poly::<T>)(input)?;

    let (input, q) = context(
        "Parsing q",
        preceded(
            tuple((parse_plus, char('('), multispace0)),
            parse_poly::<T>,
        ),
    )(input)?;
    let (input, _) = tuple((multispace0, char(')')))(input)?;
    let (input, r) = opt(context(
        "Parsing r",
        preceded(
            tuple((char('.'), multispace0, char('['), multispace0)),
            terminated(parse_gd::<T>, tuple((char(']'), multispace0, char('*')))),
        ),
    ))(input)?;
    let r = r.unwrap_or_else(gd::zero);

    let result = series {
        p,
        q,
        r,
        canonise: false,
    };

    Ok((input, result))
}
fn parse_series<T>(input: &str) -> IResult<&str, series<T>>
where
    T: ParseAndArithmeticOps+Copy,
    <T as str::FromStr>::Err: fmt::Debug,
{
 
    let parse_zero = map(tag("e"), |_| series::<T>{p:Poly::Epsilon(),q:Poly::zero(),r:gd::zero(),canonise:true});
    let parse_epsilon = map(tag("eps"), |_| series::<T>{p:Poly::Epsilon(),q:Poly::Epsilon(),r:gd::zero(),canonise:true});
    let parse_top = map(tag("T"), |_| series::<T>{p:Poly::Epsilon(),q:Poly::Top(),r:gd::zero(),canonise:true});

    let parse_special_cases = alt((parse_epsilon,parse_zero, parse_top));
    
    let (input, result) = alt((
        parse_series_normal,
        parse_special_cases,
    ))(input)?;

    Ok((input, result))
}
impl<T> str::FromStr for series<T>
where
    T: ParseAndArithmeticOps + Copy+fmt::Debug,
    <T as str::FromStr>::Err: fmt::Debug,
{
    type Err = ParsePolyError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse_series::<T>(s) {
            Ok((remaining_input, poly)) if remaining_input.trim().is_empty() => Ok(poly),
            a => {
                println!("{:?}",a);
                Err(ParsePolyError)},
        }
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn series_parse() {
        
        let test_cases = vec![
            ("g1.d2+(g2.d4).[g3.d5]*", series::<i32>{q:Poly::from(vec![(2,4)]),p:Poly::from(vec![(1,2)]),r:(3,5).into(),canonise:false}),
        ];
    
        for (input, expected) in test_cases {
            println!(" ayy {:?} lmao",parse_series_normal::<i32>(input));
            let  parsed_value:series<i32> = input.parse().ok().unwrap();
            assert_eq!(parsed_value, expected);
        }
    }
    #[test]
    fn poly_parse() {
        
        assert_eq!(Poly::<gd<DashNumber<i16>>>::zero(),"e".parse().ok().unwrap());
        let test_cases = vec![
            ("e", Poly::<gd<DashNumber<i32>>>::zero()),
            ("eps", Poly::Epsilon()),
            ("T", Poly::Top()),
            ("g0.d1+g1.d2", Poly::from(vec![(0,1),(1,2)])),
            ("g1 .d2 + g3 . d4", Poly::from(vec![(1,2),(3,4)])),
        ];
    
        for (input, expected) in test_cases {
            let  parsed_value:Poly::<gd<DashNumber<i32>>> = input.parse().ok().unwrap();
            assert_eq!(parsed_value, expected);
        }
    }
    #[test]
    fn gd_parse() {
        assert_eq!(("g3  .  d2").parse::<gd<DashNumber<i32>>>().ok().unwrap(),(3,2).into());
        assert_eq!(("g3.d2").parse::<gd<DashNumber<i32>>>().ok().unwrap(),(3,2).into());
    }
}
