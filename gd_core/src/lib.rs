use core::*;
use nom::{
    character::complete::{char, digit1,one_of,none_of,multispace0,multispace1},
    combinator::{cut, map, map_res,map_parser, opt,recognize,peek,verify,not,eof,all_consuming},
    sequence::{pair,preceded,terminated,tuple},
    multi::{separated_list1,many0, many1},
    branch::alt,
    bytes::complete::{tag,take_till1,take_while1,is_not},
    IResult,
    error::{ParseError,VerboseError,context,ErrorKind,Error},
    Parser,
    Needed
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
impl<T> str::FromStr for DashNumber<T> where
T: ParseAndArithmeticOps,
<T as str::FromStr>::Err: fmt::Debug{
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse_dash_number(s) {
            Ok((remaining, value)) => {
                if remaining.is_empty() {
                    Ok(value)
                } else {
                    Err(format!("Unexpected characters remaining: {:?}", remaining))
                }
            }
            Err(err) => Err(format!("Parse error: {:?}", err)),
        }
    }
}



//pub(crate) use get_number; 
//pub(crate) use get_number_opt; 
#[derive(Debug,Eq!,Copy!)]
pub struct gd<T> {
   pub g: T,
   pub d: T,
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
   pub epsNTop: epsortop,
   pub data: Vec<T>,
   pub simple: bool,
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
    pub p: Poly<gd<DashNumber<T>>>,
    pub q: Poly<gd<DashNumber<T>>>,
    pub r: gd<DashNumber<T>>
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
        r
    };

    Ok((input, result))
}
fn parse_series<T>(input: &str) -> IResult<&str, series<T>>
where
    T: ParseAndArithmeticOps+Copy,
    <T as str::FromStr>::Err: fmt::Debug,
{
 
    let parse_zero = map(tag("e"), |_| series::<T>{p:Poly::Epsilon(),q:Poly::zero(),r:gd::zero()});
    let parse_epsilon = map(tag("eps"), |_| series::<T>{p:Poly::Epsilon(),q:Poly::Epsilon(),r:gd::zero()});
    let parse_top = map(tag("T"), |_| series::<T>{p:Poly::Epsilon(),q:Poly::Top(),r:gd::zero()});

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

#[derive(Debug, Eq!, Clone)]
pub enum DashNumberString {
    NegInfinity,
    Number(String),
    Infinity,
    Zero,
    One,
    Placeholder,
}



fn parse_infinity(input: &str) -> IResult<&str, &str, VerboseError<&str>> {
    context("infinity",alt((tag("INF"), tag("∞"))))(input)
}
fn parse_dash_number_string(input: &str) -> IResult<&str, DashNumberString, VerboseError<&str>> {
    nom::branch::alt((
        nom::combinator::map(parse_infinity, |_| DashNumberString::Infinity),
        nom::combinator::map(
            nom::sequence::tuple((char('-'), multispace0, parse_infinity)),
            |_| DashNumberString::NegInfinity,
        ),
        nom::combinator::map(char('#'), |_| DashNumberString::Placeholder),
        nom::combinator::map(terminated(char('1'),not(parse_string_number)), |_| DashNumberString::One),
        nom::combinator::map(terminated(char('0'),not(parse_string_number)), |_| DashNumberString::Zero),
        nom::combinator::map(parse_string_number, DashNumberString::Number),
        
    ))(input)
}


// Match any character except ), +, or ].
fn not_closing(input: &str) -> IResult<&str, char, VerboseError<&str>> {
    context(
        "not_closing",
        none_of(")+]"),
    )(input)
}


// Match dot followed by optional whitespace and a 'd'
fn dot_d(input: &str) -> IResult<&str,  (&str, char, &str, char), VerboseError<&str>> {
    context(
        "dot_d",
        tuple((multispace0, char('.'), multispace0, char('d')))
    )(input)
}
// Match . at the end of the string.
fn dot_end(input: &str) -> IResult<&str, char, VerboseError<&str>> {
    context(
        "dot_end",
        all_consuming(char('.')),
    )(input)
}

// Match space(s) followed by a `.`.
fn space_dot(input: &str) -> IResult<&str, (&str,char), VerboseError<&str>> {
    context(
        "space_dot",
        tuple((multispace1, char('.'))
    )
    )(input)
}

// Match any character except ., ), ] or +.
fn not_special(input: &str) -> IResult<&str, char, VerboseError<&str>> {
    context(
        "not_special",
        none_of(".+])"),
    )(input)
}

//use nom::error::VerboseError;
fn dashnumber_digit(input: &str) -> IResult<&str, char, VerboseError<&str>> {
    let (input, _) = peek(not(dot_d))(input)?;
    let (input, _) = peek(not(dot_d))(input)?;
    let (input, _) = peek(not(space_dot))(input)?;
    let (input, _) = peek(not(dot_end))(input)?;
    let (input, char_match) = none_of(")+]") (input)?;
    
    Ok((input, char_match))
} 

fn parse_string_number(input: &str) -> IResult<&str, String, VerboseError<&str>> {
    map(
        many1(dashnumber_digit),
        |vec| {
            vec.into_iter().collect()
        }
    )(input)
}

#[derive(Debug,Eq!,Clone)]
pub struct GdString {
   pub g: DashNumberString,
   pub d: DashNumberString,
}

#[derive(Debug,Clone,Eq!)]
pub enum gdPlaceholder{
     gdString(GdString),
     Placeholder,
}
impl From<GdString> for gdPlaceholder {
    fn from(item: GdString) -> Self {
        gdPlaceholder::gdString(item)
    }
}
fn parse_gd_placeholder(input: &str) -> IResult<&str, gdPlaceholder, VerboseError<&str>> {
    alt((parse_gd_string_placeholder, parse_placeholder_hash))(input)
}

fn parse_gd_string_placeholder(input: &str) -> IResult<&str, gdPlaceholder, VerboseError<&str>> {
    let (input, gd_string) = parse_gd_string(input)?;
    Ok((input, gdPlaceholder::gdString(gd_string)))
}

fn parse_placeholder_hash(input: &str) -> IResult<&str, gdPlaceholder, VerboseError<&str>> {
    let (input, _) = char('#')(input)?;
    Ok((input, gdPlaceholder::Placeholder))
}

impl From<(String, String)> for GdString {
    fn from(item: (String, String)) -> Self {
        GdString {
            g: DashNumberString::Number(item.0),
            d: DashNumberString::Number(item.1),
        }
    }
}
impl From<(DashNumberString, DashNumberString)> for GdString {
    fn from(item: (DashNumberString, DashNumberString)) -> Self {
        GdString {
            g: item.0,
            d: item.1,
        }
    }
}
fn parse_gd_string2(input: &str) -> IResult<&str, GdString, VerboseError<&str>>
{
    let (input, _) = char('g')(input)?;
    let (input, g) = parse_dash_number_string(input)?;
    let (input,_)=   multispace0(input)?;
    let (input, _) = char('.')(input)?;
    let (input,_)=   multispace0(input)?;
    let (input, _) = char('d')(input)?;
    let (input, d) = parse_dash_number_string(input)?;
    Ok((input, GdString { g, d }))
}
fn parse_gd_string(input: &str) -> IResult<&str, GdString, VerboseError<&str>>
{
 
    let parse_zero = map(one_of("eE"), |_| GdString::zero());
    let parse_epsilon = map(tag("eps"), |_| GdString::epsilon());
    let parse_top = map(tag("T"), |_| GdString::top());

    let parse_special_cases = alt((parse_epsilon,parse_zero, parse_top));

    let (input, result) = alt((
        parse_gd_string2,
        parse_special_cases,
    ))(input)?;

    Ok((input, result))
}
#[derive(Debug, Clone, PartialEq)]
pub struct PolyString(pub Vec<gdPlaceholder>);
impl PolyString{
    fn Top() -> Self {
        GdString::top().into()
    }
    fn Epsilon() -> Self {
        GdString::epsilon().into()
    }
    fn zero() -> Self {
        GdString::zero().into()
    }
}
fn parse_plus_string(input: &str) -> IResult<&str, char, VerboseError<&str>> {
    preceded(multispace0, terminated(char('+'), multispace0))(input)
}

fn parse_poly_numbercase_string(input: &str) -> IResult<&str, PolyString, VerboseError<&str>>
{
    let parse_term = parse_gd_placeholder;
    let parse_separator =parse_plus_string;

    let (input, terms) = many1(preceded(opt(parse_separator), parse_term))(input)?;

    let poly = PolyString(terms);
    Ok((input, poly))
}
fn parse_poly_string(input: &str) -> IResult<&str, PolyString, VerboseError<&str>>
{
 
    let parse_zero = map(tag("e"), |_| PolyString::zero());
    let parse_epsilon = map(tag("eps"), |_| PolyString::Epsilon());
    let parse_top = map(tag("T"), |_| PolyString::Top());

    let parse_special_cases = alt((parse_epsilon,parse_zero, parse_top));

    let (input, result) = alt((
        parse_poly_numbercase_string,
        parse_special_cases,
    ))(input)?;

    Ok((input, result))
}
#[derive(Clone, PartialEq, Debug)]
pub enum PolyPlaceholder{
    PolyString(PolyString),
    Placeholder
}
impl From<PolyString> for PolyPlaceholder {
    fn from(item: PolyString) -> Self {
        PolyPlaceholder::PolyString(item)
    }
}

fn parse_poly_placeholder(input: &str) -> IResult<&str, PolyPlaceholder, VerboseError<&str>> {
    let (input, poly_string) = parse_poly_string(input)?;  
    Ok((input, if poly_string==PolyString(vec![gdPlaceholder::Placeholder]){PolyPlaceholder::Placeholder}else{PolyPlaceholder::PolyString(poly_string)}))
}

impl From<GdString> for PolyString {
    fn from(item: GdString) -> Self {
        PolyString(vec![gdPlaceholder::gdString(item.clone())])
    }
}
impl From<Vec<GdString>> for PolyString {
    fn from(item: Vec<GdString>) -> Self {
        PolyString(item.into_iter().map(|gd| gdPlaceholder::gdString(gd)).collect())
    }
}
impl From<Vec<gdPlaceholder>> for PolyString {
    fn from(item: Vec<gdPlaceholder>) -> Self {
        PolyString(item)
    }
}
impl GdString {
    fn isDegenerate(&self) -> bool {
        use DashNumberString::*;
        match (&self.g, &self.d) {
            (Infinity | NegInfinity, _) => true,
            (_, Infinity | NegInfinity) => true,
            _ => false,
        }
        // self.g == NegInfinity || self.g == Infinity || self.d == NegInfinity || self.d == Infinity
    }
    fn is_epsilon_or_top(&self) -> epsortop {
        use epsortop::*;
        use DashNumberString::*;
        match (&self.g, &self.d) {
            (Infinity, Infinity) | (_, NegInfinity) => eps,
            (NegInfinity, _) => top,
            (_, _) => NoXtreme,
        }
    }
    fn epsilon() -> Self {
        use DashNumberString::*;
        (Infinity, NegInfinity).into()
    }
    fn top() -> Self {
        use DashNumberString::*;
        (NegInfinity, Infinity).into()
    }   
    fn zero() -> Self {
        use DashNumberString::*;
        (Zero, Zero).into()
    }   
}
#[derive(Clone, PartialEq, Debug)]
pub struct SeriesString {
    pub p: PolyPlaceholder,
    pub q: PolyPlaceholder,
    pub r: gdPlaceholder
}
fn parse_series_normal_string(input: &str) -> IResult<&str, SeriesString, VerboseError<&str>>{
    let (input, p) = opt(context("Parsing p", terminated(parse_poly_placeholder,tuple((parse_plus_string,multispace0)))))(input)?;
    let p:PolyPlaceholder = p.unwrap_or(PolyPlaceholder::PolyString(PolyString::Epsilon()));
    let (input, q) = context(
        "Parsing q",
        preceded(
            tuple((char('('), multispace0)),
            parse_poly_placeholder,
        ),
    )(input)?;
    let (input, _) = tuple((multispace0, char(')')))(input)?;
    let (input, r) = opt(context(
        "Parsing r",
        preceded(
            tuple((multispace0,char('.'), multispace0, char('['), multispace0)),
            terminated(parse_gd_placeholder, tuple((char(']'), multispace0, char('*')))),
        ),
    ))(input)?;
    let r:gdPlaceholder = r.unwrap_or(GdString::zero().into());

    let result = SeriesString {
        p,
        q,
        r
    };

    Ok((input, result))
}
fn parse_series_string(input: &str) -> IResult<&str, SeriesString, VerboseError<&str>>
{
 
    let parse_zero = map(tag("e"), |_| SeriesString{p:PolyString::Epsilon().into(),q:PolyString::zero().into(),r:GdString::zero().into()});
    let parse_epsilon = map(tag("eps"), |_| SeriesString{p:PolyString::Epsilon().into(),q:PolyString::Epsilon().into(),r:GdString::zero().into()});
    let parse_top = map(tag("T"), |_| SeriesString{p:PolyString::Epsilon().into(),q:PolyString::Top().into(),r:GdString::zero().into()});

    let parse_special_cases = alt((parse_epsilon,parse_zero, parse_top));
    
    let (input, result) = alt((
        parse_series_normal_string,
        parse_special_cases,
    ))(input)?;

    Ok((input, result))
}
#[derive(Clone, PartialEq, Debug)]
pub enum SeriesPlaceholder {
    SeriesString(SeriesString),
    Placeholder,
}
fn parse_series_placeholder(input: &str) -> IResult<&str, SeriesPlaceholder, VerboseError<&str>> {
    alt((parse_series_string_placeholder, parse_series_placeholder2))(input)
}

fn parse_series_string_placeholder(input: &str) -> IResult<&str, SeriesPlaceholder, VerboseError<&str>> {
    let (input, series_string) = parse_series_string(input)?;
    Ok((input, SeriesPlaceholder::SeriesString(series_string)))
}

fn parse_series_placeholder2(input: &str) -> IResult<&str, SeriesPlaceholder, VerboseError<&str>> {
    let (input, _) = char('#')(input)?;
    Ok((input, SeriesPlaceholder::Placeholder))
}
macro_rules! impl_from_str {
    ($type:ty, $parse_fn:ident) => {
        impl std::str::FromStr for $type {
            type Err = String;

            fn from_str(s: &str) -> Result<Self, Self::Err> {
                match $parse_fn(s) {
                    Ok((remaining, value)) => {
                        if remaining.is_empty() {
                            Ok(value)
                        } else {
                            Err(format!("Unexpected characters remaining: {:?}", remaining))
                        }
                    },
                    Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => Err(nom::error::convert_error(s, e)),
                    Err(nom::Err::Incomplete(_)) => Err(String::from("Incomplete input")),
                }
            }
        }
    };
}

impl_from_str!(SeriesString, parse_series_string);
impl_from_str!(PolyString, parse_poly_string);
impl_from_str!(GdString, parse_gd_string);
impl_from_str!(DashNumberString, parse_dash_number_string);
impl_from_str!(SeriesPlaceholder, parse_series_placeholder);
impl_from_str!(PolyPlaceholder, parse_poly_placeholder);
impl_from_str!(gdPlaceholder, parse_gd_placeholder);
/*impl str::FromStr for DashNumberString {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse_dash_number_string(s) {
            Ok((remaining, value)) => {
                if remaining.is_empty() {
                    Ok(value)
                } else {
                    Err(format!("Unexpected characters remaining: {:?}", remaining))
                }
            }
            Err(err) => Err(format!("Parse error: {:?}", err)),
        }
    }
} */
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn dashnumberstring_parse(){
        println!("{:?}",parse_series_placeholder("(#+#).[#]*"));
//assert!(false)
    }
    #[test]
    fn series_parse() {
        
        let test_cases = vec![
            ("g1.d2+(g2.d4).[g3.d5]*", series::<i32>{q:Poly::from(vec![(2,4)]),p:Poly::from(vec![(1,2)]),r:(3,5).into()}),
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
