use gd_core::{
    GdPlaceholder, DashNumberString, PolyPlaceholder, SeriesPlaceholder,
};
use proc_macro::TokenStream;
use proc_macro2::{Delimiter, Group, TokenTree};
use quote::{quote, ToTokens};
use std::collections::VecDeque;
use syn::{Type};
fn parse_arguments(
    input: proc_macro2::TokenStream,
) -> Result<(String, Option<Type>, VecDeque<proc_macro2::TokenStream>), String> {
    let mut iter = input.into_iter();
    let mut first_arg = String::new();
    let mut second_arg: Option<Type> = None;
    let mut place_holders = VecDeque::new();
    while let Some(token) = iter.next() {
        match token {
            TokenTree::Group(group) => {
                let (group_str, group_placeholders) = handle_group(group)?;
                first_arg.push_str(&group_str);
                place_holders.extend(group_placeholders);
            }
            TokenTree::Punct(punct) => {
                if punct.as_char() == ',' {
                    let remaining_tokens: proc_macro2::TokenStream = iter.collect();
                    second_arg =
                        Some(syn::parse2(remaining_tokens).map_err(|_| "Failed to parse type")?);
                    break;
                } else {
                    first_arg.push(punct.as_char());
                }
            }
            TokenTree::Literal(lit) => {
                first_arg.push_str(&lit.to_string());
            }
            TokenTree::Ident(ident) => {
                first_arg.push_str(&ident.to_string());
            }
        }
    }

    Ok((first_arg, second_arg, place_holders))
}
fn handle_group(
    group: Group,
) -> Result<(String, VecDeque<proc_macro2::TokenStream>), String> {
    let delimiter_start = match group.delimiter() {
        Delimiter::Parenthesis => '(',
        Delimiter::Brace => '{',
        Delimiter::Bracket => '[',
        Delimiter::None => ' ',
    };

    let delimiter_end = match group.delimiter() {
        Delimiter::Parenthesis => ')',
        Delimiter::Brace => '}',
        Delimiter::Bracket => ']',
        Delimiter::None => ' ',
    };
    if group.delimiter() == Delimiter::Brace {
        let mut place_holders = VecDeque::new();
        place_holders.push_back(group.to_token_stream());
        Ok(("#".to_string(), place_holders))
    } else {
        let mut content = String::new();
        let mut place_holders = VecDeque::new();
        content.push(delimiter_start);
        for token in group.stream() {
            match token {
                TokenTree::Group(inner_group) => {
                    let (inner_group_str, inner_placeholders) = handle_group(inner_group)?;
                    content.push_str(&inner_group_str);
                    place_holders.extend(inner_placeholders);
                }
                TokenTree::Punct(punct) => {
                    content.push(punct.as_char());
                }
                TokenTree::Literal(lit) => {
                    content.push_str(&lit.to_string());
                }
                TokenTree::Ident(ident) => {
                    content.push_str(&ident.to_string());
                }
            }
        }
        content.push(delimiter_end);
        Ok((content, place_holders))
    }
}
fn dn2(
    a: (
        DashNumberString,
        Option<Type>,
        &mut VecDeque<proc_macro2::TokenStream>,
    ),
) -> proc_macro2::TokenStream {
    let (b, c,  e) = a;
    let mut start = quote! {DashNumber};
    match c.clone() {
        Some(typ) => {
            let asdf: proc_macro2::TokenStream = quote! {::<#typ>};
            start = quote! {
                #start
                #asdf
            }
        }
        None => {}
    };
    if DashNumberString::Placeholder == b {
        return e.pop_front().expect("This is probably a bug with the macro library, a placeholder was expected, but none found.");
    }
    let end: proc_macro2::TokenStream = match b {
        DashNumberString::Infinity => quote!(::Infinity),
        DashNumberString::NegInfinity => quote!(::NegInfinity),
        DashNumberString::Zero => match c {
            Some(typ) => {
                let asdf: proc_macro2::TokenStream =
                    quote! {::Number(<#typ as ::num::Zero>::zero())};
                asdf
            }
            None => {
                quote! {::Number(0)}
            }
        },
        DashNumberString::One => match c {
            Some(typ) => {
                let asdf: proc_macro2::TokenStream =
                    quote! {::Number(<#typ as ::num::One>::one())};
                asdf
            }
            None => {
                quote! {::Number(1)}
            }
        },
        DashNumberString::Number(stri) => {
            let strng: proc_macro2::TokenStream = stri.parse().ok().unwrap();
            quote! {::Number(#strng)}
        }
        DashNumberString::Placeholder => unreachable!(),
    };
    quote! {
        #start
        #end
    }
}

fn poly_string2(
    a: (
        PolyPlaceholder,
        Option<Type>,
        &mut VecDeque<proc_macro2::TokenStream>,
    ),
) -> proc_macro2::TokenStream {
    let (poly_string, typ, vec) = a;
    match poly_string{
   PolyPlaceholder::PolyString(poly_string)=>{ let gd_strings: Vec<proc_macro2::TokenStream> = poly_string
        .0
        .into_iter()
        .map(|gd_string| gd_string2((gd_string, typ.clone(),vec)))
        .collect();

    let gd_strings = quote! {#(#gd_strings),*};
    quote! {
        Poly{epsNTop: Epsortop::NoXtreme,data:vec![#gd_strings],simple:false}
    }},
PolyPlaceholder::Placeholder=>vec.pop_front().expect("This is probably a bug with the macro library, a placeholder was expected, but none found.")

}
}
fn series_string2(
    a: (
        SeriesPlaceholder,
        Option<Type>,
        &mut VecDeque<proc_macro2::TokenStream>,
    ),
) -> proc_macro2::TokenStream {
    let (series_string, typ, vec) = a;
    match series_string{
    SeriesPlaceholder::SeriesString(series_string)=>{
        let p_token_stream = poly_string2((series_string.p.clone(), typ.clone(),vec));
    let q_token_stream = poly_string2((series_string.q.clone(), typ.clone(),vec));
    let r_token_stream = gd_string2((series_string.r, typ,vec));

    quote! {
        Series {
            p: #p_token_stream,
            q: #q_token_stream,
            r: #r_token_stream,
            canonise:false
        }
    }},
    SeriesPlaceholder::Placeholder=>vec.pop_front().expect("This is probably a bug with the macro library, a placeholder was expected, but none found.")

}
}

fn gd_string2(
    a: (
        GdPlaceholder,
        Option<Type>,
        &mut VecDeque<proc_macro2::TokenStream>,
    ),
) -> proc_macro2::TokenStream {
    let (gd_string, typ, vec) = a;
    match gd_string{
    GdPlaceholder::GdString(gd_string)=>{let g_token_stream = dn2((gd_string.g.clone(), typ.clone(),vec));
    let d_token_stream = dn2((gd_string.d.clone(), typ,vec));

    quote! {
        gd {
            g: #g_token_stream,
            d: #d_token_stream,
        }
    }},
     GdPlaceholder::Placeholder=>vec.pop_front().expect("This is probably a bug with the macro library, a placeholder was expected, but none found.")

}
}
macro_rules! define_parse_function {
    ($fn_name:ident, $error_msg:expr, $string2_fn:ident) => {
        fn $fn_name(input: proc_macro2::TokenStream) -> Result<proc_macro2::TokenStream, String> {
            let (first_arg, second_arg, mut tokenstream) = parse_arguments(input)?;
            //println!("first arg is :{}",first_arg);
            let parsed_string = first_arg.parse().map_err(|e| format!($error_msg, e))?;

            Ok($string2_fn((parsed_string, second_arg, &mut tokenstream)))
        }
    };
}

// Define parse functions using the macro
define_parse_function!(dn_parse, "Failed to parse DashNumber: {}", dn2);
define_parse_function!(gd_parse, "Failed to parse gd: {}", gd_string2);
define_parse_function!(poly_parse, "Failed to parse Poly: {}", poly_string2);
define_parse_function!(series_parse, "Failed to parse Series: {}", series_string2);

macro_rules! define_proc_macro {
    ($proc_macro_name:ident, $parse_fn:ident) => {
        #[proc_macro]
        pub fn $proc_macro_name(input: TokenStream) -> TokenStream {
            match $parse_fn(input.into()) {
                Ok(output) => output.into(),
                Err(err) => {
                    let error_message = format!(
                        "Error in {} proc_macro: {}",
                        stringify!($proc_macro_name),
                        err
                    );
                    let error_tokens = quote! { compile_error!(#error_message); };
                    error_tokens.into()
                }
            }
        }
    };
}

// Define proc_macros using the macro
define_proc_macro!(dn, dn_parse);
define_proc_macro!(g_d, gd_parse);
define_proc_macro!(poly, poly_parse);
define_proc_macro!(series, series_parse);
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        /*println!(
            "{:?}",
            {let (a,_,b)=parse_arguments(quote! {g{1+2}.d3+g2.d3+(g5.d3).[g3.d4]*,i32}).ok().unwrap();(a.to_string(),b)}
        );*/
        assert_eq!(
            "Series { p : Poly { epsNTop : epsortop :: NoXtreme , data : vec ! [gd { g : DashNumber :: < T > :: Infinity , d : DashNumber :: < T > :: NegInfinity , }] , simple : false } , q : Poly { epsNTop : epsortop :: NoXtreme , data : vec ! [gd { g : DashNumber :: < T > :: Number (< T as :: num :: One > :: One ()) , d : DashNumber :: < T > :: Infinity , }] , simple : false } , r : gd { g : DashNumber :: < T > :: Number (< T as :: num :: Zero > :: zero ()) , d : DashNumber :: < T > :: Infinity , } , canonise : false }",
            series_parse(quote! {(g1.dINF).[g0.dINF]*,T}).ok().unwrap().to_string()
        );
       // assert!(false);
    }
}
