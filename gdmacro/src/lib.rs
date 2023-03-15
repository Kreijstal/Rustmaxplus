use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, ItemFn};

#[proc_macro]
pub fn hello_world(_input: TokenStream) -> TokenStream {
    // Parse the input tokens into an AST
    let input = parse_macro_input!(_input as ItemFn);

    // Generate the output tokens using the `quote!` macro
    let output = quote! {
        fn hello_world() {
            println!("Hello, world!");
        }
    };

    // Convert the output tokens into a TokenStream
    TokenStream::from(output)
}


#[proc_macro]
pub fn noideawhatimdoing(input: TokenStream) -> TokenStream {
println!("Hello, this is the compiler checking input of noideawhatimdoing {:?}",input);

TokenStream::new()
}



#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        fn example(s: &str)->proc_macro2::TokenStream {
            let stream: proc_macro2::TokenStream = s.parse().unwrap();
            return stream
        }
        println!("excuse me {{:?}} something more {:?}",quote!{")g1.d2"}.to_string());
        println!("EXECUTED{:?}",::gd_core::DashNumber::<i32>::Infinity);
        println!("why are we here just to suffer?");
        assert_eq!(1,2);
    }
}
