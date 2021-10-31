mod kirl_function;

use proc_macro::TokenStream;

#[proc_macro_attribute]
pub fn kirl_function(args: TokenStream, input: TokenStream) -> TokenStream {
    kirl_function::kirl_function_inner(args, input)
}
