use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{Data, DeriveInput};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(input as DeriveInput);
    let input_name = input.ident;
    let bs_name = format_ident!("{}Builder", input_name);
    let input_data = match input.data {
        Data::Struct(ds) => ds,
        _ => panic!("Only works in named structs"),
    };
    let bs_attr: Vec<_> = input_data
        .fields
        .iter()
        .map(|f| {
            let f_name = format_ident!("{}", f.ident.as_ref().unwrap());
            let f_type = match &f.ty {
                syn::Type::Path(v) => v,
                _ => panic!("Only works in named structs"),
            };
            quote! { #f_name: Option<#f_type> }
        })
        .collect();
    let builder_struct = quote! {
        pub struct #bs_name {
            #(#bs_attr),*
        }
    };

    let bf_attr: Vec<_> = input_data
        .fields
        .iter()
        .map(|f| {
            let f_name = format_ident!("{}", f.ident.as_ref().unwrap());
            quote! { #f_name: None }
        })
        .collect();

    let impl_struct = quote! {
        impl #input_name {
            pub fn builder() -> #bs_name {
                #bs_name {
                    #(#bf_attr),*
                }
            }
        }
    };

    let funcs: Vec<_> = input_data
        .fields
        .iter()
        .map(|f| {
            let f_name = format_ident!("{}", f.ident.as_ref().unwrap());
            let f_type = match &f.ty {
                syn::Type::Path(v) => v,
                _ => panic!("Only works in named structs"),
            };
            quote! {
                pub fn #f_name(&mut self, #f_name: #f_type) -> &mut Self {
                    self.#f_name = Some(#f_name);
                    self
                }
            }
        })
        .collect();

    let build: Vec<_> = input_data
        .fields
        .iter()
        .map(|f| {
            let f_ident = f.ident.as_ref().unwrap();
            let f_name = format_ident!("{}", f_ident);
            let f_string = format!("{f_ident}");
            quote! {
                if(self.#f_name.is_none()) {
                    return Err(format!("None value in {}\n you need all attributes set", #f_string).into());
                }
            }
        })
        .collect();

    let command_f: Vec<_> = input_data
        .fields
        .iter()
        .map(|f| {
            let f_name = format_ident!("{}", f.ident.as_ref().unwrap());
            quote! {
                #f_name: self.#f_name.clone().unwrap()
            }
        })
        .collect();

    let impl_builder = quote! {
        impl #bs_name {
            #(#funcs)*
            pub fn build(&mut self) -> Result<#input_name, Box<dyn std::error::Error>> {
                #(#build)*
                Ok(
                    #input_name {
                        #(#command_f),*
                    }
                )
            }
        }
    };

    // Final expanded code
    let expanded = quote! {
        #builder_struct
        #impl_struct
        #impl_builder
    };
    TokenStream::from(expanded)
}
