use proc_macro::TokenStream;
use proc_macro2::Ident;
use quote::{format_ident, quote};
use syn::{Data, DeriveInput, Meta, NestedMeta};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(input as DeriveInput);
    let input_name = input.ident;
    let bs_name = format_ident!("{}Builder", input_name);
    let input_data = match input.data {
        Data::Struct(ds) => ds,
        _ => unreachable!("Only works in named structs"),
    };
    let idents: Vec<_> = input_data
        .fields
        .into_iter()
        .map(|f| {
            let name = format_ident!("{}", f.ident.unwrap());
            // Asummes only one attribute
            let mut attribute: Option<Ident> = None;
            if let Some(attr) = f.attrs.first() {
                if let Ok(Meta::List(list)) = attr.parse_meta() {
                    if let Some(NestedMeta::Meta(Meta::NameValue(value))) = list.nested.first() {
                        if let Some(path) = value.path.segments.first() {
                            let ident = &path.ident;
                            if (format!("{ident}") != "each") {
                                unreachable!(
                                    "Invalid name for attribute, the correct value is each"
                                )
                                // Investigate how to span the compile error
                                // compile_error!(
                                //     "Invalid name for attribute, the correct value is each"
                                // );
                            }
                        }
                        if let syn::Lit::Str(str) = value.lit.to_owned() {
                            let at = Ident::new(&str.value(), str.span());
                            attribute = Some(at);
                        }
                    }
                }
            }
            let ty = match f.ty {
                syn::Type::Path(v) => v,
                _ => unreachable!("Only works in named structs"),
            };
            (name, ty, attribute)
        })
        .collect();
    let bs_attr: Vec<_> = idents
        .iter()
        .map(|(name, ty, _)| match inner_type(ty, "Option") {
            (true, ty) => quote! { #name: std::option::Option<#ty> },
            (false, ty) => quote! { #name: std::option::Option<#ty> },
        })
        .collect();
    let bf_attr: Vec<_> = idents
        .iter()
        .map(|(name, _, attr)| match attr {
            Some(_) => quote! { #name: std::option::Option::Some(std::vec::Vec::new()) },
            None => quote! { #name: std::option::Option::None },
        })
        .collect();
    let funcs: Vec<_> = idents
        .iter()
        .map(|(name, ty, attr)| {
            let (_, ty) = inner_type(ty, "Option");
            match attr {
                Some(str) => {
                    let (_, ty) = inner_type(ty, "Vec");
                    quote! {
                        pub fn #str(&mut self, #str: #ty) -> &mut Self {
                            if(self.#name.is_none()){
                            }
                            self.#name.as_mut().unwrap().push(#str);
                            self
                        }
                    }
                }
                None => {
                    quote! {
                        pub fn #name(&mut self, #name: #ty) -> &mut Self {
                            self.#name = std::option::Option::Some(#name);
                            self
                        }
                    }
                }
            }
        })
        .collect();
    let build: Vec<_> = idents.iter().filter(|(_, ty, attr)| {
            let (ok, _) = inner_type(ty, "Option");
            !ok && attr.is_none()
        })
        .map(|(name, _, _)| {
            let f_name = format!("{name}");
            quote! {
                if(self.#name.is_none()) {
                    return std::result::Result::Err(format!("None value in {}\n All not Optional attributes must be set", #f_name).into());
                }
            }
        })
        .collect();
    let command_f: Vec<_> = idents
        .iter()
        .map(|(name, ty, _)| match inner_type(ty, "Option") {
            (true, _) => quote! { #name: self.#name.clone() },
            (false, _) => quote! { #name: self.#name.clone().unwrap() },
        })
        .collect();
    let builder_struct = quote! {
        pub struct #bs_name {
            #(#bs_attr),*
        }
    };

    let impl_struct = quote! {
        impl #input_name {
            pub fn builder() -> #bs_name {
                #bs_name {
                    #(#bf_attr),*
                }
            }
        }
    };

    let impl_builder = quote! {
        impl #bs_name {
            #(#funcs)*
            pub fn build(&mut self) -> std::result::Result<#input_name, std::boxed::Box<dyn std::error::Error>> {
                #(#build)*
                std::result::Result::Ok(
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

fn inner_type<'ty>(ty: &'ty syn::TypePath, outter: &str) -> (bool, &'ty syn::TypePath) {
    if let Some(syn::PathSegment { ident, arguments }) = ty.path.segments.first() {
        // Assumes Option to be written like so
        if &*ident.to_string() == outter {
            if let syn::PathArguments::AngleBracketed(ab) = arguments {
                if let Some(syn::GenericArgument::Type(syn::Type::Path(path))) = ab.args.first() {
                    return (true, path);
                }
            }
            return (true, ty);
        }
    }
    (false, ty)
}
