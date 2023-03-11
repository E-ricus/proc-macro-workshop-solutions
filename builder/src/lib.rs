use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote};
use syn::{Error, ItemStruct, Meta, NestedMeta, Type, TypePath};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input!(input as ItemStruct);
    let input_name = input.ident;
    let builder_name = format_ident!("{}Builder", input_name);

    // Transform input into values used to parse each expantion
    let len = input.fields.len();
    let mut values: Vec<(Ident, &TypePath, Option<Ident>, bool)> = Vec::with_capacity(len);
    for field in input.fields.iter() {
        let name = format_ident!("{}", field.ident.as_ref().unwrap());
        let attribute: Option<Ident> = match field.attrs.first() {
            Some(attr) => match get_attribute(attr) {
                Ok(at) => Some(at),
                Err(err) => return err.into(),
            },
            None => None,
        };
        let ty = match &field.ty {
            Type::Path(v) => v,
            _ => return span_error(input_name, "expected a named struct").into(),
        };
        let (is_option, ty) = inner_type(ty, "Option");
        values.push((name, ty, attribute, is_option));
    }

    let builder_attrs: Vec<TokenStream> = values
        .iter()
        .map(|(name, ty, _, _)| {
            quote! { #name: std::option::Option<#ty> }
        })
        .collect();
    let attrs_init: Vec<TokenStream> = values.iter().map(attr_init).collect();
    let builder_funcs: Vec<TokenStream> = values.iter().map(builder_func).collect();
    let build_validations: Vec<TokenStream> = values
        .iter()
        .filter(|(_, _, attr, option)| !option && attr.is_none())
        .map(|(name, _, _, _)| build_validation(name))
        .collect();
    let command_inits: Vec<TokenStream> = values
        .iter()
        .map(|(name, _, _, is_option)| command_init(name, *is_option))
        .collect();

    let builder_struct = quote! {
        pub struct #builder_name {
            #(#builder_attrs),*
        }
    };

    let impl_struct = quote! {
        impl #input_name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #(#attrs_init),*
                }
            }
        }
    };

    let impl_builder = quote! {
        impl #builder_name {
            #(#builder_funcs)*
            pub fn build(&mut self) -> std::result::Result<#input_name, std::boxed::Box<dyn std::error::Error>> {
                #(#build_validations)*
                std::result::Result::Ok(
                    #input_name {
                        #(#command_inits),*
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
    proc_macro::TokenStream::from(expanded)
}

fn span_error<T: quote::ToTokens>(t: T, message: &str) -> TokenStream {
    Error::new_spanned(t, message).to_compile_error()
}

fn builder_func(value: &(Ident, &TypePath, Option<Ident>, bool)) -> TokenStream {
    let name = &value.0;
    let ty = value.1;
    let attr = &value.2;
    match attr {
        Some(ident) => {
            let (_, ty) = inner_type(ty, "Vec");
            quote! {
                pub fn #ident(&mut self, #ident: #ty) -> &mut Self {
                    self.#name.as_mut().unwrap().push(#ident);
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
}

fn attr_init(value: &(Ident, &TypePath, Option<Ident>, bool)) -> TokenStream {
    let name = &value.0;
    let attr = &value.2;

    match attr {
        Some(_) => {
            quote! { #name: std::option::Option::Some(std::vec::Vec::new()) }
        }
        None => {
            quote! { #name: std::option::Option::None }
        }
    }
}

fn build_validation(name: &Ident) -> TokenStream {
    let f_name = format!("{name}");
    quote! {
        if(self.#name.is_none()) {
            return std::result::Result::Err(format!("None value in {}\n All not Optional attributes must be set", #f_name).into());
        }
    }
}

fn command_init(name: &Ident, is_option: bool) -> TokenStream {
    match is_option {
        true => {
            quote! { #name: self.#name.clone() }
        }
        false => {
            quote! { #name: self.#name.clone().unwrap() }
        }
    }
}

fn get_attribute(attr: &syn::Attribute) -> Result<Ident, TokenStream> {
    // Asummes only one attribute
    let message = "expected `builder(each = \"...\")`";
    match attr.parse_meta() {
        Ok(Meta::List(list)) => {
            if let Some(NestedMeta::Meta(Meta::NameValue(value))) = list.nested.first() {
                if let Some(path) = value.path.segments.first() {
                    let ident = &path.ident;
                    if (format!("{ident}") != "each") {
                        return Err(span_error(list, message));
                    }
                }
                if let syn::Lit::Str(str) = value.lit.to_owned() {
                    let at = Ident::new(&str.value(), str.span());
                    return Ok(at);
                }
            }
            Err(span_error(list, message))
        }
        _ => Err(span_error(attr, message)),
    }
}

fn inner_type<'ty>(ty: &'ty TypePath, outter: &str) -> (bool, &'ty TypePath) {
    if let Some(syn::PathSegment { ident, arguments }) = ty.path.segments.first() {
        // Assumes Option to be written like so
        if &*ident.to_string() == outter {
            if let syn::PathArguments::AngleBracketed(ab) = arguments {
                if let Some(syn::GenericArgument::Type(Type::Path(path))) = ab.args.first() {
                    return (true, path);
                }
            }
            return (true, ty);
        }
    }
    (false, ty)
}
