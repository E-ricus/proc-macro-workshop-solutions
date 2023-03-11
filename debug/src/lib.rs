use proc_macro2::TokenStream;
use quote::quote;
use syn::{GenericParam, Generics, ItemStruct, LitStr, Meta};

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input!(input as ItemStruct);
    let input_name = input.ident;
    let f_input_name = format!("{input_name}");

    let generics = add_trait_bounds(input.generics);
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let debug_fields: Vec<TokenStream> = input
        .fields
        .iter()
        .map(|field| {
            let name = field.ident.as_ref().unwrap();
            let attribute: Option<LitStr> = match field.attrs.first() {
                Some(attr) => match get_attribute(attr) {
                    Ok(lit) => Some(lit),
                    // TODO: Handle error
                    Err(_) => None,
                },
                None => None,
            };
            let f_name = format!("{name}");

            match attribute {
                Some(lit) => quote! { field(#f_name, &format_args!(#lit, self.#name)) },
                None => quote! { field(#f_name, &self.#name) },
            }
        })
        .collect();

    let expanded = quote! {
        impl #impl_generics std::fmt::Debug for #input_name #ty_generics #where_clause {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct(#f_input_name)
                .#(#debug_fields).*
                .finish()
        }
    }
        };

    proc_macro::TokenStream::from(expanded)
}

fn span_error<T: quote::ToTokens>(t: T, message: &str) -> TokenStream {
    syn::Error::new_spanned(t, message).to_compile_error()
}

fn get_attribute(attr: &syn::Attribute) -> Result<LitStr, TokenStream> {
    // Asummes only one attribute
    let message = "expected `debug = \"...\"`";
    match attr.parse_meta() {
        Ok(Meta::NameValue(value)) => {
            if let Some(path) = value.path.segments.first() {
                let ident = &path.ident;
                if (format!("{ident}") != "debug") {
                    return Err(span_error(path, message));
                }
            }
            if let syn::Lit::Str(str) = value.lit {
                return Ok(str);
            }
            Err(span_error(attr, message))
        }
        Err(err) => Err(err.to_compile_error()),
        _ => Err(span_error(attr, message)),
    }
}

// Add a bound `T: Debug` to every type parameter T.
fn add_trait_bounds(mut generics: Generics) -> Generics {
    for param in &mut generics.params {
        if let GenericParam::Type(ref mut type_param) = *param {
            type_param.bounds.push(syn::parse_quote!(std::fmt::Debug));
        }
    }
    generics
}
