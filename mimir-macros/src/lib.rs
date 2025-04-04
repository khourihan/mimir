extern crate proc_macro;
use proc_macro::TokenStream;
use quote::quote;
use syn::{Data, DeriveInput, Expr, Fields, Meta, parse_macro_input};

#[proc_macro_derive(IterEnum)]
pub fn derive_iter_enum(input: TokenStream) -> TokenStream {
    let DeriveInput {
        ident,
        data,
        generics,
        vis,
        ..
    } = parse_macro_input!(input as DeriveInput);

    let where_clause = &generics.where_clause;

    let Data::Enum(enum_data) = data else {
        return quote! { () }.into();
    };
    let variant_idents = enum_data.variants.iter().map(|x| &x.ident);

    let iter_ident = quote::format_ident!("{}Iter", ident);

    quote! {
        impl #generics #ident #generics #where_clause {
            #vis fn iter_fields() -> #iter_ident {
                #iter_ident { iter: vec![#(#ident::#variant_idents),*].into_iter() }
            }
        }

        #vis struct #iter_ident #generics #where_clause {
            iter: std::vec::IntoIter<#ident>,
        }

        impl Iterator for #iter_ident {
            type Item = #ident;

            fn next(&mut self) -> Option<Self::Item> {
                self.iter.next()
            }
        }
    }
    .into()
}

#[proc_macro_derive(FieldConstructor)]
pub fn derive_field_constructor(input: TokenStream) -> TokenStream {
    let DeriveInput {
        ident,
        data,
        generics,
        vis,
        ..
    } = parse_macro_input!(input as DeriveInput);

    let where_clause = &generics.where_clause;

    let Data::Enum(data) = data else {
        return quote! { () }.into();
    };
    let variant_idents: Vec<_> = data.variants.iter().map(|x| &x.ident).collect();
    let fn_names = variant_idents
        .iter()
        .map(|x| quote::format_ident!("{}", x.to_string().to_lowercase()));
    let (variant_params, variant_fields): (Vec<_>, Vec<_>) = data
        .variants
        .iter()
        .map(|x| match &x.fields {
            Fields::Unit => (quote! {}, quote! {}),
            Fields::Named(names) => {
                let idents: Vec<_> = names.named.iter().map(|x| x.ident.as_ref().unwrap()).collect();
                let types = names.named.iter().map(|x| &x.ty);
                (quote! { #(#idents: #types,)* }, quote! { { #(#idents,)* } })
            },
            Fields::Unnamed(unnamed) => {
                let idents: Vec<_> = unnamed
                    .unnamed
                    .iter()
                    .enumerate()
                    .map(|(i, _)| quote::format_ident!("x{}", i))
                    .collect();
                let types = unnamed.unnamed.iter().map(|x| &x.ty);
                (quote! { #(#idents: #types,)* }, quote! { ( #(#idents,)* ) })
            },
        })
        .unzip();

    quote! {
        impl #generics #ident #generics #where_clause {
            #(
                #vis fn #fn_names(#variant_params) -> #ident {
                    #ident::#variant_idents #variant_fields
                }
            )*
        }
    }
    .into()
}

#[proc_macro_derive(StringifyEnum)]
pub fn derive_stringify_enum(input: TokenStream) -> TokenStream {
    let DeriveInput {
        ident,
        data,
        generics,
        vis,
        ..
    } = parse_macro_input!(input as DeriveInput);

    let where_clause = &generics.where_clause;

    let Data::Enum(enum_data) = data else {
        return quote! { () }.into();
    };
    let variant_idents: Vec<_> = enum_data.variants.iter().map(|x| &x.ident).collect();
    let variant_fields: Vec<_> = enum_data
        .variants
        .iter()
        .map(|x| &x.fields)
        .map(|x| match x {
            Fields::Unit => quote! {},
            Fields::Named(_) => quote! { { .. } },
            Fields::Unnamed(_) => quote! { ( .. ) },
        })
        .collect();
    let variant_docs = enum_data
        .variants
        .iter()
        .map(|x| {
            if x.attrs.is_empty() {
                None
            } else {
                Some(
                    x.attrs
                        .iter()
                        .map(|x| {
                            if let Meta::NameValue(v) = &x.meta {
                                Some(v)
                            } else {
                                None
                            }
                        })
                        .map(|y| {
                            if let Some(x) = y {
                                if let Some(p) = x.path.get_ident() {
                                    if *p == "doc" {
                                        let Expr::Lit(lit) = &x.value else { unreachable!() };
                                        let syn::Lit::Str(litstr) = &lit.lit else {
                                            unreachable!()
                                        };
                                        litstr.value().trim().to_string()
                                    } else {
                                        "".to_string()
                                    }
                                } else {
                                    "".to_string()
                                }
                            } else {
                                "".to_string()
                            }
                        }),
                )
            }
        })
        .map(|x| {
            if let Some(mut s) = x {
                s.next().unwrap()
            } else {
                "".to_string()
            }
        });
    let variant_strs = variant_idents.iter().map(|x| x.to_string());

    quote! {
        impl #generics #ident #generics #where_clause {
            #vis fn stringify_field(&self) -> &'static str {
                match self {
                    #(
                        #ident::#variant_idents #variant_fields => #variant_strs,
                    )*
                }
            }

            #vis fn stringify_pretty(&self) -> &'static str {
                match self {
                    #(
                        #ident::#variant_idents #variant_fields => #variant_docs,
                    )*
                }
            }
        }
    }
    .into()
}
