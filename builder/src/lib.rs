use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse_macro_input,
    spanned::Spanned,
    AngleBracketedGenericArguments,
    Data,
    DataStruct,
    DeriveInput,
    Fields,
    FieldsNamed,
    GenericArgument,
    Meta,
    MetaList,
    Path,
    PathArguments,
    PathSegment,
    Type,
    TypePath,
};

fn inner_type(ty: &Type) -> Option<(&proc_macro2::Ident, &Type)> {
    match ty {
        Type::Path(TypePath { path: Path { segments, .. }, .. }) if segments.len() == 1 => {
            let PathSegment { ident: wrapper_ident, arguments } = &segments[0];
            match arguments {
                PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }) if args.len() == 1 =>
                    match &args[0] {
                        GenericArgument::Type(inner_ty) => Some((wrapper_ident, inner_ty)),
                        _ => None,
                    },
                _ => None,
            }
        }
        _ => None,
    }
}

fn has_builder_attr(attrs: &[syn::Attribute]) -> bool {
    attrs
        .iter()
        .any(|attr| matches!(&attr.meta, Meta::List(MetaList { path, .. }) if path.is_ident("builder")))
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let sname = input.ident;

    let bname = format!("{}Builder", sname);
    let bname = syn::Ident::new(&bname, sname.span());

    let sfields = match input.data {
        Data::Struct(DataStruct { fields: Fields::Named(FieldsNamed { named, .. }), .. }) => named,
        _ => unreachable!(),
    };

    let bidents = sfields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        let ty = match inner_type(ty) {
            Some((ident, _)) if ident == "Option" => quote! { #ty },
            Some((ident, _)) if ident == "Vec" && has_builder_attr(&f.attrs) => quote! { #ty },
            _ => quote! { ::std::option::Option<#ty> },
        };
        quote! { #name: #ty, }
    });

    let bempty = sfields.iter().map(|f| {
        let name = &f.ident;
        let ty = match inner_type(&f.ty) {
            Some((ident, _)) if ident == "Vec" && has_builder_attr(&f.attrs) => quote! { ::std::vec::Vec::new() },
            _ => quote! { ::std::option::Option::None },
        };
        quote! { #name: #ty, }
    });

    let bfields = sfields.iter().map(|f| {
        let name = &f.ident;
        let val = match inner_type(&f.ty) {
            Some((ident, _)) if ident == "Option" => quote! { self.#name.clone() },
            Some((ident, _)) if ident == "Vec" && has_builder_attr(&f.attrs) => quote! { self.#name.clone() },
            _ => quote! { self.#name.as_ref().ok_or_else(|| concat!(stringify!(#name), " is missing")).cloned()? },
        };
        quote! { #name: #val, }
    });

    let bmethods = sfields.iter().flat_map(|f| {
        let name = &f.ident;
        let ty = &f.ty;

        match inner_type(ty) {
            Some((wrapper, inner_ty)) if wrapper == "Vec" => {
                use proc_macro2::TokenTree::*;
                let methods: Result<Vec<_>, _> = f
                    .attrs
                    .iter()
                    .filter(|attr| attr.path().is_ident("builder"))
                    .map(|attr| {
                        let gen_error = || syn::Error::new(attr.meta.span(), r#"expected `builder(each = "...")`"#);
                        match &attr.meta {
                            Meta::List(MetaList { tokens, .. }) => Ok((tokens, gen_error)),
                            _ => Err(gen_error()),
                        }
                    })
                    .map(|tokens| {
                        tokens.and_then(|(tokens, gen_error)| {
                            let mut iter = tokens.clone().into_iter();
                            match (iter.next(), iter.next(), iter.next(), iter.next()) {
                                (Some(Ident(ident)), Some(Punct(punct)), Some(Literal(lit)), None) =>
                                    match syn::Lit::new(lit) {
                                        syn::Lit::Str(lit) if ident == "each" && punct.as_char() == '=' => {
                                            let fnname = syn::Ident::new(&lit.value(), lit.span());
                                            Ok(quote! {
                                                pub fn #fnname(&mut self, #fnname: #inner_ty) -> &mut Self {
                                                    self.#name.push(#fnname);
                                                    self
                                                }
                                            })
                                        }
                                        _ => Err(gen_error()),
                                    },
                                _ => Err(gen_error()),
                            }
                        })
                    })
                    .collect();
                match methods {
                    Ok(methods) if methods.is_empty() => vec![quote! {
                        pub fn #name(&mut self, #name: #ty) -> &mut Self {
                            self.#name = ::std::option::Option::Some(#name);
                            self
                        }
                    }],
                    Ok(methods) => methods,
                    Err(e) => vec![e.into_compile_error()],
                }
            }
            Some((wrapper, inner_ty)) if wrapper == "Option" => {
                vec![quote! {
                    pub fn #name(&mut self, #name: #inner_ty) -> &mut Self {
                        self.#name = ::std::option::Option::Some(#name);
                        self
                    }
                }]
            }
            _ => {
                vec![quote! {
                    pub fn #name(&mut self, #name: #ty) -> &mut Self {
                        self.#name = ::std::option::Option::Some(#name);
                        self
                    }
                }]
            }
        }
    });

    let expanded = quote! {
        pub struct #bname {
            #(#bidents)*
        }

        impl #sname {
            pub fn builder() -> #bname {
                #bname {
                    #(#bempty)*
                }
            }
        }

        impl #bname {
            #(#bmethods)*

            pub fn build(&mut self) -> ::std::result::Result<#sname, ::std::boxed::Box<dyn ::std::error::Error>> {
                Ok(#sname {
                    #(#bfields)*
                })
            }
        }
    };

    TokenStream::from(expanded)
}
