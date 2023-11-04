use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse_macro_input,
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
        match inner_type(&f.ty) {
            Some((ident, _)) if ident == "Option" => {
                quote! { #name: #ty, }
            }
            Some((ident, _)) if ident == "Vec" && has_builder_attr(&f.attrs) => {
                quote! { #name: #ty, }
            }
            _ => {
                quote! { #name: std::option::Option<#ty>, }
            }
        }
    });

    let bempty = sfields.iter().map(|f| {
        let name = &f.ident;
        match inner_type(&f.ty) {
            Some((ident, _)) if ident == "Vec" && has_builder_attr(&f.attrs) => {
                quote! { #name: Vec::new(), }
            }
            _ => {
                quote! { #name: None, }
            }
        }
    });

    let bfields = sfields.iter().map(|f| {
        let name = &f.ident;
        match inner_type(&f.ty) {
            Some((ident, _)) if ident == "Option" => {
                quote! { #name: self.#name.clone(), }
            }
            Some((ident, _)) if ident == "Vec" && has_builder_attr(&f.attrs) => {
                quote! { #name: self.#name.clone(), }
            }
            _ => {
                quote! { #name: self.#name.as_ref().ok_or_else(|| concat!(stringify!(#name), " is missing")).cloned()?, }
            }
        }
    });

    let bmethods = sfields.iter().flat_map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        match inner_type(ty) {
            Some((wrapper, inner_ty)) if wrapper == "Vec" => {
                use proc_macro2::TokenTree::*;
                let methods: Vec<_> = f
                    .attrs
                    .iter()
                    .filter(|attr| attr.path().is_ident("builder"))
                    .map(|attr| match &attr.meta {
                        Meta::List(MetaList { tokens, .. }) => tokens,
                        _ => panic!("Invalid syntax: {:#?}", attr.meta),
                    })
                    .map(|tokens| {
                        let mut iter = tokens.clone().into_iter();
                        match (iter.next(), iter.next(), iter.next(), iter.next()) {
                            (Some(Ident(ident)), Some(Punct(punct)), Some(Literal(lit)), None) => {
                                assert_eq!(ident, "each");
                                assert_eq!(punct.as_char(), '=');
                                let fnname = match syn::Lit::new(lit) {
                                    syn::Lit::Str(lit) => syn::Ident::new(&lit.value(), lit.span()),
                                    _ => panic!("Invalid syntax: {:#?}", tokens),
                                };
                                quote! {
                                    pub fn #fnname(&mut self, #fnname: #inner_ty) -> &mut Self {
                                        self.#name.push(#fnname);
                                        self
                                    }
                                }
                            }
                            _ => panic!("Invalid syntax: {:#?}", tokens),
                        }
                    })
                    .collect();
                match methods.is_empty() {
                    true => vec![quote! {
                        pub fn #name(&mut self, #name: #ty) -> &mut Self {
                            self.#name = Some(#name);
                            self
                        }
                    }],
                    false => methods,
                }
            }
            Some((wrapper, inner_ty)) if wrapper == "Option" => {
                vec![quote! {
                    pub fn #name(&mut self, #name: #inner_ty) -> &mut Self {
                        self.#name = Some(#name);
                        self
                    }
                }]
            }
            _ => {
                vec![quote! {
                    pub fn #name(&mut self, #name: #ty) -> &mut Self {
                        self.#name = Some(#name);
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

            pub fn build(&mut self) -> Result<#sname, Box<dyn std::error::Error>> {
                Ok(#sname {
                    #(#bfields)*
                })
            }
        }
    };

    TokenStream::from(expanded)
}
