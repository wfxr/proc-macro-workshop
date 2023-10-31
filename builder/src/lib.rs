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

fn option_inner_type(ty: &Type) -> Option<&Type> {
    match ty {
        Type::Path(TypePath { path: Path { segments, .. }, .. }) if segments.len() == 1 => match &segments[0] {
            PathSegment { ident, arguments } if ident == "Option" => match arguments {
                PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }) if args.len() == 1 =>
                    match &args[0] {
                        GenericArgument::Type(inner_ty) => Some(inner_ty),
                        _ => None,
                    },
                _ => None,
            },
            _ => None,
        },
        _ => None,
    }
}

fn vec_inner_type(ty: &Type) -> Option<&Type> {
    match ty {
        Type::Path(TypePath { path: Path { segments, .. }, .. }) if segments.len() == 1 => match &segments[0] {
            PathSegment { ident, arguments } if ident == "Vec" => match arguments {
                PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }) if args.len() == 1 =>
                    match &args[0] {
                        GenericArgument::Type(inner_ty) => Some(inner_ty),
                        _ => None,
                    },
                _ => None,
            },
            _ => None,
        },
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
        if option_inner_type(&f.ty).is_some() || (vec_inner_type(&f.ty).is_some() && has_builder_attr(&f.attrs)) {
            quote! { #name: #ty, }
        } else {
            quote! { #name: std::option::Option<#ty>, }
        }
    });

    let bempty = sfields.iter().map(|f| {
        let name = &f.ident;
        if vec_inner_type(&f.ty).is_some() && has_builder_attr(&f.attrs) {
            quote! { #name: Vec::new(), }
        } else {
            quote! { #name: None, }
        }
    });

    let bfields = sfields.iter().map(|f| {
        let name = &f.ident;
        if option_inner_type(&f.ty).is_some() || (vec_inner_type(&f.ty).is_some() && has_builder_attr(&f.attrs)) {
            quote! { #name: self.#name.clone(), }
        } else {
            quote! { #name: self.#name.as_ref().ok_or_else(|| concat!(stringify!(#name), " is missing")).cloned()?, }
        }
    });

    let bmethods = sfields.iter().flat_map(|f| {
        let name = &f.ident;
        let ty = option_inner_type(&f.ty).unwrap_or(&f.ty);

        use proc_macro2::TokenTree::*;
        let mut methods: Vec<_> = f
            .attrs
            .iter()
            .map(|attr| match &attr.meta {
                Meta::List(MetaList { path, tokens, .. }) if path.is_ident("builder") => {
                    let mut iter = tokens.clone().into_iter();
                    match (iter.next(), iter.next(), iter.next(), iter.next()) {
                        (Some(Ident(ident)), Some(Punct(punct)), Some(Literal(val)), None) => {
                            assert_eq!(ident, "each");
                            assert_eq!(punct.as_char(), '=');
                            let fnname = val.to_string().trim_matches('"').to_string();
                            let fnname = syn::Ident::new(&fnname, val.span());
                            let name = name.clone();
                            let ty = ty.clone();
                            match vec_inner_type(&ty) {
                                Some(ty) => {
                                    quote! {
                                        pub fn #fnname(&mut self, #fnname: #ty) -> &mut Self {
                                            self.#name.push(#fnname);
                                            self
                                        }
                                    }
                                }
                                None => panic!("Invalid syntax"),
                            }
                        }
                        _ => panic!("Invalid syntax"),
                    }
                }
                _ => quote! {},
            })
            .collect();

        if methods.is_empty() {
            methods.push(quote! {
                pub fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#name = Some(#name);
                    self
                }
            });
        }
        methods
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
            // #(#bmethods)*

            pub fn build(&mut self) -> Result<#sname, Box<dyn std::error::Error>> {
                Ok(#sname {
                    #(#bfields)*
                })
            }
        }
    };

    TokenStream::from(expanded)
}
