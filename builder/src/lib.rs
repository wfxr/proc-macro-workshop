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
    Path,
    PathArguments,
    PathSegment,
    Type,
    TypePath,
};

fn inner_type(ty: &Type) -> Option<&Type> {
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

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let sname = input.ident;

    let bname = format!("{}Builder", sname);
    let bname = syn::Ident::new(&bname, sname.span());

    let fields = match input.data {
        Data::Struct(DataStruct { fields: Fields::Named(FieldsNamed { named, .. }), .. }) => named,
        _ => unreachable!(),
    };

    let bidents = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        if inner_type(&f.ty).is_some() {
            quote! { #name: #ty, }
        } else {
            quote! { #name: std::option::Option<#ty>, }
        }
    });

    let bempty = fields.iter().map(|f| {
        let name = &f.ident;
        quote! { #name: None, }
    });

    let bfields = fields.iter().map(|f| {
        let name = &f.ident;
        if inner_type(&f.ty).is_some() {
            quote! { #name: self.#name.clone(), }
        } else {
            quote! { #name: self.#name.as_ref().ok_or_else(|| concat!(stringify!(#name), " is missing")).cloned()?, }
        }
    });

    let bmethods = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = inner_type(&f.ty).unwrap_or(&f.ty);
        quote! {
            pub fn #name(&mut self, #name: #ty) -> &mut Self {
                self.#name = Some(#name);
                self
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
