use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, AngleBracketedGenericArguments, Data, DeriveInput, GenericArgument, Path, TypePath};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let struct_name = input.ident;

    let builder_name = format!("{}Builder", struct_name);
    let builder_name = syn::Ident::new(&builder_name, struct_name.span());

    let mut builder_fields = Vec::new();
    let mut builder_defaults = Vec::new();
    let mut builder_methods = Vec::new();
    let mut build_stmts = Vec::new();
    match input.data {
        Data::Struct(s) => s
            .fields
            .iter()
            .filter_map(|f| f.ident.as_ref().map(|name| (name, &f.ty)))
            .for_each(|(name, ty)| {
                let (ty, is_option) = match ty {
                    syn::Type::Path(TypePath { path: Path { segments, .. }, .. }) => {
                        if segments.len() == 1 && segments[0].ident == "Option" {
                            match &segments[0].arguments {
                                syn::PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }) =>
                                    match &args[0] {
                                        GenericArgument::Type(ty) => (ty, true),
                                        _ => unreachable!(),
                                    },
                                _ => unreachable!(),
                            }
                        } else {
                            (ty, false)
                        }
                    }
                    _ => unreachable!(),
                };

                builder_fields.push(quote! {
                    #name: Option<#ty>,
                });


                builder_defaults.push(quote! {
                    #name: None,
                });

                builder_methods.push(quote! {
                    pub fn #name(&mut self, #name: #ty) -> &mut Self {
                        self.#name = Some(#name);
                        self
                    }
                });

                match is_option {
                    true => {
                        build_stmts.push(quote! {
                            #name: self.#name.clone(),
                        });
                    }
                    false => {
                        build_stmts.push(quote! {
                            #name: self.#name.as_ref().ok_or_else(|| "#name is missing").cloned()?,
                        });
                    }
                }
            }),
        _ => panic!("Only structs are supported!"),
    };

    let expanded = quote! {
        pub struct #builder_name {
            #(#builder_fields)*
        }

        impl #struct_name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #(#builder_defaults)*
                }
            }
        }

        impl #builder_name {
            #(#builder_methods)*

            pub fn build(&mut self) -> Result<#struct_name, Box<dyn std::error::Error>> {
                Ok(#struct_name {
                    #(#build_stmts)*
                })
            }
        }
    };

    TokenStream::from(expanded)
}
