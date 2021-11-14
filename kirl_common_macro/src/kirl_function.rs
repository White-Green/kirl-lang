use kirl_common::typing::HIRType;
use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use std::collections::HashMap;
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::{braced, bracketed, parenthesized, parse_macro_input, token, AngleBracketedGenericArguments, ConstParam, FnArg, GenericArgument, GenericParam, Generics, Ident, ItemFn, LifetimeDef, LitStr, PatType, Path, PathArguments, PathSegment, ReturnType, Signature, Token, TraitBound, Type, TypeParam, TypeParamBound, TypeParen, TypePath, TypeTraitObject};

#[derive(Debug)]
struct TypeWithGenerics {
    _for: Option<Token![for]>,
    _open: Option<Token![<]>,
    items: Option<Punctuated<Ident, Token![,]>>,
    _close: Option<Token![>]>,
    ty: TypeSyntax,
}

impl TypeWithGenerics {
    fn expand_generics(self) -> HIRType {
        let TypeWithGenerics { items, ty, .. } = self;
        let ty: HIRType = ty.into();
        if let Some(items) = items {
            let mut map = HashMap::<String, usize>::new();
            for (i, item) in items.into_iter().enumerate() {
                map.insert(item.to_string(), i).ok_or(()).expect_err("duplicated type argument name");
            }
            fn expand_generics_inner(ty: HIRType, map: &HashMap<String, usize>) -> HIRType {
                match ty {
                    ty @ (HIRType::Infer | HIRType::Unreachable | HIRType::GenericsTypeArgument(_)) => ty,
                    HIRType::Named { path, generics_arguments } => {
                        if generics_arguments.is_empty() {
                            if let [name] = path.as_slice() {
                                if let Some(index) = map.get(name) {
                                    return HIRType::GenericsTypeArgument(*index);
                                }
                            }
                        }
                        HIRType::Named {
                            path,
                            generics_arguments: generics_arguments.into_iter().map(|ty| expand_generics_inner(ty, map)).collect(),
                        }
                    }
                    HIRType::Tuple(items) => HIRType::Tuple(items.into_iter().map(|ty| expand_generics_inner(ty, map)).collect()),
                    HIRType::Array(item) => HIRType::Array(Box::new(expand_generics_inner(*item, map))),
                    HIRType::Function { arguments, result } => HIRType::Function {
                        arguments: arguments.into_iter().map(|ty| expand_generics_inner(ty, map)).collect(),
                        result: Box::new(expand_generics_inner(*result, map)),
                    },
                    HIRType::AnonymousStruct(members) => HIRType::AnonymousStruct(members.into_iter().map(|(k, ty)| (k, expand_generics_inner(ty, map))).collect()),
                    HIRType::Or(items) => HIRType::Or(items.into_iter().map(|ty| expand_generics_inner(ty, map)).collect()),
                }
            }
            expand_generics_inner(ty, &map)
        } else {
            ty
        }
    }
}

impl Parse for TypeWithGenerics {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let (_for, _open, items, _close) = if input.peek(Token![for]) {
            (
                Some(input.parse()?),
                Some(input.parse()?),
                Some({
                    let mut items = Punctuated::new();
                    loop {
                        if input.peek(Token![>]) {
                            break items;
                        }
                        items.push_value(input.parse()?);
                        if input.peek(Token![>]) {
                            break items;
                        }
                        items.push_punct(input.parse()?);
                    }
                }),
                Some(input.parse()?),
            )
        } else {
            (None, None, None, None)
        };
        Ok(TypeWithGenerics { _for, _open, items, _close, ty: input.parse()? })
    }
}

#[derive(Debug)]
struct StructItem {
    name: Ident,
    _colon: Token![:],
    ty: TypeSyntax,
}

impl Parse for StructItem {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(StructItem { name: input.parse()?, _colon: input.parse()?, ty: input.parse()? })
    }
}

#[derive(Debug)]
enum TypeSyntax {
    Unreachable(Token![!]),
    Named { path: Punctuated<Ident, Token![::]>, _open: Option<Token![<]>, args: Option<Punctuated<TypeSyntax, Token![,]>>, _close: Option<Token![>]> },
    Tuple { _paren: token::Paren, items: Punctuated<TypeSyntax, Token![,]> },
    Array { _bracket: token::Bracket, item: Box<TypeSyntax> },
    Function { _paren: token::Paren, args: Punctuated<TypeSyntax, Token![,]>, _arrow: Token![->], result: Box<TypeSyntax> },
    AnonymousStruct { _sharp: Token![#], _brace: token::Brace, members: Punctuated<StructItem, Token![,]> },
    Or { _paren: token::Paren, items: Punctuated<TypeSyntax, Token![|]> },
}

impl Parse for TypeSyntax {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(Token![!]) {
            Ok(TypeSyntax::Unreachable(input.parse()?))
        } else if input.peek(Ident) {
            let path = {
                let mut path = Punctuated::new();
                loop {
                    if !input.peek(Ident) {
                        break path;
                    }
                    path.push_value(input.parse()?);
                    if !input.peek(Token![::]) {
                        break path;
                    }
                    path.push_punct(input.parse()?);
                }
            };
            if input.peek(Token![<]) {
                let _open = input.parse()?;
                let mut args = Punctuated::new();
                loop {
                    if input.peek(Token![>]) {
                        break Ok(TypeSyntax::Named { path, _open: Some(_open), args: Some(args), _close: Some(input.parse()?) });
                    }
                    args.push_value(input.parse()?);
                    if input.peek(Token![>]) {
                        break Ok(TypeSyntax::Named { path, _open: Some(_open), args: Some(args), _close: Some(input.parse()?) });
                    }
                    args.push_punct(input.parse()?);
                }
            } else {
                Ok(TypeSyntax::Named { path, _open: None, args: None, _close: None })
            }
        } else if input.peek(token::Bracket) {
            let item;
            #[allow(clippy::eval_order_dependence)]
            Ok(TypeSyntax::Array { _bracket: bracketed!(item in input), item: Box::new(item.parse()?) })
        } else if input.peek(Token![#]) {
            let members;
            #[allow(clippy::eval_order_dependence)]
            Ok(TypeSyntax::AnonymousStruct {
                _sharp: input.parse()?,
                _brace: braced!(members in input),
                members: members.parse_terminated(StructItem::parse)?,
            })
        } else {
            let content;
            let paren: token::Paren = parenthesized!(content in input);
            let tuple = if content.is_empty() {
                Punctuated::<TypeSyntax, Token![,]>::new()
            } else {
                let first: TypeSyntax = content.parse()?;
                if content.is_empty() || content.peek(Token![,]) {
                    let mut tuple_items = Punctuated::<TypeSyntax, Token![,]>::new();
                    tuple_items.push_value(first);
                    loop {
                        if content.is_empty() {
                            break tuple_items;
                        }
                        tuple_items.push_punct(content.parse()?);
                        if content.is_empty() {
                            break tuple_items;
                        }
                        tuple_items.push_value(content.parse()?);
                    }
                } else {
                    let mut or_items = Punctuated::<TypeSyntax, Token![|]>::new();
                    or_items.push_value(first);
                    loop {
                        if content.is_empty() {
                            return Ok(TypeSyntax::Or { _paren: paren, items: or_items });
                        }
                        or_items.push_punct(content.parse()?);
                        if content.is_empty() {
                            return Ok(TypeSyntax::Or { _paren: paren, items: or_items });
                        }
                        or_items.push_value(content.parse()?);
                    }
                }
            };
            if input.peek(Token![->]) {
                Ok(TypeSyntax::Function {
                    _paren: paren,
                    args: tuple,
                    _arrow: input.parse()?,
                    result: Box::new(input.parse()?),
                })
            } else {
                Ok(TypeSyntax::Tuple { _paren: paren, items: tuple })
            }
        }
    }
}

impl From<TypeSyntax> for HIRType {
    fn from(ty: TypeSyntax) -> Self {
        match ty {
            TypeSyntax::Unreachable(_) => HIRType::Unreachable,
            TypeSyntax::Named { path, args, .. } => {
                let path = path.iter().map(|ident| ident.to_string()).collect();
                HIRType::Named {
                    path,
                    generics_arguments: args.into_iter().flatten().map(Into::into).collect(),
                }
            }
            TypeSyntax::Tuple { items, .. } => HIRType::Tuple(items.into_iter().map(Into::into).collect()),
            TypeSyntax::Array { item, .. } => HIRType::Array(Box::new((*item).into())),
            TypeSyntax::Function { args, result, .. } => HIRType::Function {
                arguments: args.into_iter().map(Into::into).collect(),
                result: Box::new((*result).into()),
            },
            TypeSyntax::AnonymousStruct { members, .. } => HIRType::AnonymousStruct(members.into_iter().map(|StructItem { name, ty, .. }| (name.to_string(), ty.into())).collect()),
            TypeSyntax::Or { items, .. } => HIRType::Or(items.into_iter().map(Into::into).collect()),
        }
    }
}

fn quote_type(ty: &HIRType) -> proc_macro2::TokenStream {
    match ty {
        HIRType::Infer => quote! { kirl_common::typing::HIRType::Infer },
        HIRType::Unreachable => quote! {kirl_common::typing::HIRType::Unreachable },
        HIRType::GenericsTypeArgument(i) => quote! { kirl_common::typing::HIRType::GenericsTypeArgument(#i) },
        HIRType::Named { path, generics_arguments } => {
            let path = path.iter().map(|s| syn::LitStr::new(s, Span::call_site()));
            let generics_arguments = generics_arguments.iter().map(|ty| quote_type(ty));
            quote! { kirl_common::typing::HIRType::Named { path: vec![#(#path.to_string()),*], generics_arguments: vec![#(#generics_arguments),*] } }
        }
        HIRType::Tuple(items) => {
            let items = items.iter().map(|ty| quote_type(ty));
            quote! { kirl_common::typing::HIRType::Tuple(vec![#(#items),*]) }
        }
        HIRType::Array(item) => {
            let item = quote_type(item);
            quote! { kirl_common::typing::HIRType::Array(Box::new(#item)) }
        }
        HIRType::Function { arguments, result } => {
            let arguments = arguments.iter().map(|ty| quote_type(ty));
            let result = quote_type(result);
            quote! { kirl_common::typing::HIRType::Function { arguments: vec![#(#arguments),*], result: Box::new(#result) } }
        }
        HIRType::AnonymousStruct(members) => {
            let members = members.iter().map(|(k, v)| {
                let k = LitStr::new(k, Span::call_site());
                let v = quote_type(v);
                quote! { (#k.to_string(), #v) }
            });
            quote! { kirl_common::typing::HIRType::AnonymousStruct(std::collections::BTreeMap::from([#(#members),*])) }
        }
        HIRType::Or(items) => {
            let items = items.iter().map(|ty| quote_type(ty));
            quote! { kirl_common::typing::HIRType::Or(vec![#(#items),*]) }
        }
    }
}

enum ParamType<'a> {
    NonCast,
    Cast(&'a Type),
    Owned(&'a Type),
}

fn unwrap_paren(ty: &Type) -> &Type {
    if let Type::Paren(TypeParen { elem, .. }) = ty {
        unwrap_paren(elem)
    } else {
        ty
    }
}

fn unwrap_result(ty: &Type) -> Option<(&Type, &Type)> {
    if let Type::Path(TypePath { path: Path { segments, .. }, .. }) = unwrap_paren(ty) {
        if segments.len() <= 3 && segments.iter().rev().zip(["std", "result", "Result"].iter().rev()).all(|(seg, name)| seg.ident == name) || segments.len() <= 2 && segments.iter().rev().zip(["std", "Result"].iter().rev()).all(|(seg, name)| seg.ident == name) {
            if segments.iter().rev().skip(1).any(|seg| !matches!(seg.arguments, PathArguments::None)) {
                return None;
            }
            if let Some(PathSegment {
                arguments: PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }),
                ..
            }) = segments.last()
            {
                if args.len() != 2 {
                    return None;
                }
                if let (GenericArgument::Type(ty0), GenericArgument::Type(ty1)) = (&args[0], &args[1]) {
                    Some((ty0, ty1))
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            None
        }
    } else {
        None
    }
}

fn get_param_type(ty: &Type) -> ParamType {
    fn unwrap_type<'a>(ty: &'a Type, path: &[&str]) -> Option<&'a Type> {
        if let Type::Path(TypePath { path: Path { segments, .. }, .. }) = unwrap_paren(ty) {
            if segments.len() > path.len() {
                return None;
            }
            if segments.iter().rev().zip(path.iter().rev()).any(|(seg, name)| seg.ident != name) {
                return None;
            }
            if segments.iter().rev().skip(1).any(|seg| !matches!(seg.arguments, PathArguments::None)) {
                return None;
            }
            if let Some(PathSegment {
                arguments: PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }),
                ..
            }) = segments.last()
            {
                if args.len() != 1 {
                    return None;
                }
                if let Some(GenericArgument::Type(ty)) = args.first() {
                    Some(ty)
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            None
        }
    }
    fn is_a(ty: &TypeParamBound, path: &[&str]) -> bool {
        if let TypeParamBound::Trait(TraitBound { path: Path { segments, .. }, .. }) = ty {
            if segments.len() > path.len() {
                return false;
            }
            if segments.iter().rev().zip(path.iter().rev()).any(|(seg, name)| seg.ident != name) {
                return false;
            }
            if segments.iter().rev().any(|seg| !matches!(seg.arguments, PathArguments::None)) {
                return false;
            }
            true
        } else {
            false
        }
    }
    match unwrap_type(ty, &["std", "sync", "Arc"]).map(unwrap_paren) {
        Some(Type::TraitObject(TypeTraitObject { bounds, .. })) if bounds.iter().any(|tr| is_a(tr, &["kirl_common", "interface", "KirlVMValueLock"])) => ParamType::NonCast,
        Some(ty) => match unwrap_type(ty, &["std", "sync", "RwLock"]) {
            Some(ty) => ParamType::Cast(ty),
            None => ParamType::Owned(ty),
        },
        None => ParamType::Owned(ty),
    }
}

pub(crate) fn kirl_function_inner(args: TokenStream, input: TokenStream) -> TokenStream {
    let ty = parse_macro_input!(args as TypeWithGenerics).expand_generics();
    let input = parse_macro_input!(input as ItemFn);
    let ItemFn {
        vis,
        sig: Signature {
            ident: function_name,
            generics: Generics { params, where_clause, .. },
            inputs,
            output,
            ..
        },
        ..
    } = &input;
    let where_clause = match where_clause {
        None => quote! {},
        Some(where_clause) => quote! { #where_clause },
    };
    if let HIRType::Function { arguments, .. } = &ty {
        assert_eq!(arguments.len(), inputs.len(), "count of argument is mismatched");
    } else {
        panic!("type should be Function");
    }
    let params_ref = params
        .iter()
        .map(|param| match param {
            GenericParam::Type(TypeParam { ident, .. }) => quote! { #ident },
            GenericParam::Lifetime(LifetimeDef { lifetime, .. }) => quote! { #lifetime },
            GenericParam::Const(ConstParam { ident, .. }) => quote! { #ident },
        })
        .collect::<Vec<_>>();
    let kirl_type = quote_type(&ty);
    let argument_count = inputs.len();
    let arguments = inputs
        .iter()
        .map(|arg| match arg {
            FnArg::Receiver(_) => panic!("receiver argument is not supported"),
            FnArg::Typed(PatType { ty, .. }) => get_param_type(ty),
        })
        .enumerate()
        .map(|(i, ty)| {
            let name = Ident::new(&format!("param{}", i), Span::call_site());
            match ty {
                ParamType::NonCast => quote! { #name },
                ParamType::Cast(ty) => quote! { <#ty as kirl_common::interface::InterchangeKirlVMValue>::try_from_kirl_value(#name).unwrap_or_else(|value|panic!("expected type {:?} but found {:?}.", std::any::type_name::<#ty>(), value.type_name())) },
                ParamType::Owned(ty) => quote! { {
                    let value = <#ty as kirl_common::interface::InterchangeKirlVMValue>::try_from_kirl_value(#name).unwrap_or_else(|value|panic!("expected type {:?} but found {:?}.", std::any::type_name::<#ty>(), value.type_name()));
                    std::sync::Arc::try_unwrap(value).map(|lock| lock.into_inner().expect("")).unwrap_or_else(|arc| arc.read().expect("").clone())
                } },
            }
        });
    let param_names = (0..argument_count).map(|i| Ident::new(&format!("param{}", i), Span::call_site()));
    let output_map = match output {
        ReturnType::Default => quote! { Ok(<() as kirl_common::interface::InterchangeKirlVMValue>::into_kirl_value(())) },
        ReturnType::Type(_, ty) => {
            if let Some((ty_ok, _)) = unwrap_result(ty) {
                match get_param_type(ty_ok) {
                    ParamType::NonCast => quote! { result.map_err(Into::into) },
                    ParamType::Cast(_) => quote! { result.map(|result| result as std::sync::Arc<std::sync::RwLock<dyn kirl_common::interface::KirlVMValueCloneable>>).map_err(Into::into) },
                    ParamType::Owned(ty) => quote! { result.map(<#ty as kirl_common::interface::InterchangeKirlVMValue>::into_kirl_value).map_err(Into::into) },
                }
            } else {
                match get_param_type(ty) {
                    ParamType::NonCast => quote! { Ok(result) },
                    ParamType::Cast(_) => quote! { Ok(result as std::sync::Arc<dyn kirl_common::interface::KirlVMValueLock>) },
                    ParamType::Owned(ty) => quote! { Ok(<#ty as kirl_common::interface::InterchangeKirlVMValue>::into_kirl_value(result)) },
                }
            }
        }
    };
    let result = quote! {
        #input
        #[allow(non_camel_case_types)]
        #vis struct #function_name<#params> #where_clause { phantom: std::marker::PhantomData<(#(#params_ref),*)> }
        impl<#params> Default for #function_name<#(#params_ref),*> #where_clause {
            fn default() -> Self {
                #function_name { phantom: Default::default() }
            }
        }
        impl<#params> #function_name<#(#params_ref),*> #where_clause {
            fn new() -> Self {
                #function_name { phantom: Default::default() }
            }
        }
        impl<#params> kirl_common::interface::KirlRustFunction for #function_name<#(#params_ref),*> #where_clause {
            fn static_type() -> std::borrow::Cow<'static, kirl_common::typing::HIRType> {
                static TYPE: kirl_common::once_cell::sync::Lazy<kirl_common::typing::HIRType> = kirl_common::once_cell::sync::Lazy::new(|| #kirl_type);
                std::borrow::Cow::Borrowed(&*TYPE)
            }
            fn argument_count(&self) -> usize {
                #argument_count
            }
            fn call(&mut self, args: Vec<std::sync::Arc<dyn kirl_common::interface::KirlVMValueLock>>) -> Result<std::sync::Arc<dyn kirl_common::interface::KirlVMValueLock>, Box<dyn std::error::Error>> {
                if let Ok([#(#param_names),*]) = <[std::sync::Arc<dyn kirl_common::interface::KirlVMValueLock>; #argument_count]>::try_from(args) {
                    let result = #function_name::<#(#params_ref),*>(#(#arguments),*);
                    #output_map
                } else {
                    unreachable!("invalid argument count in KirlRustFunction::call");
                }
            }
        }
    };
    result.into()
}
