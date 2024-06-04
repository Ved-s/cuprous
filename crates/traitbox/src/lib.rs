use std::ops::{Deref, Not};

use proc_macro2::{Group, Ident, Span, TokenStream, TokenTree};
use quote::{quote, ToTokens};
use syn::{ext::IdentExt, parse::Parse, spanned::Spanned};

#[proc_macro]
pub fn traitbox(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    match traitbox2(tokens.into()) {
        Ok(s) => s.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

enum ContainerType {
    Box,
}

impl syn::parse::Parse for ContainerType {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        input.step(|cursor| {
            if let Some((ident, rest)) = cursor.ident() {
                if ident == "box" {
                    Ok((Self::Box, rest))
                } else {
                    Err(cursor.error(format_args!("expected `box`, found `{}`", ident,)))
                }
            } else {
                Err(cursor.error("expected `box`"))
            }
        })
    }
}

struct Container {
    pub attrs: Vec<syn::Attribute>,
    pub vis: syn::Visibility,
    pub ty: ContainerType,
    pub name: syn::Ident,
}

impl syn::parse::Parse for Container {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let attrs = syn::Attribute::parse_outer(input)?;
        let vis = syn::Visibility::parse(input)?;
        let ty = ContainerType::parse(input)?;
        let name = syn::Ident::parse(input)?;
        <syn::Token![;]>::parse(input)?;

        Ok(Self {
            attrs,
            vis,
            ty,
            name,
        })
    }
}

enum MacroInputItem {
    Container(Container),
    Trait(syn::ItemTrait),
    Impl(Vec<syn::Attribute>, Vec<syn::ImplItemFn>),
}

impl syn::parse::Parse for MacroInputItem {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let attrs = syn::Attribute::parse_outer(input)?;
        let vis = syn::Visibility::parse(input)?;

        let c = input.cursor();
        let Some((ident, _)) = c.ident() else {
            let m = if matches!(vis, syn::Visibility::Inherited) {
                "expected `box`, `trait` or `impl`"
            } else {
                "expected `box` or `trait`"
            };
            return Err(syn::Error::new(c.span(), m));
        };
        let str = ident.to_string();

        match str.as_str() {
            "box" => {
                let mut c = Container::parse(input)?;
                c.attrs = attrs;
                c.vis = vis;
                Ok(Self::Container(c))
            }

            "auto" | "trait" => {
                let mut t = syn::ItemTrait::parse(input)?;
                t.attrs = attrs;
                t.vis = vis;
                Ok(Self::Trait(t))
            }

            "impl" if matches!(vis, syn::Visibility::Inherited) => {
                let _ = syn::Ident::parse_any(input);
                let content;
                syn::braced!(content in input);
                let mut fns = vec![];

                while !content.is_empty() {
                    fns.push(content.parse()?);
                }

                Ok(Self::Impl(attrs, fns))
            }

            _ => Err(syn::Error::new(ident.span(), "expected `box` or `trait`")),
        }
    }
}

/*
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum ContainerAccept {
    Sized,
}

impl ContainerAccept {
    fn parse_set(input: TokenStream, errors: &mut Vec<syn::Error>) -> HashSet<Self> {
        let parser = |input: syn::parse::ParseStream| {
            let mut set = HashSet::new();

            loop {
                let i = syn::Ident::parse_any(input)?;
                let s = i.to_string();
                let accept = match s.as_str() {
                    "sized" => Self::Sized,
                    _ => {
                        return Err(syn::Error::new(
                            i.span(),
                            format_args!("expected `sized`, got `{s}`"),
                        ))
                    }
                };

                if set.contains(&accept) {
                    return Err(syn::Error::new(
                        i.span(),
                        format_args!("`{s}` is already specified"),
                    ));
                }

                set.insert(accept);

                if input.is_empty() {
                    break;
                }

                <syn::Token![,]>::parse(input)?;

                if input.is_empty() {
                    break;
                }
            }

            Ok(set)
        };
        let out = syn::parse::Parser::parse2(parser, input);
        match out {
            Ok(s) => s,
            Err(e) => {
                errors.push(e);
                HashSet::new()
            }
        }
    }
}
*/

#[derive(Clone)]
enum CastFailBehavior {
    Panic,
    Expr(syn::Expr),
}

impl CastFailBehavior {
    pub fn parse(e: syn::Expr) -> Result<Self, syn::Error> {
        let span = e.span();

        let string_literal = match &e {
            syn::Expr::Lit(syn::ExprLit {
                lit: syn::Lit::Str(str),
                ..
            }) => Some(str),
            _ => None,
        };

        if let Some(str_lit) = string_literal {
            let str = str_lit.value();
            return match str.as_str() {
                "panic" => Ok(CastFailBehavior::Panic),
                _ => Err(syn::Error::new(
                    span,
                    format_args!("expected \"panic\", got \"{str}\""),
                )),
            };
        }

        let tokens = map_tokens(e.into_token_stream(), &mut |tt| match tt {
            TokenTree::Ident(i) if i == "Self" => TokenTree::Ident(Ident::new("Tinner", i.span())),
            tt => tt,
        });

        let expr = match syn::parse2(tokens) {
            Ok(e) => e,
            Err(_) => {
                return Err(syn::Error::new(
                    span,
                    "could not swap Self in the expression",
                ))
            }
        };

        Ok(Self::Expr(expr))
    }
}

#[allow(unused)]
struct ParsedContainer {
    // accept: HashSet<ContainerAccept>,
    downcast: bool,
    on_cast_fail: Option<CastFailBehavior>,
    ty: ContainerType,
    vis: syn::Visibility,
    name: String,
    lowercase_name: String,
    name_ident: syn::Ident,

    sized_vtable_name: syn::Ident,
}

impl ParsedContainer {
    fn parse(input: Container, errors: &mut Vec<syn::Error>) -> Self {
        let name = input.name.to_string();
        let mut container = Self {
            // accept: HashSet::new(),
            downcast: false,
            on_cast_fail: None,
            ty: input.ty,
            vis: input.vis,
            lowercase_name: name.to_lowercase(),
            name_ident: input.name.clone(),
            sized_vtable_name: syn::Ident::new(&format!("{}SizedVtable", name), input.name.span()),
            name,
        };

        for attr in input.attrs {
            let span = attr.span();
            let ident = attr.meta.path().get_ident();
            let string = ident.map(|i| i.to_string());

            match (string.as_deref(), attr.meta) {
                (Some("downcast"), syn::Meta::Path(_)) => {
                    container.downcast = true;
                }
                (Some("downcast"), _) => {
                    errors.push(syn::Error::new(span, "invalid downcast attribute, valid form is `#[downcast]`"))
                }

                // (Some("accept"), syn::Meta::List(l)) => {
                //     container.accept = ContainerAccept::parse_set(l.tokens, errors);
                // }
                // (Some("accept"), _) => {
                //     errors.push(syn::Error::new(span, "invalid accept attribute, valid form is `#[accept(...)]`"))
                // }

                (Some("on_cast_fail"), syn::Meta::NameValue(v)) => {
                    match CastFailBehavior::parse(v.value) {
                        Ok(b) => container.on_cast_fail = Some(b),
                        Err(e) => errors.push(e),
                    }
                }
                (Some("on_cast_fail"), _) => {
                    errors.push(syn::Error::new(span, "invalid on_cast_fail attribute, valid form is `#[on_cast_fail = ...]`"))
                }

                _ => errors.push(syn::Error::new(span, "invalid attribute, expected `#[downcast]` or `#[on_cast_fail = ...]`"))
            }
        }

        container
    }
}

#[derive(Clone, Copy)]
enum ValuePassType {
    Ref,
    Mut,
    Owned,
}

impl ValuePassType {
    pub fn parse(r: &syn::Receiver) -> Self {
        if r.reference.is_some() {
            if r.mutability.is_some() {
                Self::Mut
            } else {
                Self::Ref
            }
        } else {
            Self::Owned
        }
    }
}

impl quote::ToTokens for ValuePassType {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            ValuePassType::Owned => {}
            ValuePassType::Ref => tokens.extend(quote!(&)),
            ValuePassType::Mut => tokens.extend(quote!(&mut)),
        }
    }
}

struct ParsedTraitType {
    #[allow(unused)]
    name: String,
    name_ident: syn::Ident,
}

impl ParsedTraitType {
    pub fn parse(input: syn::TraitItemType, errors: &mut Vec<syn::Error>) -> Self {

        if !input.attrs.is_empty() {
            errors.push(syn::Error::new(input.ident.span(), "attributes on trait types are not supported"));
        }

        if input.generics.lt_token.is_some() {
            errors.push(syn::Error::new(input.generics.span(), "generics in trait types are not supported"));
        }

        if !input.bounds.is_empty() {
            errors.push(syn::Error::new(input.bounds.span(), "bounds on trait types are not supported.\nomit them"));
        }

        if let Some(def) = &input.default {
            errors.push(syn::Error::new(def.1.span(), "defaults for trait types are not supported"));
        }

        Self {
            name: input.ident.to_string(),
            name_ident: input.ident.clone(),
        }
    }
}

enum ParsedFnArgType {
    Type(syn::Type),
    TraitType(ValuePassType, syn::Path),
}

impl ParsedFnArgType {
    pub fn parse(ty: syn::Type) -> Self {
        match ty {
            syn::Type::Path(p) if p.path.segments.first().is_some_and(|s| s.ident == "Self") => {
                Self::TraitType(
                    ValuePassType::Owned,
                    syn::Path {
                        leading_colon: None,
                        segments: p.path.segments.into_iter().skip(1).collect(),
                    },
                )
            }
            syn::Type::Reference(r) => match *r.elem {
                syn::Type::Path(p)
                    if p.path.segments.first().is_some_and(|s| s.ident == "Self") =>
                {
                    let pass = if r.mutability.is_some() {
                        ValuePassType::Mut
                    } else {
                        ValuePassType::Ref
                    };

                    Self::TraitType(
                        pass,
                        syn::Path {
                            leading_colon: None,
                            segments: p.path.segments.into_iter().skip(1).collect(),
                        },
                    )
                }
                _ => Self::Type(syn::Type::Reference(r)),
            },
            ty => Self::Type(ty),
        }
    }
}

struct ParsedFnArg {
    on_cast_fail: Option<CastFailBehavior>,
    name: syn::Ident,
    ty: ParsedFnArgType,
}

impl ParsedFnArg {
    pub fn parse(pat: &syn::PatType, index: usize, errors: &mut Vec<syn::Error>) -> Self {
        let name = match pat.pat.deref() {
            syn::Pat::Ident(i) => i.ident.clone(),

            _ => syn::Ident::new(&format!("arg{index}"), pat.pat.span()),
        };

        let mut on_cast_fail = None;
        for attr in &pat.attrs {
            let span = attr.span();
            let ident = attr.meta.path().get_ident();
            let string = ident.map(|i| i.to_string());

            match (string.as_deref(), &attr.meta) {

                (Some("on_cast_fail"), syn::Meta::NameValue(v)) => {
                    match CastFailBehavior::parse(v.value.clone()) {
                        Ok(b) => on_cast_fail = Some(b),
                        Err(e) => errors.push(e),
                    }
                }
                (Some("on_cast_fail"), _) => {
                    errors.push(syn::Error::new(span, "invalid on_cast_fail attribute, valid form is `#[on_cast_fail = ...]`"))
                }

                _ => errors.push(syn::Error::new(span, "invalid attribute, expected `#[on_cast_fail = ...]`"))
            }
        }

        Self {
            name,
            ty: ParsedFnArgType::parse(pat.ty.deref().clone()),
            on_cast_fail,
        }
    }
}

enum ParsedFnRet {
    SelfType,
    Type(syn::Type),
    TraitType(syn::Path),
}

impl ParsedFnRet {
    pub fn parse(ty: syn::Type) -> Self {
        match ty {
            syn::Type::Path(p) if p.path.segments.first().is_some_and(|s| s.ident == "Self") => {
                if p.path.segments.len() == 1 {
                    Self::SelfType
                } else {
                    Self::TraitType(syn::Path {
                        leading_colon: None,
                        segments: p.path.segments.into_iter().skip(1).collect(),
                    })
                }
            }
            ty => Self::Type(ty),
        }
    }
}

struct ParsedTraitFn {
    name: String,
    name_ident: syn::Ident,
    self_pass: Option<ValuePassType>,
    args: Vec<ParsedFnArg>,
    ret: Option<ParsedFnRet>,
    on_cast_fail: Option<CastFailBehavior>,
}

impl ParsedTraitFn {
    pub fn parse(
        input: syn::TraitItemFn,
        errors: &mut Vec<syn::Error>,
    ) -> Result<Self, syn::Error> {
        let mut recv = None;

        if let Some(def) = &input.default {
            errors.push(syn::Error::new(def.span(), "defaults for trait functions are not supported"));
        }

        let args = input
            .sig
            .inputs
            .iter()
            .enumerate()
            .filter_map(|(i, a)| match a {
                syn::FnArg::Receiver(r) => {
                    if recv.is_some() {
                        Some(Err(syn::Error::new(r.span(), "duplicate receiver")))
                    } else {
                        recv = Some(r);
                        None
                    }
                }
                syn::FnArg::Typed(pat) => Some(Ok(ParsedFnArg::parse(pat, i + 1, errors))),
            })
            .collect::<Result<Vec<_>, _>>()?;
        let ret = match input.sig.output {
            syn::ReturnType::Default => None,
            syn::ReturnType::Type(_, ty) => Some(ParsedFnRet::parse(*ty)),
        };

        let mut on_cast_fail = None;
        for attr in input.attrs {
            let span = attr.span();
            let ident = attr.meta.path().get_ident();
            let string = ident.map(|i| i.to_string());

            match (string.as_deref(), attr.meta) {

                (Some("on_cast_fail"), syn::Meta::NameValue(v)) => {
                    match CastFailBehavior::parse(v.value) {
                        Ok(b) => on_cast_fail = Some(b),
                        Err(e) => errors.push(e),
                    }
                }
                (Some("on_cast_fail"), _) => {
                    errors.push(syn::Error::new(span, "invalid on_cast_fail attribute, valid form is `#[on_cast_fail = ...]`"))
                }

                _ => errors.push(syn::Error::new(span, "invalid attribute, expected `#[on_cast_fail = ...]`"))
            }
        }

        Ok(Self {
            name: input.sig.ident.to_string(),
            name_ident: input.sig.ident.clone(),
            self_pass: recv.map(ValuePassType::parse),
            args,
            ret,
            on_cast_fail,
        })
    }
}

struct ParsedTrait {
    #[allow(unused)]
    name: String,
    lowercase_name: String,
    name_ident: syn::Ident,
    auto: bool,
    as_impl: bool,
    types: Vec<ParsedTraitType>,
    fns: Vec<ParsedTraitFn>,
    on_cast_fail: Option<CastFailBehavior>,
}

impl ParsedTrait {
    pub fn parse(input: syn::ItemTrait, errors: &mut Vec<syn::Error>) -> Self {

        if input.generics.lt_token.is_some() {
            errors.push(syn::Error::new(input.generics.span(), "generics in traits are not supported"));
        }

        let name = input.ident.to_string();
        let lowercase_name = name.to_lowercase();

        let mut output = Self {
            name,
            lowercase_name,
            name_ident: input.ident.clone(),
            auto: input.auto_token.is_some(),
            types: vec![],
            fns: vec![],
            on_cast_fail: None,
            as_impl: false,
        };

        for item in input.items {
            match item {
                syn::TraitItem::Const(i) => {
                    errors.push(syn::Error::new(i.span(), "consts are not supported"))
                }
                syn::TraitItem::Macro(i) => errors.push(syn::Error::new(
                    i.span(),
                    "nested macro invocations aren't allowed",
                )),
                syn::TraitItem::Type(ty) => {
                    output.types.push(ParsedTraitType::parse(ty, errors));
                }
                syn::TraitItem::Fn(f) => match ParsedTraitFn::parse(f, errors) {
                    Ok(f) => output.fns.push(f),
                    Err(e) => errors.push(e),
                },
                i => errors.push(syn::Error::new(i.span(), "invalid trait item")),
            }
        }

        for attr in input.attrs {
            let span = attr.span();
            let ident = attr.meta.path().get_ident();
            let string = ident.map(|i| i.to_string());

            match (string.as_deref(), attr.meta) {

                (Some("on_cast_fail"), syn::Meta::NameValue(v)) => {
                    match CastFailBehavior::parse(v.value) {
                        Ok(b) => output.on_cast_fail = Some(b),
                        Err(e) => errors.push(e),
                    }
                }
                (Some("on_cast_fail"), _) => {
                    errors.push(syn::Error::new(span, "invalid on_cast_fail attribute, valid form is `#[on_cast_fail = ...]`"))
                }

                (Some("as_impl"), syn::Meta::Path(_)) => {
                    output.as_impl = true;
                }
                (Some("as_impl"), _) => {
                    errors.push(syn::Error::new(span, "invalid as_impl attribute, valid form is `#[as_impl]`"))
                }

                _ => errors.push(syn::Error::new(span, "invalid attribute, expected `#[as_impl]` or `#[on_cast_fail = ...]`"))
            }
        }

        if output.fns.iter().any(|f| f.self_pass.is_none()) && !output.as_impl {
            errors.push(syn::Error::new(
                output.name_ident.span(),
                "this trait contains functions that don't receive `self`.\n\
                mark it as #[as_impl] to generate trait's functions as type's functions.",
            ));
        }

        output
    }
}
/*
struct ParsedImplFnTypeBound {
    name: syn::Ident,
    traits: Vec<syn::Path>,
    static_lifetime: bool,
}

impl ParsedImplFnTypeBound {
    pub fn parse(input: syn::Generics, fn_span: Span) -> Result<Self, syn::Error> {
        let mut generics = input.params.into_iter();
        let generic = generics.next().filter(|_| generics.next().is_none());
        let generic_type = generic.and_then(|generic| match generic {
            syn::GenericParam::Type(ty) => Some(ty),
            _ => None,
        });

        let Some(generic_type) = generic_type else {
            return Err(syn::Error::new(
                fn_span,
                "function must have a single generic type parameter for inner type",
            ));
        };

        let where_clause = input.where_clause.filter(|w| !w.predicates.is_empty());

        let where_type = where_clause.map(|w| {
            let span = w.span();
            let mut predicates = w.predicates.into_iter();
            let single_predicate = predicates.next().filter(|_| predicates.next().is_none());

            let type_predicate = single_predicate.and_then(|p| match p {
                syn::WherePredicate::Type(ty) => Some(ty),
                _ => None,
            });
            let pre = type_predicate.and_then(|ty| {
                let bound_name = generic_type.ident.to_string();
                match ty.bounded_ty {
                    syn::Type::Path(ref p)
                        if p.path.get_ident().is_some_and(|i| i == bound_name.as_str()) =>
                    {
                        Some(ty)
                    }
                    _ => None,
                }
            });
            (span, pre)
        });

        let where_type = match where_type {
            None => None,
            Some((span, None)) => {
                return Err(syn::Error::new(
                    span,
                    "where clause can only specify bounds for the inner type",
                ))
            }
            Some((_, Some(ty))) => Some(ty),
        };

        let mut output = Self {
            name: generic_type.ident,
            traits: vec![],
            static_lifetime: false,
        };

        let mut add_bounds =
            |bounds: syn::punctuated::Punctuated<syn::TypeParamBound, syn::Token![+]>| {
                for bound in bounds.into_iter() {
                    let span = bound.span();
                    match bound {
                        syn::TypeParamBound::Trait(t) => output.traits.push(t.path),
                        syn::TypeParamBound::Lifetime(lt) if lt.ident == "static" => {
                            output.static_lifetime = true;
                        }
                        _ => {
                            return Err(syn::Error::new(
                                span,
                                "only trait and 'static bounds can be specified",
                            ))
                        }
                    }
                }
                Ok(())
            };

        add_bounds(generic_type.bounds)?;

        if let Some(where_type) = where_type {
            add_bounds(where_type.bounds)?;
        }

        Ok(output)
    }
}
*/

struct ParsedImplFn {
    name: String,
    name_ident: syn::Ident,
    self_pass: Option<ValuePassType>,
    type_bounds: syn::TypeParam,
    where_clause: Option<syn::WhereClause>,
    args: Vec<ParsedFnArg>,
    ret: Option<ParsedFnRet>,
    on_cast_fail: Option<CastFailBehavior>,
    block: syn::Block,
}
impl ParsedImplFn {
    fn parse(input: syn::ImplItemFn, errors: &mut Vec<syn::Error>) -> Result<Self, syn::Error> {
        let mut recv = None;

        let args = input
            .sig
            .inputs
            .iter()
            .enumerate()
            .filter_map(|(i, a)| match a {
                syn::FnArg::Receiver(r) => {
                    if recv.is_some() {
                        Some(Err(syn::Error::new(r.span(), "duplicate receiver")))
                    } else {
                        recv = Some(r);
                        None
                    }
                }
                syn::FnArg::Typed(pat) => Some(Ok(ParsedFnArg::parse(pat, i + 1, errors))),
            })
            .collect::<Result<Vec<_>, _>>()?;

        let ret = match input.sig.output {
            syn::ReturnType::Default => None,
            syn::ReturnType::Type(_, ty) => Some(ParsedFnRet::parse(*ty)),
        };

        let mut on_cast_fail = None;
        for attr in input.attrs {
            let span = attr.span();
            let ident = attr.meta.path().get_ident();
            let string = ident.map(|i| i.to_string());

            match (string.as_deref(), attr.meta) {

                (Some("on_cast_fail"), syn::Meta::NameValue(v)) => {
                    match CastFailBehavior::parse(v.value) {
                        Ok(b) => on_cast_fail = Some(b),
                        Err(e) => errors.push(e),
                    }
                }
                (Some("on_cast_fail"), _) => {
                    errors.push(syn::Error::new(span, "invalid on_cast_fail attribute, valid form is `#[on_cast_fail = ...]`"))
                }

                _ => errors.push(syn::Error::new(span, "invalid attribute, expected `#[on_cast_fail = ...]`"))
            }
        }

        let mut generics = input.sig.generics.params.into_iter();
        let generic = generics.next().filter(|_| generics.next().is_none());
        let type_bounds = generic.and_then(|generic| match generic {
            syn::GenericParam::Type(ty) => Some(ty),
            _ => None,
        });

        let Some(type_bounds) = type_bounds else {
            return Err(syn::Error::new(
                input.sig.ident.span(),
                "function must have a single generic type parameter for inner type",
            ));
        };

        Ok(Self {
            name: input.sig.ident.to_string(),
            name_ident: input.sig.ident.clone(),
            self_pass: recv.map(ValuePassType::parse),
            args,
            type_bounds,
            where_clause: input.sig.generics.where_clause,
            ret,
            on_cast_fail,
            block: input.block,
        })
    }
}

trait ParsedFn {
    fn name(&self) -> &str;
    fn name_ident(&self) -> &syn::Ident;
    fn self_pass(&self) -> Option<ValuePassType>;
    fn args(&self) -> &[ParsedFnArg];
    fn ret(&self) -> Option<&ParsedFnRet>;
}

impl ParsedFn for ParsedTraitFn {
    fn name(&self) -> &str {
        &self.name
    }
    fn name_ident(&self) -> &syn::Ident {
        &self.name_ident
    }
    fn self_pass(&self) -> Option<ValuePassType> {
        self.self_pass
    }
    fn args(&self) -> &[ParsedFnArg] {
        &self.args
    }
    fn ret(&self) -> Option<&ParsedFnRet> {
        self.ret.as_ref()
    }
}

impl ParsedFn for ParsedImplFn {
    fn name(&self) -> &str {
        &self.name
    }
    fn name_ident(&self) -> &syn::Ident {
        &self.name_ident
    }
    fn self_pass(&self) -> Option<ValuePassType> {
        self.self_pass
    }
    fn args(&self) -> &[ParsedFnArg] {
        &self.args
    }
    fn ret(&self) -> Option<&ParsedFnRet> {
        self.ret.as_ref()
    }
}

struct ParsedImpl {
    fns: Vec<ParsedImplFn>,
    on_cast_fail: Option<CastFailBehavior>,
}

impl ParsedImpl {
    pub fn parse(
        attrs: Vec<syn::Attribute>,
        fns: Vec<syn::ImplItemFn>,
        errors: &mut Vec<syn::Error>,
    ) -> Self {
        let mut out_fns = vec![];

        for f in fns {
            match ParsedImplFn::parse(f, errors) {
                Ok(f) => out_fns.push(f),
                Err(e) => errors.push(e),
            }
        }

        let mut on_cast_fail = None;
        for attr in attrs {
            let span = attr.span();
            let ident = attr.meta.path().get_ident();
            let string = ident.map(|i| i.to_string());

            match (string.as_deref(), attr.meta) {

                (Some("on_cast_fail"), syn::Meta::NameValue(v)) => {
                    match CastFailBehavior::parse(v.value) {
                        Ok(b) => on_cast_fail = Some(b),
                        Err(e) => errors.push(e),
                    }
                }
                (Some("on_cast_fail"), _) => {
                    errors.push(syn::Error::new(span, "invalid on_cast_fail attribute, valid form is `#[on_cast_fail = ...]`"))
                }

                _ => errors.push(syn::Error::new(span, "invalid attribute, expected `#[on_cast_fail = ...]`"))
            }
        }

        Self {
            fns: out_fns,
            on_cast_fail,
        }
    }
}

struct ParsedMacroInput {
    containers: Vec<ParsedContainer>,
    traits: Vec<ParsedTrait>,
    impls: Vec<ParsedImpl>,
    all_value_bounds: TokenStream,
}

impl ParsedMacroInput {
    fn parse(input: Vec<MacroInputItem>) -> Result<ParsedMacroInput, syn::Error> {
        let mut containers = vec![];
        let mut traits = vec![];
        let mut impls = vec![];
        let mut errors = vec![];

        for item in input {
            match item {
                MacroInputItem::Container(c) => {
                    containers.push(ParsedContainer::parse(c, &mut errors))
                }

                MacroInputItem::Trait(t) => traits.push(ParsedTrait::parse(t, &mut errors)),

                MacroInputItem::Impl(attrs, fns) => {
                    impls.push(ParsedImpl::parse(attrs, fns, &mut errors))
                }
            }
        }

        let mut drain = errors.drain(..);
        if let Some(mut e) = drain.next() {
            for other in drain {
                e.combine(other);
            }
            return Err(e);
        }

        let trait_names = traits.iter().map(|t| &t.name_ident);
        let all_value_bounds = quote! { #(#trait_names+)* Sized + 'static };

        Ok(ParsedMacroInput {
            containers,
            traits,
            impls,
            all_value_bounds,
        })
    }
}

fn map_tokens(tokens: TokenStream, f: &mut dyn FnMut(TokenTree) -> TokenTree) -> TokenStream {
    let mut output = vec![];
    for token in tokens.into_iter() {
        let token = f(token);
        if let TokenTree::Group(g) = token {
            let new_tokens = map_tokens(g.stream(), f);
            let mut group = Group::new(g.delimiter(), new_tokens);
            group.set_span(g.span());
            output.push(TokenTree::Group(group));
        } else {
            output.push(token);
        }
    }
    TokenStream::from_iter(output)
}

fn parse_input(input: syn::parse::ParseStream) -> syn::Result<Vec<MacroInputItem>> {
    let mut items = vec![];
    loop {
        items.push(MacroInputItem::parse(input)?);
        if input.is_empty() {
            break;
        }
    }
    Ok(items)
}

fn generate_vtable(c: &ParsedContainer, parsed: &ParsedMacroInput) -> TokenStream {
    let make_vtable_fn =
        |name: &str, has_self: bool, args: &[ParsedFnArg], ret: Option<&ParsedFnRet>| {
            let args = args.iter().map(|a| match &a.ty {
                ParsedFnArgType::Type(ty) => ty.to_token_stream(),
                ParsedFnArgType::TraitType(pass, _) => quote!(#pass Box<dyn std::any::Any>),
            });

            let return_type = ret
                .map(|ret| match ret {
                    ParsedFnRet::SelfType => {
                        let ident = &c.name_ident;
                        quote! { super::#ident }
                    }
                    ParsedFnRet::Type(ty) => ty.to_token_stream(),
                    ParsedFnRet::TraitType(_) => quote! { Box<dyn std::any::Any> },
                })
                .map(|r| quote! { -> #r });

            let s = has_self.then(|| quote! { std::ptr::NonNull<()>, });

            let name = Ident::new(name, Span::call_site());

            quote! {
                pub #name: fn(#s #(#args),*) #return_type
            }
        };

    let trait_fns = parsed.traits.iter().flat_map(|t| {
        t.fns.iter().map(|f| {
            let name = format!("trait_{}_{}", t.lowercase_name, f.name);
            make_vtable_fn(&name, f.self_pass.is_some(), &f.args, f.ret.as_ref())
        })
    });

    let impl_fns = parsed.impls.iter().flat_map(|i| {
        i.fns.iter().map(|f| {
            let name = format!("impl_{}", f.name);
            make_vtable_fn(&name, f.self_pass.is_some(), &f.args, f.ret.as_ref())
        })
    });

    let name = &c.sized_vtable_name;
    let type_id_fn = c
        .downcast
        .then(|| quote! { pub type_id: fn() -> std::any::TypeId, });

    quote! {
        pub struct #name {
            pub size: usize,
            pub align: usize,
            pub drop: fn(std::ptr::NonNull<()>),
            #type_id_fn

            #(#impl_fns,)*
            #(#trait_fns,)*
        }
    }
}

fn generate_new(c: &ParsedContainer, parsed: &ParsedMacroInput) -> TokenStream {
    let all_value_bounds = &parsed.all_value_bounds;

    let type_id_decl = c.downcast.then(|| {
        quote! {
            fn type_id<Tinner: 'static>() -> std::any::TypeId {
                std::any::TypeId::of::<Tinner>()
            }
        }
    });

    struct Intermediate<'a> {
        name: Ident,
        vtable_name: Ident,
        args: &'a [ParsedFnArg],
        ret: Option<&'a ParsedFnRet>,
        self_pass: Option<ValuePassType>,
        on_cast_fail: Option<&'a CastFailBehavior>,
        bounds: TokenStream,
        call_prefix: Option<TokenStream>,
        extra_defs: Option<TokenStream>,
        pass_type_arg: bool,
    }

    let trait_fn_decls = parsed.traits.iter().flat_map(|t| {
        t.fns.iter().map(|f| {
            let vtable_name = format!("trait_{}_{}", t.lowercase_name, f.name);
            let vtable_name = syn::Ident::new(&vtable_name, f.name_ident.span());
            let on_cast_fail = f
                .on_cast_fail
                .as_ref()
                .or(t.on_cast_fail.as_ref())
                .or(c.on_cast_fail.as_ref());
            let bounds = if matches!(f.ret, Some(ParsedFnRet::SelfType)) {
                parsed.all_value_bounds.clone()
            } else {
                t.name_ident.to_token_stream()
            };

            Intermediate {
                name: f.name_ident.clone(),
                vtable_name,
                args: &f.args,
                ret: f.ret.as_ref(),
                self_pass: f.self_pass,
                on_cast_fail,
                bounds,
                call_prefix: Some(quote! { Tinner:: }),
                extra_defs: None,
                pass_type_arg: false,
            }
        })
    });

    let impl_fn_decls = parsed.impls.iter().flat_map(|i| {
        i.fns.iter().map(|f| {
            let vtable_name = format!("impl_{}", f.name);
            let vtable_name = syn::Ident::new(&vtable_name, f.name_ident.span());
            let on_cast_fail = f
                .on_cast_fail
                .as_ref()
                .or(i.on_cast_fail.as_ref())
                .or(c.on_cast_fail.as_ref());

            let type_name = &f.type_bounds.ident;
            let name = &f.name_ident;
            let bounds = &f.type_bounds;
            let self_pass = f.self_pass.map(|p| quote! { #p self, });

            let args = f.args.iter().map(|a| {
                let name = &a.name;
                match &a.ty {
                    ParsedFnArgType::Type(ty) => quote! { #name: #ty },
                    ParsedFnArgType::TraitType(pass, p) => {
                        quote!(#name: #pass #type_name::#p)
                    }
                }
            });

            let return_type = f
                .ret
                .as_ref()
                .map(|ret| match ret {
                    ParsedFnRet::SelfType => quote! { Self },
                    ParsedFnRet::Type(ty) => ty.to_token_stream(),
                    ParsedFnRet::TraitType(p) => quote!(#type_name::#p),
                })
                .map(|r| quote! { -> #r });

            let where_clause = f.where_clause.as_ref();
            let block = &f.block;

            let extra_defs = quote! {
                fn #name<#bounds>(#self_pass #(#args,)*) #return_type #where_clause
                #block
            };

            Intermediate {
                name: f.name_ident.clone(),
                vtable_name,
                args: &f.args,
                ret: f.ret.as_ref(),
                self_pass: f.self_pass,
                on_cast_fail,
                bounds: parsed.all_value_bounds.clone(),
                call_prefix: None,
                extra_defs: Some(extra_defs),
                pass_type_arg: true,
            }
        })
    });

    let intermediates = impl_fn_decls.chain(trait_fn_decls);

    let fn_decls = intermediates.map(|i| {
        let args = i.args.iter().map(|a| {
            let name = &a.name;
            match &a.ty {
                ParsedFnArgType::Type(ty) => quote! { #name: #ty },
                ParsedFnArgType::TraitType(pass, _) => quote!(#name: #pass Box<dyn std::any::Any>),
            }
        });

        let return_type = i
            .ret
            .as_ref()
            .map(|ret| match ret {
                ParsedFnRet::SelfType => c.name_ident.clone().into_token_stream(),
                ParsedFnRet::Type(ty) => ty.to_token_stream(),
                ParsedFnRet::TraitType(_) => quote! { Box<dyn std::any::Any> },
            })
            .map(|r| quote! { -> #r });

        let deref_this = i.self_pass.as_ref().map(|p| match p {
            ValuePassType::Ref => quote! { let this = unsafe { this.cast::<Tinner>().as_ref() }; },
            ValuePassType::Mut => quote! { let this = unsafe { this.cast::<Tinner>().as_mut() }; },
            ValuePassType::Owned => quote! { let this = unsafe { this.cast::<Tinner>().read() }; },
        });

        let arg_proc = i.args.iter().filter_map(|a| {
            let ParsedFnArgType::TraitType(pass, ty) = &a.ty else {
                return None;
            };

            let downcast = match pass {
                ValuePassType::Ref => quote! { .downcast_ref::<Tinner:: #ty>() },
                ValuePassType::Mut => quote! { .downcast_mut::<Tinner:: #ty>() },
                ValuePassType::Owned => quote! { .downcast::<Tinner:: #ty>().ok().map(|b| *b) },
            };

            let name = &a.name;

            let on_cast_fail = a.on_cast_fail.as_ref().or(i.on_cast_fail);

            let invalid_cast = match on_cast_fail {
                Some(CastFailBehavior::Panic) => {
                    quote! {
                        panic!(
                            "invalid type cast! expected {}",
                            std::any::type_name::<Tinner:: #ty>()
                        );
                    }
                }
                Some(CastFailBehavior::Expr(e)) => e.to_token_stream(),
                None => {
                    let error =
                        "#[on_cast_fail = ...] not specified but type downcast may fail";
                    let error = syn::Error::new(name.span().join(ty.span()).unwrap_or(name.span()), error);
                    error.into_compile_error()
                }
            };

            Some(quote! {
                let #name = match #name #downcast {
                    Some(a) => a,
                    None => { #invalid_cast }
                };
            })
        });

        let arg_names = i.args.iter().map(|a| &a.name);

        let call_prefix = i.call_prefix;
        let name = &i.name;
        let this = i.self_pass.as_ref().map(|_| quote! { this, });

        let type_arg = i.pass_type_arg.then(|| quote! { ::<Tinner> });

        let call = quote! { #call_prefix #name #type_arg (#this #(#arg_names),* ) };

        let contatiner = &c.name_ident;

        let call_and_return = match i.ret {
            Some(ParsedFnRet::SelfType) => quote! { #contatiner::new(#call) },
            Some(ParsedFnRet::TraitType(_)) => quote! { Box::new(#call) },
            Some(ParsedFnRet::Type(_)) => call,
            None => quote! { #call; },
        };

        let tt_paths = i.args.iter().filter_map(|a| match &a.ty {
            ParsedFnArgType::TraitType(_, tt) => Some(tt),
            _ => None,
        });
        let ret_tt_path = i.ret.and_then(|r| match r {
            ParsedFnRet::TraitType(tt) => Some(tt),
            _ => None,
        });

        let mut static_bounds = tt_paths
            .chain(ret_tt_path)
            .map(|p| quote! { Tinner::#p: 'static })
            .peekable();

        let static_bounds = static_bounds
            .peek()
            .is_some()
            .then(|| quote! { where #(#static_bounds,)* });

        let vtable_name = i.vtable_name;
        let this_arg = i
            .self_pass
            .is_some()
            .then(|| quote! { this: std::ptr::NonNull<()>, });
        let bounds = i.bounds;
        let extra_defs = &i.extra_defs;

        quote! {
            fn #vtable_name<Tinner: #bounds>(#this_arg #(#args),*) #return_type #static_bounds {
                #extra_defs
                #(#arg_proc)*
                #deref_this
                #call_and_return
            }
        }
    });

    let mod_name = syn::Ident::new(&format!("_{}_impl", c.lowercase_name), c.name_ident.span());
    let type_id_construct = c.downcast.then(|| {
        quote! {
            type_id: type_id::<T>,
        }
    });

    let vtable_impl_names = parsed.impls.iter().flat_map(|i| {
        i.fns.iter().map(|f| {
            let vtable_name = format!("impl_{}", f.name);
            syn::Ident::new(&vtable_name, f.name_ident.span())
        })
    });

    let vtable_trait_names = parsed.traits.iter().flat_map(|t| {
        t.fns.iter().map(|f| {
            let vtable_name = format!("trait_{}_{}", t.lowercase_name, f.name);
            syn::Ident::new(&vtable_name, f.name_ident.span())
        })
    });

    let vtable_constructs = vtable_impl_names.chain(vtable_trait_names).map(|n| {
        quote! {
            #n: #n::<T>,
        }
    });

    let sized_vtable_name = &c.sized_vtable_name;

    quote! {
        pub fn new<T: #all_value_bounds>(value: T) -> Self {
            let ptr = std::ptr::NonNull::from(Box::leak(Box::new(value))).cast();

            fn drop<Tinner>(p: std::ptr::NonNull<()>) {
                unsafe { p.cast::<Tinner>().drop_in_place() }
            }

            #type_id_decl
            #(#fn_decls)*

            Self {
                data: ptr,
                vtable: &#mod_name::#sized_vtable_name {
                    size: std::mem::size_of::<T>(),
                    align: std::mem::align_of::<T>(),
                    drop: drop::<T>,
                    #type_id_construct
                    #(#vtable_constructs)*
                },
            }
        }
    }
}

fn generate_impl(c: &ParsedContainer, parsed: &ParsedMacroInput) -> TokenStream {
    let name = &c.name_ident;

    let downcast_fns = c.downcast.then(|| {
        let all_value_bounds = &parsed.all_value_bounds;
        quote! {
            pub fn type_id(&self) -> std::any::TypeId {
                (self.vtable.type_id)()
            }

            pub fn downcast<T: #all_value_bounds>(
                mut self,
            ) -> Result<Box<T>, Self> {
                if self.type_id() != std::any::TypeId::of::<T>() {
                    return Err(self);
                }
                let b = unsafe { Box::from_raw(self.data.cast().as_ptr()) };
                std::mem::forget(self);
                Ok(b)
            }

            pub fn downcast_ref<T: #all_value_bounds>(&self) -> Option<&T> {
                if self.type_id() != std::any::TypeId::of::<T>() {
                    return None;
                }
                Some(unsafe { self.data.cast().as_ref() })
            }

            pub fn downcast_mut<T: #all_value_bounds>(
                &mut self,
            ) -> Option<&mut T> {
                if self.type_id() != std::any::TypeId::of::<T>() {
                    return None;
                }
                Some(unsafe { self.data.cast().as_mut() })
            }
        }
    });

    let new = generate_new(c, parsed);

    let as_impl_trait_fns = parsed
        .traits
        .iter()
        .filter(|&t| t.as_impl)
        .map(|t| generate_impl_fns(c, Some(t), &t.fns, &format!("trait_{}_", t.lowercase_name)));

    let impl_fns = parsed
        .impls
        .iter()
        .map(|i| generate_impl_fns(c, None, &i.fns, "impl_"));

    quote! {
        impl #name {
            #new

            #(#as_impl_trait_fns)*
            #(#impl_fns)*

            #downcast_fns

            fn drop_value(&mut self) {
                (self.vtable.drop)(self.data);
                let size = self.vtable.size;
                let align = self.vtable.align;
                unsafe {
                    let layout = std::alloc::Layout::from_size_align_unchecked(size, align);
                    std::alloc::dealloc(self.data.cast().as_ptr(), layout);
                }
            }

            fn into_raw(mut self) -> std::ptr::NonNull<()> {
                let d = self.data;
                std::mem::forget(self);
                d
            }
        }
    }
}

fn generate_impl_fns<F: ParsedFn>(
    c: &ParsedContainer,
    t: Option<&ParsedTrait>,
    fns: &[F],
    prefix: &str,
) -> TokenStream {
    let fns = fns.iter().map(|f| {
        let name = &f.name_ident();
        let args = f.args().iter().map(|a| {
            let name = &a.name;
            match &a.ty {
                ParsedFnArgType::Type(ty) => quote! { #name: #ty },
                ParsedFnArgType::TraitType(pass, _) => {
                    quote!(#name: #pass Box<dyn std::any::Any>)
                }
            }
        });

        let return_type = f
            .ret()
            .map(|ret| match ret {
                ParsedFnRet::SelfType => quote! { Self },
                ParsedFnRet::Type(ty) => ty.to_token_stream(),
                ParsedFnRet::TraitType(_) => quote! { Box<dyn std::any::Any> },
            })
            .map(|r| quote! { -> #r });
        let vtable_name = format!("{}{}", prefix, f.name());

        // Specify call_site span so rust-analyzer won't color it as a field
        let vtable_name = syn::Ident::new(&vtable_name, Span::call_site());

        let arg_names = f.args().iter().map(|a| &a.name);

        let call = if matches!(f.self_pass(), Some(ValuePassType::Owned)) {
            let data_deref = f
                .self_pass()
                .is_some()
                .then(|| quote! { let data = self.into_raw(); });
            let data_pass = f.self_pass().is_some().then(|| quote! { data, });
            quote! {
                let func = self.vtable.#vtable_name;

                #data_deref

                (func)(#data_pass #(#arg_names,)*)
            }
        } else {
            let data_pass = f.self_pass().is_some().then(|| quote! { self.data, });
            quote! { (self.vtable.#vtable_name)(#data_pass #(#arg_names,)*) }
        };

        let self_pass = f.self_pass().unwrap_or(ValuePassType::Ref);

        let vis = t.is_some_and(|t| !t.as_impl).not().then(|| quote!(pub));

        quote! {
            #vis fn #name(#self_pass self #(,#args)*) #return_type {
                #call
            }
        }
    });

    if let Some(t) = t {
        if t.as_impl {
            quote! {
                #(#fns)*
            }
        } else {
            let types = t.types.iter().map(|t| {
                let name = &t.name_ident;
                quote! { type #name = Box::<dyn std::any::Any>; }
            });

            let uns = t.auto.then(|| quote! { unsafe });

            let name = &c.name_ident;
            let trait_name = &t.name_ident;

            quote! {
                #uns impl #trait_name for #name {
                    #(#types)*
                    #(#fns)*
                }
            }
        }
    } else {
        quote! {
            #(#fns)*
        }
    }
}

fn generate_container(c: &ParsedContainer, parsed: &ParsedMacroInput) -> TokenStream {
    let mod_name = syn::Ident::new(&format!("_{}_impl", c.lowercase_name), c.name_ident.span());
    let vtable = generate_vtable(c, parsed);
    let imp = generate_impl(c, parsed);

    let name = &c.name_ident;
    let vtable_name = &c.sized_vtable_name;
    let vis = &c.vis;

    let trait_impls = parsed
        .traits
        .iter()
        .filter(|&t| t.as_impl.not())
        .map(|t| generate_impl_fns(c, Some(t), &t.fns, &format!("trait_{}_", t.lowercase_name)));

    quote! {
        mod #mod_name {
            #vtable
        }
        #vis struct #name {
            data: std::ptr::NonNull<()>,
            vtable: &'static #mod_name::#vtable_name,
        }

        #imp

        impl Drop for #name {
            fn drop(&mut self) {
                self.drop_value();
            }
        }

        #(#trait_impls)*
    }
}

fn traitbox2(tokens: TokenStream) -> Result<TokenStream, syn::Error> {
    let items = syn::parse::Parser::parse2(parse_input, tokens)?;
    let parsed = ParsedMacroInput::parse(items)?;

    let container_impls = parsed
        .containers
        .iter()
        .map(|c| generate_container(c, &parsed));

    Ok(quote! {
        #(#container_impls)*
    })
}
