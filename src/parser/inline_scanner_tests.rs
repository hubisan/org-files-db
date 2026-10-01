//! Tests of `inline_scanner`. Expectations of the tables come from Emacs 29.3 (Org 9.6.15):
//! `org-element-map` over `org-element-parse-buffer` of `* H` and the text, listing the
//! `timestamp` objects and the `code`, `verbatim`, `inline-src-block` and `export-snippet`
//! objects; titles are the visible text of the parsed headline title (emphasis markers
//! dropped, links replaced by their description or target, statistics cookies removed).
//! `scripts/emacs-oracle.py` covers links and headline facts against Emacs as well.

use std::collections::HashSet;

use super::inline_scanner::{normalize_title_text, scan_inline};
use super::line_index::LineIndex;
use super::link_scanner::{plain_link_protocol_set, LinkScannerConfig};
use super::model::TodoKeywordConfig;
use super::structure_scanner::scan_structure;

fn protocols() -> HashSet<String> {
    plain_link_protocol_set(&LinkScannerConfig::default())
}

/// Timestamps and ignored ranges (code, verbatim, inline source, snippet) of `content`, as
/// source slices in source order.
fn facts_of_content(content: &str) -> (Vec<String>, Vec<String>) {
    let structure = scan_structure(content);
    let facts = scan_inline(
        content,
        &structure,
        &LineIndex::new(content),
        &protocols(),
        &TodoKeywordConfig::default(),
    );
    let timestamps = facts
        .timestamps
        .iter()
        .map(|timestamp| timestamp.raw_value.clone())
        .collect();
    let mut ranges = facts.ignored_ranges;
    ranges.sort_by_key(|range| (range.start, range.end));
    (
        timestamps,
        ranges
            .into_iter()
            .map(|range| content[range].to_string())
            .collect(),
    )
}

/// The facts of `text` below a headline.
fn facts(text: &str) -> (Vec<String>, Vec<String>) {
    facts_of_content(&format!("* H\n{text}\n"))
}

/// (text, timestamps, ignored ranges)
type Case = (
    &'static str,
    &'static [&'static str],
    &'static [&'static str],
);

fn check(cases: &[Case]) {
    let mut failures = Vec::new();
    for (text, timestamps, ignored) in cases {
        let (got_timestamps, got_ignored) = facts(text);
        if got_timestamps != *timestamps || got_ignored != *ignored {
            failures.push(format!(
                "{text:?}\n  want {timestamps:?} {ignored:?}\n  got  {got_timestamps:?} {got_ignored:?}"
            ));
        }
    }
    assert!(
        failures.is_empty(),
        "{} of {} cases differ from Emacs:\n{}",
        failures.len(),
        cases.len(),
        failures.join("\n")
    );
}

#[test]
fn timestamps_follow_org_element() {
    check(&[
        ("<2024-04-01 Mon>", &["<2024-04-01 Mon>"], &[]),
        ("[2024-04-01 Mon 10:00]", &["[2024-04-01 Mon 10:00]"], &[]),
        (
            "<2024-04-01 Mon>--<2024-04-03 Wed>",
            &["<2024-04-01 Mon>--<2024-04-03 Wed>"],
            &[],
        ),
        (
            "[2024-04-01 Mon]--[2024-04-02 Tue 10:00]",
            &["[2024-04-01 Mon]--[2024-04-02 Tue 10:00]"],
            &[],
        ),
        (
            "<2024-04-01 Mon>--[2024-04-03 Wed]",
            &["<2024-04-01 Mon>--[2024-04-03 Wed]"],
            &[],
        ),
        (
            "<2024-04-01 Mon>-<2024-04-03 Wed>",
            &["<2024-04-01 Mon>", "<2024-04-03 Wed>"],
            &[],
        ),
        (
            "<2024-04-01 Mon 10:00-11:30>",
            &["<2024-04-01 Mon 10:00-11:30>"],
            &[],
        ),
        ("<2024-04-01 Mon +1w>", &["<2024-04-01 Mon +1w>"], &[]),
        (
            "<2024-04-01 Mon ++1y --3d>",
            &["<2024-04-01 Mon ++1y --3d>"],
            &[],
        ),
        (
            "<2024-04-01 Mon .+1m/2m>",
            &["<2024-04-01 Mon .+1m/2m>"],
            &[],
        ),
        ("<2024-04-01 Mon +1x>", &["<2024-04-01 Mon +1x>"], &[]),
        ("<2024-04-01 Mon 9:00>", &["<2024-04-01 Mon 9:00>"], &[]),
        (
            "<2024-04-01 Mon 10:00--11:00>",
            &["<2024-04-01 Mon 10:00--11:00>"],
            &[],
        ),
        ("<2024-04-01>", &["<2024-04-01>"], &[]),
        ("<2024-04-01 >", &["<2024-04-01 >"], &[]),
        ("<%%(diary-float t 3)>", &["<%%(diary-float t 3)>"], &[]),
        ("<%%(a [b] c)> x", &["<%%(a [b]"], &[]),
        ("<%%()>", &[], &[]),
        ("[%%(x)]", &[], &[]),
        ("<2024-04-01", &[], &[]),
        ("<2024-04-01x>", &[], &[]),
        ("<2024-4-1 Mon>", &[], &[]),
        ("<2024-04-01 Mon]", &["<2024-04-01 Mon]"], &[]),
        ("[2024-04-01 Mon>", &["[2024-04-01 Mon>"], &[]),
        (
            "<2024-04-01  <2024-05-05>",
            &["<2024-04-01  <2024-05-05>"],
            &[],
        ),
        (
            "<2024-04-01<2024-05-05 Mon +1d>",
            &["<2024-04-01<2024-05-05 Mon +1d>"],
            &[],
        ),
        ("<2024-04-01+1d>", &["<2024-04-01+1d>"], &[]),
        ("<2024-04-01x +1d>", &["<2024-04-01x +1d>"], &[]),
        ("<2024-04-01x +1d]", &[], &[]),
        ("<2024-13-45>", &["<2024-13-45>"], &[]),
        ("<2024-04-01 Mon 25:61>", &["<2024-04-01 Mon 25:61>"], &[]),
        ("a<2024-04-01 Mon>b", &["<2024-04-01 Mon>"], &[]),
        ("=<2024-04-01 Mon>=", &[], &["=<2024-04-01 Mon>="]),
        ("~<2024-04-01 Mon>~", &[], &["~<2024-04-01 Mon>~"]),
        (
            " =a <2024-04-01 Mon>= <2024-05-05>",
            &["<2024-05-05>"],
            &["=a <2024-04-01 Mon>="],
        ),
        ("[[a][<2024-04-01 Mon>]]", &[], &[]),
        ("[[file:a<2024-04-01 Mon>.org]]", &[], &[]),
        ("[[a][*<2024-04-01 Mon>*]]", &["<2024-04-01 Mon>"], &[]),
        ("<https://x.org/<2024-04-01 Mon>>", &[], &[]),
        ("*a <2024-04-01 Mon> b*", &["<2024-04-01 Mon>"], &[]),
        ("/a [2024-04-01 Mon] b/", &["[2024-04-01 Mon]"], &[]),
        ("[fn:1:<2024-04-01 Mon>]", &["<2024-04-01 Mon>"], &[]),
        ("[fn::x <2024-04-01 Mon>]", &["<2024-04-01 Mon>"], &[]),
        ("a_{<2024-04-01 Mon>}", &["<2024-04-01 Mon>"], &[]),
        ("a^{b <2024-04-01 Mon>}", &["<2024-04-01 Mon>"], &[]),
        (
            "src_sh{<2024-04-01 Mon>}",
            &[],
            &["src_sh{<2024-04-01 Mon>}"],
        ),
        (
            "@@html:<2024-04-01 Mon>@@",
            &[],
            &["@@html:<2024-04-01 Mon>@@"],
        ),
        ("{{{m(<2024-04-01 Mon>)}}}", &[], &[]),
        ("$<2024-04-01 Mon>$", &[], &[]),
        ("\\(<2024-04-01 Mon>\\)", &[], &[]),
        ("<<<2024-04-01 Mon>>>", &[], &[]),
        ("<<t <2024-04-01 Mon>>>", &["<2024-04-01 Mon>"], &[]),
        ("\\alpha[2024-04-01 Mon]", &["[2024-04-01 Mon]"], &[]),
        ("\\foo[2024-04-01 Mon]", &[], &[]),
        ("\\foo{<2024-04-01 Mon>}", &[], &[]),
        ("| <2024-04-01 Mon> | b |", &["<2024-04-01 Mon>"], &[]),
        ("| a =b | <2024-04-01 Mon> c= |", &["<2024-04-01 Mon>"], &[]),
        (
            "|---+---|\n| <2024-04-01 Mon> |",
            &["<2024-04-01 Mon>"],
            &[],
        ),
        ("+---+---+\n| <2024-04-01 Mon> |\n+---+---+", &[], &[]),
        (
            "+---+\n+ <2024-04-01 Mon>\n\n<2024-05-05>",
            &["<2024-04-01 Mon>", "<2024-05-05>"],
            &[],
        ),
        (
            "- <2024-04-01 Mon> :: <2024-05-05>",
            &["<2024-04-01 Mon>", "<2024-05-05>"],
            &[],
        ),
        ("- a :: <2024-04-01 Mon> b", &["<2024-04-01 Mon>"], &[]),
        ("1. <2024-04-01 Mon> :: b", &["<2024-04-01 Mon>"], &[]),
        (
            "- [ ] <2024-04-01 Mon> :: <2024-05-05>",
            &["<2024-04-01 Mon>", "<2024-05-05>"],
            &[],
        ),
        ("- =a :: b=", &[], &[]),
        ("- =a :: b :: c= x", &[], &[]),
        ("- a\n<2024-04-01 Mon>=x\n  y=", &["<2024-04-01 Mon>"], &[]),
        ("- =a\n- b=", &[], &[]),
        ("=a\n- b=", &[], &[]),
        ("- a\n  =b\n  c=", &[], &["=b\n  c="]),
        ("- a\n =b\nc=", &[], &[]),
        (
            "[fn:1] =a\n[fn:2] b= <2024-04-01 Mon>",
            &["<2024-04-01 Mon>"],
            &[],
        ),
        ("a\n%%(x) <2024-04-01 Mon>\nb", &[], &[]),
        (
            "\\begin{x}\n<2024-04-01 Mon>\n\\end{x}\n<2024-05-05>",
            &["<2024-05-05>"],
            &[],
        ),
        ("\\begin{x}\n<2024-04-01 Mon>", &["<2024-04-01 Mon>"], &[]),
        ("-----\n<2024-04-01 Mon>", &["<2024-04-01 Mon>"], &[]),
        ("a =b\n\n<2024-04-01 Mon> c=", &["<2024-04-01 Mon>"], &[]),
        (
            "#+begin_verse\na =b\n\n<2024-04-01 Mon> c=\n#+end_verse",
            &[],
            &["=b\n\n<2024-04-01 Mon> c="],
        ),
        (
            "#+begin_quote\na =b\n\n<2024-04-01 Mon> c=\n#+end_quote",
            &["<2024-04-01 Mon>"],
            &[],
        ),
        ("#+begin_src sh\n<2024-04-01 Mon>\n#+end_src", &[], &[]),
        (
            ":LOGBOOK:\n<2024-04-01 Mon>\n:END:",
            &["<2024-04-01 Mon>"],
            &[],
        ),
        ("# <2024-04-01 Mon>", &[], &[]),
        (": <2024-04-01 Mon>", &[], &[]),
        ("#+CAPTION: <2024-04-01 Mon>", &[], &[]),
        ("日本語<2024-04-01 Mon>é", &["<2024-04-01 Mon>"], &[]),
        (
            "😀 [2024-04-01 Mon 10:00] 😀",
            &["[2024-04-01 Mon 10:00]"],
            &[],
        ),
    ]);
}

#[test]
fn emphasis_code_snippets_and_inline_source_follow_org_element() {
    check(&[
        ("x =v=", &[], &["=v="]),
        ("x-=v=", &[], &["=v="]),
        ("x(=v=", &[], &["=v="]),
        ("x'=v=", &[], &["=v="]),
        ("x\"=v=", &[], &["=v="]),
        ("x{=v=", &[], &["=v="]),
        ("xa=v=", &[], &[]),
        ("x.=v=", &[], &[]),
        ("x)=v=", &[], &[]),
        ("x[=v=", &[], &[]),
        ("x\u{a0}=v=", &[], &["=v="]),
        ("x\u{2003}=v=", &[], &["=v="]),
        ("x\u{2028}=v=", &[], &[]),
        ("x\t=v=", &[], &["=v="]),
        ("x*=v=", &[], &[]),
        ("x$=v=", &[], &[]),
        ("xé=v=", &[], &[]),
        ("x =v= y", &[], &["=v="]),
        ("x =v=-y", &[], &["=v="]),
        ("x =v=.y", &[], &["=v="]),
        ("x =v=,y", &[], &["=v="]),
        ("x =v=;y", &[], &["=v="]),
        ("x =v=:y", &[], &["=v="]),
        ("x =v=!y", &[], &["=v="]),
        ("x =v=?y", &[], &["=v="]),
        ("x =v='y", &[], &["=v="]),
        ("x =v=\"y", &[], &["=v="]),
        ("x =v=)y", &[], &["=v="]),
        ("x =v=}y", &[], &["=v="]),
        ("x =v=\\y", &[], &["=v="]),
        ("x =v=[y", &[], &["=v="]),
        ("x =v=ay", &[], &[]),
        ("x =v=(y", &[], &[]),
        ("x =v=*y", &[], &[]),
        ("x =v==y", &[], &[]),
        ("x =v=\u{a0}y", &[], &["=v="]),
        ("x =v=\u{2028}y", &[], &[]),
        ("x =v=]y", &[], &[]),
        ("x =v={y", &[], &[]),
        ("x =v=/y", &[], &[]),
        ("x =v=_y", &[], &[]),
        ("=v=", &[], &["=v="]),
        ("~c~", &[], &["~c~"]),
        ("=v= ~c~", &[], &["=v=", "~c~"]),
        ("=a=b=", &[], &["=a=b="]),
        ("==v=", &[], &["==v="]),
        ("=v==", &[], &["=v=="]),
        ("= a=", &[], &[]),
        ("=a =", &[], &[]),
        ("=a  b=", &[], &["=a  b="]),
        ("a =b\nc= d", &[], &["=b\nc="]),
        ("a =b\n\nc= d", &[], &[]),
        ("a =b\nc\nd= e", &[], &["=b\nc\nd="]),
        ("a =b\nc\nd\n\ne= f", &[], &[]),
        ("*a ~b* c~", &[], &[]),
        ("*a ~b~ c*", &[], &["~b~"]),
        ("*a =b* c= d*", &[], &[]),
        ("_a ~b~ c_", &[], &["~b~"]),
        ("+a ~b~ c+", &[], &["~b~"]),
        ("/a ~b~ c/", &[], &["~b~"]),
        ("~a *b* c~", &[], &["~a *b* c~"]),
        ("~*a*~", &[], &["~*a*~"]),
        ("*=a=*", &[], &["=a="]),
        ("=*a*=", &[], &["=*a*="]),
        ("[[a][=b=]]", &[], &["=b="]),
        ("[[a=b][c]]", &[], &[]),
        ("[[a][b =c]] d=", &[], &[]),
        ("=a [[b]] c=", &[], &["=a [[b]] c="]),
        ("=a [[b=]]", &[], &[]),
        ("[[a=]] =b=", &[], &["=b="]),
        ("https://x.org/a=b=c d", &[], &[]),
        ("x=https://x.org/?a=b=", &[], &[]),
        ("<https://x.org/a=b=c>", &[], &[]),
        ("<https://x.org/a\n=b=c> d", &[], &[]),
        ("http:ab=c=d e=", &[], &[]),
        ("src_sh{=x=}", &[], &["src_sh{=x=}"]),
        ("src_sh{a{b}c} =d=", &[], &["src_sh{a{b}c}", "=d="]),
        ("src_sh[:a b]{x}", &[], &["src_sh[:a b]{x}"]),
        ("src_sh[:a]x{}", &[], &[]),
        ("src_sh{x", &[], &[]),
        ("src_sh{x\ny} =z=", &[], &["src_sh{x\ny}", "=z="]),
        ("xsrc_sh{x}", &[], &[]),
        ("$src_sh{x}", &[], &[]),
        (".src_sh{x}", &[], &["src_sh{x}"]),
        ("a_src_sh{x}", &[], &[]),
        ("src_{x}", &[], &[]),
        ("src_sh {x}", &[], &[]),
        ("SRC_sh{x}", &[], &[]),
        ("src_sh[a{b}]{x}", &[], &["src_sh[a{b}]{x}"]),
        ("@@html:<b>@@", &[], &["@@html:<b>@@"]),
        ("@@html:x", &[], &[]),
        ("@@:x@@", &[], &[]),
        ("@@a-b:c d@@ =v=", &[], &["@@a-b:c d@@", "=v="]),
        ("@@a b:c@@", &[], &[]),
        ("@@html:a\nb@@ =v=", &[], &["@@html:a\nb@@", "=v="]),
        ("x@@html:=a=@@", &[], &["@@html:=a=@@"]),
        ("=a @@html:b= c@@", &[], &["=a @@html:b="]),
        ("{{{m(=a=)}}} =b=", &[], &["=b="]),
        ("{{{m}}} =b=", &[], &["=b="]),
        ("{{{m(a}}} =b= c)}}}", &[], &[]),
        ("{{{1m}}} =b=", &[], &["=b="]),
        ("{{{m-a_b(x)}}}", &[], &[]),
        ("$a=b$ =c=", &[], &["=c="]),
        ("$a =b$ c=", &[], &[]),
        ("$$ =a= $$ =b=", &[], &["=b="]),
        ("$ a=b$ =c=", &[], &["=c="]),
        ("$a=b $ =c=", &[], &["=c="]),
        ("\\(=a=\\) =b=", &[], &["=b="]),
        ("\\[=a=\\] =b=", &[], &["=b="]),
        ("\\alpha =a=", &[], &["=a="]),
        ("\\alpha{}=a=", &[], &[]),
        ("\\foo{=a=} =b=", &[], &["=b="]),
        ("\\foo[=a=]", &[], &[]),
        ("a_{b =c=} =d=", &[], &["=c=", "=d="]),
        ("a_{b c =d} e=", &[], &[]),
        ("a^{=b=}", &[], &["=b="]),
        ("a_b =c=", &[], &["=c="]),
        ("a_(=b=) =c=", &[], &["=b=", "=c="]),
        ("a_{b{c{d{e}}}} =f=", &[], &["=f="]),
        ("a_*b* =c=", &[], &["=c="]),
        ("a_*=b=", &[], &[]),
        ("[fn:1:=a=] =b=", &[], &["=a=", "=b="]),
        ("[fn::a =b] c=", &[], &[]),
        ("[fn:1] =b=", &[], &["=b="]),
        ("[fn:a$b:=c=]", &[], &["=c="]),
        ("[fn:x:[y =z]] w=", &[], &[]),
        ("<<a =b=>> =c=", &[], &["=c="]),
        ("<<<a =b=>>> =c=", &[], &["=b=", "=c="]),
        ("<<a>> =b=", &[], &["=b="]),
        ("<<a =b>> c=", &[], &[]),
        ("| =a | b= |", &[], &[]),
        ("| =a= | =b= |", &[], &["=a=", "=b="]),
        ("|---+---|\n| =a | b= |", &[], &[]),
        ("+---+\n| =a= |\n+---+", &[], &[]),
        ("- =a :: b=", &[], &[]),
        ("- a :: =b=", &[], &["=b="]),
        ("- =a=\n- =b=", &[], &["=a=", "=b="]),
        ("- =a\n  b=", &[], &["=a\n  b="]),
        ("1. =a :: b=", &[], &["=a :: b="]),
        ("*bold =a= text* =b=", &[], &["=a=", "=b="]),
        ("/i/ =a=", &[], &["=a="]),
        ("_u_ =a=", &[], &["=a="]),
        ("*a*=b=", &[], &[]),
        ("*a* =b=", &[], &["=b="]),
        ("(=a=)", &[], &["=a="]),
        ("\"=a=\"", &[], &["=a="]),
        ("'=a='", &[], &["=a="]),
        ("-=a=-", &[], &["=a="]),
        ("{=a=}", &[], &["=a="]),
        ("[=a=]", &[], &[]),
        ("=a=,", &[], &["=a="]),
        ("=a=[", &[], &["=a="]),
        ("日本語 =日本語= 日本語", &[], &["=日本語="]),
        ("😀=a=😀", &[], &[]),
        ("é=a=é", &[], &[]),
        (" \u{a0}=a=\u{a0} ", &[], &["=a="]),
        (
            "#+begin_verse\na =b\n\nc= d\n#+end_verse",
            &[],
            &["=b\n\nc="],
        ),
        ("#+begin_quote\na =b\n\nc= d\n#+end_quote", &[], &[]),
        ("#+begin_example\n=a=\n#+end_example", &[], &[]),
        ("a =b\r\nc= d", &[], &["=b\r\nc="]),
    ]);
}

#[test]
fn titles_show_their_visible_text() {
    let cases: &[(&str, &str)] = &[
        ("plain", "plain"),
        ("*bold* /it/ _u_ +s+ =v= ~c~", "bold it u +s+ v c"),
        ("*a /b/ c*", "a b c"),
        ("=*x*= *=y=*", "*x* y"),
        ("*bold =v <2024-04-01 Mon>=*", "bold v <2024-04-01 Mon>"),
        ("[1/2] [50%] [%] [/] x [1/] [/2]", "    x  "),
        ("a [[b]] c", "a b c"),
        ("[[a][b]] x", "b x"),
        ("[[id:x]]", "id:x"),
        ("*[[x][y]]* *[[x][*z*]]*", "y z"),
        ("[[a][b *c* d]]", "b c d"),
        ("[[file:a.org::*h][=q=]]", "q"),
        ("[[a b][c]]", "c"),
        ("[[a]] [[b][c]] [[d]]", "a c d"),
        (
            "see https://a.org/x?a=b~c~ d",
            "see https://a.org/x?a=b~c~ d",
        ),
        ("<https://a.org/x_y_> z", "<https://a.org/x_y_> z"),
        ("<file:a b.org> q", "<file:a b.org> q"),
        ("plain:x *b*", "plain:x b"),
        ("mailto:a@b.c *b*", "mailto:a@b.c b"),
        ("https://p.org/x[1/2]", "https://p.org/x"),
        ("https://p.org/x(a)[[b][c]]", "https://p.org/x(a)c"),
        ("(*b*) \"*c*\" -*d*- {*e*}", "(b) \"c\" -d- {e}"),
        (
            "*b*, *c*. *d*: *e*! *f*? *g*) *h*\\ *i*[ *j*;",
            "b, c. d: e! f? g) h\\ i[ j;",
        ),
        ("*b*' *c*", "b' c"),
        ("a*b*c", "a*b*c"),
        ("*  x*", "*  x*"),
        ("*x *", "*x *"),
        ("**", "**"),
        ("*****", "*"),
        ("_a_b_", "a_b"),
        ("_a b_ and _ c_", "a b and _ c_"),
        ("=a=b=", "a=b"),
        ("+a+ +b", "+a+ +b"),
        ("/x/ y/z/ /a/b", "x y/z/ /a/b"),
        ("~a~~b~", "a~~b"),
        ("a=b=c ~d", "a=b=c ~d"),
        ("a_{b} c^{d}", "a_{b} c^{d}"),
        ("a_{*b*} c^{/d/}", "a_{b} c^{d}"),
        ("a_b a_{b ~c~} a_(~d~)", "a_b a_{b c} a_(d)"),
        ("\\alpha *x* $a*b$ *c*", "\\alpha x $a*b$ c"),
        ("{{{m(*a*)}}} *z*", "{{{m(*a*)}}} z"),
        ("[fn:1] *x* [fn::*in* y]", "[fn:1] x [fn::in y]"),
        ("x <<t*a*>> <<<r>>> *z*", "x <<t*a*>> <<<r>>> z"),
        ("<<<a *b* c>>> *z*", "<<<a *b* c>>> z"),
        ("<2024-04-01 Mon> [#A] *x*", "<2024-04-01 Mon> [#A] x"),
        ("@@html:*x*@@ *y*", "@@html:*x*@@ y"),
        ("src_sh{*a*} *b*", "src_sh{*a*} b"),
        ("\u{a0}*x*\u{a0}", "\u{a0}x\u{a0}"),
        ("*x*\u{a0}y", "x\u{a0}y"),
        ("日本語 *日本語* 日本語", "日本語 日本語 日本語"),
        ("*😀* é*x*é", "😀 é*x*é"),
        ("*a* [[x][y]] *b* [[z]]", "a y b z"),
        ("*[[x][y]]*", "y"),
        ("[[x][*y*]]*z*", "y*z*"),
        ("x [[a][b]]*c*", "x b*c*"),
        ("*a*[[b]]", "ab"),
    ];
    let protocols = protocols();
    let mut failures = Vec::new();
    for (title, want) in cases {
        let got = normalize_title_text(title, &protocols);
        let want = want.trim_matches([' ', '\t']);
        if got != want {
            failures.push(format!("{title:?}: want {want:?}, got {got:?}"));
        }
    }
    assert!(failures.is_empty(), "{}", failures.join("\n"));
}

/// Emacs keeps the timestamp of a clock line in the `:value` of the clock element, which
/// `org-element-map` does not list. orgfdb has always stored it as a body timestamp.
#[test]
fn clock_lines_keep_their_timestamps_and_nothing_else() {
    let cases: &[(&str, &[&str])] = &[
        (
            "CLOCK: [2024-04-01 Mon 10:00]--[2024-04-01 Mon 11:00] =>  1:00",
            &["[2024-04-01 Mon 10:00]--[2024-04-01 Mon 11:00]"],
        ),
        ("CLOCK: [2024-04-01 Mon 10:00]", &["[2024-04-01 Mon 10:00]"]),
        (
            "  CLOCK: [2024-04-01 Mon 10:00]  ",
            &["[2024-04-01 Mon 10:00]"],
        ),
        // Not a clock line: a paragraph with a timestamp and a code.
        ("CLOCK: =a [2024-04-01 Mon 10:00]=", &[]),
        (
            "CLOCK: [2024-04-01 Mon 10:00] x",
            &["[2024-04-01 Mon 10:00]"],
        ),
    ];
    for (text, want) in cases {
        assert_eq!(facts(text).0, *want, "{text:?}");
    }
}

fn is_boundary_range(text: &str, start: usize, end: usize) -> bool {
    start <= end && end <= text.len() && text.is_char_boundary(start) && text.is_char_boundary(end)
}

#[test]
fn adversarial_multibyte_text_never_panics_and_keeps_ranges_on_characters() {
    const PIECES: &[&str] = &[
        "é",
        "日本",
        "😀",
        "\u{a0}",
        "\u{2003}",
        "\u{200b}",
        "ß",
        "İ",
        "\u{301}",
        "*",
        "/",
        "_",
        "+",
        "=",
        "~",
        "[",
        "]",
        "[[",
        "]]",
        "<",
        ">",
        "<<",
        ">>",
        "@@",
        "{",
        "}",
        "{{{",
        "}}}",
        "(",
        ")",
        "$",
        "$$",
        "\\",
        "\\(",
        "\\)",
        "^",
        "_{",
        "^{",
        "src_",
        "src_é{",
        "@@é:",
        "<2024-04-01",
        "<2024-04-01 é>",
        "[2024-04-01 Mon 10:00-11:30]",
        "<%%(",
        ")>",
        "--",
        "[fn:",
        "[fn:é:",
        "]",
        ":",
        "::",
        "- ",
        "1. ",
        "| ",
        "\n",
        "\n\n",
        "\n  ",
        "\t",
        " ",
        "http:é",
        "https://é.org/x(é)",
        "\\alpha",
        "\\é",
        "[1/2]",
        "[é%]",
        "<<é>>",
        "<<<é>>>",
        "-----",
        "+---+",
        "#+",
        "#+begin_verse\n",
        "#+end_verse\n",
        ":é:",
        "CLOCK: ",
        "\\begin{é}",
        "\\end{é}",
    ];
    let protocols = protocols();
    let mut state = 0x9e37_79b9_7f4a_7c15_u64;
    let mut next = move |bound: usize| {
        state ^= state << 13;
        state ^= state >> 7;
        state ^= state << 17;
        (state % bound as u64) as usize
    };
    for round in 0..3000 {
        let mut text = String::new();
        for _ in 0..1 + next(40) {
            text.push_str(PIECES[next(PIECES.len())]);
        }
        let content = if round % 3 == 0 {
            format!("* {text}\n{text}\n")
        } else {
            format!("* H\n{text}\n** {text} :t:\n{text}")
        };
        {
            let structure = scan_structure(&content);
            let facts = scan_inline(
                &content,
                &structure,
                &LineIndex::new(&content),
                &protocols,
                &TodoKeywordConfig::default(),
            );
            for range in &facts.ignored_ranges {
                assert!(
                    range.start < range.end && is_boundary_range(&content, range.start, range.end),
                    "{content:?}: ignored {range:?}"
                );
            }
            for timestamp in &facts.timestamps {
                assert!(
                    is_boundary_range(&content, timestamp.byte_start, timestamp.byte_end)
                        && content[timestamp.byte_start..timestamp.byte_end]
                            == *timestamp.raw_value,
                    "{content:?}: timestamp {timestamp:?}"
                );
            }
        }
        let _ = normalize_title_text(&text, &protocols);
    }
}

#[test]
fn pathological_input_stays_fast_and_does_not_overflow_the_stack() {
    let protocols = protocols();
    let cases = [
        "[fn::".repeat(50_000),
        "*a /b _c +d ".repeat(20_000),
        "=a ".repeat(100_000),
        "a_{b ".repeat(50_000),
        "src_a{x ".repeat(50_000),
        "src_a[{x ".repeat(50_000),
        "<https://a ".repeat(50_000),
        "<2024-04-01 ".repeat(50_000),
        "@@a:".repeat(50_000),
        "{{{m(".repeat(50_000),
        "$a ".repeat(50_000),
        "https://a.org/x(((((".repeat(20_000),
        "*\n".repeat(50_000),
        "\\( ".repeat(50_000),
        "\\begin{x}\n".repeat(50_000),
        format!("{}+x\n", "+-+\n".repeat(50_000)),
        "<<a ".repeat(50_000),
        "src_".repeat(100_000),
        "a.".repeat(200_000),
        "<https://a\n x\n".repeat(30_000),
        "- a :: b :: ".repeat(30_000),
        "| =a ".repeat(50_000),
    ];
    for text in cases {
        let content = format!("* H\n{text}\n");
        let structure = scan_structure(&content);
        let facts = scan_inline(
            &content,
            &structure,
            &LineIndex::new(&content),
            &protocols,
            &TodoKeywordConfig::default(),
        );
        let _ = facts.ignored_ranges.len();
        let _ = normalize_title_text(&text, &protocols);
    }
}

/// Org parses the objects of a title as a text of its own: it starts behind the TODO keyword,
/// the priority cookie and the word `COMMENT`, and ends before the tags.
#[test]
fn headline_objects_start_after_keyword_priority_and_comment() {
    let cases: &[(&str, &[&str], &[&str])] = &[
        (
            "* TODO [#A]=a <2024-04-01 Mon>= x <2024-05-05 Mon>\n",
            &["<2024-05-05 Mon>"],
            &["=a <2024-04-01 Mon>="],
        ),
        (
            "* [#A]*b <2024-04-01 Mon>* <2024-05-05 Mon>\n",
            &["<2024-04-01 Mon>", "<2024-05-05 Mon>"],
            &[],
        ),
        (
            "* TODO=a <2024-04-01 Mon>= x <2024-05-05 Mon>\n",
            &["<2024-04-01 Mon>", "<2024-05-05 Mon>"],
            &[],
        ),
        (
            "* COMMENT=a <2024-04-01 Mon>= x\n",
            &[],
            &["=a <2024-04-01 Mon>="],
        ),
        (
            "* COMMENT =a <2024-04-01 Mon>= x <2024-05-05 Mon> :t:\n",
            &["<2024-05-05 Mon>"],
            &["=a <2024-04-01 Mon>="],
        ),
        (
            "* [#A] =a <2024-04-01 Mon>=:t:\n",
            &[],
            &["=a <2024-04-01 Mon>="],
        ),
    ];
    for (content, timestamps, ignored) in cases {
        let (got_timestamps, got_ignored) = facts_of_content(content);
        assert_eq!(got_timestamps, *timestamps, "{content:?}");
        assert_eq!(got_ignored, *ignored, "{content:?}");
    }
}
