use std::{
    fs,
    path::{Path, PathBuf},
};

use org_files_db::{
    parser::{OrgParser, OrgizeAdapter, ParseOptions},
    PARSER_INDEXER_CONTRACT_VERSION,
};
use sha2::{Digest, Sha256};

const MESSAGE: &str = "parser output changed: bump PARSER_INDEXER_CONTRACT_VERSION in src/indexing_context.rs and update tests/data/parser/CONTRACT";

fn fixture_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/data/parser")
}

fn collect(dir: &Path, out: &mut Vec<PathBuf>) {
    for entry in fs::read_dir(dir).expect("fixture dir should be readable") {
        let path = entry.expect("entry should load").path();
        if path.is_dir() {
            collect(&path, out);
        } else if path.file_name().is_some_and(|n| n == "fixture.org") {
            out.push(path);
        }
    }
}

fn output_hash() -> String {
    let root = fixture_root();
    let mut files = Vec::new();
    collect(&root, &mut files);
    files.sort();
    assert!(!files.is_empty(), "no parser fixtures found");
    let parser = OrgizeAdapter::new();
    let mut hasher = Sha256::new();
    for file in files {
        let content = fs::read_to_string(&file).expect("fixture should be readable");
        let relative = file.strip_prefix(&root).expect("fixture under root");
        let document = parser
            .parse_document(relative, &content, &ParseOptions::default())
            .unwrap_or_else(|err| panic!("fixture {} should parse: {err:?}", file.display()));
        let json = serde_json::to_string(&document).expect("document should serialize");
        hasher.update(relative.to_string_lossy().as_bytes());
        hasher.update(b"\n");
        hasher.update(json.as_bytes());
        hasher.update(b"\n");
    }
    hasher
        .finalize()
        .iter()
        .map(|b| format!("{b:02x}"))
        .collect()
}

fn contract_value(content: &str, key: &str) -> String {
    content
        .lines()
        .filter_map(|line| line.split_once('='))
        .find(|(k, _)| k.trim() == key)
        .unwrap_or_else(|| panic!("CONTRACT is missing {key}"))
        .1
        .trim()
        .to_string()
}

#[test]
fn parser_output_matches_contract_version() {
    let content =
        fs::read_to_string(fixture_root().join("CONTRACT")).expect("CONTRACT should exist");
    assert_eq!(
        contract_value(&content, "version"),
        PARSER_INDEXER_CONTRACT_VERSION,
        "{MESSAGE}"
    );
    assert_eq!(
        contract_value(&content, "output_sha256"),
        output_hash(),
        "{MESSAGE}"
    );
}
