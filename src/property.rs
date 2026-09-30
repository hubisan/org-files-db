use std::collections::{BTreeMap, HashMap};

/// Normalize an Org property key and report whether the trailing append marker was present.
pub(crate) fn normalize_property_key(raw_key: &str) -> (String, bool) {
    let (key, append) = if let Some(key) = raw_key.strip_suffix('+') {
        (key, true)
    } else {
        (raw_key, false)
    };
    (key.to_uppercase(), append)
}

const DRAWER_SOURCE: &str = "property_drawer";

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PropertyRow {
    pub id: i64,
    pub heading_id: i64,
    pub key: String,
    pub value: Option<String>,
    pub append: bool,
    pub line_number: Option<i64>,
    /// Storage source (`properties.source`): `property_drawer`, `property_keyword`, ...
    pub source: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DerivedEffectiveProperty {
    pub heading_id: i64,
    pub key: String,
    pub local_value: Option<String>,
    pub effective_value: String,
}

/// Materialize the property view for one file.  The caller supplies the file's
/// complete heading tree and canonical property facts; no database identity is
/// used to determine values.
pub fn derive_effective_properties(
    parent_by_heading: &HashMap<i64, Option<i64>>,
    rows_by_heading: &HashMap<i64, Vec<PropertyRow>>,
) -> Vec<DerivedEffectiveProperty> {
    let mut children = HashMap::<Option<i64>, Vec<i64>>::new();
    for (&heading_id, &parent_id) in parent_by_heading {
        children.entry(parent_id).or_default().push(heading_id);
    }
    for ids in children.values_mut() {
        ids.sort_unstable();
    }

    fn visit(
        heading_id: i64,
        children: &HashMap<Option<i64>, Vec<i64>>,
        rows_by_heading: &HashMap<i64, Vec<PropertyRow>>,
        inherited: &BTreeMap<String, String>,
        output: &mut Vec<DerivedEffectiveProperty>,
    ) {
        let local = rows_by_heading
            .get(&heading_id)
            .map(|rows| resolve_local_properties_with_flags(rows))
            .unwrap_or_default();
        let mut effective = inherited.clone();
        for (key, value) in &local {
            apply_local(&mut effective, key, value);
        }
        for (key, effective_value) in &effective {
            output.push(DerivedEffectiveProperty {
                heading_id,
                key: key.clone(),
                local_value: local.get(key).map(|value| value.value.clone()),
                effective_value: effective_value.clone(),
            });
        }
        for child_id in children.get(&Some(heading_id)).into_iter().flatten() {
            visit(*child_id, children, rows_by_heading, &effective, output);
        }
    }

    let mut output = Vec::new();
    for root_id in children.get(&None).into_iter().flatten() {
        visit(
            *root_id,
            &children,
            rows_by_heading,
            &BTreeMap::new(),
            &mut output,
        );
    }
    output
}

fn apply_local(target: &mut BTreeMap<String, String>, key: &str, local: &LocalResolvedProperty) {
    if local.has_non_append {
        target.insert(key.to_string(), local.value.clone());
    } else {
        let inherited = target.remove(key);
        target.insert(
            key.to_string(),
            combine_property_values(inherited.as_deref(), std::slice::from_ref(&local.value)),
        );
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct LocalResolvedProperty {
    value: String,
    has_non_append: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct OrderedPropertyValues {
    base: Option<String>,
    appended: Vec<String>,
}

fn resolve_local_properties_with_flags(
    rows: &[PropertyRow],
) -> BTreeMap<String, LocalResolvedProperty> {
    let mut grouped = BTreeMap::<String, Vec<&PropertyRow>>::new();
    for row in rows {
        grouped
            .entry(normalize_property_key(&row.key).0)
            .or_default()
            .push(row);
    }

    grouped
        .into_iter()
        .map(|(key, mut property_rows)| {
            property_rows.sort_by(|left, right| {
                left.line_number
                    .cmp(&right.line_number)
                    .then_with(|| left.id.cmp(&right.id))
            });

            let ordered = ordered_property_values(&property_rows);
            let current = combine_property_values(ordered.base.as_deref(), &ordered.appended);

            (
                key,
                LocalResolvedProperty {
                    value: current,
                    has_non_append: ordered.base.is_some(),
                },
            )
        })
        .collect()
}

fn ordered_property_values(rows: &[&PropertyRow]) -> OrderedPropertyValues {
    let mut base = None;
    let mut appended: Vec<(bool, String)> = Vec::new();

    for row in rows {
        let value = row.value.clone().unwrap_or_default();
        let keyword = row.source != DRAWER_SOURCE;
        if row.append {
            appended.push((keyword, value));
        } else if base.is_none() || keyword {
            // Org (`org-entry-properties`): in a property drawer the first plain
            // definition wins and appends may precede it. `#+PROPERTY` keywords are
            // processed in order, so a later plain definition replaces the earlier
            // keyword definition and the keyword appends collected before it.
            if keyword {
                appended.retain(|(from_keyword, _)| !from_keyword);
            }
            base = Some(value);
        }
    }

    OrderedPropertyValues {
        base,
        appended: appended.into_iter().map(|(_, value)| value).collect(),
    }
}

/// Join components with a single space like Emacs, keeping empty components (an empty
/// base or append still contributes its separator: `"" + "x"` is `" x"`).
fn combine_property_values(base: Option<&str>, appended: &[String]) -> String {
    base.into_iter()
        .chain(appended.iter().map(String::as_str))
        .collect::<Vec<_>>()
        .join(" ")
}

#[cfg(test)]
mod tests {
    use super::{derive_effective_properties, PropertyRow, DRAWER_SOURCE};
    use std::collections::HashMap;

    fn row(
        id: i64,
        heading_id: i64,
        key: &str,
        value: &str,
        append: bool,
        line_number: i64,
    ) -> PropertyRow {
        PropertyRow {
            id,
            heading_id,
            key: key.to_string(),
            value: Some(value.to_string()),
            append,
            line_number: Some(line_number),
            source: DRAWER_SOURCE.to_string(),
        }
    }

    fn resolve_local_properties(
        rows: &[PropertyRow],
    ) -> std::collections::BTreeMap<String, String> {
        let parents = HashMap::from([(10, None)]);
        let rows_by_heading = HashMap::from([(10, rows.to_vec())]);
        derive_effective_properties(&parents, &rows_by_heading)
            .into_iter()
            .filter(|row| row.heading_id == 10)
            .map(|row| (row.key, row.effective_value))
            .collect()
    }

    #[test]
    fn resolves_local_append_sequences() {
        let resolved = resolve_local_properties(&[
            row(1, 10, "VALUE", "base", false, 1),
            row(2, 10, "VALUE", "one", true, 2),
            row(3, 10, "VALUE", "two", true, 3),
        ]);
        assert_eq!(
            resolved.get("VALUE").map(String::as_str),
            Some("base one two")
        );
    }

    #[test]
    fn first_duplicate_base_wins_and_appends_stay() {
        let resolved = resolve_local_properties(&[
            row(1, 10, "VALUE", "old", false, 1),
            row(2, 10, "VALUE", "new", false, 2),
            row(3, 10, "VALUE", "extra", true, 3),
        ]);
        // Emacs `org-entry-properties`: the first base definition wins.
        assert_eq!(resolved.get("VALUE").map(String::as_str), Some("old extra"));
    }

    #[test]
    fn last_property_keyword_base_wins() {
        let mut rows = [
            row(1, 10, "VALUE", "one", false, 1),
            row(2, 10, "VALUE", "two", false, 2),
            row(3, 10, "VALUE", "three", true, 3),
        ];
        for row in &mut rows {
            row.source = "property_keyword".to_string();
        }
        // Emacs `org-keyword-properties` for `#+PROPERTY:` lines.
        assert_eq!(
            resolve_local_properties(&rows)
                .get("VALUE")
                .map(String::as_str),
            Some("two three")
        );
    }

    #[test]
    fn resolves_append_only_local_values() {
        let resolved = resolve_local_properties(&[row(1, 10, "VALUE", "only", true, 1)]);
        assert_eq!(resolved.get("VALUE").map(String::as_str), Some("only"));
    }

    #[test]
    fn resolves_append_before_base_definition() {
        let resolved = resolve_local_properties(&[
            row(1, 10, "VALUE", "before", true, 1),
            row(2, 10, "VALUE", "definition", false, 2),
        ]);
        assert_eq!(
            resolved.get("VALUE").map(String::as_str),
            Some("definition before")
        );
    }

    #[test]
    fn append_between_duplicate_base_definitions_keeps_first_base() {
        let resolved = resolve_local_properties(&[
            row(1, 10, "VALUE", "first", false, 1),
            row(2, 10, "VALUE", "appended", true, 2),
            row(3, 10, "VALUE", "second", false, 3),
        ]);
        assert_eq!(
            resolved.get("VALUE").map(String::as_str),
            Some("first appended")
        );
    }

    #[test]
    fn resolves_multiple_append_positions_around_winning_base() {
        let resolved = resolve_local_properties(&[
            row(1, 10, "VALUE", "before", true, 1),
            row(2, 10, "VALUE", "first", false, 2),
            row(3, 10, "VALUE", "middle", true, 3),
            row(4, 10, "VALUE", "second", false, 4),
            row(5, 10, "VALUE", "after", true, 5),
        ]);
        assert_eq!(
            resolved.get("VALUE").map(String::as_str),
            Some("first before middle after")
        );
    }

    #[test]
    fn matches_emacs_joining_of_empty_and_whitespace_components() {
        // (rows as (value, append), expected) verified with Emacs 29.3 / Org 9.6.15.
        let cases: &[(&[(&str, bool)], &str)] = &[
            (&[("", false), ("x", true)], " x"),
            (&[("v", false), ("", true)], "v "),
            (&[("", false), ("", true)], " "),
            (&[("a", true), ("b", true)], "a b"),
            (&[("pre", true), ("base", false)], "base pre"),
            (&[("", false)], ""),
            (&[("", true)], ""),
            (&[("a", false), ("", true)], "a "),
        ];
        for (rows, expected) in cases {
            let rows = rows
                .iter()
                .enumerate()
                .map(|(i, (value, append))| row(i as i64, 10, "VALUE", value, *append, i as i64))
                .collect::<Vec<_>>();
            assert_eq!(
                resolve_local_properties(&rows)
                    .get("VALUE")
                    .map(String::as_str),
                Some(*expected),
                "{rows:?}"
            );
        }
    }

    #[test]
    fn property_keywords_process_in_order_and_drawer_append_extends_inherited() {
        let kw = |id, key: &str, value: &str, append| PropertyRow {
            source: "property_keyword".to_string(),
            ..row(id, 10, key, value, append, id)
        };
        let rows_by_heading = HashMap::from([
            (
                10,
                vec![
                    kw(1, "P", "c", true),
                    kw(2, "P", "b", false),
                    kw(3, "Q", "a", false),
                    kw(4, "Q", "z", true),
                    kw(5, "S", "s1", true),
                    kw(6, "S", "s2", true),
                ],
            ),
            (11, vec![row(7, 11, "P", "d", true, 9)]),
        ]);
        let parents = HashMap::from([(10, None), (11, Some(10))]);
        let derived = derive_effective_properties(&parents, &rows_by_heading);
        let get = |heading: i64, key: &str| {
            derived
                .iter()
                .find(|row| row.heading_id == heading && row.key == key)
                .map(|row| row.effective_value.as_str())
        };
        assert_eq!(get(10, "P"), Some("b"));
        assert_eq!(get(10, "Q"), Some("a z"));
        assert_eq!(get(10, "S"), Some("s1 s2"));
        assert_eq!(get(11, "P"), Some("b d"));
    }

    #[test]
    fn inherited_append_only_child_extends_parent_when_inheritance_is_enabled() {
        let rows_by_heading = HashMap::from([
            (10, vec![row(1, 10, "VALUE", "root", false, 1)]),
            (11, vec![row(2, 11, "VALUE", "parent", false, 2)]),
            (12, vec![row(3, 12, "VALUE", "child", true, 3)]),
        ]);
        let parents = HashMap::from([(10, None), (11, Some(10)), (12, Some(11))]);

        let derived = derive_effective_properties(&parents, &rows_by_heading);
        assert_eq!(
            derived
                .iter()
                .find(|row| row.heading_id == 12)
                .map(|row| row.effective_value.as_str()),
            Some("parent child")
        );
        assert_eq!(
            derived
                .iter()
                .find(|row| row.heading_id == 12)
                .and_then(|row| row.local_value.as_deref()),
            Some("child")
        );
    }

    #[test]
    fn inherited_local_base_wins_while_preserving_all_local_appends() {
        let rows_by_heading = HashMap::from([
            (11, vec![row(1, 11, "VALUE", "parent", false, 1)]),
            (
                12,
                vec![
                    row(2, 12, "VALUE", "before", true, 2),
                    row(3, 12, "VALUE", "child", false, 3),
                    row(4, 12, "VALUE", "after", true, 4),
                ],
            ),
        ]);
        let parents = HashMap::from([(11, None), (12, Some(11))]);

        let derived = derive_effective_properties(&parents, &rows_by_heading);
        assert_eq!(
            derived
                .iter()
                .find(|row| row.heading_id == 12)
                .map(|row| row.effective_value.as_str()),
            Some("child before after")
        );
    }
}
