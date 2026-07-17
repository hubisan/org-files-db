use std::collections::{BTreeMap, HashMap};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PropertyRow {
    pub id: i64,
    pub heading_id: i64,
    pub key: String,
    pub value: Option<String>,
    pub append: bool,
    pub line_number: Option<i64>,
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

pub fn resolve_local_properties(rows: &[PropertyRow]) -> BTreeMap<String, String> {
    resolve_local_properties_with_flags(rows)
        .into_iter()
        .map(|(key, property)| (key, property.value))
        .collect()
}

pub struct PropertyResolver<'a> {
    parent_by_heading: &'a HashMap<i64, Option<i64>>,
    rows_by_heading: &'a HashMap<i64, Vec<PropertyRow>>,
    inherited_cache: HashMap<i64, BTreeMap<String, String>>,
}

impl<'a> PropertyResolver<'a> {
    pub fn new(
        parent_by_heading: &'a HashMap<i64, Option<i64>>,
        rows_by_heading: &'a HashMap<i64, Vec<PropertyRow>>,
    ) -> Self {
        Self {
            parent_by_heading,
            rows_by_heading,
            inherited_cache: HashMap::new(),
        }
    }

    pub fn effective_properties(
        &mut self,
        heading_id: i64,
        inherit: bool,
    ) -> BTreeMap<String, String> {
        if !inherit {
            return self
                .rows_by_heading
                .get(&heading_id)
                .map(|rows| resolve_local_properties(rows))
                .unwrap_or_default();
        }

        if let Some(cached) = self.inherited_cache.get(&heading_id) {
            return cached.clone();
        }

        let mut resolved = self
            .parent_by_heading
            .get(&heading_id)
            .and_then(|parent_id| *parent_id)
            .map(|parent_id| self.effective_properties(parent_id, true))
            .unwrap_or_default();

        if let Some(rows) = self.rows_by_heading.get(&heading_id) {
            for (key, local) in resolve_local_properties_with_flags(rows) {
                if local.has_non_append {
                    resolved.insert(key, local.value);
                } else {
                    let inherited = resolved.remove(&key);
                    resolved.insert(
                        key,
                        combine_property_values(
                            inherited.as_deref(),
                            std::slice::from_ref(&local.value),
                        ),
                    );
                }
            }
        }

        self.inherited_cache.insert(heading_id, resolved.clone());
        resolved
    }
}

fn resolve_local_properties_with_flags(
    rows: &[PropertyRow],
) -> BTreeMap<String, LocalResolvedProperty> {
    let mut grouped = BTreeMap::<String, Vec<&PropertyRow>>::new();
    for row in rows {
        grouped.entry(row.key.clone()).or_default().push(row);
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
    let mut appended = Vec::new();

    for row in rows {
        let value = row.value.clone().unwrap_or_default();
        if row.append {
            appended.push(value);
        } else {
            base = Some(value);
        }
    }

    OrderedPropertyValues { base, appended }
}

fn combine_property_values(base: Option<&str>, appended: &[String]) -> String {
    let mut components = Vec::with_capacity(appended.len() + usize::from(base.is_some()));

    if let Some(base) = base {
        if !base.is_empty() {
            components.push(base.to_string());
        }
    }

    for value in appended {
        if !value.is_empty() {
            components.push(value.clone());
        }
    }

    components.join(" ")
}

#[cfg(test)]
mod tests {
    use super::{resolve_local_properties, PropertyResolver, PropertyRow};
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
        }
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
    fn resolves_local_replacement_then_append() {
        let resolved = resolve_local_properties(&[
            row(1, 10, "VALUE", "old", false, 1),
            row(2, 10, "VALUE", "new", false, 2),
            row(3, 10, "VALUE", "extra", true, 3),
        ]);
        assert_eq!(resolved.get("VALUE").map(String::as_str), Some("new extra"));
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
    fn resolves_append_between_duplicate_base_definitions() {
        let resolved = resolve_local_properties(&[
            row(1, 10, "VALUE", "first", false, 1),
            row(2, 10, "VALUE", "appended", true, 2),
            row(3, 10, "VALUE", "second", false, 3),
        ]);
        assert_eq!(
            resolved.get("VALUE").map(String::as_str),
            Some("second appended")
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
            Some("second before middle after")
        );
    }

    #[test]
    fn resolves_empty_components_without_artificial_spaces() {
        let resolved = resolve_local_properties(&[
            row(1, 10, "VALUE", "", false, 1),
            row(2, 10, "VALUE", "valid", true, 2),
        ]);
        assert_eq!(resolved.get("VALUE").map(String::as_str), Some("valid"));

        let resolved = resolve_local_properties(&[
            row(1, 10, "VALUE", "valid", false, 1),
            row(2, 10, "VALUE", "", true, 2),
        ]);
        assert_eq!(resolved.get("VALUE").map(String::as_str), Some("valid"));
    }

    #[test]
    fn inherited_append_only_child_extends_parent_when_inheritance_is_enabled() {
        let rows_by_heading = HashMap::from([
            (10, vec![row(1, 10, "VALUE", "root", false, 1)]),
            (11, vec![row(2, 11, "VALUE", "parent", false, 2)]),
            (12, vec![row(3, 12, "VALUE", "child", true, 3)]),
        ]);
        let parents = HashMap::from([(10, None), (11, Some(10)), (12, Some(11))]);

        let mut resolver = PropertyResolver::new(&parents, &rows_by_heading);
        assert_eq!(
            resolver
                .effective_properties(12, true)
                .get("VALUE")
                .map(String::as_str),
            Some("parent child")
        );
        assert_eq!(
            resolver
                .effective_properties(12, false)
                .get("VALUE")
                .map(String::as_str),
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

        let mut resolver = PropertyResolver::new(&parents, &rows_by_heading);
        assert_eq!(
            resolver
                .effective_properties(12, true)
                .get("VALUE")
                .map(String::as_str),
            Some("child before after")
        );
    }
}
