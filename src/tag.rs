use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct DerivedEffectiveTag {
    pub heading_id: i64,
    pub tag: String,
    pub position: i64,
}

/// Materialize the effective tags visible at every heading in one file.
///
/// Direct tags remain canonical elsewhere. This projection walks the stored
/// heading tree from the synthetic root to the leaves, keeps inherited tags in
/// their existing order, appends new local tags in source order, and removes
/// duplicates by first occurrence. Tag comparison remains case-sensitive.
pub(crate) fn derive_effective_tags(
    parent_by_heading: &HashMap<i64, Option<i64>>,
    direct_tags_by_heading: &HashMap<i64, Vec<String>>,
) -> Vec<DerivedEffectiveTag> {
    let mut children = HashMap::<Option<i64>, Vec<i64>>::new();
    for (&heading_id, &parent_id) in parent_by_heading {
        children.entry(parent_id).or_default().push(heading_id);
    }
    for child_ids in children.values_mut() {
        child_ids.sort_unstable();
    }

    fn visit(
        heading_id: i64,
        children: &HashMap<Option<i64>, Vec<i64>>,
        direct_tags_by_heading: &HashMap<i64, Vec<String>>,
        inherited: &[String],
        output: &mut Vec<DerivedEffectiveTag>,
    ) {
        let mut visible = inherited.to_vec();
        let mut seen = visible.iter().cloned().collect::<HashSet<_>>();
        if let Some(local_tags) = direct_tags_by_heading.get(&heading_id) {
            for tag in local_tags {
                if seen.insert(tag.clone()) {
                    visible.push(tag.clone());
                }
            }
        }

        output.extend(
            visible
                .iter()
                .enumerate()
                .map(|(position, tag)| DerivedEffectiveTag {
                    heading_id,
                    tag: tag.clone(),
                    position: i64::try_from(position)
                        .expect("effective tag position must fit into i64"),
                }),
        );

        for child_id in children.get(&Some(heading_id)).into_iter().flatten() {
            visit(
                *child_id,
                children,
                direct_tags_by_heading,
                &visible,
                output,
            );
        }
    }

    let mut output = Vec::new();
    for root_id in children.get(&None).into_iter().flatten() {
        visit(
            *root_id,
            &children,
            direct_tags_by_heading,
            &[],
            &mut output,
        );
    }
    output
}

#[cfg(test)]
mod tests {
    use super::derive_effective_tags;
    use std::collections::HashMap;

    fn tags_for(rows: &[super::DerivedEffectiveTag], heading_id: i64) -> Vec<String> {
        rows.iter()
            .filter(|row| row.heading_id == heading_id)
            .map(|row| row.tag.clone())
            .collect()
    }

    #[test]
    fn derives_root_parent_and_local_tags_in_first_occurrence_order() {
        let parents = HashMap::from([(10, None), (11, Some(10)), (12, Some(11))]);
        let direct = HashMap::from([
            (10, vec!["file".to_string(), "shared".to_string()]),
            (11, vec!["parent".to_string(), "shared".to_string()]),
            (12, vec!["local".to_string(), "file".to_string()]),
        ]);

        let derived = derive_effective_tags(&parents, &direct);

        assert_eq!(tags_for(&derived, 10), vec!["file", "shared"]);
        assert_eq!(tags_for(&derived, 11), vec!["file", "shared", "parent"]);
        assert_eq!(
            tags_for(&derived, 12),
            vec!["file", "shared", "parent", "local"]
        );
        assert_eq!(
            derived
                .iter()
                .filter(|row| row.heading_id == 12)
                .map(|row| row.position)
                .collect::<Vec<_>>(),
            vec![0, 1, 2, 3]
        );
    }

    #[test]
    fn tag_deduplication_remains_case_sensitive() {
        let parents = HashMap::from([(10, None), (11, Some(10))]);
        let direct = HashMap::from([
            (10, vec!["Work".to_string()]),
            (11, vec!["work".to_string(), "Work".to_string()]),
        ]);

        let derived = derive_effective_tags(&parents, &direct);

        assert_eq!(tags_for(&derived, 11), vec!["Work", "work"]);
    }
}
