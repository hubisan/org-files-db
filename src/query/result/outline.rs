use super::*;

pub(in crate::query::result) fn insert_heading_outline(
    file_node: &mut FileResultNode,
    heading_chain: &[i64],
    matched_heading_id: i64,
    context: &EnrichmentContext,
    includes: &[QueryInclude],
) -> Result<(), QueryShapeError> {
    let parent = ensure_heading_outline_path(file_node, heading_chain, context, includes)?;
    parent.matched = true;
    *parent = context.shape_heading_node(matched_heading_id, true, includes, true)?;
    Ok(())
}

pub(in crate::query::result) fn ensure_heading_outline_path<'a>(
    file_node: &'a mut FileResultNode,
    heading_chain: &[i64],
    context: &EnrichmentContext,
    _includes: &[QueryInclude],
) -> Result<&'a mut HeadingResultNode, QueryShapeError> {
    let file_node_id = file_node.id;
    let children = file_node.children.as_mut().ok_or_else(|| {
        QueryShapeError::missing(format!(
            "missing outline children for file node {file_node_id}"
        ))
    })?;
    ensure_heading_outline_children(children, heading_chain, context)
}

pub(in crate::query::result) fn ensure_heading_outline_children<'a>(
    children: &'a mut Vec<QueryResultNode>,
    heading_chain: &[i64],
    context: &EnrichmentContext,
) -> Result<&'a mut HeadingResultNode, QueryShapeError> {
    let (head, tail) = heading_chain
        .split_first()
        .ok_or_else(|| QueryShapeError::missing("missing outline heading chain"))?;
    let index = if let Some(index) = children
        .iter()
        .position(|node| matches!(node, QueryResultNode::Heading(heading) if heading.id == *head))
    {
        index
    } else {
        children.push(QueryResultNode::Heading(context.shape_heading_node(
            *head,
            false,
            &[],
            true,
        )?));
        children.len() - 1
    };
    let node = match &mut children[index] {
        QueryResultNode::Heading(node) => node,
        _ => {
            return Err(QueryShapeError::missing(format!(
                "missing expected outline heading node for stored heading {head}"
            )))
        }
    };
    if tail.is_empty() {
        Ok(node)
    } else {
        let node_id = node.id;
        let children = node.children.as_mut().ok_or_else(|| {
            QueryShapeError::missing(format!(
                "missing outline children for heading node {node_id}"
            ))
        })?;
        ensure_heading_outline_children(children, tail, context)
    }
}

pub(in crate::query::result) fn sort_outline_children(children: Option<&mut Vec<QueryResultNode>>) {
    if let Some(children) = children {
        for child in children.iter_mut() {
            match child {
                QueryResultNode::File(node) => sort_outline_children(node.children.as_mut()),
                QueryResultNode::Heading(node) => sort_outline_children(node.children.as_mut()),
                QueryResultNode::Link(_) => {}
            }
        }
        children.sort_by_key(node_sort_key);
    }
}

pub(in crate::query::result) fn node_sort_key(node: &QueryResultNode) -> (i64, i64, u8) {
    match node {
        QueryResultNode::File(node) => (node.location.byte_start.unwrap_or(i64::MIN), node.id, 0),
        QueryResultNode::Heading(node) => {
            (node.location.byte_start.unwrap_or(i64::MIN), node.id, 1)
        }
        QueryResultNode::Link(node) => (node.location.byte_start.unwrap_or(i64::MIN), node.id, 2),
    }
}
