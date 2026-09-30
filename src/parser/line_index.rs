/// Maps byte offsets to 1-based line numbers using a sorted list of newline
/// offsets, so each lookup is a binary search instead of a scan from byte 0.
#[derive(Debug, Clone)]
pub(crate) struct LineIndex {
    newline_offsets: Vec<usize>,
}

impl LineIndex {
    pub(crate) fn new(content: &str) -> Self {
        let newline_offsets = content
            .bytes()
            .enumerate()
            .filter(|(_, byte)| *byte == b'\n')
            .map(|(index, _)| index)
            .collect();
        Self { newline_offsets }
    }

    /// Returns the 1-based line containing `offset`: 1 plus the number of
    /// newlines strictly before `offset`.
    pub(crate) fn line_for(&self, offset: usize) -> u32 {
        self.newline_offsets
            .partition_point(|&newline| newline < offset) as u32
            + 1
    }
}

#[cfg(test)]
mod tests {
    use super::LineIndex;

    fn old_line_number_for_offset(content: &str, offset: usize) -> u32 {
        content[..offset]
            .bytes()
            .filter(|byte| *byte == b'\n')
            .count() as u32
            + 1
    }

    #[test]
    fn matches_scan_from_start_for_every_offset() {
        for content in ["", "abc", "a\nbc\n\ndef", "\n", "one\ntwo\n", "\n\nx"] {
            let index = LineIndex::new(content);
            for offset in 0..=content.len() {
                assert_eq!(
                    index.line_for(offset),
                    old_line_number_for_offset(content, offset),
                    "content {content:?} offset {offset}"
                );
            }
        }
    }

    #[test]
    fn handles_start_newline_after_last_newline_and_empty() {
        let content = "ab\ncd\n";
        let index = LineIndex::new(content);
        assert_eq!(index.line_for(0), 1);
        assert_eq!(index.line_for(2), 1); // on the newline itself
        assert_eq!(index.line_for(3), 2);
        assert_eq!(index.line_for(content.len()), 3); // after last newline
        assert_eq!(LineIndex::new("").line_for(0), 1);
    }
}
