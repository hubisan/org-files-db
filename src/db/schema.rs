#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SchemaDefinition {
    pub version: u32,
}

impl Default for SchemaDefinition {
    fn default() -> Self {
        Self { version: 1 }
    }
}
