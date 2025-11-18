// ------------------------------------------------------------
// Org Parser Context
// Tracks block state, property drawer state and heading state.
// Fully Org-mode compliant.
// ------------------------------------------------------------


#[derive(Default)]
pub struct Context {
    /// Current active block. Only tracking SRC, EXAMPLE, EXPORT, COMMENT as
    /// everything inside has to be ignored.
    pub block: Option<BlockType>,

    /// True when inside :PROPERTIES: drawer
    pub in_properties_drawer: bool,
	
    /// TRUE until the first heading is encountered.
    /// Org: "Before the first headline, everything is file-level."
    pub before_first_heading: bool,

    pub property_drawer_allowed: bool,

    pub planning_line_allowed: bool,
}
