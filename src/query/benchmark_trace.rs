use std::{
    cell::RefCell,
    time::{Duration, Instant},
};

pub(crate) const QUERY_COMPILATION: &str = "query-compilation";
pub(crate) const MATCHED_SQL_EXECUTION: &str = "matched-sql-execution";
pub(crate) const ENRICHMENT_SQL_EXECUTION: &str = "enrichment-sql-execution";
pub(crate) const SQLITE_ROW_DECODING: &str = "sqlite-row-decoding";
pub(crate) const RUST_GROUPING: &str = "rust-grouping";
pub(crate) const RUST_LOCAL_SORTING: &str = "rust-local-sorting";
pub(crate) const OUTLINE_VALIDATION: &str = "outline-validation";
pub(crate) const HEADING_PATH_CONSTRUCTION: &str = "heading-path-construction";
pub(crate) const FINAL_RESULT_SHAPING: &str = "final-result-shaping";

#[derive(Debug, Clone)]
pub(crate) struct BenchmarkTraceRecord {
    pub(crate) phase: &'static str,
    pub(crate) operation: &'static str,
    pub(crate) duration_ns: u128,
    pub(crate) rows: usize,
    pub(crate) statement_count: usize,
    pub(crate) bound_parameters: usize,
}

#[derive(Debug, Default)]
struct BenchmarkTraceState {
    active: bool,
    records: Vec<BenchmarkTraceRecord>,
}

thread_local! {
    static TRACE_STATE: RefCell<BenchmarkTraceState> = RefCell::new(BenchmarkTraceState::default());
}

pub(crate) fn begin() {
    TRACE_STATE.with(|state| {
        let mut state = state.borrow_mut();
        state.active = true;
        state.records.clear();
    });
}

pub(crate) fn finish() -> Vec<BenchmarkTraceRecord> {
    TRACE_STATE.with(|state| {
        let mut state = state.borrow_mut();
        state.active = false;
        std::mem::take(&mut state.records)
    })
}

pub(crate) fn active() -> bool {
    TRACE_STATE.with(|state| state.borrow().active)
}

pub(crate) fn timed<T>(operation: impl FnOnce() -> T) -> (T, Duration) {
    if !active() {
        return (operation(), Duration::ZERO);
    }
    let started = Instant::now();
    let result = operation();
    (result, started.elapsed())
}

pub(crate) fn record(
    phase: &'static str,
    operation: &'static str,
    duration: Duration,
    rows: usize,
    statement_count: usize,
    bound_parameters: usize,
) {
    if !active() {
        return;
    }
    TRACE_STATE.with(|state| {
        state.borrow_mut().records.push(BenchmarkTraceRecord {
            phase,
            operation,
            duration_ns: duration.as_nanos(),
            rows,
            statement_count,
            bound_parameters,
        });
    });
}
