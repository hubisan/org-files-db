use rusqlite::{limits::Limit, Connection};

pub(crate) fn variable_number_limit(connection: &Connection) -> usize {
    usize::try_from(connection.limit(Limit::SQLITE_LIMIT_VARIABLE_NUMBER))
        .ok()
        .filter(|limit| *limit > 0)
        .unwrap_or(0)
}

pub(crate) fn id_chunk_capacity(connection: &Connection, fixed_parameters: usize) -> usize {
    let limit = variable_number_limit(connection);
    assert!(
        fixed_parameters < limit,
        "fixed SQL parameters must leave capacity for at least one dynamic parameter"
    );
    limit - fixed_parameters
}

#[cfg(test)]
mod tests {
    use super::{id_chunk_capacity, variable_number_limit};
    use rusqlite::{limits::Limit, Connection};

    #[test]
    fn id_chunk_capacity_uses_runtime_variable_limit() {
        let connection = Connection::open_in_memory().expect("in-memory database should open");
        let previous = connection.set_limit(Limit::SQLITE_LIMIT_VARIABLE_NUMBER, 7);

        assert_eq!(variable_number_limit(&connection), 7);
        assert_eq!(id_chunk_capacity(&connection, 0), 7);
        assert_eq!(id_chunk_capacity(&connection, 2), 5);

        connection.set_limit(Limit::SQLITE_LIMIT_VARIABLE_NUMBER, previous);
    }

    #[test]
    #[should_panic(expected = "fixed SQL parameters must leave capacity")]
    fn id_chunk_capacity_rejects_exhausted_runtime_limit() {
        let connection = Connection::open_in_memory().expect("in-memory database should open");
        connection.set_limit(Limit::SQLITE_LIMIT_VARIABLE_NUMBER, 2);

        let _ = id_chunk_capacity(&connection, 2);
    }
}
