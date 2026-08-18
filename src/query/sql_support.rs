use rusqlite::{limits::Limit, Connection};

pub(crate) fn variable_number_limit(connection: &Connection) -> usize {
    connection
        .limit(Limit::SQLITE_LIMIT_VARIABLE_NUMBER)
        .ok()
        .and_then(|limit| usize::try_from(limit).ok())
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
        let previous = connection
            .set_limit(Limit::SQLITE_LIMIT_VARIABLE_NUMBER, 7)
            .expect("runtime variable limit should change");

        assert_eq!(variable_number_limit(&connection), 7);
        assert_eq!(id_chunk_capacity(&connection, 0), 7);
        assert_eq!(id_chunk_capacity(&connection, 2), 5);

        connection
            .set_limit(Limit::SQLITE_LIMIT_VARIABLE_NUMBER, previous)
            .expect("runtime variable limit should restore");
    }

    #[test]
    #[should_panic(expected = "fixed SQL parameters must leave capacity")]
    fn id_chunk_capacity_rejects_exhausted_runtime_limit() {
        let connection = Connection::open_in_memory().expect("in-memory database should open");
        connection
            .set_limit(Limit::SQLITE_LIMIT_VARIABLE_NUMBER, 2)
            .expect("runtime variable limit should change");

        let _ = id_chunk_capacity(&connection, 2);
    }
}
