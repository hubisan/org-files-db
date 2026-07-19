use super::QueryValue;

pub(crate) fn normalize_priority(value: &str) -> Result<i64, String> {
    if value.len() == 1 && value.as_bytes()[0].is_ascii_uppercase() {
        Ok(i64::from(value.as_bytes()[0] - b'A' + 1))
    } else {
        value
            .parse::<i64>()
            .ok()
            .filter(|rank| (0..=64).contains(rank))
            .ok_or_else(|| {
                format!(
                    "priority must be one uppercase letter from A through Z or a number from 0 through 64, got {value:?}"
                )
            })
    }
}

pub(crate) fn normalize_priority_value(value: &QueryValue) -> Result<i64, String> {
    match value {
        QueryValue::String(value) => normalize_priority(value),
        _ => Err("priority values must be strings".to_string()),
    }
}

#[cfg(test)]
mod tests {
    use super::normalize_priority;

    #[test]
    fn normalizes_alphabetic_and_numeric_priorities() {
        assert_eq!(normalize_priority("A"), Ok(1));
        assert_eq!(normalize_priority("Z"), Ok(26));
        assert_eq!(normalize_priority("0"), Ok(0));
        assert_eq!(normalize_priority("10"), Ok(10));
        assert_eq!(normalize_priority("64"), Ok(64));
    }

    #[test]
    fn rejects_invalid_priority_values() {
        assert!(normalize_priority("a").is_err());
        assert!(normalize_priority("65").is_err());
        assert!(normalize_priority("A1").is_err());
    }
}
