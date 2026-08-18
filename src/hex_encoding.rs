pub(crate) fn encode_lower(bytes: impl AsRef<[u8]>) -> String {
    let bytes = bytes.as_ref();
    let mut output = String::with_capacity(bytes.len() * 2);
    for &byte in bytes {
        output.push(hex_digit(byte >> 4));
        output.push(hex_digit(byte & 0x0f));
    }
    output
}

fn hex_digit(value: u8) -> char {
    match value {
        0..=9 => char::from(b'0' + value),
        _ => char::from(b'a' + value - 10),
    }
}

#[cfg(test)]
mod tests {
    use super::encode_lower;

    #[test]
    fn encodes_lowercase_hex_with_two_digits_per_byte() {
        assert_eq!(encode_lower([0x00, 0x0f, 0x10, 0xab, 0xff]), "000f10abff");
    }
}
