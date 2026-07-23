use std::{
    ffi::OsStr,
    fmt,
    os::unix::ffi::OsStrExt,
    path::{Path, PathBuf},
};

/// Stable, version-tagged native-path identity for one canonical filesystem path.
///
/// The byte form is persisted, while `display_path` remains the human-facing
/// database and JSON representation. The encoding is intentionally tagged so a
/// future platform or encoding change cannot compare unrelated values.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct FileIdentity(Vec<u8>);

impl FileIdentity {
    const PREFIX: &[u8] = b"orgfdb-path-v1\0unix\0";

    pub(crate) fn from_canonical_path(path: &Path) -> Self {
        let mut bytes = Self::PREFIX.to_vec();
        bytes.extend_from_slice(path.as_os_str().as_bytes());

        Self(bytes)
    }

    pub(crate) fn as_bytes(&self) -> &[u8] {
        &self.0
    }

    pub(crate) fn from_stored_bytes(bytes: Vec<u8>) -> Option<Self> {
        Self::valid_payload(bytes.strip_prefix(Self::PREFIX)?).then_some(Self(bytes))
    }

    pub(crate) fn to_path(&self) -> Option<PathBuf> {
        let bytes = self.0.strip_prefix(Self::PREFIX)?;
        if !Self::valid_payload(bytes) {
            return None;
        }

        Some(PathBuf::from(OsStr::from_bytes(bytes)))
    }

    fn valid_payload(path_bytes: &[u8]) -> bool {
        !path_bytes.is_empty()
            && !path_bytes.contains(&0)
            && Path::new(OsStr::from_bytes(path_bytes)).is_absolute()
    }
}

impl fmt::Debug for FileIdentity {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter
            .debug_tuple("FileIdentity")
            .field(&self.0)
            .finish()
    }
}

pub(crate) fn display_path(path: &Path) -> String {
    match path.to_str() {
        Some(path) => path.to_string(),
        None => escaped_path_bytes(path),
    }
}

fn escaped_path_bytes(path: &Path) -> String {
    let mut output = String::from("path-bytes:");
    for byte in path.as_os_str().as_bytes() {
        output.push('%');
        output.push(hex_digit(byte >> 4));
        output.push(hex_digit(byte & 0x0f));
    }
    output
}

fn hex_digit(value: u8) -> char {
    match value {
        0..=9 => char::from(b'0' + value),
        _ => char::from(b'A' + (value - 10)),
    }
}

#[cfg(test)]
mod tests {
    use super::{display_path, FileIdentity};
    use std::path::Path;

    #[test]
    fn utf8_paths_keep_their_display_representation() {
        let path = Path::new("/tmp/notes.org");
        assert_eq!(display_path(path), "/tmp/notes.org");
        assert_ne!(
            FileIdentity::from_canonical_path(path).as_bytes(),
            b"/tmp/notes.org"
        );
    }

    #[test]
    fn non_utf8_paths_use_a_reversible_escaped_display_representation() {
        use std::{ffi::OsStr, os::unix::ffi::OsStrExt, path::PathBuf};

        let path = PathBuf::from(OsStr::from_bytes(b"/tmp/invalid-\xff.org"));
        assert_eq!(
            display_path(&path),
            "path-bytes:%2F%74%6D%70%2F%69%6E%76%61%6C%69%64%2D%FF%2E%6F%72%67"
        );
    }

    #[test]
    fn tagged_unix_identity_restores_the_original_unix_path_bytes() {
        use std::{ffi::OsStr, os::unix::ffi::OsStrExt, path::PathBuf};

        let path = PathBuf::from(OsStr::from_bytes(b"/tmp/identity-\xff.org"));
        let identity = FileIdentity::from_canonical_path(&path);
        assert_eq!(identity.to_path(), Some(path));
        assert!(FileIdentity::from_stored_bytes(b"unknown\0/path".to_vec()).is_none());
        assert!(
            FileIdentity::from_stored_bytes(b"orgfdb-path-v1\0unknown\0/path".to_vec()).is_none()
        );
        assert!(FileIdentity::from_stored_bytes(b"orgfdb-path-v1\0unix".to_vec()).is_none());
        assert!(FileIdentity::from_stored_bytes(b"orgfdb-path-v1\0unix\0".to_vec()).is_none());
        assert!(
            FileIdentity::from_stored_bytes(b"orgfdb-path-v1\0unix\0relative.org".to_vec())
                .is_none()
        );
        assert!(FileIdentity::from_stored_bytes(
            b"orgfdb-path-v1\0unix\0/tmp/embedded\0nul.org".to_vec()
        )
        .is_none());
        assert!(FileIdentity::from_stored_bytes(b"malformed".to_vec()).is_none());
    }
}
