/// Count UTF-16 code units in a UTF-8 slice (for LSP `Position.character`).
fn utf8_slice_utf16_len(s: &str) -> u32 {
  let mut n = 0u32;
  for c in s.chars() {
    n += if c <= '\u{FFFF}' { 1 } else { 2 };
  }
  n
}

/// 0-based line and UTF-16 column for a byte offset in `source` (LSP-compatible).
pub fn byte_offset_to_line_character_utf16(source: &str, byte_offset: usize) -> (u32, u32) {
  let byte_offset = byte_offset.min(source.len());
  let mut line: u32 = 0;
  let mut line_start_byte: usize = 0;

  for (idx, c) in source.char_indices() {
    if idx >= byte_offset {
      let col = utf8_slice_utf16_len(&source[line_start_byte..idx]);
      return (line, col);
    }
    if c == '\n' {
      line += 1;
      line_start_byte = idx + c.len_utf8();
    }
  }

  let col = utf8_slice_utf16_len(&source[line_start_byte..byte_offset]);
  (line, col)
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn ascii_first_line() {
    let s = "abc\ndef";
    assert_eq!(byte_offset_to_line_character_utf16(s, 0), (0, 0));
    assert_eq!(byte_offset_to_line_character_utf16(s, 1), (0, 1));
    assert_eq!(byte_offset_to_line_character_utf16(s, 3), (0, 3));
    assert_eq!(byte_offset_to_line_character_utf16(s, 4), (1, 0));
  }

  #[test]
  fn bmp_unicode_column_is_chars_not_bytes() {
    let s = "é"; // 2 UTF-8 bytes, 1 UTF-16 unit
    assert_eq!(byte_offset_to_line_character_utf16(s, 0), (0, 0));
    assert_eq!(byte_offset_to_line_character_utf16(s, 2), (0, 1));
  }

  #[test]
  fn astral_plane_counts_two_utf16_units() {
    let s = "a𝄞b"; // '𝄞' is U+1D11E => 2 UTF-16 code units
    assert_eq!(byte_offset_to_line_character_utf16(s, 0), (0, 0));
    assert_eq!(byte_offset_to_line_character_utf16(s, 1), (0, 1));
    let after_emoji = 1 + '𝄞'.len_utf8();
    assert_eq!(byte_offset_to_line_character_utf16(s, after_emoji), (0, 3));
  }
}
