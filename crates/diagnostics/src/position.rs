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

/// Byte offset for a 0-based `(line, character)` with UTF-16 columns (LSP `Position`).
///
/// Returns [`None`] if `line` is past the last line. If `character` is past the line's
/// UTF-16 length, clamps to the end of that line (before `\n` or EOF).
pub fn line_character_utf16_to_byte_offset(
  source: &str,
  line: u32,
  character: u32,
) -> Option<usize> {
  let mut line_start: usize = 0;
  let mut current_line: u32 = 0;

  for (idx, c) in source.char_indices() {
    if current_line == line {
      break;
    }
    if c == '\n' {
      current_line += 1;
      if current_line == line {
        line_start = idx + c.len_utf8();
        break;
      }
    }
  }

  if current_line != line {
    return None;
  }

  let tail = &source[line_start..];
  let mut col_utf16: u32 = 0;
  for (rel, c) in tail.char_indices() {
    if c == '\n' {
      break;
    }
    let w = if (c as u32) <= 0xFFFF { 1 } else { 2 };
    if character < col_utf16 + w {
      return Some(line_start + rel);
    }
    col_utf16 += w;
  }

  // End of line (or empty line): clamp `character` past content to end-of-line byte offset.
  let line_end = line_start
    + tail
      .chars()
      .take_while(|&c| c != '\n')
      .map(|c| c.len_utf8())
      .sum::<usize>();
  Some(line_end.min(source.len()))
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

  #[test]
  fn roundtrip_ascii() {
    let s = "abc\ndef\n";
    for off in 0..=s.len() {
      let (l, c) = byte_offset_to_line_character_utf16(s, off);
      let back = line_character_utf16_to_byte_offset(s, l, c).unwrap();
      assert_eq!(back, off, "offset {off}");
    }
  }

  #[test]
  fn roundtrip_bmp_unicode() {
    let s = "é\n"; // line 0: one char, 1 UTF-16 unit, 2 UTF-8 bytes
    assert_eq!(line_character_utf16_to_byte_offset(s, 0, 0), Some(0));
    assert_eq!(line_character_utf16_to_byte_offset(s, 0, 1), Some(2));
    assert_eq!(line_character_utf16_to_byte_offset(s, 1, 0), Some(3));
    let (l, c) = byte_offset_to_line_character_utf16(s, 0);
    assert_eq!(line_character_utf16_to_byte_offset(s, l, c), Some(0));
    let (l, c) = byte_offset_to_line_character_utf16(s, 2);
    assert_eq!(line_character_utf16_to_byte_offset(s, l, c), Some(2));
  }

  #[test]
  fn roundtrip_astral() {
    let s = "a𝄞b";
    for off in [0usize, 1, 5, 6] {
      let (l, c) = byte_offset_to_line_character_utf16(s, off);
      let back = line_character_utf16_to_byte_offset(s, l, c).unwrap();
      assert_eq!(back, off, "offset {off} -> ({l},{c})");
    }
  }

  #[test]
  fn line_past_eof_is_none() {
    assert_eq!(line_character_utf16_to_byte_offset("a", 1, 0), None);
  }
}
