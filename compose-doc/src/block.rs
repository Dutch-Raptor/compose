use std::collections::VecDeque;

pub(crate) struct BlockHeader {
    pub lang: String,
    pub expected_errors: Vec<String>,
    pub expected_warnings: Vec<String>,
    pub stdout: bool,
}

pub(crate) fn parse_block_header(header: &str) -> Result<BlockHeader, String> {
    let mut tokens = tokenize(header);
    let mut lang = None;
    let mut expected_errors = Vec::new();
    let mut expected_warnings = Vec::new();
    let mut stdout = false;

    while let Some(token) = tokens.pop_front() {
        match token.as_str() {
            "error" => {
                let codes = parse_code_list(&mut tokens).ok_or("expected (...) after `error`")?;
                expected_errors.extend(codes);
            }
            "warn" => {
                let codes = parse_code_list(&mut tokens).ok_or("expected (...) after `warn`")?;
                expected_warnings.extend(codes);
            }
            "stdout" => {
                if stdout {
                    return Err("`stdout` specified twice".into());
                }
                stdout = true;
            }
            other => {
                if lang.is_none() {
                    lang = Some(other.to_string());
                } else {
                    return Err(format!("unexpected token `{other}`"));
                }
            }
        }
    }

    Ok(BlockHeader {
        lang: lang.unwrap_or_else(|| "text".into()),
        expected_errors,
        expected_warnings,
        stdout,
    })
}

fn tokenize(header: &str) -> VecDeque<String> {
    header
        .split_whitespace()
        .flat_map(|chunk| {
            // tokenize parentheses and commas too
            let mut tokens = Vec::new();
            let mut buf = String::new();
            for ch in chunk.chars() {
                match ch {
                    '(' | ')' | ',' => {
                        if !buf.is_empty() {
                            tokens.push(buf.clone());
                            buf.clear();
                        }
                        tokens.push(ch.to_string());
                    }
                    _ => buf.push(ch),
                }
            }
            if !buf.is_empty() {
                tokens.push(buf);
            }
            tokens
        })
        .collect()
}

fn parse_code_list(tokens: &mut VecDeque<String>) -> Option<Vec<String>> {
    if tokens.pop_front()?.as_str() != "(" {
        return None;
    }

    let mut codes = Vec::new();
    while let Some(tok) = tokens.pop_front() {
        match tok.as_str() {
            ")" => return Some(codes),
            "," => continue,
            code => codes.push(code.to_string()),
        }
    }
    None // missing closing paren
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_header() {
        let header =
            parse_block_header("compose error(E0001, E0002)").expect("failed to parse header");
        assert_eq!(header.expected_errors, vec!["E0001", "E0002"]);
        assert!(header.expected_warnings.is_empty());
        assert!(!header.stdout);
        assert_eq!(header.lang, "compose");
    }

    #[test]
    fn test_warn_stdout_header() {
        let header =
            parse_block_header("output warn(E0001, E0002) stdout").expect("failed to parse header");
        assert_eq!(header.expected_warnings, vec!["E0001", "E0002"]);
        assert!(header.stdout);
        assert!(header.expected_errors.is_empty());
    }
}