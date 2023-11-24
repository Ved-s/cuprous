use std::collections::HashSet;

#[derive(Debug)]
enum FormatParameter<'a> {
    String(&'a str),
    Increment { next: usize, seen: HashSet<usize> },
}

impl FormatParameter<'_> {
    fn collect_str(&mut self, str: &mut &str) -> bool {
        match self {
            FormatParameter::String(param) => match str.strip_prefix(*param) {
                Some(post) => {
                    *str = post;
                    true
                }
                None => false,
            },
            FormatParameter::Increment { next, seen } => {
                let digits = str
                    .char_indices()
                    .take_while(|(_, c)| c.is_ascii_digit())
                    .last()
                    .map(|(i, _)| i + 1)
                    .unwrap_or(0);
                let number = &str[..digits];

                let value = match number.parse() {
                    Ok(v) => v,
                    Err(_) => return false,
                };

                seen.insert(value);
                while seen.contains(next) {
                    *next += 1;
                }

                true
            }
        }
    }

    fn modify_value(&mut self, str: &mut String, pos: &mut usize) {
        match self {
            FormatParameter::String(param) => {
                if !str[*pos..].starts_with(*param) {
                    str.replace_range(*pos.., param);
                }
                *pos += param.len();
            }
            FormatParameter::Increment { next, seen } => {
                let digits = str[*pos..]
                    .char_indices()
                    .take_while(|(_, c)| c.is_ascii_digit())
                    .last()
                    .map(|(i, _)| i + 1)
                    .unwrap_or(0);
                let number = &str[..digits];

                let value = number.parse::<usize>().ok();

                if let Some(value) = value {
                    if !seen.contains(&value) {
                        *pos += digits;
                        seen.insert(value);
                        return;
                    } else {
                        *next += 1;
                        while seen.contains(next) {
                            *next += 1;
                        }

                        let next_str = format!("{next}");
                        str.replace_range(*pos..*pos + digits, &next_str);
                        *pos += next_str.len();
                    }
                } else {
                    let next_str = format!("{next}");
                    str.replace_range(*pos.., &next_str);
                    *pos += next_str.len();
                }

                *next += 1;
                while seen.contains(next) {
                    *next += 1;
                }
            }
        }
    }
}

fn parse_format_param<'a>(str: &mut &'a str) -> Option<FormatParameter<'a>> {
    if str.starts_with('i') {
        *str = &str[1..];
        Some(FormatParameter::Increment {
            next: 0,
            seen: Default::default(),
        })
    } else {
        None
    }
}

/// returns None if format string doesn't contain any format arguments
fn create_format_params(mut str: &str) -> Option<Vec<FormatParameter<'_>>> {
    let mut params = vec![];
    let mut pos = 0;
    while !str.is_empty() {
        let next_str = &str[pos..];
        match next_str.find('%') {
            Some(chpos) => {
                let mut next_str = &str[chpos + 1..];
                match parse_format_param(&mut next_str) {
                    Some(param) => {
                        pos = 0;
                        if chpos > 0 {
                            params.push(FormatParameter::String(&str[..chpos]));
                        }
                        str = next_str;
                        params.push(param);
                    }
                    None => pos = chpos + 1,
                }
            }
            None => {
                if params.is_empty() {
                    return None;
                }
                params.push(FormatParameter::String(str));
                break;
            }
        }
    }
    Some(params)
}

enum FormatterParams<'a> {
    Multiple(Vec<FormatParameter<'a>>),
    None(&'a str),
}

pub struct StringFormatterState<'a> {
    params: FormatterParams<'a>,
}

impl<'a> StringFormatterState<'a> {
    pub fn new(format: &'a str) -> Self {
        let params = match create_format_params(format) {
            Some(vec) => FormatterParams::Multiple(vec),
            None => FormatterParams::None(format),
        };
        Self { params }
    }

    pub fn add_evironment_string(&mut self, mut str: &str) {
        if let FormatterParams::Multiple(params) = &mut self.params {
            for param in params.iter_mut() {
                if !param.collect_str(&mut str) {
                    break;
                }
            }
        }
    }

    pub fn process_string(&mut self, string: &mut String) {
        match &mut self.params {
            FormatterParams::None(format) => {
                string.clear();
                string.push_str(format);
            }
            FormatterParams::Multiple(params) => {
                let mut pos = 0;
                for param in params.iter_mut() {
                    param.modify_value(string, &mut pos);
                }
                if pos < string.len() {
                    string.replace_range(pos.., "");
                }
            }
        }
    }

    pub fn has_formatting(&self) -> bool {
        matches!(&self.params, FormatterParams::Multiple(_))
    }
}