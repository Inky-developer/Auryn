use std::fmt::{Display, Write};

pub fn fmt_items<
    I: IntoIterator<Item = T, IntoIter = E>,
    E: ExactSizeIterator<Item = T>,
    T: Display,
>(
    values: I,
    joiner: &str,
) -> String {
    let mut values = values.into_iter();
    match values.len() {
        0 => "nothing".to_string(),
        1 => format!("`{}`", values.next().unwrap()),
        len => {
            let mut result = String::new();

            for (index, option) in values.enumerate() {
                if index != 0 && index + 1 < len {
                    result.push_str(", ");
                } else if index + 1 == len {
                    write!(result, " {joiner} ").unwrap();
                }

                result.push('`');
                write!(result, "{option}").unwrap();
                result.push('`');
            }

            result
        }
    }
}

pub fn pluralize<'a>(count: usize, singular: &'a str, plural: &'a str) -> &'a str {
    if count == 1 { singular } else { plural }
}
