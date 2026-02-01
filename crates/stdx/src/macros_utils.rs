#[macro_export]
macro_rules! count {
    ($first:tt $($rest:tt)*) => {
        1 + $crate::count!($($rest)*)
    };

    () => {
        0
    };
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_counts() {
        assert_eq!(count!(), 0);
        assert_eq!(count!(1), 1);
        assert_eq!(count!(1 2), 2);
        assert_eq!(count!(1 2 3), 3);
    }
}
