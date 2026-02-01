use std::{
    fmt::Display,
    io::{StdoutLock, Write},
};

use yansi::Paint;

struct TestFinishedLogger(StdoutLock<'static>);

impl Drop for TestFinishedLogger {
    fn drop(&mut self) {
        if std::thread::panicking() {
            let _ = writeln!(self.0, "{}", "FAILED".red());
        } else {
            let _ = writeln!(self.0, "{}", "ok".green());
        }
    }
}

#[must_use]
pub fn log_test(name: impl Display) -> impl Drop {
    let mut stdout = std::io::stdout().lock();
    let _ = write!(stdout, "test {name} ... ");
    TestFinishedLogger(stdout)
}
