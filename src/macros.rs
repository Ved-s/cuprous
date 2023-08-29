#[macro_export]
macro_rules! unwrap_option_or_continue {
    ($e:expr) => {
        match $e {
            Some(v) => v,
            None => continue
        }
    };
}

#[macro_export]
macro_rules! unwrap_option_or_break {
    ($e:expr) => {
        match $e {
            Some(v) => v,
            None => break
        }
    };
    ($e:expr, $suf:tt) => {
        match $e {
            Some(v) => v,
            None => break $suf
        }
    };
}

#[macro_export]
macro_rules! unwrap_option_or_return {
    ($e:expr) => {
        match $e {
            Some(v) => v,
            None => return
        }
    };

    ($e:expr, $ret:expr) => {
        match $e {
            Some(v) => v,
            None => return $ret
        }
    };
}