pub const SIZEOF_I_64: i32 = 8; // size of an integer in bytes

/**
 * Function Labels for Internal Language Infra
 */

pub const ENTRYPOINT_LABEL: &str = "our_code_starts_here";
pub const OVERFLOW_LABEL: &str = "overflow";
pub const MAIN_LABEL: &str = "__main";

/**
 * Type Flags for SnekPrint
 */

pub const BOOL_TYPE_FLAG: i32 = 0;
pub const INT_TYPE_FLAG: i32 = 1;
