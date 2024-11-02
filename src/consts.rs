pub const SIZEOF_I_64: i32 = 8; // size of an integer in bytes

pub const BUFFER_NAME: &str = "bump_array";
pub const BUFFER_SIZE: i32 = 1 << 20;
/**
 * Function Labels for Internal Language Infra
 */

pub const ENTRYPOINT_LABEL: &str = "our_code_starts_here";
pub const ERROR_LABEL: &str = "__snek_error";
pub const MAIN_LABEL: &str = "__snek_main";

/**
 * Type Flags for SnekPrint
 */

pub const BOOL_TYPE_FLAG: i32 = 0;
pub const INT_TYPE_FLAG: i32 = 1;
