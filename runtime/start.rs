use std::env;

#[link(name = "our_code")]
extern "C" {
    // The \x01 here is an undocumented feature of LLVM that ensures
    // it does not add an underscore in front of the name.
    // Courtesy of Max New (https://maxsnew.com/teaching/eecs-483-fa22/hw_adder_assignment.html)
    #[link_name = "\x01our_code_starts_here"]
    fn our_code_starts_here(input: u64) -> u64;
}

#[export_name = "\x01snek_error"]
pub extern "C" fn snek_error(errcode: i64) {
    // TODO: print error message according to writeup
    eprintln!("an error ocurred {errcode}");
    std::process::exit(1);
}

const is_bool: u64 = 0;
const is_int: u64 = 1;

#[export_name = "\x01snek_print"]
pub extern "C" fn snek_print(value: i64, type_flag: u64) {
    if type_flag == is_bool {
        if value == 0 {
            println!("false");
        } else {
            println!("true");
        }
    } else if type_flag == is_int {
        println!("{}", value);
    } else {
        println!("unknown flag");
    }


    // TODO: print the value according to the writeup
}

fn parse_input(input: &str) -> u64 {
    // TODO: Panic on invalid input??
   match input.parse::<u64>() {
        Err(error) => panic!("Invalid Input"),
        Ok(val) => val
   }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = if args.len() == 2 { &args[1] } else { "0" };
    let input = parse_input(&input);

    let i: u64 = unsafe { our_code_starts_here(input) };
}
