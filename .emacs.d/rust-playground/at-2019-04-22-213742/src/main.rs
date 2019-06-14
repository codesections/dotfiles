// snippet of code @ 2019-04-22 21:37:45

// === Rust Playground ===
// This snippet is in: ~/.emacs.d/rust-playground/at-2019-04-22-213742/

// Execute the snippet: C-c C-c
// Delete the snippet completely: C-c k
// Toggle between main.rs and Cargo.toml: C-c b

fn fib(n: u32) -> u32 {
    match n {
        0 => 0,
        1 => 1,
        2 => 2,
        n if n > 2 => fib(n - 1) + fib(n - 2),
    }
}
fn main() {
    let z = 5;
    dbg!(fib(z));
}
