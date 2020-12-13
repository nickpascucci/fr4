mod bdl;

fn main() {
    let mut ctxt = bdl::Context::new();
    ctxt.interpret();
}
