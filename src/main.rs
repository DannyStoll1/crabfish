#![allow(non_upper_case_globals)]
#![allow(dead_code)]
#![feature(adt_const_params)]
#![feature(sync_unsafe_cell)]

mod nnue;
mod syzygy;
mod memory;
mod bitboard;
mod types;
mod misc;
mod uci;

const fn main() {
    // println!("{}", engine_info());
    //
    // Bitboards::init();
    // Position::init();

    // UCIEngine uci(argc, argv);

    // Tune::init(uci.engine_options());
    //
    // uci.loop();
}
