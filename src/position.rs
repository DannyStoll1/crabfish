/*
  Stockfish, a UCI chess playing engine derived from Glaurung 2.1
  Copyright (C) 2004-2024 The Stockfish developers (see AUTHORS file)

  Stockfish is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Stockfish is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

use std::{assert_matches::debug_assert_matches, collections::VecDeque};

use enum_map::EnumMap;

use crate::{
    bitboard::Bitboard,
    misc::Prng,
    types::{BoardMap, CastlingRights, Color, File, Key, Move, Piece, PieceType, Square, Value},
};

// StateInfo struct stores information needed to restore a Position object to
// its previous state when we retract a move. Whenever a move is made on the
// board (by calling Position::do_move), a StateInfo object must be passed.

struct StateInfo
{
    // Copied when making a move
    material_key: Key,
    pawn_key: Key,
    non_pawn_material: EnumMap<Color, Value>,
    castling_rights: CastlingRights,
    rule50: i16,
    plies_from_null: i16,
    ep_square: Square,

    // Not copied when making a move (will be recomputed anyhow)
    key: Key,
    checkers: Bitboard,
    previous: *const StateInfo,
    blockers_for_king: EnumMap<Color, Bitboard>,
    pinners: EnumMap<Color, Bitboard>,
    check_squares: EnumMap<PieceType, Bitboard>,
    captured_piece: Piece,
    repetition: i8,
    // Used by NNUE
    // accumulatorBig: Eval::NNUE::Accumulator<Eval::NNUE::TransformedFeatureDimensionsBig>,
    // accumulatorSmall: Eval::NNUE::Accumulator<Eval::NNUE::TransformedFeatureDimensionsSmall>,
    // dirtyPiece: DirtyPiece,
}

// A list to keep track of the position states along the setup moves (from the
// start position to the position just before the search starts). Needed by
// 'draw by repetition' detection. Use a VecDeque because pointers to
// elements are not invalidated upon list resizing.
type StateListPtr = Box<VecDeque<StateInfo>>;

struct CuckooTables
{
    cuckoo: [Key; 8192],
    cuckoo_move: [Move; 8192],
}
impl CuckooTables
{
    pub fn init(zobrist: &Zobrist) -> Self
    {
        // Prepare the cuckoo tables
        let mut cuckoo: [Key; 8192] = [Key(0); 8192];
        let mut cuckoo_move = [Move::none(); 8192];

        let mut count = 0;
        for pc in Piece::iter() {
            for s1 in Square::iter_board() {
                for s2 in s1.next()..=Square::H8 {
                    let pt = pc.piece_type();
                    if pt != PieceType::Pawn
                        && Bitboard::attacks_for_piece(pt, s1, Bitboard::EMPTY).contains_square(s2)
                    {
                        let mut mv = Move::standard(s1, s2);
                        let mut key = zobrist.psq[pc][s1] ^ zobrist.psq[pc][s2] ^ zobrist.side;
                        let mut i = key.hash_0();
                        loop {
                            std::mem::swap(&mut cuckoo[i as usize], &mut key);
                            std::mem::swap(&mut cuckoo_move[i as usize], &mut mv);
                            if mv == Move::none() {
                                // Arrived at empty slot?
                                break;
                            }
                            let u = key.hash_0();
                            if i == u {
                                i = key.hash_1();
                            } else {
                                i = u;
                            } // Push victim to alternative slot
                        }
                        count += 1;
                    }
                }
            }
        }
        debug_assert_eq!(count, 3668);
        Self {
            cuckoo,
            cuckoo_move,
        }
    }
}

struct Zobrist
{
    psq: EnumMap<Piece, BoardMap<Key>>,
    enpassant: EnumMap<File, Key>,
    castling: EnumMap<CastlingRights, Key>,
    side: Key,
    no_pawns: Key,
}
impl Zobrist
{
    fn init() -> Self
    {
        let mut rng = Prng::new(1070372);

        Self {
            psq: EnumMap::from_fn(|_| BoardMap::from_fn(|_| rng.rand())),
            enpassant: EnumMap::from_fn(|_| rng.rand()),
            castling: EnumMap::from_fn(|_| rng.rand()),
            side: rng.rand(),
            no_pawns: rng.rand(),
        }
    }
}

// Position class stores information regarding the board representation as
// pieces, side to move, hash keys, castling info, etc. Important methods are
// do_move() and undo_move(), used by the search to update node info when
// traversing the search tree.
// #[derive(Default)]
struct Position
{
    // Data members
    board: BoardMap<Piece>,
    by_type_bb: EnumMap<PieceType, Bitboard>,
    by_color_bb: EnumMap<Color, Bitboard>,
    piece_count: EnumMap<Piece, u8>,
    castling_rights_mask: BoardMap<u32>,
    castling_rook_square: EnumMap<CastlingRights, Square>,
    castling_path: EnumMap<CastlingRights, Bitboard>,
    st: *mut StateInfo,
    game_ply: u32,
    side_to_move: Color,
    chess960: bool,
}
impl Position
{
    //  Position()                           = default;
    //  Position(const Position&)            = delete;
    //  Position& operator=(const Position&) = delete;
    //
    //  // FEN string input/output
    //  Position&   set(const std::string& fenStr, bool isChess960, StateInfo* si);
    //  Position&   set(const std::string& code, c: Color, StateInfo* si);
    //  std::string fen() const;

    // Position representation
    #[inline] fn pieces_of_type(&self, pt: PieceType) -> Bitboard { self.by_type_bb[pt] }

    // template<typename... PieceTypes>
    // #[inline] pub fn pieces(&self, PieceType pt, PieceTypes... pts) -> Bitboard {
    //     return pieces(pt) | pieces(pts...);
    // }
    //
    // #[inline] pub fn pieces(&self, c: Color) -> Bitboard { return self.by_color_bb[c]; }
    //
    #[inline] pub fn pieces(&self, c: Color, pt: PieceType) -> Bitboard {
        return self.by_color_bb[c] & self.by_type_bb[pt];
    }
    // template<typename... PieceTypes>
    // #[inline] pub fn pieces(&self, c: Color, PieceTypes... pts) -> Bitboard {
    //     return pieces(c) & pieces(pts...);
    // }
    //  Piece    piece_on(s: Square) const;
    //  Square   ep_square() const;
    //  bool     empty(s: Square) const;
    //  template<PieceType Pt>
    //  int count(c: Color) const;
    //  template<PieceType Pt>
    //  int count() const;
    //  template<PieceType Pt>
    //  Square square(c: Color) const;
    //
    //  // Castling
    //  CastlingRights castling_rights(c: Color) const;
    //  bool           can_castle(cr: CastlingRights) const;
    //  bool           castling_impeded(cr: CastlingRights) const;
    //  Square         castling_rook_square(cr: CastlingRights) const;
    //
    //  // Checking
    //  Bitboard checkers() const;
    //  Bitboard blockers_for_king(c: Color) const;
    //  Bitboard check_squares(PieceType pt) const;
    //  Bitboard pinners(c: Color) const;
    //
    //  // Attacks to/from a given square
    //  Bitboard attackers_to(s: Square) const;
    //  Bitboard attackers_to(s: Square, Bitboard occupied) const;
    //  void     update_slider_blockers(c: Color) const;

    #[inline]
    pub fn attacks_by<const Pt: PieceType>(&self, c: Color) -> Bitboard {
        if matches!(Pt, PieceType::Pawn) {
            match c {
                Color::White => Bitboard::pawn_attacks::<{Color::White}>(self.pieces(Color::White, PieceType::Pawn)),
                Color::Black => Bitboard::pawn_attacks::<{Color::Black}>(self.pieces(Color::Black, PieceType::Pawn)),
            }
        }
        else
        {
            let mut threats   = Bitboard::EMPTY;
            let mut attackers = self.pieces(c, Pt);
            while attackers.nonempty() {
                threats |= Bitboard::attacks_bb::<{Pt}>(attackers.pop_lsb(), self.pieces());
            }
            threats
        }
    }
    #[inline] pub fn can_castle(&self, cr: CastlingRights) -> bool { self.st.castlingRights & cr }

    #[inline] pub fn castling_rights(&self, c: Color) -> CastlingRights {
        c & self.st.castlingRights
    }

    #[inline] pub fn castling_impeded(&self, cr: CastlingRights) -> bool {
        debug_assert_matches!(cr, CastlingRights::WHITE_OO || CastlingRights::WHITE_OOO || CastlingRights::BLACK_OO || CastlingRights::BLACK_OOO);
        return pieces() & castling_path[cr];
    }

    #[inline] pub fn castling_rook_square(&self, cr: CastlingRights) -> Square {
        debug_assert_matches!(cr, CastlingRights::WHITE_OO || CastlingRights::WHITE_OOO || CastlingRights::BLACK_OO || CastlingRights::BLACK_OOO);
        self.castling_rook_square[cr]
    }

    #[inline] pub fn attackers_to(&self, s: Square) -> Bitboard { return attackers_to(s, pieces()); }

    #[inline] pub fn checkers(&self, ) -> Bitboard { return self.st.checkersBB; }

#[inline] pub fn blockers_for_king(&self, c: Color) -> Bitboard { return self.st.blockersForKing[c]; }

#[inline] pub fn pinners(&self, c: Color) -> Bitboard { return self.st.pinners[c]; }

#[inline] pub fn check_squares(&self, PieceType pt) -> Bitboard { return self.st.checkSquares[pt]; }

#[inline] pub fn key(&self, ) -> Key { return adjust_key50<false>(self.st.key); }

#[inline] pub fn adjust_key50<const AfterMove: bool>(&self, Key k) -> Key {
    return self.st.rule50 < 14 - AfterMove ? k : k ^ Key::new((self.st.rule50 - (14 - AfterMove)) / 8);
}

#[inline] pub fn pawn_key(&self, ) -> Key { return self.st.pawnKey; }

#[inline] pub fn material_key(&self, ) -> Key { return self.st.materialKey; }

#[inline] pub fn non_pawn_material(&self, c: Color) -> Value { return self.st.nonPawnMaterial[c]; }

#[inline] pub fn non_pawn_material(&self, ) -> Value {
    return non_pawn_material(Color::White) + non_pawn_material(Color::Black);
}

#[inline] pub fn game_ply(&self, ) -> int { return gamePly; }

#[inline] pub fn rule50_count(&self, ) -> int { return self.st.rule50; }

#[inline] pub fn is_chess960(&self, ) -> bool { return chess960; }

#[inline] pub fn capture(&self, Move m) -> bool {
    assert(m.is_ok());
    return (!empty(m.to_sq()) && m.type_of() != CASTLING) || m.type_of() == EN_PASSANT;
}

// Returns true if a move is generated from the capture stage, having also
// queen promotions covered, i.e. consistency with the capture stage move
// generation is needed to avoid the generation of duplicate moves.
#[inline] pub fn capture_stage(&self, Move m) -> bool {
    assert(m.is_ok());
    return capture(m) || m.promotion_type() == QUEEN;
}

#[inline] pub fn captured_piece(&self, ) -> Piece { return self.st.capturedPiece; }

#[inline] pub fn put_piece(&mut self, Piece pc, s: Square) {

    board[s] = pc;
    self.by_type_bb[ALL_PIECES] |= self.by_type_bb[type_of(pc)] |= s;
    self.by_color_bb[color_of(pc)] |= s;
    pieceCount[pc]++;
    pieceCount[make_piece(color_of(pc), ALL_PIECES)]++;
}

#[inline] pub fn remove_piece(&mut self, s: Square) {

    Piece pc = board[s];
    self.by_type_bb[ALL_PIECES] ^= s;
    self.by_type_bb[type_of(pc)] ^= s;
    self.by_color_bb[color_of(pc)] ^= s;
    board[s] = NO_PIECE;
    pieceCount[pc]--;
    pieceCount[make_piece(color_of(pc), ALL_PIECES)]--;
}

#[inline] pub fn move_piece(&mut self, from: Square, to: Square) {

    let pc     = self.board[from];
    let fromTo = from | to;
    self.by_type_bb[PieceType::AllPieces] ^= fromTo;
    self.by_type_bb[pc.piece_type()] ^= fromTo;
    self.by_color_bb[color_of(pc)] ^= fromTo;
    board[from] = NO_PIECE;
    board[to]   = pc;
}

    #[inline] pub fn do_move(&mut self, m: Move, new_st: &StateInfo) {
        let gives_check = self.gives_check(m);
        self.do_move_maybe_check(m, new_st, gives_check);
    }

    #[inline] fn do_move_maybe_check(&mut self, m: Move, new_st: &StateInfo, gives_check: bool);

    #[inline] fn state(&self) -> *mut StateInfo  { self.st }


    //
    //  // Properties of moves
    //  bool  legal(Move m) const;
    //  bool  pseudo_legal(const Move m) const;
    //  bool  capture(Move m) const;
    //  bool  capture_stage(Move m) const;
    //  bool  gives_check(Move m) const;
    //  Piece moved_piece(Move m) const;
    //  Piece captured_piece() const;
    //
    //  // Doing and undoing moves
    //  void do_move(Move m, StateInfo& newSt);
    //  void do_move(Move m, StateInfo& newSt, bool givesCheck);
    //  void undo_move(Move m);
    //  void do_null_move(StateInfo& newSt, TranspositionTable& tt);
    //  void undo_null_move();
    //
    //  // Static Exchange Evaluation
    //  bool see_ge(Move m, int threshold = 0) const;
    //
    //  // Accessing hash keys
    //  Key key() const;
    //  Key key_after(Move m) const;
    //  Key material_key() const;
    //  Key pawn_key() const;
    //
    //  // Other properties of the position
    //  Color side_to_move() const;
    //  int   game_ply() const;
    //  bool  is_chess960() const;
    //  bool  is_draw(int ply) const;
    //  bool  upcoming_repetition(int ply) const;
    //  bool  has_repeated() const;
    //  int   rule50_count() const;
    //  Value non_pawn_material(c: Color) const;
    //  Value non_pawn_material() const;
    //
    //  // Position consistency check, for debugging
    //  bool pos_is_ok() const;
    //  void flip();
    //
    //  // Used by NNUE
    //  StateInfo* state() const;
    //
    //  void put_piece(Piece pc, s: Square);
    //  void remove_piece(s: Square);
    //
    // private:
    //  // Initialization helpers (used while setting up a position)
    //  void set_castling_right(c: Color, Square rfrom);
    //  void set_state() const;
    //  void set_check_info() const;
    //
    //  // Other helpers
    //  void move_piece(from: Square, to: Square);
    //  template<bool Do>
    //  void do_castling(Color us, from: Square, Square& to, Square& rfrom, Square& rto);
    //  template<bool AfterMove>
    //  Key adjust_key50(Key k) const;
}

// std::ostream& operator<<(std::ostream& os, const Position& pos);
//
// #[inline] pub fn side_to_move(&self, ) -> Color { return side_to_move; }
//
// #[inline] pub fn piece_on(&self, s: Square) -> Piece {
//     assert(is_ok(s));
//     return board[s];
// }
//
// #[inline] pub fn empty(&self, s: Square) -> bool { return piece_on(s) == NO_PIECE; }
//
// #[inline] pub fn moved_piece(&self, Move m) -> Piece { return piece_on(m.from_sq()); }
//
//
// template<PieceType Pt>
// #[inline] pub fn count(&self, c: Color) -> int {
//     return pieceCount[make_piece(c, Pt)];
// }
//
// template<PieceType Pt>
// #[inline] pub fn count(&self, ) -> int {
//     return count<Pt>(Color::White) + count<Pt>(Color::Black);
// }
//
// template<PieceType Pt>
// #[inline] pub fn square(&self, c: Color) -> Square {
//     assert(count<Pt>(c) == 1);
//     return lsb(pieces(c, Pt));
// }
//
// #[inline] pub fn ep_square(&self, ) -> Square { return self.st.epSquare; }
//

// Initializes the position object with the given FEN string.
// This function is not very robust - make sure that input FENs are correct,
// this is assumed to be the responsibility of the GUI.
// Position& Position::set(const string& fenStr, bool isChess960, StateInfo* si) {
//     /*
//    A FEN string defines a particular position using only the ASCII character set.
//
//    A FEN string contains six fields separated by a space. The fields are:
//
//    1) Piece placement (from white's perspective). Each rank is described, starting
//       with rank 8 and ending with rank 1. Within each rank, the contents of each
//       square are described from file A through file H. Following the Standard
//       Algebraic Notation (SAN), each piece is identified by a single letter taken
//       from the standard English names. White pieces are designated using upper-case
//       letters ("PNBRQK") whilst Black uses lowercase ("pnbrqk"). Blank squares are
//       noted using digits 1 through 8 (the number of blank squares), and "/"
//       separates ranks.
//
//    2) Active color. "w" means white moves next, "b" means black.
//
//    3) Castling availability. If neither side can castle, this is "-". Otherwise,
//       this has one or more letters: "K" (White can castle kingside), "Q" (White
//       can castle queenside), "k" (Black can castle kingside), and/or "q" (Black
//       can castle queenside).
//
//    4) En passant target square (in algebraic notation). If there's no en passant
//       target square, this is "-". If a pawn has just made a 2-square move, this
//       is the position "behind" the pawn. Following X-FEN standard, this is recorded
//       only if there is a pawn in position to make an en passant capture, and if
//       there really is a pawn that might have advanced two squares.
//
//    5) Halfmove clock. This is the number of halfmoves since the last pawn advance
//       or capture. This is used to determine if a draw can be claimed under the
//       fifty-move rule.
//
//    6) Fullmove number. The number of the full move. It starts at 1, and is
//       incremented after Black's move.
// */
//
//     unsigned char      col, row, token;
//     size_t             idx;
//     Square             sq = SQ_A8;
//     std::istringstream ss(fenStr);
//
//     std::memset(this, 0, sizeof(Position));
//     std::memset(si, 0, sizeof(StateInfo));
//     st = si;
//
//     ss >> std::noskipws;
//
//     // 1. Piece placement
//     while ((ss >> token) && !isspace(token))
//     {
//         if (isdigit(token))
//             sq += (token - '0') * EAST;  // Advance the given number of files
//
//         else if (token == '/')
//             sq += 2 * SOUTH;
//
//         else if ((idx = PieceToChar.find(token)) != string::npos)
//         {
//             put_piece(Piece(idx), sq);
//             ++sq;
//         }
//     }
//
//     // 2. Active color
//     ss >> token;
//     sideToMove = (token == 'w' ? WHITE : Color::Black);
//     ss >> token;
//
//     // 3. Castling availability. Compatible with 3 standards: Normal FEN standard,
//     // Shredder-FEN that uses the letters of the columns on which the rooks began
//     // the game instead of KQkq and also X-FEN standard that, in case of Chess960,
//     // if an inner rook is associated with the castling right, the castling tag is
//     // replaced by the file letter of the involved rook, as for the Shredder-FEN.
//     while ((ss >> token) && !isspace(token))
//     {
//         Square rsq;
//         Color  c    = islower(token) ? BLACK : WHITE;
//         Piece  rook = make_piece(c, ROOK);
//
//         token = char(toupper(token));
//
//         if (token == 'K')
//             for (rsq = relative_square(c, SQ_H1); piece_on(rsq) != rook; --rsq)
//             {}
//
//         else if (token == 'Q')
//             for (rsq = relative_square(c, SQ_A1); piece_on(rsq) != rook; ++rsq)
//             {}
//
//         else if (token >= 'A' && token <= 'H')
//             rsq = make_square(File(token - 'A'), relative_rank(c, RANK_1));
//
//         else
//             continue;
//
//         set_castling_right(c, rsq);
//     }
//
//     // 4. En passant square.
//     // Ignore if square is invalid or not on side to move relative rank 6.
//     bool enpassant = false;
//
//     if (((ss >> col) && (col >= 'a' && col <= 'h'))
//         && ((ss >> row) && (row == (sideToMove == WHITE ? '6' : '3'))))
//     {
//         self.st.epSquare = make_square(File(col - 'a'), Rank(row - '1'));
//
//         // En passant square will be considered only if
//         // a) side to move have a pawn threatening epSquare
//         // b) there is an enemy pawn in front of epSquare
//         // c) there is no piece on epSquare or behind epSquare
//         enpassant = pawn_attacks_bb(~sideToMove, self.st.epSquare) & pieces(sideToMove, PieceType::Pawn)
//                  && (pieces(~sideToMove, PieceType::Pawn) & (self.st.epSquare + pawn_push(~sideToMove)))
//                  && !(pieces() & (self.st.epSquare | (self.st.epSquare + pawn_push(sideToMove))));
//     }
//
//     if (!enpassant)
//         self.st.epSquare = SQ_NONE;
//
//     // 5-6. Halfmove clock and fullmove number
//     ss >> std::skipws >> self.st.rule50 >> gamePly;
//
//     // Convert from fullmove starting from 1 to gamePly starting from 0,
//     // handle also common incorrect FEN with fullmove = 0.
//     gamePly = std::max(2 * (gamePly - 1), 0) + (sideToMove == Color::Black);
//
//     chess960 = isChess960;
//     set_state();
//
//     assert(pos_is_ok());
//
//     return *this;
// }
