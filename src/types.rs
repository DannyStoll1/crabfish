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

use bitmask_enum::bitmask;
use derive_more::{Add, AddAssign, Display, Neg, Sub, SubAssign};
use enum_map::{Enum, EnumMap};
use std::marker::ConstParamTy;
use strum::{EnumCount, EnumIter};

use crate::bitboard::square_distance_table;

// When compiling with provided Makefile (e.g. for Linux and OSX), configuration
// is done automatically. To get started type 'make help'.
//
// When Makefile is not used (e.g. with Microsoft Visual Studio) some switches
// need to be set manually:
//
// -DNDEBUG      | Disable debugging mode. Always use this for release.
//
// -DNO_PREFETCH | Disable use of prefetch asm-instruction. You may need this to
//               | run on some very old machines.
//
// -DUSE_POPCNT  | Add runtime support for use of popcnt asm-instruction. Works
//               | only in 64-bit mode and requires hardware with popcnt support.
//
// -DUSE_PEXT    | Add runtime support for use of pext asm-instruction. Works
//               | only in 64-bit mode and requires hardware with pext support.

// Predefined macros hell:
//
// __GNUC__                Compiler is GCC, Clang or ICX
// __clang__               Compiler is Clang or ICX
// __INTEL_LLVM_COMPILER   Compiler is ICX
// _MSC_VER                Compiler is MSVC
// _WIN32                  Building on Windows (any)
// _WIN64                  Building on Windows 64 bit

// #if defined(__GNUC__) && (__GNUC__ < 9 || (__GNUC__ == 9 && __GNUC_MINOR__ <= 2)) \
//   && defined(_WIN32) && !defined(__clang__)
//     #define ALIGNAS_ON_STACK_VARIABLES_BROKEN
// #endif
//
// #define ASSERT_ALIGNED(ptr, alignment) assert(reinterpret_cast<uintptr_t>(ptr) % alignment == 0)
//
// #if defined(_WIN64) && defined(_MSC_VER)  // No Makefile used
//     #include <intrin.h>                   // Microsoft header for _BitScanForward64()
//     #define IS_64BIT
// #endif
//
// #if defined(USE_POPCNT) && defined(_MSC_VER)
//     #include <nmmintrin.h>  // Microsoft header for _mm_popcnt_u64()
// #endif
//
// #if !defined(NO_PREFETCH) && defined(_MSC_VER)
//     #include <xmmintrin.h>  // Microsoft header for _mm_prefetch()
// #endif
//
// #if defined(USE_PEXT)
//     #include <immintrin.h>  // Header for _pext_u64() intrinsic
//     #define pext(b, m) _pext_u64(b, m)
// #else
//     #define pext(b, m) 0
// #endif

#[cfg(target_feature = "popcnt")]
pub const HasPopCnt: bool = true;

#[cfg(not(target_feature = "popcnt"))]
pub const HasPopCnt: bool = false;

#[cfg(target_feature = "bmi2")]
pub const HasPext: bool = true;
#[cfg(not(target_feature = "bmi2"))]
pub const HasPext: bool = false;

#[cfg(target_pointer_width = "64")]
pub const Is64Bit: bool = true;
#[cfg(not(target_pointer_width = "64"))]
pub const Is64Bit: bool = false;

#[derive(Clone, Copy, Debug)]
pub struct Key(u64);
impl Key
{
    // Based on a congruential pseudo-random number generator
    pub const fn new(seed: u64) -> Self
    {
        Self(seed * 6_364_136_223_846_793_005_u64 + 1_442_695_040_888_963_407_u64)
    }
}

const MAX_MOVES: i32 = 256;
const MAX_PLY: i32 = 246;

#[derive(Clone, Copy, Debug, PartialEq, Eq, ConstParamTy, Enum, EnumIter, EnumCount)]
pub enum Color
{
    White,
    Black,
}
impl Color
{
    const fn discriminant(self) -> u8
    {
        unsafe { std::mem::transmute(self) }
    }
    const fn opposite(self) -> Self
    {
        match self {
            Self::White => Self::Black,
            Self::Black => Self::White,
        }
    }
    const fn pawn_push(self) -> Direction
    {
        match self {
            Self::White => Direction::North,
            Self::Black => Direction::South,
        }
    }
}

#[bitmask]
pub enum CastlingRights
{
    WhiteOO,
    WhiteOOO,
    BlackOO,
    BlackOOO,
}
impl CastlingRights
{
    const NoCastling: Self = Self::none();
    const KingSide: Self = Self::WhiteOO.or(Self::BlackOO);
    const QueenSide: Self = Self::WhiteOOO.or(Self::BlackOOO);
    const WhiteCastling: Self = Self::WhiteOO.or(Self::WhiteOOO);
    const BlackCastling: Self = Self::BlackOO.or(Self::BlackOOO);
    const AnyCastling: Self = Self::WhiteCastling.or(Self::BlackCastling);
    const COUNT: usize = 16;
}

#[bitmask]
pub enum Bound
{
    Upper,
    Lower,
}
impl Bound
{
    const None: Self = Self::none();
    const Exact: Self = Self::Upper.or(Self::Lower);
}

// Value is used as an alias for i32, this is done to differentiate between a search
// value and any other integer value. The values used in search are always supposed
// to be in the range (-VALUE_NONE, VALUE_NONE] and should not exceed this range.
#[derive(Clone, Copy, Add, Sub, Neg, AddAssign, SubAssign)]
pub struct Value(i32);

impl Value
{
    const ZERO: Self = Self(0);
    const DRAW: Self = Self(0);
    const NONE: Self = Self(32002);
    const INFINITE: Self = Self(32001);

    const MATE: Self = Self(32000);
    const MATE_IN_MAX_PLY: Self = Self(Self::MATE.0 - MAX_PLY);
    const MATED_IN_MAX_PLY: Self = Self(-Self::MATE_IN_MAX_PLY.0);

    const TB: Self = Self(Self::MATE_IN_MAX_PLY.0 - 1);
    const TB_WIN_IN_MAX_PLY: Self = Self(Self::TB.0 - MAX_PLY);
    const TB_LOSS_IN_MAX_PLY: Self = Self(-Self::TB_WIN_IN_MAX_PLY.0);

    // In the code, we make the assumption that these values
    // are such that non_pawn_material() can be used to uniquely
    // identify the material on the board.
    const PawnValue: Self = Self(208);
    const KnightValue: Self = Self(781);
    const BishopValue: Self = Self(825);
    const RookValue: Self = Self(1276);
    const QueenValue: Self = Self(2538);

    const fn mate_in(ply: i32) -> Self
    {
        Self(Self::MATE.0 - ply)
    }
    const fn mated_in(ply: i32) -> Self
    {
        Self(ply - Self::MATE.0)
    }
}

#[derive(Clone, Copy, Debug, Enum, EnumIter, ConstParamTy, PartialEq, Eq)]
pub enum PieceType
{
    // AllPieces = 0,
    // None,
    Pawn,
    Knight,
    Bishop,
    Rook,
    Queen,
    King,
}
impl PieceType
{
    const fn discriminant(self) -> u8
    {
        unsafe { std::mem::transmute(self) }
    }
}

#[derive(Enum)]
pub enum Piece
{
    WPawn = 0,
    WKnight,
    WBishop,
    WRook,
    WQueen,
    WKing,
    _Invalid6,
    _Invalid7,
    BPawn = 8,
    BKnight,
    BBishop,
    BRook,
    BQueen,
    BKing,
    _Invalid14,
    _Invalid15,
}
impl Piece
{
    const VALUE_MAP: EnumMap<Self, Value> = EnumMap::from_array([
        Value::PawnValue,
        Value::KnightValue,
        Value::BishopValue,
        Value::RookValue,
        Value::QueenValue,
        Value::ZERO,
        Value::ZERO,
        Value::ZERO,
        Value::PawnValue,
        Value::KnightValue,
        Value::BishopValue,
        Value::RookValue,
        Value::QueenValue,
        Value::ZERO,
        Value::ZERO,
        Value::ZERO,
    ]);

    pub fn value(self) -> Value
    {
        Self::VALUE_MAP[self]
    }
}

#[derive(Clone, Copy, Add, Sub, Neg, AddAssign, SubAssign)]
pub struct Depth(i32);

impl Depth
{
    // The following DEPTH_ constants are used for transposition table entries
    // and quiescence search move generation stages. In regular search, the
    // depth stored in the transposition table is literal: the search depth
    // (effort) used to make the corresponding transposition table value. In
    // quiescence search, however, the transposition table entries only store
    // the current quiescence move generation stage (which should thus compare
    // lower than any regular search depth).
    const QS: Self = Self(0);

    // For transposition table entries where no searching at all was done
    // (whether regular or qsearch) we use DEPTH_UNSEARCHED, which should thus
    // compare lower than any quiescence or regular depth. DEPTH_ENTRY_OFFSET
    // is used only for the transposition table entry occupancy check (see tt.cpp),
    // and should thus be lower than DEPTH_UNSEARCHED.
    const UNSEARCHED: Self = Self(-2);
    const ENTRY_OFFSET: Self = Self(-3);
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Square(pub i8);
impl Square
{
    pub const COUNT: usize = Rank::COUNT * File::COUNT;
    pub const NO_SQUARE: Self = Self(64);
    pub const A1: Self = Self(0);
    pub const H1: Self = Self(File::COUNT as i8 - 1);
    pub const A8: Self = Self(Self::COUNT as i8 - File::COUNT as i8);
    pub const H8: Self = Self(Self::COUNT as i8 - 1);

    pub const fn new(file: File, rank: Rank) -> Self
    {
        Self((file.discriminant() + 8 * rank.discriminant()) as i8)
    }

    pub const fn is_ok(self) -> bool
    {
        self.0 >= Self::A1.0 && self.0 <= Self::H8.0
    }

    pub const fn file(self) -> File
    {
        unsafe { std::mem::transmute(self.0 & 7) }
    }

    pub const fn rank(self) -> Rank
    {
        unsafe { std::mem::transmute(self.0 >> 3) }
    }

    pub const fn flip_rank(self) -> Self
    {
        Self(self.0 ^ Self::A8.0)
    }
    pub const fn flip_file(self) -> Self
    {
        Self(self.0 ^ Self::H1.0)
    }

    pub const fn relative_square(self, c: Color) -> Self
    {
        Self(self.0 ^ (c.discriminant() as i8 * 56))
    }

    pub const fn relative_rank(self, c: Color) -> Rank
    {
        self.rank().relative_to(c)
    }

    pub fn iter_board() -> impl Iterator<Item = Self>
    {
        (0..Self::COUNT as i8).map(Self)
    }

    pub(crate) fn prev(self) -> Self
    {
        debug_assert!(self != Self::A1);
        Self(self.0 - 1)
    }
}
// {
//     A1, B1, C1, D1, E1, F1, G1, H1,
//     A2, B2, C2, D2, E2, F2, G2, H2,
//     A3, B3, C3, D3, E3, F3, G3, H3,
//     A4, B4, C4, D4, E4, F4, G4, H4,
//     A5, B5, C5, D5, E5, F5, G5, H5,
//     A6, B6, C6, D6, E6, F6, G6, H6,
//     A7, B7, C7, D7, E7, F7, G7, H7,
//     A8, B8, C8, D8, E8, F8, G8, H8,
//     None,
// }

#[derive(Clone)]
pub struct BoardMap<T>([T; Square::COUNT]);
impl<T> BoardMap<T>
{
    pub fn from_fn(mut f: impl FnMut(Square) -> T) -> Self
    {
        Self(std::array::from_fn(|i| f(Square(i as i8))))
    }
    pub fn map<V>(self, f: impl FnMut(T) -> V) -> BoardMap<V>
    {
        BoardMap(self.0.map(f))
    }
}
impl<T> std::ops::Index<Square> for BoardMap<T>
{
    type Output = T;
    fn index(&self, index: Square) -> &Self::Output
    {
        &self.0[index.0 as usize]
    }
}
impl<T> std::ops::IndexMut<Square> for BoardMap<T>
{
    fn index_mut(&mut self, index: Square) -> &mut Self::Output
    {
        &mut self.0[index.0 as usize]
    }
}
impl<T: Default> Default for BoardMap<T>
{
    fn default() -> Self
    {
        Self(std::array::from_fn(|_| T::default()))
    }
}

pub trait Metric
{
    fn distance(x: Square, y: Square) -> u8;
}

impl Metric for Square
{
    fn distance(x: Square, y: Square) -> u8
    {
        square_distance_table()[x][y]
    }
}
impl Metric for Rank
{
    fn distance(x: Square, y: Square) -> u8
    {
        (x.rank().discriminant() as i8 - y.rank().discriminant() as i8).unsigned_abs()
    }
}
impl Metric for File
{
    fn distance(x: Square, y: Square) -> u8
    {
        (x.file().discriminant() as i8 - y.file().discriminant() as i8).unsigned_abs()
    }
}

#[derive(Clone, Copy, Add, Neg, AddAssign, SubAssign, PartialEq, Eq, ConstParamTy)]
pub struct Direction(i8);
impl Direction
{
    pub const North: Self = Self(8);
    pub const East: Self = Self(1);
    pub const South: Self = Self(-8);
    pub const West: Self = Self(-1);

    pub const NorthEast: Self = Self(9);
    pub const SouthEast: Self = Self(-7);
    pub const SouthWest: Self = Self(-9);
    pub const NorthWest: Self = Self(7);

    pub const North2: Self = Self(Self::North.0 * 2);
    pub const East2: Self = Self(Self::East.0 * 2);
    pub const South2: Self = Self(Self::South.0 * 2);
    pub const West2: Self = Self(Self::West.0 * 2);

    pub const KING_MOVES: [Self; 8] = [
        Self::SouthWest,
        Self::South,
        Self::SouthEast,
        Self::West,
        Self::East,
        Self::NorthWest,
        Self::North,
        Self::NorthEast,
    ];
    pub const KNIGHT_MOVES: [Self; 8] = [
        Self::new(-1, -2),
        Self::new(1, -2),
        Self::new(-2, -1),
        Self::new(2, -1),
        Self::new(-2, 1),
        Self::new(2, 1),
        Self::new(-1, 2),
        Self::new(1, 2),
    ];

    pub const fn new(dx: i8, dy: i8) -> Self
    {
        Self(dx + 8 * dy)
    }
}
impl std::ops::Mul<Direction> for i8
{
    type Output = Direction;
    fn mul(self, rhs: Direction) -> Self::Output
    {
        Direction(self * rhs.0)
    }
}

#[derive(Clone, Copy, Debug, EnumIter, EnumCount)]
// struct File(i8);
#[repr(u8)]
pub enum File
{
    A,
    B,
    C,
    D,
    E,
    F,
    G,
    H,
}
impl File
{
    // const A: Self = Self(0);
    // const B: Self = Self(1);
    // const C: Self = Self(2);
    // const D: Self = Self(3);
    // const E: Self = Self(4);
    // const F: Self = Self(5);
    // const G: Self = Self(6);
    // const H: Self = Self(7);

    pub(crate) const fn discriminant(self) -> u8
    {
        unsafe { std::mem::transmute(self) }
    }

    #[inline]
    pub const fn edge_distance(self) -> u8
    {
        let d = self.discriminant();
        if d < 4 {
            d
        } else {
            7 - d
        }
    }
}

#[derive(Clone, Copy, Debug, EnumIter, EnumCount, Display)]
// struct Rank(i8);
#[repr(u8)]
pub enum Rank
{
    #[display("0")]
    Row0,
    #[display("1")]
    Row1,
    #[display("2")]
    Row2,
    #[display("3")]
    Row3,
    #[display("4")]
    Row4,
    #[display("5")]
    Row5,
    #[display("6")]
    Row6,
    #[display("7")]
    Row7,
}
impl Rank
{
    pub(crate) const fn discriminant(self) -> u8
    {
        unsafe { std::mem::transmute(self) }
    }
    const fn relative_to(self, c: Color) -> Self
    {
        unsafe { std::mem::transmute(self.discriminant() ^ (c.discriminant() * 7)) }
    }
}

// Keep track of what a move changes on the board (used by NNUE)
pub struct DirtyPiece
{
    // Number of changed pieces
    dirty_num: i8,

    // Max 3 pieces can change in one move. A promotion with capture moves
    // both the pawn and the captured piece to SQ_NONE and the piece promoted
    // to from SQ_NONE to the capture square.
    piece: [Piece; 3],

    // From and to squares, which may be SQ_NONE
    from: [Square; 3],
    to: [Square; 3],
}

// Additional operators to add a Direction to a Square
impl std::ops::Add<Direction> for Square
{
    type Output = Self;
    fn add(self, rhs: Direction) -> Self::Output
    {
        Self(self.0 + rhs.0)
    }
}
impl std::ops::Sub<Direction> for Square
{
    type Output = Self;
    fn sub(self, rhs: Direction) -> Self::Output
    {
        Self(self.0 - rhs.0)
    }
}
impl std::ops::AddAssign<Direction> for Square
{
    fn add_assign(&mut self, rhs: Direction)
    {
        self.0 += rhs.0;
    }
}
impl std::ops::SubAssign<Direction> for Square
{
    fn sub_assign(&mut self, rhs: Direction)
    {
        self.0 -= rhs.0;
    }
}

// Swap color of piece B_KNIGHT <-> W_KNIGHT
// const operator: Piece~(Piece pc) { return Piece(pc ^ 8); }
//
// const operator: CastlingRights&(Color c, CastlingRights cr) {
//     return CastlingRights((c == WHITE ? WHITE_CASTLING : BLACK_CASTLING) & cr);
// }

impl Piece
{
    pub const fn new(c: Color, pt: PieceType) -> Self
    {
        let discrim = (c.discriminant() << 3) + pt.discriminant();
        unsafe { std::mem::transmute(discrim) }
    }
    const fn discriminant(self) -> u8
    {
        unsafe { std::mem::transmute(self) }
    }
    pub const fn piece_type(self) -> PieceType
    {
        let discrim = self.discriminant();
        unsafe { std::mem::transmute(discrim & 7) }
    }
    #[inline]
    const fn color(self) -> Color
    {
        // assert_ne!(self, Self::None);
        unsafe { std::mem::transmute(self.discriminant() >> 3) }
    }
}

#[repr(u16)]
pub enum MoveType
{
    Normal = 0,
    Promotion = 1 << 14,
    EnPassant = 2 << 14,
    Castling = 3 << 14,
}
impl MoveType
{
    const fn discriminant(self) -> u16
    {
        unsafe { std::mem::transmute(self) }
    }
}

// A move needs 16 bits to be stored
//
// bit  0- 5: destination square (from 0 to 63)
// bit  6-11: origin square (from 0 to 63)
// bit 12-13: promotion piece type - 2 (from KNIGHT-2 to QUEEN-2)
// bit 14-15: special move flag: promotion (1), en passant (2), castling (3)
// NOTE: en passant bit is set only when a pawn can be captured
//
// Special cases are Move::none() and Move::null(). We can sneak these in because
// in any normal move the destination square and origin square are always different,
// but Move::none() and Move::null() have the same origin and destination square.

#[derive(Default, Clone, Copy, PartialEq, Eq)]
pub struct Move(pub u16);
impl Move
{
    pub const fn null() -> Self
    {
        Self(65)
    }
    pub const fn none() -> Self
    {
        Self(0)
    }

    pub const fn standard(from: Square, to: Square) -> Self
    {
        Self(((from.0 as u16) << 6) + to.0 as u16)
    }

    pub const fn new(from: Square, to: Square, pt: PieceType, move_type: MoveType) -> Self
    {
        let move_type_mask = move_type.discriminant();
        let piece_type_mask = ((pt.discriminant() - PieceType::Knight.discriminant()) as u16) << 12;
        let from_mask = (from.0 as u16) << 6;
        let to_mask = to.0 as u16;
        Self(move_type_mask + piece_type_mask + from_mask + to_mask)
    }

    const fn src_square(self) -> Square
    {
        assert!(self.is_ok());
        Square(((self.0 >> 6) & 0x3F) as i8)
    }

    const fn dest_square(self) -> Square
    {
        assert!(self.is_ok());
        Square((self.0 & 0x3F) as i8)
    }

    pub const fn src_and_dest_mask(self) -> u16
    {
        self.0 & 0xFFF
    }

    pub const fn move_type(self) -> MoveType
    {
        unsafe { std::mem::transmute(self.0 & (3 << 14)) }
    }

    pub const fn promotion_type(self) -> PieceType
    {
        unsafe {
            std::mem::transmute(((self.0 >> 12) & 3) as u8 + PieceType::Knight.discriminant())
        }
    }

    pub const fn is_ok(self) -> bool
    {
        self.0 != Self::none().0 && self.0 != Self::null().0
    }

    pub const fn raw(self) -> u16
    {
        self.0
    }

    pub const fn move_hash(self) -> Key
    {
        Key::new(self.0 as u64)
    }
}
impl From<Move> for bool
{
    fn from(value: Move) -> Self
    {
        value.0 != 0
    }
}
