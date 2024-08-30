use std::{cell::SyncUnsafeCell, sync::LazyLock};

// /*
//   Stockfish, a UCI chess playing engine derived from Glaurung 2.1
//   Copyright (C) 2004-2024 The Stockfish developers (see AUTHORS file)
//
//   Stockfish is free software: you can redistribute it and/or modify
//   it under the terms of the GNU General Public License as published by
//   the Free Software Foundation, either version 3 of the License, or
//   (at your option) any later version.
//
//   Stockfish is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//   GNU General Public License for more details.
//
//   You should have received a copy of the GNU General Public License
//   along with this program.  If not, see <http://www.gnu.org/licenses/>.
// */
use crate::types::{
    BoardMap, Color, Direction, File, HasPext, Is64Bit, Metric, PieceType, Rank, Square,
};
use derive_more::{
    BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Shl, ShlAssign, Shr, ShrAssign,
};
use enum_map::EnumMap;
use strum::IntoEnumIterator as _;

#[inline]
pub fn square_distance_table() -> &'static BoardMap<BoardMap<u8>>
{
    static TABLE: LazyLock<BoardMap<BoardMap<u8>>> = LazyLock::new(init_square_distances);
    &TABLE
}

#[inline]
pub fn popcnt_table() -> &'static [u8; 1 << 16]
{
    static TABLE: LazyLock<[u8; 1 << 16]> = LazyLock::new(init_popcnt);
    &TABLE
}

#[inline]
pub fn pseudo_attacks() -> &'static EnumMap<PieceType, BoardMap<Bitboard>>
{
    static TABLE: LazyLock<EnumMap<PieceType, BoardMap<Bitboard>>> =
        LazyLock::new(init_pseudo_attacks);
    &TABLE
}

#[inline]
fn bishop_magics() -> &'static BoardMap<Magic>
{
    static MAGICS: LazyLock<BoardMap<Magic>> = LazyLock::new(|| {
        let init_buf: BoardMap<MagicInit> = BoardMap::default();
        init_magics::<{ PieceType::Bishop }>(init_buf)
    });
    &MAGICS
}

#[inline]
fn rook_magics() -> &'static BoardMap<Magic>
{
    static MAGICS: LazyLock<BoardMap<Magic>> = LazyLock::new(|| {
        let init_buf: BoardMap<MagicInit> = BoardMap::default();
        init_magics::<{ PieceType::Rook }>(init_buf)
    });
    &MAGICS
}

fn init_popcnt() -> [u8; 1 << 16]
{
    let mut popcnt16 = [0u8; 1 << 16];
    for (i, x) in popcnt16.iter_mut().enumerate() {
        *x = i.count_ones() as u8;
    }
    popcnt16
}

fn init_square_distances() -> BoardMap<BoardMap<u8>>
{
    let mut distances: BoardMap<BoardMap<u8>> = BoardMap::default();
    for src in Square::iter_board() {
        for dst in Square::iter_board() {
            distances[src][dst] = Rank::distance(src, dst).max(File::distance(src, dst));
        }
    }
    distances
}

fn init_pawn_attacks() -> BoardMap<EnumMap<Color, Bitboard>>
{
    let mut pawn_attacks: BoardMap<EnumMap<Color, Bitboard>> = BoardMap::default();
    for square in Square::iter_board() {
        let bb = Bitboard::square_bb(square);
        pawn_attacks[square][Color::White] = bb.pawn_attacks::<{ Color::White }>();
        pawn_attacks[square][Color::Black] = bb.pawn_attacks::<{ Color::Black }>();
    }
    pawn_attacks
}

fn init_pseudo_attacks() -> EnumMap<PieceType, BoardMap<Bitboard>>
{
    let mut pseudo_attacks: EnumMap<PieceType, BoardMap<Bitboard>> = EnumMap::default();
    for sq in Square::iter_board() {
        for dir in Direction::KING_MOVES {
            pseudo_attacks[PieceType::King][sq] |= Bitboard::safe_destination(sq, dir);
        }

        for dir in Direction::KNIGHT_MOVES {
            pseudo_attacks[PieceType::Knight][sq] |= Bitboard::safe_destination(sq, dir);
        }

        pseudo_attacks[PieceType::Bishop][sq] =
            Bitboard::attacks_bb::<{ PieceType::Bishop }>(sq, Bitboard::EMPTY);
        pseudo_attacks[PieceType::Rook][sq] =
            Bitboard::attacks_bb::<{ PieceType::Rook }>(sq, Bitboard::EMPTY);
        pseudo_attacks[PieceType::Queen][sq] =
            pseudo_attacks[PieceType::Bishop][sq] | pseudo_attacks[PieceType::Rook][sq];
    }
    pseudo_attacks
}

fn init_line_bbs() -> BoardMap<BoardMap<Bitboard>>
{
    let mut line_bbs: BoardMap<BoardMap<Bitboard>> = BoardMap::default();
    let pseudos = pseudo_attacks();
    for src in Square::iter_board() {
        for pt in [PieceType::Bishop, PieceType::Rook] {
            for dst in Square::iter_board() {
                if pseudos[pt][src].contains_square(dst) {
                    line_bbs[src][dst] = (Bitboard::attacks_for_piece(pt, src, Bitboard::EMPTY)
                        & Bitboard::attacks_for_piece(pt, dst, Bitboard::EMPTY))
                        | Bitboard::square_bb(src)
                        | Bitboard::square_bb(dst);
                }
            }
        }
    }
    line_bbs
}

fn init_between_bbs() -> BoardMap<BoardMap<Bitboard>>
{
    let mut between_bbs: BoardMap<BoardMap<Bitboard>> = BoardMap::default();
    for src in Square::iter_board() {
        for pt in [PieceType::Bishop, PieceType::Rook] {
            for dst in Square::iter_board() {
                if (pseudo_attacks()[pt][src] & dst.into()).nonzero() {
                    between_bbs[src][dst] = Bitboard::attacks_for_piece(pt, src, dst.into())
                        & Bitboard::attacks_for_piece(pt, dst, src.into());
                }
                between_bbs[src][dst] |= dst.into();
            }
        }
    }
    between_bbs
}

static ROOK_TABLE: SyncUnsafeCell<[Bitboard; 0x19000]> =
    SyncUnsafeCell::new([Bitboard::EMPTY; 0x19000]);
static BISHOP_TABLE: SyncUnsafeCell<[Bitboard; 0x1480]> =
    SyncUnsafeCell::new([Bitboard::EMPTY; 0x1480]);

// static ROOK_TABLE: [SyncUnsafeCell<Bitboard>; 0x19000] =
//     unsafe { std::mem::transmute([Bitboard::EMPTY; 0x19000]) };
// static BISHOP_TABLE: [SyncUnsafeCell<Bitboard>; 0x1480] =
//     unsafe { std::mem::transmute([Bitboard::EMPTY; 0x1480]) };

// Computes all rook and bishop attacks at startup. Magic
// bitboards are used to look up attacks of sliding pieces. As a reference see
// www.chessprogramming.org/Magic_Bitboards. In particular, here we use the so
// called "fancy" approach.
fn init_magics<const PIECE: PieceType>(mut init_buf: BoardMap<MagicInit>) -> BoardMap<Magic>
{
    let table: &'static SyncUnsafeCell<[Bitboard]> = match PIECE {
        PieceType::Rook => &ROOK_TABLE,
        PieceType::Bishop => &BISHOP_TABLE,
        _ => unreachable!(),
    };

    // const seeds: [[i32; Rank::COUNT]; 2] = [
    //     [8977, 44560, 54343, 38998, 5731, 95205, 104912, 17020],
    //     [728, 10316, 55013, 32803, 12281, 15100, 16645, 255],
    // ];

    let mut occupancy = [Bitboard::EMPTY; 4096];
    let mut reference = [Bitboard::EMPTY; 4096];
    // let mut epoch = [0; 4096];
    // let mut cnt = 0;
    let mut size: usize = 0;

    for sq in Square::iter_board() {
        // Board edges are not considered in the relevant occupancies
        let edges = ((Bitboard::Rank1 | Bitboard::Rank8) & !Bitboard::rank_bb(sq.rank()))
            | ((Bitboard::FileA | Bitboard::FileH) & !Bitboard::file_bb(sq.file()));

        // Given a square 's', the mask is the bitboard of sliding attacks from
        // 's' computed on an empty board. The index must be big enough to contain
        // all the attacks for each possible subset of the mask and so is 2 power
        // the number of 1s of the mask. Hence we deduce the size of the shift to
        // apply to the 64 or 32 bits word to get the index.

        let attacks = if sq == Square::A1 {
            0
        } else {
            init_buf[sq.prev()].attacks + size
        };

        let m = &mut init_buf[sq];
        m.mask = Bitboard::sliding_attack(PIECE, sq, Bitboard::EMPTY) & !edges;
        m.shift = (if Is64Bit { 64 } else { 32 }) - m.mask.popcount();

        // Set the offset for the attacks table of the square. We have individual
        // table sizes for each square with "Fancy Magic Bitboards".
        m.attacks = attacks;

        // Use Carry-Rippler trick to enumerate all subsets of masks[s] and
        // store the corresponding sliding attack bitboard in reference[].
        let mut b = Bitboard::EMPTY;
        size = 0;
        loop {
            occupancy[size] = b;
            reference[size] = Bitboard::sliding_attack(PIECE, sq, b);

            if HasPext {
                unsafe {
                    let offset = std::arch::x86_64::_pext_u64(b.0, m.mask.0) as usize;
                    (*table.get())[m.attacks + offset] = reference[size];
                }
            }

            size += 1;
            b = Bitboard((b.0.wrapping_sub(m.mask.0)) & m.mask.0);
            if b.is_empty() {
                break;
            }
        }

        if HasPext {
            continue;
        }

        // let mut rng = PRNG::new(seeds[Is64Bit as usize][s.rank().0 as usize]);
        //
        // 'outer: loop {
        //     m.magic = Bitboard::EMPTY;
        //     while ((m.magic * m.mask).0 >> 56).count_ones() < 6 {
        //         m.magic = rng.sparse_rand();
        //     }
        //
        //     cnt += 1;
        //     for i in 0..size {
        //         let idx = m.index(occupancy[i]);
        //
        //         if epoch[idx] < cnt {
        //             epoch[idx] = cnt;
        //             unsafe {
        //                 *m.attacks.add(idx) = reference[i];
        //             }
        //         } else if unsafe { *m.attacks.add(idx) } != reference[i] {
        //             continue 'outer;
        //         }
        //     }
        //     break;
        // }
    }
    let table_frozen: &[Bitboard] = unsafe { &*table.get() };
    unsafe { init_buf.map(|m| m.finalize(table_frozen)) }
}

#[derive(
    Clone,
    Copy,
    Default,
    BitOr,
    BitAnd,
    BitXor,
    BitOrAssign,
    BitAndAssign,
    BitXorAssign,
    Shl,
    Shr,
    ShlAssign,
    ShrAssign,
)]
pub struct Bitboard(pub u64);
impl Bitboard
{
    pub const FileA: Self = Self(0x0101_0101_0101_0101_u64);
    pub const FileB: Self = Self(Self::FileA.0 << 1);
    pub const FileC: Self = Self(Self::FileA.0 << 2);
    pub const FileD: Self = Self(Self::FileA.0 << 3);
    pub const FileE: Self = Self(Self::FileA.0 << 4);
    pub const FileF: Self = Self(Self::FileA.0 << 5);
    pub const FileG: Self = Self(Self::FileA.0 << 6);
    pub const FileH: Self = Self(Self::FileA.0 << 7);

    pub const Rank1: Self = Self(0xFF);
    pub const Rank2: Self = Self(Self::Rank1.0 << 8);
    pub const Rank3: Self = Self(Self::Rank1.0 << (8 * 2));
    pub const Rank4: Self = Self(Self::Rank1.0 << (8 * 3));
    pub const Rank5: Self = Self(Self::Rank1.0 << (8 * 4));
    pub const Rank6: Self = Self(Self::Rank1.0 << (8 * 5));
    pub const Rank7: Self = Self(Self::Rank1.0 << (8 * 6));
    pub const Rank8: Self = Self(Self::Rank1.0 << (8 * 7));

    pub const EMPTY: Self = Self(0);

    #[must_use]
    #[inline]
    pub const fn square_bb(s: Square) -> Self
    {
        debug_assert!(s.is_ok());
        Self(1 << s.0)
    }
    const fn rank_bb(r: Rank) -> Self
    {
        Self(Self::Rank1.0 << (8 * r.discriminant()))
    }
    const fn file_bb(f: File) -> Self
    {
        Self(Self::FileA.0 << f.discriminant())
    }

    const fn more_than_one(self) -> bool
    {
        self.0 & (self.0 - 1) != 0
    }

    const fn is_empty(self) -> bool
    {
        self.0 != 0
    }

    // Returns the squares attacked by pawns of the given color
    // from the squares in the given bitboard.
    const fn pawn_attacks<const C: Color>(self) -> Self
    {
        Self(match C {
            Color::White => {
                self.shift::<{ Direction::NorthWest }>().0
                    | self.shift::<{ Direction::NorthEast }>().0
            }
            Color::Black => {
                self.shift::<{ Direction::SouthWest }>().0
                    | self.shift::<{ Direction::SouthEast }>().0
            }
        })
    }

    // Moves a bitboard one or two steps as specified by the direction D
    const fn shift<const D: Direction>(self) -> Self
    {
        Self(match D {
            Direction::North => self.0 << 8,
            Direction::South => self.0 >> 8,
            Direction::North2 => self.0 << 16,
            Direction::South2 => self.0 >> 16,
            Direction::East => (self.0 & !Self::FileH.0) << 1,
            Direction::West => (self.0 & !Self::FileA.0) >> 1,
            Direction::NorthEast => (self.0 & !Self::FileH.0) << 9,
            Direction::NorthWest => (self.0 & !Self::FileA.0) << 7,
            Direction::SouthEast => (self.0 & !Self::FileH.0) >> 7,
            Direction::SouthWest => (self.0 & !Self::FileA.0) >> 9,
            _ => 0,
        })
    }

    pub const fn contains_square(self, sq: Square) -> bool
    {
        self.0 & Self::square_bb(sq).0 != 0
    }

    // Returns a bitboard representing an entire line (from board edge
    // to board edge) that intersects the two given squares. If the given squares
    // are not on a same file/rank/diagonal, the function returns 0. For instance,
    // Bitboard::line(Square::C4, Square::F7) will return a bitboard with the A2-G8 diagonal.
    // #[inline]
    pub fn line(s0: Square, s1: Square) -> Self
    {
        assert!(s0.is_ok() && s1.is_ok());
        todo!()
        // return LineBB[s1][s2];
    }

    // Returns a bitboard representing the squares in the semi-open
    // segment between the squares s1 and s2 (excluding s1 but including s2). If the
    // given squares are not on a same file/rank/diagonal, it returns s2. For instance,
    // between(Square::C4, SQ_F7) will return a bitboard with squares D5, E6 and F7, but
    // between(Square::E6, SQ_F8) will return a bitboard with the square F8. This trick
    // allows to generate non-king evasion moves faster: the defending piece must either
    // interpose itself to cover the check or capture the checking piece.
    // pub fn between(s1: Square, s2: Square) -> Self {
    //
    //     assert!(s1.is_ok() && s2.is_ok());
    //     return BetweenBB[s1][s2];
    // }

    pub const fn nonzero(self) -> bool
    {
        self.0 != 0
    }

    // Counts the number of non-zero bits in a bitboard.
    #[inline]
    pub const fn popcount(self) -> u32
    {
        self.0.count_ones()
    }

    // Returns the least significant bit in a non-zero bitboard.
    #[inline]
    pub fn lsb(self) -> Square
    {
        assert!(self.nonzero());
        Square(self.0.trailing_zeros() as i8)
    }
    // Returns the most significant bit in a non-zero bitboard.
    #[inline]
    pub fn msb(self) -> Square
    {
        assert!(self.nonzero());
        Square(63 ^ self.0.leading_zeros() as i8)
    }

    // Returns the bitboard of the least significant
    // square of a non-zero bitboard. It is equivalent to square_bb(lsb(bb)).
    #[inline]
    pub fn least_significant_square_bb(self) -> Self
    {
        assert!(self.nonzero());
        unsafe {
            let this: i64 = std::mem::transmute(self.0);
            std::mem::transmute(this & -this)
        }
    }

    // Finds and clears the least significant bit in a non-zero bitboard.
    #[inline]
    pub fn pop_lsb(&mut self) -> Square
    {
        assert!(self.nonzero());
        let s: Square = self.lsb();
        self.0 &= self.0 - 1;
        s
    }

    #[inline]
    fn attacks_for_piece(pt: PieceType, sq: Square, occupied: Self) -> Self
    {
        debug_assert!(!matches!(pt, PieceType::Pawn));
        debug_assert!(sq.is_ok());

        match pt {
            PieceType::Bishop => Self::attacks_bb::<{ PieceType::Bishop }>(sq, occupied),
            PieceType::Rook => Self::attacks_bb::<{ PieceType::Rook }>(sq, occupied),
            PieceType::Queen => Self::attacks_bb::<{ PieceType::Queen }>(sq, occupied),
            _ => pseudo_attacks()[pt][sq],
        }
    }

    #[inline]
    fn attacks_bb<const pt: PieceType>(sq: Square, occupied: Self) -> Self
    {
        debug_assert!(!matches!(pt, PieceType::Pawn));
        debug_assert!(sq.is_ok());

        match pt {
            PieceType::Bishop => bishop_magics()[sq].get_attacks(occupied),
            PieceType::Rook => rook_magics()[sq].get_attacks(occupied),
            PieceType::Queen => {
                Self::attacks_bb::<{ PieceType::Bishop }>(sq, occupied)
                    | Self::attacks_bb::<{ PieceType::Rook }>(sq, occupied)
            }
            _ => pseudo_attacks()[pt][sq],
        }
    }

    // Returns the attacks by the given piece
    // assuming the board is occupied according to the passed Bitboard.
    // Sliding piece attacks do not continue passed an occupied square.
    // #[inline] fn attacks_bb(pt: PieceType, sq: Square, occupied: Bitboard ) -> Self {
    //
    //     debug_assert!(!matches!(pt, PieceType::Pawn));
    //     debug_assert!(sq.is_ok());
    //
    //     match pt
    //     {
    //     PieceType::Bishop =>
    //         return attacks_bb<BISHOP>(s, occupied);
    //     PieceType::Rook =>
    //         return attacks_bb<ROOK>(s, occupied);
    //     PieceType::Queen =>
    //         return attacks_bb<BISHOP>(s, occupied) | attacks_bb<ROOK>(s, occupied);
    //     _ =>
    //         return PseudoAttacks[pt][s];
    //     }
    // }

    #[inline]
    /// Returns the bitboard of target square for the given step
    /// from the given square. If the step is off the board, returns empty bitboard.
    pub fn safe_destination(src: Square, step: Direction) -> Self
    {
        let dest = src + step;

        if dest.is_ok() && Square::distance(src, dest) <= 2 {
            dest.into()
        } else {
            Self::EMPTY
        }
    }

    pub fn sliding_attack(pt: PieceType, sq: Square, occupied: Self) -> Self
    {
        const RookDirections: [Direction; 4] = [
            Direction::North,
            Direction::South,
            Direction::East,
            Direction::West,
        ];
        const BishopDirections: [Direction; 4] = [
            Direction::NorthEast,
            Direction::SouthEast,
            Direction::SouthWest,
            Direction::NorthWest,
        ];

        let mut attacks = Self::default();
        let dirs = match pt {
            PieceType::Rook => &RookDirections,
            PieceType::Bishop => &BishopDirections,
            _ => unreachable!(),
        };

        for &dir in dirs {
            let mut s = sq;
            while Self::safe_destination(s, dir).nonzero() {
                s += dir;
                attacks |= s.into();
                if (occupied & s.into()).nonzero() {
                    break;
                }
            }
        }

        attacks
    }
}

impl std::ops::Mul for Bitboard
{
    type Output = Self;
    fn mul(self, rhs: Self) -> Self::Output
    {
        Self(self.0 * rhs.0)
    }
}

impl std::ops::Not for Bitboard
{
    type Output = Self;
    fn not(self) -> Self::Output
    {
        Self(!self.0)
    }
}

// Magic holds all magic bitboards relevant data for a single square
#[derive(Default)]
struct MagicInit
{
    mask: Bitboard,
    magic: Bitboard,
    attacks: usize,
    shift: u32,
}
impl MagicInit
{
    unsafe fn finalize(self, table: &'static [Bitboard]) -> Magic
    {
        Magic {
            mask: self.mask,
            magic: self.magic,
            attacks: &table[self.attacks..],
            shift: self.shift,
        }
    }
}

struct Magic
{
    mask: Bitboard,
    magic: Bitboard,
    attacks: &'static [Bitboard],
    shift: u32,
}
impl Magic
{
    // Compute the attack's index using the 'magic bitboards' approach
    fn index(&self, occupied: Bitboard) -> u32
    {
        if HasPext {
            return unsafe { std::arch::x86_64::_pext_u64(occupied.0, self.mask.0) as u32 };
        }

        if Is64Bit {
            return (((occupied & self.mask) * self.magic) >> self.shift).0 as u32;
        }

        let lo = (occupied & self.mask).0 as u32;
        let hi = ((occupied.0 >> 32) & (self.mask.0 >> 32)) as u32;
        ((lo * (self.magic.0 as u32)) ^ (hi * (self.magic.0 >> 32) as u32)) >> self.shift
    }

    pub fn get_attacks(&self, occupied: Bitboard) -> Bitboard
    {
        self.attacks[self.index(occupied) as usize]
    }
}

impl std::ops::BitAnd<Bitboard> for Square
{
    type Output = Bitboard;
    fn bitand(self, rhs: Bitboard) -> Self::Output
    {
        Bitboard::square_bb(self) & rhs
    }
}
impl std::ops::BitOr<Bitboard> for Square
{
    type Output = Bitboard;
    fn bitor(self, rhs: Bitboard) -> Self::Output
    {
        Bitboard::square_bb(self) | rhs
    }
}
impl std::ops::BitXor<Bitboard> for Square
{
    type Output = Bitboard;
    fn bitxor(self, rhs: Bitboard) -> Self::Output
    {
        Bitboard::square_bb(self) ^ rhs
    }
}
impl std::ops::BitOr for Square
{
    type Output = Bitboard;
    fn bitor(self, rhs: Self) -> Self::Output
    {
        Bitboard::square_bb(self) | Bitboard::square_bb(rhs)
    }
}
impl From<Square> for Bitboard
{
    fn from(sq: Square) -> Self
    {
        Self::square_bb(sq)
    }
}
impl From<Rank> for Bitboard
{
    fn from(rk: Rank) -> Self
    {
        Self::rank_bb(rk)
    }
}
impl From<File> for Bitboard
{
    fn from(fl: File) -> Self
    {
        Self::file_bb(fl)
    }
}

// Returns true if the squares s1, s2 and s3 are aligned either on a
// straight or on a diagonal line.
#[inline]
fn aligned(s1: Square, s2: Square, s3: Square) -> bool
{
    (Bitboard::line(s1, s2) & s3.into()).nonzero()
}

impl std::fmt::Display for Bitboard
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
    {
        const SEP: &str = "+---+---+---+---+---+---+---+---+";

        writeln!(f, "{SEP}")?;
        for rank in Rank::iter().rev() {
            for file in File::iter() {
                if (Square::new(file, rank) & *self).nonzero() {
                    write!(f, "| X ")?;
                } else {
                    write!(f, "|   ")?;
                }
            }

            writeln!(f, "| {rank}")?;
            writeln!(f, "{SEP}")?;
        }
        writeln!(f, "  a   b   c   d   e   f   g   h")?;
        Ok(())
    }
}

#[cfg(test)]
mod tests
{
    use super::*;

    #[test]
    fn display_bitboard()
    {
        let u = (1 << 16) - 1;
        let v = u << 48;
        let board = Bitboard(u + v);
        println!("{board}");
    }
}
